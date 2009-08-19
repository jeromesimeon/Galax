(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: shredded_qname_mapping.ml,v 1.6 2007/10/16 01:25:35 mff Exp $ *)

(* Module: Shredded_qname_mapping
   Description:
     This module deals with serializing qnames in the jungle store and
     returning them to main memory.
*)

(* This could be further abstracted to a 2-way mapping parameterized
   by type conversion *)


    (************************************************************************)
    (*                                                                      *)
    (* This code implements the mapping external between internal           *)
    (* qnames. Externally, symbols are triples of symbols, a prefix, a      *)
    (* uri and a qname. Internally we store symbols as prefix and eqname    *)
    (* (uri + qname). The choice was made because this is all that is       *)
    (* needed to test symbol equality. Eventually as operations get         *)
    (* pushed below the datamodel, this should speed operation.  One        *)
    (* notable operation is the (soon to come) addition of node tests to    *)
    (* the dm interface.                                                    *)
    (*                                                                      *)
    (************************************************************************)
module type Qname_Lookup = sig
  val get_name   : Namespace_symbols.symbol -> Namespace_names.rqname 
  val get_symbol : Namespace_names.rqname -> Namespace_symbols.symbol
  val get_prefix : Namespace_symbols.symbol -> Namespace_names.prefix
end

module Element_Lookup = struct
  open Namespace_symbols
  let get_name   = relem_name
  let get_symbol = relem_symbol
  let get_prefix = relem_prefix
end

module Attribute_Lookup = struct
  open Namespace_symbols
  let get_name   = rattr_name
  let get_symbol = rattr_symbol
  let get_prefix = rattr_prefix
end

module Type_Lookup = struct
  open Namespace_symbols
  let get_name   = rtype_name
  let get_symbol = rtype_symbol
  let get_prefix = rtype_prefix
end

module Shredded_Qname_Qnameid_Mapping 
  (Basetypes : Shredded_store_sigs.Shredded_Basetypes)
  (Hash_Functor : Shredded_store_sigs.Shredded_Hash_Functor_Sig)
  (Lookup_Module : Qname_Lookup)
  = struct
    exception Shredded_Qname_Error of string

    (* The four on disk hashes *)    
    module Prefixid_Prefix_Hash     = Hash_Functor (Basetypes.Prefixid_Module) (Basetypes.Prefix_Module)
    module Prefix_Prefixid_Hash     = Hash_Functor (Basetypes.Prefix_Module) (Basetypes.Prefixid_Module)
    module Eqname_Eqnameid_Hash     = Hash_Functor (Basetypes.Eqname_Module) (Basetypes.Eqnameid_Module)
    module Eqnameid_Eqname_Hash     = Hash_Functor (Basetypes.Eqnameid_Module) (Basetypes.Eqname_Module)

     (*******************)
     (* Magic Constants *)
     (*******************)
     let const_NSDefaultElementPrefix  = -1 
     let const_NSDefaultFunctionPrefix = -2
     let const_NSWildcardPrefix        = -3 
					   
    let built_in_prefix_mapping = 
      [ (Namespace_names.NSDefaultElementPrefix  , const_NSDefaultElementPrefix);
	(Namespace_names.NSDefaultFunctionPrefix , const_NSDefaultFunctionPrefix);
	(Namespace_names.NSWildcardPrefix        , const_NSWildcardPrefix)]

    let print_prefix ff pref = 
      match pref with
	| Namespace_names.NSDefaultElementPrefix ->
	    Format.fprintf ff "Default Element Prefix"
	| Namespace_names.NSPrefix pref 
	| Namespace_names.NSServerPrefix pref 
	| Namespace_names.NSInterfacePrefix pref ->
	    Format.fprintf ff "%s" pref
	| Namespace_names.NSDefaultFunctionPrefix ->
	    Format.fprintf ff "Default Function Prefix"
	| Namespace_names.NSWildcardPrefix ->
	    Format.fprintf ff "WildCard Prefix"
      

    (*******************)
    (* Exported Handle *)	
    (*******************)
    type mapping = {
      (******************************)
      (* Store prefixes and eqnames *)
      (******************************)      
      prefixid_to_prefix_hash        : Prefixid_Prefix_Hash.hash;
      prefix_to_prefixid_hash        : Prefix_Prefixid_Hash.hash;
      
      eqname_to_eqnameid_hash        : Eqname_Eqnameid_Hash.hash;
      eqnameid_to_eqname_hash        : Eqnameid_Eqname_Hash.hash;
      
      (********************************************)
      (* In memory mapping portion of the eqnames *)
      (********************************************)
      internal_eqnameid_to_external_eqnameid : 
	(Basetypes.eqnameid, (Namespace_symbols.uri_symbol * Namespace_symbols.ncname_symbol)) Hashtbl.t;
      external_eqnameid_to_internal_eqnameid : 
	((Namespace_symbols.uri_symbol * Namespace_symbols.ncname_symbol), Basetypes.eqnameid) Hashtbl.t;    
      internal_prefixid_to_external_prefixid : (Basetypes.prefixid, Namespace_symbols.prefix_symbol) Hashtbl.t;
      external_prefixid_to_internal_prefixid : (Namespace_symbols.prefix_symbol, Basetypes.prefixid) Hashtbl.t;

      (**************)
      (* Generators *)
      (**************)
      mutable prefixid_gen         : Basetypes.Prefixid_Generator.handle;
      mutable eqnameid_gen         : Basetypes.Eqnameid_Generator.handle;
    }

    (*************)
    (* Accessors *)
    (*************)
    let get_prefixid_db mapping = mapping.prefixid_to_prefix_hash
    let get_prefix_db   mapping = mapping.prefix_to_prefixid_hash
				  
    let get_eqname_db   mapping = mapping.eqname_to_eqnameid_hash
    let get_eqnameid_db mapping = mapping.eqnameid_to_eqname_hash


    let new_prefixid store = 
      Basetypes.Prefixid_Generator.new_value store.prefixid_gen
	
    let new_eqnameid store = 
      Basetypes.Eqnameid_Generator.new_value store.eqnameid_gen

	
    (************************************)
    (* These should be stored somewhere *)
    (************************************)
    let prefix_hash_fold_left f s cursor = 
      let rec cursor_fold v =
	match 	Prefixid_Prefix_Hash.hash_cursor_get_next cursor with
	  | None    -> v
	  | Some v' -> cursor_fold (f v v')
      in
	match Prefixid_Prefix_Hash.hash_cursor_get_first cursor with
	  | None -> s
	  | Some id -> 
	      cursor_fold (f s id)

    let get_max_prefixid procid_db = 
      let cursor = Prefixid_Prefix_Hash.hash_cursor_open procid_db in 
      let ret = prefix_hash_fold_left (fun v (v',_) -> if (v > v') then v else v') (-1) cursor in
      let ()  = Prefixid_Prefix_Hash.hash_cursor_close cursor in
	ret 

    let eqnameid_hash_fold_left f s cursor = 
      let rec cursor_fold v =
	match Eqnameid_Eqname_Hash.hash_cursor_get_next cursor with
	  | None    -> v
	  | Some v' -> cursor_fold (f v v')
      in
	match Eqnameid_Eqname_Hash.hash_cursor_get_first cursor with
	  | None -> s
	  | Some id -> 
	      cursor_fold (f s id)

    let get_max_eqnameid procid_db = 
      let cursor = Eqnameid_Eqname_Hash.hash_cursor_open procid_db in 
      let ret = eqnameid_hash_fold_left (fun v (v',_) -> if v > v' then v else v') (-1) cursor in      
      let ()  = Eqnameid_Eqname_Hash.hash_cursor_close cursor in
	ret 
	   
    (*************************************************************)
    (* Loading Functions: Qnames need to be brought in from disk *)
    (*************************************************************)
    let print_uri ff uri = 
      match uri with
	| Namespace_names.NSUri string  -> Format.fprintf ff "%s" string
	| Namespace_names.NSWildcardUri ->  Format.fprintf ff "*" 

    let build_external_eqnames_helper mapping (eqid,(uri,qname)) = 
      (* Format.printf " External Eqnames: %d -> (%a,%s)@." eqid print_uri uri qname;   *)
      let _,uri_id, qnameid = 
	Lookup_Module.get_symbol (Namespace_names.NSWildcardPrefix ,uri,qname) in 
	Hashtbl.add mapping.external_eqnameid_to_internal_eqnameid (uri_id,qnameid) eqid;
	Hashtbl.add mapping.internal_eqnameid_to_external_eqnameid eqid (uri_id,qnameid)
	
    let build_external_prefixids_helper mapping (a_uri,a_qname) (prefixid, prefix) =
      (* Format.printf " External Prefix: %d -> %a@." prefixid print_prefix prefix; *)
      let ext_prefixid, _, _ = Lookup_Module.get_symbol (prefix, a_uri, a_qname) in
	Hashtbl.add mapping.internal_prefixid_to_external_prefixid prefixid ext_prefixid;
	Hashtbl.add mapping.external_prefixid_to_internal_prefixid ext_prefixid prefixid
	

     (* We do two things here:
	1. Setup all stored prefixes
	2. setup mappings for the default ones *)       

     let build_external_prefixids mapping pair_opt = 
       match pair_opt with
	 | None -> ()
	 | Some uri_id_pair -> 
	     let db     = mapping.prefixid_to_prefix_hash in
	     let cursor = Prefixid_Prefix_Hash.hash_cursor_open db in 
	       (* Store builtins *)
	       List.iter (fun (prefix,prefid) ->
			    build_external_prefixids_helper mapping uri_id_pair (prefid, prefix)) 
		 built_in_prefix_mapping;
               
	       let rec cursor_iter cursor value = 
		 match value with
		   | None -> ()
		   | Some (pid,pref) ->
		       build_external_prefixids_helper mapping uri_id_pair (pid,(Namespace_names.NSPrefix pref));
		       cursor_iter cursor (Prefixid_Prefix_Hash.hash_cursor_get_next cursor)
	       in
		 cursor_iter cursor (Prefixid_Prefix_Hash.hash_cursor_get_first cursor)
	     
    let build_external_eqnameids mapping  = 
      let db     = mapping.eqnameid_to_eqname_hash in 
      let cursor = Eqnameid_Eqname_Hash.hash_cursor_open db in 
      let rec cursor_iter cursor value = 
	match value with
	  | None -> ()
	  | Some (eqid_pair) ->
	      build_external_eqnames_helper mapping (eqid_pair);
	      cursor_iter cursor (Eqnameid_Eqname_Hash.hash_cursor_get_next cursor)
		
      in
	match Eqnameid_Eqname_Hash.hash_cursor_get_first cursor with
	  | Some (d,(uri,ncname)) as eqid_pair_opt ->
	      cursor_iter cursor eqid_pair_opt;
	      Some (uri,ncname)

	  | None -> None
		
		  
    (**********************)
    (* Filename functions *)
    (**********************)
    let eqname_db_suffix = "-Eqname.db"
    let eqnameid_db_suffix = "-Eqnameid.db"
    let prefix_db_suffix = "-Prefix.db"
    let prefixid_db_suffix = "-Prefixid.db"
			      
     let create_eqnamedb_name dir name = 
       Filename.concat dir (name ^ eqname_db_suffix)
	 
     let create_eqnameiddb_name dir name = 
       Filename.concat dir (name ^ eqnameid_db_suffix)


     let create_prefixdb_name dir name = 
       Filename.concat dir (name ^ prefix_db_suffix)

     let create_prefixiddb_name dir name = 
       Filename.concat dir (name ^ prefixid_db_suffix)

     (********************)
     (* Public Functions *)
     (********************)
     let mapping_create (directory,name) mapping_name buffsize =       
       let name = name ^ "-" ^ mapping_name in
       (* Open Extended Qname <-> Extended Qname ID mappings *)
       let eqnamedb   = Eqname_Eqnameid_Hash.hash_open 
			  (create_eqnamedb_name directory name) buffsize false in
       let eqnameiddb = Eqnameid_Eqname_Hash.hash_open 
	 (create_eqnameiddb_name directory name) buffsize false in
	 
       (* Prefix <-> Prefixid *)
       let prefixdb   = Prefix_Prefixid_Hash.hash_open
			  (create_prefixdb_name directory name) buffsize false in 
       let prefixiddb = Prefixid_Prefix_Hash.hash_open
			  (create_prefixiddb_name directory name) buffsize false in 
	 
	 {
	   prefixid_to_prefix_hash = prefixiddb;
	   prefix_to_prefixid_hash = prefixdb;
	   eqname_to_eqnameid_hash = eqnamedb;
	   eqnameid_to_eqname_hash = eqnameiddb;

	   internal_eqnameid_to_external_eqnameid = Hashtbl.create 200;
	   external_eqnameid_to_internal_eqnameid = Hashtbl.create 200;
	   internal_prefixid_to_external_prefixid = Hashtbl.create 200;
	   external_prefixid_to_internal_prefixid = Hashtbl.create 200;	   

	   prefixid_gen = 
	    Basetypes.Prefixid_Generator.build_gen Basetypes.Prefixid_Generator.seed_value;
	   eqnameid_gen = 
	    Basetypes.Eqnameid_Generator.build_gen Basetypes.Eqnameid_Generator.seed_value;

	 }
	
     let mapping_open (directory,name) mapping_name buffsize = 
       let mapping = mapping_create (directory,name) mapping_name buffsize in
	 (* Update the Generators *)
	 mapping.prefixid_gen <-
	 Basetypes.Prefixid_Generator.build_gen ((get_max_prefixid mapping.prefixid_to_prefix_hash)+1) ;
	 
	 mapping.eqnameid_gen <-
	 Basetypes.Eqnameid_Generator.build_gen ((get_max_eqnameid mapping.eqnameid_to_eqname_hash)+1);	 

	 (* Build the mapping *)	 
	 let pair_opt = build_external_eqnameids mapping in 
	 build_external_prefixids mapping pair_opt;
	 mapping
       
     let mapping_sync mapping = 
       Prefixid_Prefix_Hash.hash_sync mapping.prefixid_to_prefix_hash;
       Prefix_Prefixid_Hash.hash_sync mapping.prefix_to_prefixid_hash;
       Eqname_Eqnameid_Hash.hash_sync mapping.eqname_to_eqnameid_hash;
       Eqnameid_Eqname_Hash.hash_sync mapping.eqnameid_to_eqname_hash

     let mapping_close mapping = 
       Prefixid_Prefix_Hash.hash_close mapping.prefixid_to_prefix_hash;
       Prefix_Prefixid_Hash.hash_close mapping.prefix_to_prefixid_hash;
       Eqname_Eqnameid_Hash.hash_close mapping.eqname_to_eqnameid_hash;
       Eqnameid_Eqname_Hash.hash_close mapping.eqnameid_to_eqname_hash
	 

    let prefixid_of_prefix mapping prefix =   
      match prefix with
	| Namespace_names.NSDefaultElementPrefix  -> const_NSDefaultElementPrefix 
	| Namespace_names.NSDefaultFunctionPrefix -> const_NSDefaultFunctionPrefix 
	| Namespace_names.NSWildcardPrefix        -> const_NSWildcardPrefix
	| Namespace_names.NSPrefix prefix 
	| Namespace_names.NSServerPrefix prefix 
	| Namespace_names.NSInterfacePrefix prefix -> 
	    (* Encoding Hack *)
	    let prefix_hash = get_prefix_db mapping in
	      match Prefix_Prefixid_Hash.hash_get prefix_hash prefix with
		| None -> 
		    (* Allocate a new one *)
		    let p_id = new_prefixid mapping in 
		      (* Format.printf "Allocation New prefixid: %d for %s@." p_id prefix; *)
		    let prefixid_hash = get_prefixid_db mapping in 	    
		      Prefix_Prefixid_Hash.hash_put prefix_hash prefix p_id;
		      Prefixid_Prefix_Hash.hash_put prefixid_hash p_id prefix;
		      p_id
			
		| Some v -> v
		    
    let prefix_of_prefixid mapping prefixid = 
      match prefixid with
	| -1 -> Namespace_names.NSDefaultElementPrefix   
	| -2 -> Namespace_names.NSDefaultFunctionPrefix 
	| -3 -> Namespace_names.NSWildcardPrefix        
	| _  -> 
	    let prefixid_db = get_prefixid_db mapping in 
	      match Prefixid_Prefix_Hash.hash_get prefixid_db prefixid with
		| None -> raise (Shredded_Qname_Error ("Unknown Prefixid: " ^ (string_of_int prefixid)))
		| Some pid -> 
		    Namespace_names.NSPrefix pid

    let eqnameid_of_eqname mapping eqname = 
      let eqname_ht = get_eqname_db mapping in
	match Eqname_Eqnameid_Hash.hash_get eqname_ht eqname with
	  | None -> 	  
	      let eid = new_eqnameid mapping in 
	      let eqnameid_ht = get_eqnameid_db mapping in
		Eqname_Eqnameid_Hash.hash_put eqname_ht  eqname eid;
		Eqnameid_Eqname_Hash.hash_put eqnameid_ht eid eqname;
		eid

	  | Some v -> v  

    let eqname_of_eqnameid mapping eqnameid = 
      let eqnameid_ht = get_eqnameid_db mapping in 
	match Eqnameid_Eqname_Hash.hash_get eqnameid_ht eqnameid with
	  | None -> raise (Shredded_Qname_Error ("Unknown Eqnameid"))
	  | Some v -> v

	   

    (**************************************************************)
    (* These are the public functions which implement the mapping *)
    (**************************************************************)

    let decode_eqnameid mapping eqnameid =
      try Hashtbl.find mapping.internal_eqnameid_to_external_eqnameid eqnameid
      with Not_found -> raise (Shredded_Qname_Error ("Unknown eqnameid: " ^ (string_of_int eqnameid)))
	
    let decode_prefixid mapping prefixid = 
      try Hashtbl.find mapping.internal_prefixid_to_external_prefixid prefixid
      with Not_found -> raise (Shredded_Qname_Error ("Unknown prefixid: " ^ (string_of_int prefixid)))

    (* Here if we do not find a reference, we add it on disk *)
    let encode_eqnameid mapping (prefix_id,uri_id,qname_id) =   
      if Hashtbl.mem mapping.external_eqnameid_to_internal_eqnameid (uri_id,qname_id) then
	Hashtbl.find mapping.external_eqnameid_to_internal_eqnameid (uri_id,qname_id) 
      else
	begin       
	  let (_,uri,qname) = Lookup_Module.get_name (prefix_id, uri_id, qname_id) in
	  let eqname = uri,qname in 
	  let eqid   = eqnameid_of_eqname mapping eqname in
	    Hashtbl.add mapping.external_eqnameid_to_internal_eqnameid (uri_id,qname_id) eqid;
	    Hashtbl.add mapping.internal_eqnameid_to_external_eqnameid eqid (uri_id,qname_id);
	    eqid
	end

    let encode_prefixid mapping (prefixid_external,uri_id,qname_id) = 
      if   Hashtbl.mem mapping.external_prefixid_to_internal_prefixid prefixid_external
      then Hashtbl.find mapping.external_prefixid_to_internal_prefixid prefixid_external 
      else 
	begin
	  (******************************************************)
	  (* HACK HACK HACK, we know that 0,0 are not looked at *)
	  (******************************************************)
	  let prefix = Lookup_Module.get_prefix (prefixid_external, uri_id,qname_id) in 
	  let prefixid_internal = prefixid_of_prefix mapping prefix in
	    Hashtbl.add mapping.internal_prefixid_to_external_prefixid prefixid_internal prefixid_external; 
	    Hashtbl.add mapping.external_prefixid_to_internal_prefixid prefixid_external prefixid_internal;
	    prefixid_internal
	end
	  
	  
    let decode_symbol mapping prefixid eqnameid  = 
      let uri, qnameid = decode_eqnameid mapping eqnameid in
      let p_id         = decode_prefixid mapping prefixid in
	(p_id,uri,qnameid)

    let encode_symbol mapping (prefix,uri,qname) = 
      let eqnameid = encode_eqnameid mapping (prefix,uri,qname) in 
      let prefixid = encode_prefixid mapping (prefix,uri,qname) in 
	(prefixid,eqnameid)

  end
