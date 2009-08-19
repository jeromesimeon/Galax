(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: schema_prefix_manager.ml,v 1.7 2007/02/01 22:08:53 simeon Exp $ *)

(* Module: Schema_prefix_manager
   Description:
     This modules is used to handle namespace resolution rules within
     an XML schema.
 *)


module N = Namespace_names
module NB = Namespace_builtin
open Error
type iprefix = string   (* "invented" or "imported" prefix *)

type uristr = string

let default_ns_prefix_seed = "ns"  
  (* Prefix to be automatically suggested for default namespace declarations
     (xmlns="..."), except 
     (1) the default declarations in <schema>;
     (2) any default decl for a namespace that was previously given an
     explicit prefix.
  *)

let empty_ns_prefix = NB.empty_prefix

let empty_ns_prefix_string = match empty_ns_prefix with 
  | N.NSPrefix s -> s
  | _ -> raise (Query(Schema_Internal "BUG in Prefix_manager.empty_ns_prefix_string"))

let import_error msg = raise (Error.Query (Error.Load_Error msg))


(*--VG!! Note: after adding the original prefix to pm_env, it contains all
   the info that Namespace.nsenv does, so can potentially be used
   instead of it in Schema_import.ImportState.*) 

type prefix_manager = {
  env : (N.prefix * N.uri * N.prefix) list;
      (*   (original,  uri,    invented) *)
  uri2pref : (uristr, iprefix) Hashtbl.t;
      (*     (uri,   invented)   *)
  prefcache : (iprefix, unit) Hashtbl.t; 
  counter : int ref;
}


let create () = 
  let u2p = Hashtbl.create 15 in
  let pc = Hashtbl.create 15 in
    Hashtbl.add u2p "" empty_ns_prefix_string;
    Hashtbl.add pc empty_ns_prefix_string ();
    {
      env = 
       [(empty_ns_prefix, N.NSUri "", empty_ns_prefix) ];
      uri2pref = u2p;
      prefcache = pc;
      counter = ref 1;
    }

  (* Cases: 

     pref==empty, uri<>""  (re)declaration of the default namespace.
       We put in a prefix that was used before for this namespace, 
       or invent a new one. 

     pref==empty, uri==""  undeclaring the default namespace.

     pref<>empty, uri<>""  (re)declaring a prefix.

     pref<>empty, uri==""  undeclaring a prefix 
  *)


let invent_prefix pm suggestion = 
  let rec attempt candidate cnt = 
    if not (Hashtbl.mem pm.prefcache candidate)
    then (candidate, cnt)
    else attempt (suggestion ^ (string_of_int cnt)) (cnt+1)    
  in 
    if suggestion = "" 
    then raise (Query(Schema_Internal "BUG in Prefix_manager.invent_prefix: empty string as prefix suggestion"))
    else 
      let (ipref, newcnt) = attempt suggestion !(pm.counter) in
	Hashtbl.add pm.prefcache ipref ();
	pm.counter := newcnt;
	ipref
  


let register_prefix pm pref uristr = 
    match pref with 
      | N.NSDefaultElementPrefix ->  (* Pref for the default namespace decl *)
	  (* Use the prefix most recently declared for this namespace, 
	     or invent a new one.
	     Note: we need to invent a prefix here, since the import's output
	     uses the empty prefix _exclusively_ for local elements and
	     attributes declared in no namespace.
	   *)
	  (try Hashtbl.find pm.uri2pref uristr
	   with Not_found -> 
	     let ipref = invent_prefix pm default_ns_prefix_seed 
	     in  Hashtbl.add pm.uri2pref uristr ipref;
	       ipref
	  )
      | N.NSPrefix pstr ->   (* Pref for a prefix-bound namespace *)
	  (* If this prefix was already used with this namespace, 
	     use it again.  Otherwise, invent a new one. *)
	  if List.mem pstr (Hashtbl.find_all pm.uri2pref uristr)
	  then pstr
	  else 
	    let ipref = invent_prefix pm pstr 
	    in  Hashtbl.add pm.uri2pref uristr ipref;
	      ipref
      | _ -> import_error ("BUG in Prefix_manager.register_prefix")  


let rec remove_original pref = function
  | (p,_,_) :: rest 
    when p=pref -> rest
  | b :: rest -> b :: (remove_original pref rest)
  | [] -> []
      

let add_binding pm (pref,uri) = 
  match uri with 
    | N.NSUri "" ->   (* Undeclaring a namespace *)
	(* Find the last binding for pref and remove it *)
	{ pm with env = remove_original pref pm.env }
    | N.NSUri uristr -> (* (Re)declaring a namespace *)
	let ipref = register_prefix pm pref uristr
	in
	  { pm with env = (pref, uri, N.NSPrefix ipref) :: pm.env }
    | N.NSWildcardUri ->
	import_error ("BUG in Prefix_manager.add_binding")


let add_bindings pm bindings = 
  List.fold_left add_binding pm bindings

let unresolve_uri pm uri = 
  let rec find = function
    | (_,u,ip) :: rest 
      when u=uri -> ip
    | b :: rest -> find rest
    | [] -> import_error ("BUG in Prefix_manager.unresolve_uri for uri '" ^
			  (N.quoted_string_of_uri uri) ^ "'")
  in
    find pm.env


let invented_bindings pm = 
  Hashtbl.fold 
    (fun uristr ipref lst -> (N.NSPrefix ipref, N.NSUri uristr)::lst)
    pm.uri2pref []

