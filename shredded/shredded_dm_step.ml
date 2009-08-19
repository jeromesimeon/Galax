(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: shredded_dm_step.ml,v 1.5 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Shredded_dm_step
   Description:

   This module contains some XPath evaluation that is internal to the
   store.  In particular we compile nodetest into internal versions
   which have the internal extended qname (uri + local name) as a
   single integer.  This allows us to compare internal qnames without
   querying a string pool.

*)

(* AST NOTE: The Internal AST seems a bit ad-hoc. It contains a
   mixture of external types (Namespace_symbols.xXx) and internal
   types (eqnameids).  The goal is to minimize conversion, so the
   values are stored in whatever form they are needed to be used
   without conversion. For example, schema element tests require
   interaction with the schema - and so are stored as
   relem_symbols. In constrast, untyped regular element/attribute
   tests require only comparison of eqnameids, so they are stored in
   an internal format.

*)
(* AST NOTE 2: Because Schema_judge requires rqnames instead of
   qnames, we store these in our internal AST. This will hopefully go
   away and the type declarations for the desired AST will be
   commented out so the change over will be eased.. *)



open Error
open Datatypes
open Datatypes_util

open Xquery_ast
open Xquery_algebra_ast
open Xquery_common_ast

(* open Dm_types *)

(* This functor provides a store with internal matching *)
module type Step_Store = sig  
  type handle
  type stored_nodeid
  type record_kind

  type elem_eqnameid 
  type attr_eqnameid
  type type_eqnameid

  val encode_elem_eqnameid : handle -> Namespace_symbols.relem_symbol -> elem_eqnameid
  val encode_attr_eqnameid : handle -> Namespace_symbols.rattr_symbol -> attr_eqnameid
  val encode_type_eqnameid : handle -> Namespace_symbols.rtype_symbol -> type_eqnameid

  val get_record_kind    : handle -> stored_nodeid -> record_kind  
  val get_elem_name : handle -> stored_nodeid -> Namespace_symbols.relem_symbol
  val get_attr_name : handle -> stored_nodeid -> Namespace_symbols.rattr_symbol
  val get_type      : handle -> stored_nodeid -> Namespace_symbols.rtype_symbol option

  val get_elem_eqnameid : handle -> stored_nodeid -> elem_eqnameid
  val get_attr_eqnameid : handle -> stored_nodeid -> elem_eqnameid
  val get_type_eqnameid : handle -> stored_nodeid -> elem_eqnameid option

  val get_elem_name_eqnameid_with_type_name : handle -> stored_nodeid -> elem_eqnameid * Namespace_symbols.rtype_symbol option
  val get_attr_name_eqnameid_with_type_name : handle -> stored_nodeid -> attr_eqnameid * Namespace_symbols.rtype_symbol option

  val get_single_element_node : handle -> stored_nodeid -> stored_nodeid
  val get_pi_target : handle -> stored_nodeid -> string 
end

module type Shredded_XPath_Step_Functor_Sig =
  functor (Basetypes : Shredded_store_sigs.Shredded_Basetypes) ->
    functor (Store : Step_Store with type stored_nodeid = Basetypes.stored_nodeid 
                                with type record_kind = Basetypes.record_kind
                                with type elem_eqnameid = Basetypes.eqnameid
                                with type attr_eqnameid = Basetypes.eqnameid                                
                                with type type_eqnameid = Basetypes.eqnameid 
 ) ->    
sig
  type handle = Store.handle
   
  (* We 'compile' node tests into a form that is more amenable to
	 internal comparison *)
  type shred_kind_test
  val shred_kind_test_of_akind_test : handle -> akind_test -> shred_kind_test

  type shred_node_test
  val shred_node_test_of_anode_test  : handle -> anode_test -> shred_node_test
    
  val item_matches_kind_test : handle -> Xquery_type_core_ast.cxschema -> shred_kind_test -> Store.stored_nodeid -> bool  

  val eval_node_test_gen     : handle -> Xquery_type_core_ast.cxschema option -> axis -> shred_node_test -> Store.stored_nodeid -> bool
end



module Shredded_XPath_Step_Functor 
  (Basetypes : Shredded_store_sigs.Shredded_Basetypes)
  (Store : Step_Store with type stored_nodeid = Basetypes.stored_nodeid 
                      with type record_kind   = Basetypes.record_kind
                      with type elem_eqnameid = Basetypes.eqnameid
                      with type attr_eqnameid = Basetypes.eqnameid
                      with type type_eqnameid = Basetypes.eqnameid 
  ) 
= struct

  type handle = Store.handle
(************************************************)
(* These are the internal versions of the tests *)
(************************************************)
type shred_element_test =
  | Shred_SchemaElementTest of Namespace_symbols.relem_symbol
(* | Shred_ElementTest of (Store.elem_eqnameid * Store.type_eqnameid option) option *)
  | Shred_ElementTest of (Store.elem_eqnameid * Namespace_symbols.rtype_symbol option) option

let shred_element_test_of_aelement_test store elem_test =
  match elem_test with 
    | ASchemaElementTest elem_test -> 
	Shred_SchemaElementTest elem_test

    | AElementTest None ->
	Shred_ElementTest None

    | AElementTest  (Some (relem_symbol, opt_type)) ->
	  let elem_eqnameid = (Store.encode_elem_eqnameid store relem_symbol) in
(*
	  let opt_type      = 
	    match opt_type with
	    | None -> None
	    | Some type_symbol -> 
		Some (Store.encode_type_eqnameid store type_symbol)
	  in
*)
	  Shred_ElementTest (Some (elem_eqnameid, opt_type))

type shred_attribute_test =
  | Shred_SchemaAttributeTest of Namespace_symbols.rattr_symbol
  | Shred_AttributeTest of (Store.attr_eqnameid * Namespace_symbols.rtype_symbol option) option 

let shred_attribute_test_of_aattribute_test store attr_test =
  match attr_test with 
    | ASchemaAttributeTest attr_test -> 
	Shred_SchemaAttributeTest attr_test

    | AAttributeTest None ->
	Shred_AttributeTest None

    | AAttributeTest  (Some (rattr_symbol, opt_type)) ->
	  let attr_eqnameid = (Store.encode_attr_eqnameid store rattr_symbol) in
	  (* let opt_type      = 
	    match opt_type with
	      | None -> None
	      | Some type_symbol -> 
		  Some (Store.encode_type_eqnameid store type_symbol)
	  in *)
	  (* let opt_type  = 
	    match opt_type with
	      | None -> None
	      | Some v -> Some (Namespace_symbols.rtype_name v)
	  in *)
	    Shred_AttributeTest (Some (attr_eqnameid, opt_type))


type shred_kind_test =
  | Shred_DocumentKind of shred_element_test option    (* ./document-node() *)
  | Shred_ElementKind of shred_element_test            (* ./element() *)
  | Shred_AttributeKind of shred_attribute_test        (* ./attribute() *)
  | Shred_PIKind of string option  	 	     (* ./pi("xxx") *)
  | Shred_CommentKind              	 	     (* ./comment () *)
  | Shred_TextKind                 	 	     (* ./text() *)
  | Shred_AnyKind                  	 	     (* ./node() *)


let shred_kind_test_of_akind_test store akind = 
  match akind with 
    | ADocumentKind None -> Shred_DocumentKind None
    | ADocumentKind Some(element_test) ->	
	Shred_DocumentKind (Some (shred_element_test_of_aelement_test store element_test))
    
    | AElementKind element_test -> 
	Shred_ElementKind (shred_element_test_of_aelement_test store element_test)
    | AAttributeKind attr_test  -> 
	Shred_AttributeKind (shred_attribute_test_of_aattribute_test store attr_test)

    | APIKind str_opt -> Shred_PIKind str_opt	
    | ACommentKind    -> Shred_CommentKind              	 	 
    | ATextKind       -> Shred_TextKind                 	 	
    | AAnyKind        -> Shred_AnyKind


type shred_node_test = 
  | Shred_NameTest of Namespace_symbols.anon_symbol 
  | Shred_NodeKindTest of shred_kind_test

let shred_node_test_of_anode_test store nt = 
  match nt with 
    | APNameTest anon_sym -> Shred_NameTest anon_sym
    | APNodeKindTest kt   -> Shred_NodeKindTest (shred_kind_test_of_akind_test store kt)



(* This should be an integer comparison *)
let internal_symbol_equal (e1:Basetypes.eqnameid) (e2:Basetypes.eqnameid) = e1 = e2

let relem_equal = internal_symbol_equal
let rattr_equal = internal_symbol_equal
let rtype_equal = internal_symbol_equal

(* Subtyping... Logic: 
   if eqname2 = wild_card_eqname then true
   else if wild_card_uri then compare local names match
   else if wild_card_ncname then compare uris
   else regular compare

*)




  type store = Store.handle

  let item_matches_element_test store cxschema dtk node_id =
    match dtk with
      | Shred_ElementTest None -> true
      | Shred_ElementTest (Some (relem_sym2,None)) ->
	  let relem_sym1 = Store.get_elem_eqnameid store node_id in
	    if not(relem_equal relem_sym1 relem_sym2)
	    then
	      false
	    else
	      true

      | Shred_ElementTest (Some (relem_sym2,Some rtype_sym2)) ->
	  let (relem_sym1,rtype_sym1_opt) =
	    Store.get_elem_name_eqnameid_with_type_name store node_id
	  in
	  if not(relem_equal relem_sym1 relem_sym2)
	  then
	    false
	  else
	    begin
	      match rtype_sym1_opt with
	      | None ->
		  true
	      | Some rtype_sym1 ->
		  Schema_judge.derives_from cxschema rtype_sym1 rtype_sym2
	    end
      | Shred_SchemaElementTest relem_sym2 ->
	  (* For global elements, we must look at
	     substitution groups - Jerome *)
	  let relem_sym1     = Store.get_elem_name store node_id in 
	  let rtype_sym1_opt = Store.get_type      store node_id in 

	  let elementref_content = Schema_judge.lookup_element_with_substitution_group cxschema relem_sym1 relem_sym2 in
	    begin
	      match elementref_content with
		| None -> false
		| Some (nillable,rtype_sym2) ->
		    begin
		      match rtype_sym1_opt with
			| None ->
			    true
			| Some rtype_sym1 ->
			    (Schema_judge.derives_from cxschema rtype_sym1 rtype_sym2)
		    end
	    end

let item_matches_attribute_test store cxschema dtk nodeid =
  match dtk with
  | Shred_AttributeTest None -> true
  | Shred_AttributeTest (Some (rattr_sym2,None)) ->
      let rattr_sym1 = Store.get_attr_eqnameid store nodeid in
      if not(rattr_equal rattr_sym1 rattr_sym2)
      then
	false
      else
	true
  | Shred_AttributeTest (Some (rattr_sym2,(* Some rtype_sym2 *) Some ctname2)) ->
      let rattr_sym1 = Store.get_attr_eqnameid store nodeid in
      let rtype_sym1_opt = Store.get_type store nodeid in 

      if not(rattr_equal rattr_sym1 rattr_sym2)
      then
	false
      else
	begin
	  match rtype_sym1_opt with
	  | None ->
	      true
	  | Some rtype_sym1 ->
	      (Schema_judge.derives_from cxschema rtype_sym1 ctname2)
	end
  | Shred_SchemaAttributeTest rattr_sym2 ->
      let rattr_sym1     = Store.get_attr_name store nodeid in
      let rtype_sym1_opt = Store.get_type store nodeid in 

      if not(Namespace_symbols.rattr_equal rattr_sym1 rattr_sym2)
      then
	false
      else
	begin
	  match rtype_sym1_opt with
	  | None -> true
	  | Some rtype_sym1 ->
	      let (_,rtype_sym2) = Schema_judge.lookup_attribute cxschema rattr_sym2 in
	      (Schema_judge.derives_from cxschema rtype_sym1 rtype_sym2)
	end

let item_matches_kind_test store cxschema dtk nodeid =
  if dtk = Shred_AnyKind then true else
    begin
      match Store.get_record_kind store nodeid with
	| Basetypes.DocumentRecordKind ->
	    begin
	      match dtk with
		| Shred_DocumentKind None -> true
		| Shred_DocumentKind (Some etk) ->
		    (* We need to get the single element node in there - Jerome *)
		    let n = Store.get_single_element_node store nodeid in
		      item_matches_element_test store cxschema etk n
		| _ ->
		    false
	    end
	| Basetypes.ElementRecordKind ->
	    begin
	      match dtk with
		| Shred_ElementKind etk ->
		    item_matches_element_test store cxschema etk nodeid
		| _ -> false
	    end
	| Basetypes.AttributeRecordKind ->
	    begin
	      match dtk with
		| Shred_AttributeKind atk ->
		    item_matches_attribute_test store cxschema atk nodeid
		| _ -> false
	    end
	| Basetypes.TextRecordKind ->
	    begin
	      match dtk with
		| Shred_TextKind -> true
		| _ -> false
	    end
	| Basetypes.PIRecordKind ->
	    begin
	      match dtk with
		| Shred_PIKind None -> true
		| Shred_PIKind (Some t) ->
		    let target_string = Store.get_pi_target store nodeid in
		      (t = target_string)
		| _ -> false
	    end
	| Basetypes.CommentRecordKind ->
	    begin
	      match dtk with
		| Shred_CommentKind -> true
		| _ -> false
	    end
    end


let eval_node_test_gen store ocxschema a nt nodeid =
  let cxschema =
    match ocxschema with
      | None -> Schema_builtin.built_in_cxschema
      | Some cxschema -> cxschema
  in
  let pnk = Xquery_common_ast_util.principal_node_kind a in
  let pnk = Dm_step.node_kind_of_principal_node_kind pnk in
    match nt with
      | Shred_NameTest test_symbol ->
	  begin
	    match Store.get_record_kind store nodeid with
	      | Basetypes.ElementRecordKind ->
		  if pnk = Dm_types.ElementNodeKind
		  then
		    let relem_symbol = Store.get_elem_name store nodeid in
		      Namespace_symbols.anon_subtag relem_symbol test_symbol
		  else
		    false
	      | Basetypes.AttributeRecordKind ->
		  if pnk = Dm_types.AttributeNodeKind then
		    let rattr_symbol = Store.get_attr_name store nodeid in
		      Namespace_symbols.anon_subtag rattr_symbol test_symbol
		  else
		    false
	      | _ ->
		  false
	  end
      | Shred_NodeKindTest node_kind ->
	  (* New code to support kind tests 09/06/2005 - Jerome *)
	  item_matches_kind_test store cxschema node_kind nodeid

end
 
