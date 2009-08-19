(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: shredded_dm_step.mli,v 1.2 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Shredded_dm_step
   Description:
     This module contains some generic evaluation code for XPath
     steps.
*)

open Xquery_common_ast
open Xquery_type_core_ast
open Xquery_algebra_ast

open Dm_types

(* This signature is the limit of our internal
   access to the store. We deal in internal types
   which are not exposed in the store interface, so this
   allows us to share them explicitly *)
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
    
  val item_matches_kind_test : handle -> cxschema -> shred_kind_test -> Store.stored_nodeid -> bool  
  val eval_node_test_gen     : handle -> cxschema option -> axis -> shred_node_test -> Store.stored_nodeid -> bool
end

module Shredded_XPath_Step_Functor : Shredded_XPath_Step_Functor_Sig
