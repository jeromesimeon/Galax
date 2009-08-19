(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xquery_type_ast_map.mli,v 1.6 2007/07/05 14:58:44 ndonose Exp $ *)

(* Interface for the module Xquery_type_ast_map
   Description:
    Helping functions to build @see xquery_type_ast.mli type AST 
    using information from element and type declarations.
*)

open Xquery_ast
open Xquery_type_ast
open Namespace_names

open Wsdl_ast

val anytype : Xquery_ast.sequencetype
val make_sequencetype : Xquery_ast.itemtype -> Xquery_ast.sequencetype
  
val create_new_var : string list -> string -> string



(** 
  Maps a WSDL type reference to a sequence type.
  @param type_name: the (resolved) name of the type we are looking for
  @param schema: the schema we use
  @return the type associated by the WSDL/XQuery binding or None, if no mapping 
  is possible
*)
val xquery_type_from_wsdl_type : Namespace_names.rqname -> xschema -> sequencetype option

(** 
  Maps a WSDL element reference to a sequence type. If the prefix is not found,
  then it returns None.
*)
val xquery_element_from_wsdl_element : Namespace_names.rqname -> xschema -> sequencetype option



(** other functions *)

(** find the corresponding namespace declaration in the schema or raises Not_found *)
val lookup_prefix_for_uri : xschema -> Namespace_names.uri -> Namespace_names.prefix
  
(** find the corresponding type declaration in the schema or raises Not_found *)
val lookup_type_in_xschema : xschema -> Xquery_common_ast.tname -> xtype_derivation 
  
(** find/create a WSDL type for a given type from an XQuery module *)
val get_wsdl_type_for_xquery_type :
    sequencetype option -> wsdl_module -> part_type_decl
