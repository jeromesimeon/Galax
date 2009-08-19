(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: namespace_resolve.mli,v 1.4 2007/08/17 18:28:54 simeon Exp $ *)

(* Module: Namespace_resolve
   Description:
     This module implements namespace resolution.
*)

open Namespace_names
open Namespace_context
open Namespace_symbols


(**********************************************)
(* Resolving QNames to Expanded QNames        *)
(* Expanded QNames are represented by symbols *)
(**********************************************)

val resolve_type_qname      	  : nsenv -> uqname -> rqname
val resolve_group_qname      	  : nsenv -> uqname -> rqname
val resolve_attrGroup_qname    	  : nsenv -> uqname -> rqname
val resolve_element_qname   	  : nsenv -> uqname -> rqname
val resolve_element_qname_default : nsenv -> uqname -> rqname * bool
val resolve_attribute_qname 	  : nsenv -> uqname -> rqname
val resolve_function_qname  	  : nsenv -> uqname -> rqname
val resolve_variable_qname  	  : nsenv -> uqname -> rqname
val resolve_pragma_qname   	  : nsenv -> uqname -> rqname

(* Resolves directly to a symbol *)

val resolve_element_qname_to_symbol   : nsenv -> uqname -> relem_symbol
val resolve_attribute_qname_to_symbol : nsenv -> uqname -> rattr_symbol
val resolve_type_qname_to_symbol      : nsenv -> uqname -> rtype_symbol
val resolve_anon_qname_to_symbol      : nsenv -> uqname -> anon_symbol

