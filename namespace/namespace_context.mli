(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: namespace_context.mli,v 1.8 2007/10/16 01:25:34 mff Exp $ *)

(* Module: Namespace_context
   Description:
     This module implements support for the namespace context.
*)

open Namespace_names


(**********************)
(* Namespace bindings *)
(**********************)

(* Namespace binding *)

type binding = prefix * uri

(* Type for binding tables *)

type binding_table = binding list


(*************************)
(* Namespace environment *)
(*************************)

(* type for namespace environments *)

type nsenv

(* creates an empty environment *)

val empty_nsenv           : nsenv
val make_empty_nsenv      : unit -> nsenv
val dump_nsenv            : nsenv -> unit
val default_xquery_nsenv  : nsenv
val default_xml_nsenv     : nsenv
val default_xml_out_nsenv : unit -> nsenv
val default_all_nsenv     : nsenv

(* adds a new prefix,uri binding in a namespace environment *)

val add_all_ns        : nsenv -> binding_table -> nsenv
val add_all_ns_xquery : nsenv -> binding_table -> nsenv

val filter_nsenv_in_scope : nsenv -> binding_table -> nsenv * binding_table

(* gets the namespace URI bound to a given prefix *)

val get_ns_of_prefix  : nsenv -> prefix -> uri

(* gets the closest namespace prefix bound to a given URI *)

val make_binding : nsenv -> rqname -> (uqname * binding option * binding)
val make_attribute_binding : nsenv -> rqname -> (uqname * binding option * binding)

(* Returns the set of current bindings *)

val active_bindings : nsenv -> binding_table
val delta_bindings : nsenv -> nsenv -> binding_table
val patch_bindings : nsenv -> binding_table -> nsenv

(* Returns all of the bindings *)

val flatten_bindings : nsenv -> binding_table
val cleanup_bindings : binding_table -> binding_table -> binding_table
val cleanup_out_bindings : binding_table -> binding_table -> binding_table
val cleanup_actual_out_bindings : binding_table -> binding_table -> binding_table

(* Are the nsenvs the identical ?*)

val same_nsenv : nsenv -> nsenv -> bool

(* Print a binding table *)

val print_binding_table : string -> Format.formatter -> binding_table -> unit

val print_special_attributes : string -> Format.formatter -> (uqname * string) list -> unit

