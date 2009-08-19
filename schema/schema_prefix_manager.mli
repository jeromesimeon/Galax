(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: schema_prefix_manager.mli,v 1.5 2007/02/01 22:08:53 simeon Exp $ *)

(* Module: Schema_prefix_manager
   Description:
     This modules is used to handle namespace resolution rules within
     an XML schema.
 *)


type prefix_manager 

  (* Caution: a prefix manager combines functional and imperative
     properties.  For the most part, it behaves like a functional
     environment, but all prefix managers that originate from the same
     create() call (call them "relatives") share an imperative store
     that keeps track of all the bindings ever added to them.  See
     other comments for more details.  *)


val create : unit -> prefix_manager 


(* Adds a binding as usual in environments, but with a twist: if the
   prefix being added was ever (in the history of this prefix
   manager) used with another uri, then the prefix's name is changed
   to avoid the conflict.

   Also:
   - if this is a binding removal (uri="") 

  *)
val add_binding : prefix_manager -> (Namespace_names.prefix * Namespace_names.uri) -> prefix_manager
val add_bindings : prefix_manager -> (Namespace_names.prefix * Namespace_names.uri) list -> prefix_manager


(* Returns the prefix most recently associated with the given uri. *)

val unresolve_uri : prefix_manager -> Namespace_names.uri -> Namespace_names.prefix


(* Returns all bindings ever added to this prefix manager or its relatives. *)

val invented_bindings : prefix_manager -> (Namespace_names.prefix * Namespace_names.uri) list

