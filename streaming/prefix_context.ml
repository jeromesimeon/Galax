(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: prefix_context.ml,v 1.5 2007/08/30 22:39:53 simeon Exp $ *)

(* Module: Prefix_context
   Description:
     This module implements the context necessary to turn a resolved
     SAX stream back into an unresolved one with the proper xmlns
     attributes.
*)


(*********************************)
(* A type for the prefix context *)
(*********************************)

type prefix_context =
    { prefix_context_nsenv : Namespace_context.nsenv Stack.t;
      current_bindings : Namespace_context.nsenv }


(********************************)
(* Creates a new prefix context *)
(********************************)

let build_prefix_context () =
  { prefix_context_nsenv = Stack.create ();
    current_bindings = Namespace_context.empty_nsenv }



(*********************************)
(* Operations on prefix contexts *)
(*********************************)

let push_nsenv_in_prefix_context prefix_context new_nsenv =
  let delta_bindings,new_nsenv =
    try
      let previous_nsenv = Stack.top prefix_context.prefix_context_nsenv in
      let delta_bindings =
	if Namespace_context.same_nsenv new_nsenv previous_nsenv
	then []
	else
	  Namespace_context.delta_bindings new_nsenv previous_nsenv
      in
      let new_nsenv,delta_bindings = Namespace_context.filter_nsenv_in_scope previous_nsenv delta_bindings in
      (delta_bindings,new_nsenv)
    with
    | Stack.Empty ->
	Namespace_context.flatten_bindings new_nsenv,new_nsenv
  in
  Stack.push new_nsenv prefix_context.prefix_context_nsenv;
  Namespace_context.cleanup_actual_out_bindings delta_bindings []

let pop_nsenv_from_prefix_context prefix_context =
  ignore(Stack.pop prefix_context.prefix_context_nsenv)

