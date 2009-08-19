(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: fn_error.mli,v 1.11 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Fn_error
   Description:
     This module implements support for the user-level fn:error()
     function.
*)

(* QName, Description, Error objects *)
type xquery_error_msg = Physical_value.item * Physical_value.item option * Physical_value.item list 

exception Xquery_error of xquery_error_msg

val raise_error : Processing_context.processing_context -> Physical_value.item list -> 'a

val downgrade_error : Processing_context.processing_context -> xquery_error_msg -> 'a

