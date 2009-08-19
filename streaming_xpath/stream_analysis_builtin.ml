(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: stream_analysis_builtin.ml,v 1.3 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Stream_analysis_builtin
   Description:
     This module encapsulates judgements for deciding wether a call to
     a builtin function is fatal for streaming XPath evaluation or
     not.

   - Michael *)


open Xquery_core_ast

open Namespace_builtin


let is_malicious_builtin_funcall ac_handle =
  let (opt_cexpr, _ ) = ac_handle in
    match opt_cexpr with
      | Some cexpr ->
	  begin
	    match cexpr.pcexpr_desc with
	      | CECall (x, _, _, _,_ ) when x = fn_root -> true
	      | _ -> false
	  end
      | None -> false

