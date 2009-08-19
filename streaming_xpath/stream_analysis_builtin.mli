(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: stream_analysis_builtin.mli,v 1.2 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Stream_analysis_builtin
   Description:
     This module encapsulates judgements for deciding wether a call to
     a builtin function is fatal for streaming XPath evaluation or
     not.

   - Michael *)


val is_malicious_builtin_funcall :
  Df_analysis.ac_handle -> bool
