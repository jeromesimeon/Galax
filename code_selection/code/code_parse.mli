(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_parse.mli,v 1.8 2007/02/21 21:14:44 simeon Exp $ *)

(* Module: Code_parse
   Description:
     This module contains code building for operators that implement
     parsing of XML documents.
*)

val build_parse_code : 
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr -> 
	string -> 
	  (Algebra_type.alg_eval_code_dep  * Code_selection_context.code_selection_context)

	  
val select_physical_op : 
    Code_selection_context.code_selection_context -> 
      Algebra_type.algop_expr -> 
	string -> 
	  Xquery_physical_algebra_ast.physop_expr_name
	  
