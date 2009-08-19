(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* Module: Compile_annotate
   Description:
     This module implements the walker to calculate annotations on the
     algebra AST. 
*)

(****************************)
(* Main annotation function *)
(****************************)

(* This function has a side-effect of filing out the
   compile_annotations field for an operator. It should only be called
   once. This condition is asserted inside the code. It will raise an
   Internal_Error if this does not hold.

   It calls itself on the entire sub-tree. This means it should only
   be called once on the root of each sub-tree.
*)

val annotate_algebraic_expression   : ('a, 'b) Xquery_algebra_ast.aalgop_expr -> unit
val reannotate_algebraic_expression : ('a, 'b) Xquery_algebra_ast.aalgop_expr -> unit

val annotate_algebraic_module       : ('a, 'b, 'c) Xquery_algebra_ast.aalgop_xmodule -> unit

