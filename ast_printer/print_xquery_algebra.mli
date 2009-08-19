(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: print_xquery_algebra.mli,v 1.6 2007/02/01 22:08:45 simeon Exp $ *)

(* Module Algebra_printer
   Description:
     This module implements pretty-printing for the XQuery Algebra AST.
*)

val fprintf_logical_algstatement : Format.formatter -> string ->  ('a, 'b) Xquery_algebra_ast.aalgop_expr -> unit
val printf_logical_algstatement  : string ->  ('a, 'b) Xquery_algebra_ast.aalgop_expr -> unit
val bprintf_logical_algstatement : string -> ('a, 'b) Xquery_algebra_ast.aalgop_expr -> string

val fprintf_logical_algprolog : Format.formatter -> string -> ('a, 'b, 'c) Xquery_algebra_ast.aalgop_prolog -> unit
val printf_logical_algprolog  : string -> ('a, 'b, 'c) Xquery_algebra_ast.aalgop_prolog -> unit
val bprintf_logical_algprolog : string -> ('a, 'b, 'c) Xquery_algebra_ast.aalgop_prolog -> string

val fprintf_logical_algprolog : Format.formatter -> string -> ('a, 'b, 'c) Xquery_algebra_ast.aalgop_prolog -> unit
val printf_logical_algprolog  : string -> ('a, 'b, 'c) Xquery_algebra_ast.aalgop_prolog -> unit
val bprintf_logical_algprolog : string -> ('a, 'b, 'c) Xquery_algebra_ast.aalgop_prolog -> string

val fprintf_logical_algmodule : Format.formatter -> string -> ('a, 'b, 'c) Xquery_algebra_ast.aalgop_xmodule -> unit
val printf_logical_algmodule  : string -> ('a, 'b, 'c) Xquery_algebra_ast.aalgop_xmodule -> unit
val bprintf_logical_algmodule : string -> ('a, 'b, 'c) Xquery_algebra_ast.aalgop_xmodule -> string


val fprintf_optimized_algstatement : Format.formatter -> string ->  ('a, 'b) Xquery_algebra_ast.aalgop_expr -> unit
val printf_optimized_algstatement  : string ->  ('a, 'b) Xquery_algebra_ast.aalgop_expr -> unit
val bprintf_optimized_algstatement : string -> ('a, 'b) Xquery_algebra_ast.aalgop_expr -> string

val fprintf_optimized_algprolog : Format.formatter -> string -> ('a, 'b, 'c) Xquery_algebra_ast.aalgop_prolog -> unit
val printf_optimized_algprolog  : string -> ('a, 'b, 'c) Xquery_algebra_ast.aalgop_prolog -> unit
val bprintf_optimized_algprolog : string -> ('a, 'b, 'c) Xquery_algebra_ast.aalgop_prolog -> string

val fprintf_optimized_algprolog : Format.formatter -> string -> ('a, 'b, 'c) Xquery_algebra_ast.aalgop_prolog -> unit
val printf_optimized_algprolog  : string -> ('a, 'b, 'c) Xquery_algebra_ast.aalgop_prolog -> unit
val bprintf_optimized_algprolog : string -> ('a, 'b, 'c) Xquery_algebra_ast.aalgop_prolog -> string

val fprintf_optimized_algmodule : Format.formatter -> string -> ('a, 'b, 'c) Xquery_algebra_ast.aalgop_xmodule -> unit
val printf_optimized_algmodule  : string -> ('a, 'b, 'c) Xquery_algebra_ast.aalgop_xmodule -> unit
val bprintf_optimized_algmodule : string -> ('a, 'b, 'c) Xquery_algebra_ast.aalgop_xmodule -> string


val fprintf_physical_algstatement : Format.formatter -> string ->  ('a, 'b) Xquery_algebra_ast.aalgop_expr -> unit
val printf_physical_algstatement  : string ->  ('a, 'b) Xquery_algebra_ast.aalgop_expr -> unit
val bprintf_physical_algstatement : string -> ('a, 'b) Xquery_algebra_ast.aalgop_expr -> string

val fprintf_physical_algprolog : Format.formatter -> string -> ('a, 'b, 'c) Xquery_algebra_ast.aalgop_prolog -> unit
val printf_physical_algprolog  : string -> ('a, 'b, 'c) Xquery_algebra_ast.aalgop_prolog -> unit
val bprintf_physical_algprolog : string -> ('a, 'b, 'c) Xquery_algebra_ast.aalgop_prolog -> string

val fprintf_physical_algmodule : Format.formatter -> string -> ('a, 'b, 'c) Xquery_algebra_ast.aalgop_xmodule -> unit
val printf_physical_algmodule  : string -> ('a, 'b, 'c) Xquery_algebra_ast.aalgop_xmodule -> unit
val bprintf_physical_algmodule : string -> ('a, 'b, 'c) Xquery_algebra_ast.aalgop_xmodule -> string

(* Node kind *)

val print_anode_test : Format.formatter ->  Xquery_algebra_ast.anode_test -> unit
val printf_anode_test  : string -> Xquery_algebra_ast.anode_test -> unit
val bprintf_anode_test : string -> Xquery_algebra_ast.anode_test -> string

val print_asequencetype : Format.formatter ->  Xquery_algebra_ast.asequencetype -> unit

val print_use_counts :  
    Format.formatter ->  (Namespace_names.rqname * (int * Xquery_algebra_ast.variable_usage)) list -> unit
