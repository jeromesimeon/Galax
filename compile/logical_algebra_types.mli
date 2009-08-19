(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: logical_algebra_types.mli,v 1.5 2007/02/01 22:08:45 simeon Exp $ *)

(* Module logical_algebra_types
   Description:
     This *interface* contains type declarations for logical code.
*)

open Xquery_algebra_ast

open Alg_path_struct

(* Additional 'semi-logical' types. Introduced in order to
   keep everything related to logical types in one place (here).

    - Michael
*)
type 'a semilogical_opt_algop_expr = ('a option, path_annotation) aalgop_expr
type 'a semilogical_opt_algop_sub_exprs = ('a option, path_annotation) aalgop_sub_exprs

type 'a semilogical_algop_sub_exprs = ('a, path_annotation) aalgop_sub_exprs
type ('a, 'b) semilogical_algop_decl = ('a, path_annotation ,'b option) aalgop_decl

type logical_algop_sub_exprs = (unit, path_annotation) aalgop_sub_exprs
type logical_algop_expr = (unit, path_annotation) aalgop_expr

type logical_algop_decl = (unit, path_annotation, unit) aalgop_decl
type logical_algop_prolog = (unit, path_annotation, unit) aalgop_prolog
type logical_algop_xmodule = (unit, path_annotation, unit) aalgop_xmodule
type logical_algop_function_body = (unit, path_annotation) aalgop_expr
type logical_compile_context = (unit, path_annotation) Compile_context.compile_context

