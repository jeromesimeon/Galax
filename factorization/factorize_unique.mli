(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: factorize_unique.mli,v 1.3 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Factorize_unique
   Description:
   This module assigns all variables a unique name in the core expression.
   
   o It assumes that names that are not bound are globals, or defined in another
     scope. These variable names remain untouched.

     o factorize_unique_with_context allows you to pass in initial bindings, 
       from global variables/parameters if renaming is done there.
       
   o fs:dot is a special variable and is never rebound.         

   The purpose is to simplify other factorization code.
*)


type binding_list = (Xquery_common_ast.cvname * Xquery_common_ast.cvname) list

val factorize_unique_with_context : binding_list -> Xquery_core_ast.acexpr -> Xquery_core_ast.acexpr
val factorize_unique              : Xquery_core_ast.acexpr -> Xquery_core_ast.acexpr

