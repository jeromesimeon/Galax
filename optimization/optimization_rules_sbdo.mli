(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: optimization_rules_sbdo.mli,v 1.4 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Optimization_rules_sbdo
   Description:
     This module implements the SBDO optimization, described in

       Optimizing Sorting and Duplicate Elimination 
       in XQuery Path Expressions
       
       Mary Fernández, Jan Hidders, Philippe Michiels, 
       Jérôme Siméon, Roel Vercammen
       
       DEXA 2005
       http://www.adrem.ua.ac.be/bibrem/pubs/fernandez.1.pdf
*)

open Compile_context
open Xquery_algebra_ast

open Optimization_util
open Optimization_walker

val sbdo_rewrite: optimization_rewrite_rule
val sbdo_rewrite_two: optimization_rewrite_rule


