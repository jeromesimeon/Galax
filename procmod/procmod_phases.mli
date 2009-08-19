(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: procmod_phases.mli,v 1.33 2007/05/16 15:32:12 mff Exp $ *)

(* Module: Procmod_phase
   Description:
     This module supports a modular approach to the XQuery processing
     model. A structure is used to describe each processing phase and
     can be used to register the appropriate call for each
     phase. Currently, the signature of the call for each phase is
     *fixed*.
*)

open Processing_context
open Parse_context
open Norm_context
open Typing_context
open Compile_context
open Code_selection_context
open Execution_context

open Xquery_type_core_ast
open Xquery_ast
open Xquery_core_ast
open Xquery_algebra_ast
open Algebra_type
open Physical_value_util

open Procmod_types


(***************************)
(* Global handler creation *)
(***************************)

(* Creates a global handler from specific handlers for each phase *)

val build_phase_handler :
    parsing_phase_handler ->
      preprocessing_phase_handler ->
	normalization_phase_handler ->
	  rewriting_phase_handler ->
	    factorization_phase_handler ->
	      compile_phase_handler ->
		optimization_phase_handler ->
		  selection_phase_handler ->
		    evaluation_phase_handler -> phase_handler

(* Creates a phase handler according to a given processing context *)

val phase_handler_of_processing_context : processing_context -> phase_handler


