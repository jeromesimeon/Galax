(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: top_util.mli,v 1.11 2007/11/06 16:13:09 mff Exp $ *)

(* Module: Top_util
   Description:
     This module implements some support functions for Galax
     command-line tools.
 *)

(************)
(* Printing *)
(************)

val separator : string ref
val print_processing_file : string -> unit


(*********************************)
(* Argument Processing functions *)
(*********************************)

val init_output_refs  : string -> out_channel ref -> Format.formatter ref -> unit
val close_channel_ref : out_channel ref -> unit


(***********************)
(* Top-level execution *)
(***********************)

(* Both these functions evaluate "exit 1" on exceptions *)
val exec :
    (Processing_context.processing_context -> 'a -> unit) ->
      Processing_context.processing_context -> 'a -> unit
val low_exec :
    (unit -> unit) -> unit -> unit


(******************)
(* Initialization *)
(******************)

val galax_run_proc_ctxt     : unit -> Processing_context.processing_context
val galax_compile_proc_ctxt : unit -> Processing_context.processing_context

val init_all : Processing_context.processing_context -> Galax.compiled_program


(*******************************)
(* Set up the external context *)
(*******************************)

val set_up_external_context :
    Processing_context.processing_context -> (bool * Galax.external_context)

val compile_main_module_helper :
    bool -> Galax.compiled_program -> Galax_io.input_spec -> Galax.compiled_program * (Galax.compiled_statement list)

(*
  Functions to evaluate internal queries over item lists
*)
module InternalQuery :
  sig
    val default_proc_ctxt : unit -> Processing_context.processing_context
    val defaultcp : unit -> Galax.compiled_program

    val get_string : Physical_value.item list -> Datatypes.xs_string
    val get_string_list : Physical_value.item list -> Datatypes.xs_string list
    val get_int : Physical_value.item list -> int
    val get_float : Physical_value.item list -> Datatypes.xs_float
    val get_item : Physical_value.item list -> Physical_value.item
    val get_item_list : Physical_value.item list -> Physical_value.item list
    val make_from_string : Datatypes.xs_string -> Physical_value.item list
    val make_from_int : int -> Physical_value.item list
    val load_document_item : Galax_io.input_spec -> Physical_value.item
    val load_prolog : Galax_io.input_spec -> Galax.compiled_program
	(* Default compiled program with empty main module *)
    val ccp : Galax.compiled_program

    val eval_to_string :
      (string -> Physical_value.item -> Physical_value.item list) ->
      string -> Physical_value.item -> Datatypes.xs_string
    val eval_to_string_list :
      (string -> Physical_value.item -> Physical_value.item list) ->
      string -> Physical_value.item -> Datatypes.xs_string list
    val eval_to_int :
      (string -> Physical_value.item -> Physical_value.item list) ->
      string -> Physical_value.item -> int
    val eval_to_float :
      (string -> Physical_value.item -> Physical_value.item list) ->
      string -> Physical_value.item -> Datatypes.xs_float
    val eval_to_float_list :
      (string -> Physical_value.item -> Physical_value.item list) ->
      string -> Physical_value.item -> Datatypes.xs_float list
    val eval_to_item :
      (string -> Physical_value.item -> Physical_value.item list) ->
      string -> Physical_value.item -> Physical_value.item
    val eval_to_item_list :
      (string -> Physical_value.item -> Physical_value.item list) ->
      string -> Physical_value.item -> Physical_value.item list
    val eval_expr : string -> Physical_value.item -> Physical_value.item list
  end

(*
  Functions to manipulate graphs
*)
module Graph :
    sig
      type graph_edge = (string * string * string list * float list)
      val load_graph : Physical_value.item (* Document node *) -> string (* graph name *) -> graph_edge list
    end
