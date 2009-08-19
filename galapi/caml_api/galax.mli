(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: galax.mli,v 1.39 2007/10/16 01:25:34 mff Exp $ *)

(* Module: Galax
   Description:
     This module contains the Galax Caml user API.
*)

open Dm_atomic
open Dm

open Physical_value


(********************)
(* External context *)
(********************)

type external_context

(***********)
(* Queries *)
(***********)

(*   A compiled program contains a processing context, extended
     with global/external variables and their values.

     A prepared program has all global variables (external and
     internal) bound to values.
*)

type compiled_program
type compiled_module
type compiled_library_module
type compiled_statement

type prepared_program

(********************************)
(* Module context accessors     *)
(********************************)

val procctxt_of_compiled_program           : compiled_program -> Processing_context.processing_context
val procctxt_of_prepared_program           : prepared_program -> Processing_context.processing_context
val compiled_program_of_prepared_program   : prepared_program -> compiled_program
val algebra_context_of_compiled_program    : compiled_program -> Execution_context.algebra_context
val main_module_of_compiled_program        : compiled_program -> compiled_module
val module_of_compiled_program             : compiled_program -> string -> compiled_library_module

val nsenv_of_main_module                   : compiled_program -> Namespace_context.nsenv
val code_selection_context_of_main_module  : compiled_program -> Code_selection_context.code_selection_context

(********************************)
(* Document I/O functions       *)
(********************************)

val load_document       : Processing_context.processing_context -> Galax_io.input_spec -> node list
    (* [load_document io] load the XML document from the
       file/string/http uri [io] *)

val serialize           : Processing_context.processing_context -> Galax_io.output_spec -> item list -> unit
    (* [serialize gout x] serializes an XML value using to the given
        galax output *)

val serialize_to_string : Processing_context.processing_context -> item list -> string
    (* [serialize_to_string x] serializes an XML value to a string *)

(*
   External context:
   Context item (optional item)
   Timezone (optional dayTimeDuration) 
   External variables and their values
   External functions 

   Redefining any symbol raises an error
*)

val default_external_context : unit -> external_context
val build_external_context : 
    Processing_context.processing_context -> 
      (item option) -> 
	(atomicDayTimeDuration option) ->
	  (string * item list) list -> external_context

(* 
   Compile a library module or a main module : includes normalization,
   typing, annotation, and logical optimization.

   When importing a main module, must specify whether a context-item
   value will be passed as an external value -- the actual value is
   passed to prepare_program.  

*)
val load_standard_library : Processing_context.processing_context -> compiled_program 

(* A prolog can only be used within the main module. *)
val import_prolog_only    : 
    compiled_program -> 
      bool (* does prolog require external context item? *) -> 
	Galax_io.input_spec ->
	  compiled_program

val import_library_module : compiled_program -> Galax_io.input_spec -> (compiled_program)
val import_main_module    :
    bool (* does module require external context item? *) -> 
      compiled_program -> 
	Galax_io.input_spec -> 
	  compiled_program * (compiled_statement list)

(* Export a DXQ server module : Namespace prefix, URI, compiled program *)
val export_server_module : compiled_program -> Galax_io.input_spec -> (Namespace_names.ncname * string * compiled_program)

(* Prepare a compiled program, so that it can be used during evaluation of a statement: *)
val prepare_program          : compiled_program -> external_context option -> prepared_program

(* Evaluation functions require a prepared program : *)
val eval_statement           : prepared_program -> Galax_io.input_spec -> item list
val eval_compiled_statement  : prepared_program -> compiled_statement -> item list
val eval_compiled_closure_statement  : 
    prepared_program -> compiled_statement -> (Xquery_physical_type_ast.physical_type * Physical_value.physical_value)

(* Compilation interfaces for external plans *)
val compile_serialized_logical_statement     : 
    compiled_program -> Galax_io.input_spec -> compiled_program * compiled_statement
val compile_serialized_logical_main_module   : 
    compiled_program -> Galax_io.input_spec -> compiled_program * compiled_statement list
val compile_serialized_optimized_main_module : 
    compiled_program -> Galax_io.input_spec -> compiled_program * compiled_statement list
val compile_serialized_closure               : 
    compiled_program -> Galax_io.input_spec -> prepared_program * compiled_statement
val compile_serialized_closure_in_module     : 
    compiled_program -> compiled_library_module -> Galax_io.input_spec -> prepared_program * compiled_statement

val serialize_logical_statement : Namespace_context.nsenv -> compiled_statement -> item list
val serialize_logical_module    : Namespace_context.nsenv -> compiled_module -> item list

(* Validation *)
val validate_document : prepared_program -> item list -> item list

(************************)
(* Streaming evaluation *)
(************************)

(* Note:
     the following are prototype operations that may return the result
     as a cursor or XML stream.
*)

val eval_statement_as_item_cursor :
    prepared_program -> Galax_io.input_spec -> item Cursor.cursor
val eval_compiled_statement_as_item_cursor  :
    prepared_program -> compiled_statement -> item Cursor.cursor

val eval_statement_as_sax :
    prepared_program -> Galax_io.input_spec -> Streaming_types.typed_xml_stream
val eval_compiled_statement_as_sax :
    prepared_program -> compiled_statement -> Streaming_types.typed_xml_stream

val serialize_as_item_cursor :
    Processing_context.processing_context -> Galax_io.output_spec -> item Cursor.cursor -> unit
val serialize_as_sax :
    Processing_context.processing_context -> Galax_io.output_spec -> Streaming_types.typed_xml_stream -> unit

