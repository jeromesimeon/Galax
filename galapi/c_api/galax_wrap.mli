(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: galax_wrap.mli,v 1.23 2007/05/02 19:31:00 mff Exp $ *)

open Physical_value

(*****************************)
(* Atomic value constructors *)
(*****************************)

val galax_atomicString   : string 	 	   -> item
val galax_atomicBoolean  : bool   	 	   -> item
val galax_atomicDecimal  : int    	 	   -> item
val galax_atomicInteger  : int    	 	   -> item
val galax_atomicFloat  	 : float  	 	   -> item
val galax_atomicDouble 	 : float  	 	   -> item
val galax_atomicAnyURI   : string 		   -> item
val galax_atomicQName    : Namespace_context.nsenv * string -> item
val galax_atomicUntyped  : string                   -> item
val galax_atomicDateTime : string 	 	   -> item
val galax_atomicDate     : string 	 	   -> item
val galax_atomicTime     : string 	 	   -> item
val galax_atomicDayTimeDuration    : string 	   -> item
val galax_atomicYearMonthDuration  : string 	   -> item

(*********************)
(* Node constructors *)
(*********************)

val galax_documentNode  	    : (string * item list) -> item
val galax_elementNode   	    : item * item list * item list * item -> item
val galax_attributeNode 	    : item * item * item -> item
val galax_textNode      	    : item -> item
val galax_commentNode   	    : item -> item
val galax_processingInstructionNode : item * item -> item


(**********************)
(* Accessors on nodes *)
(**********************)

val galax_string_value    : item -> string
val galax_item_kind       : item -> string

(**********************)
(* Accessors on nodes *)
(**********************)

val galax_parent      : item -> item list
val galax_children    : item -> item list
val galax_base_uri    : item -> item list
val galax_node_kind   : item -> string
val galax_node_name   : item -> item list
val galax_typed_value : item -> item list
val galax_attributes  : item -> item list


(************************) 
(* Conversion functions *)
(************************) 

(* "Down cast" functions *)

val galax_string_of_atomicValue    : item -> string 
val galax_boolean_of_atomicBoolean : item -> bool
val galax_integer_of_atomicInteger : item -> int
val galax_decimal_of_atomicDecimal : item -> int
val galax_float_of_atomicFloat     : item -> float
val galax_float_of_atomicDouble    : item -> float


(**************************)
(* Document I/O functions *)
(**************************)

type input_source_kind = int

(* Changed! *)
val galax_load_document       : Processing_context.processing_context -> input_source_kind -> string -> item list
val galax_serialize_to_string : Processing_context.processing_context -> item list -> string
val galax_serialize_to_stdout : Processing_context.processing_context -> item list -> unit
val galax_serialize_to_file   : Processing_context.processing_context -> string -> item list -> unit

(**********************)
(* Evaluation context *)
(**********************)

(* Default processing context *)

val galax_default_processing_context : unit -> Processing_context.processing_context

(* Compile a query program : includes normalization, typing, & optimization *)
val galax_load_standard_library : Processing_context.processing_context -> Galax.compiled_program
val galax_import_library_module : Galax.compiled_program -> input_source_kind -> string -> Galax.compiled_program
val galax_import_main_module    : Galax.compiled_program -> bool -> input_source_kind -> string -> Galax.compiled_program * (Galax.compiled_statement list)

(********************************)
(* Module context accessors     *)
(********************************)

val galax_nsenv_from_compiled_program : Galax.compiled_program -> Namespace_context.nsenv

(* 
   External context: 
   Context item 
   External variables and their values
   External functions 

   Redefining any symbol raises an error
*)
val galax_default_external_context   : unit -> Galax.external_context 
(* Changed! *)
val galax_build_external_context : Processing_context.processing_context -> (item list) -> (item list) -> string list -> (item list) list -> Galax.external_context
val galax_eval_program : Galax.compiled_program -> Galax.external_context -> Galax.prepared_program (* different type? *)

(********************)
(* Query evaluation *)
(********************)

(* Evaluate *)
val galax_eval_statement           : Galax.prepared_program -> input_source_kind -> string -> item list 
val galax_eval_compiled_statement  : Galax.prepared_program -> Galax.compiled_statement -> item list 

(* Query monitoring *)

val galax_set_monitor_mem      : Processing_context.processing_context -> bool -> unit
val galax_set_monitor_time     : Processing_context.processing_context -> bool -> unit
val galax_start_monitor_call   : Processing_context.processing_context -> string -> unit
val galax_end_monitor_call     : Processing_context.processing_context -> unit
val galax_monitor_of_last_call : Processing_context.processing_context -> item list
val galax_monitor_of_all_calls : Processing_context.processing_context -> item list

(* Processing phases *)

val  galax_set_normalization_phase   : Processing_context.processing_context -> bool -> unit
val  galax_set_typing_phase          : Processing_context.processing_context -> bool -> unit
val  galax_set_rewriting_phase       : Processing_context.processing_context -> bool -> unit
val  galax_set_evaluation_phase      : Processing_context.processing_context -> bool -> unit

(* Data model options *)

val  galax_set_xml_whitespace        : Processing_context.processing_context -> bool -> unit
val  galax_set_xml_pis_and_comments  : Processing_context.processing_context -> bool -> unit

val  galax_set_sbdo_kind             : Processing_context.processing_context ->  int -> unit
val  galax_set_typing_kind           : Processing_context.processing_context ->  int -> unit
val  galax_set_serialization_kind    : Processing_context.processing_context ->  int -> unit
val  galax_set_projection_kind       : Processing_context.processing_context ->  int -> unit

