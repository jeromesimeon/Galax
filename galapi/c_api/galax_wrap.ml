(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: galax_wrap.ml,v 1.36 2007/05/16 15:32:10 mff Exp $ *)

open Error
open Galax

open Dm_atomic
open Dm
open Dm_functions

open Processing_context

type input_source_kind = int
(* 
  In galax.h:
  typedef int input_source_kind ;
  #define File_Input 0
  #define Buffer_Input 1
  #define Http_Input 2
*)

(*********************)
(* Exception wrapper *)
(*********************)

(* Turns an arbitrary Galax error into a simple Caml Failure *)

(* Note:  Converts internal Query exception into generic Caml exception.  
   Attempting to raise the Query exception directly caused problems.  *)

let wrap_exception exn =
  let error_msg =
    Error.bprintf_error_safe "" exn
  in
  raise (Failure error_msg)


(*****************************)
(* Atomic value constructors *)
(*****************************)

let galax_atomicString s =
  try
    to_atomicValue_item ((atomicString s) :> atomicValue)
  with
  | exn ->
      wrap_exception exn
	
let galax_atomicBoolean b =
  try
    to_atomicValue_item(atomicBoolean b)
  with
  | exn ->
      wrap_exception exn

let galax_atomicDecimal i =
  try
    to_atomicValue_item(atomicDecimal (Decimal._decimal_of_int i))
  with
  | exn ->
      wrap_exception exn

let galax_atomicInteger i =
  try
    to_atomicValue_item(atomicInteger (Decimal._integer_of_int i))
  with
  | exn ->
      wrap_exception exn

let galax_atomicFloat d =
  try
    to_atomicValue_item(atomicFloat d)
  with
  | exn ->
      wrap_exception exn

let galax_atomicDouble d =
  try
    to_atomicValue_item(atomicDouble d)
  with
  | exn ->
      wrap_exception exn

let galax_atomicAnyURI b =
  try
    to_atomicValue_item(atomicAnyURI (AnyURI._kinda_uri_of_string b))
  with
  | exn ->
      wrap_exception exn

let galax_atomicQName(nsenv,s) =
  try
    to_atomicValue_item(atomicQName(nsenv,s))
  with
  | exn ->
      wrap_exception exn

let galax_atomicUntyped s =
  try
    to_atomicValue_item(atomicUntyped s)
  with
  | exn ->
      wrap_exception exn


let galax_atomicDateTime s =
  try
    to_atomicValue_item(atomicDateTime s)
  with
  | exn ->
      wrap_exception exn

let galax_atomicDate s =
  try
    to_atomicValue_item(atomicDate s)
  with
  | exn ->
      wrap_exception exn

let galax_atomicTime s =
  try
    to_atomicValue_item(atomicTime s)
  with
  | exn ->
      wrap_exception exn

let galax_atomicDayTimeDuration s =
  try
    to_atomicValue_item(atomicDayTimeDuration s)
  with
  | exn ->
      wrap_exception exn

let galax_atomicYearMonthDuration s =
  try
    to_atomicValue_item(atomicYearMonthDuration s)
  with
  | exn ->
      wrap_exception exn


(*********************)
(* Node constructors *)
(*********************)

let galax_documentNode(s, nl) =
  try
    to_node_item(documentNode (s,List.map get_node nl))
  with
  | exn ->
      wrap_exception exn

let galax_attributeNode(q, s, t) =
  try
    to_node_item(attributeNode(get_atomicValue q, get_atomicValue s, get_atomicValue t))
  with
  | exn ->
      wrap_exception exn

let galax_elementNode(q, al, nl, t) =
  try
    to_node_item(elementNode(get_atomicValue q, List.map get_attribute al, List.map get_node nl, get_atomicValue t))
  with
  | exn ->
      wrap_exception exn

let galax_textNode s =
  try
    to_node_item(textNode(get_atomicValue s))
  with
  | exn ->
      wrap_exception exn

let galax_commentNode s =
  try
    to_node_item(commentNode (get_atomicValue s))
  with
  | exn ->
      wrap_exception exn

let galax_processingInstructionNode (s1,s2) =
  try
    to_node_item(processingInstructionNode (get_atomicValue s1,get_atomicValue s2))
  with
  | exn ->
      wrap_exception exn


(**********************)
(* Accessors on items *)
(**********************)

let galax_string_value item =
  try
    string_value item
  with
  | exn ->
      wrap_exception exn

let galax_item_kind item =
  try 
    item_kind item
  with
  | exn ->
      wrap_exception exn

let galax_get_node item =
  try
    get_node item
  with
  | exn ->
      wrap_exception exn

let galax_get_atomicValue item =
  try
    get_atomicValue item
  with
  | exn ->
      wrap_exception exn

let galax_get_element item =
  try
    get_element item
  with
  | exn ->
      wrap_exception exn

let galax_get_attribute item =
  try
    get_attribute item
  with
  | exn ->
      wrap_exception exn


(**********************)
(* Accessors on nodes *)
(**********************)

let galax_parent node =
  try
    List.map to_node_item (parent (get_node node))
  with
  | exn ->
      wrap_exception exn

let galax_children node =
  try
    List.map to_node_item (children (get_node node))
  with
  | exn ->
      wrap_exception exn

let galax_base_uri node =
  try
    List.map to_atomicValue_item (base_uri (get_node node))
  with
  | exn ->
      wrap_exception exn

let galax_node_kind node =
  try
    node_kind (get_node node)
  with
  | exn ->
      wrap_exception exn

let galax_node_name node =
  try
    List.map to_atomicValue_item (node_name (get_node node))
  with
  | exn ->
      wrap_exception exn

let galax_typed_value node =
  try
    List.map to_atomicValue_item (typed_value (get_node node))
  with
  | exn ->
      wrap_exception exn

let galax_attributes node =
  try
    List.map to_node_item (attributes (get_node node))
  with
  | exn ->
      wrap_exception exn


(********************************)
(* Conversion functions         *)
(********************************)

(* "Down cast" functions *)

let galax_string_of_atomicValue  a =
  try
    string_of_atomicValue (get_atomicValue a)
  with
  | exn ->
      wrap_exception exn

let galax_boolean_of_atomicBoolean b =
  try
    boolean_of_atomicBoolean (get_atomicValue b)
  with
  | exn ->
      wrap_exception exn

let galax_integer_of_atomicInteger i =
  try
    Decimal._int_of_integer(integer_of_atomicInteger (get_atomicValue i))
  with
  | exn ->
      wrap_exception exn

let galax_decimal_of_atomicDecimal d =
  try
    Decimal._int_of_decimal (decimal_of_atomicDecimal (get_atomicValue d))
  with
  | exn ->
      wrap_exception exn

let galax_float_of_atomicFloat f =
  try
    float_of_atomicFloat (get_atomicValue f)
  with
  | exn ->
      wrap_exception exn

let galax_float_of_atomicDouble d =
  try
    float_of_atomicDouble (get_atomicValue d)
  with
  | exn ->
      wrap_exception exn


(********************************)
(* Input / output of operations *)
(********************************)

let galax_load_document proc_ctxt input_kind input =
  try
    let gio = 
      match input_kind with
      | 0 -> (Galax_io.File_Input input)
      | 1 -> (Galax_io.String_Input input)
      | 2 -> (Galax_io.Http_Input input)
      (* Do some exception handling ? For now, switch to default behavior *)
      | _ -> (Galax_io.String_Input input)
    in List.map to_node_item (load_document proc_ctxt gio)
  with
  | exn ->
      wrap_exception exn

let galax_serialize_to_string proc_ctxt v = 
  try
    serialize_to_string proc_ctxt v
  with
  | exn ->
      wrap_exception exn

let galax_serialize_to_file  proc_ctxt string v =
  try
    serialize proc_ctxt (Galax_io.File_Output string) v
  with
  | exn ->
      wrap_exception exn

let galax_serialize_to_stdout proc_ctxt v =
  try
    serialize proc_ctxt (Galax_io.Formatter_Output Format.std_formatter) v
  with
  | exn ->
      wrap_exception exn


(***********************************)
(* Operations on the query context *)
(***********************************)

let galax_default_processing_context () =
  try
    Processing_context.default_processing_context () 
  with
  | exn ->
      wrap_exception exn

let galax_default_external_context () =
  try
    default_external_context ()
  with
  | exn ->
      wrap_exception exn

let galax_load_standard_library ctxt =
  try
    load_standard_library ctxt
  with
  | exn ->
      wrap_exception exn

let galax_import_library_module compiled_program input_kind input =
  try
    let gio = 
      match input_kind with
      | 0 -> (Galax_io.File_Input input)
      | 1 -> (Galax_io.String_Input input)
      | 2 -> (Galax_io.Http_Input input)
      (* Do some exception handling ? For now, switch to default behavior *)
      | _ -> (Galax_io.String_Input input)
    in import_library_module compiled_program gio
  with
  | exn ->
      wrap_exception exn

let galax_import_main_module compiled_program ext_ctxt_item input_kind input =
  try
    let gio = 
      match input_kind with
      | 0 -> (Galax_io.File_Input input)
      | 1 -> (Galax_io.String_Input input)
      | 2 -> (Galax_io.Http_Input input)
      (* Do some exception handling ? For now, switch to default behavior *)
      | _ -> (Galax_io.String_Input input)
    in import_main_module ext_ctxt_item compiled_program gio
  with
  | exn ->
      wrap_exception exn

(********************************)
(* Module context accessors     *)
(********************************)

let galax_nsenv_from_compiled_program ctxt =
  try
    nsenv_of_main_module ctxt
  with
  | exn ->
      wrap_exception exn

(***********************)
(* Querying operations *)
(***********************)

let galax_build_external_context pc ctxt_item_list tz_list var_list val_list = 
  try
    begin
      let opt_ctxt_item = 
	match ctxt_item_list with
	| [] -> None
	| [i] -> Some i
	| _ -> raise (Query(Load_Error ("Context item argument to galax_build_external_context contains more than one item.")))
      in
      let opt_tz = 
	match tz_list with
	| [] -> None
	| [i] -> Some (get_atomicValue i)
	| _ -> raise (Query(Load_Error ("Timezone argument to galax_build_external_context contains more than one item.")))
      in
      let var_val_list = List.combine var_list val_list in
      build_external_context pc opt_ctxt_item opt_tz var_val_list
    end
  with
  | exn ->
      wrap_exception exn

let galax_eval_program compiled_program ext_ctxt = 
  try
    prepare_program compiled_program (Some ext_ctxt)
  with
  | exn ->
      wrap_exception exn

let galax_eval_compiled_statement prepared_program compiled_stmt = 
  try
    eval_compiled_statement prepared_program compiled_stmt
  with
  | exn ->
      wrap_exception exn

let galax_eval_statement prepared_program input_kind input = 
  try
    let gio = 
      match input_kind with
      | 0 -> (Galax_io.File_Input input)
      | 1 -> (Galax_io.String_Input input)
      | 2 -> (Galax_io.Http_Input input)
      (* Do some exception handling ? For now, switch to default behavior *)
      | _ -> (Galax_io.String_Input input)
    in eval_statement prepared_program gio
  with
  | exn ->
      wrap_exception exn

let galax_start_monitor_call pc name = 
  Monitor.start_monitor_external_call pc name

let galax_end_monitor_call pc = 
  Monitor.end_monitor_external_call pc 

let galax_monitor_of_last_call pc =
  try
    Monitor.monitor_of_last_call pc
  with
  | exn ->
      wrap_exception exn

let galax_monitor_of_all_calls pc =
  try
    Monitor.monitor_of_all_calls pc
  with
  | exn ->
      wrap_exception exn

let galax_set_monitor_mem pc b =
  try
    Monitoring_context.set_monitor_mem (pc.monitor_context) b
  with
  | exn ->
      wrap_exception exn

let galax_set_monitor_time pc b =
  try
    Monitoring_context.set_monitor_time (pc.monitor_context) b
  with
  | exn ->
      wrap_exception exn


(* Processing phases *)
let galax_set_normalization_phase pc b = 
  try
    Processing_context.set_normalization_phase pc b 
  with
  | exn ->
      wrap_exception exn

let galax_set_typing_phase pc b = 
  try
    Processing_context.set_typing_phase pc b
  with
  | exn ->
      wrap_exception exn
let galax_set_rewriting_phase pc b = 
  try
    Processing_context.set_rewriting_phase pc b 
  with
  | exn ->
      wrap_exception exn

let galax_set_evaluation_phase pc b = 
  try
    Processing_context.set_evaluation_phase pc b 
  with
  | exn ->
      wrap_exception exn

let galax_set_xml_whitespace pc b = 
  try
    Processing_context.set_xml_whitespace pc b
  with
  | exn ->
      wrap_exception exn

let galax_set_xml_pis_and_comments pc b =
  try
    Processing_context.set_xml_pis_and_comments pc b
  with
  | exn ->
      wrap_exception exn

(*
let galax_set_sbdo_kind pc sbdo_kind = 
  try
    Processing_context.set_sbdo_kind pc sbdo_kind
  with
  | exn ->
      wrap_exception exn
*)

(**
 * Philippe -- following is a bypass for a problem with fetching the hash_variant
 * of Processing_context.SBODI_modes from the C API which makes the API crash. 
 * Instead of passing the CAML hash_variant, passing an int seems to do the job.
 * Still this remains not-to-good coding, we should find a better way.
 * According to the C API:
 *   0 -> Remove
 *   1 -> Preserve
 *   2 -> AdHoc
 *   3 -> Automata
 *)
let galax_set_sbdo_kind pc sbdo_kind_int =
  try
  match sbdo_kind_int with
    | 0 -> Processing_context.set_sbdo_kind pc Processing_context.SBDO_Remove
    | 1 -> Processing_context.set_sbdo_kind pc Processing_context.SBDO_Preserve
    | 2 -> Processing_context.set_sbdo_kind pc Processing_context.SBDO_AdHoc
    | 3 -> Processing_context.set_sbdo_kind pc Processing_context.SBDO_Tidy
    | 4 -> Processing_context.set_sbdo_kind pc Processing_context.SBDO_DupTidy
    | 5 -> Processing_context.set_sbdo_kind pc Processing_context.SBDO_Sloppy
    | _ ->
      (* Do some exception handling ? For now, switch to default behavior *)
      Processing_context.set_sbdo_kind pc Processing_context.SBDO_DupTidy
  with
  | exn ->
      wrap_exception exn

(**
 * Philippe -- same note as on galax_set_sbdo_kind_int
 * According to the C API:
 *   0 -> None
 *   1 -> Weak
 *   2 -> Strong
 **)
let galax_set_typing_kind pc typing_kind_int = 
  try
  match typing_kind_int with
    | 0 -> Processing_context.set_typing_kind pc Processing_context.Typing_None
    | 1 -> Processing_context.set_typing_kind pc Processing_context.Typing_Weak
    | 2 -> Processing_context.set_typing_kind pc Processing_context.Typing_Strong
    | _ ->
      (* Do some exception handling ? For now, switch to default behavior *)
      Processing_context.set_typing_kind pc Processing_context.Typing_None
  with
  | exn ->
      wrap_exception exn      
      
(**
 * Philippe -- same note as on galax_set_sbdo_kind_int
 * According to the C API:
 *   0 -> Serialize_As_Well_Formed
 *   1 -> Serialize_As_XQuery
 *   2 -> Serialize_As_Canonical
 **)
let galax_set_serialization_kind pc ser_kind_int = 
  try
  match ser_kind_int with
    | 0 -> Processing_context.set_serialization_kind pc Processing_context.Serialize_As_Well_Formed
    | 1 -> Processing_context.set_serialization_kind pc Processing_context.Serialize_As_XQuery
    | 2 -> Processing_context.set_serialization_kind pc Processing_context.Serialize_As_Canonical
    | _ ->
      (* Do some exception handling ? For now, switch to default behavior *)
      Processing_context.set_serialization_kind pc Processing_context.Serialize_As_Well_Formed
  with
  | exn ->
      wrap_exception exn      
      
     
let galax_set_projection_kind pc nsk = 
  try
    Processing_context.set_projection_kind pc nsk
  with
  | exn ->
      wrap_exception exn

(**
 * Philippe -- same note as on galax_set_sbdo_kind_int
 * According to the C API:
 *   0 -> Projection_None
 *   1 -> Projection_Standard
 *   2 -> Projection_Optimized
 **)
let galax_set_projection_kind pc prj_int = 
  try
  match prj_int with
    | 0 -> Processing_context.set_projection_kind pc Processing_context.Projection_None
    | 1 -> Processing_context.set_projection_kind pc Processing_context.Projection_Standard
    | 2 -> Processing_context.set_projection_kind pc Processing_context.Projection_Optimized
    | _ ->
      (* Do some exception handling ? For now, switch to default behavior *)
      Processing_context.set_projection_kind pc Processing_context.Projection_None
  with
  | exn ->
      wrap_exception exn      

      
let _ =
  begin
    Callback.register "galax_atomicString" galax_atomicString;
    Callback.register "galax_atomicBoolean" galax_atomicBoolean;
    Callback.register "galax_atomicInteger" galax_atomicInteger;
    Callback.register "galax_atomicDecimal" galax_atomicDecimal;
    Callback.register "galax_atomicFloat" galax_atomicFloat;
    Callback.register "galax_atomicDouble" galax_atomicDouble;
    Callback.register "galax_atomicAnyURI" galax_atomicAnyURI;
    Callback.register "galax_atomicUntyped" galax_atomicUntyped;
    Callback.register "galax_atomicQName" galax_atomicQName;
    Callback.register "galax_atomicDateTime" galax_atomicDateTime;
    Callback.register "galax_atomicDate" galax_atomicDate;
    Callback.register "galax_atomicTime" galax_atomicTime;
    Callback.register "galax_atomicDayTimeDuration" galax_atomicDayTimeDuration;
    Callback.register "galax_atomicYearMonthDuration" galax_atomicYearMonthDuration;
    
    Callback.register "galax_documentNode" galax_documentNode;
    Callback.register "galax_elementNode" galax_elementNode; 
    Callback.register "galax_textNode" galax_textNode;
    Callback.register "galax_attributeNode" galax_attributeNode; 
    Callback.register "galax_commentNode" galax_commentNode;
    Callback.register "galax_processingInstructionNode" galax_processingInstructionNode;
    
    Callback.register "galax_string_value" galax_string_value;
    Callback.register "galax_item_kind" galax_item_kind;
    Callback.register "galax_get_node" galax_get_node;
    Callback.register "galax_get_atomicValue" galax_get_atomicValue;
    Callback.register "galax_get_element" galax_get_element;
    Callback.register "galax_get_attribute" galax_get_attribute;
    
    Callback.register "galax_parent" galax_parent;
    Callback.register "galax_children" galax_children;
    
    Callback.register "galax_base_uri" galax_base_uri;
    Callback.register "galax_node_kind" galax_node_kind;
    Callback.register "galax_node_name" galax_node_name;
    Callback.register "galax_typed_value" galax_typed_value;
    Callback.register "galax_attributes" galax_attributes;
    
    Callback.register "galax_string_of_atomicValue" galax_string_of_atomicValue;
    Callback.register "galax_boolean_of_atomicBoolean" galax_boolean_of_atomicBoolean; 
    Callback.register "galax_integer_of_atomicInteger" galax_integer_of_atomicInteger;
    Callback.register "galax_decimal_of_atomicDecimal" galax_decimal_of_atomicDecimal; 
    Callback.register "galax_float_of_atomicFloat" galax_float_of_atomicFloat; 
    Callback.register "galax_float_of_atomicDouble" galax_float_of_atomicDouble;
    
    Callback.register "galax_load_document" galax_load_document;
    Callback.register "galax_serialize_to_string" galax_serialize_to_string;
    Callback.register "galax_serialize_to_stdout" galax_serialize_to_stdout; 
    Callback.register "galax_serialize_to_file"  galax_serialize_to_file;
    
    Callback.register "galax_default_processing_context" galax_default_processing_context;
    Callback.register "galax_default_external_context" galax_default_external_context;
    Callback.register "galax_load_standard_library" galax_load_standard_library;
    Callback.register "galax_import_library_module" galax_import_library_module;
    Callback.register "galax_import_main_module" galax_import_main_module;
    Callback.register "galax_nsenv_from_compiled_program" galax_nsenv_from_compiled_program;

    Callback.register "galax_build_external_context" galax_build_external_context;
    Callback.register "galax_eval_program" galax_eval_program;
    
    Callback.register "galax_eval_statement" galax_eval_statement;
    Callback.register "galax_eval_compiled_statement" galax_eval_compiled_statement;
    
    (* Monitoring queries *)
    Callback.register "galax_start_monitor_call" galax_start_monitor_call;
    Callback.register "galax_end_monitor_call" galax_end_monitor_call;
    Callback.register "galax_monitor_of_last_call" galax_monitor_of_last_call;
    Callback.register "galax_monitor_of_all_calls" galax_monitor_of_all_calls;
    Callback.register "galax_set_monitor_mem" galax_set_monitor_mem;
    Callback.register "galax_set_monitor_time" galax_set_monitor_time;

    (* Processing phases *)
    Callback.register "galax_set_normalization_phase" galax_set_normalization_phase;
    Callback.register "galax_set_typing_phase"        galax_set_typing_phase;
    Callback.register "galax_set_rewriting_phase"     galax_set_rewriting_phase;
    Callback.register "galax_set_evaluation_phase"    galax_set_evaluation_phase;

    (* Data model options *)
    Callback.register "galax_set_xml_whitespace"       galax_set_xml_whitespace;
    Callback.register "galax_set_xml_pis_and_comments" galax_set_xml_pis_and_comments;

    Callback.register "galax_set_sbdo_kind"           galax_set_sbdo_kind;
    Callback.register "galax_set_typing_kind"         galax_set_typing_kind;
    Callback.register "galax_set_serialization_kind"  galax_set_serialization_kind;
    Callback.register "galax_set_projection_kind"     galax_set_projection_kind;
  end

