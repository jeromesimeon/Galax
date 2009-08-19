(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: galax.ml,v 1.78 2007/10/16 01:25:34 mff Exp $ *)

(* Module: Galax
   Description:
     This module contains the Galax Caml user API.
*)
open Compile_context
open Code_selection_context

open Format

open Error
open Namespace_context

open Xquery_common_ast
open Xquery_ast
open Xquery_core_ast
open Xquery_algebra_ast
open Print_top

open Compiled_program_units
open Procmod_compiler

open Physical_item
open Physical_sequence
open Physical_value_util

open Processing_context
open Monitoring_context
open Procmod_phases
open Monitor

open Streaming_types
(******************)
(* Galapi context *)
(******************)

(* Everything you wanted to know about context, but were afraid to ask *)

type external_context = Procmod_compiler.external_context

(********************************)
(* Module context accessors     *)
(********************************)

let procctxt_of_compiled_program = Compiled_program_units.processing_context_of_compiled_program
let procctxt_of_prepared_program = Compiled_program_units.processing_context_of_compiled_program 
let algebra_context_of_compiled_program = Compiled_program_units.algebra_context_of_compiled_program
let main_module_of_compiled_program = Compiled_program_units.main_module_of_compiled_program

let module_of_compiled_program compiled_program module_uri = 
  if (module_uri = Compiled_program_units.main_module_uri) then 
    let (comp_mod, _) = Compiled_program_units.main_module_of_compiled_program compiled_program in comp_mod
  else Compiled_program_units.module_of_compiled_program compiled_program module_uri

let nsenv_of_main_module = Compiled_program_units.nsenv_of_main_module 
let code_selection_context_of_main_module = Compiled_program_units.code_selection_context_of_main_module

(*************)
(* AST Types *)
(*************)

type compiled_program = Compiled_program_units.compiled_program
type prepared_program = Compiled_program_units.prepared_program
type compiled_library_module = Compiled_program_units.compiled_library_module
type compiled_module = Compiled_program_units.compiled_prolog * Compiled_program_units.compiled_statement list
type compiled_statement = Algebra_type.algop_expr

let compiled_program_of_prepared_program p = p 

(********************************)
(* Document I/O functions       *)
(********************************)

let aux_load_document proc_ctxt gio =
  let aux_load_document' proc_ctxt  = 
    let (dtdopt, xml_stream)= Streaming_parse.open_xml_stream_from_io gio in
    let resolved_xml_stream = Streaming_ops.resolve_xml_stream xml_stream in
    let typed_xml_stream    = Streaming_ops.typed_of_resolved_xml_stream resolved_xml_stream in
    let nodeid_context      = Nodeid_context.default_nodeid_context () in
    let v = Physical_load.load_xml_document_from_typed_stream nodeid_context typed_xml_stream in
    (List.map (fun n -> getNode n) v)
  in
  wrap_monitor proc_ctxt (Document_Toplevel_ParsingLoading_Phase (Parse_io.name_of_input_spec gio)) aux_load_document' proc_ctxt

let poly_serialize proc_ctxt output_spec serialize_fun v =
  let gout = Parse_io.galax_output_from_output_spec output_spec in
  let fmt = Parse_io.formatter_of_galax_output gout in
  if (!Conf.verbose) then
    begin
      Format.fprintf !Conf.xml_formatter "%s" !Conf.xml_header;
      pp_print_flush !Conf.xml_formatter ()
    end;
  (* Serialization call is HERE ! *)
  serialize_fun proc_ctxt fmt v;
  Format.pp_print_newline fmt ();
  if (!Conf.verbose) then
    begin
      Format.fprintf !Conf.xml_formatter "%s" !Conf.xml_footer;
      pp_print_flush !Conf.xml_formatter ()
    end;
  (* Note that a galax_output must be closed, as below *)
  Parse_io.close_galax_output gout 

let aux_serialize proc_ctxt output_spec v =
  begin
    (* Serialization should be part of the processing model and all
    this code should be moved into Procmod *)
    let serialize_datamodel v =
      poly_serialize proc_ctxt output_spec Serialization.fserialize_datamodel v
    in 
    wrap_monitor proc_ctxt Document_Serialization_Phase serialize_datamodel v
  end

let aux_serialize_typed_xml_stream proc_ctxt output_spec v =
  begin
    (* Serialization should be part of the processing model and all
    this code should be moved into Procmod *)
    let serialize_datamodel v =
      poly_serialize proc_ctxt output_spec Serialization.fserialize_typed_xml_stream v
    in 
    wrap_monitor proc_ctxt Document_Serialization_Phase serialize_datamodel v
  end

let aux_serialize_to_string proc_ctxt v =
  let buff = Buffer.create 64 in
  begin
    aux_serialize proc_ctxt (Galax_io.Buffer_Output buff) v;
    wrap_monitor proc_ctxt Document_Serialization_Phase Buffer.contents buff
  end

let default_external_context () = Procmod_compiler.default_external_context ()

let build_external_context proc_ctxt opt_ctxt_item opt_timezone var_val_list =
  Procmod_compiler.create_external_context
    proc_ctxt
    opt_ctxt_item
    opt_timezone
    var_val_list

(*********************************)
(* Monitored Galax API Functions *)
(*********************************)

(* 
   All monitored calls must be wrapped so that if the API function
   raises an error, it is caught, the monitoring is terminated and
   end_monitor_call is called.

*)
let load_document proc_ctxt gio =
  start_monitor_call proc_ctxt Prolog "load_document" ;
  try
    let doc = aux_load_document proc_ctxt gio in
    end_monitor_call proc_ctxt; 
    doc
  with
  | ex -> (end_monitor_call proc_ctxt; raise ex)

let serialize proc_ctxt output_spec v =
  start_monitor_call proc_ctxt Serialization "serialize" ; 
  try
    aux_serialize proc_ctxt output_spec (Cursor.cursor_of_list v);
    end_monitor_call proc_ctxt;
  with 
  | ex -> (end_monitor_call proc_ctxt; raise ex)

let serialize_to_string proc_ctxt v =
  start_monitor_call proc_ctxt Serialization "serialize_to_string" ; 
  try
    let s = aux_serialize_to_string proc_ctxt (Cursor.cursor_of_list v) in
    end_monitor_call proc_ctxt; 
    s
  with 
  | ex -> (end_monitor_call proc_ctxt; raise ex)

let load_standard_library proc_ctxt =
  start_monitor_call proc_ctxt Prolog "load_standard_library" ; 
  try
    let m = Procmod_compiler.compile_standard_library_module proc_ctxt in
    end_monitor_call proc_ctxt;
    m
  with 
  | ex -> (end_monitor_call proc_ctxt; raise ex)

let load_standard_library_no_monitor proc_ctxt =
    Procmod_compiler.compile_standard_library_module proc_ctxt

let import_prolog_only compiled_program ext_ctxt_item gio =
  let proc_ctxt = procctxt_of_compiled_program compiled_program in
  start_monitor_call proc_ctxt Prolog "import_prolog_only" ; 
  try
    let m = Procmod_compiler.compile_prolog ext_ctxt_item compiled_program gio in
    end_monitor_call proc_ctxt;
    m
  with 
  | ex -> (end_monitor_call proc_ctxt; raise ex)

let import_library_module compiled_program gio =
  let proc_ctxt = procctxt_of_compiled_program compiled_program in
  start_monitor_call proc_ctxt Prolog "import_library_module" ; 
  try
    let (prefix, uri, m) = Procmod_compiler.compile_library_module compiled_program gio in
    end_monitor_call proc_ctxt;
    m
  with 
  | ex -> (end_monitor_call proc_ctxt; raise ex)

(* Export a DXQ server module *)
let export_server_module compiled_program gio =
  let proc_ctxt = procctxt_of_compiled_program compiled_program in
  start_monitor_call proc_ctxt Prolog "export_server_module" ; 
  try
    let (prefix, uri, comp_prog) = Procmod_compiler.compile_library_module compiled_program gio in
    let m = Compiled_program_units.module_of_compiled_program comp_prog  uri in 
    comp_prog.compiled_program_main_module <- Some(m, []);  
(*
    let import_server_module = "import module namespace "^prefix^" = \""^uri^"\";" in 
    let comp_prog = compile_prolog false comp_prog (Galax_io.String_Input import_server_module)  in
*)
    end_monitor_call proc_ctxt;
    (prefix, uri, comp_prog)
  with 
  | ex -> (end_monitor_call proc_ctxt; raise ex)

let import_main_module ext_ctxt_item compiled_program gio =
 
(*
  (* Resetting variable counts!! *)
   Don't need to do this, because name generators are associated with modules.
   begin
    let proc_ctxt = procctxt_of_compiled_program compiled_program in
    Processing_context.reset_name_gens proc_ctxt 
  end;
*)
  let proc_ctxt = procctxt_of_compiled_program compiled_program in
  start_monitor_call proc_ctxt Prolog "import_main_module" ; 
  try
    let m = Procmod_compiler.compile_main_module ext_ctxt_item compiled_program gio in 
    end_monitor_call proc_ctxt;
    m
  with
  | ex -> (end_monitor_call proc_ctxt; raise ex)

let prepare_program compiled_program opt_ext_ctxt =
  let proc_ctxt = procctxt_of_compiled_program compiled_program in
  start_monitor_call proc_ctxt Prolog "prepare_program" ; 
  try
    let p = Procmod_compiler.prepare_compiled_program opt_ext_ctxt compiled_program in
    end_monitor_call proc_ctxt;
    p
  with
  | ex -> (end_monitor_call proc_ctxt; raise ex)

let eval_statement compiled_program gio =
  let proc_ctxt = procctxt_of_compiled_program compiled_program in
  start_monitor_call proc_ctxt Statement "eval_statement" ; 
  try
    let (compiled_program', compiled_statement) = Procmod_compiler.compile_statement compiled_program gio in
    let v = Procmod_compiler.execute_compiled_statement compiled_program compiled_statement in
    (**************************************************************************)
    (* NB!! For queries with element construction, evaluation does not        *)
    (* actually occur until the sequence is materialized in list_of_sequence! *)
    (* So list_of_sequence must be included in evaluation phase.              *)
    (**************************************************************************)
    let v' =  wrap_monitor proc_ctxt (PQuery Evaluation_Phase) (fun v -> list_of_sequence (dom_value_of_physical_value v)) v in
    end_monitor_call proc_ctxt ; 
    (v')
    with 
    | ex -> (end_monitor_call proc_ctxt; raise ex)

let eval_compiled_closure_statement compiled_program cs =
  let proc_ctxt = procctxt_of_compiled_program compiled_program in
  start_monitor_call proc_ctxt Statement "eval_compiled_closure_statement" ; 
  try
    let v = Procmod_compiler.execute_compiled_statement compiled_program cs in 
    let pt = 
      match cs.palgop_expr_eval_sig with
      |	None -> raise(Query(Physical_Type_Error("In eval_compiled_closure_statement: Physical type missing\n")))
      |	Some (pop, input_sig, output_phys_type) -> output_phys_type
    in
    (********************************************************************)
    (* NB!! For closure results, which are serialized to wire, we don't *)
    (* explicitly materialize the result.                               *)
    (********************************************************************)
    let v' = wrap_monitor proc_ctxt (PQuery Evaluation_Phase) (fun v -> v) v
    (* let (physop, input_sig, output_type) = cs.palgop_expr_eval_sig in *)
    (* Type check physical value with physical signature here? *)
    in
    end_monitor_call proc_ctxt ; 
    (pt, v')
  with 
  | ex -> (end_monitor_call proc_ctxt; raise ex)
	
let eval_compiled_statement compiled_program cs =
  let proc_ctxt = procctxt_of_compiled_program compiled_program in
  start_monitor_call proc_ctxt Statement "eval_compiled_statement" ; 
  try
    let v = Procmod_compiler.execute_compiled_statement compiled_program cs in 
    (**************************************************************************)
    (* NB!! For queries with element construction, evaluation does not        *)
    (* actually occur until the sequence is materialized in list_of_sequence! *)
    (* So list_of_sequence must be included in evaluation phase.              *)
    (**************************************************************************)
    let v' =  wrap_monitor proc_ctxt (PQuery Evaluation_Phase) (fun v -> list_of_sequence (dom_value_of_physical_value v)) v in
    end_monitor_call proc_ctxt ; 
    v'
  with 
  | ex -> (end_monitor_call proc_ctxt; raise ex)
	
let compile_serialized_optimized_main_module comp_prog input =
  let proc_ctxt = procctxt_of_compiled_program comp_prog in
  start_monitor_call proc_ctxt Statement "compile_serialized_optimized_main_module" ; 
  try
    let comp_ctxt = Compiled_program_units.compile_context_of_main_module comp_prog in
    let parsed_plan, comp_ctxt= 
      Planio_top.parse_logical_algebra_module proc_ctxt comp_ctxt input 
    in
    let m = Procmod_compiler.compile_main_module_from_optimized_logical_plan
      comp_prog (comp_ctxt, parsed_plan) in
    end_monitor_call proc_ctxt ; 
    m
  with 
  | ex -> (end_monitor_call proc_ctxt; raise ex)

let compile_serialized_logical_main_module comp_prog input =
  let proc_ctxt = procctxt_of_compiled_program comp_prog in
  start_monitor_call proc_ctxt Statement "compile_serialized_logical_main_module" ; 
  try
    let comp_ctxt = Compiled_program_units.compile_context_of_main_module comp_prog in
    let parsed_plan, comp_ctxt= 
      Planio_top.parse_logical_algebra_module proc_ctxt comp_ctxt input 
    in
    let m = Procmod_compiler.compile_main_module_from_logical_plan
      comp_prog (comp_ctxt, parsed_plan) in
    end_monitor_call proc_ctxt ; 
    m
  with 
  | ex -> (end_monitor_call proc_ctxt; raise ex)


let compile_serialized_logical_statement comp_prog input =
  let proc_ctxt = procctxt_of_compiled_program comp_prog in
  start_monitor_call proc_ctxt Statement "compile_serialized_statement" ; 
  try
      (* 0. Create a new compiled program in which to evaluate the statement *)
    let comp_prog = Compiled_program_units.copy_compiled_program comp_prog in 
    let comp_ctxt = Compiled_program_units.compile_context_of_main_module comp_prog in
    let parsed_expr = Planio_top.parse_logical_algebra_statement proc_ctxt comp_ctxt input in
    let (comp_mod, _) = main_module_of_compiled_program comp_prog in 
    let (comp_prog', s) = Procmod_compiler.compile_statement_from_logical_plan comp_prog comp_mod (parsed_expr) in
    end_monitor_call proc_ctxt ; 
    (comp_prog', s)
  with 
  | ex -> (end_monitor_call proc_ctxt; raise ex)


let validate_document comp_prog input =
  let proc_ctxt = procctxt_of_compiled_program comp_prog in
  start_monitor_call proc_ctxt Statement "compile_serialized_statement" ; 
  try
	(* find the schema for the main program *)
	let norm_ctxt = norm_context_of_main_module comp_prog in
	let cxschema = Norm_context.cxschema_from_norm_context norm_ctxt in (* ??? is this the right norm_context? *)

	(* create an XML stream from the data model *)
	let document = Cursor.cursor_of_list input in
(*	let resolved_xml_stream = Physical_export.resolved_xml_stream_of_datamodel document in *)
	let typed_xml_stream = Physical_export.typed_xml_stream_of_datamodel document in 
    let resolved_xml_stream = Streaming_ops.erase_xml_stream typed_xml_stream in

	(* validate *)
	let typed_xml_stream = Schema_validation.validate cxschema resolved_xml_stream in

	(* convert the typed XML stream to a typed data model *)
	let nodeid_ctxt = Nodeid_context.default_nodeid_context () in
	let valid = Physical_load.load_xml_document_from_typed_stream nodeid_ctxt typed_xml_stream in
	  end_monitor_call proc_ctxt;
	  valid
  with 
	| ex -> (end_monitor_call proc_ctxt; raise ex)

(************************)
(* Closures             *)
(************************)

let compile_serialized_closure_in_module comp_prog comp_mod input =
  let proc_ctxt = procctxt_of_compiled_program comp_prog in
  start_monitor_call proc_ctxt Statement "compile_serialized_closure" ; 
  try
    begin
      (* 0. Create a new compiled program and module in which to evaluate the statement *)
      (* Is this overkill???? *)
      let comp_prog = Compiled_program_units.copy_compiled_program comp_prog in 
      let comp_mod = Compiled_program_units.copy_compiled_module comp_mod in 

      let comp_ctxt = Compiled_program_units.compile_context_of_module comp_mod in
      let s = match input with Galax_io.String_Input s -> s | _ -> "Nothing..." in 
      Debug.print_dxq_debug ("Before parse_serialized_closure\n"^s^"\n");
      let (env, parsed_expr) = 
	Planio_top.parse_closure proc_ctxt comp_ctxt input 
      in
      Debug.print_dxq_debug ("After parse_serialized_closure\n@?");
    (* From Cs_code_selection_top.single_op_default_code_selection_decl: *)
      let execution_ctxt =  algebra_context_of_compiled_program comp_prog in
      let code_ctxt = code_selection_context_of_module comp_mod in 

    (* Protocol for initializing variable-manager-context and
       tuple-manager-context: 

       1. Set up variable-manager-context ("frame") by mapping variable names
          to frame locations and tuple fields to field locations. 
    *)
      let (var_env, tuple_env) = env in 
      let vnames = List.map fst var_env in 
      let tnames = List.map fst tuple_env in 
      let code_ctxt' = Code_selection_context.enter_closure_context code_ctxt vnames tnames in

    (* 2. _Allocate_ the space for variable frame.  *)
      Code_selection_context.exit_closure_context code_ctxt; 

    (* 3. Populate the frame with values from closure environment *)
      let (code_ctxt', execution_ctxt') = 
	List.fold_left Cs_code_top.extend_var_context (code_ctxt', execution_ctxt) var_env in
      let phys_tuple_type = List.map (Cs_code_top.extend_tuple_context code_ctxt') tuple_env in

    (* 4. Give physical type of INPUT tuple *)
      let ctc' = Code_typing_context.add_input_type 
	  (code_type_context_from_code_selection_context code_ctxt') phys_tuple_type in
      let code_ctxt'' = replace_code_type_context_in_code_selection_context ctc' code_ctxt' in

      Debug.print_dxq_debug ("After loading environment\n@?");

    (* 5. Update the program's algebra (external) context and the module's code selection context *)
      Compiled_program_units.replace_algebra_context_of_compiled_program comp_prog execution_ctxt';
      Compiled_program_units.replace_code_selection_context_of_module comp_mod code_ctxt'';

    (* 5a. The compiled program must have the right module before compiling the statement. *)

    (* 6. Compile the closure *)
      let (comp_prog', s) = Procmod_compiler.compile_statement_from_logical_plan comp_prog comp_mod (parsed_expr) in
      Debug.print_dxq_debug ("After compile_statement_from_logical_plan\n@?");
    (* 7. Should we force a table materialization here for statements that return tuples? *)
      end_monitor_call proc_ctxt ; 
      (comp_prog', s)
    end
   with 
   | ex -> (end_monitor_call proc_ctxt; raise ex)

let compile_serialized_closure comp_prog input =
  let (comp_mod, _) = main_module_of_compiled_program comp_prog in 
  compile_serialized_closure_in_module comp_prog comp_mod input 

let serialize_logical_statement nsenv cs =
  let rs = Planio_top.box_logical_algebra_statement nsenv cs in
  let ts = Streaming_ops.typed_of_resolved_xml_stream rs in
  let ds = Physical_xml_value.dom_value_of_sax_value ts in
  Physical_sequence.list_of_sequence ds

let serialize_logical_module nsenv (comp_mod,cs) =
  let cm = Xquery_algebra_ast_util.fmkalgop_xmodule comp_mod.compiled_prolog_plan cs in
  let rs = Planio_top.box_logical_algebra_module nsenv cm in
  let ts = Streaming_ops.typed_of_resolved_xml_stream rs in
  let ds = Physical_xml_value.dom_value_of_sax_value ts in
  Physical_sequence.list_of_sequence ds

(************************)
(* Streaming evaluation *)
(************************)

(* Note:
     the following are prototype operations that may return the result
     as a cursor or XML stream.
*)


let eval_statement_as_item_cursor compiled_program gio =
let _ = Printf.printf "************ eval statement\n@?" in
  let proc_ctxt = procctxt_of_compiled_program compiled_program in
  start_monitor_call proc_ctxt Statement "eval_statement_as_item_cursor" ; 
  try
    let (compiled_program', compiled_statement) = Procmod_compiler.compile_statement compiled_program gio in
    let v = Procmod_compiler.execute_compiled_statement compiled_program' compiled_statement in
    (**************************************************************************)
    (* NB!! For queries with element construction, evaluation does not        *)
    (* actually occur until the sequence is materialized in list_of_sequence! *)
    (* So list_of_sequence must be included in evaluation phase.              *)
    (**************************************************************************)
    let v' =  wrap_monitor proc_ctxt (PQuery Evaluation_Phase) (fun x -> x) (item_cursor_of_physical_value v) in
    end_monitor_call proc_ctxt ; 
    v'
    with 
    | ex -> (end_monitor_call proc_ctxt; raise ex)

let eval_compiled_statement_as_item_cursor compiled_program cs =
  let proc_ctxt = procctxt_of_compiled_program compiled_program in
  start_monitor_call proc_ctxt Statement "eval_compiled_statement_as_item_cursor" ; 
  try
    let v = Procmod_compiler.execute_compiled_statement compiled_program cs in 
    (**************************************************************************)
    (* NB!! For queries with element construction, evaluation does not        *)
    (* actually occur until the sequence is materialized in list_of_sequence! *)
    (* So list_of_sequence must be included in evaluation phase.              *)
    (**************************************************************************)
    let v' =  wrap_monitor proc_ctxt (PQuery Evaluation_Phase) (fun x -> x) (item_cursor_of_physical_value v) in
    end_monitor_call proc_ctxt ; 
     v'
  with 
  | ex -> (end_monitor_call proc_ctxt; raise ex)

let eval_statement_as_sax compiled_program gio =
  let proc_ctxt = procctxt_of_compiled_program compiled_program in
  start_monitor_call proc_ctxt Statement "eval_statement_as_sax" ; 
  try
    let (compiled_program', compiled_statement) = Procmod_compiler.compile_statement compiled_program gio in
    let v = Procmod_compiler.execute_compiled_statement compiled_program' compiled_statement in
    (**************************************************************************)
    (* NB!! For queries with element construction, evaluation does not        *)
    (* actually occur until the sequence is materialized in list_of_sequence! *)
    (* So list_of_sequence must be included in evaluation phase.              *)
    (**************************************************************************)
    let v' =  wrap_monitor proc_ctxt (PQuery Evaluation_Phase) (fun x -> x) (sax_value_of_physical_value v) in
    end_monitor_call proc_ctxt ; 
    v'
    with 
    | ex -> (end_monitor_call proc_ctxt; raise ex)

let eval_compiled_statement_as_sax compiled_program cs =
  let proc_ctxt = procctxt_of_compiled_program compiled_program in
  start_monitor_call proc_ctxt Statement "eval_compiled_statement_as_sax" ; 
  try
    let v = Procmod_compiler.execute_compiled_statement compiled_program cs in 
    (**************************************************************************)
    (* NB!! For queries with element construction, evaluation does not        *)
    (* actually occur until the sequence is materialized in list_of_sequence! *)
    (* So list_of_sequence must be included in evaluation phase.              *)
    (**************************************************************************)
    let v' =  wrap_monitor proc_ctxt (PQuery Evaluation_Phase) (fun x -> x) (sax_value_of_physical_value v) in
    end_monitor_call proc_ctxt ; 
     v'
  with 
  | ex -> (end_monitor_call proc_ctxt; raise ex)


let serialize_as_item_cursor proc_ctxt output_spec v =
  start_monitor_call proc_ctxt Serialization "serialize_as_item_cursor" ; 
  try
    aux_serialize proc_ctxt output_spec v;
    end_monitor_call proc_ctxt;
  with 
  | ex -> (end_monitor_call proc_ctxt; raise ex)

let serialize_as_sax proc_ctxt output_spec v =
  start_monitor_call proc_ctxt Serialization "serialize_as_sax" ; 
  try
    let typed_stream_value =
      v
    in
    aux_serialize_typed_xml_stream proc_ctxt output_spec typed_stream_value;
    end_monitor_call proc_ctxt;
  with 
  | ex -> (end_monitor_call proc_ctxt; raise ex)

