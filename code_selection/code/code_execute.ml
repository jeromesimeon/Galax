(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_execute.ml,v 1.45 2007/10/16 01:25:34 mff Exp $ *)

(* Module: Code_execute
   Description:
     This module contains code to execute query plans.
*)

open Code_selection_context
open Compile_context
open Cs_util
open Cs_util_coercion
open Debug
open Galax_server_util
open Error
open Execution_context
open Namespace_names
open Processing_context
open Physical_item
open Physical_item_util
open Physical_value
open Planio_common
open Serialization
open Streaming_types
open Xquery_algebra_ast
open Xquery_algebra_ast_util
open Xquery_physical_algebra_ast
open Xquery_physical_type_ast


(* This function needs to be somewhere else, but I don't know where yet... *)
let parse_resolved_xml_stream_from_io pc gio =
	  (* 1. Open a SAX cursor on the input document *)
  let (dtd_opt, xml_stream) = Streaming_parse.open_xml_stream_from_io gio  in
	  (* 2. Resolve namespaces *)
  let resolved_xml_stream = Streaming_ops.resolve_xml_stream xml_stream in
  resolved_xml_stream

let user_defined_fun algop = 
    match algop.palgop_expr_name with      
    | AOECallUserDefined ((n, arity), _, _, _, _) -> [n]
    | _ -> []

let livemem = ref 0 
let recmem msg = 
  let st = (Gc.full_major(); Gc.stat ()) in
(*  Printf.eprintf "%s %d\n%!" msg st.Gc.live_words *)
  Printf.eprintf "%s\t\t%d\t%d\n%!" msg (st.Gc.live_words) (st.Gc.live_words - !livemem);
  livemem := st.Gc.live_words

(* 
  for server P close E 

  coerce_nodep coerce_unit_to_sax
*)
let build_boxed_closure code_ctxt algop_remote =
(* Unused -JS
  let comp_ctxt = annotated_compile_context_from_code_selection_context code_ctxt in
*)

  (* Compile-time 1. Identify all the free variables ($x_k) and tuple fields accessed in the remote plan. *)

  let freevars = Xquery_algebra_ast_annotation_util.algop_get_free_variables algop_remote in 
  let tuple_fields = Code_selection_context.get_input_tuple_fields code_ctxt in 
(*  let nsenv = Processing_context.get_external_nsenv proc_ctxt in *)
  let nsenv = Namespace_context.make_empty_nsenv () in 
  let nsenv' = Namespace_context.add_all_ns nsenv [(algebra_prefix, algebra_uri)]  in 

  (* Compile-time 2. Compute transitive closure of all "local"
     functions (F_k), i.e., functions implemented in modules at this
     server, and associated variables called from the remote plan. *)

  (* let (vns *)

  (* Serialize all these functions and cache their serialized rep. *)

  (* We have to serialize this plan each time a remote call is made, thus it must be cached. *)
  let plan_stream = 
    Streaming_ops.typed_of_resolved_xml_stream(Planio_top.box_logical_algebra_statement nsenv algop_remote) in 
  let nodeid_context = Nodeid_context.default_nodeid_context () in
  let plan = Physical_load.load_xml_value_from_typed_stream nodeid_context plan_stream in 
  (* Following function is executed at run-time *)
  (fun alg_ctxt () ->
    let var_value_pairs = 
      List.map (fun v -> 
	let value = Code_selection_context.build_current_retrieve_code code_ctxt v () in
	(v, value)) freevars
    in
    let tuple_field_pairs = 
      List.map (fun f -> 
	let value = (Code_selection_context.build_retrieve_tuple_code code_ctxt f) () in
	(f, value)) tuple_fields
    in
    List.iter (fun (v,value) -> 
      Debug.print_dxq_debug("Variable-value pairs:"^(Namespace_names.prefixed_string_of_rqname(v))^
			    "\n")) var_value_pairs;
    List.iter (fun (v,value) -> 
      Debug.print_dxq_debug("Tuple-fields:"^(Namespace_names.prefixed_string_of_rqname(v))^
			    "\n")) tuple_field_pairs;

    (* We have to pass the _same_ namespace environment when constructing 
       the environment stream and the closure stream. *)

    let env_stream = Planio_top.box_closure_environment nsenv' var_value_pairs tuple_field_pairs in 
    let plan_stream = Streaming_ops.erase_xml_stream(Physical_export.typed_xml_stream_of_datamodel 
						       (Cursor.cursor_of_list plan)) in 
    Streaming_ops.typed_of_resolved_xml_stream(Planio_top.box_closure nsenv' env_stream plan_stream))

let build_default_ceexecute_code async plan_name code_ctxt closure_code unboxing_fn =
  let comp_ctxt = annotated_compile_context_from_code_selection_context code_ctxt in
  let norm_ctxt = Compile_context.norm_context_from_compile_context comp_ctxt in
  let proc_ctxt = Norm_context.processing_context_from_norm_context norm_ctxt in

  let (evaluate_closure_fn, evaluate_remote_query_fn, async_eval, interpret_hostport_string) = 
    match proc_ctxt.dxq_server with 
    | Some dxq_server -> dxq_server
    | None -> raise(Query(DXQ_Error("Distributed XQuery server not defined in Code_execute")))
  in

  let local_host_opt = proc_ctxt.dxq_host in 
  let local_port_opt = proc_ctxt.dxq_port in 
  let _ = match (local_host_opt, local_port_opt) with 
    | (Some local_host, Some local_port) ->
	Debug.print_dxq_debug(Format.sprintf "This server %s:%s\n" local_host (string_of_int local_port))
    | _ -> ()
  in
  
  (* Following function is executed at run-time *)
  (fun alg_ctxt hostportpv ->
    let server_loc = interpret_hostport_string (Physical_util.get_string hostportpv) in
    let (vhost, host, port) = server_loc in
    Debug.print_dxq_debug(Format.sprintf "Execute on %s (%s:%s)\n" vhost host (string_of_int port));
    let execute_locally = 
      match (local_host_opt, local_port_opt) with 
      | (Some local_host, Some local_port) when
	  (host = "localhost" && port = local_port) || 
	  (host = local_host && port = local_port) -> true 
      | _ -> false
    in
    let closure_stream = closure_code alg_ctxt () in 

    let buf' = Buffer.create 8192 in 
    let fmt = Parse_io.formatter_of_galax_output (Parse_io.galax_output_from_output_spec(Galax_io.Buffer_Output buf')) in
    (Serialization.fserialize_typed_xml_stream proc_ctxt fmt closure_stream);
    let closure = (Buffer.contents buf') in 
    Buffer.reset buf';
    Debug.print_dxq_debug("Remote closure:\n"^closure^"\n");
    let exec_body plankind =
      begin
          (* SHIP THE CLOSURE *)
   	Debug.print_dxq_debug("Before evaluate_remote_query host="^host^" port="^(string_of_int port)^"\n");
	let optres = evaluate_remote_query_fn(execute_locally, server_loc, plankind, closure, plan_name) in 
	match optres with
	| Some res -> 
	    begin
          (* CLIENT-SIDE RESULT HANDLING *)
	      Debug.print_dxq_debug("Server.evaluate_remote_query result is:\n"^res^"\n");
	      let boxed_stream =
		parse_resolved_xml_stream_from_io proc_ctxt (Galax_io.String_Input res) in 
	      Debug.print_dxq_debug("After loading typed stream: \n");
	      unboxing_fn code_ctxt proc_ctxt boxed_stream
	    end
	| None -> Cursor.cursor_empty()
      end
    in
    if (async) then 
      let async_body() = ignore(exec_body XQueryPlanAsync) in 
      (async_eval async_body; Cursor.cursor_empty())
    else exec_body XQueryPlan)

let unbox_tree_result phys_type code_ctxt proc_ctxt boxed_stream = 
  let xv = Planio_physical_value.unbox_tree_result proc_ctxt (boxed_stream) in 
  let item_list = Physical_xml_value.item_list_of_xml_value xv in 
  Debug.print_dxq_debug("Loaded tree result is: \n");
  if (Debug.dxq_debug()) then 
    Serialization.fserialize_datamodel proc_ctxt (!Conf.glx_err_formatter) (Cursor.cursor_of_list item_list);
  (Cursor.cursor_of_list item_list)

let unbox_table_result phys_type code_ctxt proc_ctxt boxed_stream = 
  let tuple_cursor = Planio_physical_value.unbox_table_result proc_ctxt phys_type boxed_stream in 
  Debug.print_dxq_debug("*** Loaded result is a tuple cursor\n");
  Code_util_materialize.import_tuple_cursor_as_input_tuple code_ctxt phys_type tuple_cursor

(****************************)
(* Fold over algebraic plan *)
(****************************)
let rewrite_server_qnames (p, uri) se =
(* print_string ("Rewrite server qnames"^(p)^"{"^(uri)^"}\n"); *)
  let rec aux_rewrite_server_qnames se =
    (* Rewrite reference to server QName *)
    (match se.palgop_expr_name with 
    | AOEVar (NSServerPrefix p', NSUri uri', lcname) when (p = p') && (uri = uri') -> 
	se.palgop_expr_name <- AOEVar(NSPrefix p', NSUri uri', lcname)
    | AOECallUserDefined 
	(((NSServerPrefix p', NSUri uri', lcname), arity), arg_types, return_type, upd, recur)
      when p = p' && (uri = uri') -> 
	se.palgop_expr_name <- 
	  AOECallUserDefined (((NSPrefix p', NSUri uri', lcname), arity), arg_types, return_type, upd, recur)
    | _ -> ());
    (* Rewrite independent sub-expressions *)
    (match se.psub_expression with
      NoSub -> ()
    | OneSub s0 -> aux_rewrite_server_qnames s0 
    | TwoSub (s0,s1) -> aux_rewrite_server_qnames s0; aux_rewrite_server_qnames s1
    | ManySub a -> Array.iter aux_rewrite_server_qnames a);
    let hides_prefix = 
      (match se.palgop_expr_name with
      |	AOEServerImplements (p', uri') when p = p' -> true
      |	_ -> false)
    in
    (* Rewrite dependent sub-expressions if this expression does not hide prefix *)
    if (not(hides_prefix)) then 
      (match se.pdep_sub_expression with
      |	NoSub -> ()
      | OneSub s0 ->  aux_rewrite_server_qnames s0
      | TwoSub (s0,s1) ->  aux_rewrite_server_qnames s0; aux_rewrite_server_qnames s1
      | ManySub a -> Array.iter aux_rewrite_server_qnames a)
    else ()
  in aux_rewrite_server_qnames se

let build_server_close_code code_ctxt algop (ncname, uri) = 
  let algop_closure = access_onesub algop.psub_expression in

  if (not(get_in_remote_execute_operator code_ctxt)) then  
    rewrite_server_qnames (ncname, uri) algop_closure
  else ();

  match algop.palgop_expr_eval_sig with
  | Some(POForServerCloseTree, input_phys_type, output_phys_type) ->
      begin
	let fn = build_boxed_closure code_ctxt algop_closure in
	if (not(get_in_remote_execute_operator code_ctxt)) then  
	  begin
	    algop.psub_expression <- NoSub;
	    algop.palgop_expr_eval_sig <- Some(POForServerCloseTree, input_phys_type, output_phys_type)
	  end
	else ();
	(coerce_nodep fn coerce_unit_to_sax), code_ctxt
      end
  | Some(po, _, _) -> 
      raise (Query(Internal_Error("Expected POForServerCloseTree.  Found "^
				  (Print_xquery_physical_type.string_of_physop_expr_name po))))
  | None -> 
      raise (Query(Internal_Error("No physical operator for AOEForServerClose")))

let build_execute_code code_ctxt algop (async, ncname, uri) =

  (* AOEExecute takes two independent sub-expressions: the host-port 
     and the plan to execute remotely *)
  let hostportao, algop_remote = access_twosub algop.psub_expression in

  (* Transform remote expression *IN PLACE* so that all references to
     symbols at server [server]prefix{uri} are rewritten to
     prefix{uri}.  
  *)
  if (not(get_in_remote_execute_operator code_ctxt)) then  
    rewrite_server_qnames (ncname, uri) algop_remote
  else ();
  (* Find name of first user-defined function: Used to "name" distributed plan. *)
  let plan_name = 
    match (Optimization_walker.fold_over_algop user_defined_fun (fun n1 n2 -> n1@n2) [] algop_remote) with 
    | [] -> "Unknown"
    | (prefix,uri,ncname)::l -> ncname
  in

  let closure_code =  build_boxed_closure code_ctxt algop_remote in 
  match algop.palgop_expr_eval_sig with
  | Some(POExecuteTree, TwoInput (in_type1, in_type2), output_phys_type) ->
      begin
	let _ = Xquery_physical_type_ast_util.assert_non_discarded_xml_type output_phys_type in 
	let fn = build_default_ceexecute_code 
	    async plan_name code_ctxt closure_code (unbox_tree_result output_phys_type) in

  (* 

     After we have serialized the plan to execute remotely, the
     independent subexpression of the remote expression is removed
     from the local plan, unless this AOEExecute operator is nested
     within another AOEExecute operator, in which case it is serialized
     during evaluation of the outer-most AOEExecute.

     The effect is that the physical plan for the body of the
     "execute" expression is never executed locally.

     I considered an alternative, in which the remote plan was a
     dependent expression, but this would have required hacking
     Cs_code_selection_expr.default_code_selection to be aware of
     side-effects in this function. -Mary

     Same logic is applied in both cases below.
  *)
	if (not(get_in_remote_execute_operator code_ctxt)) then  
	  begin
	    algop.psub_expression <- OneSub (hostportao);
	    algop.palgop_expr_eval_sig <- Some(POExecuteTree, OneInput(in_type1), output_phys_type)
	  end
	else ();
	(coerce_nodep fn coerce_unary_item_cursor_to_item_cursor), code_ctxt
      end
  | Some(POExecuteTuple, TwoInput (in_type1, in_type2), output_phys_type) -> 
      begin
	(* *)
	let tuple_type = Xquery_physical_type_ast_util.assert_tuple_type output_phys_type in
	let fn = build_default_ceexecute_code 
	    async plan_name code_ctxt closure_code (unbox_table_result tuple_type) in
	if (not(get_in_remote_execute_operator code_ctxt)) then  
	  begin
	    algop.psub_expression <- OneSub (hostportao);
	    algop.palgop_expr_eval_sig <- Some(POExecuteTuple, OneInput(in_type1), output_phys_type)
	  end
	else ();
	(coerce_nodep fn coerce_unary_item_cursor_to_tuple_cursor), code_ctxt
      end
  | Some(po, _, _) -> 
      raise (Query(Internal_Error("Expected POExecuteTuple or POExecuteTree.  Found "^(Print_xquery_physical_type.string_of_physop_expr_name po))))
  | None -> 
      raise (Query(Internal_Error("No physical operator for AOEExecute")))

let build_default_ceserver_implements_code uri code_ctxt coerce_fn =  
  (* Following function is executed at run-time *)
  (fun serverbody eval alg_ctxt hostportav ->
(*    if (hostport implements uri) then  *)
    coerce_fn(eval alg_ctxt serverbody)
(* else raise error *)
      )

(* let-server-implement *)
let build_server_implements_code code_ctxt algop (ncname, uri) =

  (* Physical code selection has already occurred for the body of the Execute operator, 
     i.e., the dependent sub expression 
  *)
  let algop_remote = access_onesub algop.pdep_sub_expression in

  match algop.palgop_expr_eval_sig with
  | Some(POServerImplementsTree, input_sig, output_phys_type) ->
      begin
	let _ = Xquery_physical_type_ast_util.assert_non_discarded_xml_type output_phys_type in 
	let fn = build_default_ceserver_implements_code uri code_ctxt 
	    Physical_value_util.item_cursor_of_physical_value
	in (coerce_onedep fn algop_remote coerce_unary_item_cursor_to_item_cursor), code_ctxt
      end
  | Some(POServerImplementsTuple, input_sig, output_phys_type) -> 
      begin
	let _ = Xquery_physical_type_ast_util.assert_tuple_type output_phys_type in
	let fn = build_default_ceserver_implements_code uri code_ctxt 	    
	    Physical_value_util.tuple_cursor_of_physical_value 
	in (coerce_onedep fn algop_remote coerce_unary_tuple_cursor_to_tuple_cursor), code_ctxt
      end
  | Some(po, _, _) -> 
      raise (Query(Internal_Error("Expected POServerImplements.  Found "^
				  (Print_xquery_physical_type.string_of_physop_expr_name po))))
  | None -> 
      raise (Query(Internal_Error("No physical operator for AOEServerImplements")))


(* 
  eval closure { serialized-plan }

  coerce_nodep coerce_unary_sax_to_sax...but for now return the closure as a string.
*)
let build_default_ceeval_closure_code module_uri code_ctxt =
  let comp_ctxt = annotated_compile_context_from_code_selection_context code_ctxt in
  let norm_ctxt = Compile_context.norm_context_from_compile_context comp_ctxt in
  let proc_ctxt = Norm_context.processing_context_from_norm_context norm_ctxt in

  let (evaluate_closure_fn, evaluate_remote_query_fn, async_eval, interpret_hostport_string) = 
    match proc_ctxt.dxq_server with 
    | Some dxq_server -> dxq_server
    | None -> raise(Query(DXQ_Error("Distributed XQuery server not defined in Code_execute")))
  in

  (fun alg_ctxt closure_stream ->
    (* This is crazy: we are printing the closure, then re-parsing it! *)
    let buf = Buffer.create 8192 in 
    let fmt = Parse_io.formatter_of_galax_output (Parse_io.galax_output_from_output_spec(Galax_io.Buffer_Output buf)) in
    (Serialization.fserialize_typed_xml_stream proc_ctxt fmt closure_stream);
    let closure = (Buffer.contents buf) in 
    Buffer.reset buf;
    
    let (pt, pv) = (evaluate_closure_fn module_uri closure) in
    try 
      Physical_value_util.item_cursor_of_physical_value pv
    with    
    | e -> raise (Query(Internal_Error("In eval box: "^(Error.bprintf_error "Invalid closure result" e))))

(*    Cursor.cursor_of_singleton (_string closure) *)
   )

let build_eval_closure_code module_uri code_ctxt algop =

  match algop.palgop_expr_eval_sig with
  | Some(POEvalClosureTree, input_sig, output_phys_type) ->
      begin
	let _ = Xquery_physical_type_ast_util.assert_non_discarded_xml_type output_phys_type in 
	let fn = build_default_ceeval_closure_code module_uri code_ctxt 
	in (coerce_nodep fn coerce_unary_sax_to_item_cursor), code_ctxt
      end
  | Some(po, _, _) -> 
      raise (Query(Internal_Error("Expected POEvalClosureTree.  Found "^
				  (Print_xquery_physical_type.string_of_physop_expr_name po))))
  | None -> 
      raise (Query(Internal_Error("No physical operator for AOEEvalClosure")))



