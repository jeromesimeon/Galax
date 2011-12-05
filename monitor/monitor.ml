(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: monitor.ml,v 1.52 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Monitor.
   Description:
     This module is used for monitoring the activity of Galax. 
*)

open Error

open Small_stream_ast

open Monitoring_context
open Processing_context

open Unix

let time_phase_stack = ref []

(* Note:

   - Assumptions: 

   1. Monitor output is reported on a per-function-call basis. 
      Only one Galax API function call is monitored at any one time. 

   2. A function call is either part of the "prolog" or is a "query/statement".

   3. Phases within a function call are mutually exclusive and not
      nested.  

      The one exception is document parsing/loading, which may be
      nested within evaluation (as a call to fn:doc) or may occur at
      the top-level (as a call to Galax.load_document).

      The evaluation phase contains all document parse/load times for
      documents parsed/loaded during evaluation.  The individual
      document parse/load times are reported for reference.  

   4. The DTD of the monitor output is below.
      A processing phase is reported if it has some non-zero statistic.
      A statistic is reported if it is not zero.

      <!ELEMENT monitor (summary, call* )>
      <!ENTITY % stats (allocated_memory?, elapsed_time?, node_count?)>
      <!ELEMENT summary %stats, prolog, statements)>
      <!ELEMENT prolog  %stats>
      <!ELEMENT statements %stats>
      <!ELEMENT call (parsing?, normalization?, rewriting?, cleaning?, factorization?, compile?, optimization?, selection?, evaluation?, serialization?, toplevel-load-document*>
      <!ATTLIST call name CDATA #REQUIRED
                kind (prolog|statements) #REQUIRED>
      <!ELEMENT parsing %stats>
      <!ELEMENT normalization %stats>
      <!ELEMENT rewriting %stats>
      <!ELEMENT cleaning %stats>
      <!ELEMENT factorization %stats>
      <!ELEMENT compile %stats>
      <!ELEMENT optimization %stats>
      <!ELEMENT selection %stats>
      <!ELEMENT evaluation (%stats, load-document* )>
      <!ELEMENT serialization %stats>
      <!ELEMENT toplevel-load-document %stats>
      <!ATTLIST toplevel-load-document name CDATA #REQUIRED>
      <!ELEMENT load-document %stats>
      <!ATTLIST load-document name CDATA #REQUIRED>
      <!ELEMENT allocated_memory #PCDATA> <!-- memory allocated during this call in KB formatted as %.3fK -->
      <!ELEMENT live_memory #PCDATA>      <!-- live memory in heap at end of call KB formatted as %.3fK -->
      <!ELEMENT elapsed_time #PCDATA>     <!-- time formatted as %.6fs or %im%.6fs or %ih%im%.6fs -->
      <!ELEMENT node_count #PCDATA>       <!-- an integer -->

   5. monitor_of_last_call yields well-formed document of element call
      monitor_of_all_calls yields well-formed document of element monitor
*)
(************************)
(* Statistics functions *)
(************************)

let print_stats msg stats = 
  prerr_endline (msg^"{ glx_time = "^(Format.sprintf "%.4f" stats.glx_time)^"}")

let empty_stats_item =
    { glx_total_memory = 0.0;
      glx_live_memory = 0.0;
      glx_time = 0.0 ; 
      glx_node_count = 0 }

let empty_stats_point () = 
  { relative_stats = empty_stats_item ; 
    absolute_stats = empty_stats_item  } 

let get_glx_time () =
  Unix.gettimeofday () 
(*  let process_times = Unix.times() 
  in process_times.tms_utime +. process_times.tms_stime +. process_times.tms_cutime +. process_times.tms_cstime 
*)

let empty_call_summary name ck =
  { call_name = name ; 
    call_kind = ck ; 
    current_phase = None; 
    query_toplevel_phase = empty_stats_point();
    query_parsing_phase = empty_stats_point();
    query_normalization_phase = empty_stats_point();
    query_rewriting_phase = empty_stats_point();
    query_factorization_phase = empty_stats_point();
    query_compile_phase = empty_stats_point();
    query_optimization_phase = empty_stats_point();
    query_selection_phase = empty_stats_point();
    query_evaluation_phase = empty_stats_point();
    query_serialization_phase = empty_stats_point();
    document_toplevel_parsingloading_phase = Hashtbl.create 17;
    document_parsingloading_phase = Hashtbl.create 17;
    external_phase = empty_stats_point(); }

let new_absolute_point mc =
  let glx_absolute_time = 
    if get_monitor_time mc then get_glx_time () 
    else 0.0
  in
  let (glx_absolute_total_memory, glx_absolute_live_memory) = 
    if get_monitor_mem mc then 
      let st =
	Gc.full_major(); 
	Gc.stat () in
      ((st.Gc.major_words) *. float_of_int (Sys.word_size / 8),
       float_of_int (st.Gc.live_words) *. float_of_int (Sys.word_size / 8))
    else (0.0, 0.0)
  in
  let stats = 
  { glx_total_memory = glx_absolute_total_memory;
    glx_live_memory = glx_absolute_live_memory;
    glx_time = glx_absolute_time; 
    glx_node_count = get_node_count() }
  in
  stats

let new_relative_point mc abs1 = 
  let abs2 = new_absolute_point mc in 
  let rel = 
    { glx_total_memory = abs2.glx_total_memory -. abs1.glx_total_memory;
      glx_live_memory = abs2.glx_live_memory; 
      glx_time = abs2.glx_time -. abs1.glx_time ; 
      glx_node_count = abs2.glx_node_count - abs1.glx_node_count } 
  in
  rel

let sum_relative_points rel1 rel2 = 
  { glx_total_memory = rel2.glx_total_memory +. rel1.glx_total_memory;
    glx_live_memory = rel2.glx_live_memory +. rel1.glx_live_memory;
    glx_time = rel2.glx_time +. rel1.glx_time ; 
    glx_node_count = rel2.glx_node_count + rel1.glx_node_count } 

let update_relative_stats mc stats = 
  let rel2 = new_relative_point mc stats.absolute_stats in 
  stats.relative_stats <- sum_relative_points stats.relative_stats rel2
  

(*****************************)
(* Monitored call interface  *)
(*****************************)

let set_current_phase call phase = (call.current_phase <- phase)

let get_current_phase call = 
  match call.current_phase with
  | Some phase -> phase
  | None -> raise (Query(Monitor_Error("In get_current_phase: Not monitoring any phase")))

let start_phase mc phase =
  if (List.length (!time_phase_stack) != 0) then 
    begin
      let _ = List.hd (!time_phase_stack) in
      time_phase_stack := List.tl (!time_phase_stack)
    end;
  let call = 
    begin
      match get_current_call mc with
      |	None -> raise (Query(Monitor_Error("In start_phase: Not monitoring any function call (phase "^(string_of_phase phase)^")")))
      |	Some call -> call
    end
  in
  set_current_phase call (Some(phase));
  match phase with
  | PQuery Parsing_Phase ->
      (call.query_parsing_phase.absolute_stats <- new_absolute_point mc)
  | PQuery Normalization_Phase ->
      (call.query_normalization_phase.absolute_stats <- new_absolute_point mc)
  | PQuery Rewriting_Phase ->
      (call.query_rewriting_phase.absolute_stats  <- new_absolute_point mc ; )
  | PQuery Factorization_Phase ->
      (call.query_factorization_phase.absolute_stats <- new_absolute_point mc ; )
  | PQuery Compile_Phase ->
      (call.query_compile_phase.absolute_stats <- new_absolute_point mc ; )
  | PQuery Optimization_Phase ->
      (call.query_optimization_phase.absolute_stats <- new_absolute_point mc ; )
  | PQuery Selection_Phase ->
      (call.query_selection_phase.absolute_stats <- new_absolute_point mc ; )
  | PQuery Evaluation_Phase ->
      (call.query_evaluation_phase.absolute_stats  <- new_absolute_point mc)
  | Document_Serialization_Phase ->
      (call.query_serialization_phase.absolute_stats <- new_absolute_point mc)
  | Document_Toplevel_ParsingLoading_Phase uri ->
      let doc_table = call.document_toplevel_parsingloading_phase in
      if not(Hashtbl.mem doc_table uri)
      then
	Hashtbl.add doc_table uri (empty_stats_point());
      let previous_stats = Hashtbl.find doc_table uri in
      previous_stats.absolute_stats <- new_absolute_point mc 
  | Document_ParsingLoading_Phase uri ->
      let doc_table = call.document_parsingloading_phase in
      if not(Hashtbl.mem doc_table uri)
      then
	Hashtbl.add doc_table uri (empty_stats_point());
      let previous_stats = Hashtbl.find doc_table uri in
      previous_stats.absolute_stats <- new_absolute_point mc 
  | External_Phase ->
      ((call.external_phase.absolute_stats <- new_absolute_point mc))
  
let end_phase mc y =
  time_phase_stack := (Unix.gettimeofday ()) :: (!time_phase_stack); 
  let call = 
    begin
      match get_current_call mc with
      |	None -> raise (Query(Monitor_Error("In end_phase: Not monitoring any function call")))
      |	Some call -> call
    end
  in
  let phase = call.current_phase in 
  set_current_phase call None; 
  (match phase with
  | Some(PQuery Parsing_Phase) ->
      (update_relative_stats mc call.query_parsing_phase)
  | Some(PQuery Normalization_Phase) ->
      (update_relative_stats mc call.query_normalization_phase)
  | Some(PQuery Rewriting_Phase) ->
      (update_relative_stats mc call.query_rewriting_phase)
  | Some(PQuery Factorization_Phase) ->
      (update_relative_stats mc call.query_factorization_phase)
  | Some(PQuery Compile_Phase) ->
      (update_relative_stats mc call.query_compile_phase)
  | Some(PQuery Optimization_Phase) ->
      (update_relative_stats mc call.query_optimization_phase)
  | Some(PQuery Selection_Phase) ->
      (update_relative_stats mc call.query_selection_phase)
  | Some(PQuery Evaluation_Phase) ->
      (update_relative_stats mc call.query_evaluation_phase)
  | Some(Document_Serialization_Phase) ->
      (update_relative_stats mc call.query_serialization_phase)
  | Some(Document_Toplevel_ParsingLoading_Phase uri) ->
      begin
(*	set_current_phase call (Some PQuery Evaluation_Phase) ;  *)
	let doc_table = call.document_toplevel_parsingloading_phase in
	try 
	  let previous_stats = Hashtbl.find doc_table uri in
	  update_relative_stats mc previous_stats
	with
	| Not_found -> raise(Query(Monitor_Error("Document "^uri^" not found in monitor table.")))
      end
  | Some(Document_ParsingLoading_Phase uri) ->
      begin
        (* Order matters here -- Document_ParsingLoading is always nested within Query Evaluation *)
	set_current_phase call (Some (PQuery Evaluation_Phase));
	let doc_table = call.document_parsingloading_phase in
	try 
	  let previous_stats = Hashtbl.find doc_table uri in
	  update_relative_stats mc previous_stats
	with
	| Not_found -> raise(Query(Monitor_Error("Document "^uri^" not found in monitor table.")))
      end
  | Some(External_Phase) ->
      (update_relative_stats mc call.external_phase)
  | None -> ());
  y

let rec wrap_monitor proc_ctxt phase f x =
  let mc = proc_ctxt.monitor_context in
  if get_monitor_mem mc || get_monitor_time mc then
    begin
      match get_current_call mc with
      |	None -> 
	  start_monitor_call proc_ctxt Statement ("Unknown function calling "^(string_of_phase phase));
	  start_phase mc phase;
	  let y = f x in
	  begin
	    let w = end_phase mc y in 
	    end_monitor_call proc_ctxt;
	    w
	  end
      |	Some call -> 
	  start_phase mc phase;
	  let y = f x in
	  begin
	    end_phase mc y
	  end
    end
  else
    f x

and start_monitor_call proc_ctxt ck name = 
  let mc = proc_ctxt.monitor_context in
  if get_monitor_mem mc || get_monitor_time mc then
    begin
      (match get_current_call mc with 
      | None -> ()
      | Some call -> 
	  begin
	    let phase = call.current_phase in
	    let _ = end_phase mc  in
	    let _ = set_current_phase call phase in
            push_resume_call mc call
	  end);
      set_current_call mc (Some (empty_call_summary name ck))
    end
  else ()

(*	raise (Query(Monitor_Error("start_monitor_call on "^name^": Already monitoring function call "^call.call_name)))*)

and end_monitor_call proc_ctxt = 
  let mc = proc_ctxt.monitor_context in
  if get_monitor_mem mc || get_monitor_time mc then
    begin
      begin
	match get_current_call mc with
	| None -> 
	    begin
	      ()
	    end
	| Some call -> 
	    begin
	      add_completed_call mc call;
	    end
      end ;
      begin
	match (pop_resume_call mc) with
	| None -> 
	    begin
	      set_current_call mc None
	    end
	| Some call -> 
	    begin
	      set_current_call mc (Some call); 
	      start_phase mc (get_current_phase call)
	    end
      end
    end 
  else ()

let start_monitor_external_call proc_ctxt name = 
  let mc = proc_ctxt.monitor_context in
  let ck = Prolog in
  let _ = start_monitor_call proc_ctxt ck name in
  let phase = External_Phase in 
  if get_monitor_mem mc || get_monitor_time mc then
    start_phase mc phase
  else ()


let end_monitor_external_call proc_ctxt = 
  let mc = proc_ctxt.monitor_context in
  if get_monitor_mem mc || get_monitor_time mc then
    end_phase mc ()
  else ();
  end_monitor_call proc_ctxt 

(*****************************)
(* Data model representation *)
(*****************************)

let string_of_time time =
  let total_seconds = time in
  let total_minutes = floor (total_seconds /. 60.0) in
  let total_hours = floor (total_minutes /. 60.0) in
  let relative_seconds_float = mod_float total_seconds 60.0 in
  let relative_minutes_float = mod_float total_minutes 60.0 in
  let seconds = relative_seconds_float in
  let minutes = int_of_float relative_minutes_float in
  let hours = int_of_float total_hours in
    if hours = 0 then
      if minutes = 0 then
	Format.sprintf "PT%.6fS" seconds
      else
	Format.sprintf "PT%iM%.6fS" minutes seconds
    else
      Format.sprintf "PT%iH%iM%.6fS" hours minutes seconds
	
let string_of_memory memory =
  let bytes = memory in
  let kilo_bytes = (bytes /. 1024.0) in
  Format.sprintf "%.3fK" kilo_bytes

let elem_name name = (Namespace_builtin.glx_prefix, Namespace_builtin.glx_uri, name)
let attr_name name content =
  let uqname = (Namespace_builtin.glx_prefix, name) in
  let rsym = Namespace_symbols.rattr_symbol
      (Namespace_builtin.glx_prefix, Namespace_builtin.glx_uri, name)
  in
  (uqname,content,ref false,ref (Some rsym), ref None)

let sum_document_stats key point1 stats_item = sum_relative_points point1.relative_stats stats_item

let elements_of_stats_item nsenv stats_item = 
  [ SElem (elem_name "allocated_memory", Some [], nsenv, [], Dm_atomic_util.default_no_uri_dm, [SText (string_of_memory stats_item.glx_total_memory)], ref None) ]
  @
  [ SElem (elem_name "live_memory", Some [], nsenv, [], Dm_atomic_util.default_no_uri_dm, [SText (string_of_memory stats_item.glx_live_memory)], ref None) ]
  @
  [ SElem (elem_name "elapsed_time", Some [], nsenv, [], Dm_atomic_util.default_no_uri_dm, [SText (string_of_time stats_item.glx_time)], ref None)] 
  @
  [ SElem (elem_name "nodes_created", Some [], nsenv, [], Dm_atomic_util.default_no_uri_dm, [SText (string_of_int stats_item.glx_node_count)], ref None) ]

(*    SElem (elem_name "live_memory",  Some [], [], [SText (string_of_memory stats_item.glx_live_memory)]) ;  *)

let element_of_document_phase nsenv elemname key point doc_elems = 
  (SElem(elem_name elemname, Some [], nsenv, [(attr_name "name" key)], Dm_atomic_util.default_no_uri_dm, elements_of_stats_item nsenv point.relative_stats, ref None)) :: doc_elems

let create_call_content nsenv call = 
  (let elems = elements_of_stats_item nsenv call.query_parsing_phase.relative_stats in
  if not(elems = []) then 
    [SElem(elem_name (string_of_phase (PQuery Parsing_Phase)), Some [], nsenv, [],  Dm_atomic_util.default_no_uri_dm, elems, ref None)]
  else [])
  @
    (let elems = elements_of_stats_item nsenv call.query_normalization_phase.relative_stats in
    if not(elems = []) then 
      [SElem(elem_name (string_of_phase (PQuery Normalization_Phase)), Some [], nsenv, [],  Dm_atomic_util.default_no_uri_dm, elems, ref None)]
    else [])
  @
    (let elems = elements_of_stats_item nsenv call.query_rewriting_phase.relative_stats in
    if not(elems = []) then 
      [SElem(elem_name (string_of_phase (PQuery Rewriting_Phase)), Some [], nsenv, [], Dm_atomic_util.default_no_uri_dm, elems, ref None)]
    else [])
  @
    (let elems = elements_of_stats_item nsenv call.query_factorization_phase.relative_stats in
    if not(elems = []) then 
      [SElem(elem_name (string_of_phase (PQuery Factorization_Phase)), Some [], nsenv, [], Dm_atomic_util.default_no_uri_dm, elems, ref None)]
    else [])
  @
    (let elems = elements_of_stats_item nsenv call.query_compile_phase.relative_stats in
    if not(elems = []) then 
      [SElem(elem_name (string_of_phase (PQuery Compile_Phase)), Some [], nsenv, [], Dm_atomic_util.default_no_uri_dm, elems, ref None)]
    else [])
  @
    (let elems = elements_of_stats_item nsenv call.query_optimization_phase.relative_stats in
    if not(elems = []) then 
      [SElem(elem_name (string_of_phase (PQuery Optimization_Phase)), Some [], nsenv, [], Dm_atomic_util.default_no_uri_dm, elems, ref None)]
    else [])
  @
    (let elems = elements_of_stats_item nsenv call.query_selection_phase.relative_stats in
    if not(elems = []) then 
      [SElem(elem_name (string_of_phase (PQuery Selection_Phase)), Some [], nsenv, [], Dm_atomic_util.default_no_uri_dm, elems, ref None)]
    else [])
  @
(*
    (let doc_stats = Hashtbl.fold sum_document_stats call.document_parsingloading_phase empty_stats_item in
     let elems = elements_of_stats_item nsenv doc_stats in
     let docelems = 
     if not(elems = []) then 
       [SElem(elem_name (string_of_phase (Document_ParsingLoading_Phase "dummy")), [], [], elems)]
     else [])
*)
     let docload_elems = Hashtbl.fold (element_of_document_phase nsenv (string_of_phase (Document_ParsingLoading_Phase "dummy"))) call.document_parsingloading_phase [] in  
     (let elems = elements_of_stats_item nsenv call.query_evaluation_phase.relative_stats in
     if not(elems = []) then 
       [SElem(elem_name (string_of_phase (PQuery Evaluation_Phase)), Some [], nsenv, [], Dm_atomic_util.default_no_uri_dm, elems @ docload_elems, ref None)]
     else [])
  @
    Hashtbl.fold (element_of_document_phase nsenv (string_of_phase (Document_Toplevel_ParsingLoading_Phase "dummy"))) call.document_toplevel_parsingloading_phase [] 
(*
   (let doc_stats = Hashtbl.fold sum_document_stats call.document_toplevel_parsingloading_phase empty_stats_item in
   let elems = elements_of_stats_item nsenv doc_stats in
   if not(elems = []) then 
   [SElem(elem_name (string_of_phase (Document_Toplevel_ParsingLoading_Phase "dummy")), [], [], Dm_atomic_util.default_no_uri_dm, elems)]
   else [])
 *)
  @
    (let elems = elements_of_stats_item nsenv call.query_serialization_phase.relative_stats in
    if not(elems = []) then 
      [SElem(elem_name (string_of_phase Document_Serialization_Phase), Some [], nsenv, [], Dm_atomic_util.default_no_uri_dm, elems, ref None)]
    else [])
  @
    (let elems = elements_of_stats_item nsenv call.external_phase.relative_stats in
    if not(elems = []) then 
      [SElem(elem_name (string_of_phase External_Phase), Some [], nsenv, [], Dm_atomic_util.default_no_uri_dm, elems, ref None)]
    else [])

let element_of_call nsenv call =
  let attrs =
    [(attr_name "name" call.call_name); (attr_name "kind" (string_of_call_kind call.call_kind)) ]
  in
  SElem(elem_name "call", Some [], nsenv, attrs, Dm_atomic_util.default_no_uri_dm, create_call_content nsenv call, ref None)

let top_element_of_call nsenv call =
  let attrs =
    [(attr_name "name" call.call_name); (attr_name "kind" (string_of_call_kind call.call_kind)) ]
  in
  (SDocument(Dm_atomic_util.default_no_uri_dm, [SElem(elem_name "call", Some [(Namespace_builtin.glx_prefix,Namespace_builtin.glx_uri)], nsenv, attrs, Dm_atomic_util.default_no_uri_dm, create_call_content nsenv call, ref None)]))

let monitor_of_last_call nsenv proc_ctxt  = 
  let mc = proc_ctxt.monitor_context in
  if get_monitor_mem mc || get_monitor_time mc
  then
    begin
      (* Cleanly end last call in case it raised an exception and never called end_monitor_call *)
      let _ = end_monitor_call proc_ctxt  in
      let call = 
	match get_last_call mc with
	| Some call -> call
	| None -> raise (Query(Monitor_Error("In monitor_of_last_call: Not monitoring any function call")))
      in
      let nodeid_context = Nodeid_context.default_nodeid_context () in
      let init_nsenv = Namespace_context.default_xml_nsenv in
      let rsexpr = top_element_of_call nsenv call in
      let sexpr = Small_stream_context.sexpr_of_rsexpr init_nsenv rsexpr in
      Physical_load.load_xml_value_from_typed_stream nodeid_context 
	(Streaming_ops.typed_of_resolved_xml_stream (
	 Small_stream_context.resolved_xml_stream_of_sexpr sexpr))
    end
  else
    []

let sum_phase_stats call stats = 
  let toplevel_doc_stats = Hashtbl.fold sum_document_stats call.document_toplevel_parsingloading_phase empty_stats_item in
(*  let nested_doc_stats = Hashtbl.fold sum_document_stats call.document_parsingloading_phase empty_stats_item in *)
  { glx_total_memory = 
      stats.glx_total_memory +.
      call.query_toplevel_phase.relative_stats.glx_total_memory +. 
      call.query_parsing_phase.relative_stats.glx_total_memory +. 
      call.query_normalization_phase.relative_stats.glx_total_memory +. 
      call.query_rewriting_phase.relative_stats.glx_total_memory +. 
      call.query_factorization_phase.relative_stats.glx_total_memory +. 
      call.query_compile_phase.relative_stats.glx_total_memory +. 
      call.query_optimization_phase.relative_stats.glx_total_memory +. 
      call.query_selection_phase.relative_stats.glx_total_memory +. 
      call.query_evaluation_phase.relative_stats.glx_total_memory +. 
      toplevel_doc_stats.glx_total_memory +. 
(*      nested_doc_stats.glx_total_memory +.  *)
      call.query_serialization_phase.relative_stats.glx_total_memory +.
      call.external_phase.relative_stats.glx_total_memory ; 
    (* Live memory is an absolute, fixed value after a phase. *)
    glx_live_memory = 0.0 ;
    glx_time = 
      stats.glx_time +.
      call.query_toplevel_phase.relative_stats.glx_time +. 
      call.query_parsing_phase.relative_stats.glx_time +. 
      call.query_normalization_phase.relative_stats.glx_time +. 
      call.query_rewriting_phase.relative_stats.glx_time +. 
      call.query_factorization_phase.relative_stats.glx_time +. 
      call.query_compile_phase.relative_stats.glx_time +. 
      call.query_optimization_phase.relative_stats.glx_time +. 
      call.query_selection_phase.relative_stats.glx_time +. 
      call.query_evaluation_phase.relative_stats.glx_time +. 
      toplevel_doc_stats.glx_time +. 
(*      nested_doc_stats.glx_time +.  *)
      call.query_serialization_phase.relative_stats.glx_time +. 
      call.external_phase.relative_stats.glx_time ; 
    glx_node_count = 
      stats.glx_node_count + 
      call.query_toplevel_phase.relative_stats.glx_node_count + 
      call.query_parsing_phase.relative_stats.glx_node_count + 
      call.query_normalization_phase.relative_stats.glx_node_count + 
      call.query_rewriting_phase.relative_stats.glx_node_count + 
      call.query_factorization_phase.relative_stats.glx_node_count + 
      call.query_compile_phase.relative_stats.glx_node_count + 
      call.query_optimization_phase.relative_stats.glx_node_count + 
      call.query_selection_phase.relative_stats.glx_node_count + 
      call.query_evaluation_phase.relative_stats.glx_node_count + 
      toplevel_doc_stats.glx_node_count +
(*      nested_doc_stats.glx_node_count + *)
      call.query_serialization_phase.relative_stats.glx_node_count +
      call.external_phase.relative_stats.glx_node_count }

let aux_monitor_of_all_calls nsenv proc_ctxt = 
  let mc = proc_ctxt.monitor_context in
  let sum_call_stats (prolog_stats, stmt_stats, serialization_stats) call  = 
    match call.call_kind with
    | Prolog -> (sum_phase_stats call prolog_stats, stmt_stats, serialization_stats)
    | Statement -> (prolog_stats, sum_phase_stats call stmt_stats, serialization_stats)
    | Serialization -> (prolog_stats, stmt_stats, sum_phase_stats call serialization_stats)
  in
  let call_stack = (get_all_calls mc) in
  let call_elements = List.map (element_of_call nsenv) (List.rev call_stack) in
  let (prolog_stats, stmts_stats, serialization_stats) = List.fold_left sum_call_stats (empty_stats_item, empty_stats_item, empty_stats_item) call_stack  in
  let prolog_element = SElem(elem_name "prolog", Some [], nsenv, [], Dm_atomic_util.default_no_uri_dm, elements_of_stats_item nsenv prolog_stats, ref None) in
  let stmt_element = SElem(elem_name "statements", Some [], nsenv, [], Dm_atomic_util.default_no_uri_dm, elements_of_stats_item nsenv stmts_stats, ref None) in
  let serialization_element = SElem(elem_name "serialization", Some [], nsenv, [], Dm_atomic_util.default_no_uri_dm, elements_of_stats_item nsenv serialization_stats, ref None) in
  let summary_stats = sum_relative_points(sum_relative_points prolog_stats stmts_stats) serialization_stats in 
  let summary_element = SElem(elem_name "summary", Some [], nsenv, [], Dm_atomic_util.default_no_uri_dm, (elements_of_stats_item nsenv summary_stats) @ [prolog_element ; stmt_element; serialization_element ], ref None) in
  let monitor_element = SElem(elem_name "monitor", Some [(Namespace_builtin.glx_prefix,Namespace_builtin.glx_uri)], nsenv, [], Dm_atomic_util.default_no_uri_dm, summary_element :: call_elements, ref None) in
  let rmonitor_doc = SDocument(Dm_atomic_util.default_no_uri_dm, [monitor_element]) in
  let init_nsenv = Namespace_context.default_xml_nsenv in
  let monitor_doc = Small_stream_context.sexpr_of_rsexpr init_nsenv rmonitor_doc in
  Small_stream_context.resolved_xml_stream_of_sexpr monitor_doc

let monitor_of_all_calls nsenv proc_ctxt = 
  let mc = proc_ctxt.monitor_context in
  if get_monitor_mem mc || get_monitor_time mc then
    begin
      (* Cleanly end last call in case it raised an exception and never called end_monitor_call *)
      let _ = end_monitor_call proc_ctxt in
      let rxmlstr = aux_monitor_of_all_calls nsenv proc_ctxt in 
      let nodeid_context = Nodeid_context.default_nodeid_context () in
      Physical_load.load_xml_value_from_typed_stream nodeid_context
	(Streaming_ops.typed_of_resolved_xml_stream rxmlstr)
    end
  else []

let serialize_monitor nsenv proc_ctxt = 
  let mc = proc_ctxt.monitor_context in
  if get_monitor_mem mc || get_monitor_time mc then
        (* Note that a galax_output must be closed, as below *)
    let gout = Parse_io.galax_output_from_output_spec (get_monitor_output proc_ctxt.monitor_context) in
    try
      begin
	let rxmlstr = aux_monitor_of_all_calls nsenv proc_ctxt in
	let fmt = Parse_io.formatter_of_galax_output gout in
	(Serialization.fserialize_resolved_xml_stream proc_ctxt fmt rxmlstr;
	 Format.pp_print_newline fmt ();
	 Parse_io.close_galax_output gout)
      end
    with
    | exn -> (Parse_io.close_galax_output gout; raise exn)
  else ()

(* For some reason, serializing a resolved_xml_stream does not do
   pretty printing...

      let v' = monitor_of_all_calls proc_ctxt in 
      Serialization.fserialize_datamodel proc_ctxt fmt v';
      Format.pp_print_newline fmt ()
*)

