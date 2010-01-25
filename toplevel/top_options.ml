(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: top_options.ml,v 1.63 2007/10/26 15:49:33 simeon Exp $ *)

(* Module: Top_options
   Description:
     This module contains code and messages for command-line options.
 *)

open Format

open Processing_context
open Monitoring_context

open Top_config
open Top_util


(*******************)
(* Version message *)
(*******************)

let print_version () =
  begin
    printf "\n";
    printf "  This is %s version %s\n" Conf.system Conf.version;
    printf "  %s\n\n" Conf.copyright;
    printf "  Build reference: %s\n" Conf.build;
    printf "  Status: %s\n" Conf.status;
    printf "  %s library directory: %s\n\n" Conf.system Conf.galax_library
  end


(***********)
(* Options *)
(***********)

(* Misc options *)

let opt_version = "-version"
let arg_version proc_ctxt = fun () -> print_version (); exit 0
let msg_version = "Prints the "^Conf.system^" version"

let opt_verbose = "-verbose"
let arg_verbose proc_ctxt =
  fun onoff -> Conf.verbose := bool_of_onoff(onoff)
let msg_verbose = "Emit descriptive headers in output"

let opt_debug = "-debug"
let arg_debug proc_ctxt = set_debug_kind_arg proc_ctxt
let msg_debug = "Emit debugging [default,dxq,materialization,join,typing], e.g., -debug default,dxq"

let opt_streaming_shebang = "-streaming-shebang"
let arg_streaming_shebang proc_ctxt = fun onoff -> set_streaming_shebang proc_ctxt onoff
let msg_streaming_shebang = "Enable streaming by switching on all the required options"

let opt_scjoin_shebang = "-scjoin-shebang"
let arg_scjoin_shebang proc_ctxt = fun onoff -> set_twigjoin_shebang proc_ctxt onoff
let msg_scjoin_shebang = "Enable staircase join by switching on all the required options"

let opt_twigjoin_shebang = "-twigjoin-shebang"
let arg_twigjoin_shebang proc_ctxt = fun onoff -> set_twigjoin_shebang proc_ctxt onoff
let msg_twigjoin_shebang = "Enable twig joins by switching on all the required options"


let opt_print_plan_shebang = "-print-plan-shebang"
let arg_print_plan_shebang proc_ctxt = fun onoff -> set_print_plan_shebang proc_ctxt onoff
let msg_print_plan_shebang = "Print all plans and queries, from the normalized core to the physical plan"

(* Monitoring options *)

let opt_monitor = "-monitor"
let arg_monitor proc_ctxt =
  fun onoff ->
    begin
      set_monitor_mem proc_ctxt.monitor_context (bool_of_onoff(onoff));
      set_monitor_time proc_ctxt.monitor_context (bool_of_onoff(onoff))
    end
let msg_monitor = "Monitors memory and CPU consumption"

let opt_monitor_mem = "-monitor-mem"
let arg_monitor_mem proc_ctxt =
  fun onoff -> set_monitor_mem proc_ctxt.monitor_context (bool_of_onoff(onoff))
let msg_monitor_mem = "Monitors memory consumption"

let opt_monitor_time = "-monitor-time"
let arg_monitor_time proc_ctxt =
  fun onoff -> set_monitor_time proc_ctxt.monitor_context (bool_of_onoff(onoff))
let msg_monitor_time = "Monitors CPU consumption"

let opt_output_monitor = "-output-monitor"
let arg_output_monitor proc_ctxt =
  fun f ->
    set_monitor_output
      proc_ctxt.monitor_context (Galax_io.Channel_Output (open_out(f)))
let msg_output_monitor = "Output monitor actibity to file" 

(* Character encoding options *)

let opt_unicode_maps = "-unicode-maps"
let arg_unicode_maps proc_ctxt =
  fun s -> Conf.unicode_maps := Some s
let msg_unicode_maps = "Set the location of Unicode character maps"

let opt_internal_encoding = "-internal-encoding"
let arg_internal_encoding proc_ctxt =
  fun s ->
    Encoding.set_internal_encoding
      (Encoding.rep_encoding_of_encoding(Encoding.encoding_of_string s))
let msg_internal_encoding = "Set the internal encoding representation"

let opt_output_encoding = "-output-encoding"
let arg_output_encoding proc_ctxt =
  fun s -> Encoding.set_output_encoding (Encoding.encoding_of_string s)
let msg_output_encoding = "Set the output encoding representation"

(* Input context options *)

let opt_var = "-var"
let arg_var proc_ctxt =
  fun vf ->
    let (v, f) = Gmisc.split_left_on_char vf '='
    in global_vars := (v, f)::(!global_vars)
let msg_var = "var=filename/value, Binds the global variable var to an atomic value (integer, decimal, double, or string)"

let opt_doc = "-doc"
let arg_doc proc_ctxt =
  fun vf ->
    let (v, f) = Gmisc.split_left_on_char vf '='
    in global_docs := (v, f)::(!global_docs)
let msg_doc = "doc=filename/value, Binds the global variable to the document in filename"

let opt_context_item = "-context-item"
let arg_context_item proc_ctxt = fun f -> context_item := (Some f)
let msg_context_item = "Binds the context item to the document in filename or '-' for stdin"

let opt_context = "-context"
let arg_context proc_ctxt = fun f -> context_file := Some f
let msg_context = "Load the context query from file or '-' for stdin"

(* Data model options *)

let opt_xml_whitespace = "-xml-whitespace"
let arg_xml_whitespace proc_ctxt = set_xml_whitespace_arg proc_ctxt
let msg_xml_whitespace = "Preserves whitespace in XML documents"

let opt_xml_pic = "-xml-pic"
let arg_xml_pic proc_ctxt = set_xml_pis_and_comments_arg proc_ctxt
let msg_xml_pic = "Preserves PI's and comments in XML documents"

(* Serialization options *)

let opt_serialize = "-serialize"
let arg_serialize proc_ctxt = set_serialization_kind_arg proc_ctxt
let msg_serialize = "Set serialization kind [standard, wf, canonical or xquery]"

(* Behavior options *)

let opt_language = "-language"
let arg_language proc_ctxt = set_language_kind_arg proc_ctxt
let msg_language = "Language class [xquery10, ultf, xquerybang, xqueryp, dxq]"

let opt_syntax = "-syntax"
let arg_syntax proc_ctxt = set_syntax_kind_arg proc_ctxt
let msg_syntax = "Syntax class [xquery, xqueryx]"

let opt_typing = "-typing"
let arg_typing proc_ctxt = set_typing_kind_arg proc_ctxt
let msg_typing = "Static typing behavior [none, weak (default), or strong]"

(* Processing phases options *)

let opt_normalize = "-normalize"
let arg_normalize proc_ctxt = set_normalization_phase_arg proc_ctxt
let msg_normalize = "Normalization phase"

let opt_static_typing = "-static-typing"
let arg_static_typing proc_ctxt = set_typing_phase_arg proc_ctxt
let msg_static_typing = "Static typing phase [off/on(default)]"

let opt_rewriting = "-rewriting"
let arg_rewriting proc_ctxt = set_rewriting_phase_arg proc_ctxt
let msg_rewriting = "Rewriting phase"

let opt_factorization = "-factorization"
let arg_factorization proc_ctxt = set_factorization_phase_arg proc_ctxt
let msg_factorization = "Factorization phase"

let opt_optimization = "-optimization"
let arg_optimization proc_ctxt = set_optimization_phase_arg proc_ctxt
let msg_optimization = "Optimization phase"

let opt_code_selection = "-code-selection"
let arg_code_selection proc_ctxt = set_code_selection_phase_arg proc_ctxt
let msg_code_selection = "Code selection phase"

let opt_dynamic = "-dynamic"
let arg_dynamic proc_ctxt =
  fun onoff ->
    begin
      set_evaluation_phase_arg proc_ctxt onoff;
      Conf.print_xml := bool_of_onoff(onoff)
    end
let msg_dynamic = "Evaluation phase"

let opt_execute = "-execute"
let arg_execute proc_ctxt =
  fun execute_kind ->
    set_execute_kind_arg proc_ctxt execute_kind
let msg_execute = "Set the expression kind [standard/normalized/logical/physical]"


(* Printing options *)

let opt_print_error = "-print-error-kind"
let arg_print_error proc_ctxt = fun error_kind -> set_error_arg proc_ctxt error_kind
let msg_print_error = "Set the kind of error printing [standard/code]"

let opt_print_warnings = "-print-warnings"
let arg_print_warnings proc_ctxt = fun onoff -> Conf.warning := bool_of_onoff(onoff)
let msg_print_warnings = "Set warnings [on/off]"

let opt_print_plan = "-print-plan-kind"
let arg_print_plan proc_ctxt = fun plan_kind -> set_plan_arg proc_ctxt plan_kind
let msg_print_plan = "Set the kind of plan printing [standard/xml]"

let opt_output_err = "-output-err"
let arg_output_err proc_ctxt =
  fun f -> init_output_refs f Conf.glx_stderr Conf.glx_err_formatter
let msg_output_err = "Redirect error output"

let opt_print_xml = "-print-xml"
let arg_print_xml proc_ctxt = fun onoff -> Conf.print_xml := bool_of_onoff(onoff)
let msg_print_xml = "Print XML result"

let opt_output_xml = "-output-xml"
let arg_output_xml proc_ctxt =
  fun f -> init_output_refs f Conf.xml_output Conf.xml_formatter
let msg_output_xml = "Output XML to result file"

let opt_print_type = "-print-type"
let arg_print_type proc_ctxt = fun onoff -> Conf.print_type := bool_of_onoff(onoff)
let msg_print_type = "Print type of expression"

let opt_output_type = "-output-type"
let arg_output_type proc_ctxt =
  fun f -> init_output_refs f Conf.type_output Conf.type_formatter
let msg_output_type = "Output type to file"

let opt_print_expr = "-print-expr"
let arg_print_expr proc_ctxt = fun onoff -> Conf.print_expr := bool_of_onoff(onoff)
let msg_print_expr = "Print input expression"

let opt_output_expr = "-output-expr"
let arg_output_expr proc_ctxt =
  fun f -> init_output_refs f Conf.expr_output Conf.expr_formatter
let msg_output_expr = "Output expr to file"

let opt_print_norm_expr = "-print-normalized-expr"
let arg_print_norm_expr proc_ctxt =
  fun onoff -> Conf.print_core_expr := bool_of_onoff(onoff)
let msg_print_norm_expr = "Print normalized expression"

let opt_output_norm_expr = "-output-normalized-expr"
let arg_output_norm_expr proc_ctxt =
  fun f -> init_output_refs f Conf.core_expr_output Conf.core_expr_formatter
let msg_output_norm_expr = "Output normalized expression to file" 

let opt_print_rewr_expr = "-print-rewritten-expr"
let arg_print_rewr_expr proc_ctxt =
  fun onoff -> Conf.print_optimized_expr := bool_of_onoff(onoff)
let msg_print_rewr_expr = "Print optimized expression"

let opt_output_rewr_expr = "-output-rewritten-expr"
let arg_output_rewr_expr proc_ctxt =
  fun f -> init_output_refs f Conf.optimized_expr_output Conf.optimized_expr_formatter
let msg_output_rewr_expr = "Output optimized expression to file."

let opt_print_fact_expr = "-print-factorized-expr"
let arg_print_fact_expr proc_ctxt =
  fun onoff -> Conf.print_factorized_expr := bool_of_onoff(onoff)
let msg_print_fact_expr = "Print optimized expression"

let opt_output_fact_expr = "-output-factorized-expr"
let arg_output_fact_expr proc_ctxt =
  fun f ->
    init_output_refs f Conf.factorized_expr_output Conf.factorized_expr_formatter
let msg_output_fact_expr = "Output optimized expression to file"

let opt_print_global = "-print-global"
let arg_print_global proc_ctxt =
  fun onoff -> Conf.print_global := bool_of_onoff(onoff)
let msg_print_global = "Global printing [on/off]"

let opt_print_annotations = "-print-annotations"
let arg_print_annotations proc_ctxt =
  fun onoff -> Conf.print_annotations := bool_of_onoff(onoff)
let msg_print_annotations = "Print expression annotations"

let opt_print_prolog = "-print-prolog"
let arg_print_prolog proc_ctxt =
  fun onoff -> Conf.print_prolog := bool_of_onoff(onoff)
let msg_print_prolog = "Print the prolog as well"

let opt_output_dfgraph = "-output-dfgraph"
let arg_output_dfgraph proc_ctxt =
  fun f ->
    begin
      Conf.print_dfgraph := true;
      init_output_refs f Conf.dfgraph_output Conf.dfgraph_formatter
    end
let msg_output_dfgraph = "Output XQuery core data flow graph in dot format to file"

let opt_genresults = "-generate-results"
let arg_genresults proc_ctxt =
  fun () ->
    begin
      Conf.genresults := true
    end
let msg_genresults = "Generate Expected Results"

let opt_output_all = "-output-all"
let arg_output_all proc_ctxt =
  fun f ->
    begin
      output_all := true;
      init_output_refs f Conf.xml_output Conf.xml_formatter; 
      Conf.type_formatter := !Conf.xml_formatter;
      Conf.expr_formatter := !Conf.xml_formatter;
      Conf.core_expr_formatter := !Conf.xml_formatter;
      Conf.optimized_expr_formatter := !Conf.xml_formatter;
      Conf.factorized_expr_formatter := !Conf.xml_formatter
    end
let msg_output_all = "Output everything to file"

let opt_print_log_plan = "-print-logical-plan"
let arg_print_log_plan proc_ctxt =
  fun onoff -> Conf.print_logical_algebra := bool_of_onoff(onoff)
let msg_print_log_plan = "Print logical plan"

let opt_output_log_plan = "-output-logical-plan"
let arg_output_log_plan proc_ctxt =
  fun f ->
    begin
      Conf.print_logical_algebra := true;
      init_output_refs f Conf.logical_algebra_output Conf.logical_algebra_formatter
    end
let msg_output_log_plan = "Print logical plan to file"

let opt_print_log_rewrites = "-print-logical-rewrites"
let arg_print_log_rewrites proc_ctxt =
  fun onoff -> Conf.print_algebra_optimization_rewrite := bool_of_onoff(onoff)
let msg_print_log_rewrites = "Print Logical Rewrites in order of application to file"

let opt_output_log_rewrites = "-output-logical-rewrites"
let arg_output_log_rewrites proc_ctxt =
  fun f ->
    begin
      Conf.print_algebra_optimization_rewrite := true;
      init_output_refs f Conf.algebra_optimization_rewrite_output Conf.algebra_optimization_rewrite_formatter
    end
let msg_output_log_rewrites = "Print Logical Rewrites in order of application to file"


let opt_print_opt_plan = "-print-optimized-plan"
let arg_print_opt_plan proc_ctxt =
  fun onoff -> Conf.print_optimized_algebra := bool_of_onoff(onoff)
let msg_print_opt_plan = "Print optimized plan"

let opt_output_opt_plan = "-output-optimized-plan"
let arg_output_opt_plan proc_cxt =
  fun f ->
    begin
      Conf.print_optimized_algebra := true;
      init_output_refs f Conf.optimized_algebra_output Conf.optimized_algebra_formatter
    end
let msg_output_opt_plan = "Print optimized plan to file"

let opt_print_phys_plan = "-print-physical-plan"
let arg_print_phys_plan proc_ctxt =
  fun onoff -> Conf.print_physical_algebra := bool_of_onoff(onoff)
let msg_print_phys_plan = "Print physical plan"

let opt_output_phys_plan = "-output-physical-plan"
let arg_output_phys_plan proc_ctxt =
  fun f ->
    begin
      Conf.print_physical_algebra := true;
      init_output_refs f Conf.physical_algebra_output Conf.physical_algebra_formatter
    end
let msg_output_phys_plan = "Print physical plan to file"

let opt_print_comp_annot = "-print-compile-annotations"
let arg_print_comp_annot proc_ctxt =
  fun onoff -> Conf.bPrinting_comp_annotations := bool_of_onoff(onoff)
let msg_print_comp_annot = "Print compile annotations (off by default)"

let opt_print_materialize = "-print-materialize"
let arg_print_materialize proc_ctxt =
  fun onoff -> Conf.print_materialize := bool_of_onoff(onoff)
let msg_print_materialize = "Prints whenever materialization occurs"

(* Optimization options *)

let opt_aggressive = "-aggressive"
let arg_aggressive proc_ctxt =
  fun onoff -> Conf.aggressive_sbdo_remove := bool_of_onoff(onoff)
let msg_aggressive = "Aggressive removal of SBDO calls"

let opt_treejoin_log = "-treejoin-log"
let arg_treejoin_log proc_ctxt =
  set_treejoin_log_arg proc_ctxt
let msg_treejoin_log = "Control logical treejoin ops [default, treejoin, twig]"

let opt_treejoin_phys = "-treejoin-phys"
let arg_treejoin_phys proc_ctxt =
  set_treejoin_phys_arg proc_ctxt
let msg_treejoin_phys = "Control physical treejoin evaluation [nestedloop, scjoin, twigjoin, stream]"

let opt_streaming = "-streaming"
let arg_streaming proc_ctxt = set_streaming_arg proc_ctxt 
let msg_streaming = "Turn on/off streaming"

let opt_infer_independence = "-infer-independence"
let arg_infer_indepedence proc_ctxt = set_infer_indepedence_arg proc_ctxt 
let msg_infer_indepedence = "Turn on/off indepedence analysis for algebraic optimizations"

let opt_dxq = "-dxq"
let arg_dxq proc_ctxt = set_dxq_arg proc_ctxt 
let msg_dxq = "Turn on/off Distributed XQuery optimizations"

let opt_embed_xqueryx = "-embed-xqueryx"
let arg_embed_xqueryx proc_ctxt = set_embed_xqueryx_arg proc_ctxt 
let msg_embed_xqueryx = "Turn on/off embedding of XQueryX in queries"

let opt_buffer_chunks = "-buffer-chunks"
let arg_buffer_chunks proc_ctxt =
  (fun s -> Conf.buffer_chunks := (buffer_chunks_of_arg s))
let msg_buffer_chunks = "Number of buffer chunks for streaming XPath evaluation [int]."

let opt_buffer_csize = "-buffer-csize"
let arg_buffer_csize proc_ctxt =
  (fun s -> Conf.buffer_csize := (buffer_csize_of_arg s))
let msg_buffer_csize = "Size of a buffer chunk for streaming XPath evaluation [int]."

let opt_buffer_inc = "-buffer-inc"
let arg_buffer_inc proc_ctxt =
  (fun s -> Conf.buffer_inc := (buffer_inc_of_arg s))
let msg_buffer_inc = "Increment of buffer chunks for streaming XPath evaluation [int]."

let opt_nested_loop_join = "-nested-loop-join"
let arg_nested_loop_join proc_ctxt =
  fun onoff -> Conf.nested_loop_join := bool_of_onoff(onoff)
let msg_nested_loop_join = "Turns off sort/hash joins and uses only nested-loop joins"

let opt_sbdo = "-sbdo"
let arg_sbdo proc_ctxt =
  set_sbdo_kind_arg proc_ctxt
let msg_sbdo = "Document-order/duplicate-elimination optimization behavior [remove, preserve, adhoc, tidy, sloppy, or duptidy (new)]"

let opt_inline = "-inline-functions"
let arg_inline proc_ctxt =
  set_inline_functions_arg proc_ctxt
let msg_inline = "Inline function calls [on/off] (default on)"

let opt_inline_var = "-inline-variables"
let arg_inline_var proc_ctxt =
  set_inline_variables_arg proc_ctxt
let msg_inline_var = "Inline variables [on/off] (default off)"

let opt_force_materialized = "-force-materialized"
let arg_force_materialized proc_ctxt =
  fun onoff -> Conf.force_materialized_variables := bool_of_onoff(onoff)
let msg_force_materialized = "Force materialization of values in variables [on/off]"

let opt_materialize_tables = "-materialize-tables"
let arg_materialize_tables proc_ctxt = set_materialize_tables_kind_arg proc_ctxt
let msg_materialize_tables = "Materialize-tables option [always, analysis, never]"

let opt_allow_streamed_tuple_fields = "-allow-streamed-tuple-fields"
let arg_allow_streamed_tuple_fields proc_ctxt =
  fun onoff -> Conf.allow_streamed_tuple_fields := bool_of_onoff(onoff)
let msg_allow_streamed_tuple_fields = "Allow XML token stream values in tuple fields [on/off]"

let opt_descendant_hack = "-shredded-descendant-hack"
let arg_descendant_hack proc_ctxt = 
  fun onoff -> Conf.new_descendant_style := bool_of_onoff(onoff)
let msg_descendant_hack = "Allow preorder descendant hack"

(* galax-project options *)

let opt_projection = "-projection"
let arg_projection proc_ctxt = set_projection_kind_arg proc_ctxt
let msg_projection = "Document projection behavior [none, standard, or optimized]"

let opt_output_projection = "-output-projection"
let arg_output_projection proc_ctxt =
  fun f ->
    begin
      Conf.print_projection := true;
      init_output_refs f Conf.projection_output Conf.projection_formatter
    end
let msg_output_projection = "Output the projections paths to a file"

let opt_output_projected = "-output-projection"
let arg_output_projected proc_ctxt =
  fun f ->
    begin
      Conf.print_projected_file := true;
      init_output_refs f Conf.projected_file_output Conf.projected_file_formatter
    end
let msg_output_projected = "Output the projected documents to files; this will, for each document identified during analysis, create a new file in the same location, adding " ^ Conf.projection_suffix ^ " to the former filename"

(* galax-parse options *)

let opt_dtd = "-dtd"
let arg_dtd proc_ctxt = dtd
let msg_dtd = "Validate against an embedded DTD"

let opt_diff = "-diff"
let arg_diff proc_ctxt = diff
let msg_diff = "XML diff option"

let opt_pxp = "-pxp"
let arg_pxp proc_ctxt =
  fun onoff -> pxp := bool_of_onoff(onoff) 
let msg_pxp = "Enable PXP parsing only [on]"

let opt_stream = "-stream"
let arg_stream proc_ctxt =
  fun onoff -> stream := bool_of_onoff(onoff) 
let msg_stream = "Enable "^Conf.system^" parsing only [on]"

let opt_resolve = "-resolve"
let arg_resolve proc_ctxt =
  fun onoff -> resolve := bool_of_onoff(onoff) 
let msg_resolve = "Enable namespace resolution of XML stream [on]"

let opt_prefix = "-prefix"
let arg_prefix proc_ctxt =
  fun onoff -> prefix := bool_of_onoff(onoff) 
let msg_prefix = "Enable URI to prefix of XML stream (Inverse of -resolve) [off]"

let opt_annotate = "-annotate"
let arg_annotate proc_ctxt =
  fun onoff -> annotate := bool_of_onoff(onoff) 
let msg_annotate = "Enable type annotation of XML stream [off]"

let opt_erase = "-erase"
let arg_erase proc_ctxt =
  fun onoff -> erase := bool_of_onoff(onoff) 
let msg_erase = "Enable type erasure of XML stream (Inverse of -annotate) [off]"

let opt_load = "-load"
let arg_load proc_ctxt =
  fun onoff -> load := bool_of_onoff(onoff) 
let msg_load = "Enable materialization of loaded document [off]"

let opt_export = "-export"
let arg_export proc_ctxt =
  fun onoff -> export := bool_of_onoff(onoff) 
let msg_export = "Export the document back as a stream [off]"

(* Meta-options *)

let opt_dm = "-dm"
let arg_dm proc_ctxt =
  fun onoff -> 
    (arg_pxp proc_ctxt onoff; 
     arg_stream proc_ctxt onoff; 
     arg_resolve proc_ctxt onoff; 
     arg_annotate proc_ctxt onoff; 
     arg_load proc_ctxt onoff)
let msg_dm = "Enable all phases through loading [off]"

let opt_validate = "-validate"
let arg_validate proc_ctxt = 
  fun onoff -> 
    (arg_pxp proc_ctxt onoff; 
     arg_stream proc_ctxt onoff; 
     arg_resolve proc_ctxt onoff; 
     arg_annotate proc_ctxt onoff; 
     validation := bool_of_onoff(onoff))
let msg_validate = "Enable all phases through validation [off]"

let opt_xmlschema = "-xmlschema"
let arg_xmlschema proc_ctxt = (fun f -> schemafile := Some f)
let msg_xmlschema = "Schema for validation"

(* Daemon Options *)

let opt_dxq_port = "-port"
let arg_dxq_port proc_ctxt =
  fun p -> set_dxq_host_port proc_ctxt (proc_ctxt.dxq_host) (Some p)
let msg_dxq_port = Conf.system^" Daemon port"

let opt_dxq_dir = "-d"
let arg_dxq_dir proc_ctxt =
  fun d -> set_dxq_source proc_ctxt (RemoteExecution d)
let msg_dxq_dir = "<d> Look for host.xq file in directory <d>"

let opt_dxq_sim = "-s"
let arg_dxq_sim proc_ctxt =
  fun d -> set_dxq_source proc_ctxt (LocalSimulation d)
let msg_dxq_sim = "<d> Simulate directory <d> on localhost"

let opt_dxq_topology = "-t"
let arg_dxq_topology proc_ctxt =
  fun d -> set_dxq_topology proc_ctxt d
let msg_dxq_topology = "<f> Load network node, pair-wise latencies from <f>"

let opt_dxq_drop_msgs = "-drop"
let arg_dxq_drop_msgs proc_ctxt =
  fun d -> set_dxq_drop_msgs proc_ctxt (bool_of_onoff d)
let msg_dxq_drop_msgs = "Do not report all messages to GUI; Report only state changes."

let opt_zerod_port = "-zerodport"
let arg_zerod_port proc_ctxt =
  fun p -> set_zerod_host_port proc_ctxt (proc_ctxt.zerod_host) (Some p)
let msg_zerod_port = "zerod proxy port"

let opt_xqueryx_batch = "-batch"
let arg_xqueryx_batch proc_ctxt = fun () -> Conf.batch_xqueryx := true
let msg_xqueryx_batch = "XQueryX batch processing"

(* WSDL options *)

let opt_wsdl_client_name = "-wsdl-client-with-name"
let arg_wsdl_client_name proc_ctxt = fun xq -> Conf.generate_client := true; Conf.client_filename := xq
let msg_wsdl_client_name = "Sets the name of the output .xq server stub"

let opt_wsdl_client = "-wsdl-client"
let arg_wsdl_client proc_ctxt = fun () -> Conf.generate_client := true
let msg_wsdl_client = "Generate an .xq client file"

let opt_wsdl_server_name = "-wsdl-server-with-name"
let arg_wsdl_server_name proc_ctxt = fun xq -> Conf.generate_server := true ; Conf.server_filename := xq
let msg_wsdl_server_name = "Sets the name of the output .xq server stub"

let opt_wsdl_server = "-wsdl-server"
let arg_wsdl_server proc_ctxt = fun () -> Conf.generate_server := true
let msg_wsdl_server = "Generate an .xq server stub"

let opt_wsdl_impl_name = "-wsdl-impl-with-name"
let arg_wsdl_impl_name proc_ctxt = fun xq -> Conf.impl_filename := Some xq
let msg_wsdl_impl_name = "Sets the name of the server implementation module"

let opt_wsdl_namespace = "-wsdl-namespace"
let arg_wsdl_namespace proc_ctxt = fun nms -> Conf.service_namespace := nms
let msg_wsdl_namespace = "Sets the namespace prefix in the XQuery source to be generated"

let opt_wsdl_service = "-wsdl-service"
let arg_wsdl_service proc_ctxt = fun s -> Conf.chosen_service := Some s
let msg_wsdl_service = "Specifies which service to choose; by default the first service in the WSDL file"

let opt_wsdl_port = "-wsdl-port"
let arg_wsdl_port proc_ctxt = fun p -> Conf.chosen_port := Some p
let msg_wsdl_port = "Specifies which port to choose for the service; by default the first valid port for the service"

(* SOAP options *)

let opt_soap_wsdl = "-soap-wsdl"
let arg_soap_wsdl proc_ctxt = fun wsdl -> Conf.wsdl_url := wsdl
let msg_soap_wsdl = "Sets the wsdl to be used for the service"

let opt_soap_port = "-soap-port"
let arg_soap_port proc_ctxt = fun p -> Conf.chosen_port := Some p
let msg_soap_port = "Port from the WSDL service element; by default the first port"

let opt_soap_binding = "-soap-binding"
let arg_soap_binding proc_ctxt = fun b -> Conf.chosen_binding := Some b
let msg_soap_binding = "Binding from WSDL; it cannot be used together with -port. NOTE: not implemented"

let opt_soap_installdir = "-soap-installdir"
let arg_soap_installdir proc_ctxt = fun i -> Conf.installdir := i
let msg_soap_installdir = "Sets installation directory for the stub"

let opt_soap_interfacedir = "-soap-interfacedir"
let arg_soap_interfacedir proc_ctxt = fun i -> Conf.interfacedir := i
let msg_soap_interfacedir = "Generates an .xq client file"

let opt_soap_address = "-soap-address"
let arg_soap_address proc_ctxt = fun a -> Conf.address_uri := Some a
let msg_soap_address = "Sets the soap:address of the exported service"

let opt_soap_namespace = "-soap-namespace"
let arg_soap_namespace proc_ctxt = fun s -> Conf.nms_uri := s
let msg_soap_namespace = "The namespace of the exported module"


(******************************)
(* Titles for option clusters *)
(******************************)

let title_main = "\n//\n// Here is the list of available options:\n//\n"

let title_misc_options               = "\n\n // Misc options\n"
let title_monitor_options            = "\n\n // Monitoring options\n"
let title_character_encoding_options = "\n\n // Character encoding options\n"
let title_input_context_options      = "\n\n // Input context options\n"
let title_data_model_options         = "\n\n // Data model options\n"
let title_serialization_options      = "\n\n // Serialization options\n"
let title_behavior_options           = "\n\n // Behavior options\n"
let title_processing_phases_options  = "\n\n // Processing phases options\n"
let title_printing_options           = "\n\n // Printing options\n"
let title_testing_options            = "\n\n // Testing options\n"
let title_prototype_options          = "\n\n // Prototype options\n"
let title_optimization_options       = "\n\n // Optimization options\n"
let title_code_selection_options     = "\n\n // Code selection options\n"
let title_runtime_options            = "\n\n // Runtime options\n"
let title_daemon_options             = "\n\n // DXQ server options\n"
let title_zerod_options              = "\n\n // Zerod Proxy server options\n"
let title_xqueryx_options            = "\n\n // XQueryX options\n"
let title_galax_parse_options        = "\n\n // Parse specific options\n"
let title_galax_project_options      = "\n\n // Project specific options\n"
let title_wsdl_options               = "\n\n // WSDL specific options\n"
let title_soap_options               = "\n\n // SOAP specific options\n"


(*****************)
(* Usage message *)
(*****************)

let msg_galax_run () = 
  sprintf "Usage: %s %s [options] input-queries (or '-' for stdin)" Sys.argv.(0) Sys.argv.(1)

let msg_galax_project () = 
  sprintf "Usage: %s %s [options] input-queries (or '-' for stdin)" Sys.argv.(0) Sys.argv.(1)

let msg_galax_schema () = 
  sprintf "Usage: %s %s [options] input-queries (or '-' for stdin)" Sys.argv.(0) Sys.argv.(1)

let msg_galax_daemon () =
  sprintf "Usage: %s [options] )" Sys.argv.(0)

let msg_galax_parse () =
  sprintf "Usage: %s %s [options] input-xml-documents (or '-' for stdin)" Sys.argv.(0) Sys.argv.(1)

let msg_galax_compile () =
  sprintf "Usage: %s %s [options] input-queries (or '-' for stdin)" Sys.argv.(0) Sys.argv.(1)

let msg_zerod () =
  sprintf "Usage: %s [options] )" Sys.argv.(0)

let msg_wsdl () =
  sprintf "Usage: %s %s [options] input-wsdl-files" Sys.argv.(0) Sys.argv.(1)

let msg_soap () =
  sprintf "Usage: %s %s XQueryModule [-wsdl WSDL] [-port WSDLPort] [-binding WSDLBinding] [-installdir Directory] [-interfacedir Directory] [-address URI] [-nms module-namespace]\n" Sys.argv.(0) Sys.argv.(1)

let usage_galax_run ()  	= (msg_galax_run ()) ^ title_main
let usage_galax_project ()  	= (msg_galax_project ()) ^ title_main
let usage_galax_schema ()	= (msg_galax_schema ()) ^ title_main
let usage_galax_daemon ()	= (msg_galax_daemon ()) ^ title_main
let usage_galax_parse ()	= (msg_galax_parse ()) ^ title_main
let usage_galax_compile ()      = (msg_galax_compile ()) ^ title_main ^ title_misc_options
let usage_zerod ()	        = (msg_zerod ()) ^ title_main
let usage_wsdl ()               = (msg_wsdl ()) ^ title_main
let usage_soap ()               = (msg_soap ()) ^ title_main

(* Galax project options *)

let make_galax_project_options bos title =
  let (bo_unit,bo_int,bo_string,bo_set,bo_clear) = bos in
  [ opt_projection, (bo_string arg_projection), msg_projection;
    opt_output_projection, (bo_string arg_output_projection), msg_output_projection;
    opt_output_projected, (bo_string arg_output_projected), msg_output_projected ]

(* Galax parse options *)
let make_galax_parse_options bos title =
  let (bo_unit,bo_int,bo_string,bo_set,bo_clear) = bos in
  [ opt_pxp,       (bo_string arg_pxp),       msg_pxp;
    opt_stream,    (bo_string arg_stream),    msg_stream;
    opt_resolve,   (bo_string arg_resolve),   msg_resolve;
    opt_annotate,  (bo_string arg_annotate),  msg_annotate;
    opt_erase,     (bo_string arg_erase),     msg_erase;
    opt_prefix,    (bo_string arg_prefix),    msg_prefix;
    opt_export,    (bo_string arg_export),    msg_export;
    opt_load,  	   (bo_string arg_load),      msg_load;
    opt_dm,  	   (bo_string arg_dm),        msg_dm;
    opt_validate,  (bo_string arg_validate),  msg_validate;
    opt_xmlschema, (bo_string arg_xmlschema), msg_xmlschema;
    opt_diff, 	   (bo_set arg_diff), 	      msg_diff;
    opt_dtd, 	   (bo_set arg_dtd), 	      msg_dtd ^ title 
    ]

(* Misc options *)
let make_misc_options bos title =
  let (bo_unit,bo_int,bo_string,bo_set,bo_clear) = bos in
  [ opt_version, (bo_unit   arg_version), msg_version;
    opt_verbose, (bo_string arg_verbose), msg_verbose;
    opt_debug,   (bo_string arg_debug),   msg_debug ^ title
  ]

(* Monitoring options *)
let make_monitoring_options bos title =
  let (bo_unit,bo_int,bo_string,bo_set,bo_clear) = bos in
  [ opt_monitor,        (bo_string arg_monitor), 	msg_monitor;
    opt_monitor_mem,    (bo_string arg_monitor_mem),    msg_monitor_mem;
    opt_monitor_time,   (bo_string arg_monitor_time),   msg_monitor_time;
    opt_output_monitor, (bo_string arg_output_monitor), msg_output_monitor ^ title]

(* Character encoding options *)
let make_encoding_options bos title =
  let (bo_unit,bo_int,bo_string,bo_set,bo_clear) = bos in
  [ opt_internal_encoding, (bo_string arg_internal_encoding), msg_internal_encoding;
    opt_output_encoding, (bo_string arg_output_encoding), msg_output_encoding ^ title ]

(* Input context options *)
let make_context_options bos title =
  let (bo_unit,bo_int,bo_string,bo_set,bo_clear) = bos in
  [ (* opt_base_uri,    (bo_string arg_base_uri),     msg_base_uri; *)
    opt_var,         (bo_string arg_var),          msg_var;
    opt_doc,         (bo_string arg_doc),          msg_doc;
    opt_context_item,(bo_string arg_context_item), msg_context_item;
    opt_context,     (bo_string arg_context),      msg_context ^ title ]

(* Data model options *)
let make_data_model_options bos title =
  let (bo_unit,bo_int,bo_string,bo_set,bo_clear) = bos in
  [ opt_xml_whitespace, (bo_unit arg_xml_whitespace), msg_xml_whitespace;
    opt_xml_pic,        (bo_unit arg_xml_pic),        msg_xml_pic ^ title ]

(* Serialization options *)
let make_serialization_options bos title =
  let (bo_unit,bo_int,bo_string,bo_set,bo_clear) = bos in
  [ opt_serialize, (bo_string arg_serialize), msg_serialize ^ title ]

(* Behavior options *)
let make_behavior_options bos title =
  let (bo_unit,bo_int,bo_string,bo_set,bo_clear) = bos in
  [ opt_language,      (bo_string arg_language),       msg_language;
    opt_syntax,        (bo_string arg_syntax),         msg_syntax;
    opt_embed_xqueryx, (bo_string arg_embed_xqueryx),  msg_embed_xqueryx;
    opt_typing,        (bo_string arg_typing),         msg_typing ^ title ;
(*
    opt_boundary_space,(bo_string arg_boundary_space), msg_boundary_space;
    opt_construction,  (bo_string arg_construction),   msg_construction;
    opt_ordering,      (bo_string arg_ordering),       msg_ordering;
    opt_default_order, (bo_string arg_default_order),  msg_default_order;
    opt_ns_preserve,   (bo_string arg_ns_preserve),    msg_ns_preserve;
    opt_ns_inherit,    (bo_string arg_ns_inherit),     msg_ns_inherit 
*)]

(* Processing phases options *)
let make_processing_phases_options bos title =
  let (bo_unit,bo_int,bo_string,bo_set,bo_clear) = bos in
  [ opt_execute,        (bo_string arg_execute),          msg_execute;
    opt_normalize,     	(bo_string arg_normalize),        msg_normalize;
    opt_static_typing, 	(bo_string arg_static_typing),    msg_static_typing;
    opt_rewriting,     	(bo_string arg_rewriting),        msg_rewriting;
    opt_factorization, 	(bo_string arg_factorization),    msg_factorization;
    opt_optimization,  	(bo_string arg_optimization),     msg_optimization;
    opt_code_selection,	(bo_string arg_code_selection),   msg_code_selection;
    opt_dynamic,       	(bo_string arg_dynamic),          msg_dynamic ^ title ]

(* Printing options *)
let make_printing_options bos title =
  let (bo_unit,bo_int,bo_string,bo_set,bo_clear) = bos in
  [ opt_print_global,       (bo_string arg_print_global),     	msg_print_global;
    opt_print_expr,         (bo_string arg_print_expr),       	msg_print_expr;
    opt_output_expr,        (bo_string arg_output_expr),      	msg_output_expr;
    opt_print_norm_expr,    (bo_string arg_print_norm_expr),  	msg_print_norm_expr;
    opt_output_norm_expr,   (bo_string arg_output_norm_expr), 	msg_output_norm_expr;
    opt_print_type,         (bo_string arg_print_type),       	msg_print_type;
    opt_output_type,        (bo_string arg_output_type),      	msg_output_type;
    opt_print_rewr_expr,    (bo_string arg_print_rewr_expr),  	msg_print_rewr_expr;
    opt_output_rewr_expr,   (bo_string arg_output_rewr_expr), 	msg_output_rewr_expr;
    opt_print_fact_expr,    (bo_string arg_print_fact_expr),  	msg_print_fact_expr;
    opt_output_fact_expr,   (bo_string arg_output_fact_expr), 	msg_output_fact_expr;
    opt_print_log_plan,     (bo_string arg_print_log_plan),   	msg_print_log_plan;
    opt_output_log_plan,    (bo_string arg_output_log_plan),  	msg_output_log_plan;
    opt_print_opt_plan,     (bo_string arg_print_opt_plan),   	msg_print_opt_plan;
    opt_output_opt_plan,    (bo_string arg_output_opt_plan),  	msg_output_opt_plan;
    opt_print_phys_plan,    (bo_string arg_print_phys_plan),  	msg_print_phys_plan;
    opt_output_phys_plan,   (bo_string arg_output_phys_plan), 	msg_output_phys_plan;
    opt_print_xml,          (bo_string arg_print_xml),        	msg_print_xml;
    opt_output_xml,         (bo_string arg_output_xml),       	msg_output_xml;
    opt_print_error,        (bo_string arg_print_error),        msg_print_error;
    opt_print_warnings,     (bo_string arg_print_warnings),     msg_print_warnings;
    opt_print_plan,         (bo_string arg_print_plan),         msg_print_plan;
    opt_print_annotations,  (bo_string arg_print_annotations),  msg_print_annotations;
    opt_print_prolog,       (bo_string arg_print_prolog),       msg_print_prolog;
    opt_output_all,         (bo_string arg_output_all),         msg_output_all;
    opt_output_err,         (bo_string arg_output_err),         msg_output_err;
    opt_print_log_rewrites, (bo_string arg_print_log_rewrites), msg_print_log_rewrites;
    opt_output_log_rewrites,(bo_string arg_output_log_rewrites),msg_output_log_rewrites;
    opt_print_comp_annot,   (bo_string arg_print_comp_annot),   msg_print_comp_annot;
    opt_print_materialize,  (bo_string arg_print_materialize),  msg_print_materialize;
    opt_output_dfgraph,     (bo_string arg_output_dfgraph),     msg_output_dfgraph;
    opt_print_plan_shebang, (bo_string arg_print_plan_shebang), msg_print_plan_shebang ^ title ]

let make_testing_options bos title =
  let (bo_unit,bo_int,bo_string,bo_set,bo_clear) = bos in
  [ opt_genresults, (bo_unit arg_genresults), msg_genresults ^ title ]

let make_printing_parse_options bos title =
  let (bo_unit,bo_int,bo_string,bo_set,bo_clear) = bos in
  [ opt_print_error, (bo_string arg_print_error), msg_print_error;
    opt_output_err,  (bo_string arg_output_err),  msg_output_err;
    opt_print_xml,   (bo_string arg_print_xml),   msg_print_xml;
    opt_output_xml,  (bo_string arg_output_xml),  msg_output_xml ^ title ]

(* Logical optimization options *)
let make_logical_optimization_options bos title =
  let (bo_unit,bo_int,bo_string,bo_set,bo_clear) = bos in
  [ opt_aggressive,        (bo_string arg_aggressive),        msg_aggressive;
    opt_projection,        (bo_string arg_projection),        msg_projection;
    opt_treejoin_log,      (bo_string arg_treejoin_log),      msg_treejoin_log;
    opt_streaming,         (bo_string arg_streaming),         msg_streaming;
    opt_infer_independence,(bo_string arg_infer_indepedence), msg_infer_indepedence;
    opt_dxq,               (bo_string arg_dxq),               msg_dxq;
    opt_inline,            (bo_string arg_inline),            msg_inline;
    opt_inline_var,        (bo_string arg_inline_var),        msg_inline_var;
    opt_sbdo,              (bo_string arg_sbdo),              msg_sbdo;
    opt_streaming_shebang, (bo_string arg_streaming_shebang), msg_streaming_shebang; 
    opt_scjoin_shebang, (bo_string arg_scjoin_shebang), msg_scjoin_shebang; 
    opt_twigjoin_shebang, (bo_string arg_twigjoin_shebang), msg_twigjoin_shebang ^ title ]

(* Code selection options *)
let make_physical_optimization_options bos title =
  let (bo_unit,bo_int,bo_string,bo_set,bo_clear) = bos in
  [ opt_treejoin_phys,     (bo_string arg_treejoin_phys),     msg_treejoin_phys;
    opt_nested_loop_join,  (bo_string arg_nested_loop_join),  msg_nested_loop_join;
    opt_force_materialized,(bo_string arg_force_materialized),msg_force_materialized;
    opt_materialize_tables,(bo_string arg_materialize_tables),msg_materialize_tables;
    opt_allow_streamed_tuple_fields,(bo_string arg_allow_streamed_tuple_fields),msg_allow_streamed_tuple_fields;
    opt_descendant_hack, (bo_string arg_descendant_hack), msg_descendant_hack ^ title
  ]

(* Runtime options *)
let make_runtime_options bos title =
  let (bo_unit,bo_int,bo_string,bo_set,bo_clear) = bos in
  [ opt_buffer_chunks,     (bo_string arg_buffer_chunks),     msg_buffer_chunks;
    opt_buffer_csize,      (bo_string arg_buffer_csize),      msg_buffer_csize;
    opt_buffer_inc,        (bo_string arg_buffer_inc),        msg_buffer_inc ^ title ]

(* Printing options *)
let make_daemon_options bos title =
  let (bo_unit,bo_int,bo_string,bo_set,bo_clear) = bos in
  [ opt_dxq_port, (bo_int arg_dxq_port), msg_dxq_port;
    opt_dxq_dir, (bo_string arg_dxq_dir), msg_dxq_dir;
    opt_dxq_sim, (bo_string arg_dxq_sim), msg_dxq_sim;
    opt_dxq_topology, (bo_string arg_dxq_topology), msg_dxq_topology;
    opt_dxq_drop_msgs, (bo_string arg_dxq_drop_msgs), msg_dxq_drop_msgs;
  ] 

let make_zerod_options bos title =
  let (bo_unit,bo_int,bo_string,bo_set,bo_clear) = bos in
    [ opt_zerod_port, (bo_int arg_zerod_port), msg_zerod_port;
      opt_verbose, (bo_string arg_verbose), msg_verbose ]
      
let make_xqueryx_options bos title =
  let (bo_unit,bo_int,bo_string,bo_set,bo_clear) = bos in
    [ opt_xqueryx_batch, (bo_unit arg_xqueryx_batch), msg_xqueryx_batch ]

(* WSDL options *)

let make_wsdl_options bos title =
  let (bo_unit,bo_int,bo_string,bo_set,bo_clear) = bos in
    [ opt_wsdl_client_name, (bo_string arg_wsdl_client_name), msg_wsdl_client_name;
      opt_wsdl_client, (bo_unit arg_wsdl_client), msg_wsdl_client;
      opt_wsdl_server_name, (bo_string arg_wsdl_server_name), msg_wsdl_server_name;
      opt_wsdl_server, (bo_unit arg_wsdl_server), msg_wsdl_server;
      opt_wsdl_impl_name, (bo_string arg_wsdl_impl_name), msg_wsdl_impl_name;
      opt_wsdl_namespace, (bo_string arg_wsdl_namespace), msg_wsdl_namespace;
      opt_wsdl_service, (bo_string arg_wsdl_service), msg_wsdl_service;
      opt_wsdl_port, (bo_string arg_wsdl_port), msg_wsdl_port ]

(* SOAP options *)

let make_soap_options bos title =
  let (bo_unit,bo_int,bo_string,bo_set,bo_clear) = bos in
    [ opt_soap_wsdl, (bo_string arg_soap_wsdl), msg_soap_wsdl;
      opt_soap_port, (bo_string arg_soap_port), msg_soap_port;
      opt_soap_binding, (bo_string arg_soap_binding), msg_soap_binding;
      opt_soap_installdir, (bo_string arg_soap_installdir), msg_soap_installdir;
      opt_soap_interfacedir, (bo_string arg_soap_interfacedir), msg_soap_interfacedir;
      opt_soap_address, (bo_string arg_soap_address), msg_soap_address;
      opt_soap_namespace, (bo_string arg_soap_namespace), msg_soap_namespace ]

type option_classes =
  | GalaxProject_Options
  | GalaxParse_Options
  | Misc_Options
  | Monitoring_Options
  | Encoding_Options
  | Context_Options
  | DataModel_Options
  | Serialization_Options
  | Behavior_Options
  | ProcessingPhases_Options
  | Printing_Options
  | Optimization_Options
  | CodeSelection_Options
  | Runtime_Options
  | PrintParse_Options
  | Daemon_Options
  | Zerod_Options
  | Testing_Options
  | XQueryX_Options
  | WSDL_Options
  | SOAP_Options

let option_table =
  [ GalaxProject_Options, (make_galax_project_options,title_galax_project_options);
    GalaxParse_Options, (make_galax_parse_options,title_galax_parse_options);
    Misc_Options, (make_misc_options, title_misc_options);
    Monitoring_Options, (make_monitoring_options, title_monitor_options);
    Encoding_Options, (make_encoding_options, title_character_encoding_options);
    Context_Options, (make_context_options, title_input_context_options);
    DataModel_Options, (make_data_model_options, title_data_model_options);
    Serialization_Options, (make_serialization_options, title_serialization_options);
    Behavior_Options, (make_behavior_options, title_behavior_options);
    ProcessingPhases_Options, (make_processing_phases_options, title_processing_phases_options);
    Printing_Options, (make_printing_options, title_printing_options);
    Optimization_Options, (make_logical_optimization_options, title_optimization_options);
    CodeSelection_Options, (make_physical_optimization_options, title_code_selection_options);
    Runtime_Options, (make_runtime_options, title_runtime_options);
    PrintParse_Options, (make_printing_parse_options, title_printing_options);
    Daemon_Options, (make_daemon_options, title_daemon_options);
    Zerod_Options, (make_zerod_options, title_zerod_options);
    Testing_Options, (make_testing_options, title_testing_options);
    XQueryX_Options, (make_xqueryx_options,title_xqueryx_options);
    WSDL_Options, (make_wsdl_options,title_wsdl_options);
    SOAP_Options, (make_soap_options,title_soap_options) ]

let rec make_parse_list bos usage option_list first =
  match option_list with
  | [] -> ([],usage)
  | x :: [] ->
      let (makefunction,title) = List.assoc x option_table in
      let usage =
	if first
	then usage ^ title
	else usage
      in
      (makefunction bos "",usage)
  | x1 :: x2 :: l ->
      let (makefunction1,title1) = List.assoc x1 option_table in
      let (_,title2) = List.assoc x2 option_table in
      let usage =
	if first
	then usage ^ title1
	else usage
      in
      let (previous_table,usage) = make_parse_list bos usage (x2::l) false in
      let new_table = makefunction1 bos title2 in
      (new_table @ previous_table,usage)

let make_options proc_ctxt usage option_list =
  (* Build the functions to process each options *)
  let bo f = f proc_ctxt in
  let bo_unit f = Arg.Unit (bo f) in
  let bo_int f = Arg.Int (bo f) in
  let bo_string f = Arg.String (bo f) in
  let bo_set f = Arg.Set (bo f) in
  let bo_clear f = Arg.Clear (bo f) in
  let bos = (bo_unit,bo_int,bo_string,bo_set,bo_clear) in

  (* Initialize the remaining arguments *)
  let args = ref [] in

  (* Create the arguments parsing operation *)
  let (parse_list,real_usage) = 
    make_parse_list bos usage option_list true
  in
  begin
    Arg.parse parse_list (fun arg -> args := arg :: !args) real_usage;
    !args
  end

let make_options_argv proc_ctxt usage option_list gargs =
  (* Build the functions to process each options *)
  let bo f = f proc_ctxt in
  let bo_unit f = Arg.Unit (bo f) in
  let bo_int f = Arg.Int (bo f) in
  let bo_string f = Arg.String (bo f) in
  let bo_set f = Arg.Set (bo f) in
  let bo_clear f = Arg.Clear (bo f) in
  let bos = (bo_unit,bo_int,bo_string,bo_set,bo_clear) in

  (* Initialize the remaining arguments *)
  let args = ref [] in

  (* Create the arguments parsing operation *)
  let (parse_list,real_usage) = 
    make_parse_list bos usage option_list true
  in
  begin
    begin
      try
	Arg.parse_argv ~current:(ref 0) gargs parse_list (fun arg -> args := arg :: !args) real_usage;
      with
      | Arg.Bad msg -> eprintf "%s" msg; exit 2;
      | Arg.Help msg -> printf "%s" msg; exit 0;
    end;
    !args
  end
