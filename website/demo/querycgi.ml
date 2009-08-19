(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: querycgi.ml,v 1.35 2008/03/12 22:30:58 simeon Exp $ *)

(* Galax modules *)

open Error
open Conf

open Xquery_ast
open Print_top

open Processing_context
open Top_util
open Top_config

open Galax

(* Caml modules *)

open Format

open Netcgi
open Netcgi_types

(* demo modules *)

open Demo_conf

(* Additional global parameters *)

let showresult = ref true
let showtype = ref true

let html_setup () =
  separator := "<HR/>"

(* Extract CGI arguments *)

let language = ref "xquery10"
let has_side_effects () =
  match !language with
  | "xquery10" -> false
  | _ -> true

let the_documents = ref []
let set_documents doc =
  let docs = Gmisc.split_on_char doc ' ' in
  the_documents := docs

let print_docs cgiobj prepared_prolog =
  let proc_ctxt = default_processing_context () in
  (* The default for the command-line are different than those for the API *)
  set_serialization_kind proc_ctxt Serialize_As_Standard;
  Top_config.set_language_kind_arg proc_ctxt !language;
  begin
    let buff = Buffer.create 512 in
    Conf.xml_formatter := Format.formatter_of_buffer buff;
    let verbose = !Conf.verbose in
    let pe = !Conf.print_expr in
    let pce = !Conf.print_core_expr in
    let pt = !Conf.print_type in
    let poe = !Conf.print_optimized_expr in
    let px = !Conf.print_xml in
    List.iter (fun x ->
      Conf.verbose := false;
      Conf.print_expr := false;
      Conf.print_core_expr := false;
      Conf.print_type := false;
      Conf.print_optimized_expr := false;
      Conf.print_xml := true;
      let v = eval_statement prepared_prolog (Galax_io.String_Input x) in
      Galax.serialize proc_ctxt (Galax_io.Formatter_Output (!Conf.xml_formatter)) v;
      let res = Buffer.contents buff in
      let escaped_res = !Conf.xml_charescape_fn res in
      Conf.verbose := verbose;
      Conf.print_expr := pe;
      Conf.print_core_expr := pce;
      Conf.print_type := pt;
      Conf.print_optimized_expr := poe;
      Conf.print_xml := px;
      cgiobj # output # output_string escaped_res) !the_documents;
    cgiobj # output # output_string ("\n</pre>\n"^(!separator));
  end

let print_docs_before cgiobj prepared_prolog =
  begin
    cgiobj # output # output_string "<H2>Documents Before the Query:</H2>\n<pre>\n";
    print_docs cgiobj prepared_prolog
  end

let print_docs_after cgiobj prepared_prolog =
  begin
    cgiobj # output # output_string "<H2>Documents Before the Query:</H2>\n<pre>\n";
    print_docs cgiobj prepared_prolog
  end

let process_args proc_ctxt (cgiobj : cgi_activation) =
(*  Conf.unsafe_join_hack := true ;  *)
  begin
    try
      language := (cgiobj # argument_value "language")
    with
    | _ -> ()
  end;
  begin
    try
      let the_documents = (cgiobj # argument_value "documents") in
      set_documents the_documents
    with
    | _ -> ()
  end;
  begin
    try
      if (bool_of_string (cgiobj # argument_value "debug")) then Debug.set_debug [Debug.DefaultDebug] else ()
    with
    | _ -> ()
  end;
  begin
    try
      Conf.print_expr := bool_of_string (cgiobj # argument_value "expr-print")
    with
    | _ -> ()
  end;
  begin
    try
      Conf.print_core_expr := bool_of_string (cgiobj # argument_value "core-print")
    with
    | _ -> ()
  end;
  begin
    try
      set_typing_phase_arg proc_ctxt (cgiobj # argument_value "typing")
    with
    | _ -> ()
  end;
  begin
    try
      Conf.print_type := bool_of_string (cgiobj # argument_value "typing-print");
    with
    | _ -> ()
  end;
  begin
    try
      set_rewriting_phase_arg proc_ctxt (cgiobj # argument_value "optim")
    with
    | _ -> ()
  end;
  begin
    try
      Conf.print_optimized_expr := bool_of_string (cgiobj # argument_value "optim-print")
    with
    | _ -> ()
  end;
  begin
    try
      set_evaluation_phase_arg proc_ctxt (cgiobj # argument_value "eval")
    with
    | _ -> ()
  end;
  begin
    try
      Conf.print_xml := bool_of_string (cgiobj # argument_value "eval-print")
    with
    | _ -> ()
  end;
  check_config proc_ctxt;
  let query_string = (cgiobj # argument_value "query")
  (* Load local context file *)
  and query_context = Gmisc.load_file (cgiobj # argument_value "context") in
  (query_context,query_string)

let html_xquery_loop cgiobj prepared_prolog proc_ctxt t cs =
  let buff = Buffer.create 512 in
  Conf.xml_formatter := Format.formatter_of_buffer buff;
  let verbose = !Conf.verbose in
  let v = eval_compiled_statement prepared_prolog cs in
  Conf.verbose := false;
  Galax.serialize proc_ctxt (Galax_io.Formatter_Output (!Conf.xml_formatter)) v;
  let res = Buffer.contents buff in
  let escaped_res = !Conf.xml_charescape_fn res in
  Conf.verbose := verbose;
  cgiobj # output # output_string (!Conf.xml_header); 
  cgiobj # output # output_string escaped_res;
  cgiobj # output # output_string (!Conf.xml_footer) 

let main cgiobj =
  (* The default for the command-line are different than those for the API *)
  html_setup ();
  demo_init "galax_demo_config";

  Conf.verbose := true; 

  (* Set up the output headers and footers *)
  Conf.expr_header := "\n"^(!separator)^"\n<H2>XQuery Expression:</H2>\n<pre>\n";
  Conf.expr_footer := "</pre><br/>\n"^(!separator);

  Conf.core_expr_header := "\n<H2>Normalized Expression (XQuery Core):</H2>\n<pre>\n";
  Conf.core_expr_footer := "</pre><br/>\n"^(!separator);

  Conf.type_header := "<H2>Static Type Analysis:</H2>\n<pre>\n";
  Conf.type_footer := "</pre><br/>\n"^(!separator);

  Conf.optimized_expr_header := "\n<H2>Optimized Normalized Expression (XQuery Core):</H2>\n<pre>\n";
  Conf.optimized_expr_footer := "</pre><br/>\n"^(!separator);

  Conf.xml_header := "<H2>Dynamic Evaluation:</H2>\n<pre>\n";
  Conf.xml_footer := "</pre><br/>\n"^(!separator);

  (* Why was that commented? - Jerome *)
  Sys.chdir !document_dir;

  (* First get the default processing context *)
  let proc_ctxt = default_processing_context () in
  (* The default for the command-line are different than those for the API *)
  set_serialization_kind proc_ctxt Serialize_As_XQuery;
  
  let (context,queries) = process_args proc_ctxt cgiobj in

  Top_config.set_language_kind_arg proc_ctxt !language;
  Top_config.set_serialization_kind_arg proc_ctxt "xquery";

  let full_text = (context ^ queries) in
  let full_text =
    let b = Netbuffer.create 20 in
    Gmisc.load_string_in_buffer b full_text;
    b
  in

  if (Debug.default_debug()) then
    begin
      cgiobj # output # output_string "Query context is:\n\n";
      cgiobj # output # output_string context;
      begin
	let fi = open_out "/tmp/content.txt" in
	output_string fi "Query is:\n\n";
	output_string fi queries;
	close_out fi
      end
    end;

  begin
    Conf.xml_charescape_fn := Netencoding.Html.encode_from_latin1;  
    let compiled_prolog = Galax.load_standard_library proc_ctxt in
    (* Load main module *)
    let (compiled_module, stmts) = Galax.import_main_module false compiled_prolog (Galax_io.Buffer_Input full_text) in
    (* Evaluate global variables *)
    let prepared_prolog = Galax.prepare_program compiled_module None in 

    if has_side_effects () then
      print_docs_before cgiobj prepared_prolog;

    List.iter (html_xquery_loop cgiobj prepared_prolog proc_ctxt full_text) stmts;

    if has_side_effects () then
      print_docs_after cgiobj prepared_prolog
  end

let beg_html =
  "<html><head><title>Galax</title><link href=\"../galax.css\" type=\"text/css\" rel=\"stylesheet\"></head>\n<body>\n<multicol cols=1>\n"

let end_html =
  "</multicol>\n\
</body></html>\n"

let exec func (cgiobj : cgi_activation) =
   try 
    cgiobj # set_header ();
    cgiobj # output # output_string beg_html;
    cgiobj # output # commit_work();
    func cgiobj;   
    cgiobj # output # output_string end_html;
    cgiobj # output # commit_work()
  with
    e ->
      begin
	cgiobj # output # rollback_work();
	cgiobj # set_header ~status:`Internal_server_error ();
	cgiobj # output # output_string beg_html;
        cgiobj # output # output_string "<H2>An Error Was Raised:</H2>\n<pre>\n";
	cgiobj # output # output_string ("Document dir:" ^ !document_dir ^"\n");
        cgiobj # output # output_string (bprintf_error_safe "  " e);
        cgiobj # output # output_string ("\n</pre>\n"^(!separator));
        cgiobj # output # output_string end_html;
        cgiobj # output # commit_work()
      end

let _ =
  let operating_type = Netcgi.buffered_transactional_optype in
  let cgiobj = ((new Netcgi.std_activation ~operating_type ()) :> cgi_activation) in 
  exec main cgiobj

