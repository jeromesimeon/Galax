(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: usecase.ml,v 1.23 2008/02/13 20:18:50 mff Exp $ *)

open Xquery_ast
open Demo_conf

(* Automatic generation of the use cases JavaScript files from the
   examples use cases *)

let usage_msg = 
  Format.sprintf "Usage: %s inputdir outputdir configfile" Sys.argv.(0)

(* Process the input arguments *)

let language = ref "xquery10"
let the_documents = ref ""

let process_args () =
  let args = ref [] in
  Arg.parse
    [
     "-language", Arg.String (fun s -> language := s; ), "Language class [xquery10, ultf, xquerybang, xqueryp]"
   ]
    (fun arg -> args := arg :: !args) usage_msg;
  match !args with
  |  configfile :: javascriptdir :: usecasedir :: [] ->
      (usecasedir, javascriptdir, configfile)
  | _ ->
      failwith "You must specify input and output usecase directories, and a configfile"

(* Extract the comments from the original use case *)

let rec process_comments sl =
  match sl with
    [] -> []
  | Str.Text txt :: sl' -> (process_comments sl')
  | (Str.Delim smalltext) :: (Str.Text _) :: (Str.Delim longtext) :: (Str.Text _) :: sl' ->
      let smalltext' = Str.global_replace (Str.regexp "^(: \\| :)")  "" smalltext in
      let longtext' = Str.global_replace (Str.regexp "^(: \\| :)")  "" longtext in
      (smalltext',longtext') :: (process_comments sl')
  | Str.Delim x :: sl' ->
      failwith "Use case file not in appropriate format: two SINGLE-line comments, followed by query, then semi-colon."

let extract_comment usecasefile =
  let querycontent = Gmisc.load_file usecasefile in
(*  let reg = Str.regexp "^(: \\| :)" in *)
  let reg = Str.regexp "^(: Q[0-9]+.* :)[ \t]*$" in
  let splited_list = Str.full_split reg querycontent in
  process_comments splited_list

(* HTML templates *)

let build_queries all_comments =
  let query = ref "" in
  let counter = ref 0 in
  let rec add_query all_comments =
    match all_comments with
    | [] -> ()
    | (smallcomment,_) :: all_comments' ->
	begin
	  counter := !counter + 1;
	  let new_query =
	    ("<option value=\""
	     ^ (string_of_int !counter)
	     ^ "\">"
	     ^ smallcomment
	     ^ "\n")
	  in
	  add_query all_comments';
	  query := new_query ^ !query
	end
  in
  begin
    add_query all_comments;
    !query
  end

let build_documents all_documents =
  let docs = ref "" in
  let rec add_doc all_documents =
    match all_documents with
    | [] -> ()
    | (file,desc,_) :: all_documents' ->
	begin
	  let new_doc =
	    ("<li><a href=\"" 
	     ^ !cgibin_prefix 
	     ^ ""  (* We now link to files directly; viewxmlopt.cgi?file= *)
	     ^ file
	     ^ "\">"
	     ^ desc
	     ^ "</a>\n")
	  in
	  add_doc all_documents';
	  docs := new_doc ^ !docs
	end
  in
  begin
    add_doc all_documents;
    !docs
  end
  

let create_html_script context all_documents all_comments (name,title,typing) =
  let documents = build_documents all_documents in
  let queries = build_queries all_comments in
  let html_template =
    Gmisc.load_file "template.html"
  in
  let fix_documents = Str.global_replace (Str.regexp "\\$documents\\$") !the_documents html_template in
  let fix_language = Str.global_replace (Str.regexp "\\$language\\$") !language fix_documents in
  let fix_cgibin = Str.global_replace (Str.regexp "\\$cgibin_prefix\\$") !cgibin_prefix fix_language in
  let add_title = Str.global_replace (Str.regexp "\\$title\\$") title fix_cgibin in
  let add_context = Str.global_replace (Str.regexp "\\$context\\$") context add_title in
  let add_name = Str.global_replace (Str.regexp "\\$name\\$") name add_context in
  let add_documents = Str.global_replace (Str.regexp "\\$documents\\$") documents add_name in
  let (typingon, typingoff) = if (typing = "true") then ("checked=\"true\"", "") else ("","checked=\"true\"")  in
  let add_typing = Str.global_replace (Str.regexp "\\$typingon\\$") typingon add_documents in
  let add_typing' = Str.global_replace (Str.regexp "\\$typingoff\\$") typingoff add_typing in
  Str.global_replace (Str.regexp "\\$queries\\$") queries add_typing'

(* Javascript templates *)

let create_context context =
  "function context () {\n   document.forms[0].context.value =\n\""
  ^ (String.escaped context)
  ^ "\";\n}\n"

(*
let create_query_script counter querytext query =
  "function q"
  ^ (string_of_int counter)
  ^ " () {\n context() \n\n  document.forms[0].text.value =\""
  ^ (String.escaped querytext)
  ^ "\";\n  document.forms[0].query.value =\""
  ^ (String.escaped query)
  ^ "\";\n}\n"
*)

let create_query_script counter querytext query =
  "function q"
  ^ (string_of_int counter)
  ^ " () {\n  \n\n  document.forms[0].query.value =\"(: "
  ^ (String.escaped querytext)
  ^ " :)\\n\\n"
  ^ (String.escaped query)
  ^ "\";\n}\n"

let create_top_query_call morequery =
  "function displayQuery (select) {\n  index = select.selectedIndex;\n  "
  ^ morequery
  ^ "}\n\n"

let create_query_call counter morequery =
  let sc = string_of_int counter in
  "  if (index == \""
  ^ sc
  ^ "\") {\n    q"
  ^ sc
  ^ " ();\n  } else {\n  "
  ^ morequery
  ^ "  }\n"

let create_last_query_call counter =
  let sc = string_of_int counter in
  "  if (index == \""
  ^ sc
  ^ "\") {\n    q"
  ^ sc
  ^ " ();\n  }\n"

let create_complete_query_call counter =
 let querytext = ref (create_last_query_call counter) in
  let i = ref (counter-1) in
  while !i > 0 do
    querytext := create_query_call !i !querytext;
    i := !i - 1
  done;
  create_top_query_call !querytext

let create_all_queries all_comments qs =
  let counter = ref 0 in
  let query = ref "" in
  let rec create_complete_query_call_aux all_comments qs =
    match (all_comments,qs) with
    | ([],[]) ->
	()
    | (((_,longtext):: all_comments'),(q :: qs')) ->
	begin
	  counter := !counter + 1;
	  query := !query ^ (create_query_script !counter longtext q);
	  create_complete_query_call_aux all_comments' qs'
	end
    | _ ->
	failwith "Comments and queries do not match in usecase."
  in
  begin
    create_complete_query_call_aux all_comments qs;
    !query
  end

let create_javascript context counter all_comments expression_text =
  (create_context context)
  ^ (create_all_queries all_comments expression_text)
  ^ (create_complete_query_call counter)

(* Generates the Javascript for a given use case *)

let extract_xquery_text_from_file q t =
  let query_finfo = q.pexpr_loc in
  try
    Finfo.extract t query_finfo
  with
  | Invalid_argument _ ->
      "<Missing query use case>"

(* extracting documents *)

let rec process_documents sdc =
  match sdc with
  | [] -> []
  | file :: desc :: v :: sdc' ->
      (file,desc,v) :: (process_documents sdc')
  | _ ->
      []

let set_documents all_docs =
  let all_files = List.map (fun (_,_,v) -> "$" ^ v) all_docs in
  the_documents := String.concat " " all_files

let extract_documents df =
  let dc = Gmisc.load_file df in
  let sdc = Str.split (Str.regexp "\\$\\|\n") dc in
  let all_docs = process_documents sdc in
  set_documents all_docs;
  all_docs

(* generates a single use case *)

let generate_use_case usedir jsdir (name,title,typing) =
  print_string ("Generating: " ^ name ^ "\n");
  let contextlocalfile = name ^ "_context.xq"
  and querylocalfile = name ^ "_usecase.xq"
  and documentslocalfile = name ^ "_documents.txt" in
  let contextfile = Filename.concat usedir contextlocalfile
  and queryfile = Filename.concat usedir querylocalfile
  and documentsfile = Filename.concat usedir documentslocalfile in

  let contextcontent = 
    if (Sys.file_exists contextfile) then Gmisc.load_file contextfile 
    else ""
  in 
  let pc = Processing_context.default_processing_context () in
  Top_config.set_language_kind_arg pc !language;
  let parse_ctxt = Parse_context.build_xquery_parse_context pc in 
  let (parse_ctxt', original_xquery) = (Parse_top.parse_main_module_from_io parse_ctxt (Galax_io.File_Input queryfile)) in
  let actual_stmts = original_xquery.pmain_module_statements in
  let counter = List.length actual_stmts in
  let expression_text =
    List.map (fun s -> Print_top.bprintf_statement "" s) actual_stmts in
  let all_comments = extract_comment queryfile in
  let all_documents = extract_documents documentsfile in
  let js_content = create_javascript contextcontent counter all_comments expression_text in
  let js_file_name = name ^ "XQuery.js" in
  let js_file = Filename.concat jsdir js_file_name in
  let oc = open_out js_file in
  begin
    output_string oc js_content;
    close_out oc
  end;
  let html_content = create_html_script contextcontent all_documents all_comments (name,title,typing) in
  let html_file_name = "galax_" ^ name ^ ".html" in
  let html_file = Filename.concat jsdir html_file_name in
  let oc = open_out html_file in
  begin
    output_string oc html_content;
    close_out oc
  end

(* Extracts the list of use cases to generate *)

let rec get_use_case_list usecaselistfile =
  extract_documents usecaselistfile

(* Main *)

let main () =
  let (usecasedir,javascriptdir,configfile) = process_args () in
  begin
    demo_init configfile;
    try
      let usecaselistfile = Filename.concat usecasedir "Usecases" in
      let usecaselist = get_use_case_list usecaselistfile in
      List.iter (generate_use_case usecasedir javascriptdir) usecaselist
    with
    | x ->
	Error.printf_error "  " x
  end

let _ =
  main ()

