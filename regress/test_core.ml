(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: test_core.ml,v 1.45 2007/10/16 01:25:35 mff Exp $ *)

(* Module: Test_core
   Description:
     Contains the core code of the test harness.
 *)

open Galax
open Galax_io

open Error
open Processing_context

open Datatypes
open Dm_atomic
open Dm

open Physical_value
open Physical_item

open Streaming_types
open Streaming_util
open Streaming_ops
open Streaming_constructors
open Streaming_diff

open Top_util
open InternalQuery

(*************)
(* Utilities *)
(*************)

(* Errors *)

let raise_test_error msg =
  raise (Query (Testing_Error msg))

let raise_harness_error msg =
  raise (Query (Internal_Error msg))

(* Context *)

(* By default, all test cases are weak type checked, except for the
   "StaticTyping" group, which is strong type checked. *)
let proc_ctxt =
  begin
    let pc = default_processing_context() in
    pc.serialization_kind <- Serialize_As_Standard;
    pc.typing_kind <- Typing_Weak; 
    pc.merge_module_locations <- true; 
    (* The test suite assumes that multiple modules in the same target namespace are merged. *)
    pc
  end

let cp () = load_standard_library proc_ctxt

let eval_to_opt_string eval query dot =
  match eval_to_string_list eval query dot with
  | [] -> None
  | [x] -> Some x
  | _ -> raise_harness_error "Multiple context items in test case"

(* Streams *)

let typed_stream_of_io gio =
  let (_,s) = Streaming_parse.open_xml_stream_from_io gio in
  let rs = resolve_xml_stream s in
  typed_of_resolved_xml_stream rs

let cleaned_event event =
  match event.se_desc with
  | SAX_characters s ->
      let newchars =
	Whitespace.remove_newlines s
      in
      fmkse_event (SAX_characters newchars) event.se_loc
  | _ -> event

let cleaned_stream s =
  let next_event xml_stream () =
    try
      let next_event = Cursor.cursor_next xml_stream in
      Some (cleaned_event next_event)
    with
    | Stream.Failure ->
	None
  in
  Cursor.cursor_of_function (next_event s)

let cleaned_typed_stream_of_io gio =
  let s = typed_stream_of_io gio in
  cleaned_stream s

(* Fragments *)

let open_fragment = "<glx:result xmlns:glx=\"http://www.galaxquery.org\">"
let close_fragment = "</glx:result>"

let buffer_of_fragment file =
  let nb = Netbuffer.create 100 in
  begin
    Netbuffer.add_string nb open_fragment;
    let s = Gmisc.load_file file in
    (* let s = Whitespace.remove_trailing_spaces s in *)
    Netbuffer.add_string nb s;
    Netbuffer.add_string nb close_fragment;
    nb
  end

let typed_stream_of_fragment file =
  let buf = buffer_of_fragment file in
  let gio = Buffer_Input buf in
  typed_stream_of_io gio



(*******************)
(* Data structures *)
(*******************)

(* Configuration *)

type unit_config =
    { unit_id       : string;
      unit_name     : string;
      result_file   : string;
      testsuite_dir : string;
      catalog_dir   : string;
      query_dir     : string;
      sources       : (string,item list) Hashtbl.t;
(*      modules       : (string, string) Hashtbl.t; Maps module ID to file name *)
      expected_dir  : string;
      catalog_file  : string;
      catalog_doc   : item option ref;
      unit_version  : string ref;
      temp_groups   : string list;
      test_count    : int ref }

type test_config =
    { bugs_file   : string;
      units       : (string * unit_config) list }

(* Bugs *)

type bug_location =
  | GalaxBug
  | XQTSBug

type bug =
    { bug_name : string;
      bug_id   : int;
      bug_in   : bug_location }

(* Results *)

type eval_kind =
  | Eval_KnownBug of int * bug_location
  | Eval_Succeeded of item list
  | Eval_Failed of exn

type case_kind =
  | Case_Pass
  | Case_Pass_Inspect of string
  | Case_Fail of string
  | Case_Bug of int * string

type case_result =
    { case_result_name : string;
      case_result_kind : case_kind }

(* Catalog *)

type compare_kind =
  | XML_Comparison of string
  | Fragment_Comparison of string
  | Text_Comparison of string
  | Error_Comparison of string     (* Error code *)
  | Ignore_Comparison of string
  | Inspect_Comparison of string
(* Type_Comparison *)

type test_case =
    { test_name : string;
      test_path : string;
      test_scen : string;
      test_desc : string;
      test_qfil : string;
      test_mod_locs : (string * string) list;
      test_interface_locs : (string * string) list;
      test_ifil : (string * string) list;
      test_iuri : (string * string) list;
      test_contextItem : string option;
      test_qinp : (string * string) list;
      test_comp : compare_kind list;
      test_plan : string list }

type group_result = case_result list ref

type group_kind =
  | Subgroup of group list * group_result
  | Leafgroup of group_result

and group =
    { group_item : item;
      group_name : string;
      group_info : string;
      group_subs : group_kind }


(*********************)
(* Config processing *)
(*********************)

(* Create a new configuration *)

let make_unit_config id un rf td cd qd ed cad gps =
  let catdir = Filename.concat td cd in
  let querydir = Filename.concat catdir qd in
  let expecteddir = Filename.concat catdir ed in
  let catalogfile = Filename.concat catdir cad in
  { unit_id       = id;
    unit_name     = un;
    result_file   = rf;
    testsuite_dir = td;
    catalog_dir   = catdir;
    query_dir     = querydir;
    sources       = Hashtbl.create 59;
(*    modules       = Hashtbl.create 59; *)
    expected_dir  = expecteddir;
    catalog_file  = catalogfile;
    catalog_doc   = ref None;
    unit_version  = ref "";
    temp_groups   = gps;
    test_count    = ref 0 }

let make_test_config testdir bf rf unit_configs =
  Printf.printf "Loading configuration...";flush stdout;
  let bugsfile = Filename.concat testdir bf in
  let conf =
    { bugs_file   = bugsfile;
      units       = unit_configs }
  in
  conf

(* Those can be set explicitely *)

(* Configuration file *)

let eval_config query dot = 
  let ec = build_external_context (default_proc_ctxt()) (Some dot) None [] in
  let pp = prepare_program ccp (Some ec) in
  eval_statement pp (String_Input query)

let query_test_dir    = "./config/galax-test/testdir/string()"
let query_bug_file    = "./config/galax-test/bugfile/string()"
let query_units       = "./config/test-unit"

let query_id          = "./@id/string()"
let query_name        = "./@name/string()"
let query_result_file = "./resultfile/string()"
let query_testdir     = "./testdir/string()"
let query_catalogdir  = "./catalogdir/string()"
let query_catalog     = "./catalog/string()"
let query_querydir    = "./querydir/string()"
let query_expecteddir = "./expecteddir/string()"
let query_groups      = "./groups/test-group/@name/string()"

let id unit          = eval_to_string eval_config query_id unit
let name unit        = eval_to_string eval_config query_name unit
let result_file unit = eval_to_string eval_config query_result_file unit
let testdir unit     = eval_to_string eval_config query_testdir unit
let catalogdir unit  = eval_to_string eval_config query_catalogdir unit
let catalog unit     = eval_to_string eval_config query_catalog unit
let querydir unit    = eval_to_string eval_config query_querydir unit
let expecteddir unit = eval_to_string eval_config query_expecteddir unit
let groups unit      = eval_to_string_list eval_config query_groups unit

let make_config_from_unit unit =
  let id = id unit in
  let name = name unit in
  let rf = result_file unit in
  let testdir = testdir unit in
  let catalogdir = catalogdir unit in
  let catalog = catalog unit in
  let querydir = querydir unit in
  let expecteddir = expecteddir unit in
  let groups = groups unit in
  (id,make_unit_config
     id
     name
     rf
     testdir
     catalogdir
     querydir
     expecteddir
     catalog
     groups)

let make_test_config configdoc =
  let test_dir = eval_to_string eval_config query_test_dir configdoc in
  let bug_file = eval_to_string eval_config query_bug_file configdoc in
  let units = eval_to_item_list eval_config query_units configdoc in
  make_test_config
    test_dir
    bug_file
    result_file
    (List.map make_config_from_unit units)

let top_make_test_config file =
  try
    let make_config_doc config_file =
      Item_Node (List.nth (load_document proc_ctxt (File_Input config_file)) 0)
    in
    let configdoc = make_config_doc file in
    let c = make_test_config configdoc in
    begin
      Printf.printf "done.\n\n";flush stdout;
      c
    end
  with
  | e -> raise(Query(Internal_Error("In test_core.test_make_test_config"^(bprintf_error "  " e)^"\n")))

(**********************)
(* Catalog processing *)
(**********************)

(* The catalog *)

let catalog_prolog =
  "declare default element namespace \"http://www.w3.org/2005/02/query-test-XQTSCatalog\";"

let dcp = load_prolog (String_Input catalog_prolog)

let eval_catalog query dot =
  let ec = build_external_context (default_proc_ctxt()) (Some dot) None [] in
  let pp = prepare_program dcp (Some ec) in
  eval_statement pp (String_Input query)

let query_version = "./test-suite/@version/string()"

let query_schemas = "./test-suite/sources/schema"
let query_hint_id = "./@uri/string()"
let query_hint_loc = "./@FileName/string()"

let query_modules = "./test-suite/sources/module"
let query_interfaces = "./test-suite/sources/interface"

let add_schema_location_hints dir catalog =
  let get_location_hint hint =
    (eval_to_string eval_catalog query_hint_id hint,
     Filename.concat dir (eval_to_string eval_catalog query_hint_loc hint))
  in
  let schemas = eval_to_item_list eval_catalog query_schemas catalog in
  let lhints = List.map get_location_hint (schemas) in
  Processing_context.add_schema_location_hints proc_ctxt lhints

(*
let my_set_module_inputs dir catalog =
  let add_module_input module =
    (eval_to_string eval_catalog query_hint_id hint,
     Filename.concat dir (eval_to_string eval_catalog query_hint_loc hint))
  in
  let modules = eval_to_item_list eval_catalog query_modules catalog in
  List.iter add_module_input (modules)
*)

let make_catalog config () =
  try
    Printf.printf "Loading catalog...";flush stdout;
    let ct = config.catalog_doc in
    match !ct with
    | Some _ -> ()
    | None ->
	let catalog =
	  Item_Node
	    (List.nth (load_document proc_ctxt (File_Input config.catalog_file)) 0)
	in
	begin
	(* Added here to avoid re-loading it... *)
	  Hashtbl.add config.sources "XQTSCatalog" [catalog];
	  let version =
	    try
	      eval_to_string eval_catalog query_version catalog
	    with
	    | _ -> "" (* Not provided *)
	  in
	  config.unit_version := version;
	  add_schema_location_hints config.catalog_dir catalog;
(*	  my_set_module_inputs config.catalog_dir catalog; *)
	  ct := Some catalog;
	  Printf.printf "done.\n\n";flush stdout
	end
  with
  | e -> raise(Query(Internal_Error("In test_core.make_catalog"^(bprintf_error "  " e)^"\n")))

let get_catalog_from_config config =
  match !(config.catalog_doc) with
  | None -> raise_harness_error ("Catalog for unit [" ^ config.unit_id ^ "] not found")
  | Some doc -> doc

(* Generic catalog processing code *)

let query_initial_group = "./test-suite/test-group"
let query_group_name = "./@name/string()"
let query_group_info = "./GroupInfo/title/string()"
let query_group_subs = "./test-group"
let query_group_case = ".//test-case"
let query_group_case_here = "./test-case"

let query_case_name = "./@name/string()"
let query_case_path = "./@FilePath/string()"
let query_case_scen = "./@scenario/string()"
let query_case_desc = "./description/string()"
let query_case_qfil = "./query/@name/string()"
let query_case_mod = "./module"
let query_case_interface = "./interface"
let query_case_namespace = "./@namespace/string()"
let query_case_inpt = "./input-file"
let query_case_inpt_uri = "./input-URI"
let input_query_case_inpt = "./input-query"
let query_case_ifil = "./string()"
let query_case_ivar = "./@variable/string()"
let input_query_case_ifil = "./@name/string()"
let input_query_case_ivar = "./@variable/string()"
let query_contextItem = "./contextItem/string()"

let query_expected = "expected-error | output-file[@role=\"principal\"]"
let plan_expected  = "output-file[@role=\"plan\"]"

let query_case_comp = "if (fn:exists(self::expected-error)) then \"Error\" else self::output-file/@compare/string()"
let query_case_ofil = "if (fn:exists(self::expected-error)) then self::expected-error/string() else self::output-file/string()"

let query_initial_sources = "./test-suite/sources"

let build_hint inpt =
(* print_string ("In build_hint\n"); *)
  let m =  eval_to_string eval_catalog query_case_ifil inpt in
  let ns = eval_to_string eval_catalog query_case_namespace inpt in
(*  print_string ("Module namespace "^ns^" id "^m^"\n"); *)
  (ns,m) 

let build_input_file inpt =
  let ifil = eval_to_string eval_catalog query_case_ifil inpt in
  let ivar = eval_to_string eval_catalog query_case_ivar inpt in
  (ivar,ifil)

let build_input_query_file inpt =
  let ifil = eval_to_string eval_catalog input_query_case_ifil inpt in
  let ivar = eval_to_string eval_catalog input_query_case_ivar inpt in
  (ivar,ifil)

let make_comparison_kind expected =
  let comp = eval_to_string eval_catalog query_case_comp expected in
  let ofil = eval_to_string eval_catalog query_case_ofil expected in
  match comp with
  | "XML"      -> XML_Comparison ofil
  | "Fragment" -> Fragment_Comparison ofil
  | "Text"     -> Text_Comparison ofil
  | "Error"    -> Error_Comparison ofil
  | "Ignore"   -> Ignore_Comparison ofil
  | "Inspect"  -> Inspect_Comparison ofil
  | _ ->
      raise_test_error ("Unknown kind of comparison: [" ^ comp ^ "]")

let make_plan_comparison_kind expected =
  let comp = eval_to_string eval_catalog query_case_comp expected in
  let ofil = eval_to_string eval_catalog query_case_ofil expected in
  match comp with
  | "Plan"     -> ofil
  | _ ->
      raise_test_error ("Unknown kind of plan comparison: [" ^ comp ^ "]")

let get_contextItem case cn =
  eval_to_opt_string eval_catalog query_contextItem case

let lookup_module_file config sourceid =
(*   print_string ("lookup_module_file "^(sourceid)^"\n");  *)
  let unit_catalog = get_catalog_from_config config in
  let init_sources = eval_to_item eval_catalog query_initial_sources unit_catalog in
  let query_module = "./module[@ID=\""^ sourceid ^"\"]/@FileName/string()" in
  let m = eval_catalog query_module init_sources in
  let file = Filename.concat config.catalog_dir ((get_string m)^".xq") in
  file

let lookup_interface_file config sourceid =
(*   print_string ("lookup_interface_file "^(sourceid)^"\n");  *)
  let unit_catalog = get_catalog_from_config config in
  let init_sources = eval_to_item eval_catalog query_initial_sources unit_catalog in
  let query_interface = "./interface[@ID=\""^ sourceid ^"\"]/@FileName/string()" in
  let m = eval_catalog query_interface init_sources in
  let file = Filename.concat config.catalog_dir ((get_string m)^".xqi") in
  file

let build_case config case =
    try
    begin
    let name = eval_to_string eval_catalog query_case_name case in
  let path = eval_to_string eval_catalog query_case_path case in
  let scen = eval_to_string eval_catalog query_case_scen case in
  let desc = eval_to_string eval_catalog query_case_desc case in
  let qfil = eval_to_string eval_catalog query_case_qfil case in

(* Add module hints here.  Why aren't they defined in one global <module> declaration
   instead of in every test? 
*)
  let mod_locs = eval_to_item_list eval_catalog query_case_mod case in 
  let mod_locs = List.map (fun (ns,m) -> (ns, lookup_module_file config m)) (List.map build_hint mod_locs) in
  let interface_locs = eval_to_item_list eval_catalog query_case_interface case in 
  let interface_locs = List.map (fun (ns,m) -> (ns, lookup_interface_file config m)) (List.map build_hint interface_locs) in
  let inpt = eval_to_item_list eval_catalog query_case_inpt case in
  let inpt = List.map build_input_file inpt in
  let inpt_uri = eval_to_item_list eval_catalog query_case_inpt_uri case in
  let inpt_uri = List.map build_input_file inpt_uri in
  let qinp = eval_to_item_list eval_catalog input_query_case_inpt case in
  let qinp = List.map build_input_query_file qinp in
  let expected = eval_to_item_list eval_catalog query_expected case in
  let plan_expected = eval_to_item_list eval_catalog plan_expected case in
  let comp = List.map make_comparison_kind expected in
  let plan_comp = List.map make_plan_comparison_kind plan_expected in
  let ci = get_contextItem case name in
  { test_name = name;
    test_path = path;
    test_scen = scen;
    test_desc = desc;
    test_qfil = qfil ^ ".xq";
    test_mod_locs = mod_locs;
    test_interface_locs = interface_locs;
    test_ifil = inpt;
    test_iuri = inpt_uri;
    test_contextItem = ci;
    test_qinp = qinp;
    test_comp = comp;
    test_plan = plan_comp }
end
with 
| e -> (print_string((Error.bprintf_error "In build case" e)^"\n");
   raise e)

let rec build_groups group =
  let name = eval_to_string eval_catalog query_group_name group in
  let info = eval_to_string eval_catalog query_group_info group in
  let subgroups = eval_catalog query_group_subs group in
  let subs =
    List.map build_groups subgroups
  in
  { group_item = group;
    group_name = name;
    group_info = info;
    group_subs = if subgroups = [] then Leafgroup (ref []) else Subgroup (subs, ref []) }

let groups init_group =
  List.map build_groups init_group

let get_all_groups config () =
  let unit_catalog = get_catalog_from_config config in
  let init_group = eval_to_item_list eval_catalog query_initial_group unit_catalog in
  groups init_group


(*******************)
(* Bugs processing *)
(*******************)

let get_bugs_file config = config.bugs_file

let make_bugs_document bugs_file =
  try
    Item_Node (List.nth (load_document proc_ctxt (File_Input bugs_file)) 0)
  with
  | e -> raise(Query(Internal_Error("In test_core.make_bugs_document"^(bprintf_error "  " e)^"\n")))


let bcp = load_prolog (String_Input "")

let eval_bug query dot =
  let ec = build_external_context (default_proc_ctxt()) (Some dot) None [] in
  let pp = prepare_program bcp (Some ec) in
  eval_statement pp (String_Input query)

let query_bug       = "./bugs/bug[@status=\"open\"]"
let query_bug_loc   = "xs:string(./@in)"
let query_bug_id    = "xs:integer(./@id)"
let query_bug_names = "./test-case/@name/string()"


let make_bug_with_loc bug_id bug_loc testname =
  { bug_name = testname;
    bug_id = bug_id;
    bug_in = bug_loc }

let make_bug bug =
  let bug_id = eval_to_int eval_bug query_bug_id bug in
  let bug_loc =
    match eval_to_string eval_bug query_bug_loc bug with
    | "galax" -> GalaxBug
    | "xqts" -> XQTSBug
    | _ -> raise_test_error ("Unknow bug location in bugs file...")
  in
  let bug_names = eval_to_string_list eval_bug query_bug_names bug in
  List.map (fun x -> make_bug_with_loc bug_id bug_loc x) bug_names

let get_all_bugs config =
  let bugs_file = get_bugs_file config in
  let bugs_document = make_bugs_document bugs_file in
  let all_bugs = eval_to_item_list eval_bug query_bug bugs_document in
  List.concat ((List.map make_bug) all_bugs)


(*******************)
(* Compare results *)
(*******************)

let make_unexpected_fail case error =
  let name = case.test_name in
  let msg =
    match error with
    | Query err ->
	let errorname = bprintf_error_safe "" (Query err) in
	"Test case [" ^ name ^ "] raised error [" ^ errorname ^ "] :-("
    | _ ->
	"Test case [" ^ name ^ "] failed with a FATAL ERROR!! :-((("
  in
  { case_result_name = name;
    case_result_kind = Case_Fail msg }

let make_harness_fail case error =
  let name = case.test_name in
  let errorname = bprintf_error_safe "" error in
  let msg =
    "Test case or harness raised a really bad error [" ^ errorname ^ "] :( :("
  in
  { case_result_name = name;
    case_result_kind = Case_Fail msg }

let make_known_bug case error bug_loc =
  match bug_loc with
  | GalaxBug ->
      let name = case.test_name in
      let msg =
	"Known bug :-|"
      in
      { case_result_name = name;
	case_result_kind = Case_Bug (error,msg) }
  | XQTSBug ->
      let name = case.test_name in
      let msg =
	"[INSPECT] Known XQTS bug :-|"
      in
      { case_result_name = name;
	case_result_kind = Case_Pass_Inspect msg }

let make_unexpected_succeed query_file case result =
  let name = case.test_name in
  let msg =
    "Test case [" ^ name ^ "] in file [" ^ query_file ^ "] succeeded :-("
  in
  { case_result_name = name;
    case_result_kind = Case_Fail msg }

let make_differ_aux case output_file msg =
  let name = case.test_name in
  { case_result_name = name;
    case_result_kind = Case_Fail msg }

let make_differ case output_file =
  let name = case.test_name in
  let msg = "Result of case [" ^ name ^ "] and expected result in [" ^ output_file ^ "] differ :-(" in
  make_differ_aux case output_file msg

let make_pass case output_file compared_plan =
  match compared_plan with
  | None ->
      let name = case.test_name in
      { case_result_name = name;
	case_result_kind = Case_Pass }
  | Some t ->
      if t then
	let name = case.test_name in
	{ case_result_name = name;
	  case_result_kind = Case_Pass }
      else
	let name = case.test_name in
  	let msg = "Plan of case [" ^ name ^ "] and expected plan in [" ^ output_file ^ "] differ :-(" in
	make_differ_aux case output_file msg

let make_pass_inspect case =
  let name = case.test_name in
  { case_result_name = name;
    case_result_kind = Case_Pass_Inspect "[INSPECT]" }

let compare_streams case output_file result_stream expected_stream compared_plan =
  let comp =
    stream_boolean_diff result_stream expected_stream
  in
  if comp
  then
    make_pass case output_file compared_plan
  else
    make_differ case output_file

let try_serialize case result =
  match case.test_comp with
  | [Error_Comparison _] ->
      let result_stream =
	let result_stream1 =
	  Physical_export.typed_xml_stream_of_datamodel (Cursor.cursor_of_list result)
	in
	sequence_normalization result_stream1
      in
      discard_typed_xml_stream result_stream
  | _ -> ()

let compare_xml case output_file result compared_plan =
  let result_stream =
    let result_stream1 =
      Physical_export.typed_xml_stream_of_datamodel (Cursor.cursor_of_list result)
    in
    sequence_normalization result_stream1
  in
  try
    let expected_stream = typed_stream_of_io (File_Input output_file) in
    compare_streams case output_file result_stream expected_stream compared_plan
  with
  | Pxp_types.Error _ as e ->
      if !(Conf.genresults)
      then
	begin
	  let oc = open_out output_file in
	  let ff = Format.formatter_of_out_channel oc in
	  Serialization.fserialize_typed_xml_stream (default_proc_ctxt ()) ff result_stream;
	  make_pass_inspect case
	end
      else
	raise e

let compare_plan case output_file result compared_plan =
  let result_stream =
    let result_stream1 =
      Physical_export.typed_xml_stream_of_datamodel (Cursor.cursor_of_list result)
    in
    sequence_normalization result_stream1
  in
  try
    let expected_stream = typed_stream_of_io (File_Input output_file) in
    compare_streams case output_file result_stream expected_stream compared_plan
  with
  | Pxp_types.Error _ as e ->
      if !(Conf.genresults)
      then
	begin
	  let oc = open_out output_file in
	  let ff = Format.formatter_of_out_channel oc in
	  Serialization.fserialize_typed_xml_stream (default_proc_ctxt ()) ff result_stream;
	make_pass_inspect case
	end
      else
	raise e

let compare_many comp case output_file result compared_plan =
  try
    comp case output_file result compared_plan
  with
  | error ->
      make_harness_fail case error

let compare_fragments case output_file result compared_plan =
  let result_stream1 =
    Physical_export.typed_xml_stream_of_datamodel (Cursor.cursor_of_list result)
  in
  try
    let expected_stream = typed_stream_of_fragment output_file in
    let result_stream =
      let result_stream2 = glx_result_serialization result_stream1 in
      sequence_normalization result_stream2
    in
    let cs = compare_streams case output_file result_stream expected_stream compared_plan in
    cs
  with
  | Sys_error _ as e ->
      if !(Conf.genresults)
      then
	begin
	  let result_stream = sequence_normalization result_stream1 in
	  let oc = open_out output_file in
	  let ff = Format.formatter_of_out_channel oc in
	  Serialization.fserialize_typed_xml_stream (default_proc_ctxt ()) ff result_stream;
	  close_out oc;
	  make_pass_inspect case
	end
      else
	raise e
      
let compare_text_fragments = compare_fragments

let compare_error case expected_error actual_error compared_plan =
  (* Don't worry about it for now *)
  make_pass case actual_error compared_plan

let compare_results query_file case output_dir result test_comp compared_plan =
  match test_comp with
  | XML_Comparison ofil ->
      let output_file = Filename.concat output_dir ofil in
      begin
	match result with
	| Eval_KnownBug (bugid,bugloc) ->
	    make_known_bug case bugid bugloc
	| Eval_Succeeded result ->
	    compare_many compare_xml case output_file result compared_plan
	| Eval_Failed error ->
	    make_unexpected_fail case error
      end
  | Fragment_Comparison ofil ->
      let output_file = Filename.concat output_dir ofil in
      begin
	match result with
	| Eval_KnownBug (bugid,bugloc) ->
	    make_known_bug case bugid bugloc
	| Eval_Succeeded result ->
	    compare_many compare_fragments case output_file result compared_plan
	| Eval_Failed error ->
	    make_unexpected_fail case error
      end
  | Text_Comparison ofil ->
      let output_file = Filename.concat output_dir ofil in
      begin
	match result with
	| Eval_KnownBug (bugid,bugloc) ->
	    make_known_bug case bugid bugloc
	| Eval_Succeeded result ->
	    compare_many compare_text_fragments case output_file result compared_plan
	| Eval_Failed error ->
	    make_unexpected_fail case error
      end
  | Error_Comparison err ->
      begin
	match result with
	| Eval_KnownBug (bugid,bugloc) ->
	    make_known_bug case bugid bugloc
	| Eval_Succeeded result ->
	    make_unexpected_succeed query_file case result
	| Eval_Failed error ->
	    compare_many compare_error case error err compared_plan
      end
  | Ignore_Comparison ofil ->
      let output_file = Filename.concat output_dir ofil in
      begin
	match result with
	| Eval_KnownBug (bugid,bugloc) ->
	    make_known_bug case bugid bugloc
	| Eval_Succeeded result ->
	    make_pass case output_file compared_plan
	| Eval_Failed error ->
	    make_pass case output_file compared_plan
      end
  | Inspect_Comparison ofil ->
      make_pass_inspect case


(************************)
(* Test case processing *)
(************************)

let cases_of_group config group =
  let group_item = group.group_item in
  let subcase = eval_catalog query_group_case group_item in
  List.map (build_case config) subcase

let cases_of_group_here config group =
  let group_item = group.group_item in
  let subcase = eval_catalog query_group_case_here group_item in
  List.map (build_case config) subcase

let rec group_iter st cond f all_groups =
  match all_groups with
  | [] -> ()
  | g :: groups' ->
      let subgroups =
	match g.group_subs with
	| Subgroup (group_list,_) -> group_list
	| Leafgroup _ -> []
      in
      let st' =
	st || (g.group_name = "StaticTyping")
      in
      if (cond g)
      then
	begin
	  (f st' g);
	  (group_iter st' (fun x -> true) f subgroups);
	  (group_iter st cond f groups')
	end
      else
	begin
	  (group_iter st cond f (subgroups @ groups'))
	end

let rec group_map_concat cond f all_groups =
  match all_groups with
  | [] -> []
  | g :: groups' ->
      let subgroups =
	match g.group_subs with
	| Subgroup (group_list,_) -> group_list
	| Leafgroup _ -> []
      in
      if (cond g)
      then
	begin
	  (f g) @ (group_map_concat (fun x -> true) f subgroups) @ (group_map_concat cond f groups')
	end
      else
	(group_map_concat cond f (subgroups @ groups'))

let rec group_map cond f all_groups =
  match all_groups with
  | [] -> []
  | g :: groups' ->
      let subgroups =
	match g.group_subs with
	| Subgroup (group_list,_) -> group_list
	| Leafgroup _ -> []
      in
      if (cond g)
      then
	begin
	  (f g) :: (group_map (fun x -> true) f subgroups) @ (group_map cond f groups')
	end
      else
	(group_map cond f (subgroups @ groups'))

let rec find_group_by_name_aux all_groups group_name =
  match all_groups with
  | [] -> []
  | g :: groups' ->
      let subgroups =
	match g.group_subs with
	| Subgroup (group_list,_) -> group_list
	| Leafgroup _ -> []
      in
      if g.group_name = group_name
      then
	g :: (find_group_by_name_aux (subgroups @ groups') group_name)
      else
	find_group_by_name_aux (subgroups @ groups') group_name

let find_group_by_name all_groups group_name =
  try
    let groups = find_group_by_name_aux all_groups group_name in
    List.nth groups 0
  with
  | _ ->
      raise_test_error ("Group [" ^ group_name ^ "] is unknown")

let cases_of_group_name config all_groups group_name =
  let group = find_group_by_name all_groups group_name in
  cases_of_group config group

let lookup_source_file config sourceid =
  let unit_catalog = get_catalog_from_config config in
  let init_sources = eval_to_item eval_catalog query_initial_sources unit_catalog in
  let query_source = "./source[@ID=\""^ sourceid ^"\"]/@FileName/string()" in
  eval_to_string eval_catalog query_source init_sources

let create_uri_var config (var,sourceid) =
  let input_file = Filename.concat config.catalog_dir (lookup_source_file config sourceid) in
  let input_uri = [Item_Atomic (new Dm_atomic.atomicString(input_file))]
  in (var,input_uri)

let create_var config (var,sourceid) =
  try
  if Hashtbl.mem config.sources sourceid
  then
    (var,Hashtbl.find config.sources sourceid)
  else
    let input_file = Filename.concat config.catalog_dir (lookup_source_file config sourceid) in
    let input_doc =
      [(Item_Node (List.nth (load_document proc_ctxt (File_Input input_file)) 0))]
    in
    Hashtbl.add config.sources sourceid input_doc;
    (var,input_doc)
  with
  | e -> raise(Query(Internal_Error("In test_core.create_var["^var^"]"^(bprintf_error "  " e)^"\n")))

let create_ci config sid =
try
  match sid with
  | None -> None
  | Some sourceid ->
      if Hashtbl.mem config.sources sourceid
      then
	Some (List.nth (Hashtbl.find config.sources sourceid) 0)
      else
	let input_file =
	  Filename.concat config.catalog_dir (lookup_source_file config sourceid) in
	let input_doc =
	  (Item_Node (List.nth (load_document proc_ctxt (File_Input input_file)) 0))
	in
	Hashtbl.add config.sources sourceid [input_doc];
	Some input_doc
  with
  | e -> raise(Query(Internal_Error("In test_core.create_ci"^(bprintf_error "  " e)^"\n")))


let create_qinp_var case_dir config (var,sourceid) =
  try
    if Hashtbl.mem config.sources sourceid
    then
      (var,Hashtbl.find config.sources sourceid)
    else
      let f = Filename.concat case_dir (sourceid ^ ".xq") in
      let input_doc =
	let (program,statements) = import_main_module false (cp ()) (File_Input f) in
	let statement = List.nth statements 0 in
	let pp = prepare_program program None in
	eval_compiled_statement pp statement
      in
      Hashtbl.add config.sources sourceid input_doc;
      (var,input_doc)
  with
  | e ->
      raise(Query(Internal_Error("In test_core.create_qinp_var "^(bprintf_error "  " e)^"\n")))

let wrap_result all_bugs case eval_case (query_file, mod_locs, interface_locs, vars) ci create_prolog_var prologvars =
  try
    let bug =
      List.find (fun x -> x.bug_name = case.test_name) all_bugs
    in
    Eval_KnownBug (bug.bug_id,bug.bug_in)
  with
  | Not_found ->
      begin
	try
	  (* Printf.printf "[Eval][file] %s" query_file; flush stdout; *)
	  let vars_qinp = List.map create_prolog_var prologvars in
	  let result = eval_case (query_file, mod_locs, interface_locs, (vars @ vars_qinp)) ci in
	  begin
	    try_serialize case result;
	    Eval_Succeeded result
	  end
	with
	| error ->
	    Eval_Failed error
      end

let eval_case (query, mod_locs, interface_locs, vars) ci =
  try
    let have_contextItem = match ci with | None -> false | Some _ -> true in
    let proc_ctxt' = copy_processing_context proc_ctxt in
    let _ = Processing_context.add_module_location_hints proc_ctxt' mod_locs in 
    let _ = Processing_context.add_interface_location_hints proc_ctxt' interface_locs in 
(* Printf.printf("Location hints\n"); 
    Processing_context.print_location_hints (Processing_context.get_module_location_hints proc_ctxt');
    Processing_context.print_location_hints (Processing_context.get_interface_location_hints proc_ctxt');
*)
    let (program,statements) = import_main_module have_contextItem (load_standard_library proc_ctxt') (File_Input query) in
    let statement = List.nth statements 0 in
    let ec = build_external_context proc_ctxt' ci None vars in
    let pp = prepare_program program (Some ec) in
    eval_compiled_statement pp statement
  with
  | e -> raise(Query(Internal_Error("In test_core.eval_case "^query^" " ^(bprintf_error "  " e)^"\n")))

let eval_plan (query, mod_locs, interface_locs, vars) ci =
  try
    let have_contextItem = match ci with | None -> false | Some _ -> true in
(* Module location hints needs to be set here! *)
(* Why are we using two different processing contexts here? *)
    let proc_ctxt' = copy_processing_context proc_ctxt in
    let _ = Processing_context.add_module_location_hints proc_ctxt' mod_locs in 
    let _ = Processing_context.add_interface_location_hints proc_ctxt' interface_locs in 
    let (comp_prog,statements) = import_main_module have_contextItem (load_standard_library proc_ctxt') (File_Input query) in
    let nsenv = nsenv_of_main_module comp_prog in
    serialize_logical_module nsenv (main_module_of_compiled_program comp_prog)
  with
  | e -> raise(Query(Internal_Error("In test_core.eval_case "^query^" " ^(bprintf_error "  " e)^"\n")))

let rec merge_results case compared_results =
  match compared_results with
  | [] -> raise_harness_error ("Not expected result for test" ^ case.test_name)
  | res :: [] -> res
  | res1 :: others ->
      begin
	match res1.case_result_kind with
	| Case_Fail _ -> merge_results case others
	| _ -> res1
      end

let rec merge_plan_results case compared_results =
  match compared_results with
  | [] -> None
  | res :: [] ->
      begin
	match res.case_result_kind with
	| Case_Pass | Case_Pass_Inspect _ -> Some true
	| _ -> Some false
      end
  | res1 :: others ->
      begin
	match res1.case_result_kind with
	| Case_Fail _ -> merge_plan_results case others
	| _ -> Some true
      end

let run_case_eval config all_bugs case eval_fun =
(*  print_string (case.test_name^"\n"); *)
  try
    let count = config.test_count in
    incr count;
    if (!count) mod 10 = 0 then (Printf.printf ".";flush stdout);
    let case_dir = Filename.concat config.query_dir case.test_path in
    let query_file = Filename.concat case_dir case.test_qfil in
    let vars = List.map (create_var config) case.test_ifil in
    let vars' = vars @ (List.map (create_uri_var config) case.test_iuri) in
    let ci = create_ci config case.test_contextItem in
    wrap_result all_bugs case eval_fun (query_file, case.test_mod_locs, case.test_interface_locs, vars') ci (create_qinp_var case_dir config) case.test_qinp
  with
  | exn -> Eval_Failed exn

let build_compare_plans config all_bugs case output_dir test_plans =
  let test_plans = Filename.concat output_dir test_plans in
  let result = run_case_eval config all_bugs case eval_plan in
  match result with
  | Eval_KnownBug (bugid,bugloc) -> make_known_bug case bugid bugloc
  | Eval_Failed error -> make_unexpected_fail case error
  | Eval_Succeeded result -> compare_many compare_plan case test_plans result None

let run_case config all_bugs case =
  let case_dir = Filename.concat config.query_dir case.test_path in
  let output_dir = Filename.concat config.expected_dir case.test_path in
  let query_file = Filename.concat case_dir case.test_qfil in
  let result = run_case_eval config all_bugs case eval_case in
  let test_plans = case.test_plan in
  let test_comps = case.test_comp in
  let compared_plans =
    List.map (fun x -> build_compare_plans config all_bugs case output_dir x) test_plans
  in
  let compared_plan = merge_plan_results case compared_plans in
  let compared_results = List.map (fun x -> compare_results query_file case output_dir result x compared_plan) test_comps in
  merge_results case compared_results

let fix_name name =
  let l = String.length name in
  name ^ (String.make (28-l) ' ')

let run_group_cases st name config all_bugs cases =
  let count = config.test_count in
  (Printf.printf "%s." (fix_name name);flush stdout);
  let typing_kind = proc_ctxt.typing_kind in
  if st then proc_ctxt.typing_kind <- Typing_Strong;   
  let results = List.map (run_case config all_bugs) cases in
  (Printf.printf "%i\n" !count;flush stdout);
  proc_ctxt.typing_kind <- typing_kind;   
  results

let clean_groups all_groups =
  let cond g = true in
  let f st g =
    match g.group_subs with
    | Subgroup (_,group_result) -> group_result := []
    | Leafgroup group_result ->
	group_result := []
  in
  group_iter false cond f all_groups

let run_groups config all_bugs =
  Printf.printf "Running tests now:\n\n";flush stdout;
  let group_names = config.temp_groups in
  let all_groups = get_all_groups config () in
  let cond g = List.exists (fun x -> g.group_name = x) group_names in
  let f st g =
    match g.group_subs with
    | Subgroup (_,group_result) ->
	let cases = cases_of_group_here config g in
	let result = run_group_cases st g.group_name config all_bugs cases in
	group_result := result
    | Leafgroup group_result ->
	let cases = cases_of_group config g in
	let result = run_group_cases st g.group_name config all_bugs cases in
	group_result := result
  in
  group_iter false cond f all_groups;
  Printf.printf "Number of tests run: %i\n\n" (!(config.test_count));flush stdout;
  all_groups

let is_failed case =
  match case.case_result_kind with
  | Case_Fail _ -> true
  | _ -> false

let is_bug case =
  match case.case_result_kind with
  | Case_Bug _ -> true
  | _ -> false

let is_passed case =
  match case.case_result_kind with
  | Case_Pass | Case_Pass_Inspect _ -> true
  | _ -> false

let is_not_passed case =
  match case.case_result_kind with
  | Case_Pass | Case_Pass_Inspect _ -> false
  | _ -> true

let get_all_results_with_kind all_groups kind_test =
  let cond g = true in
  let f g =
    match g.group_subs with
    | Subgroup (_,group_result) -> List.filter kind_test !group_result
    | Leafgroup group_result ->
	List.filter kind_test !group_result
  in
  group_map_concat cond f all_groups

let get_all_results all_groups =
  get_all_results_with_kind all_groups (fun x -> true)

let get_all_failed_results all_groups =
  get_all_results_with_kind all_groups is_failed

let get_all_bug_results all_groups =
  get_all_results_with_kind all_groups is_bug

let get_all_passed_results all_groups =
  get_all_results_with_kind all_groups is_passed

let get_all_not_passed_results all_groups =
  get_all_results_with_kind all_groups is_not_passed

let all_group_names all_groups =
  let cond g = true in
  let f g = g.group_name in
  group_map cond f all_groups


(*********************)
(* Result processing *)
(*********************)

let eval_result query vars =
  let (program,statements) = import_main_module false (defaultcp()) (String_Input query) in
  let statement = List.nth statements 0 in
  let ec = build_external_context (default_proc_ctxt()) None None vars in
  let pp = prepare_program program (Some ec) in
  eval_compiled_statement pp statement

let prepare_plan query =
  try 
    let (program,statements) = import_main_module false (defaultcp()) (String_Input query) in
    let statement = List.nth statements 0 in
    (program,statement)
  with
  | e ->
      begin
	eprintf_error "In Test_core.prepare_plan " e;
	Format.fprintf (!Conf.glx_err_formatter) "@.";
	exit 1; 
      end

let eval_result_from_plan (program,statement) vars =
  let ec = build_external_context (default_proc_ctxt()) None None vars in
  let pp = prepare_program program (Some ec) in
  eval_compiled_statement pp statement

let eval_result_as_sax query vars =
  let (program,statements) = import_main_module false (defaultcp()) (String_Input query) in
  let statement = List.nth statements 0 in
  let ec = build_external_context (default_proc_ctxt()) None None vars in
  let pp = prepare_program program (Some ec) in
  eval_compiled_statement_as_sax pp statement

let query_make_passed =
  prepare_plan "declare default element namespace \"http://www.w3.org/2005/02/query-test-XQTSResult\";declare variable $name external;(text { \"\n  \" },<test-case name=\"{$name}\" result=\"pass\"/>)"
let query_make_passed_plan =
  prepare_plan"declare default element namespace \"http://www.w3.org/2005/02/query-test-XQTSResult\";declare variable $name external;(text { \"\n  \" },<test-case name=\"{$name}\" result=\"pass\" role=\"plan\"/>)"
let query_make_passed_inspect =
  prepare_plan"declare default element namespace \"http://www.w3.org/2005/02/query-test-XQTSResult\";declare variable $name external;declare variable $comment external;(text { \"\n  \" },<test-case name=\"{$name}\" result=\"pass\" comment=\"{$comment}\"/>)"
let query_make_failed =
  prepare_plan "declare default element namespace \"http://www.w3.org/2005/02/query-test-XQTSResult\";declare variable $name external;declare variable $comment external;(text { \"\n  \" },<test-case name=\"{$name}\" result=\"fail\" comment=\"{$comment}\"/>)"
let query_make_bug =
  prepare_plan "declare default element namespace \"http://www.w3.org/2005/02/query-test-XQTSResult\";declare variable $name external;declare variable $comment external;declare variable $bug external;(text { \"\n  \" },<test-case name=\"{$name}\" result=\"fail\" comment=\"{$comment} [http://bugzilla.galaxquery.net/show_bug.cgi?id={$bug}]\"/>)"

let create_result_passed case_name =
  let vars = [("name",make_from_string case_name)] in
  eval_result_from_plan query_make_passed vars

let create_result_passed_plan case_name =
  let vars = [("name",make_from_string case_name)] in
  eval_result_from_plan query_make_passed_plan vars

let create_result_passed_inspect case_name msg =
  let vars = [("name",make_from_string case_name);("comment",make_from_string msg)] in
  eval_result_from_plan query_make_passed_inspect vars

let create_result_failed case_name msg =
  let vars = [("name",make_from_string case_name);("comment",make_from_string msg)] in
  eval_result_from_plan query_make_failed vars

let create_result_bug case_name msg bug =
  let vars = [("name",make_from_string case_name);("comment",make_from_string msg);("bug",(make_from_int bug))] in
  eval_result_from_plan query_make_bug vars

let create_case_result case_result =
  let case_name = case_result.case_result_name in
  match case_result.case_result_kind with
  | Case_Pass ->
      create_result_passed case_name
  | Case_Pass_Inspect msg ->
      create_result_passed_inspect case_name msg
  | Case_Fail msg ->
      create_result_failed case_name msg
  | Case_Bug (bug,msg) ->
      create_result_bug case_name msg bug

let query_result version =
  let date =
    DateTime.string_of_date
      (DateTime.date_from_dateTime (DateTime.current_dateTime()))
  in
  "declare default element namespace \"http://www.w3.org/2005/02/query-test-XQTSResult\";declare variable $name external;declare variable $result external;(text { \"\n\n  \"},<test-run dateRun=\"" ^ date ^ "\"><test-suite version=\"" ^ version ^ "\"/></test-run>,text { \"\n  \"},$result)"

let total_results name version case_results =
  let l = List.concat (List.map create_case_result case_results) in
  let vars = [("name",make_from_string name);("result",l)] in
  eval_result (query_result version) vars

let query_final_result =
  "declare default element namespace \"http://www.w3.org/2005/02/query-test-XQTSResult\";declare variable $result external;<test-suite-result>{text { \"\n \"}}<implementation name=\"Galax\" version=\""^Conf.version^"\" anonymous-result-column=\"false\">{text { \"\n  \"}}<organization name=\"Galax Team\" website=\""^Conf.galax_web^""/>{text { \"\n  \"}}<submittor name=\"Jerome Simeon\"/></implementation>{$result,text{\"\n\"}}</test-suite-result>"

let total_result res =
  let (x,v,y) = res in
  let l = total_results x v y in
  let vars = ["result",l] in
  eval_result_as_sax query_final_result vars

