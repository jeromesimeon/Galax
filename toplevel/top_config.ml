(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: top_config.ml,v 1.51 2007/10/25 00:08:42 mff Exp $ *)

(* Module: Top_config
   Description:
     This module implements support for handling configuration of
     Galax from the top-level.
 *)

open Error

open Processing_context
open Xquery_common_ast


(**********************)
(* Context parameters *)
(**********************)

let context_file = ref (None)
let context_item = ref (None)
let global_vars = ref []
let global_docs = ref []


(**************************)
(* Printing configuration *)
(**************************)

let output_all = ref false


(*****************************)
(* galax-parse configuration *)
(*****************************)

let validation 	  = ref false     (* True if validating *)
let dtd	          = ref false     (* True if validating against an embedded DTD *)
let diff          = ref false
let schemafile    = ref None      (* What schema should be used for validation *)
let pxp  	  = ref true      (* True if doing PXP parsing *)
let stream        = ref true      (* True if doing Galax stream parsing *)
let resolve  	  = ref true      (* True if doing namespace resolution *)
let annotate  	  = ref false     (* True if doing annotating with types *)
let erase  	  = ref false     (* True if erasing *)
let prefix  	  = ref false     (* True if prefixing *)
let load       	  = ref false     (* True if building the data model instance *)
let export     	  = ref false     (* True if exporting data model to stream *)

let prefix_opt 	  = ref None 
(* XML Schema - XQuery types processing phases *)
let import_type            = ref true
let normalize_type         = ref false
let print_type             = ref true
let print_normalized_type  = ref false
let output_type		   = ref None  (* Set to a filename *)
let output_normalized_type = ref None  (* Set to a filename *)

(*********************************)
(* Argument Processing functions *)
(*********************************)

type exec_kind =
  | Exec_Standard
  | Exec_Normalized
  | Exec_Logical
  | Exec_Physical

let execute_arg =
  [ ("standard",Exec_Standard);
    ("normalized",Exec_Normalized);
    ("logical",Exec_Logical);
    ("physical",Exec_Physical) ]

let error_arg =
  [ ("standard",true);
    ("code",false) ]

let plan_arg =
  [ ("standard",true);
    ("xml",false) ]

let onoff_arg =
  [ ("on",  true);
    ("off", false) ]

let sbdo_arg =
  [ ("preserve", SBDO_Preserve);
    ("remove",   SBDO_Remove);
    ("adhoc",    SBDO_AdHoc);      (* Default *)
    ("tidy",     SBDO_Tidy);
    ("duptidy",  SBDO_DupTidy);
    ("sloppy",   SBDO_Sloppy) ]

let language_arg =
  [ ("xquery10",   Conf.Language_XQuery10);    (* Default *)
    ("ultf",       Conf.Language_XQueryUpdates);
    ("xquerybang", Conf.Language_XQueryBang);
    ("xqueryp",    Conf.Language_XQueryP);
    ("dxq",        Conf.Language_DXQ) ]

let syntax_arg =
  [ ("xquery",     Conf.XQuery_Syntax);    (* Default *)
    ("xqueryx",    Conf.XQueryX_Syntax) ]

let typing_arg =
  [ ("none",   Typing_None);    (* Default *)
    ("weak",   Typing_Weak);
    ("strong", Typing_Strong) ]

let serialization_arg =
  [ ("standard",  Serialize_As_Standard);
    ("wf",     	  Serialize_As_Well_Formed); (* Default API *)
    ("xquery", 	  Serialize_As_XQuery);      (* Default command-line *)
    ("canonical", Serialize_As_Canonical) ]

let boundary_space_kind_arg =
  [ ("strip",     Strip);       (* Default *)
    ("preserve",  Preserve) ]

let construction_kind_arg =
  [ ("strip",     Strip);
    ("preserve",  Preserve) ]   (* Default *)

let ordering_kind_arg =
  [ ("ordered",    Ordered);    (* Default *)
    ("unordered",  Unordered) ]

let default_order_kind_arg =
  [ ("emptygreatest", EmptyGreatest);  (* Default *)
    ("emptyleast",    EmptyLeast) ]

let ns_preserve_kind_arg =
  [ ("preserve",  NSPreserve);         (* Default *)
    ("no-preserve",  NSNoPreserve) ]

let ns_inherit_kind_arg =
  [ ("inherit",  NSInherit);           (* Default *)
    ("no-inherit",  NSNoInherit) ]

let projection_arg =
  [ ("none",      Projection_None);       (* Default *)
    ("standard",  Projection_Standard);
    ("optimized", Projection_Optimized) ]

let treejoin_log_arg =
  [ ("default", Default);
    ("treejoin", TreeJoin);
    ("twig", Twig) ]
 
 let treejoin_phys_arg =
  [ ("nestedloop", NestedLoop);
    ("scjoin", SCJoin);
    ("indexjoin", IndexSortJoin);
    ("twigjoin", TwigJoin);
    ("stream", Streaming) ]
    
 let materialize_tables_arg = 
   [ ("always", Conf.Always);
     ("analysis", Conf.Analysis);
     ("never", Conf.Never) ]

let compute_arg arg arg_map =
  try
    List.assoc arg arg_map
  with
  | Not_found ->
      raise (Query (Unknown ("Unknown parameter : " ^ arg)))

let bool_of_onoff onoff =
  compute_arg onoff onoff_arg

let execute_kind_of_arg kind =
  compute_arg kind execute_arg

let bool_of_error_arg kind =
  compute_arg kind error_arg

let bool_of_plan_arg kind =
  compute_arg kind plan_arg

let sbdo_kind_of_arg sbdo =
  compute_arg sbdo sbdo_arg

let language_kind_of_arg tp =
  compute_arg tp language_arg

let syntax_kind_of_arg tp =
  compute_arg tp syntax_arg

let typing_kind_of_arg tp =
  compute_arg tp typing_arg

let serialization_kind_of_arg ser =
    compute_arg ser serialization_arg

let boundary_space_kind_of_arg ser =
    compute_arg ser boundary_space_kind_arg

let construction_kind_of_arg ser =
    compute_arg ser construction_kind_arg

let ordering_kind_of_arg ser =
    compute_arg ser ordering_kind_arg

let default_order_kind_of_arg ser =
    compute_arg ser default_order_kind_arg

let ns_preserve_kind_of_arg ser =
    compute_arg ser ns_preserve_kind_arg

let ns_inherit_kind_of_arg ser =
    compute_arg ser ns_inherit_kind_arg

let projection_kind_of_arg p =
    compute_arg p projection_arg

let treejoin_log_of_arg p =
    compute_arg p treejoin_log_arg

let treejoin_phys_of_arg p =
    compute_arg p treejoin_phys_arg

let buffer_chunks_of_arg p =
    int_of_string p

let buffer_csize_of_arg p =
    int_of_string p

let buffer_inc_of_arg p =
    int_of_string p

let materialize_tables_kind_of_arg mat =
    compute_arg mat materialize_tables_arg


(***************************)
(* Command-line parameters *)
(***************************)


(* Set specific parameters in the processing context *)

let parse_debug_string s =
  let f s =
    try
      Debug.debug_flag_of_string s
    with
    | _ ->
	raise (Query (Unknown "Unknown debug flag"))
  in
  try
    let splits = Gmisc.split_on_char s ',' in
    List.map f splits
  with
  | _ ->
      raise (Query (Unknown "Malformed debug option"))
      

let set_debug_kind_arg proc_ctxt s = 
  Debug.set_debug (parse_debug_string s)

let set_normalization_phase_arg  proc_ctxt s = set_normalization_phase proc_ctxt (bool_of_onoff s)
let set_normalization_ident_arg  proc_ctxt s = set_normalization_ident proc_ctxt (bool_of_onoff s)
let set_typing_phase_arg         proc_ctxt s = set_typing_phase proc_ctxt (bool_of_onoff s)
let set_rewriting_phase_arg      proc_ctxt s = set_rewriting_phase proc_ctxt (bool_of_onoff s)
let set_factorization_phase_arg  proc_ctxt s = set_factorization_phase proc_ctxt (bool_of_onoff s)
let set_optimization_phase_arg   proc_ctxt s = set_optimization_phase proc_ctxt (bool_of_onoff s)
let set_code_selection_phase_arg proc_ctxt s = set_code_selection_phase proc_ctxt (bool_of_onoff s)
let set_evaluation_phase_arg     proc_ctxt s = set_evaluation_phase proc_ctxt (bool_of_onoff s)
let set_execute_kind_arg         proc_ctxt s =
  match (execute_kind_of_arg s) with
  | Exec_Standard -> ()
  | Exec_Normalized ->
      set_normalization_ident_arg proc_ctxt "on"
  | Exec_Logical ->
      begin
	Conf.load_xml_plans := true;
	Conf.execute_logical_plan := true
      end
  | Exec_Physical ->
      begin
	Conf.execute_optimized_plan := true;
	Conf.load_xml_plans := true
      end

let set_xml_whitespace_arg       proc_ctxt () = set_xml_whitespace proc_ctxt true
let set_xml_pis_and_comments_arg proc_ctxt () = set_xml_pis_and_comments proc_ctxt true
let unset_xml_whitespace_arg       proc_ctxt () = set_xml_whitespace proc_ctxt false
let unset_xml_pis_and_comments_arg proc_ctxt () = set_xml_pis_and_comments proc_ctxt false

let set_inline_functions_arg     proc_ctxt s = set_inline_functions proc_ctxt (bool_of_onoff s)
let set_inline_variables_arg     proc_ctxt s = set_inline_variables proc_ctxt (bool_of_onoff s)
let set_sbdo_kind_arg            proc_ctxt s = set_sbdo_kind proc_ctxt (sbdo_kind_of_arg s)
let set_language_kind_arg        proc_ctxt s =
  let language = (language_kind_of_arg s) in
  Conf.set_language language;
  if Conf.is_xquerybang () || Conf.is_xqueryp () || Conf.is_ultf() then
    begin
      set_typing_kind proc_ctxt Typing_None
    end
let set_syntax_kind_arg        proc_ctxt s =
  let syntax = (syntax_kind_of_arg s) in
  Conf.set_syntax syntax
let set_typing_kind_arg          proc_ctxt s = set_typing_kind proc_ctxt (typing_kind_of_arg s)
let set_serialization_kind_arg   proc_ctxt s = set_serialization_kind proc_ctxt (serialization_kind_of_arg s)
(*
let set_boundary_space_kind_arg  proc_ctxt s = set_boundary_space_kind proc_ctxt (boundary_space_kind_of_arg s)
let set_construction_kind_arg    proc_ctxt s = set_construction_kind proc_ctxt (construction_kind_of_arg s)
let set_ordering_kind_arg        proc_ctxt s = set_ordering_kind proc_ctxt (ordering_kind_of_arg s)
let set_default_order_kind_arg   proc_ctxt s = set_default_order_kind proc_ctxt (default_order_kind_of_arg s)
let set_ns_preserve_kind_arg     proc_ctxt s = set_ns_preserve_kind proc_ctxt (ns_preserve_kind_of_arg s)
let set_ns_inherit_kind_arg      proc_ctxt s = set_ns_inherit_kind proc_ctxt (ns_inherit_kind_of_arg s)
*)
let set_projection_kind_arg      proc_ctxt s = set_projection_kind proc_ctxt (projection_kind_of_arg s)

let set_treejoin_log_arg         proc_ctxt s = set_treejoin_log proc_ctxt (treejoin_log_of_arg s)
let set_treejoin_phys_arg        proc_ctxt s = set_treejoin_phys proc_ctxt (treejoin_phys_of_arg s)
let set_streaming_arg            proc_ctxt s = set_streaming proc_ctxt (bool_of_onoff s)
let set_infer_indepedence_arg    proc_ctxt s = set_infer_independence proc_ctxt (bool_of_onoff s)
let set_dxq_arg                  proc_ctxt s = set_dxq proc_ctxt (bool_of_onoff s)
let set_embed_xqueryx_arg        proc_ctxt s =
  Conf.embed_xqueryx := (bool_of_onoff s)
let set_error_arg                proc_ctxt s =
  Conf.verbose_error := not((bool_of_error_arg s))
let set_plan_arg                 proc_ctxt s =
  Conf.serialize_logical_algebra := not((bool_of_plan_arg s))

(*
let set_base_uri_arg proc_ctxt s =
  let uri = AnyURI._kinda_uri_of_string s in
  set_base_uri proc_ctxt (Some uri)
*)

let set_streaming_shebang proc_ctxt s =
  if (bool_of_onoff s) then
    begin
      set_typing_kind proc_ctxt Typing_Weak;
      set_treejoin_log proc_ctxt TreeJoin;
      set_treejoin_phys proc_ctxt Streaming;
      Conf.allow_streamed_tuple_fields := true;
      set_streaming proc_ctxt true;
      set_inline_functions proc_ctxt true
    end
  else 
    begin
      set_typing_kind proc_ctxt Typing_None;
      set_treejoin_log proc_ctxt Default;
      set_treejoin_phys proc_ctxt NestedLoop;
      Conf.allow_streamed_tuple_fields := false;
      set_streaming proc_ctxt false;
      set_inline_functions proc_ctxt false
    end

let set_scjoin_shebang proc_ctxt s =
  if (bool_of_onoff s) then
    begin
      set_typing_kind proc_ctxt Typing_Weak;
      set_treejoin_log proc_ctxt Twig;
      set_treejoin_phys proc_ctxt SCJoin;
      set_inline_variables proc_ctxt true
    end
  else  
    begin
      set_typing_kind proc_ctxt Typing_None;
      set_treejoin_log proc_ctxt Default;
      set_treejoin_phys proc_ctxt NestedLoop;
      set_inline_variables proc_ctxt false
    end

let set_twigjoin_shebang proc_ctxt s = 
  if (bool_of_onoff s) then
    begin
      set_typing_kind proc_ctxt Typing_Weak;
      set_treejoin_log proc_ctxt Twig;
      set_treejoin_phys proc_ctxt TwigJoin;
      set_inline_variables proc_ctxt false
    end
  else 
    begin
      set_typing_kind proc_ctxt Typing_None;
      set_treejoin_log proc_ctxt Default;
      set_treejoin_phys proc_ctxt NestedLoop;
      set_inline_variables proc_ctxt false
    end

let set_print_plan_shebang proc_ctxt s =
  if (bool_of_onoff s) 
  then 
     begin
      Conf.verbose := true;
      Conf.print_prolog := true;
      Conf.print_core_expr := true;
      Conf.print_optimized_expr := true;
      (* Conf.print_factorized_expr := true; *)
      Conf.print_logical_algebra := true;
      Conf.print_algebra_optimization_rewrite := true;
      Conf.print_optimized_algebra := true;
      Conf.print_physical_algebra := true;
    end
  else
    begin
      Conf.verbose := false;
      Conf.print_prolog := false;
      Conf.print_core_expr := false;
      Conf.print_optimized_expr := false;
      (* Conf.print_factorized_expr := false; *)
      Conf.print_logical_algebra := false;
      Conf.print_algebra_optimization_rewrite := false;
      Conf.print_optimized_algebra := false;
      Conf.print_physical_algebra := false;
    end
 

let set_materialize_tables_kind_arg proc_ctxt m = 
  Conf.set_materialize_tables (materialize_tables_kind_of_arg m)
