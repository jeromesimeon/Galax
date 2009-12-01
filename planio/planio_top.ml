(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: planio_top.ml,v 1.20 2007/10/16 01:25:35 mff Exp $ *)

(* 
   Module: Planio_top
   Description:
     This module parses the algebraic plans which are serialized by
     algebra_print_xml.ml so that they may be run again after
     modification
*)

open Compile_annotate
open Debug
open Dm_atomic

open Error
open Namespace_context
open Namespace_names
open Namespace_symbols
open Occurrence
open Planio_common
open Planio_util

open Small_stream_ast
open Streaming_constructors
open Streaming_types

open Xquery_common_ast
open Xquery_algebra_ast 
open Xquery_algebra_ast_util 


(***************)
(* Utility     *)
(***************)

(*------------*)
(*-- Unbox  --*)
(*------------*)

let insert_location_of_string str =
  match str with 
    | "LastInto" -> AOUAsLastInto
    | "FirstInto" -> AOUAsFirstInto 
    | "Into" -> AOUInto 
    | "After" -> AOUAfter 
    | "Before" -> AOUBefore
    | _ -> raise (Query (Malformed_Expr ("Unknown Location String '"^str^"'")))

let snap_modifier_of_string sm = 
  match sm with
    | "ordered deterministic" -> Snap_Ordered_Deterministic
    | "unordered deterministic" -> Snap_Unordered_Deterministic
    | "nondeterministic"      -> Snap_Nondeterministic  
    | _ -> raise (Query (Malformed_Expr ("Unknown Snap Modifier String '"^sm^"'")))


let value_flag_of_string str =
  match str with 
    |  "Normal"  -> Normal_Replace
    | "Value of" -> Value_Of_Replace 
    | _ -> raise (Query (Malformed_Expr ("Value of flag '"^str^"'")))

(* Strip off the document event and then parse it *)
let apply_parser_skip_document parser comp_ctxt the_stream =
  let st = Cursor.stream_of_cursor
      (Streaming_ops.resolve_xml_stream 
	 the_stream) in
  
  match Stream.peek st with
  | Some event ->
      begin
	match event.rse_desc with 
	  (RSAX_startDocument _ ) ->
	    Stream.junk st;
	    parser comp_ctxt st
	| _ -> 
	    raise (Query (Algebra_Parsing_Error ("No document start for serialized plan")))
      end
  | _ -> 
      raise (Query (Algebra_Parsing_Error ("No document start for serialized plan")))

(*------------*)
(*-- Boxing --*)
(*------------*)

let boxattr_stable stable =
  stable_attr_name, (
  match stable with
      Stable ->  "Stable"
    | NonStable -> "Unstable"
  )

let boxattr_insert_location loc = 
  let loc_str =
    match loc with 
      | AOUAsLastInto -> "LastInto"
      | AOUAsFirstInto -> "FirstInto"
      | AOUInto -> "Into"
      | AOUAfter -> "After"
      | AOUBefore -> "Before"
  in
    insert_location_attr_name, loc_str

let boxattr_snap_modifier sm = 
  let sm_str = 
    match sm with
      | Snap_Ordered_Deterministic   -> "ordered deterministic"
      | Snap_Unordered_Deterministic -> "unordered deterministic"
      | Snap_Nondeterministic   -> "nondeterministic"
  in
    snap_modifier_attr_name, sm_str

let boxattr_value_of_flag avf =
  let avf_str = 
    match avf with
      | Normal_Replace -> "Normal"
      | Value_Of_Replace -> "Value of"
  in
    value_of_flag_attr_name, avf_str

let boxattr_sortkind sk =
  let sk_str  = string_of_sortkind sk in
    sort_kind_attr_name, sk_str 

let boxattr_emptysortkind esk =
  let esk_str = string_of_emptysortkind esk in
    empty_sort_kind_attr_name, esk_str 

(* Predicates *)
let rec box_predicate_desc pd =
  match pd with
    | SimpleConjunct (s,e) ->
	let start_attr = simple_conjunct_start_attr_name, (string_of_int s) in
	let end_attr   = simple_conjunct_end_attr_name  , (string_of_int e) in
	  construct_element simple_conjunct_elem_name (start_attr :: end_attr :: []) []
    | ComplexConjunct (l,r) ->
	let l_elem = box_predicate_desc l in
	let r_elem = box_predicate_desc r in
	  construct_element complex_conjunct_elem_name [] (l_elem :: r_elem :: [])
    | Disjunct (l,r) ->
	let l_elem = box_predicate_desc l in
	let r_elem = box_predicate_desc r in
	  construct_element disjunct_elem_name [] (l_elem :: r_elem :: [])

(* Axis *)
let box_node_test nt =
  let child_type =
   match nt with
    | APNameTest qn ->
	let attr = [name_test_attr_name, serializable_string_of_rqname (relem_name qn)] in
	construct_element  name_test_elem_name attr []
    | APNodeKindTest ckt ->
	construct_element kind_test_elem_name [] [box_kind_test ckt]
  in
    construct_element node_test_elem_name [] [child_type]

(* TupleTreePatterns *)

let box_child_twigs twig_node =
  let create_elem_of_axis_index_pair name (axis, index) =
    let ax = axis_attr_name, (string_of_axis axis) in
    let ix = index_attr_name, (string_of_int index) in
    construct_element name [ax; ix] []
  in
  (match twig_node.child_twig with
  | Some (axis, index) ->
      [create_elem_of_axis_index_pair child_twig_name (axis,index)]
  | None -> [] ) @
  List.map (create_elem_of_axis_index_pair pred_twig_name) twig_node.pred_twigs

let box_twig_node twig_node =
  let attrs = [(restore_attr_name, string_of_bool twig_node.restore_out)] in
  let elems = box_child_twigs twig_node in
  let attrs', elems' =
    match twig_node.out, twig_node.node_test with
    | Some f, Some nt ->
	let nte = box_node_test nt in
	(out_field_attr_name, serializable_string_of_rqname f) :: attrs, nte :: elems
    | None, Some nt -> 
	let nte = box_node_test nt in
	attrs, nte :: elems
    | Some f, None ->
	(out_field_attr_name, serializable_string_of_rqname f) :: attrs, elems
    | _ -> attrs, elems
  in
  construct_element twig_node_name attrs' elems'

(* Group-by *)

let box_named_variable_name ename vn =
  let name = vname_attr_name, (serializable_string_of_rqname vn) in
    construct_element ename [name] []

let box_variable_name vn =
  box_named_variable_name vname_elem_name vn

let box_variable_list evl_name vlist =
  let arity = arity_attr_name, (string_of_int (List.length vlist)) in
  let elems = List.map box_variable_name vlist in
    construct_element evl_name [arity] elems

let box_group_desc gd =
  let gbn_elem = box_variable_list gbn_elem_name (get_group_names gd)     in
  let ind_elem = box_variable_list induced_elem_name (get_group_names gd) in
  let mbv_elem = box_variable_list mbv_elem_name (get_valid_names gd)     in
  let agg_elem = box_named_variable_name agg_elem_name (get_aggregate_name gd) in
  let agg_type = box_optasequencetype (get_aggregate_type gd) in 
  let elems    = gbn_elem :: ind_elem :: mbv_elem :: agg_elem :: agg_type in
    construct_element group_desc_elem_name [] elems

(* Overloaded Call Table *)

let box_function_signature fname intypes otype upd =
  let box_input_sig index t = 
    let in_elem = box_optasequencetype t in
    let i = index_attr_name, (string_of_int index) in
    let attrs = i :: [] in
      construct_element input_type_elem_name attrs in_elem
  in
  let box_output_sig t =
    let out_elem = box_asequencetype t in
    let attrs = [] in
      construct_element output_type_elem_name attrs [out_elem] 
  in

  let in_types = Array.to_list (Array.mapi box_input_sig intypes) in
  let out_types = box_output_sig otype in
  let name = fn_name_attr_name, (serializable_string_of_rqname fname) in
  let arity = arity_attr_name, (string_of_int (List.length in_types)) in
  let u = 
    match upd with
      | Updating -> "yes"
      | NonUpdating -> "no"
  in 
  let upd_attr = updating_attr_name, u in
  let attrs = name :: arity :: upd_attr :: [] in
    construct_element function_signature_elem_name attrs (in_types @ out_types :: [] )

let box_overloaded_call_table table = 
  let n_entries = List.length table in
  let entries     = List.map (fun (name, (in_sig, out_sig)) ->
    let wrapped = List.map (fun x -> Some x) in_sig in
      box_function_signature
	name (Array.of_list wrapped) out_sig NonUpdating) 
    table in

  let arity     = arity_attr_name, (string_of_int n_entries) in
    construct_element overloaded_call_elem_name [arity] entries 

(* Tuple slots *)
let box_tuple_slots tindex (odt,tname) =
  let index = index_attr_name, (string_of_int tindex) in
  let name  = var_attr_name, (serializable_string_of_rqname tname) in 
  let attrs = index :: name :: [] in
  let dt    = box_optasequencetype odt in 
    construct_element tuple_slot_elem_name attrs dt

(* Typeswitch *)	
let box_typeswitch (p,ovn) = 
    match p.papattern_desc with
      | ACase m ->
	  let ctype = box_asequencetype m in 
	  let attrs = 
	    match ovn with 
		Some s -> [ var_attr_name, (serializable_string_of_rqname s)]
	      | None -> []
	  in
	    construct_element ts_case_elem_name attrs [ctype] 
      | ADefault ->
	  let attrs = 
	    match ovn with 
		Some s -> [ var_attr_name, 
			   (serializable_string_of_rqname s)]
	      | None -> []
	  in
	  construct_element ts_default_elem_name attrs [] 
(* Take an algebraic operation,
   return a pair of the string name and the information 
   contained in the name of the algebraic operation *)

let box_algop_name algop = 
  let attrs, elems =
    match algop.palgop_expr_name with
    | AOELetvar (ocdt, vn) -> 
	let dt = box_optasequencetype ocdt in
	let vname = vname_attr_name, (serializable_string_of_rqname vn) in 
	let attrs = vname :: [] in
	attrs, dt

    | AOEIf ->
	[], []

    | AOEWhile ->
	[], []

    | AOETypeswitch(pat_name_array) ->
	let switch_list = Array.to_list pat_name_array in
	let elems = List.map box_typeswitch switch_list in
	let attrs = [ts_num_branches_attr_name, (string_of_int (List.length elems))] in
	attrs, elems 

    | AOEVar(vn) ->
	let vname = vname_attr_name, (serializable_string_of_rqname vn) in
	let attrs = [vname] in
	attrs, []

    | AOEScalar(v) ->	
	let v' = value_attr_name, (string_of_literal v) in
	let t  = atomic_type_attr_name, (attr_string_of_atomic_type (atomic_type_of_literal v)) in 
	let attrs = v' :: t :: [] in
	attrs, []

    | AOESeq ->
	[], []

    | AOEImperativeSeq ->
	[], []

    | AOEEmpty ->
	[], []

    | AOEDocument ->
	[], []

    | AOEPI(target, content) ->
	let tg = target_attr_name, target in
	let ct = content_attr_name, content in
	let attrs = tg :: ct :: [] in
	attrs, []

    | AOEPIComputed ->
	[], []

    | AOEComment comment ->
	let cmt = value_attr_name, comment in
	let attrs = cmt :: [] in
	attrs, []

    | AOECommentComputed ->
	[], []

    | AOEText dt ->
	let t = value_attr_name, dt in
	let attrs = t :: [] in
	attrs, []

    | AOETextComputed ->
	[], []

    (* NB!  We are not printing out the namespace environment
       associated with an element constructor, so when we parse the
       element here, we end up with an incomplete namespace
       environment! *)

    (* This will break round-tripping *)
    | AOEElem(rname,nsenv) ->
	let elems = box_relem_symbol rname in
	[], elems :: []

    | AOEAnyElem (nsenv1,nsenv2) ->
	[], []

    | AOEAttr(rname) -> 
	let elems = box_rattr_symbol rname in
	[], elems :: []

    | AOEAnyAttr nsenv ->
	[], []

    | AOEError -> 
	[], []

    | AOETreat(cst) ->
	let cst' = box_asequencetype cst in
	[], [cst']

    | AOEValidate _ ->
	[], []

    | AOECast(nsenv, cst) ->
	let cst' = box_asequencetype cst in
	[], [cst']

    | AOECastable(nsenv, cst) ->
	let cst' = box_asequencetype cst in
	[], [cst']

    | AOESome (ocdt, vn) ->
	let dt = box_optasequencetype ocdt in
	let vname = vname_attr_name, (serializable_string_of_rqname vn) in 
	let attrs = vname :: [] in
	attrs, dt

    | AOEEvery(ocdt, vn) ->
	let dt = box_optasequencetype ocdt in
	let vname = vname_attr_name, (serializable_string_of_rqname vn) in 
	let attrs = vname :: [] in
	attrs,dt

	  (* Function Calls *)
    | AOECallBuiltIn((fname,arity), input_types, output_type, upd) -> 
	let name = fn_name_attr_name, (serializable_string_of_rqname fname) in
	let arity = arity_attr_name, (string_of_int arity) in
(*  
   The only value of shipping the function signature is when typing
   has determined that the type check on the input type is
   unnecessary.  Need to think about this...
	let function_signature_element =
	  box_function_signature fname input_types output_type upd
	in
*)
	let attrs = name :: arity :: [] in
	attrs, []

    | AOECallOverloaded((fname, arity),table) ->
	let name = fn_name_attr_name, (serializable_string_of_rqname fname) in
	let arity = arity_attr_name, (string_of_int arity) in
	(* We are not currently serializing the call table, though we will
	   when static typing comes back *)
	(*	let table = box_overloaded_call_table table in *)
	let attrs = name :: arity :: [] in
	attrs, []

    | AOECallUserDefined((fname,arity), input_types, output_type, upd, selfrecur) -> 
	let name = fn_name_attr_name, (serializable_string_of_rqname fname) in
	let arity' = arity_attr_name, (string_of_int arity) in
	let function_signature_element = box_function_signature fname input_types output_type upd in 
	let attrs = name :: arity' :: [] in 
	attrs,[function_signature_element] 

    | AOEConvertSimple at ->  
	let t  = atomic_type_attr_name, (attr_string_of_atomic_type at) in 
	[t], []

    | AOEPromoteNumeric at  -> 
	let t  = atomic_type_attr_name, (attr_string_of_atomic_type at) in 
	[t], []

    | AOEPromoteAnyString  -> 
	let t  = atomic_type_attr_name, (attr_string_of_atomic_type Datatypes.ATString) in
	[t], []

    | AOEUnsafePromoteNumeric at  -> 
	let t  = atomic_type_attr_name, (attr_string_of_atomic_type Datatypes.ATString) in 
	[t], []
    (* DXQ *)
    | AOEServerImplements (ncname, uri) 
    | AOEForServerClose (ncname, uri) 
    | AOEExecute (ncname, uri)
    | AOEASyncExecute (ncname, uri) ->
	let n = prefix_attr_name, ncname in
	let u = uri_attr_name, uri in
	[n;u], []

    | AOEEvalClosure 
    | AOEInputTuple ->
	[],[]

	  (* Tuples *)
    | AOECreateTuple(tnames) -> 	
	let arity' = arity_attr_name, (string_of_int (Array.length tnames)) in
	let elems = Array.to_list (Array.mapi box_tuple_slots tnames) in
	[arity'], elems

    | AOEAccessTuple(tname) ->
(*	let tname = tuple_name_attr_name, (serializable_string_of_rqname tname) in *)
	let tname = tuple_name_attr_name, (serializable_string_of_rqname tname) in
	let attrs = tname :: [] in 
	attrs, []

    | AOEProject (tnames) ->
	let box_project_name names =
	  let values = 
	    Array.map 
	      (fun p -> 
		construct_element project_name_elem_name 
                  [project_name_attr_name, (serializable_string_of_rqname p)] []) 
(*                  [project_name_attr_name, (serializable_string_of_rqname p)] [])  *)
	      names in
	  construct_element project_elem_name [] (Array.to_list values)
	in
	[], [box_project_name tnames]

    | AOEConcatTuples ->
	[], []

    | AOEMapConcat ->
	[],[] 

    | AOEOuterMapConcat vn ->  
(*	let vname = null_index_attr_name, (serializable_string_of_rqname vn) in *)
	let vname = null_index_attr_name, (serializable_string_of_rqname vn) in
	(vname :: []), [] 

    | AOEMap ->
	[], []

    | AOEMapFromItem vn ->
(*	let vname = item_tuple_attr_name, (serializable_string_of_rqname vn) in *)
	let vname = item_tuple_attr_name, (serializable_string_of_rqname vn) in
	(vname :: []), []

    | AOEMapToItem ->
	[], []

    | AOENullMap null_index ->
(*	let tname = null_index_attr_name, (serializable_string_of_rqname null_index) in *)
	let tname = null_index_attr_name, (serializable_string_of_rqname null_index) in
	let attrs = tname :: [] in 
	attrs, []

    | AOEMapIndex crname ->
	let tname = tuple_name_attr_name, (serializable_string_of_rqname crname) in
	let attrs = tname :: [] in 
	attrs, []

    | AOEMapIndexStep crname ->
(*	let tname = tuple_name_attr_name, (serializable_string_of_rqname crname) in *)
	let tname = tuple_name_attr_name, (serializable_string_of_rqname crname) in
	let attrs = tname :: [] in 	    
	attrs, []

    | AOEProduct ->
	[], []

    | AOESelect pd ->
	let pd_elem = box_predicate_desc pd in
	[], (pd_elem :: [])

    | AOELeftOuterJoin (vn, pd) ->
	let vname   = null_index_attr_name, (serializable_string_of_rqname vn) in
	let pd_elem = box_predicate_desc pd in
	(vname :: []), (pd_elem :: [])

    | AOEJoin pd  -> 
	let pd_elem = box_predicate_desc pd in
	[], (pd_elem :: [])

    | AOEGroupBy gd_list ->
	let arity = arity_attr_name, (string_of_int (List.length gd_list)) in
	let attrs = arity :: [] in
	let gds   = List.map box_group_desc gd_list in 
	attrs, gds

    | AOEOrderBy (stable, sort_kinds, osig) ->
	let box_sort_kinds (sk,esk) =
	  let sk_attr  = boxattr_sortkind sk in
	  let esk_attr = boxattr_emptysortkind esk in
	  construct_element sort_spec_elem_name (sk_attr :: esk_attr :: []) []
	in
	let n_attrs = n_sort_criteria_attr_name, (string_of_int (List.length sort_kinds)) in
	let stable = boxattr_stable stable in
	let sorts = List.map box_sort_kinds sort_kinds in
	n_attrs :: stable :: [], sorts

	  (* Updates *)
    | AOECopy -> 
	[], []

    | AOEDelete -> 
	[], []

    | AOEInsert il -> 
	let il_attr = boxattr_insert_location il in
	il_attr :: [], []

    | AOERename _ ->
	[], []

    | AOEReplace val_flag -> 
	let val_flag = boxattr_value_of_flag val_flag in
	val_flag :: [], []

    | AOESnap sm ->
	let sm_attr = boxattr_snap_modifier sm in 
	sm_attr :: [], []

    | AOESet vn -> 
	let vname = vname_attr_name, (serializable_string_of_rqname vn) in 	  
	let attrs = [vname] in
	attrs, []

    | AOETreeJoin (axis, anode_test) ->
	let ax = axis_attr_name, (string_of_axis axis) in
	let nt = box_node_test anode_test in
	let attrs = ax :: [] in
	attrs, [nt]

    | AOETupleTreePattern (input_dot, pattern) ->
	let twig_list = Array.to_list pattern in
	let twig_node_elems = List.map box_twig_node twig_list in
	let input_attr = (in_field_attr_name, serializable_string_of_rqname input_dot) in 
	[input_attr], twig_node_elems

(*
    | AOEPrune (name, axis) -> 
        let ax = axis_attr_name, (string_of_axis axis) in
	let input_vname = input_prune_field_attr_name, (serializable_string_of_rqname name) in
	[ax; input_vname],[]

    | AOEDistinct name ->
	let input_vname = input_distinct_field_attr_name, (serializable_string_of_rqname name) in
	[input_vname],[]
*)
    | AOEParse docname ->
        (docname_attr_name, docname) :: [], []

    | AOECharRef dt ->
	let t = value_attr_name, (string_of_int dt) in
	let attrs = t :: [] in
	attrs, []

  in
  (get_serializable_moniker_of_algop_name algop.palgop_expr_name), attrs, elems

(***************)
(* Statements  *)
(***************)

(*------------*)
(*-- Boxing --*)
(*------------*)

(* Return list of elements corresponding to sub-expressions *)
let rec box_subexpr box_algop se =
  match se with
    NoSub -> []
    | OneSub s1 -> [box_algop s1] 
    | TwoSub(s1,s2) -> [box_algop s1; box_algop s2]
    | ManySub s_array -> (Array.to_list (Array.map box_algop s_array))

(* Wrappers for the two kinds of subexpressions *)

(* Pre code selection *)
and undecorated_box_algop algop =
  let name, attributes, (sub_elem: rsexpr list) = box_algop_name algop in
  let attributes = 
    (dep_subexpr_attrs algop.pdep_sub_expression) @
    (indep_subexpr_attrs algop.psub_expression) @
    attributes
  in
  let dependent_exprs     = box_subexpr undecorated_box_algop algop.pdep_sub_expression in
  let independent_exprs   = box_subexpr undecorated_box_algop algop.psub_expression in

  let child_elements      =  sub_elem @ dependent_exprs @ independent_exprs in
    construct_element name attributes child_elements
      
(* Statement handler *)
let box_statement box_algop s =
  construct_element expression_elem_name [] [(box_algop s)]

(* Logical handlers *)
let box_logical_algebra_statement nsenv stmt =
  let nsenv' = Namespace_context.add_all_ns nsenv [(algebra_prefix, algebra_uri)]  in 
  Small_stream_context.resolved_xml_stream_of_sexpr 
    (Small_stream_context.sexpr_of_rsexpr nsenv'
       (box_statement undecorated_box_algop stmt))

(*-----------------*)
(*-- Serializing --*)
(*-----------------*)

let init_proc_ctxt proc_ctxt = 
  Processing_context.set_serialization_kind
    proc_ctxt 
    Processing_context.Serialize_As_Well_Formed

let serialize_algebra_statement_helper box_algop proc_ctxt output stmt = 
  let gout  = Parse_io.galax_output_from_output_spec output in
    (* is this correct? *)
  let nsenv = Processing_context.get_external_nsenv proc_ctxt in
  let nsenv' = Namespace_context.add_all_ns nsenv [(algebra_prefix, algebra_uri)]  in 
    try
      begin	
	let whole_plan = box_statement box_algop stmt in
	let algop_module_doc = Small_stream_context.sexpr_of_rsexpr nsenv' (RSDocument( Dm_atomic_util.default_no_uri_dm, [whole_plan])) in 
	let rxmlstr = Small_stream_context.resolved_xml_stream_of_sexpr algop_module_doc in	  
	let fmt = Parse_io.formatter_of_galax_output gout in
	  (Serialization.fserialize_resolved_xml_stream proc_ctxt fmt rxmlstr;
	   Format.pp_print_newline fmt ();
	   Parse_io.close_galax_output gout)
      end
    with
      | exn -> (Parse_io.close_galax_output gout; raise exn)

let serialize_logical_algebra_statement proc_ctxt output stmt =
  init_proc_ctxt proc_ctxt;
  serialize_algebra_statement_helper undecorated_box_algop proc_ctxt output stmt

(*------------*)
(*-- Unbox  --*)
(*------------*)

(* Type Symbols and Namespace parsing section *)
let occurrence_sym = rattr_symbol occurrence_attr_name 
let unbox_occurrence_option attrs =
  match attrs with 
    | [] -> None
    |[(n,str)] when n = occurrence_sym ->        
       begin
	 match str with
	   | "None" -> None
	   | "*" -> Some (UP_INT 0, UNBOUNDED) 
	   | "+" -> Some (UP_INT 1, UNBOUNDED)
	   | "?" -> Some (UP_INT 0, UP_INT 1)
	   | _   -> raise (Query (Algebra_Parsing_Error ("Unknown Occurence Marker")))
       end
    | _ -> raise (Query (Algebra_Parsing_Error ("Unknown Occurence Marker")))

(* Relem symbol *)
let unbox_relem_symbol attrs st =
  let prefix = get_attr_from_attr_list attrs xml_prefix_of_string prefix_attr_name in
  let (_,uri,ncname)   = get_attr_from_attr_list attrs parse_rqname_string rqname_attr_name in
    relem_symbol (prefix, uri, ncname)

let relem_symbol_parser st =
  element_parser relem_elem_name (unbox_relem_symbol) "relem_symbol" st 

let unbox_rattr_symbol attrs st =
  let prefix = get_attr_from_attr_list attrs xml_prefix_of_string prefix_attr_name in
  let (_,uri,ncname)   = get_attr_from_attr_list attrs parse_rqname_string rqname_attr_name in
    rattr_symbol (prefix, uri, ncname)

let rattr_symbol_parser st =
  element_parser rattr_elem_name (unbox_rattr_symbol) "rattr_symbol" st

let unbox_rtype_symbol attrs st = 
  let prefix = get_attr_from_attr_list attrs xml_prefix_of_string prefix_attr_name in
  let (_,uri,ncname)   = get_attr_from_attr_list attrs parse_rqname_string rqname_attr_name in
    rtype_symbol (prefix, uri, ncname)

let rtype_symbol_parser st =
  element_parser rtype_elem_name (unbox_rtype_symbol) "rtype_symbol" st

(* AIT_xXxX Parsing Section *)
(* Default parsing function for those that do not need paramaters *)
let unbox_no_param name ret_type emsg st = 
  element_parser name (fun attrs st -> ret_type) emsg st 

let unbox_ait_attributetype attrs st =
  if (is_element_end st)
  then
    AAttributeTest None
  else
    let rname = rattr_symbol_parser st in
    let rtype = rtype_symbol_parser st in
    AAttributeTest (Some (rname, Some rtype))

let ait_attributetype_parser st =
  element_parser ait_attributetype_elem_name (unbox_ait_attributetype) "ait_attributetype" st

(* AIT_element_type *)
let unbox_ait_elementtype attrs st =
  if (is_element_end st)
  then
    AElementTest None
  else
    let rname = relem_symbol_parser st in
    let rtype = rtype_symbol_parser st in
    AElementTest (Some (rname, Some rtype))

let ait_elementtype_parser st =
  element_parser ait_elementtype_elem_name (unbox_ait_elementtype) "ait_elementtype" st

(* Schema attribute *)
let unbox_ait_schemaattribute attr st =
  ASchemaAttributeTest (rattr_symbol_parser st)

let ait_schemaattribute_parser st =
  element_parser ait_schemaattribute_elem_name (unbox_ait_schemaattribute) "ait_schemaattribute" st

(* Schema Elementtype *)
let unbox_ait_schemaelementtype attr st =
  ASchemaElementTest (relem_symbol_parser st)

let ait_schemaelementtype_parser st =
  element_parser ait_schemaelementtype_elem_name (unbox_ait_schemaelementtype) "ait_schemaelementtype" st

(* typeref *)
let unbox_ait_typeref attrs st =
  AITTypeRef (rtype_symbol_parser st)

let ait_typeref_parser st =
  element_parser ait_typeref_elem_name (unbox_ait_typeref) "ait_typeref" st

(* Processing Instruction *)
let unbox_ait_processing_intruction attrs st =
  let pi = get_opt_attr_from_attr_list attrs string_id pi_arg_attr_name in
  APIKind pi

let ait_processing_instruction_parser st =
  element_parser ait_processing_instruction_elem_name unbox_ait_processing_intruction "ait_processing_instruction" st

(* Atomic *)
let unbox_ait_atomic attrs st =
  AITAtomic (rtype_symbol_parser st)

let ait_atomic_parser st =
  element_parser ait_atomic_elem_name (unbox_ait_atomic) "ait_atomic_element" st 

(* ElementKind *)
let ait_element_kind_parser st =
  let name, attrs = start_element_get_name_no_consume st in
  let t = 
    if name = Namespace_symbols.relem_symbol ait_schemaelementtype_elem_name then 
      ait_schemaelementtype_parser st 
    else if name = Namespace_symbols.relem_symbol ait_elementtype_elem_name then 
      ait_elementtype_parser st
    else
      raise (Query (Algebra_Parsing_Error ("Invalid subelement in ait_element_kind_parser: *" ^ (string_of_relem_symbol name) ^"*")))
  in
  AElementKind t

(* AttributeKind *)
let ait_attribute_kind_parser st =
  let name, attrs = start_element_get_name_no_consume st in
  let t = 
    if name = Namespace_symbols.relem_symbol ait_schemaattribute_elem_name then 
      ait_schemaattribute_parser st 
    else if name = Namespace_symbols.relem_symbol ait_attributetype_elem_name then
      ait_attributetype_parser st 
    else
      raise (Query (Algebra_Parsing_Error ("Invalid subelement in ait_attribute_kind_parser: *" ^ (string_of_relem_symbol name) ^"*")))
  in
  AAttributeKind t

(* Document *)  
let unbox_ait_document attrs st =
  if (is_element_end st) then
    ADocumentKind None
  else
    let rname = ait_elementtype_parser st in
    ADocumentKind (Some rname)
  
let ait_document_parser st = 
  element_parser ait_document_elem_name unbox_ait_document "ait_document" st

(* without params *)
let ait_node_parser st = unbox_no_param ait_node_elem_name AAnyKind "ait_node" st
let ait_item_parser st = unbox_no_param ait_item_elem_name AITItem "ait_item" st
let ait_numeric_parser st = unbox_no_param ait_numeric_elem_name AITNumeric "ait_numeric" st
let ait_text_parser st = unbox_no_param ait_text_elem_name ATextKind "ait_text" st
let ait_comment_parser st = unbox_no_param ait_comment_elem_name ACommentKind "ait_comment" st
let ait_empty_parser st = unbox_no_param ait_empty_elem_name AITEmpty "ait_empty" st

let akind_parsers =
  List.map (fun (x,y) -> ((alg_elem x), y))
    [ (ait_schemaattribute_elem_name,ait_attribute_kind_parser);
      (ait_attributetype_elem_name,ait_attribute_kind_parser);
      (ait_schemaelementtype_elem_name,ait_element_kind_parser); 
      (ait_elementtype_elem_name,ait_element_kind_parser); 
      (ait_node_elem_name,ait_node_parser);
      (ait_text_elem_name,ait_text_parser);
      (ait_comment_elem_name,ait_comment_parser);
      (ait_processing_instruction_elem_name,ait_processing_instruction_parser);
      (ait_processing_instruction_elem_name,ait_processing_instruction_parser);
      (ait_document_elem_name,ait_document_parser)
   ]

let unbox_akind_test st = 
  let name, attrs = start_element_get_name_no_consume st in
  let cur_parser = 
    try
      (List.assoc name akind_parsers)
    with Not_found ->
      raise (Query (Algebra_Parsing_Error ("Invalid subelement in unbox_akind_test: *" ^ (string_of_relem_symbol name) ^"*")))
  in
  let kind_test = (cur_parser st)
  in 
    kind_test 


let ait_kind_test_parser st =
  let _ = start_element (relem_symbol ait_kindtest_elem_name) st in
  let t = unbox_akind_test st in
  consume_end_element st;
  AITKindTest t

(* NOTE: All of these parser functions need to consume their first 
   element token before proceeding.  We fixed ait_kind_test_parser,
   but none of the others.  Look for all calls to start_element_get_name_no_consume to 
   see where tokens are not being consumed. 
*)
let aitem_parsers =
  List.map (fun (x,y) -> ((alg_elem x), y))
    [ (ait_kindtest_elem_name,ait_kind_test_parser);
      (ait_typeref_elem_name,ait_typeref_parser);
      (ait_item_elem_name,ait_item_parser);
      (ait_numeric_elem_name,ait_numeric_parser); 
      (ait_empty_elem_name,ait_empty_parser);
      (ait_atomic_elem_name,ait_atomic_parser) ]
 
let unbox_aitemtype st = 
  let name, attrs = start_element_get_name_no_consume st in
  let cur_parser = 
    try
      List.assoc name aitem_parsers 
    with Not_found ->
      raise (Query (Algebra_Parsing_Error ("Invalid subelement in unbox_aitemtype " ^ (string_of_relem_symbol name) )))
  in
  cur_parser st

(* Parse a csequence element *)

let unbox_asequencetype attrs st =
  let occur = unbox_occurrence_option attrs in
  let item_portion = unbox_aitemtype st in
  { pasequencetype_desc = (item_portion, occur);
    pasequencetype_loc = Finfo.bogus }

let asequencetype_parser st = 
  element_parser asequencetype_elem_name unbox_asequencetype "asequencetype" st

(* If an optional sequence type is present grab it else return None *)
let unbox_optasequencetype st = 
  if (check_opt_element asequencetype_elem_name st) then
    Some (asequencetype_parser st)
  else
    None

(* Functions for parsing the Datamodel signatures *)
let parse_input_sig (st: resolved_sax_event Stream.t) = 
  let attrs = start_element (alg_elem input_datamodel_elem_name) st in
  let index = get_attr_from_attr_list attrs int_of_string index_attr_name in
  let datamodel = get_attr_from_attr_list attrs physical_type_of_string datamodel_attr_name in
  let _ = consume_end_element st in
    datamodel, index

(* Datamodel signature *)
let unbox_input_signature in_sigs str =
  let types, index = List.split in_sigs in
  (* signatures should be in order since they are elements *)
    match str with
    | "NoInput" -> 
	NoInput
    | "OneInput" -> 
	OneInput (List.nth types 0)
    | "TwoInput" -> 
	TwoInput ((List.nth types 0), (List.nth types 1))
    | "ManyInput" ->  (* Many means it is replicated *)
	ManyInput (Array.of_list types)
    | _ -> raise (Query (Algebra_Parsing_Error ("Unknown Input Cardinality for signature")))

(* DM signature *)
let parse_output_sig st = 
  let attrs = start_element (alg_elem output_datamodel_elem_name) st in
  let _ = consume_end_element st in
    get_attr_from_attr_list attrs physical_type_of_string datamodel_attr_name


let parse_element_of_expr_eval_sig st =
  let attrs = start_element (alg_elem datamodel_signature) st in
  let cardinality = get_attr_from_attr_list attrs get_cardinality arg_count_attr_name in 
    (* This implicitly checks the cardinality of the inputs *)
  let input_sigs = get_multiple cardinality parse_input_sig st in 
  let output_sig = parse_output_sig st in
  let _ = consume_end_element st in

  let inputs = get_attr_from_attr_list attrs (unbox_input_signature input_sigs) arg_count_attr_name in
    inputs, output_sig 


(* Function Type Signature *)
let unbox_input_signature attrs st =
  let _ = get_attr_from_attr_list attrs int_of_string index_attr_name in
    unbox_optasequencetype st    

let input_signature_parser st =
  element_parser input_type_elem_name unbox_input_signature "input_signature" st

(* Should assert that attrs = []? *)
let unbox_output_signature attrs st = 
  asequencetype_parser st 
  

let output_signature_parser st =
  element_parser output_type_elem_name unbox_output_signature "output_signature" st

(* Function Singature parser *)
let unbox_function_signature attrs st =
  let arity = get_attr_from_attr_list attrs int_of_string arity_attr_name in
  let fname  = get_attr_from_attr_list attrs parse_function_rqname_string fn_name_attr_name in
  let upd = match get_attr_from_attr_list attrs (fun x -> x) updating_attr_name with 
    | "yes" -> Updating
    | _ -> NonUpdating
  in     
  let input_sig = Array.of_list (get_multiple arity input_signature_parser st) in
  let output_sig = output_signature_parser st in
    fname,arity, input_sig, output_sig, upd

let function_signature_parser st =
  element_parser function_signature_elem_name unbox_function_signature "function_signature" st

(* Tuple Name parsing *)

let unbox_tuple_slot attrs st =
  let tindex = get_attr_from_attr_list attrs int_of_string index_attr_name in
  let tname  = get_attr_from_attr_list attrs parse_rqname_string var_attr_name in
  let odt    = unbox_optasequencetype st in
    (tindex, (odt,tname))

let tuple_slot_parser st =
  element_parser tuple_slot_elem_name unbox_tuple_slot "tuple_slot" st 

(* index attribute kept around just for debugging since the elements are ordered*)
let unbox_tuple_names  arity st =
  let index_type_name_tuple = get_multiple arity tuple_slot_parser st in
  let _, type_names = List.split index_type_name_tuple in
    (Array.of_list type_names)


(* Node Test Parsing *)
  
let unbox_name_test attrs st = 
  let qn = get_attr_from_attr_list attrs parse_rqname_string name_test_attr_name in
    APNameTest (relem_symbol qn)

let name_test_parser st =
  element_parser name_test_elem_name unbox_name_test "name_test" st

let unbox_pi_kind_test attrs st = 
  if attrs = [] then
    None
  else
    Some (get_attr_from_attr_list attrs string_id pi_kind_attr_name)

let pi_kind_parser st =
  element_parser pi_kind_test_elem_name unbox_pi_kind_test "pi_kind" st

let unbox_kind_test attrs st = 
  let node_kind = unbox_akind_test st in
  APNodeKindTest node_kind

let kind_test_parser st =
  element_parser kind_test_elem_name unbox_kind_test "kind_test" st

let unbox_node_test attrs st =
  (* Can be one of two elements name test or kind test *)
  let name, _ = start_element_get_name_no_consume st in
    if (name = (alg_elem name_test_elem_name)) then
      name_test_parser st
    else if (name = (alg_elem kind_test_elem_name)) then
      kind_test_parser st
    else
      raise (Query (Algebra_Parsing_Error("Expecting a node test sub-element but did not find a known one")))


let node_test_parser st = 
  element_parser node_test_elem_name unbox_node_test "node_test" st

let opt_node_test_parser st =
  if check_opt_element node_test_elem_name st then
    Some (element_parser node_test_elem_name unbox_node_test "node_test" st)
  else
    None


let twig_index_ctor attrs st =
  let ix = get_attr_from_attr_list attrs int_of_string index_attr_name in
  let ax = get_attr_from_attr_list attrs axis_of_string axis_attr_name in
  ax, ix


let child_twig_parser st = 
  if check_opt_element child_twig_name st then
    Some (element_parser child_twig_name twig_index_ctor "child_twig" st)
  else
    None

let rec pred_twig_parser st =
   if check_opt_element pred_twig_name st then
    let x = element_parser pred_twig_name twig_index_ctor "pred_node" st in
    x :: (pred_twig_parser st)
  else 
    [ ]

(* check_opt_element *)
let unbox_twig_node attrs st =
  let nt = opt_node_test_parser st in
  let child_index = child_twig_parser st in
  let pred_index_list = pred_twig_parser st in
  let out_field = 
    get_opt_attr_from_attr_list attrs parse_rqname_string out_field_attr_name in
  let restore =  get_attr_from_attr_list attrs bool_of_string restore_attr_name in
  {
    node_test = nt;
    out = out_field;
    restore_out = restore;
    child_twig = child_index;
    pred_twigs = pred_index_list;
    requires_sbdo = (true, true); (* fixme, include in xml export *)
  }

let rec tree_pattern_parser st =
  if check_opt_element twig_node_name st then
    let x = element_parser twig_node_name unbox_twig_node "twig_node" st in
    x :: (tree_pattern_parser st)
  else 
    [ ]

(* Typeswitch parsing *)

let unbox_typeswitch_case attrs st =
  let variable = get_opt_attr_from_attr_list attrs parse_rqname_string var_attr_name in
  let vtype    = asequencetype_parser st in
  let apattern = { papattern_desc = ACase vtype;
		   papattern_loc  = Finfo.bogus} in

    apattern, variable

let unbox_typeswitch_default attrs st =
  let variable = get_opt_attr_from_attr_list attrs parse_rqname_string var_attr_name in
  let apattern = { papattern_desc = ADefault;
		   papattern_loc  = Finfo.bogus} in
    apattern, variable

let typeswitch_case_parser st = 
  element_parser ts_case_elem_name unbox_typeswitch_case "typeswitch_case" st
    
let typeswitch_default_parser st =
  element_parser ts_default_elem_name unbox_typeswitch_default "typeswitch_default" st


let unbox_typeswitch_branches arity st = 
  (* We know that the last branch is always default *)
  let case_list = get_multiple (arity-1) typeswitch_case_parser st in
  let default = typeswitch_default_parser st in
    Array.of_list ( case_list @ default :: [] )


(* Projection Parsing *)
let unbox_project_name attrs st =
  get_attr_from_attr_list attrs parse_rqname_string project_name_attr_name

let project_name_parser st = 
  element_parser project_elem_name unbox_project_name "project_name_parser" st

let rec unbox_project attrs st =
  if check_opt_element project_name_elem_name st then
    let x =  project_name_parser st in
    x :: (unbox_project attrs st)
  else 
    []

(* OrderBy parsing *)
let unbox_sort_spec attrs st =
  let sk  = get_attr_from_attr_list attrs sortkind_of_string sort_kind_attr_name in
  let esk = get_attr_from_attr_list attrs emptysortkind_of_string empty_sort_kind_attr_name in
    (sk, esk)

let sort_spec_parser st =
  element_parser sort_spec_elem_name unbox_sort_spec "sort spec" st

(**********************)
(* Grouping Operation *)
(**********************)
let unbox_variable_name attrs st =
  get_attr_from_attr_list attrs parse_rqname_string vname_attr_name

let named_variable_name_parser name st =
  element_parser name unbox_variable_name "named variable name" st

let variable_name_parser st =
  named_variable_name_parser vname_elem_name st 

let unbox_variable_list attrs st =
  let arity = get_attr_from_attr_list attrs int_of_string arity_attr_name in 
    get_multiple arity variable_name_parser st

let variable_list_parser name st =
  element_parser name unbox_variable_list "variable list" st

let unbox_group_desc attrs st =
  let gbn = variable_list_parser gbn_elem_name st in
  let ind = variable_list_parser induced_elem_name st in
  let mbv = variable_list_parser mbv_elem_name st in
  let agg = named_variable_name_parser agg_elem_name st      in
  let odt = unbox_optasequencetype st in
    mk_group_desc gbn ind mbv (odt,agg)

let group_desc_parser st =
  element_parser group_desc_elem_name unbox_group_desc "group description" st

let stable_of_string str = 
  match str with
    | "Stable" -> Stable
    | "Unstable" -> NonStable
    | _ -> raise (Query (Malformed_Expr ("Unknown stable type string'"^str^"'")))
(**************)
(* Predicates *)
(**************)
let rec unbox_simple_conjunct attrs st =  
  let s = get_attr_from_attr_list attrs int_of_string simple_conjunct_start_attr_name in 
  let e = get_attr_from_attr_list attrs int_of_string simple_conjunct_end_attr_name   in 
  let _ = consume_end_element st in
    SimpleConjunct(s,e)

and unbox_complex_conjunct attrs st = 
  let l = box_pred_desc st in
  let r = box_pred_desc st in
  let _ = consume_end_element st in
    ComplexConjunct (l,r)

and unbox_disjunct attrs st = 
  let l = box_pred_desc st in
  let r = box_pred_desc st in
  let _ = consume_end_element st in
    Disjunct (l,r)

and box_pred_desc st =
  let name, attrs = start_element_get_name st in
    if relem_equal name (alg_elem simple_conjunct_elem_name) then
      unbox_simple_conjunct attrs st
    else if relem_equal name (alg_elem complex_conjunct_elem_name) then
      unbox_complex_conjunct attrs st
    else if relem_equal name (alg_elem disjunct_elem_name) then
      unbox_disjunct attrs st
    else raise (Query (Algebra_Parsing_Error ("Attempting to parse a predicate description but found " ^  
				   (string_of_relem_symbol name))))

(* parse_algop_header returns (name, indep_kind_arity, dep_kind_arity) *)
let parse_algop_header comp_ctxt st =
  let norm_ctxt = Compile_context.norm_context_from_compile_context comp_ctxt in
  let element_name, attrs = start_element_get_name st in
  let parse_attrs x = get_attr_from_attr_list attrs x in
  let algop_name, indep_kind, dep_kind = 
    match (get_moniker_of_algop element_name) with 
      (* Algop Name, Indep Kind, Dep kind *)
    | AOEIf_n ->
	AOEIf, OneSub_n, TwoSub_n
    | AOEWhile_n ->
	AOEWhile, NoSub_n, TwoSub_n
    | AOELetvar_n ->
	let odt = unbox_optasequencetype st in
	let vn = parse_attrs parse_rqname_string vname_attr_name in
	AOELetvar ( odt, vn ), OneSub_n, OneSub_n
    | AOEInputTuple_n -> AOEInputTuple, NoSub_n, NoSub_n
    | AOETypeswitch_n ->
	let arity = parse_attrs int_of_string ts_num_branches_attr_name in
	let pat_array = unbox_typeswitch_branches arity st in
	AOETypeswitch( pat_array ), OneSub_n, ManySub_n
    | AOEVar_n ->
	let vname = parse_attrs parse_rqname_string vname_attr_name in 	  
	AOEVar(vname), NoSub_n, NoSub_n
    | AOEScalar_n ->
	let scale_value = parse_attrs (fun x -> x) value_attr_name in
	let atomic_type = parse_attrs (atomic_type_of_attr_string) atomic_type_attr_name in 
	let literal = literal_of_string atomic_type scale_value in
	AOEScalar literal, NoSub_n, NoSub_n
    | AOESeq_n ->
	AOESeq, TwoSub_n, NoSub_n
    | AOEEmpty_n ->
	AOEEmpty, NoSub_n, NoSub_n
    | AOEDocument_n ->
	AOEDocument, OneSub_n, NoSub_n
    | AOEPI_n  ->
	let tg = parse_attrs string_id target_attr_name in
	let ct = parse_attrs string_id content_attr_name in
	AOEPI(tg, ct), NoSub_n, NoSub_n
    | AOEPIComputed_n  ->
	AOEPIComputed, TwoSub_n, NoSub_n
    | AOEComment_n ->
	let cmt = parse_attrs string_id value_attr_name in
	AOEComment( cmt ), NoSub_n, NoSub_n
    | AOECommentComputed_n ->
	AOECommentComputed, OneSub_n, NoSub_n
    | AOEText_n ->
	let v = parse_attrs string_id value_attr_name in
	AOEText v, NoSub_n, NoSub_n
(* Where is AOECharRef_n? *)
    | AOETextComputed_n ->
	AOETextComputed, OneSub_n, NoSub_n
    (* NB!  We are not printing out the namespace environment
       associated with an element constructor, so when we parse the
       element here, we end up with an incomplete namespace
       environment! *)
    (* This will break round-tripping *)
    | AOEElem_n ->
	let e_name = relem_symbol_parser st in
	let nsenv = empty_nsenv in
	AOEElem (e_name, nsenv), ManySub_n, NoSub_n
    | AOEAnyElem_n ->
	(* Statically known namespaces *)
	let nsenv1 = empty_nsenv in
	(* In-scope namespaces *)
	let nsenv2 = empty_nsenv in
	AOEAnyElem (nsenv1,nsenv2), TwoSub_n, NoSub_n
	  
    | AOEAttr_n -> 
	let a_name = rattr_symbol_parser st in
	AOEAttr a_name, ManySub_n, NoSub_n
	  
    | AOEAnyAttr_n ->
	let nsenv = empty_nsenv in
	AOEAnyAttr nsenv, TwoSub_n, NoSub_n
    	  
    | AOEError_n -> 
	AOEError, ManySub_n, NoSub_n
	  
    | AOETreat_n -> 
	let cst = asequencetype_parser st in
	AOETreat( cst ), OneSub_n, NoSub_n
	  
    | AOEValidate_n ->
	AOEValidate Strict, OneSub_n, NoSub_n
	  
    | AOECast_n  ->
	let cst = asequencetype_parser st in
	let nsenv = empty_nsenv in
	AOECast (nsenv, cst), OneSub_n, NoSub_n
	  
    | AOECastable_n ->
	let cst = asequencetype_parser st in
	let nsenv = empty_nsenv in
	AOECastable (nsenv, cst), OneSub_n, NoSub_n
	  
    | AOETreeJoin_n->
	let axis_name = parse_attrs axis_of_string axis_attr_name in
	let nt = node_test_parser st in
	AOETreeJoin(axis_name, nt), OneSub_n, NoSub_n
	  
    | AOETupleTreePattern_n ->
	let input_field = parse_attrs parse_rqname_string in_field_attr_name in
	let pattern = Array.of_list (tree_pattern_parser st) in
	AOETupleTreePattern (input_field, pattern), OneSub_n, NoSub_n

    | AOESome_n ->
	let odt = unbox_optasequencetype st in
	let vn = parse_attrs parse_rqname_string vname_attr_name in
	AOESome ( odt, vn ), OneSub_n, OneSub_n
	  
    | AOEEvery_n ->
	let odt = unbox_optasequencetype st in
	let vn = parse_attrs parse_rqname_string vname_attr_name in
	AOEEvery ( odt, vn ), OneSub_n, OneSub_n
	  
   (* Function Calls *)
    | AOECallBuiltIn_n -> 
      (* There are attributes, but they are name and arity which are replicated 
	 inside the signature *)
	let fn = parse_attrs (parse_function_rqname_string) fn_name_attr_name in
	let arity = parse_attrs int_of_string arity_attr_name in
	let (csig, opt_fun_kind, upd) = Norm_context.one_sig_from_norm_context norm_ctxt (fn, arity) in 
	let (ainput_types, aoutput_type) = Compile_util.compile_cfunction_sig comp_ctxt csig in 
	AOECallBuiltIn((fn,arity), Array.of_list (List.map (fun at -> Some at) ainput_types), aoutput_type, upd),
	ManySub_n, NoSub_n

    | AOECallOverloaded_n ->
	let norm_ctxt = Compile_context.norm_context_from_compile_context comp_ctxt in
	let fn = parse_attrs (parse_function_rqname_string) fn_name_attr_name in
	let arity = parse_attrs int_of_string arity_attr_name in
      (* Is that going to work here ?? PLEASE CHECK - Jerome *)
	let sigs      = Norm_overloaded.table_for_overloaded_function norm_ctxt (fn,arity) in
	let asigs     = Compile_util.compile_overloaded_table_sigs comp_ctxt sigs in
	AOECallOverloaded((fn,arity),asigs), ManySub_n, NoSub_n
	  
    | AOECallUserDefined_n -> 
	let fn,arity,input_types, output_type, upd = function_signature_parser st in 

(* Can't do this, because it assumes the normalization context is already populated with 
   the function signatures...
	let fn = parse_attrs (parse_function_rqname_string) fn_name_attr_name in
	let arity = parse_attrs int_of_string arity_attr_name in
	let (csig, upd) = Norm_context.one_sig_from_norm_context norm_ctxt (fn, arity) in 
	let (ainput_types, aoutput_type) = Compile_util.compile_cfunction_sig comp_ctxt csig in 
	AOECallUserDefined((fn,arity), Array.of_list (List.map (fun at -> Some at) ainput_types), aoutput_type, upd),

  Also, for now, we don't know if this call is self-recursive, unless the function definition is also shipped.
  So we'll assume false.
*)
	AOECallUserDefined((fn,arity), input_types, output_type, upd, false),
	  ManySub_n, NoSub_n
    | AOEConvertSimple_n -> 
	let atomic_type = parse_attrs (atomic_type_of_attr_string) atomic_type_attr_name in  
	AOEConvertSimple atomic_type, OneSub_n, NoSub_n
    | AOEPromoteNumeric_n  -> 
	let atomic_type = parse_attrs (atomic_type_of_attr_string) atomic_type_attr_name in 
	AOEPromoteNumeric atomic_type, OneSub_n, NoSub_n
	  
    | AOEUnsafePromoteNumeric_n -> 
	let atomic_type = parse_attrs (atomic_type_of_attr_string) atomic_type_attr_name in 
	AOEUnsafePromoteNumeric atomic_type, OneSub_n, NoSub_n
	  
    (* DXQ *)
    | AOEServerImplements_n -> 
	let ncname = parse_attrs string_id prefix_attr_name in
	let uri = parse_attrs string_id uri_attr_name in 
	AOEServerImplements (ncname, uri), OneSub_n, OneSub_n 
    | AOEForServerClose_n -> 
	let ncname = parse_attrs string_id prefix_attr_name in
	let uri = parse_attrs string_id uri_attr_name in 
	AOEForServerClose (ncname, uri), OneSub_n , NoSub_n
    | AOEEvalClosure_n -> 
	AOEEvalClosure, OneSub_n, NoSub_n
    | AOEASyncExecute_n -> 
	let ncname = parse_attrs string_id prefix_attr_name in
	let uri = parse_attrs string_id uri_attr_name in 
	AOEASyncExecute (ncname, uri), TwoSub_n, NoSub_n
    | AOEExecute_n ->
	let ncname = parse_attrs string_id prefix_attr_name in
	let uri = parse_attrs string_id uri_attr_name in 
	AOEExecute (ncname, uri), TwoSub_n, NoSub_n
	(* Tuples *)
    | AOECreateTuple_n -> 
	let arity = parse_attrs int_of_string arity_attr_name in
	let tnames =  unbox_tuple_names arity st in
	AOECreateTuple ( tnames ), ManySub_n, NoSub_n
	  
    | AOEAccessTuple_n ->
	let tname = parse_attrs parse_rqname_string tuple_name_attr_name in
	AOEAccessTuple tname, NoSub_n, NoSub_n
	  
    | AOEProject_n ->
	let tnames =  
	  element_parser project_elem_name unbox_project "project_parser" st
	in
	AOEProject (Array.of_list tnames), OneSub_n, NoSub_n
	  
    | AOEProduct_n -> AOEProduct, TwoSub_n, NoSub_n
    | AOESelect_n -> 
	let pd = box_pred_desc st in
	AOESelect pd, OneSub_n, ManySub_n
	  
    | AOEJoin_n -> 	  
	let pd = box_pred_desc st in
	AOEJoin pd, TwoSub_n, ManySub_n (* Unknown? *)
	  
    | AOELeftOuterJoin_n -> 
	let null_index = parse_attrs parse_rqname_string null_index_attr_name in
	let pd         = box_pred_desc st in
	AOELeftOuterJoin (null_index, pd), TwoSub_n, ManySub_n (* Unknown? *)
	  
    | AOEGroupBy_n ->
	let arity = parse_attrs int_of_string arity_attr_name in
	let gd_list =  get_multiple arity group_desc_parser  st in
	AOEGroupBy gd_list, OneSub_n, ManySub_n
	  
    | AOEMap_n    -> AOEMap, OneSub_n, OneSub_n

    | AOEMapConcat_n -> AOEMapConcat, OneSub_n, OneSub_n
	  
    | AOEOuterMapConcat_n -> 
	let null_name = parse_attrs parse_rqname_string null_index_attr_name in
	AOEOuterMapConcat null_name, OneSub_n, OneSub_n
	  
    | AOEMapFromItem_n -> 
	let vn = parse_attrs parse_rqname_string item_tuple_attr_name in
	AOEMapFromItem vn, OneSub_n, OneSub_n

    | AOENullMap_n  ->
	let null_name = parse_attrs parse_rqname_string null_index_attr_name in
	AOENullMap null_name, OneSub_n, NoSub_n
	  
    | AOEMapIndex_n  ->
	let index_name = parse_attrs parse_rqname_string tuple_name_attr_name in
	AOEMapIndex index_name, OneSub_n, NoSub_n
	  
    | AOEMapIndexStep_n  ->
	let index_name = parse_attrs parse_rqname_string tuple_name_attr_name in
	AOEMapIndexStep index_name, OneSub_n, NoSub_n
	  
    | AOEMapToItem_n ->
	AOEMapToItem, OneSub_n, OneSub_n
	  
    | AOEConcatTuples_n ->
	AOEConcatTuples, TwoSub_n, NoSub_n
	  
    | AOEDelete_n -> AOEDelete, OneSub_n, NoSub_n
	  
    | AOEInsert_n ->
	let il = parse_attrs insert_location_of_string insert_location_attr_name in
	AOEInsert il, TwoSub_n, NoSub_n
    | AOERename_n ->
	let nsenv = empty_nsenv in
	AOERename nsenv, TwoSub_n, NoSub_n
    | AOEReplace_n ->
	let val_flag = parse_attrs value_flag_of_string value_of_flag_attr_name in
	AOEReplace val_flag, TwoSub_n, NoSub_n
    | AOESnap_n ->
	let attr_modifier = parse_attrs snap_modifier_of_string snap_modifier_attr_name in
	AOESnap attr_modifier, NoSub_n, OneSub_n
    | AOESequencing_n -> 
	let sm = parse_attrs snap_modifier_of_string snap_modifier_attr_name in 
	AOESnap sm, NoSub_n, OneSub_n
	  
    | AOESet_n ->
	let vn = parse_attrs parse_rqname_string vname_attr_name in
	AOESet ( vn ), OneSub_n, NoSub_n
	  
    | AOEImperativeSeq_n ->
	AOEImperativeSeq, TwoSub_n, NoSub_n
	  
    | AOEOrderBy_n ->
	let n_attrs = parse_attrs int_of_string n_sort_criteria_attr_name in
	let stable  = parse_attrs stable_of_string stable_attr_name in
	let sk_esk  = get_multiple n_attrs sort_spec_parser st in
	let norm_ctxt = Compile_context.norm_context_from_compile_context comp_ctxt in
	let sigs    = Norm_overloaded.table_for_op_gt norm_ctxt in
	let asigs   = Compile_util.compile_overloaded_table_sigs comp_ctxt sigs in
	AOEOrderBy (stable, sk_esk, asigs), OneSub_n, ManySub_n

(*	  
    | AOEPrune_n ->
	let prunefield = parse_attrs parse_rqname_string input_prune_field_attr_name in
	let axis_name = parse_attrs axis_of_string axis_attr_name in
	AOEPrune(prunefield,axis_name)

    | AOEDistinct_n ->
	let distinctfield = parse_attrs parse_rqname_string input_distinct_field_attr_name in
	AOEDistinct(distinctfield)
*)
	  
    | AOEParse_n ->
	let doc_name = parse_attrs string_id docname_attr_name in
	AOEParse doc_name, NoSub_n, NoSub_n
    | AOEPromoteAnyString_n ->
	AOEPromoteAnyString, OneSub_n, NoSub_n
  in
  let dep_arity = get_dep_subexpr_kind_arity attrs dep_kind in
  let indep_arity = get_indep_subexpr_kind_arity attrs indep_kind in
  (* Do not consume end element *)
  (algop_name, (indep_kind, indep_arity), (dep_kind, dep_arity))

let parse_subexpression parse_algop (sub_type, arity) st =
  match sub_type, arity with
    | NoSub_n, 0   -> NoSub
    | OneSub_n, 1  -> OneSub  (parse_algop st)
    | TwoSub_n, 2  -> 
	let a1 = (parse_algop st) in 
	let a2 = (parse_algop st) in 
	  TwoSub (a1, a2)
    | ManySub_n, x -> 
	let sub_list = ref [] in
	  for i = 1 to x do
	    sub_list := (parse_algop st :: !sub_list)
	  done;
	  ManySub (Array.of_list (List.rev !sub_list))
    | _ -> raise (Query (Algebra_Parsing_Error ("Number of Subexpressions and type do not match")))


(* Below here is for the actual structural parsing *)
let rec algop_of_undecorated_element comp_ctxt st =
  let (name, indep_kind_arity, dep_kind_arity)  = parse_algop_header comp_ctxt st in    
  let dep   = parse_subexpression (algop_of_undecorated_element comp_ctxt) dep_kind_arity st in 
  let indep = parse_subexpression (algop_of_undecorated_element comp_ctxt) indep_kind_arity st in
  let _ = consume_end_element st in
    logical_aalgop_mkop name indep dep None None Finfo.bogus

let pass_parser elem_name parse_algop emsg st =
  element_parser elem_name (fun attrs st -> parse_algop st) emsg st 

let expression_parser parse_algop st = 
  pass_parser expression_elem_name parse_algop "expression parser" st 

let unbox_logical_algebra_statement comp_ctxt st =
  expression_parser (algop_of_undecorated_element comp_ctxt) st

let parse_logical_algebra_statement proc_ctxt (comp_ctxt:Compile_context.logical_compile_context) input =
  let nsenv                = add_all_ns (Processing_context.get_external_nsenv proc_ctxt) algebra_bindings in
  let _                    = Processing_context.set_namespace_env proc_ctxt nsenv in
  let (dtdopt, the_stream) = Streaming_parse.open_xml_stream_from_io input in   
  let parsed_expr          = apply_parser_skip_document unbox_logical_algebra_statement comp_ctxt the_stream in
  let _                    = annotate_algebraic_expression parsed_expr in
  (parsed_expr)

(***********)
(* Prolog  *)
(***********)

(*------------*)
(*-- Boxing --*)
(*------------*)

let box_fn_body box_algop func_defn (*type info reported elsewhere *) =
  let attrs = Array.to_list (Array.mapi
		(fun index term -> (make_input_attr_name index), (serializable_string_of_rqname term))
		func_defn.palgop_func_formal_args) in
  match !(func_defn.palgop_func_optimized_logical_plan) with
  | AOEFunctionImported ->
      construct_element function_imported_elem_name attrs []
  | AOEFunctionUser userbody ->
      let fn_b = box_algop userbody in
      construct_element function_body_elem_name attrs [fn_b]

let box_fn box_algop fn = 
  let (name,arity), signature, body, upd = fn.palgop_function_decl_desc in

    (* Attributes *)
  let arity_attr = arity_attr_name, (string_of_int arity) in
  let name_attr = fn_name_attr_name, (serializable_string_of_rqname name) in

    (* Elements *)
  let input_sig, output_sig = signature in
    (* this is nasty code *)
  let input_sig' = Array.of_list (List.map (fun x -> Some x) input_sig) in
  let signature_element = box_function_signature name input_sig' output_sig upd in
  let body_element = box_fn_body box_algop body in

  let children = signature_element :: body_element :: [] in    
  let attrs = arity_attr :: name_attr :: [] in

    construct_element function_decl_elem_name attrs children

let box_fns box_algop fdecl_list =
  let all_fns = List.map (box_fn box_algop) fdecl_list in
  let attrs = [fn_count_attr_name, (string_of_int (List.length all_fns))] in
    construct_element function_tag_elem_name attrs all_fns

let header_of_prolog_decl op =
  match op.alg_decl_name with
  | AOEVarDeclImported (ocdt, vn)
  | AOEVarDeclExternal (ocdt, vn) ->	  
      let e = box_optasequencetype ocdt in
      let a_vn = variable_name_attr_name, (serializable_string_of_rqname vn) in
      prolog_var_decl_external_name, a_vn :: [], e
  | AOEVarDecl( ocdt, vn ) ->
      let e = box_optasequencetype ocdt in
      let a_vn = variable_name_attr_name, (serializable_string_of_rqname vn) in
      prolog_var_decl_name, a_vn :: [], e
  | AOEValueIndexDecl kn ->
      let attrs = (kname_attr_name, kn) :: [] in
      prolog_value_index_decl_name, attrs, []
  | AOENameIndexDecl rname ->
      let elems = box_relem_symbol rname in
      let attrs = [] in
      prolog_name_index_decl_name, attrs, elems :: []

let box_prolog_decl box_algop prolog_decl =
  let dep_exprs   = box_subexpr box_algop prolog_decl.alg_decl_dep   in
  let indep_exprs = box_subexpr box_algop prolog_decl.alg_decl_indep in
  let elem_name, attributes, sub_elem = header_of_prolog_decl prolog_decl in
  let attributes = 
    (dep_subexpr_attrs prolog_decl.alg_decl_dep) @
    (indep_subexpr_attrs prolog_decl.alg_decl_indep) @
    attributes
  in
  let child_elements = sub_elem @ dep_exprs @ indep_exprs in
    construct_element elem_name attributes child_elements

let box_vars box_algop var_list =
  let all_vars = List.map (box_prolog_decl box_algop) var_list in
  let attrs = [prolog_var_count_attr_name, (string_of_int (List.length all_vars))] in
    construct_element prolog_vars_elem_name attrs all_vars
 
let box_indices box_algop index_list =  
  let all_indices = List.map (box_prolog_decl box_algop) index_list in
  let attrs =
    [prolog_index_count_attr_name, (string_of_int (List.length all_indices))]
  in
  construct_element prolog_indices_elem_name attrs all_indices

let box_prolog box_algop prolog =
  let fns     = box_fns  box_algop prolog.palgop_prolog_functions in
  let vars    = box_vars box_algop prolog.palgop_prolog_vars in
  let indices = box_indices box_algop prolog.palgop_prolog_indices in
  let prolog  = fns :: vars :: indices :: [] in
  construct_element algop_prolog_decl_elem_name [] prolog

(*------------*)
(*-- Unbox  --*)
(*------------*)

(* Prolog declaration *)

let unbox_prolog_decl_header st =  
   let prolog_decl_name, attrs = start_element_get_name st in 
   let parse_attrs x = get_attr_from_attr_list attrs x in
   match (get_prolog_algop_moniker prolog_decl_name) with
   | AOEVarDeclExternal_n ->
       let odt = unbox_optasequencetype st in
       let vn  = parse_attrs parse_rqname_string variable_name_attr_name in
       AOEVarDeclExternal (odt, vn), NoSub_n, NoSub_n, attrs
   | AOEVarDecl_n ->
       let odt = unbox_optasequencetype st in
       let vn  = parse_attrs parse_rqname_string variable_name_attr_name in
       AOEVarDecl (odt, vn), OneSub_n, NoSub_n, attrs
   | AOEValueIndexDecl_n ->
       let kn = parse_attrs (fun x -> x) kname_attr_name in
       AOEValueIndexDecl kn, OneSub_n, OneSub_n, attrs
   | AOENameIndexDecl_n ->
       let e_name = relem_symbol_parser st in
       AOENameIndexDecl e_name, NoSub_n, NoSub_n, attrs

let unbox_prolog_decl parse_algop st =
  let algop_name, indep_kind, dep_kind, attrs = unbox_prolog_decl_header st in
  let dep_arity = get_dep_subexpr_kind_arity attrs dep_kind in
  let indep_arity = get_indep_subexpr_kind_arity attrs indep_kind in
  let dep = parse_subexpression parse_algop (dep_kind, dep_arity) st in
  let indep = parse_subexpression parse_algop (indep_kind, indep_arity) st in
  let _ = consume_end_element st in
    logical_aalgop_decl_mkop algop_name indep dep Finfo.bogus

(* Vars *)

let unbox_module_var parse_algop attrs st = 
  let n_vars = get_attr_from_attr_list attrs int_of_string prolog_var_count_attr_name in
    get_multiple n_vars (unbox_prolog_decl parse_algop) st 

let module_var_parser parse_algop st = 
  element_parser prolog_vars_elem_name (unbox_module_var parse_algop) "var declaration" st

(* Keys *)

let unbox_module_index_definitions parse_algop attrs st = 
  let count = get_attr_from_attr_list attrs int_of_string prolog_index_count_attr_name in
    get_multiple count (unbox_prolog_decl parse_algop) st 
    

let module_index_definitions_parser parse_algop st =
  element_parser prolog_indices_elem_name (unbox_module_index_definitions parse_algop) "index definitions" st

(* Functions *)

let unbox_function_body name parse_algop attrs st =
  let aname, terms = List.split attrs in
  let body = 
    if (name = (alg_elem function_body_elem_name)) then 
      AOEFunctionUser(parse_algop st)
    else if (name = (alg_elem function_imported_elem_name)) then 
      AOEFunctionImported
    else 
      raise (Query (Algebra_Parsing_Error("Expecting a function body or imported body")))
  in
  let terms' = Array.of_list (List.map parse_rqname_string terms) in
    (* function body *)
    terms', body

let function_body_parser parse_algop st =
  let name, attrs = start_element_get_name st in
  let body = unbox_function_body name parse_algop attrs st in
  let _ = consume_end_element st in 
  body
(*  element_parser function_body_elem_name (unbox_function_body parse_algop) "fn_body" st *)

let unbox_function_decl parse_algop attrs st =
  let name  = get_attr_from_attr_list attrs parse_function_rqname_string fn_name_attr_name in
  let fname,arity,input_types, output_type, upd = function_signature_parser st in
  let input_types' = List.fold_left (fun cur_types opt ->
				       match opt with 
					   None -> raise (Query (Algebra_Parsing_Error ("User function contains None for input type")))
					 | Some s -> cur_types @ [s]) [] (Array.to_list input_types) in
  let args, body  = function_body_parser parse_algop st in
  let fn_body = fmkalgop_function_body args body None (Some output_type) in

    (* build return structure *)
  let decl_desc = (name, arity), (input_types',output_type), fn_body, upd in
    fmkalgop_function_decl decl_desc Finfo.bogus

let function_decl_parser parse_algop st =
  element_parser function_decl_elem_name (unbox_function_decl parse_algop) "function_decls" st

let unbox_module_function parse_algop attrs st = 
  let n_functions = get_attr_from_attr_list attrs int_of_string fn_count_attr_name in
    get_multiple n_functions (function_decl_parser parse_algop) st

let module_function_parser parse_algop st =
  element_parser function_tag_elem_name (unbox_module_function parse_algop) "module functions" st

(***********)
(* Modules *)
(***********)

(*------------*)
(*-- Boxing --*)
(*------------*)

let box_statements box_algop stmts = 
  let n_statements = number_of_statements_attr_name, (string_of_int (List.length stmts)) in
  let all_statements = List.map (box_statement box_algop) stmts in
    construct_element statement_declaration_elem_name (n_statements :: []) all_statements 

(* Only doing expressions *)
let algop_module_to_xml box_algop am = 
  let prolog     = box_prolog box_algop am.palgop_module_prolog in
  let statements = box_statements box_algop am.palgop_module_statements in
  let module_xml = prolog :: statements :: [] in
    construct_element algop_module_decl_elem_name [] module_xml 
 
let box_logical_algebra_module nsenv xmod = 
  let nsenv' = Namespace_context.add_all_ns nsenv [(algebra_prefix, algebra_uri)] in
  Small_stream_context.resolved_xml_stream_of_sexpr 
    (Small_stream_context.sexpr_of_rsexpr nsenv'
       (algop_module_to_xml undecorated_box_algop xmod))

(*------------*)
(*-- Unbox  --*)
(*------------*)

let unbox_module_statements parse_algop attrs st =
  let n_statements = get_attr_from_attr_list attrs int_of_string number_of_statements_attr_name in
    get_multiple n_statements (expression_parser parse_algop) st
  
let module_statements_parser parse_algop st =
  element_parser statement_declaration_elem_name (unbox_module_statements parse_algop) "statements" st

let unbox_prolog parse_algop attrs st =
  let fn_decls  = module_function_parser parse_algop st in
  let var_decls = module_var_parser parse_algop st in
  let index_defs  = module_index_definitions_parser parse_algop st in
    fmkalgop_prolog fn_decls var_decls index_defs 

let prolog_parser parse_algop st =
  element_parser algop_prolog_decl_elem_name (unbox_prolog parse_algop) "whole prolog" st

let unbox_algebra_module parse_algop attrs st =
  let prolog = prolog_parser parse_algop st in
  let stmts  = module_statements_parser parse_algop st in
    fmkalgop_xmodule prolog stmts

let algebra_module_parser parse_algop st = 
  element_parser algop_module_decl_elem_name (unbox_algebra_module parse_algop) "algebra module" st

let unbox_logical_algebra_module comp_ctxt st =
  algebra_module_parser (algop_of_undecorated_element comp_ctxt) st

let parse_logical_algebra_module proc_ctxt (comp_ctxt:Compile_context.logical_compile_context) input =
  let nsenv                = add_all_ns (Processing_context.get_external_nsenv proc_ctxt) algebra_bindings in
  let _                    = Processing_context.set_namespace_env proc_ctxt nsenv in
  let (dtdopt, the_stream) = Streaming_parse.open_xml_stream_from_io input in   
  let parsed_module        = apply_parser_skip_document unbox_logical_algebra_module comp_ctxt the_stream in
  let _                    = annotate_algebraic_module parsed_module in
  let comp_ctxt            = Compile_context.update_compile_context_from_module comp_ctxt parsed_module in 
    (parsed_module,comp_ctxt)

(************)
(* Closures *)
(************)

(*------------*)
(*-- Boxing --*)
(*------------*)

(*
   <alg:Env>
     <alg:Var cardinality=" "><alg:Bind> ...</alg:Bind>...</alg:Var>
     <alg:Tup cardinality=" "><alg:Bind> ...</alg:Bind>...</alg:Tup>
   </alg:Env>
*)
let box_closure_environment nsenv var_value_pairs tuple_field_pairs =
  let cardinality = (List.length var_value_pairs) in 
  let cardinality_attr = attribute_constructor (rtype_symbol arg_count_attr_name) 
      (Cursor.cursor_of_singleton 
	 (Streaming_util.fmktse_event (TSAX_atomicValue (new atomicInteger (Decimal._integer_of_int cardinality))) Finfo.bogus))
  in
  let var_value_stream = 
    element_constructor Dm_atomic_util.default_no_uri_dm closure_var_sym nsenv
     (Cursor.cursor_append cardinality_attr 
	(Cursor.cursor_list_fold (List.map (Planio_physical_value.box_var_value nsenv) var_value_pairs))) 
  in
  let cardinality = (List.length tuple_field_pairs) in 
  let cardinality_attr = attribute_constructor (rtype_symbol arg_count_attr_name) 
      (Cursor.cursor_of_singleton 
	 (Streaming_util.fmktse_event (TSAX_atomicValue (new atomicInteger (Decimal._integer_of_int cardinality))) Finfo.bogus))
  in
  let tuple_field_stream = 
    element_constructor Dm_atomic_util.default_no_uri_dm closure_tuple_sym nsenv
     (Cursor.cursor_append cardinality_attr 
	(Cursor.cursor_list_fold (List.map (Planio_physical_value.box_var_value nsenv) tuple_field_pairs)))
  in
  Streaming_ops.erase_xml_stream(
    element_constructor Dm_atomic_util.default_no_uri_dm closure_env_sym nsenv
     (Cursor.cursor_append var_value_stream tuple_field_stream))

(*
   <alg:Closure>
     <alg:Env> ... </alg:Env>
     <alg:Expression> ... </alg:Expression>
   </alg:Closure>
*)
let box_closure nsenv env_stream plan_stream = 
  element_constructor_of_resolved 
    Dm_atomic_util.default_no_uri_dm closure_sym nsenv (Cursor.cursor_append env_stream plan_stream)

(*------------*)
(*-- Unbox  --*)
(*------------*)

let unbox_closure_env comp_ctxt st =
  let _ = start_element (closure_env_sym) st in
  print_dxq_debug ("Saw <Env>\n");

  let attrs = start_element (closure_var_sym) st in
  let cardinality = get_attr_from_attr_list attrs (int_of_string) arg_count_attr_name in 
  print_dxq_debug ("Var Cardinality "^(string_of_int cardinality)^"\n");
  let var_value_pairs = get_multiple cardinality (Planio_physical_value.unbox_var_value) st in 
  let _ = consume_end_element st in 

  let attrs = start_element (closure_tuple_sym) st in
  let cardinality = get_attr_from_attr_list attrs (int_of_string) arg_count_attr_name in 
  print_dxq_debug ("Tuple Cardinality "^(string_of_int cardinality)^"\n");
  let tuple_value_pairs = get_multiple cardinality (Planio_physical_value.unbox_var_value) st in 
  let _ = consume_end_element st in 

  print_dxq_debug ("After bindings\n");
  let _ = consume_end_element st in 
  print_dxq_debug ("Saw </Env>\n");
  (var_value_pairs, tuple_value_pairs)

let unbox_closure comp_ctxt attrs st = 
  print_dxq_debug ("In unbox_closure\n");
  let env =  unbox_closure_env comp_ctxt st in  
  let plan = unbox_logical_algebra_statement comp_ctxt st in 
  (env, plan)  

let closure_parser comp_ctxt st =
  element_parser closure_elem_name (unbox_closure comp_ctxt) "Closure" st

let parse_closure proc_ctxt (comp_ctxt:Compile_context.logical_compile_context) input =
  let nsenv                = add_all_ns (Processing_context.get_external_nsenv proc_ctxt) algebra_bindings in
  let _                    = Processing_context.set_namespace_env proc_ctxt nsenv in
  let (dtdopt, the_stream) = Streaming_parse.open_xml_stream_from_io input in   
  let (env, plan)          = apply_parser_skip_document closure_parser comp_ctxt the_stream in
  let _                    = annotate_algebraic_expression plan in
  (env, plan)


