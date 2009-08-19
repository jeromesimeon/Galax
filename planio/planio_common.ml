(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: planio_common.ml,v 1.15 2007/10/16 01:25:35 mff Exp $ *)

(* Module: Planio_common
   Description:
     This module contains common data for query plan parser and the
     printer.
*)

open Datatypes
open Error

open Dm_util

open Namespace_names
open Namespace_symbols

(* open Print_common *)

open Small_stream_ast
open Streaming_types

open Xquery_common_ast
open Xquery_core_ast
open Xquery_algebra_ast 
open Xquery_physical_type_ast

exception AlgebraParseError of string

let algebra_prefix = NSPrefix "a"
let algebra_uri = NSUri "http://www.galaxquery.org/alg/2007"
let algebra_bindings = [(algebra_prefix, algebra_uri)]

let alg_elem x      = Namespace_symbols.relem_symbol x
let alg_attr x      = Namespace_symbols.rattr_symbol x
 
let elem_name name =  (algebra_prefix, algebra_uri, name)
let attr_name name =  (algebra_prefix, algebra_uri, name)

let string_of_rattr_symbol rsya = Namespace_symbols.rattr_prefix_string rsya 
let string_of_relem_symbol rsye = Namespace_symbols.relem_prefix_string rsye
let string_of_rtype_symbol rsyg = Namespace_symbols.rtype_prefix_string rsyg

(* Constants for element and attribute names *)
let function_tag_elem_name = elem_name "Funcs"
let function_body_elem_name = elem_name "FuncBody"
let function_imported_elem_name = elem_name "FuncImported"

let input_datamodel_elem_name = elem_name "InDM"
let output_datamodel_elem_name = elem_name "OutDM"
let datamodel_signature = elem_name "DMSig"

let dep_attr_name   = elem_name "Dep"
let indep_attr_name = elem_name "Ind"

let dep_arity_attr_name = attr_name "DepCt"
let indep_arity_attr_name = attr_name "IndepCt"

let input_type_elem_name = elem_name "InTy"
let output_type_elem_name = elem_name "OutTy"
let function_signature_elem_name = elem_name "FuncSig"

let out_type_attr_name = attr_name "OutTy"
let arg_count_attr_name = attr_name "NArg"
let index_attr_name = attr_name "Idx"
let datamodel_attr_name = attr_name "DM"

let input_prune_field_attr_name = attr_name "PruneFld"
let input_distinct_field_attr_name = attr_name "DistinctFld"

(* Put in a sample format of the prolog *)

let no_input   = "No"
let one_input  = "One"
let two_input  = "Two"
let many_input = "Many"

(* Axis Related *)
let node_test_elem_name = elem_name "NodeTest"

let name_test_attr_name = attr_name "NameTest"
let name_test_elem_name = elem_name "NameTest"

let kind_test_elem_name = elem_name "KindTest"
let kind_test_attr_name = attr_name "Kind"

let pi_kind_test_elem_name = elem_name "PIKindTest"
let pi_kind_attr_name = attr_name "PIArg"
let axis_attr_name = attr_name "Axis"

(* Typeswitch Element *)
let ts_case_elem_name = elem_name "Case"
let ts_default_elem_name = elem_name "Def"
let ts_num_branches_attr_name = attr_name "NBranches"
(* Tuple element *)
let tuple_slot_elem_name = elem_name "Tup"

let var_attr_name = attr_name "Var"
let type_attr_name = attr_name "Ty"

let vname_attr_name = attr_name "Vname"
let stablekind_attr_name = attr_name "StblKind"
let sortkind_attr_name = attr_name "SortKind"
let emptysortkind_attr_name = attr_name "EmptySortKind"
let optdatatype_attr_name = attr_name "OptDataTy"
let pos_attr_name = attr_name "Pos"
let usage_attr_name = attr_name "Usage"
let value_attr_name = attr_name "Value"
let atomic_type_attr_name = attr_name "AtomicTy"
let target_attr_name = attr_name "Target"
let content_attr_name = attr_name "Content"
let attr_name_attr_name = attr_name "AttrName"
let elem_name_attr_name = attr_name "ElemName"
let fn_name_attr_name = attr_name "Name"
let arity_attr_name = attr_name "Arity"
let updating_attr_name = attr_name "Updating" 
let kname_attr_name = attr_name "KN"
let host_attr_name = attr_name "Host"
let port_attr_name = attr_name "Port"
let tuple_name_attr_name = attr_name "Tup"
let occurrence_attr_name = attr_name "Occ"


let prefix_attr_name = attr_name "Prefix"
let uri_attr_name = attr_name "Uri"
let rqname_attr_name = attr_name "RQName"

let relem_elem_name = elem_name "RElem"
let rattr_elem_name = elem_name "RAttr"
let rtype_elem_name = elem_name "RTy"

let n_sort_criteria_attr_name = attr_name "NSortCriteria"
let stable_attr_name = attr_name "Stbl"
let sort_spec_elem_name = elem_name "SortSpec"
let empty_sort_kind_attr_name = attr_name "EmptySortKind"
let sort_kind_attr_name = attr_name "SortKind"

let item_tuple_attr_name = attr_name "ItemTupVar"

let project_elem_name = elem_name "Project"
let project_name_elem_name = elem_name "ProjectFld"
let project_name_attr_name = attr_name "Name"
let value_of_flag_attr_name = attr_name "ValueOfFlag"
let insert_location_attr_name = attr_name "InsertLoc"
let snap_modifier_attr_name   = attr_name "SnapMod"
let docname_attr_name   = attr_name "DocName"

let asequencetype_elem_name = elem_name "seqTy"
(* ait element names *)
let ait_attributetype_elem_name = elem_name "ElemName"
let ait_elementtype_elem_name = elem_name "ElemTy"
let ait_schemaattribute_elem_name = elem_name "SchAttr"
let ait_schemaelementtype_elem_name = elem_name "SchElemTy"
let ait_kindtest_elem_name = elem_name "Kindtest"
let ait_typeref_elem_name = elem_name "TypeRef"
let ait_node_elem_name = elem_name "Node"
let ait_item_elem_name = elem_name "Item"
let ait_numeric_elem_name = elem_name "Num"
let ait_anystring_elem_name = elem_name "AnyStr"
let ait_text_elem_name = elem_name "Text"
let ait_comment_elem_name = elem_name "Cmnt"
let ait_processing_instruction_elem_name = elem_name "PI"
let ait_empty_elem_name = elem_name "Empty"
let ait_anyatomic_elem_name = elem_name "AnyAtom"
let ait_atomic_elem_name = elem_name "Atom"
let ait_document_elem_name = elem_name "Doc"

(* ait attribute names *)
let schema_attr_name_attr_name = attr_name "SchAttr"
let schema_elem_name_attr_name = attr_name "SchElem"
let type_ref_attr_name = attr_name "TypeRef"
let pi_arg_attr_name = attr_name "PIArg"
let atomic_type_attr_name = attr_name "AtomTy"

let function_decl_elem_name = elem_name "FuncDcl"
let fn_count_attr_name = attr_name "NFuncs"

let variable_declarations_elem_name = elem_name "VarDcls"
let variable_count_attr_name        = attr_name "NVars"
let variable_name_attr_name         = attr_name "VarName"

let null_index_attr_name = attr_name "NullIdx"
let input_dot_attr_name  = attr_name "InDot"
let output_dot_attr_name = attr_name "OutDot"

let prolog_elem_name  = elem_name "PlgElem"
let index_definitions_elem_name = elem_name "IdxDefs"
let index_def_elem_name = elem_name "IdxDef"
let op_1_elem_name    = elem_name "Op1"
let op_2_elem_name    = elem_name "Op2"
let kname_attr_name   = attr_name "IdxName"
let indices_count_attr_name = attr_name "NIdxs"

let statement_declaration_elem_name = elem_name "Stmts"
let expression_elem_name = elem_name "Expr"
let number_of_statements_attr_name = attr_name "NStmts"

let algop_module_decl_elem_name = elem_name "ModuleDcl"

let closure_attribute_elem_name = elem_name "Attr"
let closure_bind_elem_name = elem_name "Bind"
let closure_elem_name = elem_name "Closure"
let closure_env_elem_name = elem_name "Env"
let closure_error_elem_name = elem_name "Error"
let closure_result_elem_name = elem_name "Result"
let closure_table_elem_name = elem_name "Table"
let closure_tree_elem_name = elem_name "Tree"
let closure_tuple_elem_name = elem_name "Tup"
let closure_var_elem_name = elem_name "Var"

let closure_attribute_sym  = Namespace_symbols.relem_symbol closure_attribute_elem_name
let closure_bind_sym  = Namespace_symbols.relem_symbol closure_bind_elem_name
let closure_env_sym  = Namespace_symbols.relem_symbol closure_env_elem_name
let closure_error_sym  = Namespace_symbols.relem_symbol closure_error_elem_name
let closure_result_sym  = Namespace_symbols.relem_symbol closure_result_elem_name
let closure_table_sym  = Namespace_symbols.relem_symbol closure_table_elem_name
let closure_tree_sym  = Namespace_symbols.relem_symbol closure_tree_elem_name
let closure_sym  = Namespace_symbols.relem_symbol closure_elem_name 
let closure_tuple_sym  = Namespace_symbols.relem_symbol closure_tuple_elem_name
let closure_var_sym  = Namespace_symbols.relem_symbol closure_var_elem_name

(* To avoid symbol lookups, these are all precomputed *)

let xs_string_rqname = (algebra_prefix,algebra_uri,(Datatypes_util.string_of_atomic_type ATString))
let xs_string_sym = rtype_symbol xs_string_rqname 

let xs_boolean_rqname = (algebra_prefix,algebra_uri,(Datatypes_util.string_of_atomic_type ATBoolean))
let xs_boolean_sym = rtype_symbol xs_boolean_rqname 

let xs_decimal_rqname = (algebra_prefix,algebra_uri,(Datatypes_util.string_of_atomic_type ATDecimal))
let xs_decimal_sym = rtype_symbol xs_decimal_rqname 

let xs_float_rqname = (algebra_prefix,algebra_uri,(Datatypes_util.string_of_atomic_type ATFloat))
let xs_float_sym = rtype_symbol xs_float_rqname 

let xs_double_rqname = (algebra_prefix,algebra_uri,(Datatypes_util.string_of_atomic_type ATDouble))
let xs_double_sym = rtype_symbol xs_double_rqname 

let xs_duration_rqname = (algebra_prefix,algebra_uri,(Datatypes_util.string_of_atomic_type ATDuration))
let xs_duration_sym = rtype_symbol xs_duration_rqname 

let xs_dateTime_rqname = (algebra_prefix,algebra_uri,(Datatypes_util.string_of_atomic_type ATDateTime))
let xs_dateTime_sym = rtype_symbol xs_dateTime_rqname 

let xs_time_rqname = (algebra_prefix,algebra_uri,(Datatypes_util.string_of_atomic_type ATTime))
let xs_time_sym = rtype_symbol xs_time_rqname 

let xs_date_rqname = (algebra_prefix,algebra_uri,(Datatypes_util.string_of_atomic_type ATDate))
let xs_date_sym = rtype_symbol xs_date_rqname 

let xs_gYearMonth_rqname = (algebra_prefix,algebra_uri,(Datatypes_util.string_of_atomic_type ATGYearMonth))
let xs_gYearMonth_sym = rtype_symbol xs_gYearMonth_rqname 

let xs_gYear_rqname = (algebra_prefix,algebra_uri,(Datatypes_util.string_of_atomic_type ATGYear))
let xs_gYear_sym = rtype_symbol xs_gYear_rqname 

let xs_gMonthDay_rqname = (algebra_prefix,algebra_uri,(Datatypes_util.string_of_atomic_type ATGMonthDay))
let xs_gMonthDay_sym = rtype_symbol xs_gMonthDay_rqname 

let xs_gDay_rqname = (algebra_prefix,algebra_uri,(Datatypes_util.string_of_atomic_type ATGDay))
let xs_gDay_sym = rtype_symbol xs_gDay_rqname 

let xs_gMonth_rqname = (algebra_prefix,algebra_uri,(Datatypes_util.string_of_atomic_type ATGMonth))
let xs_gMonth_sym = rtype_symbol xs_gMonth_rqname 

let xs_hexBinary_rqname = (algebra_prefix,algebra_uri,(Datatypes_util.string_of_atomic_type ATHexBinary))
let xs_hexBinary_sym = rtype_symbol xs_hexBinary_rqname 

let xs_base64Binary_rqname = (algebra_prefix,algebra_uri,(Datatypes_util.string_of_atomic_type ATBase64Binary))
let xs_base64Binary_sym = rtype_symbol xs_base64Binary_rqname 

let xs_anyURI_rqname = (algebra_prefix,algebra_uri,(Datatypes_util.string_of_atomic_type ATAnyURI))
let xs_anyURI_sym = rtype_symbol xs_anyURI_rqname 

let xs_QName_rqname = (algebra_prefix,algebra_uri,(Datatypes_util.string_of_atomic_type ATQName))
let xs_QName_sym = rtype_symbol xs_QName_rqname 

let xs_NOTATION_rqname = (algebra_prefix,algebra_uri,(Datatypes_util.string_of_atomic_type ATNOTATION))
let xs_NOTATION_sym = rtype_symbol xs_NOTATION_rqname 

let xs_integer_rqname = (algebra_prefix,algebra_uri,(Datatypes_util.string_of_atomic_type ATInteger))
let xs_integer_sym = rtype_symbol xs_integer_rqname 

let xs_yearMonthDuration_rqname = (algebra_prefix,algebra_uri,(Datatypes_util.string_of_atomic_type ATYearMonthDuration))
let xs_yearMonthDuration_sym = rtype_symbol xs_yearMonthDuration_rqname 

let xs_dayTimeDuration_rqname = (algebra_prefix,algebra_uri,(Datatypes_util.string_of_atomic_type ATDayTimeDuration))
let xs_dayTimeDuration_sym = rtype_symbol xs_dayTimeDuration_rqname 

let xs_untypedAtomic_rqname = (algebra_prefix,algebra_uri,(Datatypes_util.string_of_atomic_type ATUntypedAtomic))
let xs_untypedAtomic_sym = rtype_symbol xs_untypedAtomic_rqname 

let xs_anyAtomicType_rqname = (algebra_prefix,algebra_uri,(Datatypes_util.string_of_atomic_type ATAnyAtomic))
let xs_anyAtomicType_sym = rtype_symbol xs_anyAtomicType_rqname 

let elem_name_of_atomic_type at =
  match at with
  | ATString   	   	-> xs_string_rqname
  | ATBoolean  	   	-> xs_boolean_rqname
  | ATDecimal  	   	-> xs_decimal_rqname
  | ATFloat    	   	-> xs_float_rqname
  | ATDouble   	   	-> xs_double_rqname
  | ATDuration 	   	-> xs_duration_rqname
  | ATDateTime 	   	-> xs_dateTime_rqname
  | ATTime     	   	-> xs_time_rqname
  | ATDate     	   	-> xs_date_rqname
  | ATGYearMonth   	-> xs_gYearMonth_rqname
  | ATGYear        	-> xs_gYear_rqname
  | ATGMonthDay    	-> xs_gMonthDay_rqname
  | ATGDay         	-> xs_gDay_rqname
  | ATGMonth       	-> xs_gMonth_rqname
  | ATHexBinary    	-> xs_hexBinary_rqname
  | ATBase64Binary 	-> xs_base64Binary_rqname
  | ATAnyURI   	   	-> xs_anyURI_rqname
  | ATQName    	   	-> xs_QName_rqname
  | ATNOTATION 	   	-> xs_NOTATION_rqname
  | ATInteger  	   	-> xs_integer_rqname
  | ATYearMonthDuration -> xs_yearMonthDuration_rqname
  | ATDayTimeDuration 	-> xs_dayTimeDuration_rqname
  | ATUntypedAtomic    	-> xs_untypedAtomic_rqname
  | ATAnyAtomic    	-> xs_anyAtomicType_rqname

let elem_sym_of_atomic_type at =
  match at with
  | ATString   	   	-> xs_string_sym
  | ATBoolean  	   	-> xs_boolean_sym
  | ATDecimal  	   	-> xs_decimal_sym
  | ATFloat    	   	-> xs_float_sym
  | ATDouble   	   	-> xs_double_sym
  | ATDuration 	   	-> xs_duration_sym
  | ATDateTime 	   	-> xs_dateTime_sym
  | ATTime     	   	-> xs_time_sym
  | ATDate     	   	-> xs_date_sym
  | ATGYearMonth   	-> xs_gYearMonth_sym
  | ATGYear        	-> xs_gYear_sym
  | ATGMonthDay    	-> xs_gMonthDay_sym
  | ATGDay         	-> xs_gDay_sym
  | ATGMonth       	-> xs_gMonth_sym
  | ATHexBinary    	-> xs_hexBinary_sym
  | ATBase64Binary 	-> xs_base64Binary_sym
  | ATAnyURI   	   	-> xs_anyURI_sym
  | ATQName    	   	-> xs_QName_sym
  | ATNOTATION 	   	-> xs_NOTATION_sym
  | ATInteger  	   	-> xs_integer_sym
  | ATYearMonthDuration -> xs_yearMonthDuration_sym
  | ATDayTimeDuration 	-> xs_dayTimeDuration_sym
  | ATUntypedAtomic    	-> xs_untypedAtomic_sym
  | ATAnyAtomic    	-> xs_anyAtomicType_sym

let atomic_type_of_elem_sym elem_sym =
  if (symbol_equal elem_sym xs_string_sym) then (stringsym, ATString)
  else if (symbol_equal elem_sym xs_boolean_sym) then (booleansym, ATBoolean)
  else if (symbol_equal elem_sym xs_decimal_sym) then (decimalsym, ATDecimal)
  else if (symbol_equal elem_sym xs_float_sym) then (floatsym, ATFloat)
  else if (symbol_equal elem_sym xs_double_sym) then (doublesym, ATDouble)
  else if (symbol_equal elem_sym xs_duration_sym) then (durationsym, ATDuration)
  else if (symbol_equal elem_sym xs_dateTime_sym) then (dateTimesym, ATDateTime)
  else if (symbol_equal elem_sym xs_time_sym) then (timesym, ATTime)
  else if (symbol_equal elem_sym xs_date_sym) then (datesym, ATDate)
  else if (symbol_equal elem_sym xs_gYearMonth_sym) then (gYearMonthsym, ATGYearMonth)
  else if (symbol_equal elem_sym xs_gYear_sym) then (gYearsym, ATGYear)
  else if (symbol_equal elem_sym xs_gMonthDay_sym) then (gMonthDaysym, ATGMonthDay)
  else if (symbol_equal elem_sym xs_gDay_sym) then (gDaysym, ATGDay)
  else if (symbol_equal elem_sym xs_gMonth_sym) then (gMonthsym,ATGMonth)
  else if (symbol_equal elem_sym xs_hexBinary_sym) then (hexBinarysym, ATHexBinary)
  else if (symbol_equal elem_sym xs_base64Binary_sym) then  (base64Binarysym, ATBase64Binary)
  else if (symbol_equal elem_sym xs_anyURI_sym) then (anyURIsym, ATAnyURI)
  else if (symbol_equal elem_sym xs_QName_sym) then (qnamesym, ATQName)
  else if (symbol_equal elem_sym xs_NOTATION_sym) then (notationsym,ATNOTATION)
  else if (symbol_equal elem_sym xs_integer_sym) then  (integersym, ATInteger)
  else if (symbol_equal elem_sym xs_yearMonthDuration_sym) then (yearMonthDurationsym, ATYearMonthDuration)
  else if (symbol_equal elem_sym xs_dayTimeDuration_sym) then (dayTimeDurationsym, ATDayTimeDuration)
  else if (symbol_equal elem_sym xs_untypedAtomic_sym) then (untypedAtomicsym, ATUntypedAtomic)
  else if (symbol_equal elem_sym xs_anyAtomicType_sym) then (anyAtomicTypesym, ATAnyAtomic)
  else raise Not_found

(* This seems a bit hackish, but since we are in a well-known attribute, we are OK. *)

let xml_string_of_prefix pref = 
  match pref with
    | NSDefaultElementPrefix  -> "NSDefElemPref"
    | NSDefaultFunctionPrefix -> "NSDefFuncPref"
    | NSWildcardPrefix        -> "NSWildcardPref"
    | NSPrefix(pref_name)     -> pref_name (* This is the hackish portion since it won't give warning...*)
    | NSServerPrefix p
    | NSInterfacePrefix p -> raise(Query(Internal_Error("Server or interface prefix '"^p^"' in xml_string_of_prefix")))

let xml_prefix_of_string str  = 
  match str with
    | "NSDefElemPref"  -> NSDefaultElementPrefix
    | "NSDefFuncPref"  -> NSDefaultFunctionPrefix
    | "NSWildcardPref" -> NSWildcardPrefix
    | x                -> NSPrefix x 
      
let string_of_optint oi = 
  match oi with
      None -> "None"
    | Some i' -> string_of_int i'

let optint_of_string str =
  match str with
      "None" -> None
    | x -> Some (int_of_string str)

(* SortKind *)
let string_of_sortkind sk = 
  match sk with
    | Ascending -> "Asc"
    | Descending -> "Desc"

let sortkind_of_string str = 
  match str with
    | "Asc" -> Ascending
    | "Desc" -> Descending
    | _ -> raise (Query (Algebra_Parsing_Error ("Unknown SortKind: " ^ str)))

(* Stablekind *)
let string_of_stablekind stable =
  match stable with
    | Stable -> "Stbl"
    | NonStable -> "NonStbl"

let stablekind_of_string str =
  match str with 
    | "Stbl" -> Stable
    | "NonStbl" -> NonStable
    | _ -> raise (Query (Algebra_Parsing_Error ("Unknown StableKind: " ^ str)))

(* Empty Sort Kind *)
let string_of_emptysortkind esk = 
  match esk with
    | EmptyGreatest -> "EmptyGt"
    | EmptyLeast -> "EmptyLt"

let emptysortkind_of_string str = 
  match str with
    | "EmptyGt" -> EmptyGreatest 
    | "EmptyLt" -> EmptyLeast 
    | _ -> raise (Query (Algebra_Parsing_Error ("Unknown EmptySortKind: " ^ str)))

let axis_of_string str =
  match str with
  | "ances" -> Ancestor
  | "ances-or-self" -> Ancestor_or_self 
  | "attr" -> Attribute
  | "child" -> Child
  | "desc" -> Descendant     
  | "desc-or-self" -> Descendant_or_self
  | "follow-sib" -> Following_sibling
  | "preced-sib" -> Preceding_sibling
  | "parent" -> Parent
  | "self" -> Self
  | _ -> raise (Query (Algebra_Parsing_Error ("Unknown axis expression in algebra parsing")))


let string_of_axis str =
  match str with
  | Ancestor	      ->  "ances"         
  | Ancestor_or_self  ->  "ances-or-self" 
  | Attribute	      ->  "attr"          
  | Child	      ->  "child"         
  | Descendant        ->  "desc"          
  | Descendant_or_self->  "desc-or-self"  
  | Following_sibling ->  "follow-sib"    
  | Preceding_sibling ->  "preced-sib"    
  | Parent	      ->  "parent"        
  | Self              ->  "self"          
  | _ -> raise (Query (Algebra_Parsing_Error ("Unknown axis expression in algebra parsing")))

let string_of_physical_dom_type mt =
  match mt with 
  | PT_CursorSeq -> "ItemCur"
  | PT_ListSeq -> "ItemList"

let string_of_physical_sax_type st =
  match st with 
  | PT_Stream -> "SaxStream"
  | PT_Discarded -> "SaxDiscarded"

let string_of_physical_xml_type pt =
  match pt with 
  | PT_Dom mt -> string_of_physical_dom_type mt
  | PT_Sax st -> string_of_physical_sax_type st

let physical_type_of_xml_type_string xtstr = 
  match xtstr with 
  | "ItemCur"   -> (PT_Dom PT_CursorSeq)
  | "ItemList"     -> (PT_Dom PT_ListSeq)
  | "SaxStream"    -> (PT_Sax PT_Stream)
  | "SaxDiscarded" -> (PT_Sax PT_Discarded)
  | _ -> raise (Query (Algebra_Parsing_Error ("Unknown Physical Type " ^ xtstr)))

(* Serializing an RQName *)
let serializable_string_of_rqname rqname = 
  let (prefix, uri, ncname) = rqname in 
  if (Namespace_builtin.is_built_in_namespace(prefix,uri)) then prefixed_string_of_rqname rqname
  else Namespace_names.curly_uri_string_of_rqname rqname

let parse_rqname_string str = 
  try
    Namespace_names.parse_curly_uri_string str
  with
  | _ ->     
      Namespace_resolve.resolve_element_qname Namespace_context.default_all_nsenv (Namespace_names.uqname_element_of_string str)

let parse_function_rqname_string str = 
  try
    Namespace_names.parse_curly_uri_string str
  with
  | _ ->     
      Namespace_resolve.resolve_function_qname Namespace_context.default_all_nsenv (Namespace_names.uqname_function_of_string str)

let physical_type_of_tuple_type_string tupstr = 
  List.map (fun vxt -> 
    let (v, xtstr) = Gmisc.split_left_on_char vxt ':' 
    in (parse_rqname_string v, physical_type_of_xml_type_string xtstr) 
    ) (Gmisc.split_on_char tupstr ';') 

let string_of_physical_tuple_type tups = 
  String.concat ";" (List.map (fun (v, xt) -> "("^(serializable_string_of_rqname v)^":"^(string_of_physical_xml_type xt)^")") tups)

let string_of_physical_type pt =
  match pt with 
  | PT_XML xt -> "xml("^(string_of_physical_xml_type xt)^")"
  | PT_Table tt -> "tup("^(string_of_physical_tuple_type tt)^")"

let physical_type_of_string str = 
  match String.sub str 0 4 with 
    | "xml("  -> PT_XML(physical_type_of_xml_type_string (String.sub str 4 ((String.length str) - 5)))
    | "tup("  -> PT_Table(physical_type_of_tuple_type_string (String.sub str 4 ((String.length str) - 5)))
    | _ -> raise (Query (Algebra_Parsing_Error ("Unknown Physical Type " ^ str)))

let attr_string_of_atomic_type at = Namespace_symbols.symbol_prefix_string (Datatypes_util.symbol_of_primitive_type at)

(* Only handling builtin types right now *)
let atomic_type_of_attr_string str =
  let rqname = parse_rqname_string str in 
  let type_sym = Namespace_symbols.rtype_symbol rqname in
  try
    Datatypes_util.lookup_bltin_type type_sym
  with
    | _ -> raise (Query (Algebra_Parsing_Error ("Unknown Atomic type: " ^ str)))

(**********************)
(* Compile Annotation *)
(**********************)

let compile_annotation_elem_name  = elem_name "CompileAannotationName"
let accessed_fields_elem_name     = elem_name "AccdFlds"
let returned_fields_elem_name     = elem_name "RetFlds"
let free_variables_elem_name      = elem_name "FreeVars"
    (* Attributes *)
let num_accessed_fields_attr_name = attr_name "NAccdFlds"
let num_returned_fields_attr_name = attr_name "NRetFlds"
let num_free_variables_attr_name  = attr_name "NFreeVars"
(**************)
(* Overloaded *)
(**************)

let overloaded_call_elem_name    = elem_name "OverCallTab"

(**********************)
(* Grouping operation *)
(**********************)
let vname_elem_name          = elem_name "VarName"
(* group_desc *)
let gbn_elem_name            = elem_name "GroupByName"
let induced_elem_name        = elem_name "InducedGroups"
let mbv_elem_name            = elem_name "MustBeValid"
let agg_elem_name            = elem_name "AggregateName"
let group_desc_elem_name     = elem_name "GroupDesc"
let distinct_value_attr_name = attr_name "distinct"

(**************)
(* Predicates *)
(**************)
let simple_conjunct_elem_name       = elem_name "SimCjctElem"
let complex_conjunct_elem_name      = elem_name "ComplexCjctElem"
let disjunct_elem_name              = elem_name "DjctElem"
let simple_conjunct_start_attr_name = attr_name "SimCjctStart"
let simple_conjunct_end_attr_name   = attr_name "SimCjctEnd"

(**********************)
(* TupleTreePatterns  *)
(**********************)
let ttp_name            = elem_name "TTP"
let twig_node_name      = elem_name "TwigNode"
let in_field_attr_name = attr_name  "InFld"
let restore_attr_name   = attr_name "restore"
let out_field_attr_name = attr_name "OutFld"
let child_twig_name     = elem_name "ChildTwig"
let pred_twig_name      = elem_name "PredTwig"

(**********)
(* Prolog *)
(**********)
let prolog_var_decl_external_name = elem_name "VarDclExt"
let prolog_var_decl_name          = elem_name "VarDcl"
let prolog_value_index_decl_name  = elem_name "ValIdxDcl"
let prolog_name_index_decl_name   = elem_name "NameIdxDcl"

let prolog_var_decl_external_sym  = alg_elem prolog_var_decl_external_name 
let prolog_var_decl_sym           = alg_elem prolog_var_decl_name          
let prolog_value_index_decl_sym   = alg_elem prolog_value_index_decl_name  
let prolog_name_index_decl_sym    = alg_elem prolog_name_index_decl_name   

let prolog_var_count_attr_name    = attr_name "count"
let prolog_index_count_attr_name  = attr_name "count"
let prolog_vars_elem_name         = elem_name "VarDcls"
let prolog_indices_elem_name      = elem_name "index_defs"
let prolog_attr_name              = attr_name "prolog_op"

let algop_prolog_decl_elem_name   = elem_name "Prolog"

type prolog_algop_moniker = 
    AOEVarDeclExternal_n 
  | AOEVarDecl_n
  | AOEValueIndexDecl_n
  | AOENameIndexDecl_n

let get_prolog_algop_moniker name = 
  if (name = prolog_var_decl_external_sym) then AOEVarDeclExternal_n
  else if (name = prolog_var_decl_sym) then AOEVarDecl_n
  else if (name = prolog_value_index_decl_sym) then AOEValueIndexDecl_n
  else if (name = prolog_name_index_decl_sym) then AOENameIndexDecl_n
  else
    raise (Query (Malformed_Algebra_Expr 
		    ("Unknown prolog algop moniker: "^(Namespace_symbols.relem_prefix_string name))))

let prolog_algop_name n =
  match n with
  | AOEVarDeclImported (ocdt, vn)
  | AOEVarDeclExternal (ocdt, vn) ->
      prolog_var_decl_external_name
  | AOEVarDecl( ocdt, vn ) ->
      prolog_var_decl_name
  | AOEValueIndexDecl kn -> 
      prolog_value_index_decl_name
  | AOENameIndexDecl kn -> 
      prolog_name_index_decl_name

type algop_kind_moniker = 
  | NoSub_n
  | OneSub_n
  | TwoSub_n
  | ManySub_n

type algop_moniker = 
  | AOELetvar_n
  | AOEIf_n
  | AOEWhile_n
  | AOETypeswitch_n
  | AOEVar_n
  | AOEScalar_n
  | AOESeq_n
  | AOEEmpty_n
  | AOEDocument_n
  | AOEPI_n
  | AOEPIComputed_n
  | AOEComment_n
  | AOECommentComputed_n
  | AOEText_n
  | AOETextComputed_n
  | AOEElem_n
  | AOEAnyElem_n
  | AOEAttr_n
  | AOEAnyAttr_n
  | AOEError_n
  | AOETreat_n
  | AOEValidate_n
  | AOECast_n
  | AOECastable_n
  | AOESome_n 
  | AOEEvery_n

  | AOETreeJoin_n
  | AOETupleTreePattern_n
  | AOEProject_n
(*
  | AOEPrune_n
  | AOEDistinct_n
*)

  (* Function Calls *)
  | AOECallBuiltIn_n
  | AOECallOverloaded_n 
  | AOECallUserDefined_n
  | AOEConvertSimple_n
  | AOEPromoteNumeric_n
  | AOEUnsafePromoteNumeric_n
  (* DXQ *)
  | AOEServerImplements_n
  | AOEForServerClose_n
  | AOEEvalClosure_n
  | AOEExecute_n
  | AOEASyncExecute_n
  (* Tuples *)
  | AOECreateTuple_n          (* Creates a new tuple *)
  | AOEAccessTuple_n          (* Accesses a new tuple *)
  | AOEOrderBy_n
  | AOEProduct_n
  | AOESelect_n
  | AOEJoin_n
  | AOELeftOuterJoin_n
  | AOEConcatTuples_n         (* Tuple concatenation *)
  | AOEMapFromItem_n          (* Item to tuple iteration *)
  | AOEMapToItem_n            (* Tuple to item iteration *)
  | AOEMap_n                  (* Tuple iteration *)
  | AOEMapIndex_n
  | AOEMapIndexStep_n
  | AOEMapConcat_n
  | AOEOuterMapConcat_n
  | AOEInputTuple_n
  | AOEGroupBy_n
  | AOENullMap_n

  (* Updates *)
  | AOESnap_n
  | AOEDelete_n
  | AOEInsert_n
  | AOERename_n
  | AOEReplace_n
  | AOESequencing_n 

  | AOESet_n
  | AOEImperativeSeq_n

  (* Added because it didn't exist already *)
  | AOEParse_n
  | AOEPromoteAnyString_n

(* 
   This is here so we don't have to expose elem_name and attr_name constructors
   to force all the names to be registered between the parser and printer *)

(* This is here so we don't have to expose elem_name and attr_name
   constructors to force all the names to be registered between the
   parser and printer *)

let make_input_attr_name index = attr_name  ("in" ^ (string_of_int index))
    
let moniker_list = 
  List.map (fun (x,y) ->
	      (alg_elem (elem_name x)), y)
  [  ("Letvar"), AOELetvar_n;
     ("If") , AOEIf_n;
     ("While") , AOEWhile_n;
     ("TySwitch"), AOETypeswitch_n;
     ("Var")   , AOEVar_n;
     ("Scalar"), AOEScalar_n;
     ("Seq")   , AOESeq_n;
     ("Empty") , AOEEmpty_n;
     ("Document"), AOEDocument_n;
     ("PI"), AOEPI_n;
     ("PIComp"), AOEPIComputed_n;
     ("Comment"), AOEComment_n;
     ("CommentComp"), AOECommentComputed_n;
     ("Text"), AOEText_n;
     ("TextComp"), AOETextComputed_n;
     ("Elem"), AOEElem_n;
     ("AnyElem"), AOEAnyElem_n;
     ("Attr"), AOEAttr_n;
     ("AnyAttr"), AOEAnyAttr_n;
     ("Error") , AOEError_n;
     ("Treat") , AOETreat_n;
     ("Validate"), AOEValidate_n;
     ("Cast")  , AOECast_n;
     ("Castable"), AOECastable_n;
     ("Some"), AOESome_n ;
     ("Every"), AOEEvery_n;

     ("TreeJoin"), AOETreeJoin_n;
     ("TTP"), AOETupleTreePattern_n;
(*
     ("Prune"), AOEPrune_n;
     ("Distinct"), AOEDistinct_n;
*)	
     (* Function Calls *)
     ("CallBI"), AOECallBuiltIn_n;
     ("CallOver"), AOECallOverloaded_n ;
     ("CallUser"), AOECallUserDefined_n;
     ("ConvertSim"), AOEConvertSimple_n;
     ("PromNum"), AOEPromoteNumeric_n;
     ("UnsafePromNum"), AOEUnsafePromoteNumeric_n;
     ("ServerImplements")  , AOEServerImplements_n;
     ("Execute")  , AOEExecute_n;
     ("ASyncExecute")  , AOEASyncExecute_n;
     ("ForServerClose")  , AOEForServerClose_n;
     ("EvalClosure")  , AOEEvalClosure_n;

    (* Tuples *)
     ("CreateTup"), AOECreateTuple_n;         (* Creates a new tuple *)
     ("AccTup"), AOEAccessTuple_n;            (* Accesses a new tuple *)
     ("Project"),  AOEProject_n;
     ("Order"),  AOEOrderBy_n;
     ("Product"), AOEProduct_n;
     ("Select"), AOESelect_n;     
     ("Join", AOEJoin_n);
     ("LOJoin", AOELeftOuterJoin_n);
     ("CcatTups", AOEConcatTuples_n);
     ("MpFromItem", AOEMapFromItem_n);
     ("MpToItem",  AOEMapToItem_n );
     ("Mp" , AOEMap_n);
     ("MpIdx", AOEMapIndex_n);
     ("MpIdxStep", AOEMapIndexStep_n);
     ("MpCcat", AOEMapConcat_n);
     ("OuterMpCcat", AOEOuterMapConcat_n);
     ("InTup", AOEInputTuple_n);
     ("GroupBy", AOEGroupBy_n);
     ("NullMp", AOENullMap_n);

     (* Updates *)     
     ("Snap"), AOESnap_n;
     ("Delete"), AOEDelete_n;
     ("Insert"),AOEInsert_n;
     ("Rename"), AOERename_n;
     ("Replace"), AOEReplace_n;
     ("Sequencing"), AOESequencing_n;

     ("Set"), AOESet_n;
     ("ImperativeSeq"), AOEImperativeSeq_n;

     (* Added because it didn't exist for some reason - Doug *)
     ("Parse"), AOEParse_n;
     ("PromAnyStr"), AOEPromoteAnyString_n;
]                  (* Iteration over a sequence of tuples *)
  ;;
let moniker_hash = Hashtbl.create 167 ;;
let _ = 
  try
    List.iter (fun (x,y) -> Hashtbl.add moniker_hash x y) moniker_list
  with
  | e ->
      begin
	eprintf_error "  " e;
	Format.fprintf (!Conf.glx_err_formatter) "@."
      end

let get_moniker_of_algop alg_str = 
  try
    Hashtbl.find moniker_hash alg_str
  with Not_found ->
    raise (AlgebraParseError ("* Unknown Moniker Error: "
			      ^ (Namespace_symbols.relem_string alg_str)))

let __debug_verify_moniker = true
let verify_moniker m =
  let m' = alg_elem (elem_name m) in
    begin
      if (not (Hashtbl.mem moniker_hash m')) then	  
	  raise (Query (Malformed_Algebra_Expr ("Moniker not present: " ^ m)))
    end
    

let elem_create m = 
  if __debug_verify_moniker then
    verify_moniker m
  ;
  elem_name m 


let get_serializable_moniker_of_algop_name alg_name = 
  match alg_name with
  | AOELetvar _  -> elem_create "Letvar"
  | AOEIf     -> elem_create "If"
  | AOEWhile  -> elem_create "While"
  | AOETypeswitch _ -> elem_create "TySwitch"
  | AOEVar _   -> elem_create "Var"
  | AOEScalar _ -> elem_create "Scalar"
  | AOESeq    -> elem_create "Seq"
  | AOEImperativeSeq    -> elem_create "ImperativeSeq"
  | AOEEmpty  -> elem_create "Empty"
  | AOEDocument -> elem_create "Document"
  | AOEPI _ -> elem_create "PI"
  | AOEPIComputed -> elem_create "PIComp"
  | AOEComment _ -> elem_create "Comment"
  | AOECommentComputed -> elem_create "CommentComp"
  | AOEText _ -> elem_create "Text"
  | AOETextComputed -> elem_create "TextComp"
  | AOEElem _ -> elem_create "Elem"
  | AOEAnyElem _ -> elem_create "AnyElem"
  | AOEAttr _ -> elem_create "Attr"
  | AOEAnyAttr _ -> elem_create "AnyAttr"
  | AOEError  -> elem_create "Error"
  | AOETreat _  -> elem_create "Treat"
  | AOEValidate _ -> elem_create "Validate"
  | AOECast  _  -> elem_create "Cast"
  | AOECastable _ -> elem_create "Castable"
  | AOESome _ -> elem_create "Some" 
  | AOEEvery _ -> elem_create "Every"
	(* Function Calls *)
  | AOECallBuiltIn _ -> elem_create "CallBI"
  | AOECallOverloaded _ -> elem_create "CallOver" 
  | AOECallUserDefined _ -> elem_create "CallUser"
  | AOEUnsafePromoteNumeric _ -> elem_create "UnsafePromNum"
  | AOEPromoteNumeric _ -> elem_create "PromNum"
  | AOEPromoteAnyString -> elem_create "PromAnyStr"
  | AOEConvertSimple _ -> elem_create "ConvertSim"
  (* DXQ *)
  | AOEServerImplements  _ -> elem_create "ServerImplements"
  | AOEExecute _  -> elem_create "Execute"
  | AOEASyncExecute _  -> elem_create "ASyncExecute"
  | AOEForServerClose  _ -> elem_create "ForServerClose"
  | AOEEvalClosure -> elem_create "EvalClosure"
	(* Tuples *)
  | AOEMapConcat       -> elem_create "MpCcat"
  | AOEOuterMapConcat _ -> elem_create "OuterMpCcat"
  | AOECreateTuple _ -> elem_create "CreateTup"    (* Creates a new tuple *)
  | AOEAccessTuple _ -> elem_create "AccTup"    (* Accesses a new tuple *)
  | AOEMap -> elem_create "Mp"
  | AOEMapToItem -> elem_create "MpToItem"
  | AOEMapFromItem _ -> elem_create "MpFromItem"
  | AOEMapIndex _ -> elem_create "MpIdx"
  | AOEMapIndexStep _ -> elem_create "MpIdxStep"
  | AOEInputTuple -> elem_create "InTup"
  | AOEConcatTuples -> elem_create "CcatTups"
  | AOEProduct -> elem_create "Product"
  | AOESelect _ -> elem_create "Select"
  | AOEJoin _ -> elem_create "Join"
  | AOEOrderBy _ -> elem_create "Order"
  | AOECopy    -> elem_create "Copy"
  | AOEDelete    -> elem_create "Delete"
  | AOEInsert _ -> elem_create "Insert"
  | AOERename _ -> elem_create "Rename"
  | AOEReplace _ -> elem_create "Replace"
  | AOESnap _ -> elem_create "Snap"
  | AOESet _  -> elem_create "Set"
  | AOEGroupBy _  -> elem_create "GroupBy"
  | AOELeftOuterJoin _ -> elem_create "LOJoin"
  | AOENullMap _ -> elem_create "NullMp"
  | AOETreeJoin _ -> elem_create "TreeJoin"
  | AOETupleTreePattern _ -> elem_create "TTP"
  | AOEParse _ -> elem_create "Parse"
  | AOEProject _ -> elem_create "Project"
  | AOECharRef _ -> elem_create "CharRef"

let get_cardinality str =
  match str with 
    | "No"   -> 0
    | "One"  -> 1
    | "Two"  -> 2
    | "Many" -> 1 (* 1: Many means it is replicated *)
    | _ -> raise (Query (Algebra_Parsing_Error ("Unknown Input Cardinality")))

