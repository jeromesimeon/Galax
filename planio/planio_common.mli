(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: planio_common.mli,v 1.14 2007/10/16 01:25:35 mff Exp $ *)

(* Module: Planio_common
   Description:
     This module contains common data for query plan parser and the
     printer.
*)

open Namespace_names
open Namespace_symbols

val algebra_prefix : prefix

val algebra_uri    : uri 

val algebra_bindings : Namespace_context.binding_table

val alg_elem         : rqname -> Namespace_symbols.relem_symbol
val alg_attr         : rqname -> Namespace_symbols.rattr_symbol

val string_of_relem_symbol: Namespace_symbols.relem_symbol -> string
val string_of_rattr_symbol: Namespace_symbols.rattr_symbol -> string
val string_of_rtype_symbol: Namespace_symbols.rtype_symbol -> string

val function_tag_elem_name  : rqname
val function_body_elem_name : rqname
val function_imported_elem_name : rqname

val input_datamodel_elem_name : rqname
val output_datamodel_elem_name : rqname
val datamodel_signature : rqname

val dep_attr_name   : rqname
val dep_arity_attr_name : rqname

val indep_attr_name : rqname
val indep_arity_attr_name : rqname

val input_type_elem_name : rqname
val output_type_elem_name : rqname
val function_signature_elem_name : rqname

val out_type_attr_name : rqname
val arg_count_attr_name : rqname
val index_attr_name : rqname
val datamodel_attr_name : rqname

val no_input   : string
val one_input  : string
val two_input  : string
val many_input : string

val input_prune_field_attr_name    : rqname
val input_distinct_field_attr_name : rqname

(* Axis Related *)
val node_test_elem_name : rqname

val name_test_attr_name : rqname
val name_test_elem_name : rqname

val kind_test_elem_name : rqname
val kind_test_attr_name : rqname

val pi_kind_test_elem_name : rqname
val pi_kind_attr_name : rqname
val axis_attr_name : rqname

(* Typeswitch Element *)
val ts_case_elem_name : rqname
val ts_default_elem_name : rqname
val ts_num_branches_attr_name : rqname
(* Tuple element *)
val tuple_slot_elem_name : rqname

val var_attr_name : rqname
val type_attr_name : rqname

val vname_attr_name : rqname
val stablekind_attr_name : rqname
val sortkind_attr_name : rqname
val emptysortkind_attr_name : rqname
val optdatatype_attr_name : rqname
val pos_attr_name : rqname
val usage_attr_name : rqname
val value_attr_name : rqname
val atomic_type_attr_name : rqname
val target_attr_name : rqname
val content_attr_name : rqname
val attr_name_attr_name : rqname
val elem_name_attr_name : rqname
val fn_name_attr_name : rqname
val arity_attr_name : rqname
val updating_attr_name : rqname
val kname_attr_name : rqname
val host_attr_name : rqname
val port_attr_name : rqname
val tuple_name_attr_name : rqname
val occurrence_attr_name : rqname

val prefix_attr_name : rqname
val uri_attr_name : rqname
val rqname_attr_name : rqname

val relem_elem_name : rqname
val rattr_elem_name : rqname
val rtype_elem_name : rqname

val asequencetype_elem_name : rqname
(* ait element names *)
val ait_attributetype_elem_name : rqname
val ait_elementtype_elem_name : rqname
val ait_schemaattribute_elem_name : rqname
val ait_schemaelementtype_elem_name : rqname
val ait_kindtest_elem_name : rqname
val ait_typeref_elem_name : rqname
val ait_node_elem_name : rqname
val ait_item_elem_name : rqname
val ait_numeric_elem_name : rqname
val ait_anystring_elem_name : rqname
val ait_text_elem_name : rqname
val ait_comment_elem_name : rqname
val ait_processing_instruction_elem_name : rqname
val ait_empty_elem_name : rqname
val ait_anyatomic_elem_name : rqname
val ait_atomic_elem_name : rqname
val ait_document_elem_name : rqname

(* ait attribute names *)
val schema_attr_name_attr_name : rqname
val schema_elem_name_attr_name : rqname
val type_ref_attr_name : rqname
val pi_arg_attr_name : rqname
val atomic_type_attr_name : rqname

val function_decl_elem_name : rqname
val fn_count_attr_name : rqname

val variable_declarations_elem_name : rqname
val variable_count_attr_name        : rqname
val variable_name_attr_name         : rqname

val null_index_attr_name : rqname
val input_dot_attr_name  : rqname
val output_dot_attr_name : rqname

val index_definitions_elem_name : rqname
val index_def_elem_name     	: rqname
val op_1_elem_name    	    	: rqname
val op_2_elem_name    	    	: rqname
val kname_attr_name   	    	: rqname
val indices_count_attr_name 	: rqname

val statement_declaration_elem_name : rqname
val expression_elem_name : rqname
val number_of_statements_attr_name : rqname

val algop_module_decl_elem_name : rqname
val prolog_elem_name: rqname
val prolog_algop_name: Xquery_algebra_ast.algop_decl_name -> rqname


val insert_location_attr_name   : rqname
val value_of_flag_attr_name     : rqname  
val snap_modifier_attr_name     : rqname
val docname_attr_name           : rqname

val project_name_elem_name      : rqname
val project_name_attr_name      : rqname
val project_elem_name           : rqname
val item_tuple_attr_name        : rqname

val n_sort_criteria_attr_name   : rqname
val stable_attr_name            : rqname
val sort_spec_elem_name         : rqname
val empty_sort_kind_attr_name   : rqname
val sort_kind_attr_name         : rqname

(**********************)
(* Compile Annotation *)
(**********************)

val compile_annotation_elem_name  : rqname
val accessed_fields_elem_name     : rqname
val returned_fields_elem_name     : rqname
val free_variables_elem_name      : rqname
    (* Attributes *)
val num_accessed_fields_attr_name : rqname
val num_returned_fields_attr_name : rqname
val num_free_variables_attr_name  : rqname

(***********************)
(* Overloaded Function *)
(***********************)

val overloaded_call_elem_name     : rqname

(***********************)
(* Grouping operations *)
(***********************)
val vname_elem_name          : rqname
val gbn_elem_name            : rqname
val induced_elem_name        : rqname
val mbv_elem_name            : rqname
val agg_elem_name            : rqname
val group_desc_elem_name     : rqname
val distinct_value_attr_name : rqname

(***********************)
(* TupleTreePatterns   *)
(***********************)
val ttp_name            : rqname
val twig_node_name      : rqname
val in_field_attr_name  : rqname
val restore_attr_name   : rqname
val out_field_attr_name : rqname
val child_twig_name     : rqname
val pred_twig_name      : rqname

(**************)
(* Predicates *)
(**************)
val simple_conjunct_elem_name       : rqname
val complex_conjunct_elem_name      : rqname
val disjunct_elem_name              : rqname
val simple_conjunct_start_attr_name : rqname
val simple_conjunct_end_attr_name   : rqname

(**********)
(* Prolog *)
(**********)

type prolog_algop_moniker = 
    AOEVarDeclExternal_n 
  | AOEVarDecl_n
  | AOEValueIndexDecl_n 
  | AOENameIndexDecl_n 

val prolog_attr_name             : rqname
val prolog_var_count_attr_name   : rqname
val prolog_index_count_attr_name : rqname
val prolog_vars_elem_name        : rqname
val prolog_var_decl_external_name : rqname
val prolog_var_decl_name         : rqname
val prolog_value_index_decl_name : rqname
val prolog_name_index_decl_name  : rqname

val prolog_indices_elem_name     : rqname
val get_prolog_algop_moniker     : Namespace_symbols.relem_symbol -> prolog_algop_moniker
val algop_prolog_decl_elem_name  : rqname

(* Closures *)
val closure_attribute_elem_name   : rqname
val closure_bind_elem_name : rqname
val closure_elem_name      : rqname
val closure_env_elem_name  : rqname
val closure_error_elem_name  : rqname
val closure_result_elem_name      : rqname
val closure_table_elem_name      : rqname
val closure_tree_elem_name      : rqname
val closure_tuple_elem_name : rqname
val closure_var_elem_name : rqname

val closure_sym      : relem_symbol
val closure_attribute_sym : relem_symbol
val closure_bind_sym : relem_symbol
val closure_env_sym  : relem_symbol
val closure_error_sym  : relem_symbol
val closure_result_sym : relem_symbol
val closure_table_sym : relem_symbol
val closure_tree_sym : relem_symbol
val closure_tuple_sym : relem_symbol
val closure_var_sym : relem_symbol

val elem_name_of_atomic_type : Datatypes.atomic_type -> rqname
val elem_sym_of_atomic_type : Datatypes.atomic_type -> relem_symbol
val atomic_type_of_elem_sym : relem_symbol -> (rtype_symbol * Datatypes.atomic_type)

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

  | AOEParse_n
  | AOEPromoteAnyString_n

val make_input_attr_name : int -> rqname

val atomic_type_of_attr_string : string -> Datatypes.atomic_type
val attr_string_of_atomic_type : Datatypes.atomic_type -> string

val axis_of_string : string -> Xquery_common_ast.axis
val string_of_axis : Xquery_common_ast.axis -> string  (* NB: _Different_ than Print_common.string_of_axis *)

val string_of_optint : int option -> string
val optint_of_string : string -> int option

val string_of_sortkind : Xquery_common_ast.sortkind -> string
val sortkind_of_string : string -> Xquery_common_ast.sortkind

val string_of_stablekind : Xquery_common_ast.stablekind -> string
val stablekind_of_string : string -> Xquery_common_ast.stablekind

val string_of_emptysortkind : Xquery_common_ast.emptysortkind -> string
val emptysortkind_of_string : string -> Xquery_common_ast.emptysortkind

val string_of_physical_type : Xquery_physical_type_ast.physical_type -> string
val physical_type_of_string : string -> Xquery_physical_type_ast.physical_type

val xml_string_of_prefix : Namespace_names.prefix -> Namespace_names.ncname
val xml_prefix_of_string : Namespace_names.ncname -> Namespace_names.prefix

val get_serializable_moniker_of_algop_name : Xquery_algebra_ast.algop_expr_name -> rqname
val get_moniker_of_algop     : Namespace_symbols.relem_symbol -> algop_moniker

val get_cardinality : string -> int

val serializable_string_of_rqname : rqname -> string
val parse_rqname_string : string -> Namespace_names.rqname
val parse_function_rqname_string : string -> Namespace_names.rqname

