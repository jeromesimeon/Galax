(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xquery_algebra_ast_util.mli,v 1.47 2008/02/01 02:45:26 mff Exp $ *)

(* Module: Xquery_algebra_ast_util
   Description:
     This module implements some useful operations the XQuery Algebra
     AST.
*)


open Xquery_common_ast
open Xquery_algebra_ast

open Ast_logical_algebra_types


(*************************)
(* Algebra AST utilities *)
(*************************)

val fmkapattern : apattern_desc -> Finfo.finfo -> apattern

val fmkasequencetype : asequencetype_desc -> Finfo.finfo -> asequencetype

val algop_mkop :
    'a -> expr_eval_sig option  -> 
      algop_expr_name ->  
	('a,'b) aalgop_sub_exprs -> ('a,'b) aalgop_sub_exprs -> 
	  'b -> Xquery_algebra_ast.free_variable_desc option ->
            Xquery_ast.expr_handle -> Finfo.finfo 
	      -> ('a,'b) aalgop_expr

val algop_decl_mkop : 
  'c -> algop_decl_name ->
  ('a,'b) aalgop_sub_exprs -> ('a,'b) aalgop_sub_exprs -> 
  'b -> Finfo.finfo  -> ('a,'b,'c) aalgop_decl

val fmkalgop_function_body :
    cvname array -> ('a,'b) aalgop_function_plan -> ('a,'b) aalgop_expr option -> asequencetype option -> ('a,'b) aalgop_function_body

val fmkalgop_function_decl :
  ('a,'b) aalgop_function_decl_desc -> Finfo.finfo -> ('a,'b) aalgop_function_decl

val fmkalgop_prolog :
  ('a,'b) aalgop_function_decl list -> ('a,'b,'c) aalgop_decl list -> 
  ('a,'b,'c) aalgop_decl list -> ('a,'b,'c) aalgop_prolog

val fmkalgop_xmodule :
  ('a,'b,'c) aalgop_prolog -> ('a,'b) aalgop_expr list  ->
    ('a,'b,'c) aalgop_xmodule


(* A more specific function adding some "dummy" code *)

val aalgop_mkop : 
    algop_expr_name ->
      'a semilogical_opt_algop_sub_exprs -> (* Indep subexprs *)
	'a semilogical_opt_algop_sub_exprs-> (* Dep subexprs *) 
         Xquery_algebra_ast.free_variable_desc option ->
	  Xquery_ast.expr_handle -> Finfo.finfo -> 'a semilogical_opt_algop_expr


val logical_aalgop_mkop : 
    algop_expr_name ->
      logical_algop_sub_exprs ->
	logical_algop_sub_exprs ->
         Xquery_algebra_ast.free_variable_desc option ->
	  Xquery_ast.expr_handle -> Finfo.finfo -> logical_algop_expr


val replace_aalgop_name :
  'a semilogical_opt_algop_expr -> algop_expr_name  -> 'a semilogical_opt_algop_expr

val aalgop_decl_mkop : 
    algop_decl_name ->
      'a semilogical_algop_sub_exprs ->
	'a semilogical_algop_sub_exprs ->
	  Finfo.finfo -> ('a, 'b) semilogical_algop_decl

val logical_aalgop_decl_mkop : 
    algop_decl_name ->
      logical_algop_sub_exprs ->
	logical_algop_sub_exprs ->
	  Finfo.finfo -> logical_algop_decl


val copy_algop : ('a, 'b) aalgop_expr -> ('a, 'b) aalgop_expr

val mk_annotation :
    variable_use_count list ->  
      variable_use_count list ->
	tuple_fields ->
	  tuple_fields ->
	    (tuple_field_use_count list * tuple_fields * cardinality)->
              free_variable_desc

val access_nosub   : ('a, 'b) aalgop_sub_exprs -> unit
val access_unitsub : ('a, 'b) aalgop_sub_exprs -> unit
val access_onesub  : ('a, 'b) aalgop_sub_exprs -> ('a,'b) aalgop_expr
val access_twosub  : ('a, 'b) aalgop_sub_exprs -> (('a,'b) aalgop_expr * ('a,'b) aalgop_expr)
val access_manysub : ('a, 'b) aalgop_sub_exprs -> ('a,'b) aalgop_expr array

(*************************)
(* Operations on modules *)
(*************************)

val empty_statement : logical_algop_expr
val empty_prolog_plan : ('a,'b,'c) aalgop_prolog
val empty_xmodule : ('a,'b,'c) aalgop_xmodule

val merge_alg_prologs :
    ('a,'b,'c) aalgop_prolog -> ('a,'b,'c) aalgop_prolog -> ('a,'b,'c) aalgop_prolog

val subexpr_to_list : ('a, 'b) aalgop_sub_exprs -> ('a,'b) aalgop_expr list

val get_group_names      : group_desc -> seq_group_name list
val get_induced_group    : group_desc -> cvname list
val get_valid_names      : group_desc -> seq_group_name list
val get_aggregate_name   : group_desc -> cvname 
val get_aggregate_type   : group_desc -> asequencetype option
val get_aggregate_name_and_type : group_desc -> asequencetype option * cvname 

val mk_group_desc :
    seq_group_name list ->
      seq_group_name list -> 
	cvname list ->
	  asequencetype option * cvname -> group_desc

val split_main_module_plan :
    ('a,'b,'c) aalgop_xmodule ->
      ('a,'b,'c) aalgop_prolog * ('a,'b) aalgop_expr list

(* Twig tools *)
val get_all_outputs_from_twig_pattern : twig_pattern -> crname list
val get_restored_outputs_from_twig_pattern : twig_pattern -> crname list
val get_node_tests_from_twig_pattern : twig_pattern -> anode_test list

val get_leaf_twig_node            : twig_pattern -> int -> twig_node
val get_first_leaf_node_index     : twig_pattern -> int -> int
val get_select_path               : twig_pattern -> int array

val is_leaf_node                  : twig_pattern -> twig_node -> bool
val is_root_node                  : twig_pattern -> twig_node -> bool
val get_parent_node_index         : twig_pattern -> twig_node -> int
val get_child_node_indices        : twig_pattern -> twig_node -> int list
val get_sub_node_indices          : twig_pattern -> twig_node -> int list


val append_twigs                  : twig_pattern -> twig_pattern -> crname -> twig_pattern
val merge_twigs                   : twig_pattern -> (crname * twig_pattern) list -> twig_pattern
val replace_field_in_pattern      : twig_pattern -> crname -> crname -> bool
val is_streamable_treepattern     : twig_pattern -> bool
val is_single_step_twig           : twig_pattern -> bool

val find_attach_point : twig_pattern -> crname -> int

val get_axis_array :
    twig_pattern ->  Xquery_common_ast.axis  array

val pname_of_algop : ('a,'b) aalgop_expr -> Xquery_physical_algebra_ast.physop_expr_name
val string_of_algop_expr_name : algop_expr_name -> string

(* for the path analysis *)
(* val disjoint_paths : treejoin_path -> treejoin_path -> bool *)

val get_annotation_if_exists : string -> ('a,'b) aalgop_expr -> free_variable_desc

val aelement_test_equal : aelement_test -> aelement_test -> bool
val aattribute_test_equal : aattribute_test -> aattribute_test -> bool
val akind_test_equal : akind_test -> akind_test -> bool
val anode_test_equal : anode_test -> anode_test -> bool

(* Coomonly used stuff, self-explanatory *)
val is_access_tuple       : ('a, 'b) aalgop_expr -> bool
val is_any_join           : ('a, 'b) aalgop_expr -> bool
val is_outer_join         : ('a, 'b) aalgop_expr -> bool
val is_regular_join       : ('a, 'b) aalgop_expr -> bool

val is_a_tuple_input_map  : ('a, 'b) aalgop_expr -> bool
val is_a_sep_sequence     : ('a, 'b) aalgop_expr -> bool
val is_a_map_index        : ('a, 'b) aalgop_expr -> bool
val is_a_dep_map          : ('a, 'b) aalgop_expr -> bool
val is_a_map              : ('a, 'b) aalgop_expr -> bool
val is_an_outer_mapconcat       : ('a, 'b) aalgop_expr -> bool
val is_a_non_concat_map   : ('a, 'b) aalgop_expr -> bool
val is_a_map_concat       : ('a, 'b) aalgop_expr -> bool
val is_any_map_concat     : ('a, 'b) aalgop_expr -> bool
val is_a_map_from_item    : ('a, 'b) aalgop_expr -> bool
val is_a_map_to_item      : ('a, 'b) aalgop_expr -> bool
val is_select             : ('a, 'b) aalgop_expr -> bool
val is_project            : ('a, 'b) aalgop_expr -> bool
val is_product            : ('a, 'b) aalgop_expr -> bool
val is_empty_tuple        : ('a, 'b) aalgop_expr -> bool
val is_some               : ('a, 'b) aalgop_expr -> bool
val is_null_map           : ('a, 'b) aalgop_expr -> bool
(* Function matches *)
val is_boolean            : ('a, 'b) aalgop_expr -> bool
val is_distinct_value     : ('a, 'b) aalgop_expr -> bool
val is_fn_data            : ('a, 'b) aalgop_expr -> bool
val is_equal              : ('a, 'b) aalgop_expr -> bool
val is_gt                 : ('a, 'b) aalgop_expr -> bool
val is_ge                 : ('a, 'b) aalgop_expr -> bool
val is_lt                 : ('a, 'b) aalgop_expr -> bool
val is_le                 : ('a, 'b) aalgop_expr -> bool
val is_fs_untyped_to_any  : ('a, 'b) aalgop_expr -> bool
val is_input_tuple        : ('a, 'b) aalgop_expr -> bool
val is_group              : ('a, 'b) aalgop_expr -> bool
val is_tupletreepattern   : ('a, 'b) aalgop_expr -> bool

val get_map_from_item_name     : ('a, 'b) aalgop_expr -> crname
val get_outer_map_name         : ('a, 'b) aalgop_expr -> crname
val get_sep_sequence_name      : ('a, 'b) aalgop_expr -> crname
val get_outer_join_name        : ('a, 'b) aalgop_expr -> crname
val get_null_map_name          : ('a, 'b) aalgop_expr -> crname
val get_function_name          : ('a, 'b) aalgop_expr -> crname option

(* 
   Returns all functions called within a plan.  
   Should these be static annotations on the plan?
*)
val calls_builtin_functions    : ('a, 'b) aalgop_expr -> crname list
val calls_user_defined_functions : ('a, 'b) aalgop_expr -> crname list
