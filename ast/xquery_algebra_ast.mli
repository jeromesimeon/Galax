(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xquery_algebra_ast.mli,v 1.99 2007/10/16 01:25:34 mff Exp $ *)

(* Module Xquery_algebra_ast
   Description:
     This *interface* contains type declarations for the ALGEBRA
     abstract syntax tree.
*)

open Array

open Namespace_symbols

open Xquery_common_ast
open Xquery_physical_type_ast

(**************)
(* Node kinds *)
(**************)

type aelement_test =
  | ASchemaElementTest of relem_symbol
  | AElementTest of (relem_symbol * rtype_symbol option) option

type aattribute_test =
  | ASchemaAttributeTest of rattr_symbol
  | AAttributeTest of (rattr_symbol * rtype_symbol option) option

type akind_test =
  | ADocumentKind of aelement_test option    (* ./document-node() *)
  | AElementKind of aelement_test            (* ./element() *)
  | AAttributeKind of aattribute_test        (* ./attribute() *)
  | APIKind of string option  	 	     (* ./pi("xxx") *)
  | ACommentKind              	 	     (* ./comment () *)
  | ATextKind                 	 	     (* ./text() *)
  | AAnyKind                  	 	     (* ./node() *)

(******************)
(* Sequence types *)
(******************)

type aitemtype = 
  | AITKindTest of akind_test
  | AITTypeRef of rtype_symbol                                (* type(QName) *)
  | AITItem
  | AITNumeric
  | AITAnyString
  | AITAtomic of rtype_symbol
  | AITEmpty

(* Functional/imperative variable *)
type var_kind = 
    | Immutable
    | Mutable
        
type asequencetype_desc = aitemtype * (Occurrence.occurrence_indicator) option

type asequencetype =
    { pasequencetype_desc : asequencetype_desc;
      pasequencetype_loc  : Finfo.finfo }

type anode_test =
  | APNameTest of anon_symbol
  | APNodeKindTest of akind_test

type apattern =
    { papattern_desc : apattern_desc;
      papattern_loc : Finfo.finfo }

and apattern_desc =
  | ACase of asequencetype
  | ADefault

type predicate_desc =
  | SimpleConjunct  of int * int (* indexes into dependent sub-expression *)
  | ComplexConjunct of predicate_desc * predicate_desc
  | Disjunct        of predicate_desc * predicate_desc

type afunction_signature = asequencetype list * asequencetype

type aoverloaded_signature_table = (cfname * afunction_signature * updating_modifier) list


(***************)
(* The Algebra *)
(***************)
(* Some flags for updates *)

type algop_insert_location =
  | AOUAsLastInto
  | AOUAsFirstInto
  | AOUInto
  | AOUAfter
  | AOUBefore

type seq_group_name = cvname

type group_desc =
    { group_by_names : seq_group_name list;
      (* This can be calculated but, instead
	 we keep it with the group operator *)
      induced_groups : seq_group_name list;
      must_be_valid  : seq_group_name list;
      aggregate_name : (asequencetype option * cvname) }

type twig_node =
    { node_test : anode_test option;
      mutable out       : crname option;
      mutable restore_out : bool;
      (* the int points to the location in the 
       * twig_pattern array of the child node.
       *)
      mutable child_twig : (axis * int) option;
      mutable pred_twigs : (axis * int) list; 
      mutable requires_sbdo : (bool * bool);
     }

type twig_pattern = twig_node array

(* Alebraic operations names *)
type algop_expr_name =
  (* XQuery algebra operations *)
  | AOEIf
  | AOEWhile
  | AOELetvar of (asequencetype option * cvname)
  | AOETypeswitch of (apattern * cvname option) array
  | AOEVar of cvname
  | AOEScalar of literal
  | AOESeq
  | AOEEmpty
  | AOEDocument
  | AOEPI of Namespace_names.ncname * string
  | AOEPIComputed 
  | AOEComment of string
  | AOECommentComputed
  | AOEText of string
  | AOECharRef of int
  | AOETextComputed
  | AOEElem of relem_symbol * Namespace_context.nsenv
  | AOEAnyElem of Namespace_context.nsenv * Namespace_context.nsenv (* static/in-scope namespaces *)
  | AOEAttr of rattr_symbol * Namespace_context.nsenv
  | AOEAnyAttr of Namespace_context.nsenv
  | AOEError (* Missing from the paper *)
  | AOETreat of asequencetype
  | AOEValidate of validation_mode
  | AOECast of Namespace_context.nsenv * asequencetype
  | AOECastable of Namespace_context.nsenv * asequencetype
  | AOESome of asequencetype option * cvname
  | AOEEvery of asequencetype option * cvname

  (* Input tuple *)
  | AOEInputTuple

  (* Function Calls *)
  (* AOEExecute algebraic-plan *)
  | AOECallBuiltIn of (cfname * int) * asequencetype option array * asequencetype * updating_modifier
  | AOECallOverloaded of (cfname * int) * aoverloaded_signature_table
  | AOECallUserDefined of 
      (cfname * int) * asequencetype option array * asequencetype 
	* updating_modifier * (* self-recursive *) bool 
  | AOEConvertSimple of Datatypes.atomic_type
  | AOEPromoteNumeric of Datatypes.atomic_type
  | AOEPromoteAnyString
  | AOEUnsafePromoteNumeric of Datatypes.atomic_type

  (* Galax extensions *)
  | AOEServerImplements of Namespace_names.ncname * string
  | AOEExecute of Namespace_names.ncname * string
  | AOEASyncExecute of Namespace_names.ncname * string
  | AOEForServerClose of Namespace_names.ncname * string
  | AOEEvalClosure 

  (**************************)
  (* Basic Tuple operations *)
  (**************************)
  | AOECreateTuple of (asequencetype option * crname) array (* Tuple creation *)
  | AOEAccessTuple of crname             		    (* Tuple field access *)
  | AOEConcatTuples                       		    (* Tuple concatenation *)
  | AOEProject of crname array (* added by Philippe *)

  (**************)
  (* Tuple Maps *)
  (**************)
  | AOEMapFromItem of crname              (* Item to tuple iteration *)
  | AOEMapToItem                          (* Tuple to item iteration *)
  | AOEMap                                (* Tuple iteration *)
  | AOENullMap of crname                  (* Tuple iteration with null flag binding *)
  | AOEMapIndex of crname                 (* Tuple iteration with index binding *)
  | AOEMapIndexStep of crname             (* Tuple iteration with index binding *)

  (* Map Concats *)      
  | AOEMapConcat                          (* Map with built-in tuple concatenation *)
  | AOEOuterMapConcat of crname           (* Same with null value on the given name *)

  (* Blocking maps *)
  (*     | AOEMaterializeTable                   (\* Explicit materialization *\)  *)
  (* Nicola: this is no longer needed. Materialization should be transparent for the logical level. *)

  (******************************)
  (* Products, Joins Selections *)
  (******************************)
  | AOEProduct                            (* Cartesian product *)
  | AOESelect of predicate_desc           (* Selection *)
  | AOEJoin of predicate_desc             (* Join *)
  | AOELeftOuterJoin of                   (* Left-outer join *)
      (crname * predicate_desc)

  (************************************)
  (* Grouping and Ordering operations *)
  (************************************)
  | AOEGroupBy of group_desc list         (* Group by *)
  | AOEOrderBy of stablekind *
	(sortkind * emptysortkind) list *
	aoverloaded_signature_table (* Order by *)

  (* Update-related operations *)
  | AOECopy
  (* Update operations *)
  | AOEDelete
  | AOEInsert of algop_insert_location
  | AOERename of Namespace_context.nsenv
  | AOEReplace of value_of_flag
  | AOESnap of snap_modifier

  | AOESet of cvname

  | AOEImperativeSeq

  (********************)
  (* XPath evaluation *)
  (********************)
  | AOEParse of string
  | AOETreeJoin of axis * anode_test
  (* TupletreePatterns allow us to evaluate 
     path expressions using twig joins *)
  | AOETupleTreePattern of crname * twig_pattern

type input_signature =
  | NoInput
  | OneInput of physical_type
  | TwoInput of (physical_type * physical_type)
  | ManyInput of physical_type array
      (* We do not have any heterogenous input models - a little
         akward..*)

(* This is for the datamodel signature of the indep. expressions *)   
(* Currently not implemented *)
type tuple_fields       = cvname list
type variable_usage     = Never | Once | Many | Redefined
(* Nicola: `Redefined' means that the variable was reassigned a value *)

type variable_use_count = (cvname * (int * variable_usage))
type tuple_field_use_count = variable_use_count

(* Helps making tuple field use count analysis more accurate. - Michael *)
type table_cardinality = COne | CMany
type cardinality = NoTable | Table of table_cardinality

type treejoin_path = Empty | Variable of cvname | Step of axis * anode_test | 
    JoinChild of treejoin_path * treejoin_path | Union of treejoin_path * treejoin_path

type free_variable_desc = {
    (* Mutable so we can carry them around *)
    mutable use_counts                 : variable_use_count list;
    mutable bound_usage_counts         : variable_use_count list;
    mutable accessed_fields            : tuple_fields;
    mutable returned_fields            : tuple_fields;
    
    (* accessed fields with use counts, candidate fields (iteration), table cardinality *)
    mutable tuple_field_use_counts     : (tuple_field_use_count list * tuple_fields * cardinality);
    
    (* path analysis *)
    mutable returned_path              : treejoin_path;
    mutable accessed_path              : treejoin_path;
    mutable updated_path               : treejoin_path;
  }
    
(* An operator's physical signature includes its physical operator,
   the physical types of its independent inputs and the physical type
   of its output *)
type expr_eval_sig = Xquery_physical_algebra_ast.physop_expr_name * input_signature * physical_type

type ('a,'b) aalgop_sub_exprs =
  | NoSub
  | OneSub of ('a,'b) aalgop_expr
  | TwoSub of ('a,'b) aalgop_expr * ('a,'b) aalgop_expr
  | ManySub of ('a,'b) aalgop_expr array

and ('a,'b) aalgop_expr = { 
    mutable palgop_expr_name     : algop_expr_name;          (* The algebraic operation *)
    mutable psub_expression      : ('a,'b) aalgop_sub_exprs; (* Independent sub-expressions *)
    mutable pdep_sub_expression  : ('a,'b) aalgop_sub_exprs; (* Dependent sub-expressions *)
    palgop_expr_eval             : 'a ref ;                  (* The evaluation code and tail-recursive code *) 
    mutable palgop_expr_eval_sig : expr_eval_sig option;     (* Physical signature *)
    annotation                   : 'b;                       (* Annotations for rewrites *)
    mutable compile_annotations  : free_variable_desc option;(* Currently, present tuples and free vars *)
    palgop_expr_origin           : Xquery_ast.expr_handle;   (* Handle to the original expr *)
    palgop_expr_loc              : Finfo.finfo }             (* File location *)


(**********)
(* Prolog *)
(**********)

(* XQuery prolog operations *)

type algop_decl_name =
  | AOEVarDecl of (asequencetype option * cvname)
  | AOEVarDeclExternal of (asequencetype option * cvname)
  | AOEVarDeclImported of (asequencetype option * cvname)
  | AOEValueIndexDecl of string
  | AOENameIndexDecl of relem_symbol

type ('a,'b,'c) aalgop_decl =
    { alg_decl_name          : algop_decl_name; 	 (* Variable/index kind *)
      alg_decl_eval          : 'c ref ;           	 (* The evaluation code *)
      mutable alg_decl_indep : ('a,'b) aalgop_sub_exprs; (* Indep. sub-expr *)
      mutable alg_decl_dep   : ('a,'b) aalgop_sub_exprs; (* Dep. sub-expr *)
      alg_decl_annotation    : 'b;                       (* Annotations *)
      alg_decl_loc           : Finfo.finfo }             (* File location *)

(* Function declarations *)

type ('a,'b) aalgop_function_plan = 
  | AOEFunctionImported
  | AOEFunctionUser of ('a,'b) aalgop_expr 

type ('a,'b) aalgop_function_body =
    (* cvname array * ('a,'b) aalgop_expr ref * asequencetype option *)
    (* Formal arguments, optimized logical plan, physical plan, output type *)
    { palgop_func_formal_args : cvname array;
      palgop_func_output_type : asequencetype option;
      palgop_func_optimized_logical_plan: ('a,'b) aalgop_function_plan ref;
      (* A physical plan is always a complete expression *)
      palgop_func_physical_plan: ('a,'b) aalgop_expr option ref;
    }

type ('a,'b) aalgop_function_decl =
    { palgop_function_decl_desc : ('a,'b) aalgop_function_decl_desc;
      palgop_function_decl_loc  : Finfo.finfo }

and ('a,'b) aalgop_function_decl_desc =
    (cfname * int) * afunction_signature * ('a,'b) aalgop_function_body * updating_modifier

(* Core Query module *)

type ('a,'b,'c) aalgop_prolog =
    { palgop_prolog_functions : ('a,'b) aalgop_function_decl list;
      palgop_prolog_vars      : ('a,'b,'c) aalgop_decl list;
      palgop_prolog_indices   : ('a,'b,'c) aalgop_decl list }

type ('a,'b,'c) aalgop_xmodule =
    { palgop_module_prolog     : ('a,'b,'c) aalgop_prolog;
      palgop_module_statements : ('a,'b) aalgop_expr list }

