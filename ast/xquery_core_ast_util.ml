(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xquery_core_ast_util.ml,v 1.34 2007/10/16 21:22:29 mff Exp $ *)

(* Module: Xquery_core_ast_util
   Description:
     This module implements some useful operations the XQuery core
     AST.
*)


open Finfo

open Namespace_names
open Namespace_builtin

open Xquery_common_ast
open Xquery_type_core_ast
open Xquery_core_ast
open Xquery_core_ast_annotation

open Error


(*********************)
(* Core AST creation *)
(*********************)

(* Core typeswitch patterns *)

let mkcpattern d =
  { pcpattern_desc = d;
    pcpattern_loc = parsing_locinfo () }

let fmkcpattern d l =
  { pcpattern_desc = d;
    pcpattern_loc = l }

(* Core sequence types *)

let mkcsequencetype d =
  { pcsequencetype_desc = d;
    pcsequencetype_loc = parsing_locinfo () }

let fmkcsequencetype d l =
  { pcsequencetype_desc = d;
    pcsequencetype_loc = l }

(* Core expressions *)

let fmkcexpr d e l =
  { pcexpr_desc = d;
    pcexpr_annot = empty_ast_annot ();
    pcexpr_origin = e;
    pcexpr_loc = l }

let fmkacexpr d a e l =
  { pcexpr_desc = d;
    pcexpr_annot = a;
    pcexpr_origin = e;
    pcexpr_loc = l }

let fmkcfunction_def d l=
  { pcfunction_def_desc = d;
    pcfunction_def_loc = l }

let fmkcvar_decl d l =
    { pcvar_decl_desc = d;
      pcvar_decl_loc = l }

let fmkcserver_decl d l =
    { pcserver_decl_desc = d;
      pcserver_decl_loc = l }

let fmkcindex_def d l =
    { pcindex_def_desc = d;
      pcindex_def_loc = l }

(* Core prolog AST creation *)

let fmkcprolog funs vars indices =
    { pcprolog_functions = funs;
      pcprolog_vars      = vars;
      pcprolog_servers   = [];
      pcprolog_indices   = indices }

(* Core module AST creation *)

let fmkcmodule_from_library_module cqm cstatements =
  let pro = cqm.pcmodule_prolog in
  { pcmodule_prolog =
    fmkcprolog pro.pcprolog_functions pro.pcprolog_vars pro.pcprolog_indices;
    pcmodule_statements =
    cstatements }


(************************************)
(* Returns the core expression kind *)
(************************************)

(* Note: This is used for monitoring
   - Jerome *)


type cexpr_kind =
  | CEKUnordered
  | CEKOrdered
  | CEKFLWOR
  | CEKOrderBy
  | CEKIf
  | CEKWhile
  | CEKTypeswitch
  | CEKVar
  | CEKScalar
  | CEKProtoValue
  | CEKDocument
  | CEKPI
  | CEKPIComputed
  | CEKComment
  | CEKCommentComputed
  | CEKText
  | CEKCharRef
  | CEKTextComputed
  | CEKCall
  | CEKOverloadedCall
  | CEKSeq
  | CEKEmpty
  | CEKElem
  | CEKAnyElem
  | CEKAttr
  | CEKAnyAttr
  | CEKError
  | CEKTreat
  | CEKValidate
  | CEKCast
  | CEKCastable
  | CEKAxis
  | CEKSome
  | CEKEvery
  | CEKLetServerImplement
  | CEKExecute
  | CEKForServerClose
  | CEKEvalClosure
  | CEKCopy
  | CEKDelete
  | CEKDetach
  | CEKInsert
  | CEKRename
  | CEKReplace
  | CEKSnap
  | CEKLetvar
  | CEKSet
  | CEKImperativeSeq

let get_cexpr_kind cexpr =
  match cexpr.pcexpr_desc with
  | CEUnordered _ ->
      CEKUnordered
  | CEOrdered _ ->
      CEKOrdered
  | CEFLWOR _ ->
      CEKFLWOR
  | CEIf _ ->
      CEKIf
  | CEWhile _ ->
      CEKWhile
  | CETypeswitch _ ->
      CEKTypeswitch
  | CEVar _ ->
      CEKVar
  | CEScalar _ ->
      CEKScalar
  | CEProtoValue _ ->
      CEKProtoValue
  | CEDocument _ ->
      CEKDocument
  | CEPI _ ->
      CEKPI
  | CEPIComputed _ ->
      CEKPIComputed
  | CEComment _ ->
      CEKComment
  | CECommentComputed _ ->
      CEKCommentComputed
  | CEText _ ->
      CEKText
  | CECharRef _ ->
      CEKCharRef
  | CETextComputed _ ->
      CEKTextComputed
  | CECall _ ->
      CEKCall
  | CEOverloadedCall _ ->
      CEKOverloadedCall
  | CESeq _ ->
      CEKSeq
  | CEImperativeSeq _ ->
      CEKImperativeSeq
  | CEEmpty ->
      CEKEmpty
  | CEElem _ ->
      CEKElem
  | CEAnyElem _ ->
      CEKAnyElem
  | CEAttr _ ->
      CEKAttr
  | CEAnyAttr _ ->
      CEKAnyAttr
  | CEError _ ->
      CEKError
  | CETreat _ ->
      CEKTreat
  | CEValidate _ ->
      CEKValidate
  | CECast _ ->
      CEKCast
  | CECastable _ ->
      CEKCastable
  | CEForwardAxis _ ->
      CEKAxis
  | CEReverseAxis _ ->
      CEKAxis
  | CESome _ ->
      CEKSome
  | CEEvery _ ->
      CEKEvery
  | CELetServerImplement _ ->
      CEKLetServerImplement
  | CEExecute _ ->
      CEKExecute
  | CEForServerClose _ ->
      CEKForServerClose 
  | CEEvalClosure _ ->
      CEKEvalClosure
  | CECopy _ ->
      CEKCopy
  | CEDelete _ ->
      CEKDelete
  | CEInsert _ ->
      CEKInsert
  | CERename _ ->
      CEKRename
  | CEReplace _ ->
      CEKReplace
  | CESnap _ ->
      CEKSnap
  | CELetvar _ ->
      CEKLetvar
  | CESet _ ->
      CEKSet


(*****************************)
(* Module related operations *)
(*****************************)

let merge_cprolog cp1 cp2 =
  { pcprolog_functions = cp1.pcprolog_functions @ cp2.pcprolog_functions;
    pcprolog_vars      = cp1.pcprolog_vars @ cp2.pcprolog_vars;
    pcprolog_servers   = cp1.pcprolog_servers @ cp2.pcprolog_servers;
    pcprolog_indices   = cp1.pcprolog_indices @ cp2.pcprolog_indices }

let merge_cmodules cq1 cq2 =
  { pcmodule_prolog     = merge_cprolog cq1.pcmodule_prolog cq2.pcmodule_prolog;
    pcmodule_statements = cq1.pcmodule_statements @ cq2.pcmodule_statements }


(*****************************************)
(* Some macros used during normalization *)
(*****************************************)

(* Builds a variable expression from its name *)

let mkcvar vname eh fi =
  fmkcexpr (CEVar vname) eh fi

(* Generate new variables, in the fs: namespace *)

let cexpr_fs_dot eh fi      = mkcvar fs_dot eh fi
let cexpr_fs_sequence eh fi = mkcvar fs_sequence eh fi
let cexpr_fs_position eh fi = mkcvar fs_position eh fi
let cexpr_fs_last eh fi     = mkcvar fs_last eh fi

(* Core expression for () *)

let cexpr_empty eh fi = fmkcexpr CEEmpty eh fi


(* helper functions for mapping functions *)
(* Adding 'parenthesises right associatively: l = [e1; e2; e3 ...; en]
   -> (e1 (e2 (e3 (... en)...) *)

let rec map_to_sequence eh fi exprlist =
  let cempty = fmkcexpr CEEmpty None bogus in
  begin
    match exprlist with
    | [] -> cempty
    | [ expr ] -> expr
    | expr::exprlist' ->
	fmkcexpr (CESeq (expr, (map_to_sequence eh fi exprlist'))) eh fi
  end

(* the same thing, for `;' sequences (the content of XQueryP blocks) *)
let rec map_to_imperative_sequence eh fi exprlist =
  let cempty = fmkcexpr CEEmpty None bogus in
  begin
    match exprlist with
    | [] -> cempty
    | [ expr ] -> expr
    | expr::exprlist' ->
	fmkcexpr (CEImperativeSeq (expr, (map_to_imperative_sequence eh fi exprlist'))) eh fi
  end


(* Extract all variable bindings from a FLWR expressions *)

(* Note:
     This is used to build the tuple in a FLWR with order by clause.
   - Jerome *)

let rec all_cflwr_bindings cfl_expr_list =
  match cfl_expr_list with
  | [] -> []
  | cfl_expr ::  cfl_expr_list' ->
      begin
	match cfl_expr with
	| CELET (_, v1, _) ->
	    let prev_bindings = all_cflwr_bindings cfl_expr_list' in
	    if (List.mem v1 prev_bindings)
	    then prev_bindings
	    else v1 :: prev_bindings
	| CEFOR (_, v1, None, _) ->
	    let prev_bindings = all_cflwr_bindings cfl_expr_list' in
	    if (List.mem v1 prev_bindings)
	    then prev_bindings
	    else v1 :: prev_bindings
	| CEFOR (_, v1, (Some v2), _) ->
	    let prev_bindings = all_cflwr_bindings cfl_expr_list' in
	    let prev_bindings' =
	      if (List.mem v1 prev_bindings)
	      then prev_bindings
	      else v1 :: prev_bindings
	    in
	    if (List.mem v2 prev_bindings')
	    then prev_bindings'
	    else v2 :: prev_bindings'
      end

(* Creates a FLWOR expression only with let bindings.  This is used at
   different places in the code, notably when doing rewriting and
   factorization. If the list of input let bindings is empty, returns
   the return expression 'as is' - Jerome *)
let make_let_flwor let_bindings return_cexpr at eh fi =
  match let_bindings with
  | [] -> return_cexpr
  | _ -> fmkacexpr (CEFLWOR (let_bindings,None,None,return_cexpr)) at eh fi

let make_for_flwor for_bindings return_cexpr at eh fi =
  match for_bindings with
  | [] -> return_cexpr
  | _ -> fmkacexpr (CEFLWOR (for_bindings,None,None,return_cexpr)) at eh fi


(* Remove empty text nodes at head and tail of expression list *)

let remove_empty_textnodes celist = 
  let celist' = 
    match celist with
    | [] -> []
    | cl -> 
	let first = List.nth cl 0 in
	begin
	  match first.pcexpr_desc with 
	  | CEText s -> if (s = "") then (List.tl cl) else cl
	  | _ -> cl
	end
  in
  let revcelist' = List.rev celist' in
  let revcelist'' = 
    match revcelist' with 
    | [] -> []
    | cl -> 
	let first = List.nth cl 0 in
	begin
	  match first.pcexpr_desc with 
	  | CEText s -> if (s = "") then (List.tl cl) else cl
	  | _ -> cl
	end
  in List.rev revcelist''


(* Some operations on core FLWOR expressions *)

let get_first_fl_clause cexpr =
  match cexpr.pcexpr_desc with
  | CEFLWOR ([],_,_,_) ->
      (None,cexpr)
  | CEFLWOR (fl_clause::fl_clauses',where_clause,order_by_clause,return_clause) ->
      let ma = cexpr.pcexpr_annot in
      let eh = cexpr.pcexpr_origin in
      let loc = cexpr.pcexpr_loc in
      (Some fl_clause,
       fmkacexpr (CEFLWOR (fl_clauses',where_clause,order_by_clause,return_clause)) ma eh loc)
  | _ ->
      raise (Query (Malformed_Expr "Cannot extract an fl clause: not a FLWOR expression"))

let add_first_fl_clause fl_clause cexpr =
  match cexpr.pcexpr_desc with
  | CEFLWOR (fl_clauses',where_clause,order_by_clause,return_clause) ->
      let ma = cexpr.pcexpr_annot in
      let eh = cexpr.pcexpr_origin in
      let loc = cexpr.pcexpr_loc in
      fmkacexpr (CEFLWOR (fl_clause::fl_clauses',where_clause,order_by_clause,return_clause)) ma eh loc
  | _ ->
      raise (Query (Malformed_Expr "Cannot add an fl clause: not a FLWOR expression"))

let add_last_fl_clauses fl_clauses cexpr =
  match cexpr.pcexpr_desc with
  | CEFLWOR (fl_clauses',where_clause,order_by_clause,return_clause) ->
      let ma = cexpr.pcexpr_annot in
      let eh = cexpr.pcexpr_origin in
      let loc = cexpr.pcexpr_loc in
      fmkacexpr (CEFLWOR (fl_clauses'@fl_clauses,where_clause,order_by_clause,return_clause)) ma eh loc
  | _ ->
      raise (Query (Malformed_Expr "Cannot add fl clauses: not a FLWOR expression"))


let build_flwor_from_fl_clauses fl_clauses ce =
  let ma = ce.pcexpr_annot in
  let eh = ce.pcexpr_origin in
  let loc = ce.pcexpr_loc in
  fmkacexpr (CEFLWOR (fl_clauses,None,None,ce)) ma eh loc


(*************************)
(* Access to annotations *)
(*************************)

let set_annotation_for_cexpr cexpr annot =
  let existing_annot = cexpr.pcexpr_annot in
  set_annotation existing_annot annot

let get_type_annotation_from_cexpr cexpr =
  get_type_annot cexpr.pcexpr_annot

let set_type_annotation_for_cexpr cexpr cxtype =
  set_type_annot cexpr.pcexpr_annot cxtype

let get_expr_from_insert_location loc =
  match loc with
  | CUAsLastInto x 
  | CUAsFirstInto x
  | CUInto x
  | CUAfter x
  | CUBefore x -> x

let has_max_one acexpr =
  let type_annot = get_type_annotation_from_cexpr acexpr in
  match type_annot.pcxtype_desc with
  | CAtomicRef _ | CElementRef _ | CAttributeRef _ | CDocument _ 
  | CText | CPI _ | CComment | CEmpty -> true
  | CBound (t, o1, o2) ->
      begin
	match o1, o2 with
	| Occurrence.UP_INT i1, Occurrence.UP_INT i2 
	  when 0 <= i1 && i1 <= 1 && 0 <= i2 && i2 <= 1 -> true
	| _ -> false
      end
  | _ -> false

