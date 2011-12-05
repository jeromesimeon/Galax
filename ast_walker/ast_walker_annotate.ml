(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: ast_walker_annotate.ml,v 1.31 2007/10/16 01:25:34 mff Exp $ *)

(* Module: Ast_walker_annotate
   Description:
     This module implements a generic tree walker, which recursively
     applies rules to annotate nodes in an AST.  The annotation rules
     are imperative, i.e., the annotations on the AST nodes are
     modified in place.
*)

open Error

open Datatypes

open Xquery_core_ast
open Xquery_core_ast_util

open Ast_walker_annotate_context


(*********************)
(* Annotation walker *)
(*********************)

let rec annotate_cexpr (annotate_ctxt : 'a annotation_context) (ce : acexpr) =
  begin
    match ce.pcexpr_desc with
    | CEUnordered cexpr ->
	annotate_cexpr annotate_ctxt cexpr

    | CEOrdered cexpr ->
	annotate_cexpr annotate_ctxt cexpr

    | CEFLWOR (fl_clauses,where_clause,order_by_clause,return_clause) ->
	begin
	  let apply_one fl_clause =
	    begin
	      match fl_clause with
	      | CELET (cst,vname,cexpr) ->
		  annotate_cexpr annotate_ctxt cexpr
	      | CEFOR (cst,vname1,vname2,cexpr) ->
		  annotate_cexpr annotate_ctxt cexpr 
	    end
	  in List.iter apply_one fl_clauses ;

	  begin
	    match where_clause with
	    | None -> ()
	    | Some cexpr ->
		annotate_cexpr annotate_ctxt cexpr 
	  end;

	  begin
	    match order_by_clause with
	    | None -> ()
	    | Some (stablekind,order_spec_list,osig) ->
		let apply_one_order_spec (cexpr,sortkind,emptysortkind) =
		  annotate_cexpr annotate_ctxt cexpr 
		in List.iter apply_one_order_spec order_spec_list 
	  end;
	  
	  annotate_cexpr annotate_ctxt return_clause 
	end
    | CEIf (cexpr1, cexpr2, cexpr3) ->
	begin
	  annotate_cexpr annotate_ctxt cexpr1 ;
	  annotate_cexpr annotate_ctxt cexpr2 ;
	  annotate_cexpr annotate_ctxt cexpr3 
	end
    | CEWhile (cexpr1, cexpr2) ->
	begin
	  annotate_cexpr annotate_ctxt cexpr1 ;
	  annotate_cexpr annotate_ctxt cexpr2 
	end
    | CETypeswitch (cexpr, cases) ->
	begin
	  annotate_cexpr annotate_ctxt cexpr ;
	  let apply_one_case (pat,ovn,case_cexpr) =
	    annotate_cexpr annotate_ctxt case_cexpr
	  in List.iter apply_one_case cases 
	end
    | CEVar vname -> 
	()
    | CEScalar s ->
	()
    | CEProtoValue s ->
	()
    | CEText t ->
	()
    | CECharRef t ->
	()
    | CETextComputed cexpr1 ->
	annotate_cexpr annotate_ctxt cexpr1
    | CEPI (t,c) ->
	()
    | CEPIComputed (cexpr1, cexpr2) ->
	begin
	  annotate_cexpr annotate_ctxt cexpr1 ;
	  annotate_cexpr annotate_ctxt cexpr2 
	end
    | CEComment c ->
	()
    | CECommentComputed cexpr1 ->
	annotate_cexpr annotate_ctxt cexpr1 
    | CEDocument cexpr1 ->
	annotate_cexpr annotate_ctxt cexpr1 
    | CECall (fname, arguments, sign, _, _) ->
	List.iter (annotate_cexpr annotate_ctxt) arguments
    | CEOverloadedCall (fname, arguments, sigs) ->
	List.iter (annotate_cexpr annotate_ctxt) arguments
    | CESeq (cexpr1, cexpr2) 
    | CEImperativeSeq (cexpr1, cexpr2) ->
	begin
	  annotate_cexpr annotate_ctxt cexpr1 ;
	  annotate_cexpr annotate_ctxt cexpr2 
	end
    | CEEmpty ->
	()
    | CEElem (relem_symbol, nsenv, cexprlist) ->
	List.iter (annotate_cexpr annotate_ctxt) cexprlist
    | CEAnyElem (cexpr1, nsenv1, nsenv2, cexpr2) ->  
	begin
	  annotate_cexpr annotate_ctxt cexpr1 ;
	  annotate_cexpr annotate_ctxt cexpr2 
	end
    | CEAttr (rattr_symbol, nsenv, cexprlist)->
	List.iter (annotate_cexpr annotate_ctxt) cexprlist 
    | CEAnyAttr (cexpr1, nsenv, cexpr2) ->
	begin
	  annotate_cexpr annotate_ctxt cexpr1 ;
	  annotate_cexpr annotate_ctxt cexpr2 
	end
    | CEError cexprlist ->
	List.iter (annotate_cexpr annotate_ctxt) cexprlist
    | CETreat (cexpr, model) ->
	annotate_cexpr annotate_ctxt cexpr 
    | CECast (cexpr, _, model) ->
	annotate_cexpr annotate_ctxt cexpr 
    | CECastable (cexpr, _, model) ->
	annotate_cexpr annotate_ctxt cexpr 
    | CEValidate (vmode,cexpr) ->
	annotate_cexpr annotate_ctxt cexpr 
    | CEForwardAxis (v, axis, cnode_test) ->
	()
    | CEReverseAxis (v, axis, cnode_test) ->
	()
    | CESome (odt, vname, cexpr1, cexpr2) ->
	begin
	  annotate_cexpr annotate_ctxt cexpr1 ;
	  annotate_cexpr annotate_ctxt cexpr2 
	end
    | CEEvery (odt, vname, cexpr1, cexpr2) ->
	begin
	  annotate_cexpr annotate_ctxt cexpr1 ;
	  annotate_cexpr annotate_ctxt cexpr2 
	end
    | CECopy cexpr1 ->
	annotate_cexpr annotate_ctxt cexpr1
    | CEDelete cexpr1 ->
	annotate_cexpr annotate_ctxt cexpr1
    | CEInsert (cexpr1, cinsert_location) ->
	begin
	  annotate_cexpr annotate_ctxt cexpr1;
	  match cinsert_location with
	  | CUAsLastInto cexpr2
	  | CUAsFirstInto cexpr2
	  | CUInto cexpr2
	  | CUAfter cexpr2
	  | CUBefore cexpr2 ->
	      annotate_cexpr annotate_ctxt cexpr2
	end
    | CEReplace (vof, cexpr1, cexpr2) ->
	begin
	  annotate_cexpr annotate_ctxt cexpr1;
	  annotate_cexpr annotate_ctxt cexpr2
	end
    | CERename (nsenv, cexpr1, cexpr2) ->
	annotate_cexpr annotate_ctxt cexpr1;
	annotate_cexpr annotate_ctxt cexpr2

    | CESnap (sm,cexpr) ->
	annotate_cexpr annotate_ctxt cexpr

    | CELetServerImplement (nc1, nc2, ce1, ce2) ->
	annotate_cexpr annotate_ctxt ce1;
	annotate_cexpr annotate_ctxt ce2

    | CEExecute (_, ncname, uri, hostport, cexpr) ->
	annotate_cexpr annotate_ctxt hostport;
	annotate_cexpr annotate_ctxt cexpr

    | CEForServerClose (nc1, uri, ce1) ->
	annotate_cexpr annotate_ctxt ce1

    | CEEvalClosure (ce1) ->
	annotate_cexpr annotate_ctxt ce1

    | CELetvar (_, _, cexpr1, cexpr2) ->
	    begin
	      annotate_cexpr annotate_ctxt cexpr1 ;
	      annotate_cexpr annotate_ctxt cexpr2
	    end
    | CESet (_,cexpr) ->
        annotate_cexpr annotate_ctxt cexpr
  end;
  let ctxt = get_annotation_context annotate_ctxt in
  let update_fn = get_annotation_update_fn annotate_ctxt in
  update_fn ctxt ce 

(***********************)
(* Statement annotater *)
(***********************)

let annotate_cstatement annotate_ctxt cs =
  annotate_cexpr annotate_ctxt cs


(*************************)
(* Query prolog rewriter *)
(*************************)

(* Applying a rewriting pass to function declarations *)

let cfunction_body_annotate annotate_ctxt cfunction_body =
  match cfunction_body with
  | CEFunctionInterface
  | CEFunctionImported
  | CEFunctionBltIn -> ()
  | CEFunctionUser ce -> annotate_cexpr annotate_ctxt ce

let function_annotate annotate_ctxt cfunction_def =
  match cfunction_def.pcfunction_def_desc with
  | (rfname, vars, cfunction_signature, cfunction_body, _) ->
	cfunction_body_annotate annotate_ctxt cfunction_body

let var_annotate annotate_ctxt cvar_decl =
  match cvar_decl.pcvar_decl_desc with
  | (vname, model, CEVarUser ce) ->
      annotate_cexpr annotate_ctxt ce
  | (vname, model, _) ->
      ()

let index_annotate annotate_ctxt cindex_def =
  match cindex_def.pcindex_def_desc with
  | CValueIndex (name, ce1, ce2) ->
      begin
	annotate_cexpr annotate_ctxt ce1;
	annotate_cexpr annotate_ctxt ce2
      end
  | CNameIndex name -> ()

(* Main optimizer call for the query prolog *)

let annotate_cprolog annotate_ctxt cprolog =
  List.iter (function_annotate annotate_ctxt) cprolog.pcprolog_functions;
  List.iter(var_annotate annotate_ctxt) cprolog.pcprolog_vars;
  List.iter (index_annotate annotate_ctxt) cprolog.pcprolog_indices

let annotate_cxmodule annotate_ctxt cxmodule =
  annotate_cprolog annotate_ctxt cxmodule.pcmodule_prolog;
  List.iter (annotate_cstatement annotate_ctxt) cxmodule.pcmodule_statements


