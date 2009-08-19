(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: print_xquery.ml,v 1.53 2007/11/16 21:16:52 mff Exp $ *)

(* Module Print_xquery
   Description:
     This module implements pretty-printing for the XQuery AST.
*)

open Error

open Gmisc

open Occurrence
open Namespace_names
open Datatypes

open Xquery_common_ast
open Print_common

open Xquery_type_ast
open Print_type

open Xquery_ast

open Format


(*************)
(* Node kind *)
(*************)

let print_element_test ff et =
  match et with
  | SchemaElementTest ename ->
      fprintf ff "schema-element(%a)" print_uqname ename
  | ElementTest None ->
      fprintf ff "element()"
  | ElementTest (Some (ename, None)) ->
      fprintf ff "element(%a)" print_uqname ename
  | ElementTest (Some (ename, Some tname)) ->
      fprintf ff "element(%a,%a)" print_uqname ename print_uqname tname

let print_attribute_test ff at =
  match at with
  | SchemaAttributeTest aname ->
      fprintf ff "schema-attribute(%a)" print_uqname aname
  | AttributeTest None ->
      fprintf ff "attribute()"
  | AttributeTest (Some (aname, None)) ->
      fprintf ff "attribute(%a)" print_uqname aname
  | AttributeTest (Some (aname, Some tname)) ->
      fprintf ff "attribute(%a,%a)" print_uqname aname print_uqname tname

let print_kind_test ff kt =
  match kt with
  | DocumentKind None ->
      fprintf ff "document-node()"
  | DocumentKind (Some et) ->
      fprintf ff "document-node(%a)" print_element_test et
  | ElementKind et ->
      fprintf ff "%a" print_element_test et
  | AttributeKind at ->
      fprintf ff "%a" print_attribute_test at
  | PIKind None ->
      fprintf ff "processing-instruction()"
  | PIKind (Some s) ->
      fprintf ff "processing-instruction(\"%s\")" s
  | CommentKind ->
      fprintf ff "comment()"
  | TextKind ->
      fprintf ff "text()"
  | AnyKind ->
      fprintf ff "node()"


(******************)
(* Sequence types *)
(******************)

let print_itemtype ff dtk =
  match dtk with
  | ITKindTest kt ->
      fprintf ff "%a" print_kind_test kt
  | ITTypeRef tname ->
      fprintf ff "type(%a)" print_uqname tname
  | ITItem ->
      fprintf ff "item()"
  | ITNumeric ->
      fprintf ff "numeric()"
  | ITAnyString ->
      fprintf ff "anystring()"
  | ITEmpty ->
      fprintf ff "empty()"
  | ITAtomic bt ->
      fprintf ff "%a" print_uqname bt

let print_sequencetype ff dt =
  match dt.psequencetype_desc with
  | (dtk,occ) ->
      fprintf ff "%a%a" print_itemtype dtk print_occurence occ

let print_optsequencetype ff odt =
  match odt with
  | None -> ()
  | Some dt ->
      fprintf ff " as %a" print_sequencetype dt


(*****************************)
(* XPath 2.0 data structures *)
(*****************************)

(* Node tests *)

let print_node_test ff nt =
  match nt with
  | PNameTest qn ->
      fprintf ff "%a" print_uqname qn
  | PNodeKindTest nk ->
      fprintf ff "%a" print_kind_test nk


(**********************)
(* XQuery expressions *)
(**********************)

(* Note:
     Here is, for each XQuery expression, the code used for precedence
     in the pretty-printer. (Updated Jaunary 2005 - Jerome)

  17 - EPath        | with path_expr = PAxis,
       EVar, EScalar, EApp,
       ERoot, ESelf, EParent,
       EValidate, EElemFixed, EElemComputed,
       EAttrFixed, EAttrComputed, EDocument, EText, ETextComputed,
       EPI, EComment,
       EUnordered, EOrdered
  16 - EPath        | with path_expr = PStepQualifiers
  15 - EPath        | with path_expr = PSlash, PSlashSlash
  14 - EUnaryOp
  13 - ECast
  12 - ECastable
  11 - ETreat
  10 - EInstanceOf
  9  - EBinaryOp    | with binop = BEIntersect, BEExcept
  8  - EBinaryOp    | with binop = BEUnion, BEBar
  7  - EBinaryOp    | with binop = BEMult, BEDiv, BEIDiv, BEMod
  6  - EBinaryOp    | with binop = BEPlus, BEMinus
  5  - ERange
  4  - EBinaryOp    | with binop = BEPrecedes, BEFollows, BEEq, BENEq,
                                   BELtOp, BELte, BEGtOp, BEGte,
                                   BEEqual, BENEqual, BEIs, BEIsNot,
                                   BELt, BEGt, BELteq, BEGteq
  3  - EBinaryOp    | with binop = BEAnd
  2  - EBinaryOp    | with binop = BEOr
  1  - EFLWOR, ETypeswitch, EIf, ESome, EEvery
  0  - EList

*)

let rec print_expr_top ff e =
  print_expr_prec 0 ff e

and print_expr_prec p ff e =
  match e.pexpr_desc with
  | EBinaryOp (e1,op,e2) ->
      begin
	match op with
	| BEOr ->
	    if p > 2
	    then
	      fprintf ff "@[<hv 1>(@[<hov 1>%a@;<1 0>@[<hov 1>%a@;<1 0>%a@]@])@]" (print_expr_prec 2) e1 print_bop op (print_expr_prec 2) e2
	    else
	      fprintf ff "@[<hov 1>%a@;<1 0>@[<hov 1>%a@;<1 0>%a@]@]" (print_expr_prec 2) e1 print_bop op (print_expr_prec 2) e2

	| BEAnd ->
	    if p > 3
	    then
	      fprintf ff "@[<hv 1>(@[<hov 1>%a@;<1 0>@[<hov 1>%a@;<1 0>%a@]@])@]" (print_expr_prec 3) e1 print_bop op (print_expr_prec 3) e2
	    else
	      fprintf ff "@[<hov 1>%a@;<1 0>@[<hov 1>%a@;<1 0>%a@]@]" (print_expr_prec 3) e1 print_bop op (print_expr_prec 3) e2
	| BEPrecedes
	| BEFollows
	| BEEq
	| BENEq
	| BELtOp
	| BELte
	| BEGtOp
	| BEGte
	| BEEqual
	| BENEqual
	| BEIs
	| BELt
	| BEGt
	| BELteq
	| BEGteq ->
	    if p > 4
	    then
	      fprintf ff "@[<hv 1>(@[<hov 1>%a@;<1 0>@[<hov 1>%a@;<1 0>%a@]@])@]" (print_expr_prec 4) e1 print_bop op (print_expr_prec 4) e2
	    else
	      fprintf ff "@[<hov 1>%a@;<1 0>@[<hov 1>%a@;<1 0>%a@]@]" (print_expr_prec 4) e1 print_bop op (print_expr_prec 4) e2
	| BEPlus
	| BEMinus ->
	    if p > 6
	    then
	      fprintf ff "@[<hv 1>(@[<hov 1>%a@;<1 0>@[<hov 1>%a@;<1 0>%a@]@])@]" (print_expr_prec 6) e1 print_bop op (print_expr_prec 6) e2
	    else
	      fprintf ff "@[<hov 1>%a@;<1 0>@[<hov 1>%a@;<1 0>%a@]@]" (print_expr_prec 6) e1 print_bop op (print_expr_prec 6) e2
	| BEMult
	| BEDiv
	| BEIDiv
	| BEMod ->
	    if p > 7
	    then
	      fprintf ff "@[<hv 1>(@[<hov 1>%a@;<1 0>@[<hov 1>%a@;<1 0>%a@]@])@]" (print_expr_prec 7) e1 print_bop op (print_expr_prec 7) e2
	    else
	      fprintf ff "@[<hov 1>%a@;<1 0>@[<hov 1>%a@;<1 0>%a@]@]" (print_expr_prec 7) e1 print_bop op (print_expr_prec 7) e2
	| BEUnion
	| BEBar ->
	    if p > 8
	    then
	      fprintf ff "@[<hv 1>(@[<hov 1>%a@;<1 0>@[<hov 1>%a@;<1 0>%a@]@])@]" (print_expr_prec 8) e1 print_bop op (print_expr_prec 8) e2
	    else
	      fprintf ff "@[<hov 1>%a@;<1 0>@[<hov 1>%a@;<1 0>%a@]@]" (print_expr_prec 8) e1 print_bop op (print_expr_prec 8) e2
	| BEIntersect
	| BEExcept ->
	    if p > 9
	    then
	      fprintf ff "@[<hv 1>(@[<hov 1>%a@;<1 0>@[<hov 1>%a@;<1 0>%a@]@])@]" (print_expr_prec 9) e1 print_bop op (print_expr_prec 9) e2
	    else
	      fprintf ff "@[<hov 1>%a@;<1 0>@[<hov 1>%a@;<1 0>%a@]@]" (print_expr_prec 9) e1 print_bop op (print_expr_prec 9) e2
      end
  | EFLWOR (fls, w, o, r) ->
      if p > 1
      then
	fprintf ff "@[<hv 1>(%a%a%a%a)@]"
	  print_flclauses fls
	  print_where w
	  print_order o
	  print_return r
      else
	fprintf ff "@[<hv 0>%a%a%a%a@]" print_flclauses fls print_where w print_order o print_return r
  | EIf (e1,e2,e3) ->
      if p > 1
      then
	fprintf ff "@[<hv 1>(if (%a)@;<1 0>@[<hv 2>then@;<1 0>%a@]@;<1 0>@[<hv 2>else@;<1 0>%a@])@]" (print_expr_prec 0) e1 (print_expr_prec 1) e2 (print_expr_prec 1) e3
      else
	fprintf ff "@[<hv 0>if (%a)@;<1 0>@[<hv 2>then@;<1 0>%a@]@;<1 0>@[<hv 2>else@;<1 0>%a@]@]" (print_expr_prec 0) e1 (print_expr_prec 1) e2 (print_expr_prec 1) e3
  | ESome (bdl,e2) ->
      if p > 1
      then
	fprintf ff "@[<hv 1>(@[<hv 0>@[<hv 5>some %a@]@ @[<hv 2>satisfies@ %a@]@])@]" print_bindings_list bdl (print_expr_prec 1) e2
      else
	fprintf ff "@[<hv 0>@[<hv 5>some %a@]@ @[<hv 2>satisfies@ %a@]@]" print_bindings_list bdl (print_expr_prec 1) e2
  | EEvery (bdl,e2) ->
      if p > 1
      then
	fprintf ff "@[<hv 1>(@[<hv 0>@[<hv 5>every %a@]@ @[<hv 2>satisfies@ %a@]@])@]" print_bindings_list bdl (print_expr_prec 1) e2
      else
	fprintf ff "@[<hv 0>@[<hv 5>every %a@]@ @[<hv 2>satisfies@ %a@]@]" print_bindings_list bdl (print_expr_prec 1) e2
  | ETypeswitch (e', cs) ->
      if p > 1
      then
	fprintf ff "@[<hv 1>(@[<hv 2>@[<hv 2>typeswitch (@,%a@;<0 -2>)@]@;<1 0>%a@])@]" (print_expr_prec 0) e' print_cases cs
      else
	fprintf ff "@[<hv 2>@[<hv 2>typeswitch (@,%a@;<0 -2>)@]@;<1 0>%a@]" (print_expr_prec 0) e' print_cases cs
  | EInstanceOf (e',m) ->
      if p > 10
      then
	fprintf ff "@[<hv 1>(%a@ @[<hv 2>instance of@ %a@])@]" (print_expr_prec 10) e' print_sequencetype m
      else
	fprintf ff "@[%a@ @[<hv 2>instance of@ %a@]@]" (print_expr_prec 10) e' print_sequencetype m
  | ERange (e1,e2) ->
      if p > 5
      then
	fprintf ff "@[<hv 1>(%a to %a)@]" (print_expr_prec 5) e1 (print_expr_prec 5) e2
      else
	fprintf ff "%a to %a" (print_expr_prec 5) e1 (print_expr_prec 5) e2
  | EUnaryOp (uop,e') ->
      if p > 14
      then
	fprintf ff "@[<hv 1>(%a%a)@]" print_uop uop (print_expr_prec 14) e'
      else
	fprintf ff "%a%a" print_uop uop (print_expr_prec 14) e'
  | ERoot ->
      fprintf ff "/"
  | EPath pe ->
      fprintf ff "%a" (print_path_expr p) pe
  | ESelf ->
      fprintf ff "."
  | EParent ->
      fprintf ff ".."
  | EVar vn ->
      fprintf ff "$%a" print_uqname vn
  | EScalar l ->
      fprintf ff "%a" print_literal l
  | EApp (fn,es) ->
      fprintf ff "@[<hv 2>%a(@,%a@;<0 -2>)@]" print_uqname fn print_function_arguments es
  | EList es ->
      fprintf ff "%a" (print_expr_sequence p) es
  | ECast (e',m) ->
      if p > 13
      then
	fprintf ff "@[<hv 1>(@[<hv 2>%a@ cast as %a@])@]" (print_expr_prec 13) e' print_sequencetype m
      else
	fprintf ff "@[<hv 2>%a@ cast as %a@]" (print_expr_prec 13) e' print_sequencetype m
  | ECastable (e',m) ->
      if p > 12
      then
	fprintf ff "@[<hv 1>(%a@ castable as %a)@]" (print_expr_prec 12) e' print_sequencetype m
      else
	fprintf ff "%a@ castable as %a" (print_expr_prec 12) e' print_sequencetype m
  | ETreat (e',m) ->
      if p > 11
      then
	fprintf ff "@[<hv 1>(@[<hv 2>%a treat as %a@])@]" (print_expr_prec 11) e' print_sequencetype m
      else
	fprintf ff "@[<hv 2>%a treat as %a@]" (print_expr_prec 11) e' print_sequencetype m
  | EValidate (Some vmode,e') ->
      fprintf ff "@[<hv 2>validate %a@ {@,%a@;<0 -2>}@]" print_validation_mode vmode (print_expr_prec 0) e'
  | EValidate (None,e') ->
      fprintf ff "@[<hv 2>validate {@,%a@;<0 -2>}@]" (print_expr_prec 0) e'
  | EEnclosed e ->
      fprintf ff "%a" (print_expr_prec 0) e
  | EElemFixed (se, es1, es2) ->
      begin
	match es2 with
	| [] ->
	    fprintf ff "<%a%a/>" print_uqname se print_attrs es1
	| _ ->
	    fprintf ff "@[<hv 2><%a%a>@,%a@;<0 -2></%a>@]" print_uqname se print_attrs es1 print_elem_contents es2 print_uqname se
      end
  | EElemComputed (es0, es1) ->
      begin
	match es1 with
	| [] ->
	    fprintf ff "@[<hv 2>element {@,%a@;<0 -2>} {}@]" (print_expr_sequence 17 ) es0
	| _ ->
	    fprintf ff "@[<hv 2>element {@,%a@;<0 -2>} {@,%a@;<0 -2>}@]" (print_expr_sequence 17) es0 print_elem_contents es1
      end
  | EAttrFixed (sa,es) ->
      fprintf ff "@[<hv 2>attribute %a {@,%a@;<0 -2>}@]" print_uqname sa print_enclosed_expr es
  | EAttrComputed (es0,es1) ->
      fprintf ff "@[<hv 2>attribute {@,%a@;<0 -2>} {@,%a@;<0 -2>}@]" (print_expr_sequence 17) es0 print_enclosed_expr es1
  | EText t ->
      fprintf ff "@[<hv 2>text {@,\"%s\"@;<0 -2>}@]" t
  | ECharRef i ->
      fprintf ff "@[<hv 2>text {@,\"&#%i;\"@;<0 -2>}@]" i
  | ETextComputed es ->
      fprintf ff "@[<hv 2>text {@,%a@;<0 -2>}@]" print_enclosed_expr es
  | EDocument es ->
      fprintf ff "@[<hv 2>document {@,%a@;<0 -2>}@]" print_enclosed_expr es
  | EPI (target,pi_content) ->
      fprintf ff "<?%s %s?>" target pi_content
  | EPIComputed (e1,es) ->
      fprintf ff "@[<hv 2>processing-instruction {@,%a@;<0 -2>} {@,%a@;<0 -2>}@]" (print_expr_sequence p) e1 print_enclosed_expr es
  | EComment c ->
      fprintf ff "<!--%s-->" c
  | ECommentComputed es ->
      fprintf ff "@[<hv 2>comment {@,%a@;<0 -2>}@]" print_enclosed_expr es
  | EPragma (name,content,es) ->
      fprintf ff "@[<hv 2>(#%a %s#) {@,%a@;<0 -2>}@]" print_uqname name content print_enclosed_expr es
  | EUnordered es ->
      fprintf ff "@[<hv 2>unordered {@,%a@;<0 -2>}@]" print_enclosed_expr es
  | EOrdered es ->
      fprintf ff "@[<hv 2>ordered {@,%a@;<0 -2>}@]" print_enclosed_expr es

  | ELetServerImplement (nc1, nc2, e1, e2) ->
       fprintf ff "@[<hv 2>let@ server@ %s@ implement %s at %a%a@]" nc1 nc2 (print_expr_prec 0) e1 print_return e2
  | EExecute (async,ncname,e2) ->
      if(async) then
      fprintf ff "@[<hv 2>at@ server@ %s@;<0 -2> do@;{%a}@]" ncname (print_expr_prec 0) e2 
      else
	fprintf ff "@[<hv 2>from@ server@ %s@;<0 -2> return@;{%a}@]" ncname (print_expr_prec 0) e2
  | EForServerClose (nc1, nc2, e1) ->
       fprintf ff "@[<hv 2>for@ server@ %s@ implement @ %s@ box@ %a@]" nc1 nc2 (print_expr_prec 0) e1 
  | EEvalClosure (e1) ->
       fprintf ff "@[<hv 2>eval@ box@ %a@]" (print_expr_prec 0) e1 

  | ECopy e1 ->
      fprintf ff "@[<hv 2>copy@ {@,%a@;<0 -2>}@]" (print_expr_prec 0) e1

  | ESnap (Snap_Nondeterministic, e1) ->
      fprintf ff "@[<hv 2>snap nondeterministic@ {@,%a@;<0 -2>}@]" (print_expr_prec 0) e1
  | ESnap (Snap_Unordered_Deterministic, e1) ->
      fprintf ff "@[<hv 2>snap @ {@,%a@;<0 -2>}@]" (print_expr_prec 0) e1
  | ESnap (Snap_Ordered_Deterministic, e1) ->
      fprintf ff "@[<hv 2>snapo @ {@,%a@;<0 -2>}@]" (print_expr_prec 0) e1

  | EDelete (snap,e1) ->
      fprintf ff "@[<hv 2>%adelete nodes@ %a@]" print_snap snap (print_expr_prec 0) e1
  | EInsert (snap,e1,il) ->
      fprintf ff "@[@[<hv 2>%ainsert nodes@ %a@]@ %a@]" print_snap snap (print_expr_prec 0) e1 print_insert_location il
  | ERename (snap,e1,e2) ->
      fprintf ff "@[@[<hv 2>%arename nodes@ %a@]@ @[<hv 2>to {@,%a@;<0 -2>}@]@]" print_snap snap (print_expr_prec 0) e1 (print_expr_prec 0) e2
  | EReplace (snap,Normal_Replace,e1,e2) ->
      fprintf ff "@[@[<hv 2>%areplace node@ %a@]@ @[<hv 2>with@ %a@]@]" print_snap snap (print_expr_prec 0) e1 (print_expr_prec 0) e2
  | EReplace (snap,Value_Of_Replace,e1,e2) ->
      fprintf ff "@[@[<hv 2>%areplace value of node@ %a@]@ @[<hv 2>with@ %a@]@]" print_snap snap (print_expr_prec 0) e1 (print_expr_prec 0) e2

  | ERevalidate (snap, Some vmode,e') ->
      fprintf ff "@[<hv 2>%arevalidate %a@ {@,%a@;<0 -2>}@]" print_snap snap print_validation_mode vmode (print_expr_prec 0) e'
  | ERevalidate (snap, None,e') ->
      fprintf ff "@[<hv 2>%arevalidate {@,%a@;<0 -2>}@]" print_snap snap (print_expr_prec 0) e'
  | ETransform (cps, m, r) ->
      if p > 1
      then
	fprintf ff "@[<hv 1>(%a%a%a)@]"
	  print_cpclauses cps
	  print_modify m
	  print_return r
      else
	fprintf ff "@[<hv 0>%a%a%a@]" print_cpclauses cps print_modify m print_return r
  | EWhile (test,ret) ->
      fprintf ff "@[<hv 2>while (%a) %a@]" (print_expr_prec 0) test print_return ret
  | ELetvar (odt,v,x,r) ->
      fprintf ff "@[<hv 2>letvar $%a%a := @ %a %a@]" print_uqname v print_optsequencetype odt (print_expr_prec 1) x print_return r
  | ESet (v,x) ->
      fprintf ff "@[<hv 2>set $%a := @ %a@]" print_uqname v (print_expr_prec 1) x
  | EBlock (decls, elist) ->  	
      fprintf ff "@[<hv 1> { @[<hv 1>@,%a@,%a@]@,}@]" 
        print_block_decls decls 
        print_block_sequence elist  
        
and print_block_decls ff bldecs = 
  match bldecs with
    | [] -> ()
    | d::rest ->
        begin
          match d.bl_decl_desc with (odt,v,x) ->
            fprintf ff "@[<hv 2>declare $%a%a := @ %a;@]@,%a" print_uqname v print_optsequencetype odt (print_expr_prec 1) x print_block_decls  rest 
        end

and print_order_by_clause ff (stablekind,ssl) =
  fprintf ff "@[<hv 2>%aorder by@ %a@]" print_stablekind stablekind print_sortspeclist ssl

and print_bindings_list ff bdl =
  match bdl with
  | [] -> ()
  | (odt,vn,ce1) :: obdl ->
      fprintf ff "@[<hv -3>$%a%a in@ %a@]%a" print_uqname vn print_optsequencetype odt print_expr_top ce1 print_other_bindings_list obdl

and print_other_bindings_list ff bdl =
  match bdl with
  | [] -> ()
  | (odt,vn,ce1) :: obdl ->
      fprintf ff ",@ @[<hv -3>$%a%a in@ %a@]%a" print_uqname vn print_optsequencetype odt print_expr_top ce1 print_other_bindings_list obdl

and print_sortspeclist ff ssl =
  match ssl with
  | [x] ->
      fprintf ff "@[%a@]" print_sortspec x
  | x :: ssl' ->
      fprintf ff "@[%a@],@ @[%a@]" print_sortspec x print_sortspeclist ssl'
  | [] ->
      raise (Query (Malformed_Expr "Empty sort kind"))

and print_sortspec ff sk =
  match sk with
  | (e,sk,None) ->
      fprintf ff "@[%a@] @[%a@]" (print_expr_prec 0) e print_sortkind sk
  | (e,sk,Some esk) ->
      fprintf ff "@[%a@] @[%a@] @[%a@]" (print_expr_prec 0) e print_sortkind sk print_emptysortkind esk

and print_flclauses ff fls =
  match fls with
  | [x] ->
      begin
	match x.pfl_desc with
	| ELet (odt,vn,e') ->
	    fprintf ff "@[<hv 2>let $%a%a :=@ %a@]" print_uqname vn print_optsequencetype odt (print_expr_prec 1) e'
	| EFor (odt,vn,ovn,e') ->
	    begin
	      match ovn with
	      | None ->
		  fprintf ff "@[<hv 2>for $%a%a in@ %a@]"  print_uqname vn print_optsequencetype odt (print_expr_prec 1) e'
	      | Some vn' ->
		  fprintf ff "@[<hv 2>for $%a%a at $%a in@ %a@]" print_uqname vn print_optsequencetype odt print_uqname vn' (print_expr_prec 1) e'
	    end
      end
  | x :: fls' ->
      begin
	match x.pfl_desc with
	| ELet (odt,vn,e') ->
	    fprintf ff "@[<hv 2>let $%a%a :=@ %a@]%a" print_uqname vn print_optsequencetype odt (print_expr_prec 1) e' print_let_clauses fls'
	| EFor (odt,vn,ovn,e') ->
	    begin
	      match ovn with
	      | None ->
		  fprintf ff "@[<hv 2>for $%a%a in@ %a@]%a" print_uqname vn print_optsequencetype odt (print_expr_prec 1) e' print_for_clauses fls'
	      | Some vn' ->
		  fprintf ff "@[<hv 2>for $%a%a at $%a in@ %a@]%a" print_uqname vn print_optsequencetype odt print_uqname vn' (print_expr_prec 1) e' print_for_clauses fls'
	    end
      end
  | [] ->
      raise (Query (Malformed_Expr "Empty flower"))


and print_for_clauses ff fls =
  match fls with
  | [x] ->
      begin
	match x.pfl_desc with
	| ELet (odt,vn,e') ->
	    fprintf ff "@;<1 0>@[<hv 2>let $%a%a :=@ %a@]" print_uqname vn print_optsequencetype odt (print_expr_prec 1) e'
	| EFor (odt,vn,ovn,e') ->
	    begin
	      match ovn with
	      | None ->
		  fprintf ff ",@;<1 4>@[<hv -2>$%a%a in@ %a@]" print_uqname vn print_optsequencetype odt (print_expr_prec 1) e'
	      | Some vn' ->
		  fprintf ff ",@;<1 4>@[<hv -2>$%a%a at $%a in@ %a@]" print_uqname vn print_optsequencetype odt print_uqname vn' (print_expr_prec 1) e'
	    end
      end
  | x :: fls' ->
      begin
	match x.pfl_desc with
	| ELet (odt,vn,e') ->
	    fprintf ff "@;<1 0>@[<hv 2>let $%a%a :=@ %a@]%a" print_uqname vn print_optsequencetype odt (print_expr_prec 1) e' print_let_clauses fls'
	| EFor (odt,vn,ovn,e') ->
	    begin
	      match ovn with
	      | None ->
		  fprintf ff ",@;<1 4>@[<hv -2>$%a%a in@ %a@]%a" print_uqname vn print_optsequencetype odt (print_expr_prec 1) e' print_for_clauses fls'
	      | Some vn' ->
		  fprintf ff ",@;<1 4>@[<hv -2>$%a%a at $%a in@ %a@]%a" print_uqname vn print_optsequencetype odt print_uqname vn' (print_expr_prec 1) e' print_for_clauses fls'
	    end
      end
  | [] ->
      raise (Query (Malformed_Expr "Empty nested for expression"))

and print_let_clauses ff fls =
  match fls with
  | [x] ->
      begin
	match x.pfl_desc with
	| ELet (odt,vn,e') ->
	    fprintf ff ",@;<1 4>@[<hv -2>$%a%a :=@ %a@]" print_uqname vn print_optsequencetype odt (print_expr_prec 1) e'
	| EFor (odt,vn,ovn,e') ->
	    begin
	      match ovn with
	      | None ->
		  fprintf ff "@;<1 0>@[<hv 2>for $%a%a in@ %a@]" print_uqname vn print_optsequencetype odt (print_expr_prec 1) e'
	      | Some vn' ->
		  fprintf ff "@;<1 0>@[<hv 2>for $%a%a at $%a in@ %a@]" print_uqname vn print_optsequencetype odt print_uqname vn' (print_expr_prec 1) e'
	    end
      end
  | x :: fls' ->
      begin
	match x.pfl_desc with
	| ELet (odt,vn,e') ->
	    fprintf ff ",@;<1 4>@[<hv -2>$%a%a :=@ %a@]%a"  print_uqname vn print_optsequencetype odt (print_expr_prec 1) e' print_let_clauses fls'
	| EFor (odt,vn,ovn,e') ->
	    begin
	      match ovn with
	      | None ->
		  fprintf ff "@;<1 0>@[<hv 2>for $%a%a in@ %a@]%a" print_uqname vn print_optsequencetype odt (print_expr_prec 1) e' print_for_clauses fls'
	      | Some vn' ->
		  fprintf ff "@;<1 0>@[<hv 2>for $%a%a at $%a in@ %a@]%a" print_uqname vn print_optsequencetype odt print_uqname vn' (print_expr_prec 1) e' print_for_clauses fls'
	    end
      end
  | [] ->
      raise (Query (Malformed_Expr "Empty nested for expression"))

and print_where ff w =
  match w with
  | None ->
      ()
  | Some e ->
      fprintf ff "@;<1 0>@[<hv 2>where@;<1 0>%a@]" (print_expr_prec 1) e

and print_order ff o =
  match o with
  | None ->
      ()
  | Some oc ->
      fprintf ff "@;<1 0>%a" print_order_by_clause oc

and print_return ff r =
  fprintf ff "@;<1 0>@[<hv 2>return@;<1 0>%a@]" (print_expr_prec 1) r

and print_modify ff r =
  fprintf ff "@;<1 0>@[<hv 2>modify@;<1 0>%a@]" (print_expr_prec 1) r

and print_cases ff cs =
  match cs with
  | [] ->
      ()
  | (p,ovn,e) :: cs' ->
      begin
	match p.ppattern_desc with
	| Case m ->
	    fprintf ff "case %a%a return@;<1 2>%a@;%a" print_optvariable_as ovn print_sequencetype m (print_expr_prec 1) e print_cases cs'
	| Default ->
	    fprintf ff "default %areturn@;<1 2>%a@;%a" print_optvariable ovn (print_expr_prec 1) e print_cases cs'
      end

and print_optvariable ff ovn =
  match ovn with
  | None -> ()
  | Some vn -> fprintf ff "$%a " print_uqname vn

and print_optvariable_as ff ovn =
  match ovn with
  | None -> ()
  | Some vn -> fprintf ff "$%a as " print_uqname vn

and print_path_expr p ff pe =
  match pe with
  | PSlash (e1,e2) ->
      if p > 15 then
	fprintf ff "@[<hv 1>(@[<hv 1>%a/@,%a@])@]" (print_expr_prec 15) e1 (print_expr_prec 15) e2
      else
	fprintf ff "@[<hv 1>%a/@,%a@]" (print_expr_prec 15) e1 (print_expr_prec 15) e2
  | PSlashSlash (e1,e2) ->
      if p > 15 then
	fprintf ff "@[<hv 1>(@[<hv 1>%a//@,%a@])@]" (print_expr_prec 15) e1 (print_expr_prec 15) e2
      else
	fprintf ff "@[<hv 1>%a//@,%a@]" (print_expr_prec 15) e1 (print_expr_prec 15) e2
  | PAxis (Child, nt) ->
      fprintf ff "%a" print_node_test nt
  | PAxis (Attribute, nt) ->
      fprintf ff "@@%a" print_node_test nt
  | PAxis (a,nt) ->
      fprintf ff "%a::%a" print_axis a print_node_test nt
  | PStepQualifiers (e',sqs) ->
      if p > 16 then
	fprintf ff "@[<hv 1>(@[<hv 1>%a@,%a@])@]" (print_expr_prec 16) e' print_step_qualifiers sqs
      else
	fprintf ff "@[<hv 1>%a@,%a@]" (print_expr_prec 16) e' print_step_qualifiers sqs

and print_step_qualifiers ff sqs =
  match sqs with
  | [] ->
      ()
  | sq :: sqs' ->
      fprintf ff "%a@;<0 1>%a" print_step_qualifier sq print_step_qualifiers sqs'

and print_step_qualifier ff sq =
  match sq.pstep_qualifier_desc with
  | PredicateQualifier e ->
      fprintf ff "[%a]" (print_expr_prec 0) e

and print_enclosed_expr ff es = print_expr_sequence 0 ff es


and print_cpclauses ff cps =
  match cps with
    | [x] ->
        begin
	      match x.pcopyvar_desc with
	        | vn,e' ->
	            fprintf ff "@[<hv 2>copy $%a :=@ %a@]" print_uqname vn (print_expr_prec 1) e'
        end
    | x :: cps' ->
      begin
	    match x.pcopyvar_desc with
	      | vn,e' ->
	          fprintf ff "@[<hv 2>copy $%a :=@ %a@]%a" print_uqname vn (print_expr_prec 1) e' print_copy_clauses cps'
      end
    | [] ->
        raise (Query (Malformed_Expr "Empty copy clause in transform statement"))
          

and print_copy_clauses ff cps =
  match cps with
  | [x] ->
      begin
	match x.pcopyvar_desc with
	  | (vn,e') ->
	    fprintf ff ",@;<1 4>@[<hv -2>$%a :=@ %a@]" print_uqname vn (print_expr_prec 1) e'
      end
  | x :: cps' ->
      begin
	match x.pcopyvar_desc with
	  | (vn,e') ->
	    fprintf ff ",@;<1 4>@[<hv -2>$%a :=@ %a@]%a"  print_uqname vn (print_expr_prec 1) e' print_copy_clauses cps'
      end
  | [] ->
      raise (Query (Malformed_Expr "Empty copy expression in transform statement"))


and print_function_arguments ff es =
  match es with
  | [] ->
      ()
  | [e] ->
      fprintf ff "%a" (print_expr_prec 0) e
  | e :: es' ->
      fprintf ff "%a,@ %a" (print_expr_prec 0) e print_function_arguments es'

and print_expr_sequence p ff es =
  match es with
  | [] ->
      fprintf ff "()"
  | [e] ->
      fprintf ff "%a" (print_expr_prec p) e
  | e :: es' ->
      if p > 0
      then
	fprintf ff "@[<hov 1>(%a,@ %a)@]" (print_expr_prec 0) e (print_expr_sequence 0) es'
      else
	fprintf ff "@[<hov 0>%a,@ %a@]" (print_expr_prec 0) e (print_expr_sequence 0) es'

and print_block_sequence ff es =
  match es with
  | [] ->
      ()
  | [e] ->
      fprintf ff "%a" (print_expr_prec 0) e
  | e :: es' ->
	  fprintf ff "@[<hov 0>%a;@ %a@]" (print_expr_prec 0) e print_block_sequence es'

and print_attrs ff es =
  match es with
  | [] ->
      ()
  | e :: es' ->
      fprintf ff " @[<hv 0>%a%a@]" print_attr e print_attrs_next es'

and print_attrs_next ff es =
  match es with
  | [] ->
      ()
  | e :: es' ->
      fprintf ff "@ %a%a" print_attr e print_attrs_next es'

and print_attr ff e =
  match e.pexpr_desc with
  | EAttrFixed (sa,es) ->
      fprintf ff "%a=\"%a\"" print_uqname sa print_attr_content es
  | _ ->
      fprintf ff "@[<hv 2>{@,%a@;<0 -2>}@]" (print_expr_prec 0) e

and print_attr_content ff es =
  match es with
  | [] ->
      ()
  | [e1] ->
      begin
	match e1.pexpr_desc with
	| EText s ->
	    fprintf ff "%s" s
	| EScalar(StringLiteral s) ->
	    fprintf ff "%s" s
	| _ ->
	    fprintf ff "@[<hv 2>{@,%a@;<0 -2>}@]" (print_expr_prec 0) e1
      end
  | _ ->
      print_attr_many_content ff es

and print_attr_many_content ff es =
  match es with
  | [] -> ()
  | e :: es' ->
      begin
	match e.pexpr_desc with
	| EText s ->
	    fprintf ff "%s" s;
	    print_attr_content ff es'
	| _ ->
	    fprintf ff "@[<hv 2>{@,%a@;<0 -2>}@]" (print_expr_prec 0) e;
	    print_attr_content ff es'
      end

(* Does not work - Jerome
and print_attr_content ff es =
  match es with
  | [] ->
      ()
  | e :: es' ->
      begin
	match e.pexpr_desc with
	| EText s ->
	    fprintf ff "%s" s;
	    print_attr_content ff es'
	| EScalar(StringLiteral s) ->
	    fprintf ff "%s" s;
	    print_attr_content ff es'
	| _ ->
	    fprintf ff "%a" (print_expr_prec 0) e;
	    print_attr_content ff es'
      end
*)

and has_only_constructor_content ecs =
  let has_only_constructor e =
  match e.pexpr_desc with
  | EElemFixed _
  | EText _
  | EPI _
  | EComment _ -> true
  | _ -> false
  in
  List.for_all has_only_constructor ecs

and print_elem_contents ff ecs =
  let filtered_ecs =
    let is_not_whitespace_only_text e =
      match e.pexpr_desc with
      | EText t -> not(Whitespace.whitespace_only t)
      | _ -> true
    in
    List.filter is_not_whitespace_only_text ecs
  in
  if has_only_constructor_content ecs
  then
    print_elem_contents_aux ff filtered_ecs
  else
    fprintf ff "@[<hv 2>{@,%a@;<0 -2>}@]" print_enclosed_expr filtered_ecs

and print_elem_contents_aux ff ecs =
    match ecs with
    | [] ->
	()
    | e :: [] ->
	fprintf ff "%a" print_elem_content e
    | e :: ecs' ->
	fprintf ff "%a@,%a" print_elem_content e print_elem_contents_aux ecs'
    

and print_elem_content ff e =
  match e.pexpr_desc with
  | EElemFixed (se, es1, es2) ->
      begin
	match es2 with
	| [] ->
	    fprintf ff "<%a@[%a@]/>" print_uqname se print_attrs es1
	| _ ->
	    fprintf ff "@[<hv 2><%a%a>@,%a@;<0 -2></%a>@]" print_uqname se print_attrs es1 print_elem_contents es2 print_uqname se
      end
  | EText t ->
      fprintf ff "%s" t
  | EPI (target,pi) ->
      fprintf ff "<?%s %s?>" target pi
  | EComment c ->
      fprintf ff "<!--%s-->" c
  | _ ->
      fprintf ff "%a" (print_expr_prec 0) e

and print_insert_location ff il =
  match il with
  | EAsLastInto e1 ->
      fprintf ff "@[<hv 2>as last into@ %a@]" (print_expr_prec 0) e1
  | EAsFirstInto e1 ->
      fprintf ff "@[<hv 2>as first into@ %a@]" (print_expr_prec 0) e1
  | EInto e1 ->
      fprintf ff "@[<hv 2>into@ %a@]" (print_expr_prec 0) e1
  | EAfter e1 ->
      fprintf ff "@[<hv 2>after@ %a@]" (print_expr_prec 0) e1
  | EBefore e1 ->
      fprintf ff "@[<hv 2>before@ %a@]" (print_expr_prec 0) e1

and print_snap ff snap =
  match snap with
  | Snap -> fprintf ff "snap "
  | NoSnap -> ()

(* Top level call *)

let rec print_expr ff e =
  	print_expr_prec 1 ff e ;
	fprintf ff "@?"



(**************)
(* Statements *)
(**************)

let print_statement ff s =
  fprintf ff "%a@?" print_expr_top s

let print_statement_body ff u =
  print_statement ff u; fprintf ff "\n@?"

let rec print_statements ff ss =
  match ss with
  | [] ->
      ()
  | s :: [] ->
      fprintf ff "%a;@." print_statement s
  | s :: ss' ->
      fprintf ff "%a;@\n%a" print_statement s print_statements ss'

(**********)
(* Module *)
(**********)

(* Function definitions *)

let rec print_input_signature ff vms =
  match vms with
  | [] ->
      ()
  | (v,odt) :: []  ->
      fprintf ff "$%a%a" print_uqname v print_optsequencetype odt
  | (v,odt) :: ms'  ->
      fprintf ff "$%a%a,@,%a" print_uqname v print_optsequencetype odt print_input_signature ms'

let print_function_block ff e =
  match e.pexpr_desc with
  | EBlock (decls, elist) ->  	
      fprintf ff "%a%a"
	print_block_decls decls 
	print_block_sequence elist
  | _ ->
      print_expr_top ff e

let print_function_def ff fd =
  match fd.pfunction_def_desc with
  | (fn, vars, (dts,odt), fbody, is_updating) ->
      let input_signature = 
	try
	  List.combine vars dts
	with
	| _ ->
	    raise (Query(Expr_Error(("Function signature mismatch"))))
      in
	begin
	  let upd_flag = updating_flag_to_string is_updating
	  in
	    match fbody with
	      | EFunctionInterface ->
		  fprintf ff "@[<hv 2>declare %sfunction %a(@,%a@;<0 -2>)%a external;@]" upd_flag print_uqname fn print_input_signature input_signature print_optsequencetype odt
	      | EFunctionImported -> 
		  fprintf ff "@[<hv 2>declare %sfunction %a(@,%a@;<0 -2>)%a imported;@]" upd_flag print_uqname fn print_input_signature input_signature print_optsequencetype odt
	      | EFunctionBltIn ->
		  fprintf ff "@[<hv 2>declare %sfunction %a(@,%a@;<0 -2>)%a external;@]" upd_flag print_uqname fn print_input_signature input_signature print_optsequencetype odt
	      | EFunctionUser e ->
		  fprintf ff "@[<hv 2>declare %sfunction@ %a(@,%a@;<0 -2>)%a @,{ %a }@;<0 -2>;@]" upd_flag print_uqname fn print_input_signature input_signature print_optsequencetype odt print_function_block e
	end

(* Global variables *)

let print_var_decl ff vd =
  match vd.pvar_decl_desc with
  | (vn, dt, EVarUser e) ->
      fprintf ff "@[<hv 2>declare variable $%a%a :=@ %a@;<0 -2>;@]" print_uqname vn print_optsequencetype dt print_expr_top e
  (* For now, interface definitions are printed like external variables *)
  | (vn, dt, EVarInterface) 
  | (vn, dt, EVarImported) 
  | (vn, dt, EVarExternal) ->
      fprintf ff "@[<hv 2>declare variable $%a%a external;@]" print_uqname vn print_optsequencetype  dt

let print_option_decl ff odecl =
  match odecl with
  | (oname,ocontent) ->
      fprintf ff "@[<hv 2>declare option %a \"%s\";@]" print_uqname oname ocontent

let print_server_decl ff sd =
  let (nc1, nc2, e) = sd.pserver_decl_desc in
  fprintf ff "@[<hv 2>declare@ server@ %s@ implement %s at %a@;<0 -2>;@]" nc1 nc2 print_expr_top e

let print_varfun ff vf =
  match vf with
  | OptionDecl (oname,ocontent) -> print_option_decl ff (oname,ocontent)
  | FunDef fdef -> print_function_def ff fdef
  | VarDef vd -> print_var_decl ff vd
  | ServerDef sd -> print_server_decl ff sd

let rec print_varfuns_decls ff vds =
  match vds with
  | [] ->
      ()
  | vd :: vds' ->
      fprintf ff "%a@\n%a" print_varfun vd print_varfuns_decls vds'


(* Indices *)

let print_index_decl ff kd =
  match kd.pindex_def_desc with
  | ValueIndex (kn, e1, e2) ->
      fprintf ff "@[<hv 2>declare value index %s {@,%a@;<0 -2>} {@,%a@;<0 -2>};@]" kn print_expr_top e1 print_expr_top e2
  | NameIndex se ->
      fprintf ff "@[<hv 2>declare name index %a;@]" print_uqname se

let rec print_index_decls ff kds =
  match kds with
  | [] ->
      ()
  | kd :: kds' ->
      fprintf ff "%a@\n%a" print_index_decl kd print_index_decls kds'

(* Context declarations *)

let print_context_decl ff cd =
  match cd.pcontext_decl_desc with
  | EBaseURIDecl s ->
      fprintf ff "declare base-uri %s;" s
  | ENamespaceDecl (ncname,uri) ->
      fprintf ff "declare namespace %s = %s;" ncname (quoted_string_of_uri uri)
  | EDefaultElementNamespaceDecl uri ->
      fprintf ff "declare default element namespace %s;" (quoted_string_of_uri uri)
  | EDefaultFunctionNamespaceDecl uri ->
      fprintf ff "declare default function namespace %s;" (quoted_string_of_uri uri)
  | EDefaultCollationDecl uri ->
      fprintf ff "declare default collation \"%s\";" uri
  | ESchemaDecl (ncname_opt, schemaname, schema_location_opt) ->
      begin
	match (ncname_opt,schema_location_opt) with
	| (None,None) ->
	    fprintf ff "import schema \"%s\";" schemaname
	| (Some (NSPrefix ncname),None)->
	    fprintf ff "import schema namespace %s = \"%s\";" ncname schemaname
	| (Some NSDefaultElementPrefix,None)->
	    fprintf ff "import schema default element namespace \"%s\";" schemaname
	| (None,Some location)->
	    fprintf ff "import schema \"%s\" at \"%s\";" schemaname location
	| (Some (NSPrefix ncname),Some location)->
	    fprintf ff "import schema namespace %s = \"%s\" at \"%s\";" ncname schemaname location
	| (Some NSDefaultElementPrefix,Some location)->
	    fprintf ff "import schema default element namespace \"%s\" at \"%s\";" schemaname location
	| (Some _,_) ->
	    raise (Query (Malformed_Expr "Unexpected namespace prefix while printing schema import statement"))
      end
  | EImportServiceDecl (ncname, servicename, loc_hint_opt) ->
      begin
	match loc_hint_opt with
	| None -> 
	    fprintf ff "import service namespace %s = \"%s\";" ncname servicename
	| Some location -> 
	    fprintf ff "import service namespace %s = \"%s\" at \"%s\";" ncname servicename location
      end
  | EImportInterfaceDecl (ncname, uri, loc_hint_opt) ->
      begin
	match loc_hint_opt with
	| None -> 
	    fprintf ff "import interface namespace %s = \"%s\";" ncname uri
	| Some location -> 
	    fprintf ff "import interface namespace %s = \"%s\" at \"%s\";" ncname uri location
      end
  | EImportModuleDecl (ncname, uri, loc_hint_opt) ->
      begin
	match loc_hint_opt with
	| None -> 
	    fprintf ff "import module namespace %s = \"%s\";" ncname uri
	| Some location -> 
	    fprintf ff "import module namespace %s = \"%s\" at \"%s\";" ncname uri location
      end
  | EXmlSpaceDecl Strip ->
      fprintf ff "declare xmlspace strip;"
  | EXmlSpaceDecl Preserve ->
      fprintf ff "declare xmlspace preserve;"
  | EConstructionDecl Strip ->
      fprintf ff "declare construction strip;"
  | EConstructionDecl Preserve ->
      fprintf ff "declare construction preserve;"
  | EOrderingDecl Ordered ->
      fprintf ff "declare ordering ordered;"
  | EOrderingDecl Unordered ->
      fprintf ff "declare ordering unordered;"
  | EDefaultEmptyOrderDecl EmptyGreatest ->
      fprintf ff "declare default order empty greatest;"
  | EDefaultEmptyOrderDecl EmptyLeast ->
      fprintf ff "declare default order empty least;"
  | ECopyNamespacesDecl (NSPreserve,NSInherit) ->
      fprintf ff "declare copy-namespaces preserve,inherit;"
  | ECopyNamespacesDecl (NSPreserve,NSNoInherit) ->
      fprintf ff "declare copy-namespaces preserve,no-inherit;"
  | ECopyNamespacesDecl (NSNoPreserve,NSInherit) ->
      fprintf ff "declare copy-namespaces no-preserve,inherit;"
  | ECopyNamespacesDecl (NSNoPreserve,NSNoInherit) ->
      fprintf ff "declare copy-namespaces no-preserve,no-inherit;"

let rec print_context_decls ff cds =
  match cds with
  | [] ->
      ()
  | cd :: cds' ->
      fprintf ff "%a@\n%a" print_context_decl cd print_context_decls cds'

(* Module itself *)

let print_prolog ff qm =
  if !Conf.print_prolog
  then
    begin
      print_context_decls ff qm.pprolog_contexts;
      List.iter (print_xschema ff) qm.pprolog_xschemas;
      print_varfuns_decls ff qm.pprolog_funcvars;
      print_index_decls   ff qm.pprolog_indices;
      fprintf ff "@?"
    end
  else
    ()

let print_interface_decl ff (ncname, targetnamespace) =
  fprintf ff "interface namespace %s = \"%s\";@\n" ncname targetnamespace

let print_interface_prolog ff qm =
  if !Conf.print_prolog
  then
    begin
      print_context_decls ff qm.iprolog_contexts;
      List.iter (print_xschema ff) qm.iprolog_xschemas;
      print_varfuns_decls ff qm.iprolog_funcvars;
      fprintf ff "@?"
    end
  else
    ()

let print_interface ff i = 
  print_interface_decl ff i.pinterface_decl;
  print_interface_prolog ff i.pinterface_prolog

let print_optinterface ff optint =
  match optint with
  | None -> ()
  | Some (uri, None) -> fprintf ff "implements@ %s" uri
  | Some (uri, Some loc) -> fprintf ff "implements@ %s@ at@ %s" uri loc

let print_module_decl ff (ncname, targetnamespace, optint) =
  fprintf ff "module namespace %s = \"%s\"@ %a;@\n" ncname targetnamespace print_optinterface optint

let print_library_module ff lm =
  print_module_decl ff lm.plibrary_module_decl;
  print_prolog ff lm.plibrary_module_prolog;
  fprintf ff "@?"

let print_main_module ff mm =
  print_prolog ff mm.pmain_module_prolog;
  print_statements ff mm.pmain_module_statements;
  fprintf ff "@?"

let print_module ff qm =
  match qm with
  | ELibraryModule lm ->
      print_library_module ff lm
  | EMainModule mm ->
      print_main_module ff mm

