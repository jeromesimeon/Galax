(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: print_xquery_core.ml,v 1.43 2007/10/16 01:25:34 mff Exp $ *)

(* Module: Print_xquery_core
   Description:
     This module implements pretty-printing for the XQuery core AST.
*)

open Error

open Gmisc

open Occurrence
open Namespace_names
open Datatypes

open Xquery_common_ast
open Xquery_core_ast
open Print_common

open Format


(*************)
(* Node kind *)
(*************)

let print_celement_test ff cet =
  match cet with
  | CSchemaElementTest cename ->
      fprintf ff "schema-element(%a)" print_rqname cename
  | CElementTest None ->
      fprintf ff "element()"
  | CElementTest (Some (cename, None)) ->
      fprintf ff "element(%a)" print_rqname cename
  | CElementTest (Some (cename, Some ctname)) ->
      fprintf ff "element(%a,%a)" print_rqname cename print_rqname ctname

let print_cattribute_test ff cat =
  match cat with
  | CSchemaAttributeTest caname ->
      fprintf ff "schema-attribute(%a)" print_rqname caname
  | CAttributeTest None ->
      fprintf ff "attribute()"
  | CAttributeTest (Some (caname, None)) ->
      fprintf ff "attribute(%a)" print_rqname caname
  | CAttributeTest (Some (caname, Some ctname)) ->
      fprintf ff "attribute(%a,%a)" print_rqname caname print_rqname ctname

let print_ckind_test ff ckt =
  match ckt with
  | CDocumentKind None ->
      fprintf ff "document-node()"
  | CDocumentKind (Some cet) ->
      fprintf ff "document-node(%a)" print_celement_test cet
  | CElementKind cet ->
      fprintf ff "%a" print_celement_test cet
  | CAttributeKind cat ->
      fprintf ff "%a" print_cattribute_test cat
  | CPIKind None ->
      fprintf ff "processing-instruction()"
  | CPIKind (Some s) ->
      fprintf ff "processing-instruction(\"%s\")" s
  | CCommentKind ->
      fprintf ff "comment()"
  | CTextKind ->
      fprintf ff "text()"
  | CAnyKind ->
      fprintf ff "node()"


(******************)
(* Sequence types *)
(******************)

let print_citemtype ff cdtk =
  match cdtk with
  | CITKindTest ckt ->
      fprintf ff "%a" print_ckind_test ckt
  | CITTypeRef ctname ->
      fprintf ff "type(%a)" print_rqname ctname
  | CITItem ->
      fprintf ff "item()"
  | CITNumeric ->
      fprintf ff "numeric()"
  | CITAnyString ->
      fprintf ff "anystring()"
  | CITEmpty ->
      fprintf ff "empty()"
  | CITAtomic ctname ->
      fprintf ff "%a" print_rqname ctname

let print_csequencetype ff cdt =
  match cdt.pcsequencetype_desc with
  | (cdtk,occ) ->
      fprintf ff "%a%a" print_citemtype cdtk print_occurence occ

let print_optcsequencetype ff codt =
  match codt with
  | None -> ()
  | Some (cdt,cty) ->
      fprintf ff " as %a" print_csequencetype cdt

(* Context declarations *)

let rec print_cinput_signature ff vms =
  match vms with
  | [] ->
      () 
  | (v,(dt,cty)) :: []  ->
      fprintf ff "$%a as %a" print_rqname v print_csequencetype dt
  | (v,(dt,cty)) :: ms'  ->
      fprintf ff "$%a as %a,@ %a" print_rqname v print_csequencetype dt print_cinput_signature ms'


(*****************************)
(* XPath 2.0 data structures *)
(*****************************)

(* Core node tests *)

let print_cnode_test ff nt =
  match nt with
  | CPNameTest qn ->
      fprintf ff "%a" print_rqname qn
  | CPNodeKindTest (nk, ty) ->
      fprintf ff "%a" print_ckind_test nk


(***************************)
(* Core XQuery expressions *)
(***************************)

let print_cexpr ff e print_annot =
  let rec print_cexpr_prec p ff e =
    if (!Conf.print_annotations)
    then
      fprintf ff "%a" print_annot e.pcexpr_annot
    else ();
    fprintf ff "%a" (print_cexpr_desc p) e

  and print_enclosed_cexpr ff ecs =
    print_cexpr_sequence 0 ff ecs

  and print_single_enclosed_cexpr ff e =
    print_cexpr_prec 0 ff e

  and print_cexpr_sequence p ff ecs =
    match ecs with
    | [] ->
	fprintf ff "()"
    | e :: [] ->
	fprintf ff "%a" (print_cexpr_prec p) e
    | e :: ecs' ->
	if p > 0
	then
	  fprintf ff "@[<hov 1>(%a,@ %a)@]" (print_cexpr_prec 0) e (print_cexpr_sequence 0) ecs'
	else
	  fprintf ff "@[<hov 1>%a,@ %a@]" (print_cexpr_prec 0) e (print_cexpr_sequence 0) ecs'

  (* and print_cexpr_sequencing ff ces =
    match ces with
    | [] -> ()
    | ce :: ces' ->
	fprintf ff "%a;@ %a" (print_cexpr_prec 0) ce print_cexpr_sequencing ces' *)

  and print_cexpr_desc p ff e =
    match e.pcexpr_desc with
    | CEUnordered e ->
	fprintf ff "@[<hv 2>unordered {@,%a@;<0 -2>}@]" (print_cexpr_prec 0) e
    | CEOrdered e ->
	fprintf ff "@[<hv 2>ordered {@,%a@;<0 -2>}@]" (print_cexpr_prec 0) e
    | CEFLWOR (cfls, cw, co, cr) ->
	if p > 1
	then
	  fprintf ff "@[<hv 1>(%a%a%a%a)@]" print_cflclauses cfls print_cwhere cw print_corder co print_creturn cr
	else
	  fprintf ff "@[<hv 0>%a%a%a%a@]" print_cflclauses cfls print_cwhere cw print_corder co print_creturn cr
    | CEIf (e1,e2,e3) ->
	if p > 1
	then
	  fprintf ff "@[<hv 1>(if (%a)@;<1 0>@[<hv 2>then@;<1 0>%a@]@;<1 0>@[<hv 2>else@;<1 0>%a@])@]" (print_cexpr_prec 0) e1 (print_cexpr_prec 1) e2 (print_cexpr_prec 1) e3
	else
	  fprintf ff "@[<hv 0>if (%a)@;<1 0>@[<hv 2>then@;<1 0>%a@]@;<1 0>@[<hv 2>else@;<1 0>%a@]@]" (print_cexpr_prec 0) e1 (print_cexpr_prec 1) e2 (print_cexpr_prec 1) e3
    | CETypeswitch (e', cs) ->
	if p > 1
	then
	  fprintf ff "@[<hv 1>(@[<hv 2>@[<hv 2>typeswitch (@,%a@;<0 -2>)@]@;<1 0>%a@])@]" (print_cexpr_prec 0) e' print_ccases cs
	else
	  fprintf ff "@[<hv 2>@[<hv 2>typeswitch (@,%a@;<0 -2>)@]@;<1 0>%a@]" (print_cexpr_prec 0) e' print_ccases cs
    | CEVar v ->
	fprintf ff "$%a" print_rqname v
    | CEScalar l ->
	fprintf ff "%a" print_literal l
    | CEProtoValue l ->
	fprintf ff "%a" print_proto_value l
    | CEText t ->
	fprintf ff "@[<hv 2>text {@,\"%s\"@;<0 -2>}@]" t
    | CECharRef i ->
	fprintf ff "@[<hv 2>text {@,\"&#%i;\"@;<0 -2>}@]" i
    | CETextComputed e ->
	fprintf ff "@[<hv 2>text {@,%a@;<0 -2>}@]" print_single_enclosed_cexpr e
    | CEPI (target,pi_content) ->
	fprintf ff "<?%s %s?>" target pi_content
    | CEPIComputed (e1, e2) ->
	fprintf ff "@[<hv 2>processing-instruction {@,%a@;<0 -2>} {@,%a@;<0 -2>}@]" (print_cexpr_prec p) e1 (print_cexpr_prec p) e2
    | CEComment c ->
	fprintf ff "<!--%s-->" c
    | CECommentComputed c ->
	fprintf ff "@[<hv 2>comment {@,%a@;<0 -2>}@]" print_single_enclosed_cexpr c
    | CEDocument e ->
	fprintf ff "@[<hv 2>document {@,%a@;<0 -2>}@]" print_single_enclosed_cexpr e
    | CECall (s,params, (dts, ot), _,_) ->
(* 	let upd_flag =  *)
(* 	  if upd = Updating then "updating" else "" *)
(* 	in *)
	  fprintf ff "@[<hv 2>%a(@,%a@;<0 -2>)@]" print_rqname s print_function_arguments params
    | CEOverloadedCall (s,params,_) ->
	fprintf ff "@[<hv 2>%a(@,%a@;<0 -2>)@]" print_rqname s print_function_arguments params
    | CESeq (e1,e2) ->
	if p > 0 then
	  fprintf ff "@[<hov 1>(%a,@ %a)@]" (print_cexpr_prec 0) e1 (print_cexpr_prec 0) e2
	else
	  fprintf ff "@[<hov 0>%a,@ %a@]" (print_cexpr_prec 0) e1 (print_cexpr_prec 0) e2
    | CEEmpty ->
	fprintf ff "()"
    | CEElem (l, nsenv, el) ->
	begin
	  match el with 
	  | [] -> 
	      fprintf ff "@[<hv 2>element %a {}@]" print_rqname l 
	  | _ -> 
	      fprintf ff "@[<hv 2>element %a {@,%a@;<0 -2>}@]" print_rqname l print_enclosed_cexpr el
	end
    | CEAnyElem (e1, nsenv1, nsenv2, e2) ->
	begin
	  match e2.pcexpr_desc with
	  | CEEmpty ->
	      fprintf ff "@[<hv 2>element {@,%a@;<0 -2>} {}@]" print_single_enclosed_cexpr e1
	  | _ ->
	      fprintf ff "@[<hv 2>element {@,%a@;<0 -2>} {@,%a@;<0 -2>}@]" print_single_enclosed_cexpr e1 print_single_enclosed_cexpr e2
	end
    | CEAttr(l,nsenv,el) ->
	begin
	  match el with
	  | [] ->
	      fprintf ff "@[<hv 2>attribute %a {}@]" print_rqname l
	  | _ ->
	      fprintf ff "@[<hv 2>attribute %a {@,%a@;<0 -2>}@]" print_rqname l print_enclosed_cexpr el
	end
    | CEAnyAttr(e1,_,e2) ->
	begin
	  match e2.pcexpr_desc with
	  | CEEmpty ->
	      fprintf ff "@[<hv 2>attribute {@,%a@;<0 -2>} {}@]" print_single_enclosed_cexpr e1
	  | _ ->
	      fprintf ff "@[<hv 2>attribute {@,%a@;<0 -2>} {@,%a@;<0 -2>}@]" print_single_enclosed_cexpr e1 print_single_enclosed_cexpr e2
	end
    | CEError cexprlist ->
	fprintf ff "@[<hv 2>%a(@,%a@;<0 -2>)@]" print_rqname Namespace_builtin.fn_error print_function_arguments cexprlist
    | CETreat (e',(cdt,cty)) ->
	if p > 11
	then
	  fprintf ff "@[<hv 1>(@[<hv 2>%a treat as %a@])@]" (print_cexpr_prec 11) e' print_csequencetype cdt
	else
	  fprintf ff "@[<hv 2>%a treat as %a@]" (print_cexpr_prec 11) e' print_csequencetype cdt
    | CEValidate (vmode,e') ->
	fprintf ff "@[<hv 2>validate %a@ {@,%a@;<0 -2>}@]" print_validation_mode vmode (print_cexpr_prec 0) e'
    | CECast (e',_,(cdt, cty)) ->
	if p > 13
	then
	  fprintf ff "@[<hv 1>(@[<hv 2>%a@ cast as %a@])@]" (print_cexpr_prec 13) e' print_csequencetype cdt
	else
	  fprintf ff "@[<hv 2>%a@ cast as %a@]" (print_cexpr_prec 13) e' print_csequencetype cdt
    | CECastable (e',_,(cdt,cty)) ->
	if p > 12
	then
	  fprintf ff "@[<hv 1>(@[<hv 2>%a@ castable as %a@])@]" (print_cexpr_prec 12) e' print_csequencetype cdt
	else
	  fprintf ff "@[<hv 2>%a@ castable as %a@]" (print_cexpr_prec 12) e' print_csequencetype cdt
    | CEForwardAxis (v, a,nt)
    | CEReverseAxis (v, a,nt) ->
        if rqname_equal fs_dot v
	then fprintf ff "%a::%a" print_axis a print_cnode_test nt
	else fprintf ff "$%a/%a::%a" print_rqname v print_axis a print_cnode_test nt
    | CESome (odt, vn, ce1, ce2) ->
	if p > 1
	then
	  fprintf ff "@[<hv 1>(@[<hv 0>@[<hv 5>some %a@]@ @[<hv 2>satisfies@ %a@]@])@]" print_binding (odt,vn,ce1) (print_cexpr_prec 1) ce2
	else
	  fprintf ff "@[<hv 0>@[<hv 5>some %a@]@ @[<hv 2>satisfies@ %a@]@]" print_binding (odt,vn,ce1) (print_cexpr_prec 1) ce2
    | CEEvery (odt, vn, ce1, ce2) ->
	if p > 1
	then
	  fprintf ff "@[<hv 1>(@[<hv 0>@[<hv 5>every %a@]@ @[<hv 2>satisfies@ %a@]@])@]" print_binding (odt,vn,ce1) (print_cexpr_prec 1) ce2
	else
	  fprintf ff "@[<hv 0>@[<hv 5>every %a@]@ @[<hv 2>satisfies@ %a@]@]" print_binding (odt,vn,ce1) (print_cexpr_prec 1) ce2
    | CELetServerImplement (nc1, uri, ce1, ce2) ->
	fprintf ff "@[<hv 2>let server %s @[<hv 2>implement %s@] @[<hv 2>at %a@] @;<0 0>@[<hv 2>return@;<1 0>{%a}@]@]" nc1 uri (print_cexpr_prec 0) ce1 (print_cexpr_prec 0) ce2
    | CEExecute (async, nc, uri, hostport, remoteexpr) ->
	if (async) then 
	  fprintf ff "@[<hv 2>at@ server@ %s@ @;<0 0>@[<hv 2>do@;<1 0>%a@]@]" nc (print_cexpr_prec 0) remoteexpr
	else 
	  fprintf ff "@[<hv 2>from@ server@ %s @;<0 0>@[<hv 2>return@;<1 0>%a@]@]" nc (print_cexpr_prec 0) remoteexpr
    | CEForServerClose (nc1, uri, ce1) ->
	fprintf ff "@[<hv 2>for server %s @[<hv 2>box@] @[<hv 2>%a@]" nc1 (print_cexpr_prec 0) ce1 
    | CEEvalClosure (ce1) ->
	fprintf ff "@[<hv 2>eval box @[<hv 2>%a@]" (print_cexpr_prec 0) ce1 

    | CECopy ce1 ->
	fprintf ff "@[<hv 2>copy@ {@,%a@;<0 -2>}@]" (print_cexpr_prec 0) ce1
    | CEDelete ce1 ->
	fprintf ff "@[<hv 2>delete nodes@ %a@]" (print_cexpr_prec 0) ce1
    | CEInsert (ce1, cil) ->
	fprintf ff "@[@[<hv 2>insert nodes@ %a@]@ %a@]" (print_cexpr_prec 0) ce1 print_cinsert_location cil
    | CERename (nsenv, e1, e2) ->
	fprintf ff "@[@[<hv 2>rename node@ %a@]@ @[<hv 2>with %a@]@]" (print_cexpr_prec 0) e1 (print_cexpr_prec 0) e2
    | CEReplace (Normal_Replace, e1, e2) ->
	fprintf ff "@[@[<hv 2>replace node@ %a@]@ @[<hv 2>with %a@]@]" (print_cexpr_prec 0) e1 (print_cexpr_prec 0) e2
    | CEReplace (Value_Of_Replace, e1, e2) ->
	fprintf ff "@[@[<hv 2>replace value of node@ %a@]@ @[<hv 2>with %a@]@]" (print_cexpr_prec 0) e1 (print_cexpr_prec 0) e2
    
    | CESnap (Snap_Unordered_Deterministic,ce) ->
	fprintf ff "@[<hv 2>snap {@,%a@;<0 -2>}@]" (print_cexpr_prec 0) ce
    | CESnap (Snap_Nondeterministic,ce) ->
	fprintf ff "@[<hv 2>snap unordered {@,%a@;<0 -2>}@]" (print_cexpr_prec 0) ce
    | CESnap (Snap_Ordered_Deterministic,ce) ->
	fprintf ff "@[<hv 2>snapo {@,%a@;<0 -2>}@]" (print_cexpr_prec 0) ce
    | CELetvar (codt,vn,ce1,ce2) ->
	if p > 1
	then
	  fprintf ff "@[<hv 1>(@[<hv 2>letvar $%a%a :=@ %a@]%a)@]" print_rqname vn print_optcsequencetype codt (print_cexpr_prec 1) ce1 print_creturn ce2      
	else
	  fprintf ff "@[<hv 0>@[<hv 2>letvar $%a%a :=@ %a@]%a@]" print_rqname vn print_optcsequencetype codt (print_cexpr_prec 1) ce1 print_creturn ce2      
    | CESet (vn,ce) ->
	fprintf ff "@[<hv 2>set $%a :=@ %a@]" print_rqname vn (print_cexpr_prec 1) ce
    | CEImperativeSeq (e1,e2) ->
	if p > 0 then
	  fprintf ff "@[<hov 1>(%a;@ %a)@]" (print_cexpr_prec 0) e1 (print_cexpr_prec 0) e2
	else
	  fprintf ff "@[<hov 0>%a;@ %a@]" (print_cexpr_prec 0) e1 (print_cexpr_prec 0) e2
    | CEWhile (test,ret) ->
	fprintf ff "@[<hv 0>while (%a)%a@]" (print_cexpr_prec 0) test print_creturn ret

  and print_binding ff (odt,vn,ce1) =
    fprintf ff "@[<hv -3>$%a%a in@ %a@]" print_rqname vn print_optcsequencetype odt (print_cexpr_prec 0) ce1

  and print_cinsert_location ff cil =
    match cil with
    | CUAsLastInto ce1 ->
	fprintf ff "@[<hv 2>as last into %a@]" (print_cexpr_prec 0) ce1
    | CUAsFirstInto ce1 ->
	fprintf ff "@[<hv 2>as first into %a@]" (print_cexpr_prec 0) ce1
    | CUInto ce1 ->
	fprintf ff "@[<hv 2>into %a@]" (print_cexpr_prec 0) ce1
    | CUAfter ce1 ->
	fprintf ff "@[<hv 2>after %a@]" (print_cexpr_prec 0) ce1
    | CUBefore ce1 ->
	fprintf ff "@[<hv 2>before %a@]" (print_cexpr_prec 0) ce1

(* Note:
	Always prints the let and for keywords, when in the core
   - Jerome
   *)
and print_cflclauses ff fls =
  match fls with
  | [x] ->
      begin
	match x with
	| CELET (odt,vn,e') ->
	    fprintf ff "@[<hv 2>let $%a%a :=@ %a@]" print_rqname vn print_optcsequencetype odt (print_cexpr_prec 1) e'
	| CEFOR (odt,vn,ovn,e') ->
	    begin
	      match ovn with
	      | None ->
		  fprintf ff "@[<hv 2>for $%a%a in@ %a@]"  print_rqname vn print_optcsequencetype odt (print_cexpr_prec 1) e'
	      | Some vn' ->
		  fprintf ff "@[<hv 2>for $%a%a at $%a in@ %a@]" print_rqname vn print_optcsequencetype odt print_rqname vn' (print_cexpr_prec 1) e'
	    end
      end
  | x :: fls' ->
      begin
	match x with
	| CELET (odt,vn,e') ->
	    fprintf ff "@[<hv 2>let $%a%a :=@ %a@]%a" print_rqname vn print_optcsequencetype odt (print_cexpr_prec 1) e' print_clet_clauses fls'
	| CEFOR (odt,vn,ovn,e') ->
	    begin
	      match ovn with
	      | None ->
		  fprintf ff "@[<hv 2>for $%a%a in@ %a@]%a" print_rqname vn print_optcsequencetype odt (print_cexpr_prec 1) e' print_cfor_clauses fls'
	      | Some vn' ->
		  fprintf ff "@[<hv 2>for $%a%a at $%a in@ %a@]%a" print_rqname vn print_optcsequencetype odt print_rqname vn' (print_cexpr_prec 1) e' print_cfor_clauses fls'
	    end
      end
  | [] ->
      raise (Query (Malformed_Expr "Empty flower"))

and print_cfor_clauses ff fls =
  match fls with
  | [x] ->
      begin
	match x with
	| CELET (odt,vn,e') ->
	    fprintf ff "@;<1 0>@[<hv 2>let $%a%a :=@ %a@]" print_rqname vn print_optcsequencetype odt (print_cexpr_prec 1) e'
	| CEFOR (odt,vn,ovn,e') ->
	    begin
	      match ovn with
	      | None ->
		  fprintf ff ",@;<1 4>@[<hv -2>$%a%a in@ %a@]" print_rqname vn print_optcsequencetype odt (print_cexpr_prec 1) e'
	      | Some vn' ->
		  fprintf ff ",@;<1 4>@[<hv -2>$%a%a at $%a in@ %a@]" print_rqname vn print_optcsequencetype odt print_rqname vn' (print_cexpr_prec 1) e'
	    end
      end
  | x :: fls' ->
      begin
	match x with
	| CELET (odt,vn,e') ->
	    fprintf ff "@;<1 0>@[<hv 2>let $%a%a :=@ %a@]%a" print_rqname vn print_optcsequencetype odt (print_cexpr_prec 1) e' print_clet_clauses fls'
	| CEFOR (odt,vn,ovn,e') ->
	    begin
	      match ovn with
	      | None ->
		  fprintf ff ",@;<1 4>@[<hv -2>$%a%a in@ %a@]%a" print_rqname vn print_optcsequencetype odt (print_cexpr_prec 1) e' print_cfor_clauses fls'
	      | Some vn' ->
		  fprintf ff ",@;<1 4>@[<hv -2>$%a%a at $%a in@ %a@]%a" print_rqname vn print_optcsequencetype odt print_rqname vn' (print_cexpr_prec 1) e' print_cfor_clauses fls'
	    end
      end
  | [] ->
      raise (Query (Malformed_Expr "Empty nested for expression"))

and print_clet_clauses ff fls =
  match fls with
  | [x] ->
      begin
	match x with
	| CELET (odt,vn,e') ->
	    fprintf ff ",@;<1 4>@[<hv -2>$%a%a :=@ %a@]" print_rqname vn print_optcsequencetype odt (print_cexpr_prec 1) e'
	| CEFOR (odt,vn,ovn,e') ->
	    begin
	      match ovn with
	      | None ->
		  fprintf ff "@;<1 0>@[<hv 2>for $%a%a in@ %a@]" print_rqname vn print_optcsequencetype odt (print_cexpr_prec 1) e'
	      | Some vn' ->
		  fprintf ff "@;<1 0>@[<hv 2>for $%a%a at $%a in@ %a@]" print_rqname vn print_optcsequencetype odt print_rqname vn' (print_cexpr_prec 1) e'
	    end
      end
  | x :: fls' ->
      begin
	match x with
	| CELET (odt,vn,e') ->
	    fprintf ff ",@;<1 4>@[<hv -2>$%a%a :=@ %a@]%a"  print_rqname vn print_optcsequencetype odt (print_cexpr_prec 1) e' print_clet_clauses fls'
	| CEFOR (odt,vn,ovn,e') ->
	    begin
	      match ovn with
	      | None ->
		  fprintf ff "@;<1 0>@[<hv 2>for $%a%a in@ %a@]%a" print_rqname vn print_optcsequencetype odt (print_cexpr_prec 1) e' print_cfor_clauses fls'
	      | Some vn' ->
		  fprintf ff "@;<1 0>@[<hv 2>for $%a%a at $%a in@ %a@]%a" print_rqname vn print_optcsequencetype odt print_rqname vn' (print_cexpr_prec 1) e' print_cfor_clauses fls'
	    end
      end
  | [] ->
      raise (Query (Malformed_Expr "Empty nested for expression"))

and print_corder_by_clause ff (stablekind,ssl,_) =
  fprintf ff "@[<hv 2>%aorder by@ %a@]" print_stablekind stablekind print_csortspeclist ssl

and print_csortspeclist ff ssl =
  match ssl with
  | [x] ->
      fprintf ff "@[%a@]" print_csortspec x
  | x :: ssl' ->
      fprintf ff "@[%a@],@ @[%a@]" print_csortspec x print_csortspeclist ssl'
  | [] ->
      raise (Query (Malformed_Expr "Empty sort kind"))

and print_csortspec ff sk =
  match sk with
  | (e,sk,esk) ->
      fprintf ff "@[%a@] @[%a@] @[%a@]" (print_cexpr_prec 0) e print_sortkind sk print_emptysortkind esk

and print_cwhere ff w =
  match w with
  | None ->
      ()
  | Some e ->
      fprintf ff "@;<1 0>@[<hv 2>where@;<1 0>%a@]" (print_cexpr_prec 1) e

and print_corder ff o =
  match o with
  | None ->
      ()
  | Some oc ->
      fprintf ff "@;<1 0>%a" print_corder_by_clause oc

and print_creturn ff r =
  fprintf ff "@;<1 0>@[<hv 2>return@;<1 0>%a@]" (print_cexpr_prec 1) r

  and print_function_arguments ff es =
    match es with
    | [] ->
	()
    | [e] ->
	fprintf ff "%a" (print_cexpr_prec 0) e
    | e :: es' ->
	fprintf ff "%a,@ %a" (print_cexpr_prec 0) e print_function_arguments es'

  and print_ccases ff cs =
    match cs with
    | [] ->
	()
    | (p,ovn,e) :: cs' ->
	begin
	  match p.pcpattern_desc with
	  | CCase (m,cty) ->
	      fprintf ff "case %a%a return@;<1 2>%a@;%a" print_coptvariable_as ovn print_csequencetype m (print_cexpr_prec 1) e print_ccases cs'
	  | CDefault ->
	      fprintf ff "default %areturn@;<1 2>%a@;%a" print_coptvariable ovn (print_cexpr_prec 1) e print_ccases cs'
	end

  and print_coptvariable ff ovn =
    match ovn with
    | None -> ()
    | Some vn -> fprintf ff "$%a " print_rqname vn

  and print_coptvariable_as ff ovn =
    match ovn with
    | None -> ()
    | Some vn -> fprintf ff "$%a as " print_rqname vn

in print_cexpr_prec 0 ff e


(*******************)
(* Core Statements *)
(*******************)

(* Statements *)

let print_cstatement ff s print_annot =
  let print_cexpr ff ce = 
    print_cexpr ff ce print_annot 
  in
  fprintf ff "%a" print_cexpr s 

let rec print_cstatements ff ss print_annot =
  let print_cstatements ff cs = 
    print_cstatements ff cs print_annot 
  in
  let print_cstatement ff cs = 
    print_cstatement ff cs print_annot 
  in
  match ss with
  | [] ->
      ()
  | s :: [] ->
      fprintf ff "%a" print_cstatement s
  | s :: ss' ->
      fprintf ff "%a;@\n%a" print_cstatement s print_cstatements ss'

(***************)
(* Core Module *)
(***************)

(* Function definitions *)

let print_cprolog ff qm print_annot =
  let print_cexpr ff ce =
    print_cexpr ff ce print_annot
  in
  let rec print_cfunction_def ff fd =
    match fd.pcfunction_def_desc with
    | ((fn,arity), vars, (dts,(cdt,cty)), fbody, upd) ->
	begin
	  let input_signature =
	    try
	      List.combine vars dts
	    with
	    | _ ->
		raise (Query(Expr_Error(("Function signature mismatch"))))
	  in	    
	  let upd_flag = updating_flag_to_string upd
	  in
	  match fbody with
	  | CEFunctionInterface ->
	      fprintf ff "@[<hv 2>@[<hv 2>declare %s function %a(@,%a@;<0 2>)@]@ as %a;@ (:interface:) external;@]" upd_flag print_rqname fn print_cinput_signature input_signature print_csequencetype cdt
	  | CEFunctionImported ->
	      fprintf ff "@[<hv 2>@[<hv 2>declare %s function %a(@,%a@;<0 2>)@]@ as %a;@ (:imported:) external;@]" upd_flag print_rqname fn print_cinput_signature input_signature print_csequencetype cdt
	  | CEFunctionBltIn ->
	      fprintf ff "@[<hv 2>@[<hv 2>declare %s function %a(@,%a@;<0 2>)@]@ as %a;@ external;@]" upd_flag print_rqname fn print_cinput_signature input_signature print_csequencetype cdt
	  | CEFunctionUser e ->
	      fprintf ff "@[<hv 2>@[<hv 2>declare %s function@ %a(@,%a@;<0 -2>)@] as %a {@,%a@;<0 -2>};@]" upd_flag print_rqname fn print_cinput_signature input_signature print_csequencetype cdt print_cexpr e
	end

(* MF: pretty printing is very finicky. The @, below is essential 
   to getting good line breaks between function definitions *)
  and print_cfunction_defs ff fds =
    match fds with
    | [] ->
	()
    | fd :: fds' ->
	fprintf ff "%a@\n%a" print_cfunction_def fd print_cfunction_defs fds'

  (* Variable declaration *)

  and print_cvar_decl ff vd =
    match vd.pcvar_decl_desc with
    | (vn, odt, CEVarUser e) ->
	fprintf ff "@[<hv 2>declare variable $%a%a :=@ %a@;<0 -2>;@]" print_rqname vn print_optcsequencetype odt print_cexpr e
    | (vn, odt, CEVarExternal) ->
	fprintf ff "@[<hv 2>declare variable $%a%a external;@]" print_rqname vn print_optcsequencetype odt
    | (vn, odt, CEVarInterface) ->
	fprintf ff "@[<hv 2>declare variable $%a%a (:interface:) external;@]" print_rqname vn print_optcsequencetype odt
    | (vn, odt, CEVarImported) ->
	fprintf ff "@[<hv 2>declare variable $%a%a (:imported:) external;@]" print_rqname vn print_optcsequencetype odt

  and print_cvar_decls ff vds =
    match vds with
    | [] ->
	()
    | vd :: vds' ->
	fprintf ff "%a@\n%a" print_cvar_decl vd print_cvar_decls vds'

(* Indices *)

  and print_cindex_decl ff kd =
    match kd.pcindex_def_desc with
    | CValueIndex (kn, ce1, ce2) ->
	fprintf ff "@[<hv 2>declare value index %s {@,%a@;<0 -2>} {@,%a@;<0 -2>};@]" kn print_cexpr ce1 print_cexpr ce2
    | CNameIndex se ->
	fprintf ff "@[<hv 2>declare name index %a;@]" print_rqname se

  and print_cindex_decls ff kds =
    match kds with
    | [] ->
	()
    | kd :: kds' ->
	fprintf ff "%a@\n%a" print_cindex_decl kd print_cindex_decls kds'

  (* The module itself *)
  in
  let print_cprolog' ff p =
    print_cindex_decls   ff p.pcprolog_indices;
    print_cvar_decls     ff p.pcprolog_vars;
    print_cfunction_defs ff p.pcprolog_functions
  in
  if !Conf.print_prolog
  then
    print_cprolog' ff qm
  else
    ()

let print_cmodule ff qm print_annot =
  print_cprolog ff qm.pcmodule_prolog print_annot;
  print_cstatements ff qm.pcmodule_statements print_annot

