(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: print_xquery_algebra.ml,v 1.75 2007/10/16 01:25:34 mff Exp $ *)

(* Module: Print_xquery_algebra
   Description:
     This module implements pretty-printing for the XQuery Algebra
     AST.
*)

open Error

open Gmisc

open Occurrence
open Namespace_names
open Namespace_symbols
open Datatypes

open Xquery_common_ast
open Xquery_algebra_ast
open Xquery_algebra_ast_util
open Xquery_algebra_ast_annotation_util
open Print_common

open Format


(*************)
(* Node kind *)
(*************)

let print_aelement_test ff aet =
  match aet with
  | ASchemaElementTest elem_sym ->
      let cename = relem_name elem_sym in
      fprintf ff "schema-element(%a)" print_rqname cename
  | AElementTest None ->
      fprintf ff "element()"
  | AElementTest (Some (elem_sym, None)) ->
      let cename = relem_name elem_sym in
      fprintf ff "element(%a)" print_rqname cename
  | AElementTest (Some (elem_sym, Some type_sym)) ->
      let cename = relem_name elem_sym in
      let ctname = rtype_name type_sym in
      fprintf ff "element(%a,%a)" print_rqname cename print_rqname ctname

let print_aattribute_test ff aat =
  match aat with
  | ASchemaAttributeTest attr_sym ->
      let caname = rattr_name attr_sym in
      fprintf ff "schema-attribute(%a)" print_rqname caname
  | AAttributeTest None ->
      fprintf ff "attribute()"
  | AAttributeTest (Some (attr_sym, None)) ->
      let caname = rattr_name attr_sym in
      fprintf ff "attribute(%a)" print_rqname caname
  | AAttributeTest (Some (attr_sym, Some type_sym)) ->
      let caname = rattr_name attr_sym in
      let ctname = rtype_name type_sym in
      fprintf ff "attribute(%a,%a)" print_rqname caname print_rqname ctname

let print_akind_test ff akt =
  match akt with
  | ADocumentKind None ->
      fprintf ff "document-node()"
  | ADocumentKind (Some aet) ->
      fprintf ff "document-node(%a)" print_aelement_test aet
  | AElementKind aet ->
      fprintf ff "%a" print_aelement_test aet
  | AAttributeKind aat ->
      fprintf ff "%a" print_aattribute_test aat
  | APIKind None ->
      fprintf ff "processing-instruction()"
  | APIKind (Some s) ->
      fprintf ff "processing-instruction(\"%s\")" s
  | ACommentKind ->
      fprintf ff "comment()"
  | ATextKind ->
      fprintf ff "text()"
  | AAnyKind ->
      fprintf ff "node()"


(******************)
(* Sequence types *)
(******************)

let print_aitemtype ff adtk =
  match adtk with
  | AITKindTest akt ->
      fprintf ff "%a" print_akind_test akt
  | AITTypeRef type_sym ->
      let ctname = rtype_name type_sym in
      fprintf ff "type(%a)" print_rqname ctname
  | AITItem ->
      fprintf ff "item()"
  | AITNumeric ->
      fprintf ff "numeric()"
  | AITAnyString ->
      fprintf ff "anystring()"
  | AITEmpty ->
      fprintf ff "empty()"
  | AITAtomic type_sym ->
      let ctname = rtype_name type_sym in
      fprintf ff "%a" print_rqname ctname

let print_asequencetype ff adt =
  match adt.pasequencetype_desc with
  | (adtk,occ) ->
      fprintf ff "%a%a" print_aitemtype adtk print_occurence occ


(* Replicated code should make a print core util *)
let print_anode_test ff nt =
  match nt with
  | APNameTest qn ->
      fprintf ff "%s" (Namespace_symbols.relem_prefix_string qn)
  | APNodeKindTest nk ->
      fprintf ff "%a" print_akind_test nk

let print_optasequencetype ff t =
  match t with
  | None -> ()
  | Some dt ->
      fprintf ff " as %a" print_asequencetype dt


(***************************)
(* Core XQuery expressions *)
(***************************)

let print_algop_insert_loc ff loc =
  match loc with
  | AOUAsLastInto -> fprintf ff "as last into"
  | AOUAsFirstInto -> fprintf ff "as first into"
  | AOUInto -> fprintf ff "into"
  | AOUAfter -> fprintf ff "after"
  | AOUBefore -> fprintf ff "before"

let print_algop_value_of_flag ff vf =
  match vf with 
  | Normal_Replace -> ()
  | Value_Of_Replace -> fprintf ff "value of@;"

let rec print_var_list ff tfl =
  match tfl with 
  | [] -> ()
  | x :: [] ->
      fprintf ff "%a" print_rqname x
  | x :: rest ->
      fprintf ff "%a,%a" print_rqname x print_var_list rest

let print_free_vars ff fv =
  if fv = []
  then ()
  else fprintf ff "free : %a;@;" print_var_list fv

let string_of_variable_usage u =
  match u with
  | Never -> "Never"
  | Once  -> "Once"
  | Many  -> "Many"
  | Redefined -> "Redefined"

let print_use_count ff (var,(count,usage)) =
  fprintf ff "(%a,%i,%s),"
    print_rqname var
    count
    (string_of_variable_usage usage)

let print_use_counts ff uc_list =
  List.iter (fprintf ff "%a" print_use_count) uc_list

let print_bound_vars ff bv =
  if bv = []
  then ()
  else fprintf ff "bound : %a;@;" print_use_counts bv

let print_accessed_fields ff tf =
  if tf = []
  then ()
  else fprintf ff "accessed : %a;@;" print_var_list tf

let string_of_cardinality c =
  match c with
    | NoTable -> "NoTable"
    | Table COne -> "Table COne"
    | Table CMany -> "Table CMany"
	
let print_cardinality ff c =
  fprintf ff "cardinality: %s;@;" (string_of_cardinality c)

let print_candidate_fields ff tf =
  if tf = []
  then ()
  else fprintf ff "candidate fields : %a;@;" print_var_list tf

let print_tuple_field_use_counts ff tf = 
  if tf = [] then
    ()
  else
    fprintf ff "tuple field use counts : %a;@;" print_use_counts tf

let print_returned_fields ff tf =
  if tf = []
  then ()
  else fprintf ff "returned : %a;@;" print_var_list tf

let check_lists cur f l_of_l =
  let apply b cur_list = b || (not ((f cur_list) = cur)) in
  List.fold_left apply false l_of_l

let free_var_changed cur i_list d_list =
  (* check that cur is the same as each of the lists *)
  check_lists cur algop_get_free_variables (i_list @ d_list)

let tuple_fields_changed cur i_list d_list =
  check_lists cur algop_get_accessed_fields (i_list @ d_list)

let print_free_variable_desc ff algop =
  let has_changed algop =
    let fv_list = algop_get_free_variables  algop in
    let tf_list = algop_get_accessed_fields algop in
    let i_list  = subexpr_to_list algop.psub_expression in
    let d_list  = subexpr_to_list algop.pdep_sub_expression in
    (free_var_changed fv_list i_list d_list)
  || (tuple_fields_changed tf_list i_list d_list)
  in
  let empty_fvd op =
    ((algop_get_free_variables op) = []) &&
    ((algop_get_accessed_fields op) = []) &&
    ((algop_get_returned_fields op) = []) &&
    ((algop_get_bound_use_counts op) = [])
  in
  if ((not (empty_fvd algop)) &&
      (has_changed algop)) || true then
    begin
      let (tuple_field_use_counts, candidate_fields, cardinality) = algop_get_tuple_field_use_counts algop in
	fprintf ff "@[|%a%a%a%a%a%a%a|@]@;"
	  print_free_vars (algop_get_free_variables algop)
	  print_bound_vars      (algop_get_bound_use_counts algop)
	  print_accessed_fields (algop_get_accessed_fields algop)
	  print_returned_fields (algop_get_returned_fields algop)
	  print_tuple_field_use_counts tuple_field_use_counts
	  print_candidate_fields candidate_fields
	  print_cardinality cardinality
    end

let print_group_desc ff gd =
  match (get_aggregate_type gd) with
  | None ->
      fprintf ff "[%a][%a][%a]"
	print_var_list (Xquery_algebra_ast_util.get_group_names gd)
	print_var_list (Xquery_algebra_ast_util.get_valid_names gd)
	print_rqname (Xquery_algebra_ast_util.get_aggregate_name gd)
  | Some dt ->
      fprintf ff "[%a][%a][%a(%a)]"
	print_var_list (Xquery_algebra_ast_util.get_group_names gd)
	print_var_list (Xquery_algebra_ast_util.get_valid_names gd)
	print_rqname (Xquery_algebra_ast_util.get_aggregate_name gd)
	print_asequencetype dt

let print_http_method ff hm =
  match hm with
  | Galax_url.File str ->
      fprintf ff "%s" ("\"" ^ str ^ "\"")
  | Galax_url.Http (str1, i, str2) ->
      fprintf ff "%s" ("\"" ^ str1 ^ (string_of_int i) ^ str2 ^ "\"")
  | Galax_url.ExternalSource (str1, str2, opt_str, str4) ->
      begin
	let str3 = match opt_str with
	| None -> ""
	| Some str -> str
	in
	fprintf ff "%s" ("\"" ^ str1 ^ str2 ^ str3 ^ str4 ^ "\"")
      end

let print_ordered idname print_algop ff deps = 
  let apply v = fprintf ff "%a" (print_algop idname 0) v in
  Array.iter apply deps

let print_tuple_create idname print_algop ff (rnames,exprs) =
  begin
    if ((Array.length rnames) != (Array.length exprs))
    then
      raise (Query
	       (Malformed_Tuple
		  ("Number of tuple slots does not equal number of expressions")))
  end;
  begin
    for i = 0 to (Array.length rnames) - 1 do
      let printsemi ff i =
	if (i == (Array.length rnames -1))
	then
	  fprintf ff "%a"
	    (print_algop idname 0) exprs.(i)
	else
	  fprintf ff "%a;@ "
	    (print_algop idname 0) exprs.(i)
      in
      if (fst rnames.(i)) = None
      then
	fprintf ff "@[<hv 2>%a :@;%a@]"
	  print_rqname (snd rnames.(i))
	  printsemi i
      else
	fprintf ff "@[<hv 2>%a%a :@;%a@]"
	  print_rqname (snd rnames.(i))
	  print_optasequencetype (fst rnames.(i))
	  printsemi i
    done
  end

let rec print_params idname print_algop ff es =
  match es with
  | e :: []  ->
      fprintf ff "%a" (print_algop idname 0) e
  | e :: es  ->
      fprintf ff "%a,@ %a" (print_algop idname 0) e (print_params idname print_algop) es
  | [] -> 
      ()

let print_coptvariable_as ff ovn =
  match ovn with
  | None -> ()
  | Some vn -> fprintf ff "$%a as " print_rqname vn

let print_coptvariable  ff ovn =
  match ovn with
  | None -> ()
  | Some vn -> fprintf ff "$%a " print_rqname vn

let print_cpattern  ff (p, ovn) =
  match p.papattern_desc with
  | ACase m ->
      fprintf ff "case %a%a" print_coptvariable_as ovn print_asequencetype m
  | ADefault ->
      fprintf ff "default %a" print_coptvariable ovn

let print_cases print_algop idname ff (pattern,exprs) =
  if ((Array.length pattern) != (Array.length exprs)) then
    raise (Query
	     (Malformed_Expr
		("Number of cases does not match the number of expressions")));
  for i = 0 to (Array.length pattern)-1 do      
    fprintf ff "%a return@;<1 2>%a@;"
      print_cpattern pattern.(i)
      (print_algop idname 0) exprs.(i)
  done

let print_aoeelem_contents idname print_algop ff ecs =
  for i = 0 to (Array.length ecs) - 1 do
    fprintf ff "%a" (print_algop idname 0) ecs.(i)
  done

let print_group_by ff gds =
  List.iter (fun gd ->
    fprintf ff "%a"
      print_group_desc gd) gds

let print_dep idname print_algop ff algop =
    fprintf ff "{%a}@," (print_algop idname 0) algop

let print_many_deps idname print_algop ff algops =
  Array.iter (print_dep idname print_algop ff) algops

let rec print_pred_desc idname p ops print_algop ff (pred_desc:predicate_desc) =
  match pred_desc with
  | SimpleConjunct (start_i,end_i) ->
      begin
	fprintf ff "@[<hov 1>";
	for i = start_i to end_i - 1 do
	  fprintf ff "%a@;<1 0>and@;<1 0>" (print_algop idname 3) ops.(i)	   
	done;
	fprintf ff "%a" (print_algop idname 3) ops.(end_i);
	fprintf ff "@]"
      end
  | ComplexConjunct (l,r) ->
      if p > 3
      then
	fprintf ff "@[<hv 1>(@[<hov 1>%a@;<1 0>and@;<1 0>%a@])@]"
	  (print_pred_desc idname 3 ops print_algop) l
	  (print_pred_desc idname 3 ops print_algop) r
      else
	fprintf ff "@[<hov 1>%a@;<1 0>and@;<1 0>%a@]"
	  (print_pred_desc idname 3 ops print_algop) l
	  (print_pred_desc idname 3 ops print_algop) r
  | Disjunct (l,r) ->
      if p > 2
      then
	fprintf ff "@[<hv 1>(@[<hov 1>%a@;<1 0>or@;<1 0>%a@])@]"
	  (print_pred_desc idname 2 ops print_algop) l
	  (print_pred_desc idname 2 ops print_algop) r
      else
	fprintf ff "@[<hov 1>%a@;<1 0>or@;<1 0>%a@]"
	  (print_pred_desc idname 2 ops print_algop) l
	  (print_pred_desc idname 2 ops print_algop) r

let print_twig_node_test ff ont =
  match ont with
  | Some nt -> fprintf ff "%a" print_anode_test nt
  | None    -> fprintf ff "."

let print_out_field ff twignode =
  match twignode.out with
  | Some f ->
      begin
	if twignode.restore_out
	then fprintf ff "{%a*}" print_rqname f
	else fprintf ff "{%a}" print_rqname f
      end
  | None ->
      fprintf ff ""

let print_sbdo ff twignode =
(*  if Debug.default_debug() then *)
    begin
      let (sigma,delta) = twignode.requires_sbdo in
      let msg = (
	"(" ^
	(if sigma && delta then "s,d"
	else if delta then "d"
	else if sigma then "s"
	else "") ^ ")"
       )
      in
      (* Debug.print_default_debug msg *)
      fprintf ff "%s" msg
    end

let rec print_child_node pattern ff child_twig =
  match child_twig with
  | Some (typ, ind) ->
      fprintf ff "/%a::%a"
	print_axis typ
	(print_twig_node_aux pattern) ind
  | None -> fprintf ff ""
and print_predicates pattern ff predlist =
  match predlist with
  | hd::tl -> 
      let (typ, ind) = hd in
      fprintf ff "[./%a::%a]%a" 
	print_axis typ
	(print_twig_node_aux pattern) ind
	(print_predicates pattern) tl
  | [] ->
      fprintf ff ""
and print_twig_node_aux pattern ff index =
  fprintf ff "%a%a%a%a%a"
    print_twig_node_test pattern.(index).node_test
    print_out_field pattern.(index)
    print_sbdo pattern.(index)
    (print_predicates pattern) pattern.(index).pred_twigs
    (print_child_node pattern) pattern.(index).child_twig

let print_twig_node ff pattern =
  print_twig_node_aux pattern ff 0

let print_physical_signature ff algop_sig =
  match algop_sig with
  | None -> ()
  | Some x ->
      fprintf ff "(: %s :)@\n" (Print_xquery_physical_type.string_of_eval_sig x)

let print_algop_main idname physical p print_algop ff algop  =
  begin
    if !Conf.bPrinting_comp_annotations
    then print_free_variable_desc ff algop
  end;
  begin
    if physical
    then
      print_physical_signature ff algop.palgop_expr_eval_sig
  end;
  match algop.palgop_expr_name with
  | AOELetvar (odt,vn) ->
      let indep = access_onesub algop.psub_expression in
      let dep   = access_onesub algop.pdep_sub_expression in
      if p > 1
      then
	fprintf ff
	  "@[<hv 1>(@[<hv 2>letvar $%a%a :=@ %a@]@;<1 0>@[<hv 2>return@;<1 0>%a@])@]"
	  print_rqname vn
	  print_optasequencetype odt
	  (print_algop idname 1) indep
	  (print_algop idname 1) dep
      else
	fprintf ff
	  "@[<hv 0>@[<hv 2>letvar $%a%a :=@ %a@]@;<1 0>@[<hv 2>return@;<1 0>%a@]@]"
	  print_rqname vn
	  print_optasequencetype odt
	  (print_algop idname 1) indep
	  (print_algop idname 1) dep
  | AOEIf ->
      let e1 = access_onesub algop.psub_expression in
      let e2,e3 = access_twosub algop.pdep_sub_expression in
      if p > 1
      then
	fprintf ff
      "@[<hv 1>(if (%a)@;<1 0>@[<hv 2>then@;<1 0>%a@]@;<1 0>@[<hv 2>else@;<1 0>%a@])@]"
	  (print_algop idname 0) e1
	  (print_algop idname 1) e2
	  (print_algop idname 1) e3
      else
	fprintf ff
      "@[<hv 0>if (%a)@;<1 0>@[<hv 2>then@;<1 0>%a@]@;<1 0>@[<hv 2>else@;<1 0>%a@]@]"
	  (print_algop idname 0) e1
	  (print_algop idname 1) e2
	  (print_algop idname 1) e3
  | AOEWhile ->
      let e1,e2 = access_twosub algop.pdep_sub_expression in
      if p > 1
      then
	fprintf ff
      "@[<hv 1>(while (%a)@;<1 0>@[<hv 2>return@;<1 0>%a@])@]"
	  (print_algop idname 0) e1
	  (print_algop idname 1) e2
      else
	fprintf ff
      "@[<hv 0>while (%a)@;<1 0>@[<hv 2>return@;<1 0>%a@]@]"
	  (print_algop idname 0) e1
	  (print_algop idname 1) e2
  | AOETypeswitch pattern ->
      let e' = access_onesub algop.psub_expression in
      let cases = access_manysub algop.pdep_sub_expression in
      if p > 1
      then
        fprintf ff "@[<hv 1>(@[<hv 2>@[<hv 2>typeswitch (@,%a@;<0 -2>)@]@;<1 0>%a@])@]"
	  (print_algop idname 0) e'
	  (print_cases print_algop idname) (pattern,cases)
      else
	fprintf ff "@[<hv 2>@[<hv 2>typeswitch (@,%a@;<0 -2>)@]@;<1 0>%a@]"
	  (print_algop idname 0) e'
	  (print_cases print_algop idname) (pattern,cases)
  | AOEVar v ->
      if Namespace_names.rqname_equal v idname then
	fprintf ff "ID"
      else
	fprintf ff "$%a" print_rqname v
  | AOEScalar bv ->
      fprintf ff "%a" print_literal bv
  | AOEText t ->
      fprintf ff "text { \"%s\" }" t
  | AOECharRef i ->
      fprintf ff "@[<hv 2>text {@,\"&#%i;\"@;<0 -2>}@]" i
  | AOETextComputed  ->
      let e = access_onesub algop.psub_expression in
      fprintf ff "@[<hv 2>text {@,\"%a\"@;<0 -2>}@]"
	(print_algop idname 0) e
  | AOEPI (target,pi_content) ->
      fprintf ff "<?%s %s?>" target pi_content
  | AOEPIComputed ->
      let e1,e2 = access_twosub algop.psub_expression in
      fprintf ff "@[<hv 2>processing-instruction {@,%a@;<0 -2>} {@,%a@;<0 -2>}@]"
	(print_algop idname 0) e1
	(print_algop idname 0) e2
  | AOEComment c ->
      fprintf ff "<!--%s-->" c
  | AOECommentComputed  ->
      let e = access_onesub algop.psub_expression in
      fprintf ff "@[<hv 2>comment {@,%a@;<0 -2>}@]"
	(print_algop idname 0) e
  | AOEDocument  ->
      let e = access_onesub algop.psub_expression in
      fprintf ff "@[<hv 2>document {@,%a@;<0 -2>}@]"
	(print_algop idname 0) e
  | AOECallUserDefined ((name,arity), optintypes, opttypes, _, _)
  | AOECallBuiltIn ((name,arity), optintypes, opttypes, _) ->
      let params = Array.to_list (access_manysub algop.psub_expression) in
(*       let is_upd = updating_flag_to_string upd in *)
      fprintf ff  "@[<hv 2>%a(@,%a@;<0 -2>)@]"
	print_rqname name 
	(print_params idname print_algop) params
  | AOECallOverloaded ((name, arity),table) ->
      let params = Array.to_list (access_manysub algop.psub_expression) in
      fprintf ff "@[<hv 2>%a(@,%a@;<0 -2>)@]"
	print_rqname name
	(print_params idname print_algop) params
  | AOEConvertSimple atomic_type ->
      let name = Namespace_builtin.fs_convert_simple_operand in
      let param1 = access_onesub algop.psub_expression in
      let s = string_of_proto_value atomic_type in
      let params = [param1] in
      fprintf ff  "@[<hv 2>%a(@,%a,@ %s@;<0 -2>)@]"
	print_rqname name
	(print_params idname print_algop) params
	s
  | AOEPromoteNumeric atomic_type ->
      let name = Namespace_builtin.fs_promote_to_numeric in
      let param1 = access_onesub algop.psub_expression in
      let s = string_of_proto_value atomic_type in
      let params = [param1] in
      fprintf ff  "@[<hv 2>%a(@,%a,@ %s@;<0 -2>)@]"
	print_rqname name
	(print_params idname print_algop) params
	s
  | AOEPromoteAnyString ->
      let name = Namespace_builtin.fs_promote_to_anystring in
      let param1 = access_onesub algop.psub_expression in
      let params = [param1] in
      fprintf ff  "@[<hv 2>%a(@,%a@;<0 -2>)@]"
	print_rqname name
	(print_params idname print_algop) params
  | AOEUnsafePromoteNumeric atomic_type ->
      let name = Namespace_builtin.fs_unsafe_promote_to_numeric in
      let param1 = access_onesub algop.psub_expression in
      let s = string_of_proto_value atomic_type in
      let params = [param1] in
      fprintf ff  "@[<hv 2>%a(@,%a,@ %s@;<0 -2>)@]"
	print_rqname name
	(print_params idname print_algop) params
	s
  | AOESeq  ->
      let e1,e2 = access_twosub algop.psub_expression in
      if p > 0 then
	fprintf ff "@[<hov 1>(%a,@ %a)@]"
	  (print_algop idname 0) e1
	  (print_algop idname 0) e2
      else
	fprintf ff "@[<hov 0>%a,@ %a@]"
	  (print_algop idname 0) e1
	  (print_algop idname 0) e2
  | AOEImperativeSeq  ->
      let e1,e2 = access_twosub algop.psub_expression in
      if p > 0 then
	fprintf ff "@[<hov 1>@,%a;@ %a@,@]"
	  (print_algop idname 0) e1
	  (print_algop idname 0) e2
      else
	fprintf ff "@[<hov 0>%a;@ %a@]"
	  (print_algop idname 0) e1
	  (print_algop idname 0) e2
  | AOEEmpty ->
      fprintf ff "()"
  | AOEElem (l,nsenv) ->
      let el = access_manysub algop.psub_expression in
      begin
	match (Array.length el) with 
	| 0 ->
	    fprintf ff "@[<hv 2>element %a {}@]"
	      print_symbol l 
	| _ ->
	    fprintf ff "@[<hv 2>element %a {@,%a@;<0 -2>}@]"
	      print_symbol l
	      (print_aoeelem_contents idname print_algop) el
      end
  | AOEAnyElem (nsenv1,nsenv2) ->
      let e1, e2 = access_twosub algop.psub_expression in 
      begin
	match e2.palgop_expr_name with
	| AOEEmpty ->
	    fprintf ff "@[<hv 2>element {@,%a@;<0 -2>} {}@]"
	      (print_algop idname 0) e1
	| _ ->
	    fprintf ff "@[<hv 2>element {@,%a@;<0 -2>} {@,%a@;<0 -2>}@]"
	      (print_algop idname 0) e1
	      (print_algop idname 0) e2
      end
  | AOEAttr (l,nsenv) ->
      let el = access_manysub algop.psub_expression in
      begin
	match (Array.length el) with 
	| 0 ->
	    fprintf ff "@[<hv 2>attribute %a {}@]"
	      print_symbol l
	| _ ->
	    fprintf ff "@[<hv 2>attribute %a {@,%a@;<0 -2>}@]"
	      print_symbol l
	      (print_aoeelem_contents idname print_algop) el
      end
  | AOEAnyAttr nsenv ->
      let e1, e2 = access_twosub algop.psub_expression in 
      begin
	match e2.palgop_expr_name with
	| AOEEmpty ->
	    fprintf ff "@[<hv 2>attribute {@,%a@;<0 -2>} {}@]"
	      (print_algop idname 0) e1
	| _ ->
	    fprintf ff "@[<hv 2>attribute {@,%a@;<0 -2>} {@,%a@;<0 -2>}@]"
	      (print_algop idname 0) e1
	      (print_algop idname 0) e2
      end
  | AOEError  ->
      let params = Array.to_list (access_manysub algop.psub_expression) in
      fprintf ff "@[<hv 2>%a(@,%a@;<0 -2>)@]"
	print_rqname Namespace_builtin.fn_error
	(print_params idname print_algop) params
  | AOETreat cdt ->
      let e' = access_onesub algop.psub_expression in
      if p > 11
      then
	fprintf ff "@[<hv 1>(@[<hv 2>%a treat as %a@])@]"
	  (print_algop idname 11) e'
	  print_asequencetype cdt
      else
	fprintf ff "@[<hv 2>%a treat as %a@]"
	  (print_algop idname 11) e'
	  print_asequencetype cdt
  | AOEValidate vmode ->
      let e' = access_onesub algop.psub_expression in
      fprintf ff "@[<hv 2>validate %a@ {@,%a@;<0 -2>}@]"
	print_validation_mode vmode
	(print_algop idname 0) e'
  | AOECast (nsenv, cdt) ->
      let e' = access_onesub algop.psub_expression in
      if p > 13
      then
	fprintf ff "@[<hv 1>(@[<hv 2>%a@ cast as %a@])@]"
	  (print_algop idname 13) e'
	  print_asequencetype cdt
      else
	fprintf ff "@[<hv 2>%a@ cast as %a@]"
	  (print_algop idname 13) e'
	  print_asequencetype cdt
  | AOECastable (nsenv, cdt) ->
      let e' = access_onesub algop.psub_expression in
      if p > 12
      then
	fprintf ff "@[<hv 1>(@[<hv 2>%a@ castable as %a@])@]"
	  (print_algop idname 12) e'
	  print_asequencetype cdt
      else
	fprintf ff "@[<hv 2>%a@ castable as %a@]"
	  (print_algop idname 12) e'
	  print_asequencetype cdt
  | AOESome (odt,vn) ->
      let ae1 = access_onesub algop.psub_expression in
      let ae2 = access_onesub algop.pdep_sub_expression in
      if p > 1
      then
	fprintf ff "@[<hv 1>(@[<hv 0>@[<hv 5>some $%a%a@ in %a@]@ @[<hv 2>satisfies@ %a@]@])@]"
	  print_rqname vn
	  print_optasequencetype odt
	  (print_algop idname 0) ae1
	  (print_algop idname 1) ae2
      else
	fprintf ff "@[<hv 0>@[<hv 5>some $%a%a@ in %a@]@ @[<hv 2>satisfies@ %a@]@]"
	  print_rqname vn
	  print_optasequencetype odt
	  (print_algop idname 0) ae1
	  (print_algop idname 1) ae2
  | AOEEvery (odt,vn) ->
      let ae1 = access_onesub algop.psub_expression in
      let ae2 = access_onesub algop.pdep_sub_expression in 
      if p > 1
      then
	fprintf ff "@[<hv 1>(@[<hv 0>@[<hv 5>every $%a%a@ in %a@]@ @[<hv 2>satisfies@ %a@]@])@]"
	  print_rqname vn
	  print_optasequencetype odt
	  (print_algop idname 0) ae1
	  (print_algop idname 1) ae2
      else
	fprintf ff "@[<hv 0>@[<hv 5>every $%a%a@ in %a@]@ @[<hv 2>satisfies@ %a@]@]"
	  print_rqname vn
	  print_optasequencetype odt
	  (print_algop idname 0) ae1
	  (print_algop idname 1) ae2
  (* DXQ *)
  | AOEServerImplements (ncname,uri) -> 
      let e1 = access_onesub algop.psub_expression in 
      let e2 = access_onesub algop.pdep_sub_expression in 
      fprintf ff "@[<hv 0>let server@ %s@ implement@ %s@ @[<hv 2>at@ %a@] @[<hv 2>return@;<1 0>{%a}@]@]" ncname uri (print_algop idname 0) (e1) (print_algop idname 0) (e2)

  | AOEForServerClose (ncname,uri) -> 
      (* Handle two cases here : before code selection there is 1
         subexpressions, afterwards there is none, so do a match here :
      *)
      begin
	match algop.psub_expression with
	| NoSub -> 
	    fprintf ff "@[<hv 0>for server@ %s@ @[<hv 2>closed@;@]@]" ncname 
	| OneSub (e1) -> 
	    fprintf ff "@[<hv 0>for server@ %s@ @[<hv 2>close@;<1 0>{%a}@]@]" ncname (print_algop idname 0) (e1)
	| _  -> raise (Query(Internal_Error("Invalid number of arguments to for-server-close")))
      end
  | AOEEvalClosure -> 
Debug.print_dxq_debug ("print_xquery_alg : Before eval-box access_onesub\n");
      begin
	match algop.psub_expression with
	| NoSub -> 
	    fprintf ff "@[<hv 0>eval box@ no_indep" 
	| OneSub (e1) -> 
	    fprintf ff "@[<hv 0>eval box@ @[<hv 2>{%a}@]@]" (print_algop idname 0) (e1)
	| _  -> raise (Query(Internal_Error("Invalid number of indep arguments to eval-closure")))
      end;
      begin
	match algop.pdep_sub_expression with
	| NoSub -> 
	    fprintf ff "no_dep" 
	| OneSub (e1) -> 
	    fprintf ff "@[<hv 0>@[<hv 2>{%a}@]@]" (print_algop idname 0) (e1)
	| _  -> raise (Query(Internal_Error("Invalid number of dep arguments to eval-closure")))
      end;
Debug.print_dxq_debug ("print_xquery_alg : After eval-box access_onesub\n");
(*
      let e1 = access_onesub algop.psub_expression in 
      fprintf ff "@[<hv 0>eval closure@ {%a}@]" (print_algop idname 0) (e1) *)

  | AOEExecute (ncname, uri) ->
      (* Handle two cases here : before code selection there are 2
         subexpressions, afterwards there is one, so do a match here :
      *)
      begin
	match algop.psub_expression with
	| OneSub (e1) -> 
	    fprintf ff "from server@ %s@ at@,{%a}" ncname (print_algop idname 0) (e1) 
	| TwoSub (e1,e2) -> 
	    let (expr1, expr2) = access_twosub algop.psub_expression in
	    fprintf ff "from server@ %s@ at@,{%a} return@,{%a}" ncname (print_algop idname 0) (expr1) (print_algop idname 0) (expr2)
	| _  -> raise (Query(Internal_Error("Invalid number of arguments to Execute")))
      end
  | AOEASyncExecute (ncname, uri) ->
      (* Handle two cases here : before code selection there are 2
         subexpressions, afterwards there is one, so do a match here :
      *)
      begin
	match algop.psub_expression with
	| OneSub (e1) -> 
	    fprintf ff "at server@ %s@ at@,{%a}" ncname (print_algop idname 0) (e1) 
	| TwoSub (e1,e2) -> 
	    let (expr1, expr2) = access_twosub algop.psub_expression in
	    fprintf ff "at server@ %s@ at@,{%a} do@,{%a}" ncname (print_algop idname 0) (expr1) (print_algop idname 0) (expr2)
	| _  -> raise (Query(Internal_Error("Invalid number of arguments to Execute")))
      end

  (*******************)
  (* Tuple Operators *)
  (*******************)
  | AOECreateTuple rname_array ->
      let aes = access_manysub algop.psub_expression in
      fprintf ff "@[<h 2>[@,%a@;<0 -2>]@]"
	(print_tuple_create idname print_algop)
	(rname_array, aes)
  | AOEAccessTuple (rname) ->
      fprintf ff "#%a" print_rqname rname
  | AOEProject rname_array ->
      let print_rqname_list ff l =
	let rname_string = String.concat ", " 
	    (List.map prefixed_string_of_rqname (Array.to_list l)) in
	fprintf ff "%s" rname_string
      in
      let op3 = access_onesub algop.psub_expression in
      fprintf ff "@[<hv 2>Project[(%a)](@,%a@;<0 -2>)@]"
	print_rqname_list rname_array
	(print_algop idname 0) op3
  | AOEMapToItem ->
      let op3 = access_onesub algop.psub_expression in
      let op1 = access_onesub algop.pdep_sub_expression in
      fprintf ff "@[<hv 2>Map@,{%a}@,(%a)@]"
	(print_algop idname 0) op1
	(print_algop idname 0) op3
  | AOEMap  ->
      let op3 = access_onesub algop.psub_expression in
      let op1 = access_onesub algop.pdep_sub_expression in
      fprintf ff "@[<hv 2>Map@,{%a}@,(%a)@]"
	(print_algop idname 0) op1
	(print_algop idname 0) op3
  | AOEMapIndex index_name ->
      let op3 = access_onesub algop.psub_expression in
      fprintf ff "@[<hv 2>MapIndex[(%a)](@,%a@;<0 -2>)@]"
	print_rqname index_name
	(print_algop idname 0) op3
  | AOEMapIndexStep index_name ->
      let op3 = access_onesub algop.psub_expression in
      fprintf ff "@[<hv 2>MapIndexStep[(%a)](%a)@]"
	print_rqname index_name
	(print_algop idname 0) op3
  | AOENullMap vn ->
      let op3 = access_onesub algop.psub_expression in
      fprintf ff "@[<hv 1>NullMap[(%a)](%a)@]"
	print_rqname vn
	(print_algop idname 0) op3
  | AOEProduct ->
      let l, r = access_twosub algop.psub_expression in
      let _    = access_nosub  algop.pdep_sub_expression in
      fprintf ff "@[<hv 1>Product(%a,@,%a)@]"
	(print_algop idname 0) l
	(print_algop idname 0) r
  | AOESelect pred_desc ->
      let input = access_onesub  algop.psub_expression in
      let pred  = access_manysub algop.pdep_sub_expression in
      fprintf ff "@[<hv 2>Select@,{%a}@,@[<hv 1>(%a)@]@]"
	(print_pred_desc idname 0 pred print_algop) pred_desc
	(print_algop idname 0) input
  | AOEOrderBy(stable, sort_kind_empty_list,_) ->
      let op3  = access_onesub  algop.psub_expression in
      let deps = access_manysub algop.pdep_sub_expression in
      fprintf ff "@[<hv 2>OrderBy@,{%a}@,@[<hv 1>(%a)@]@]"
	(print_ordered idname print_algop) deps
	(print_algop idname 0) op3
  | AOEConcatTuples ->
      let t1, t2 = access_twosub algop.psub_expression in
      let _      = access_nosub  algop.pdep_sub_expression in
      if p > 9
      then
	fprintf ff "@[<hv 1>(@[<hov 1>%a@;<1 0>@[<hov 1>++@;<1 0>%a@]@])@]"
	  (print_algop idname 9) t1
	  (print_algop idname 9) t2
      else
	fprintf ff "@[<hov 1>%a@;<1 0>@[<hov 1>++@;<1 0>%a@]@]"
	  (print_algop idname 9) t1
	  (print_algop idname 9) t2
  | AOEMapFromItem vname ->
      let op3 = access_onesub algop.psub_expression in
      let op1 = access_onesub algop.pdep_sub_expression in
      fprintf ff "@[<hv 2>Map@,@[<hv 1>{%a}@]@,@[<hv 1>(%a)@]@]"
	(print_algop vname 0) op1
	(print_algop idname 0) op3
  | AOEInputTuple ->
      let _ = access_nosub algop.psub_expression in
      let _ = access_nosub algop.pdep_sub_expression in
      fprintf ff "ID"
  | AOEMapConcat ->
      let op3 = access_onesub algop.psub_expression in
      let op1 = access_onesub algop.pdep_sub_expression in
      fprintf ff "@[<hv 2>MapConcat@,@[<hv 1>{%a}@]@,@[<hv 1>(%a)@]@]"
	(print_algop idname 0) op1
	(print_algop idname 0) op3
  | AOEOuterMapConcat vn ->
      let op3 = access_onesub algop.psub_expression in
      let op1 = access_onesub algop.pdep_sub_expression in
      fprintf ff "@[<hv 2>OMapConcat[(%a)]@,@[<hv 1>{%a}@]@,@[<hv 1>(%a)@]@]"
	print_rqname vn
	(print_algop idname 0) op1
	(print_algop idname 0) op3
  | AOEJoin pred_desc ->
      let l,r  = access_twosub  algop.psub_expression in
      let pred = access_manysub algop.pdep_sub_expression in
      fprintf ff "@[<hv 2>Join@,@[<hv 1>{%a}@]@,@[<hv 1>(%a,@,%a)@]@]"
	(print_pred_desc idname 0 pred print_algop) pred_desc
	(print_algop idname 0) l
	(print_algop idname 0) r
  | AOELeftOuterJoin (v,pred_desc) ->
      let l,r  = access_twosub algop.psub_expression in
      let pred = access_manysub algop.pdep_sub_expression in
      fprintf ff "@[<hv 2>LeftOuterJoin[(%a)]@,@[<hv 1>{%a}@]@,@[<hv 1>(%a,@,%a)@]@]"
	print_rqname v
	(print_pred_desc idname 0 pred print_algop) pred_desc
	(print_algop idname 0) l
	(print_algop idname 0) r
  | AOEGroupBy gd_list ->
      let input = access_onesub algop.psub_expression in
      let many  = access_manysub algop.pdep_sub_expression in
      fprintf ff "@[<hv 2>GroupBy%a@,%a@[<hv 1>(%a)@]@]"
	print_group_by gd_list
	(print_many_deps idname print_algop) many
	(print_algop idname 0) input

  (********************)
  (* Update Operators *)
  (********************)
  | AOECopy ->
      let a1 = access_onesub algop.psub_expression in
      let _  = access_nosub  algop.pdep_sub_expression in
      fprintf ff "@[<hv 2>Copy@[<hv 1>(%a)@]@]"
	(print_algop idname 0) a1
  | AOEDelete ->
      let a1 = access_onesub algop.psub_expression in
      let _  = access_nosub  algop.pdep_sub_expression in
      fprintf ff "@[<hv 2>Delete@[<hv 1>(%a)@]@]"
	(print_algop idname 0) a1
  | AOEInsert loc ->
      let a1,a2 = access_twosub algop.psub_expression in
      let _     = access_nosub algop.pdep_sub_expression in
      fprintf ff "@[<hv 2>Insert[%a]@[<hv 1>(%a,@,%a)@]@]"
	print_algop_insert_loc loc
	(print_algop idname 0) a1
	(print_algop idname 0) a2
  | AOERename nsenv ->
      let a1, a2 = access_twosub algop.psub_expression in
      let _      = access_nosub algop.pdep_sub_expression in
      fprintf ff "@[<hv 2>Rename@[<hv 1>(%a,@,%a)@]@]"
	(print_algop idname 0) a1
	(print_algop idname 0) a2
  | AOEReplace valueflag ->
      let a1, a2 = access_twosub algop.psub_expression in
      let _      = access_nosub algop.pdep_sub_expression in
      fprintf ff "@[<hv 2>Replace[%a]@[<hv 1>(%a,@,%a)@]@]"
	print_algop_value_of_flag valueflag
	(print_algop idname 0) a1
	(print_algop idname 0) a2
  | AOESnap sm ->
      let a1  = access_onesub algop.pdep_sub_expression in
      let _   = access_nosub algop.psub_expression in
      fprintf ff "@[<hv 2>Snap[%a]@[<hv 1>(%a)@]@]"
	print_snap_modifier sm
	(print_algop idname 0) a1

  | AOESet vn ->
      let a1 = access_onesub algop.psub_expression in
	  fprintf ff "@[<hv 2>Set[$%a]@[<hv 1>(%a)@]@]"
	  print_rqname vn
	  (print_algop idname 0) a1


  (********************)
  (* XPath evaluation *)
  (********************)
  | AOEParse uri ->
      fprintf ff "Parse(%s)" uri
  | AOETreeJoin (a, nt) ->
      let ae = access_onesub algop.psub_expression in
      fprintf ff "@[<hv 2>TreeJoin[%a::%a]@,@[<hv 1>(%a)@]@]"
	print_axis a
	print_anode_test nt
	(print_algop idname 0) ae
  | AOETupleTreePattern (input, pattern) ->
      let a1 = access_onesub algop.psub_expression in
      let _  = access_nosub  algop.pdep_sub_expression in
      fprintf ff "@[<hv 2>TupleTreePattern[(%a),(%a)]@,@[<hv 1>(%a)@]@]"
	print_rqname input
	print_twig_node pattern
	(print_algop idname 0) a1
(*
  | AOEPrune (name, a) ->
      let a1 = access_onesub algop.psub_expression in
      let _  = access_nosub  algop.pdep_sub_expression in
      fprintf ff "@[<hv 2>Prune[%a,%a]@,@[<hv 1>(%a)@]@]"
	print_rqname name
	print_axis a
	(print_algop idname 0) a1
  | AOEDistinct name ->
      let a1 = access_onesub algop.psub_expression in
      let _  = access_nosub  algop.pdep_sub_expression in
      fprintf ff "@[<hv 2>Distinct[%a]@,@[<hv 1>(%a)@]@]"
	print_rqname name
	(print_algop idname 0) a1
*)

(* check_signatures *)
let msg_constructor msg x =
  if x = ""
  then []
  else [msg ^ x]

let print_physical_algop ff algop =
  let rec print_physical_algop_prec idname p ff algop =
    (* This can only be called if the signatures are filled in *)
    let _ =
      check_signatures msg_constructor algop.palgop_expr_eval_sig algop.psub_expression
    in
    (print_algop_main idname true) p print_physical_algop_prec ff algop
  in
  print_physical_algop_prec Xquery_common_ast_util.bogus_cvname 0 ff algop

let rec print_logical_algop ff (algop: ('a,'b) Xquery_algebra_ast.aalgop_expr) =
  let rec print_logical_algop idname p ff algop =
    (print_algop_main idname false) p print_logical_algop ff algop
  in
  print_logical_algop Xquery_common_ast_util.bogus_cvname 0 ff algop


(*******************)
(* Core Statements *)
(*******************)

(* Statements *)

let print_algstatement print_algop ff s =
  fprintf ff "@[%a@]" print_algop s

let rec print_algstatements print_algop ff ss =
  let print_algstatement' ff s = print_algstatement print_algop ff s in
  let print_algstatements' ff s = print_algstatements print_algop ff s in
  match ss with
  | s :: [] ->
      fprintf ff "%a;@\n" print_algstatement' s
  | s :: ss' ->
      fprintf ff "%a;@\n%a" print_algstatement' s print_algstatements' ss'
  | [] ->
      ()


(***************)
(* Core Module *)
(***************)

(* Function definitions *)

let print_algprolog print_algop ff qm =
  let rec print_cfunction_def ff fd =
    let print_fn_body ff body =
      match !(body.palgop_func_optimized_logical_plan) with
      |	AOEFunctionImported -> ()
	(* What do we print here?  The optimized logical plan or the physical plan? *)
      |	AOEFunctionUser userbody -> 
	  print_algop ff userbody
    in
    let rec print_intypes ff t =
      match t with
      | [] -> ()
      | t' :: [] ->
	  fprintf ff "%a" print_asequencetype t'
      | t' :: rest ->
	  fprintf ff "%a,%a" print_asequencetype t' print_intypes rest
    in
    let ((fn,arity), (dts,odt), fbody, upd) = fd.palgop_function_decl_desc in
    let upd_flag = updating_flag_to_string upd in
      fprintf ff
	"@[<hv 2>@[<hv 2>declare %sfunction@ %a(@,%a@;<0 -2>)@] as %a {@,%a@;<0 -2>};@]"
	upd_flag
	print_rqname fn
	print_intypes dts
	print_asequencetype odt
	print_fn_body fbody

  (* MF: pretty printing is very finicky. The @, below is essential to
     getting good line breaks between function definitions *)
  and print_cfunction_defs ff fds =
    match fds with
    | [] ->
	()
    | fd :: fds' ->
	fprintf ff "@[%a@,@]@\n%a" print_cfunction_def fd print_cfunction_defs fds'

  (* Variable declaration *)
  and print_decl ff vd =
    match vd.alg_decl_name with
    | AOEVarDecl (odt, vn) ->
	let sub = access_onesub vd.alg_decl_indep in 
	let _ = access_nosub vd.alg_decl_dep in
	fprintf ff "declare variable@ $%a%a { @[%a@]@ };"
	  print_rqname vn
	  print_optasequencetype odt
	  print_algop sub
    | AOEVarDeclImported (odt, vn) 
    | AOEVarDeclExternal (odt, vn) -> 
	let _ = access_nosub vd.alg_decl_indep in 
	let _ = access_nosub vd.alg_decl_dep in
	fprintf ff "declare variable@ $%a%a external;"
	  print_rqname vn
	  print_optasequencetype odt 
    | AOEValueIndexDecl kn ->
	let a1 = access_onesub vd.alg_decl_indep in
	let a2 = access_onesub vd.alg_decl_dep   in
	fprintf ff "@[<hv 2>declare value index %s {@,%a@;<0 -2>} {@,%a@;<0 -2>};@]" 
	  kn
	  print_algop a1
	  print_algop a2
    | AOENameIndexDecl l ->
	let _ = access_nosub vd.alg_decl_indep in
	let _ = access_nosub vd.alg_decl_dep   in
	fprintf ff "@[<hv 2>declare name index %a;@]" 
	  print_symbol l

  and print_decls ff vds =
    match vds with
    | [] ->
	()
    | vd :: vds' ->
	fprintf ff "@[%a@]@\n%a"
	  print_decl vd
	  print_decls vds'

  (* The module itself *)
  in
  let print_algprolog' ff prolog =
    print_decls          ff prolog.palgop_prolog_vars;
    print_cfunction_defs ff prolog.palgop_prolog_functions;
    print_decls          ff prolog.palgop_prolog_indices;
  in
  if !Conf.print_prolog
  then print_algprolog' ff qm
  else ()

let print_algmodule print_annot ff qm =
  print_algprolog print_annot ff qm.palgop_module_prolog;
  print_algstatements print_annot ff qm.palgop_module_statements

(* 2006-2-15 CAR - I added flushes to each of these functions as
                   the pretty printer in ocaml 3.09.1 does not do it
						 when the formatter is closed.
*)

let print_logical_algstatement ff s =
  (print_algstatement print_logical_algop) ff s;
  fprintf ff "@?"

let print_physical_algstatement ff s = 
  print_algstatement print_physical_algop ff s;
  fprintf ff "@?"

let print_logical_algprolog ff s =
  print_algprolog print_logical_algop ff s;
  fprintf ff "@?"

let print_physical_algprolog ff s =
  print_algprolog print_physical_algop ff s;
  fprintf ff "@?"

let print_logical_algmodule ff s =
  print_algmodule print_logical_algop ff s;
  fprintf ff "@?"

let print_physical_algmodule ff s =
  print_algmodule print_physical_algop ff s;
  fprintf ff "@?"


(* Exported *)
(* Logical Statements *)
let fprintf_logical_algstatement f s st = 
  fprintf_stub f s print_logical_algstatement st 
let printf_logical_algstatement s st = printf_stub s print_logical_algstatement st
let bprintf_logical_algstatement s st = bprintf_stub s print_logical_algstatement st

(* Logical prolog *)
let fprintf_logical_algprolog f s st = fprintf_stub f s print_logical_algprolog st
let printf_logical_algprolog s qms = printf_stub s print_logical_algprolog qms
let bprintf_logical_algprolog s qms = bprintf_stub s print_logical_algprolog qms

(* Logical modules *)
let fprintf_logical_algmodule f s st = fprintf_stub f s print_logical_algmodule st
let printf_logical_algmodule s qms = printf_stub s print_logical_algmodule qms
let bprintf_logical_algmodule s qms = bprintf_stub s print_logical_algmodule qms

(* Optimized - still logical *)
(* Optimized Logical Statements *)
let fprintf_optimized_algstatement f s st = 
  fprintf_stub f s print_logical_algstatement st 
let printf_optimized_algstatement s st = printf_stub s print_logical_algstatement st
let bprintf_optimized_algstatement s st = bprintf_stub s print_logical_algstatement st

(* Optimized Logical prolog *)
let fprintf_optimized_algprolog f s st = fprintf_stub f s print_logical_algprolog st
let printf_optimized_algprolog s qms = printf_stub s print_logical_algprolog qms
let bprintf_optimized_algprolog s qms = bprintf_stub s print_logical_algprolog qms

(* Optimized Logical modules *)
let fprintf_optimized_algmodule f s st = fprintf_stub f s print_logical_algmodule st
let printf_optimized_algmodule s qms = printf_stub s print_logical_algmodule qms
let bprintf_optimized_algmodule s qms = bprintf_stub s print_logical_algmodule qms



(* Physical Statements *)
let fprintf_physical_algstatement f s st  = 
  fprintf_stub f s print_physical_algstatement st
let printf_physical_algstatement s st = printf_stub s print_physical_algstatement st
let bprintf_physical_algstatement s st = bprintf_stub s print_physical_algstatement st

(* Physical prolog *)
let fprintf_physical_algprolog f s st = fprintf_stub f s print_physical_algprolog st
let printf_physical_algprolog s qms = printf_stub s print_physical_algprolog qms
let bprintf_physical_algprolog s qms = bprintf_stub s print_physical_algprolog qms

(* Physical modules *)
let fprintf_physical_algmodule f s st = fprintf_stub f s print_physical_algmodule st
let printf_physical_algmodule s qms = printf_stub s print_physical_algmodule qms
let bprintf_physical_algmodule s qms = bprintf_stub s print_physical_algmodule qms

(* Node node *)

let printf_anode_test  s akt = printf_stub s print_anode_test akt
let bprintf_anode_test s akt = bprintf_stub s print_anode_test akt

