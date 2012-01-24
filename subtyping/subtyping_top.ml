(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: subtyping_top.ml,v 1.17 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Subtyping_top
   Description:
     This module implements top-level subtyping operations used in the
     rest of the system.
*)
   
open Error
open Occurrence

open Xquery_type_core_ast
open Xquery_type_core_ast_util
open Xquery_type_core_ast_annotation

open Schema_builtin
open Schema_util
open Schema_simplification

open Subtyping_letter
open Subtyping_glushkov
open Subtyping_build_regexp


(******************)
(* Errors & Debug *)
(******************)

let raise_includes_error exn smallModel bigModel =
  let msg =
    "Subtyping_gluskov.includes small = " 
    ^ (Print_top.bprintf_cxtype "" smallModel)
    ^ " big = "
    ^ (Print_top.bprintf_cxtype "" bigModel)
  in
  let error_msg = bprintf_error msg exn in
  raise (Query (Internal_Error error_msg))

let raise_intersects_error exn smallModel bigModel =
  let msg =
    "Subtyping_gluskov.intersects small = "
    ^ (Print_top.bprintf_cxtype "" smallModel)
    ^ " big = "
    ^ (Print_top.bprintf_cxtype "" bigModel)
  in
  let error_msg = bprintf_error msg exn in
  raise (Query (Internal_Error error_msg))

(************)
(* includes *)
(************)

(* BChoi: Assumption: the bigModel is deterministic. The *)
(* code does not statically/dynamically check this       *)
(* L(M1) is included in L(M2) iff                        *)
(* L(M1 intersects (not M2)) = Empty Set                 *)

let cached_nfa_table = Hashtbl.create 167
let cached_dfa_table = Hashtbl.create 167
let cached_includes_table = Hashtbl.create 167

let printnfa s = (Format.printf "\n ***%s\n" s; TypeName_Glushkov.NFA.print_automata)
let printnfa2 s = (Format.printf "\n ***%s\n" s; TypeName_Glushkov.NFAPair.print_automata)
let printdfa s = (Format.printf "\n ***%s\n" s; TypeName_Glushkov.DFA.print_automata)

let print_position p = Format.printf "%i" p

let print_position_pair (p1,p2) =  Format.printf "(%i,%i)" p1 p2

let includes cxschema smallType bigType =
  try
    begin
      try 
	Hashtbl.find cached_includes_table (smallType, bigType) 
      with
      |	Not_found ->
	  begin
            Schema_judge.debug_print_types "1. In includes\n" smallType bigType;  

    (* Step 1. Compute all element and attribute QNames used in a type *)
	    let alphabets = get_names_from_cxtypes cxschema smallType bigType in

    (* Step 2. Expand all the wildcard elements using the alphabet of
       element/attribute QNames computed in Step 1.  The expanded type
       will only differ from the input type if it contains wildcards.
       Wildcards are expanded in both types, resulting in types with
       no wildcard symbols.  
    *)

	    let smallExpType = expand_wildcards smallType alphabets in
	    let bigExpType = expand_wildcards bigType alphabets in
	    Schema_judge.debug_print_types "2. In includes\n" smallExpType bigExpType;

    (* Step 3. From the big type, build a QName/Letter mapping *)
	    let mappings = Xquery_type_core_ast_annotation.create_letter_mappings() in
	    let _ = Xquery_type_core_ast_util.make_mappings mappings bigExpType in

	    if Debug.typing_debug()
	    then Print_type_core.print_letter_mappings Format.std_formatter mappings;

    (* Step 4. Creates the regexps ready for Glushkov construction. *)
    (* Step 5. Creates the Glushkov automata *)
    (* Once we have an expanded type, we can cache its corresponding Glushkov automata. *)
	    let smallRegexp = normalize_regexp true mappings cxschema smallExpType in
	    let smallAuto = TypeName_Glushkov.build_glushkov smallRegexp in
	    (* printnfa "Small NFA" smallAuto print_letter print_position ; *)
	    let bigRegexp = normalize_regexp false mappings cxschema bigExpType in
	    let bigAuto = TypeName_Glushkov.build_dtm_glushkov bigRegexp in
	    (* printdfa "Big DFA" bigAuto print_letter print_position ; *)
    (* Step 6. Inclusion algorithm *)
	    let all_letters = TypeName_Glushkov.NFA.Alphabet.elements (TypeName_Glushkov.NFA.get_nfa_alphabet smallAuto) in
	    let new_alph = List.fold_right TypeName_Glushkov.DFA.Alphabet.add all_letters TypeName_Glushkov.DFA.Alphabet.empty in
	    let not_mebig = TypeName_Glushkov.negates_in_alphabet_space bigAuto new_alph in
	    (* printdfa "NOT Big DFA" not_mebig print_letter print_position ; *)
	    let intersect = TypeName_Glushkov.intersects_dtm smallAuto not_mebig in
	    (* printnfa2 "INTERSECT DFA" intersect print_letter print_position_pair ; *)
	    let result = TypeName_Glushkov.NFAPair.empty intersect in
	    Hashtbl.add cached_includes_table (smallType, bigType) result;
	    result
	  end
    end
  with
  | exn -> raise_includes_error exn smallType bigType

(**************)
(* intersects *)
(**************)

(* Jerome: Assumption: the bigType is deterministic. The *)
(* code does not statically/dynamically check this        *)
(* L(M1) intersects L(M2) iff                             *)
(* L(M1 intersects M2) != Empty Set                       *)

let intersects cxschema smallType bigType = 
  try
    if Debug.typing_debug()
    then
      begin
	Print_top.printf_cxtype "\nIntersection Small model: " smallType;
	Print_top.printf_cxtype "\nIntersection Big model: " bigType;
	flush stdout
      end;
    (* Step 1. Compute all element and attribute QNames used in a type *)
    let alphabets = get_names_from_cxtypes cxschema smallType bigType in

    (* Step 2. Expand all the wildcard elements using the alphabet of
       element/attribute QNames computed in Step 1.  The expanded type
       will only differ from the input type if it contains wildcards.
       Wildcards are expanded in both types, resulting in types with
       no wildcard symbols.  
    *)
    let smallExpType = expand_wildcards smallType alphabets in
    let bigExpType = expand_wildcards bigType alphabets in

    (* Step 3. From the big type, build a QName/Letter mapping *)
    let mappings = Xquery_type_core_ast_annotation.create_letter_mappings() in
    let _ = Xquery_type_core_ast_util.make_mappings mappings bigExpType in

if Debug.typing_debug() then
   Print_type_core.print_letter_mappings Format.std_formatter mappings;

    (* Step 4. creates the regexps ready for Glushkov construction *)
    (* Step 5. Creates the Glushkov automata *)
    (* Once we have an expanded type, we can cache its corresponding Glushkov automata. *)
    let smallRegexp = normalize_regexp true mappings cxschema smallExpType in
    let smallAuto = TypeName_Glushkov.build_glushkov smallRegexp in
if Debug.typing_debug() then printnfa "Small NFA" smallAuto print_letter print_position ;
    let bigRegexp = normalize_regexp false mappings cxschema bigExpType in
    let bigAuto = TypeName_Glushkov.build_dtm_glushkov bigRegexp in
if Debug.typing_debug() then printdfa "Big DFA" bigAuto print_letter print_position ;  
    (* Step 6. Intersection algorithm *)
    let itype = (TypeName_Glushkov.intersects_dtm smallAuto bigAuto) in
if Debug.typing_debug() then printnfa2 "Intersect NFA" itype print_letter print_position_pair ; 
    not(TypeName_Glushkov.NFAPair.empty (itype))
  with
  | exn -> raise_intersects_error exn smallType bigType


(************************)
(* Subtyping operations *)
(************************)

(* Main subtyping function *)

(*    | !!!xs:anyType!!! -> *)

let is_subtype_of schema cxtype1 cxtype2 =
  try 
    (* We first try to compare the types syntactically... *)
    Schema_judge.is_syntactic_subtype_of schema cxtype1 cxtype2
  with
    (* If they cannot be compared, we resort to a full inclusion test *)
  | Query(Unknown _) -> 
      begin
	let cxtype1' = simplify_ty schema cxtype1 in
	let cxtype2' = simplify_ty schema cxtype2 in
	if (Schema_judge.equal_cxtypes schema cxtype1' cxtype2') then true
	else (includes schema cxtype1' cxtype2')
      end

let intersects_with schema cxtype1 cxtype2 =
  let (p,min,max) = Schema_util.factor_with_units cxtype1 in

  (* Syntactic short-circuit rules for common types *)
  (* ??Question?? Do we need to check for Empty or None in p? *)
  (* item() or item()+ *)
  if (cxtype2 = cxtype_expanded_anytype) then
      not(Occurrence.equal min unbounded && Occurrence.equal max occurs_zero)
  else if (cxtype2 = cxtype_item || cxtype2 = cxtype_item_plus) then 
    Occurrence.equal min occurs_one 
  (* item()? or item()* *)
  else if (cxtype2 = cxtype_item_optional || cxtype2 = cxtype_item_star) then 
     (* t1 intersects item()? or item()*  if t1 not none *)
        not (Occurrence.equal min unbounded &&
	     Occurrence.equal max occurs_zero)

  (* node() or node()+ *)
  else if (cxtype2 = cxtype_node || cxtype2 = cxtype_node_plus) then
     (* p contains nods, and max >= 1 *)
    (List.exists is_node_cxtype p) && not(Occurrence.equal max occurs_zero)
  (* node()? or node()* *)
  else if (cxtype2 = cxtype_node_optional || cxtype2 = cxtype_node_star) then
    (List.exists is_node_cxtype p)

  (* xs:anyAtomicType or  xs:anyAtomicType+ *)
  else if (cxtype2 = cxtype_atomic || cxtype2 = cxtype_atomic_plus || 
           cxtype2 = cxtype_anyAtomic || cxtype2 = cxtype_anyAtomic_plus)
  then
    (List.exists is_atomic_cxtype p) &&
    Occurrence.equal min occurs_one 
  (* xs:anyAtomicType? or xs:anyAtomicType* *)
  else if (cxtype2 = cxtype_atomic_optional || cxtype2 = cxtype_atomic_star || 
           cxtype2 = cxtype_anyAtomic_optional || cxtype2 = cxtype_anyAtomic_star) then
    (List.exists is_atomic_cxtype p)

  (* none() : The factored None type has min=unbounded, max=0*)
  else if (cxtype2 = cxtype_none) then 
    Occurrence.equal min unbounded &&
    Occurrence.equal max occurs_zero
  (* empty-sequence() *)
  else if (cxtype2 = cxtype_empty) then 
    Occurrence.equal min occurs_zero 

  (* document-node() *)
  else if (cxtype2 = cxtype_documentnode) then
    List.exists is_document_cxtype p &&
    Occurrence.equal min occurs_one 
  (* document-node()? *)
  else if (cxtype2 = cxtype_documentnode_optional) then
    List.exists is_document_cxtype p 

  else 
    let cxtype1' = simplify_ty schema cxtype1
    and cxtype2' = simplify_ty schema cxtype2 in
    if (Schema_judge.equal_cxtypes schema cxtype1' cxtype2') then true
    else intersects schema cxtype1' cxtype2'

(* Specific subtyping functions *)

let is_subtype_of_anynumeric schema model =
  is_subtype_of schema model cxtype_numeric

let is_subtype_of_anystring schema model =
  is_subtype_of schema model cxtype_anystring

let is_subtype_of_anyURI schema model =
  is_subtype_of schema model cxtype_anyURI

let is_subtype_of_anyatomic schema model =
  is_subtype_of schema model cxtype_atomic

let is_subtype_of_anyatomic_optional schema model =
  is_subtype_of schema model cxtype_atomic_optional

let is_subtype_of_anyatomic_sequence schema model =
  is_subtype_of schema model cxtype_atomic_star

let is_subtype_of_empty_sequence schema t =
  is_subtype_of schema t cxtype_empty

let is_subtype_of_empty_choice schema t =
  is_subtype_of schema t cxtype_none

let is_subtype_of_anynode schema model =
  is_subtype_of schema model cxtype_node

let is_subtype_of_anynode_sequence schema model =
  is_subtype_of schema model cxtype_node_star

let is_subtype_of_anynode_plus schema model =
  is_subtype_of schema model cxtype_node_plus

let is_subtype_of_document schema model =
  is_subtype_of schema model cxtype_documentnode

let is_subtype_of_element schema model =
  is_subtype_of schema model cxtype_element

let is_subtype_of_attribute schema model =
  is_subtype_of schema model cxtype_attribute

let is_subtype_of_comment schema model =
  is_subtype_of schema model cxtype_comment

let is_subtype_of_processing_instruction schema model =
  is_subtype_of schema model cxtype_pi

let is_subtype_of_text schema model =
  is_subtype_of schema model cxtype_text

(* let (p,min,max) = Scheam_util.factor_with_units cxtype1 in
   match cxtype2 with
   | !!!xs:anyType!!! ->
   | cxtype_item -> 
     (* (p, 1, 1) <: item() *)
     Occurrence.equal min occurs_one &&
     Occurrence.equal max occurs_one 
   | cxtype_item_optional ->
     (* max <= 1, t2 = item()? *)
     Occurrence.le max occurs_one 
   | cxtype_item_star -> true
   | cxtype_item_plus ->
     Occurrence.le occurs_one min 
   | cxtype_node ->
     (* p does not contain atomic, min = 1, max = 1 *)
     (not(List.exists is_atomic_cxtype p)) &&
     Occurrence.equal min occurs_one &&
     Occurrence.equal max occurs_one 
   | cxtype_node_optional ->
     (not(List.exists is_atomic_cxtype p)) &&
     Occurrence.le max occurs_one 
   | cxtype_node_star ->
     (not(List.exists is_atomic_cxtype p)) 
   | cxtype_node_plus ->
     (not(List.exists is_atomic_cxtype p)) &&
     Occurrence.le occurs_one min 
   | cxtype_atomic ->
     (not(List.exists is_node_cxtype p)) &&
     Occurrence.equal min occurs_one &&
     Occurrence.equal max occurs_one 
   | cxtype_atomic_optional ->
     (not(List.exists is_node_cxtype p)) &&
     Occurrence.le max occurs_one 
   | cxtype_atomic_star ->
     (not(List.exists is_node_cxtype p))
   | cxtype_empty -> ???
   | cxtype_documentnode ->
     List.for_all is_document_cxtype p &&
     Occurrence.equal min occurs_one &&
     Occurrence.equal max occurs_one 
   | cxtype_documentnode_optional
     List.for_all is_document_cxtype p &&
     Occurrence.le max occurs_one 
for all the rest, we have to do a named sub-type check...
   | cxtype_numeric -> 
   | cxtype_numeric_optional ->
   | xs:boolean
   | xs:boolean?
   | xs:date
   | xs:date?
   | xs:dateTime
   | xs:dateTime?
   | xs:decimal
   | xs:decimal?
   | xs:double
   | xs:double?
   | xs:float
   | xs:float?
   | xs:integer
   | xs:integer?
   | xs:int
   | xs:int?
   | xs:NCName
   | xs:NCName?
   | xs:QName
   | xs:QName?
   | xs:string
   | xs:string?
   | xs:string*
   | xs:time
   | xs:time?
*)
