(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: subtyping_letter.ml,v 1.8 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Subtyping_letter
   Description:
     This module implements operations necessary to map item types
     into letters used in the automata for subtyping.
*)

open Error

open Namespace_symbols_util

open Xquery_common_ast
open Xquery_type_core_ast
open Xquery_type_core_ast_annotation
open Xquery_type_core_ast_util
   
type actual_letter =
  | AtomicTypeLetter of int
  | ElementTypeLetter of int
  | AttributeTypeLetter of int
  | DocumentTypeLetter of int
  | PITypeLetter of int
  | TextTypeLetter
  | CommentTypeLetter

let compare_letters l1 l2 =
  match (l1,l2) with
  | (AtomicTypeLetter i1,AtomicTypeLetter i2) ->
      i1 - i2
  | (AtomicTypeLetter _,_) -> 1
  | (_, AtomicTypeLetter _) -> -1
  | (ElementTypeLetter i1,ElementTypeLetter i2) ->
      i1 - i2
  | (ElementTypeLetter _,_) -> 1
  | (_, ElementTypeLetter _) -> -1
  | (AttributeTypeLetter i1,AttributeTypeLetter i2) ->
      i1 - i2
  | (AttributeTypeLetter _,_) -> 1
  | (_, AttributeTypeLetter _) -> -1
  | (DocumentTypeLetter i1,DocumentTypeLetter i2) -> 
      i1 - i2
  | (DocumentTypeLetter _ ,_) -> 1
  | (_, DocumentTypeLetter _) -> -1
  | (PITypeLetter i1,PITypeLetter i2) -> 
      i1 - i2
  | (PITypeLetter _ ,_) -> 1
  | (_, PITypeLetter _) -> -1
  | (TextTypeLetter,TextTypeLetter) -> 0
  | (TextTypeLetter,_) -> 1
  | (_, TextTypeLetter) -> -1
  | (CommentTypeLetter,CommentTypeLetter) -> 0

let print_letter l =
  match l with
  | AtomicTypeLetter i ->
      Format.printf "atomic(%i)" i
  | ElementTypeLetter i ->
      Format.printf "element(%i)" i
  | AttributeTypeLetter i ->
      Format.printf "attributes(%i)" i
  | DocumentTypeLetter i ->
      Format.printf "document(%i)" i
  | PITypeLetter i ->
      Format.printf "pi(%i)" i
  | TextTypeLetter ->
      Format.printf "text()"
  | CommentTypeLetter ->
      Format.printf "comment()"

module OrderedNamePairs =
  struct
    type t = actual_letter

    let compare l1 l2 = compare_letters l1 l2
  end

(* Note: this function also initializes positions.
   - Jerome *)

type alphabets = Namespace_symbols.relem_symbol list * Namespace_symbols.rattr_symbol list

let rec qnames_of_type cxschema cxtype =
  match cxtype.pcxtype_desc with
  | CAtomicRef _               -> ([],[])
  | CElementRef cename         -> (cename :: [],[])
  | CElementLocal (cename,_,_) -> (cename :: [],[])
  | CAttributeRef caname       -> ([],caname :: [])
  | CAttributeLocal (caname,_) -> ([],caname :: [])
  | CDocument m1       	       -> qnames_of_type cxschema m1 
  | CText 	      	       -> ([],[])
  | CPI m1  	      	       -> ([],[])
  | CComment          	       -> ([],[])
  | CBound (m1,_,_)   	       -> qnames_of_type cxschema m1
  | CSequence (m1,m2) ->
      let (enames1,anames1) = qnames_of_type cxschema m1 in
      let (enames2,anames2) = qnames_of_type cxschema m2 in
      (enames1 @ enames2, anames1 @ anames2)
  | CEmpty                     -> ([],[])
  | CChoice (m1,m2) ->
      let (enames1,anames1) = qnames_of_type cxschema m1 in
      let (enames2,anames2) = qnames_of_type cxschema m2 in
      (enames1 @ enames2, anames1 @ anames2)
  | CNone -> ([],[])
  | CInterleave (m1,m2) ->
      let (enames1,anames1) = qnames_of_type cxschema m1 in
      let (enames2,anames2) = qnames_of_type cxschema m2 in
      (enames1 @ enames2, anames1 @ anames2)

let rec clean_dup_elem cxschema cenames =
  match cenames with
  | [] -> []
  | cename :: other_cenames ->
      let sub_cond x =
	if Namespace_symbols.symbol_equal x cename
	then
	  true
	else
	  (Schema_judge.substitutes_for cxschema x cename)
      in
      if (List.exists sub_cond other_cenames)
      then (clean_dup_elem cxschema other_cenames)
      else cename :: (clean_dup_elem cxschema other_cenames)

let rec clean_dup_attr cxschema canames =
  match canames with
  | [] -> []
  | caname :: other_canames ->
      let sub_cond x =
	Namespace_symbols.symbol_equal x caname
      in
      if (List.exists sub_cond other_canames)
      then (clean_dup_attr cxschema other_canames)
      else caname :: (clean_dup_attr cxschema other_canames)

let get_names_from_cxtype cxschema cxtype =
  let (enames,anames) = qnames_of_type cxschema cxtype in
  (clean_dup_elem cxschema enames,clean_dup_attr cxschema anames)

let get_names_from_cxtypes cxschema cxtype1 cxtype2 =
  let (enames1,anames1) = qnames_of_type cxschema cxtype1 in
  let (enames2,anames2) = qnames_of_type cxschema cxtype2 in
  (clean_dup_elem cxschema (enames1@enames2),clean_dup_attr cxschema (anames1@anames2))

let expand_element_wildcard (cename,nillable,ctname) alphabet fi =
  if Namespace_symbols.symbol_equal cename Namespace_symbols_builtin.wild_symbol
  then
    let make_elem x = fmkcxtype (CElementLocal (x,nillable,ctname)) fi in
    Schema_util.choice_of_list (List.map make_elem alphabet)
  else
    fmkcxtype (CElementLocal (cename,nillable,ctname)) fi

let expand_attribute_wildcard (caname,ctname) alphabet fi =
  if Namespace_symbols.symbol_equal caname Namespace_symbols_builtin.wild_symbol
  then
    let make_attr x = fmkcxtype (CAttributeLocal (x,ctname)) fi in
    Schema_util.choice_of_list (List.map make_attr alphabet)
  else
    fmkcxtype (CAttributeLocal (caname,ctname)) fi

let rec expand_wildcards cxtype alphabet =
  let type_desc = cxtype.pcxtype_desc in
  let fi = cxtype.pcxtype_loc in
  match type_desc with
  | CAtomicRef _
  | CElementRef _
  | CAttributeRef _
  | CText
  | CPI _
  | CComment
  | CEmpty
  | CNone -> cxtype
  | CBound (m1,min,max) ->
      let m1' = expand_wildcards m1 alphabet in
      fmkcxtype (CBound (m1',min,max)) fi
  | CSequence (m1,m2) ->
      let m1' = expand_wildcards m1 alphabet in
      let m2' = expand_wildcards m2 alphabet in
      fmkcxtype (CSequence (m1',m2')) fi
  | CChoice (m1,m2) ->
      let m1' = expand_wildcards m1 alphabet in
      let m2' = expand_wildcards m2 alphabet in
      fmkcxtype (CChoice (m1',m2')) fi
  | CInterleave (m1,m2) ->
      let m1' = expand_wildcards m1 alphabet in
      let m2' = expand_wildcards m2 alphabet in
      fmkcxtype (CInterleave (m1',m2')) fi
  | CElementLocal (cename,nillable,ctname) ->
      expand_element_wildcard (cename,nillable,ctname) (fst alphabet) fi
  | CAttributeLocal (caname,ctname) ->
      expand_attribute_wildcard (caname,ctname) (snd alphabet) fi
  | CDocument m ->
      let m' = expand_wildcards m alphabet in
      fmkcxtype (CDocument (m')) fi

(* Build a mapping *)

(* All letters in the subtype that are not in the super type can be considered the same! 
   Mary: I don't understand this comment *)

let make_ce_letter_force mapping(cename,typeopt,nil) = 0 
(*  raise (Query(Internal_Error ("Letter mapping not found for element name "^(Namespace_symbols.symbol_prefix_string cename))))*)

let make_ca_letter_force mapping (caname,typeopt) =  0 
(*  raise (Query(Internal_Error ("Letter mapping not found for attribute name "^(Namespace_symbols.symbol_prefix_string caname))))*)

let make_at_letter_force mapping ctname =  0
(*  raise (Query(Internal_Error ("Letter mapping not found for type "^(Namespace_symbols.symbol_prefix_string ctname))))*)

let subname cxschema ctname1 ctname2 =
  match ctname1,ctname2 with
  | None,None -> true
    (* 
       The following isn't right.  What if ctname2 is xs:anyType?
       See subname_attr below for attributes. 
      | None, _ -> false
      Replace with: 
    *)
  | None, Some ctname2 -> ctname2 = Namespace_symbols_builtin.xs_anyType
  | _,None -> false
  | (Some ctname1,Some ctname2) ->
      Schema_judge.derives_from cxschema ctname1 ctname2

let subname_attr cxschema ctname1 ctname2 =
  match ctname1,ctname2 with
  | None,None -> true
    (* 
       What if ctname2 is xs:anySimpleType for attributes? 
      | None, _ -> false
      Replace with: 
    *)
  | None, Some ctname2 -> ctname2 = Namespace_symbols_builtin.xs_anySimpleType
  | _,None -> false
  | (Some ctname1,Some ctname2) ->
      Schema_judge.derives_from cxschema ctname1 ctname2

let subnil nil1 nil2 =
  (nil2 = Nillable) || (not (nil1 = Nillable))

let letter_of_element_not_found mapping (cename1,opttype1,nil1) : int =
  make_ce_letter_force mapping (cename1,opttype1,nil1)

let rec find_letter_of_element mapping cxschema namelist (cename1,opttype1,nil1) =
  match namelist with
  | [] -> letter_of_element_not_found mapping (cename1,opttype1,nil1)
  | (opttype2,nil2,letter2) :: namelist' ->
      if (subname cxschema opttype1 opttype2) && (subnil nil1 nil2)
      then
	letter2
      else
	find_letter_of_element mapping cxschema namelist' (cename1,opttype1,nil1)

let letter_of_element mapping cxschema (cename1,opttype1,nil1) =
  let mapping = elem_letter_map mapping in
  match opttype1 with
  | Some ctname1 ->
      let namelist = SQNameHashtbl.find_all (snd mapping) cename1 in
      find_letter_of_element mapping cxschema namelist (cename1,opttype1,nil1)
  | None ->
      (* Lookup on substitution groups *)
      let namelist =
	let sub_fun x y z =
	  if (Schema_judge.substitutes_for cxschema x cename1)
	  then
	    y :: z
	  else
	    z
	in
	SQNameHashtbl.fold sub_fun (snd mapping) []
      in
      find_letter_of_element mapping cxschema namelist (cename1,opttype1,nil1)

let letter_of_attribute_not_found mapping (caname1,opttype1) : int =
  make_ca_letter_force mapping (caname1,opttype1)

let rec find_letter_of_attribute mapping cxschema namelist (caname1,opttype1) =
  match namelist with
  | [] -> letter_of_attribute_not_found mapping (caname1,opttype1)
  | (opttype2,letter2) :: namelist' ->
(*  Mary: Change this temporarily to call special subname for attributes 
   if (subname cxschema opttype1 opttype2) *)
      if (subname_attr cxschema opttype1 opttype2)
      then
	letter2
      else
	find_letter_of_attribute mapping cxschema namelist' (caname1,opttype1)

let letter_of_attribute mapping cxschema (caname1,opttype1) =
  let mapping = attr_letter_map mapping in
  let namelist = SQNameHashtbl.find_all (snd mapping) caname1 in
  find_letter_of_attribute mapping cxschema namelist (caname1,opttype1)

let letter_of_atomic mapping cxschema ctname =
  let mapping = type_letter_map mapping in
  (* Lookup on type derivation *)
  (* Mary: I don't understand why we are mapping the type 
     to the letter of its top-most super-type. *)
  let lookup_sub =
    let sub_fun x y z =
      if (Schema_judge.derives_from cxschema ctname x)
      then
	y :: z
      else
	z
    in
    SQNameHashtbl.fold sub_fun (snd mapping) []
  in
  match lookup_sub with 
(*   match SQNameHashtbl.find_all (snd mapping) ctname with*)
  | [] -> make_at_letter_force mapping ctname
  | letter2 :: _ -> letter2

