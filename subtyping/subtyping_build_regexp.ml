(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: subtyping_build_regexp.ml,v 1.8 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Subtyping_build_regexp
   Description:
     This module implements type inclusion and intersection, by
     compiling core types into automatas using the glushkov
     construction.
*)

open Error
open Occurrence

open Namespace_names

open Xquery_common_ast
open Xquery_type_core_ast
open Xquery_type_core_ast_annotation

open Subtyping_letter
open Subtyping_glushkov
open TypeName_Glushkov  (* Module defined inside Subtyping_glushkov *)
open Regexp             (* Module defined inside TypeName_Glushkov *)


(******************************************)
(* Mapping Type AST to Regular expression *)
(******************************************)

(* Normalizes a model into a 'pure' regular expression *)

let normalize_element_ref cxschema cename =
  let (cename',csubstitutes_for',cnillable',ctname') =
    Schema_judge.lookup_element cxschema cename
  in
  begin
    match csubstitutes_for' with
    | CSubstitutesFor cename'' ->
	raise (Query (Prototype "Element references with substitution groups not handled in subtyping yet"))
    | CNonSubstitutesFor -> ()
  end;
  begin
    match cnillable' with
    | Nillable ->
	raise (Query (Prototype "Nillable elements not handled in subtyping yet"))
    | NonNillable ->
	()
  end;
  (cename',ctname')

let normalize_attribute_ref cxschema cename =
  let (caname',ctname') =
    Schema_judge.lookup_attribute cxschema cename
  in
  (caname',ctname')

(* Note: this function also initializes positions.
   - Jerome *)

let rec normalize_regexp_aux expand_small_type mappings cxschema pc m =
  if Debug.typing_debug()
  then Debug.print_typing_debug (">>>normalize_regexp_aux\n");
  match m.pcxtype_desc with
  | CAtomicRef ctname ->
	(* The normalization of an atomic type is the union of all types derived from it 
	   let derived_from_set = Schema_judge.derived_from_set cxschema ctname  <== this requires compiled type ids
	*)
	(*
	   Mary: I am hard-coding a hack here just to see if I can get this right.
	*)
      begin
	let s = 
	  let letter = letter_of_atomic mappings cxschema ctname in
	  if Debug.typing_debug()
	  then Debug.print_typing_debug ("AtomicRef "^(Namespace_symbols.symbol_prefix_string ctname)^" -> "^(string_of_int letter));
	  RSym (AtomicTypeLetter letter, Id.next pc)
	in
	if (ctname = Namespace_symbols.decimalsym && expand_small_type) then 
	  let letter = letter_of_atomic mappings cxschema Namespace_symbols.integersym in
	  RChoice (s, RSym (AtomicTypeLetter letter, Id.next pc))
	else 
	  s 
      end
  | CElementRef cename ->
      begin
	let letter = letter_of_element mappings cxschema (cename,None,NonNillable) in
	if Debug.typing_debug()
	then Debug.print_typing_debug ("ElementRef "^(Namespace_symbols.symbol_prefix_string cename)^" -> "^(string_of_int letter));
	RSym (ElementTypeLetter letter, Id.next pc)
      end
  | CElementLocal (cename,cnillable,ctname) ->
      begin
	let letter = letter_of_element mappings cxschema (cename,Some ctname,cnillable) in
	if Debug.typing_debug()
	then Debug.print_typing_debug ("ElementLocal "^(Namespace_symbols.symbol_prefix_string cename)^" -> "^(string_of_int letter));
	RSym (ElementTypeLetter letter, Id.next pc)
      end
  | CAttributeRef caname ->
      begin
	let letter = letter_of_attribute mappings cxschema (caname,None) in
	if Debug.typing_debug()
	then Debug.print_typing_debug ("AttributeRef "^(Namespace_symbols.symbol_prefix_string caname)^" -> "^(string_of_int letter));
	RSym (AttributeTypeLetter letter, Id.next pc)
      end
  | CAttributeLocal (caname,ctname) ->
      begin
	let letter = letter_of_attribute mappings cxschema (caname,Some ctname) in
	if Debug.typing_debug()
	then Debug.print_typing_debug ("AttributeRef "^(Namespace_symbols.symbol_prefix_string caname)^" -> "^(string_of_int letter));
	RSym (AttributeTypeLetter letter, Id.next pc)
      end
  | CDocument m ->
      begin
      (* NB! 
	 With document types, we construct a union of DocumentType
         symbols, each of which contains the letter associated with one top-level
         element in the content type.  We ignore all the comment/PI/text stuff. 
      *)
	let (p, _, _) = Schema_util.factor_with_units m in 
	let letters = 
	  Gmisc.map_concat
	    (fun t -> 
	      match t.pcxtype_desc with 
	      | CElementRef cename -> 
		  begin
		    let letter = letter_of_element mappings cxschema (cename,None,NonNillable) in
		    if Debug.typing_debug()
		    then Debug.print_typing_debug ("DocumentElementRef "^(Namespace_symbols.symbol_prefix_string cename)^" -> "^(string_of_int letter));
		    [letter]
		  end
	      | CElementLocal (cename, cnillable, ctname) -> 
		  begin
		    let letter = letter_of_element mappings cxschema (cename,Some ctname,cnillable) in
		    if Debug.typing_debug()
		    then Debug.print_typing_debug ("DocumentElementLocal"^(Namespace_symbols.symbol_prefix_string cename)^"="^(string_of_int letter));
		    [letter]
		  end
	      | _ -> []) 
	    p
	in
	match letters with
	| [letter] -> RSym (DocumentTypeLetter letter, Id.next pc)
	| first_letter :: rest ->
	    List.fold_left (fun sum letter -> RChoice(sum, RSym (DocumentTypeLetter letter, Id.next pc))) 
	      (RSym (DocumentTypeLetter first_letter, Id.next pc)) rest
	| [] -> REmpty
      end
  | CText ->
      RSym (TextTypeLetter, Id.next pc)
  | CPI None ->
      RSym (PITypeLetter 0, Id.next pc)
  | CPI (Some _) ->
      RSym (PITypeLetter 1, Id.next pc)
  | CComment ->
      RSym (CommentTypeLetter, Id.next pc)
  | CBound (m1,min,max) ->
      normalize_bound expand_small_type mappings cxschema pc m1 min max 
  | CSequence (m1,m2) ->
      RSeq (normalize_regexp_aux expand_small_type mappings cxschema pc m1, 
	    normalize_regexp_aux expand_small_type mappings cxschema pc m2)
  | CEmpty ->
      REmpty
  | CChoice (m1,m2) ->
      RChoice (normalize_regexp_aux expand_small_type mappings cxschema pc m1, 
	       normalize_regexp_aux expand_small_type mappings cxschema pc m2)
  | CNone ->
      RNone (* Note: This will eventually raise an exception.  None is
  	       undefined in automata, and 0 <: Everything, anyway. *)
  | CInterleave (m1,m2) ->
      RAmp (normalize_regexp_aux expand_small_type mappings cxschema pc m1, 
	    normalize_regexp_aux expand_small_type mappings cxschema pc m2)

(* Note: Bound normalization proceeds in two steps.

     1. normalize lower bound decrements the lower bound until it
        reaches zero using the following law:

        if m > 0 then t{m,n} = t,t{m-1,n-1}

     2. normalize upper bound takes a bounded type with lower bound
        zero and normalizes the upper bound with either the following
        laws:

        if n = unbounded then t{0,unbounded} = t*
        if n < unbounded then t{0,n} = (() | t,t{0,n-1})

        remark that there are many ways t{0,n} could be normalized:

        t{0,n} = t?,t?....,t?
        t{0,n} = () | (t) | (t,t) | .... | (t,...,t)
        t{0,n} = () | (t, (() | (t, (..... (() | t)))))

        the law above generates the third normalization, which is the
        only one determninstic.

   Finally, there is a determinism problem when normalizing nested
   types. For instance:

   assuming:
   t = a{0,2}

   N(t)          --> (E|(a,(E|a)))

   N(t{0,2})     --> (E|(N(t),(E|N(t))))

                 --> (E|(a,(E|a))) , (E|(E|(a,(E|a))))

   - Jerome *)

(* BChoi: Modified from Jerome's email
       (I)      t{0,*} == t*
       (II)     t{1,*} == t+
       (III)    t{m,*} == t, t{m-1, *}

       (IV)     t{0,1} == ()|t
       (IV')    t{0,n} == ()|t,t{0,n-1}
       (V)      if m > 1 then t{m,n} == t,t{m-1,n-1}
*)

and normalize_bound expand_small_type mappings cxschema pc m1 min max =
  match (min, max) with
  (* the common cases *)
  | (UP_INT 0, UNBOUNDED) -> RStar (normalize_regexp_aux expand_small_type mappings cxschema pc m1) (* (I) *)
  | (UP_INT 1, UNBOUNDED) -> RPlus (normalize_regexp_aux expand_small_type mappings cxschema pc m1) (* (II) *)
  | (UP_INT min', UNBOUNDED) ->                                          (* (III) *)
      RSeq (normalize_regexp_aux expand_small_type mappings cxschema pc m1,
	    normalize_bound expand_small_type mappings cxschema pc m1 (UP_INT (min'-1)) UNBOUNDED)
  | (UP_INT 0, UP_INT 1) ->                                              (* (IV) *)
      RChoice (REmpty, normalize_regexp_aux expand_small_type mappings cxschema pc m1)
  | (UP_INT 0, UP_INT max') ->                                           (* (IV') *)
      RChoice (REmpty, RSeq (normalize_regexp_aux expand_small_type mappings cxschema pc m1,
			     normalize_bound expand_small_type mappings cxschema pc m1
			       (UP_INT 0)
			       (UP_INT (max'-1))))
  | (UP_INT min', UP_INT max') ->                                        (* (V) *)
      RSeq (normalize_regexp_aux expand_small_type mappings cxschema pc m1,
	    normalize_bound expand_small_type mappings cxschema pc m1
	      (UP_INT (min'-1))
	      (UP_INT (max'-1)))
  | _ ->
      raise (Query
	       (Schema_Internal "Error during normalizing bound type: Invalid bounds"))

(* and normalize_upper_bound *)
let normalize_regexp expand_small_type mappings cxschema m =
  let position_counter = Id.create 1 in
  normalize_regexp_aux expand_small_type mappings cxschema position_counter m

