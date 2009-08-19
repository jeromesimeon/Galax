(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: schema_simplification.ml,v 1.12 2007/02/01 22:08:53 simeon Exp $ *)

(* Module: Schema_simplification
   Description:
     This modules implements simplification rewritings over
     type. Those rewritings preserve the semantics of the type, but
     result in simpler, more readable types.
 *)


open Error
open Occurrence

open Xquery_type_core_ast
open Xquery_type_core_ast_util

open Schema_util

open Print_top


(* Note: rec_simplify__ty is applied recursively until a fixed point
   is reached.  simplify_one_ty returns a pair; the second value is a
   boolean indicating whether the type expression was changed and
   therefore it is necessary to recurse.
   - Jerome *)

let rec simplify_one_ty tr t =
  let (m, b) = 
    match t.pcxtype_desc with
    | CSequence (m, m') ->
	begin
	  match m.pcxtype_desc with 
	    (* ((e1,e2),e3) == (e1,(e2,e3)) *)
	  | CSequence(m1, m2) -> 
              let (t1',_) = simplify_one_ty tr m1
              and (t2',_) = simplify_one_ty tr m2
              and (t3',_) = simplify_one_ty tr m' in 
              let seq = fmkcxtype (CSequence(t2', t3')) t2'.pcxtype_loc in
	      (fmkcxtype (CSequence(t1', seq)) t.pcxtype_loc, true)
		(* (0,e) == 0 *)
	  | CNone ->
	      (fmkcxtype CNone t.pcxtype_loc,true)
		(* ((),e) == e *)
	  | CEmpty ->
	      let (m'',_) = simplify_one_ty tr m' in
	      (m'',true)
		(* Otherwise, match on RHS of TComma *)
	  | _ ->
	      begin
		match m'.pcxtype_desc with 
		  (* (e,0) == 0 *)
		| CNone ->
		    (fmkcxtype CNone t.pcxtype_loc,true)
		      (* (e,()) == e *)
		| CEmpty ->
		    let (m',_) = simplify_one_ty tr m in
		    (m',true)
		      (* e1, e2 == simplify(e1), simplify(e2) *)
		| _ -> 
		    let (m1',b1) = simplify_one_ty tr m
		    and (m2',b2) = simplify_one_ty tr m' in
		    (fmkcxtype (CSequence (m1', m2')) t.pcxtype_loc, b1||b2)
	      end
	end
    | CChoice (_, _) ->
	let m' = simplify_union tr t in
	if Debug.default_debug()
	then
	  begin
	    let msg = "After simplify_union:\n" ^ bprintf_cxtype "" m' in
	    Debug.print_default_debug msg
	  end;
	begin
	  match m'.pcxtype_desc with
	  | CChoice (m1,m2) ->
	      begin
		match m1.pcxtype_desc with
		  (* t|0 = t *)
		| CNone ->
		    (m2,true)
		      (* t|() = t{0,1} *)
		|	CEmpty ->
		    ((fmkcxtype (CBound (m2,Occurrence.occurs 0,Occurrence.occurs 1)) m2.pcxtype_loc), true)
		|	_ ->
		    begin
		      match m2.pcxtype_desc with
			(* 0|t = t *)
		      | CNone ->
			  (m1,true)
			    (* ()|t = t{0,1} *)
		      | CEmpty ->
			  ((fmkcxtype (CBound (m1,Occurrence.occurs 0,Occurrence.occurs 1)) m1.pcxtype_loc), true)
		      |	_ ->
			  (m',false)
		    end
 	      end
	  | _ ->
	      (m',true)
	end
          (* t{1,1} == t *)
    | CBound (m, UP_INT 1, UP_INT 1) -> 
	let (m',b) = simplify_one_ty tr m in
	(fmkcxtype m'.pcxtype_desc t.pcxtype_loc, true)
	  
    | CBound (m, UP_INT 0, n) -> 
	begin
	  match m.pcxtype_desc with  
	  | CEmpty ->
	      (fmkcxtype CEmpty m.pcxtype_loc,true)
	  | CChoice(m', m'') ->
	      (* (() | e){0,n} == e{0,n} *)
	      begin
		match m'.pcxtype_desc with 
		| CEmpty -> 
		    let (m''',_) = simplify_one_ty tr m'' in
		    (fmkcxtype (CBound (m''', UP_INT 0, n)) m.pcxtype_loc, true)
		| _ ->
		    begin
		      match m''.pcxtype_desc with 
			(* (e | ()){0,n} == e{0,n} *)
		      | CEmpty -> 
			  let (m''',_) = simplify_one_ty tr m' in
			  (fmkcxtype (CBound (m''', UP_INT 0, n)) m.pcxtype_loc, true)
		      | _ -> 
			  let (m''', b) = simplify_one_ty tr m in
			  (fmkcxtype (CBound (m''', UP_INT 0, n)) m.pcxtype_loc, b)
		    end
	      end
	  | CBound (m', _, n') -> 
	      (fmkcxtype (CBound (m', UP_INT 0, mult n n')) m.pcxtype_loc, true)
	  | _ -> 
	      let (m',b) = simplify_one_ty tr m in
	      (fmkcxtype (CBound(m', UP_INT 0, n)) m.pcxtype_loc, b)
	end

(* M&B : This case and the one above need to be coalesced *)

    | CBound (m, i, j) -> 
	(* (e{n,m}){n',m'} == e{n*n',m*m'} *)
	begin
	  match m.pcxtype_desc with 
	  |  CBound(m', i', j') ->
	      let (m'',_) = simplify_one_ty tr m' in
	      (fmkcxtype (CBound (m'', mult i i', mult j j')) m.pcxtype_loc, true)

	(* (){n,m} == () *)
	  | CEmpty ->
	      (fmkcxtype CEmpty m.pcxtype_loc,true)
	  | _ -> 
	      let (m',b) = simplify_one_ty tr m in
	      (fmkcxtype (CBound (m', i, j)) m.pcxtype_loc, b)
	end
    | _ ->
	(t,false)
  in
  if Debug.default_debug() then
    begin
      let msg =
	if b then
	  ("Changed from "^(bprintf_cxtype "" t)^" to "^(bprintf_cxtype "" m))
	else ("No change to "^(bprintf_cxtype "" t))
      in
      Debug.print_default_debug msg
    end;
  (m, b)

and simplify_union tr cxtype =
  let all_types = list_of_choice cxtype in
  if Debug.default_debug() then 
    begin
      Debug.sprintf_default_debug "After list_of_choice: %d" (List.length all_types);
      List.iter (fun x -> Debug.print_default_debug (bprintf_cxtype " ; " x)) all_types
    end;
  let all_simplified_types = List.map (rec_simplify_ty tr) all_types in
  if Debug.default_debug() then
    begin
      Debug.sprintf_default_debug ("After simplify_ty: %d") (List.length all_simplified_types);
      List.iter (fun x -> Debug.print_default_debug (bprintf_cxtype " ; " x)) all_simplified_types
    end;
  let no_nones = (List.filter (fun f -> not(Xquery_type_core_ast_util.is_none_cxtype f)) all_simplified_types) in 
  if Debug.default_debug() then
    begin
      Debug.sprintf_default_debug ("After filter TNone: %d") (List.length no_nones);
      List.iter (fun x -> Debug.print_default_debug (bprintf_cxtype "\n" x)) no_nones
    end;

  (* The goal here is to remove duplicates of item types in unions,
     not arbitrary types.  We do this by comparing them syntactically.

     If we had compiled type identifiers, (I think) we could do this more
     efficiently by sorting and removing duplicates. 

     Mary (2/2006): See Schema_judge.compare_cxtypes/equal_cxtypes for
     structural comparison/equality.
  *)
(*

  Mary (6/2006): Previously, we only removed duplicates of atomic
  types, which is unacceptable, because it ignores all the node item
  types, e.g., element(), comment(), etc., resulting in "simplified"
  union types that contain multiple occurrences of these node item
  types.

  Old code:
  let atomic_types = List.filter is_atomic_cxtype no_nones in
  let complex_types = List.filter (fun x -> not (is_atomic_cxtype x)) no_nones in 
  let derived_atomic_type t1 t2 =
    match (t1.pcxtype_desc, t2.pcxtype_desc) with
    | CAtomicRef r1, CAtomicRef r2 -> Schema_judge.derives_from tr r1 r2
    | _ -> false
  in
  let rec remove_atomic_dups before after = 
    match after with 
    | [] -> before
    | t1 :: rest ->
	if (List.exists (derived_atomic_type t1) before)
        || (List.exists (derived_atomic_type t1) rest)
	then remove_atomic_dups before rest
	else remove_atomic_dups (t1::before) rest
  in
  let nodups = (remove_atomic_dups [] atomic_types) @ complex_types in 
*)
  let conservative_syntactic_subtype t1 t2 = 
    try
      Schema_judge.is_syntactic_subtype_of tr t1 t2
    with
    | Query(Unknown _) -> false
  in
  let rec remove_dups before after = 
    match after with 
    | [] -> before
    | t1 :: rest ->
	if (List.exists (conservative_syntactic_subtype t1) before)
        || (List.exists (conservative_syntactic_subtype t1) rest)
	then remove_dups before rest
	else remove_dups (t1::before) rest
  in
  let nodups = remove_dups [] no_nones in 
  if Debug.default_debug() then
    begin
      Debug.sprintf_default_debug ("After remove duplicates: %d") (List.length nodups);
      List.iter (fun x -> Debug.print_default_debug (bprintf_cxtype "\n" x)) nodups
    end;
  let ty = Schema_util.choice_of_list nodups in 
  if Debug.default_debug() then
    begin
      Debug.print_default_debug "After choice_of_list:";
      Debug.print_default_debug (bprintf_cxtype "" ty)
    end;
  ty

(* rec_simplify_ty is applied recursively until a fixed point is
   reached.  simplify_one_ty returns a pair; the second value is a
   boolean indicating whether the type expression was changed and
   therefore it is necessary to recurse.  
*)
and rec_simplify_ty tr t =
    let (t',b) = simplify_one_ty tr t in
    if Debug.default_debug() then
      begin
	Debug.print_default_debug ("After simplify_one_ty: "^(string_of_bool b));
	Debug.print_default_debug (bprintf_cxtype ";" t')
      end;
    if b
    then rec_simplify_ty tr t'
    else t'

(* A type description is only simplified once *)
let simplify_ty tr t =
  if (t.pcxtype_simplified) then 
    t
  else
    let t' = rec_simplify_ty tr t in 
    begin
      if Debug.default_debug()
      then Debug.print_default_debug (bprintf_cxtype "*** REPLACING " t);
      t.pcxtype_desc <- t'.pcxtype_desc;
      t.pcxtype_simplified <- true;
      if Debug.default_debug()
      then Debug.print_default_debug (bprintf_cxtype "*** WITH " t);
      t
    end


