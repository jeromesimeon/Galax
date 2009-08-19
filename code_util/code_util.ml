(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_util.ml,v 1.19 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Code_util
   Description:
     This module contains some auxiliary evaluation code.
*)

open Error
open Datatypes
open Datatypes_util

open Xquery_ast
open Xquery_algebra_ast
open Xquery_common_ast

open Physical_value
open Physical_sequence
open Physical_item
open Physical_value_util

open Norm_context
open Typing_context

open Code_util_matching

(******************)
(* Error messages *)
(******************)

let raise_computed_tag_error () =
  raise (Query (Code_Selection "Computed tag not a string, untypedAtomic, or QName value"))

let raise_cast_to_symbol_failure () =
  raise (Query (Code_Selection "Failure in Cs_util.cast_to_symbol: Expected string, untypedAtomic or QName value"))

let raise_cast_to_bool_failure () =
  raise (Query (Code_Selection "Failure in Cs_util.cast_to_bool: Expected boolean value"))

let raise_in_forest_compare msg =
  raise (Query(Datamodel("In forest_compare :"^msg)))

let raise_type_error_in_function_arguments fn =
  raise (Query (Type_Error ("Arguments to function '"
				 ^ (Namespace_names.prefixed_string_of_rqname fn) 
				 ^ "' do not match function's signature.\n")))


(**********************)
(* Debugging messages *)
(**********************)

let debugging_base_type_and_atomic_type bt at =
  if Debug.default_debug()
  then Debug.print_default_debug
      ("Base type "
       ^(string_of_atomic_type bt)
       ^" atomic type "^(string_of_atomic_type at)^"\n")
  else ()

let debugging_fn fn =
  if Debug.default_debug()
  then Debug.print_default_debug (Namespace_names.prefixed_string_of_rqname fn)
  else ()


(***********************************)
(* Auxiliary functions for sorting *)
(***********************************)

(* Note:
     This part of the code deals contains XPath sort related
     functions.
   - Jerome *)

(* Compare function between two arbitrary forest *)

(* Note:
     This function takes ascending/descending and
     emptygreatest/emptyleast into account.
   - Jerome *)

let make_atomic_gt i1 i2 =
  let a1 = getAtomicValue i1 in
  let a2 = getAtomicValue i2 in
  a1#atomic_value_gt(a2)

let forest_compare sk esk if1 if2 op_gt =
  let ascending_compare =
    (* First, extract the actual values for each forests *)
    let oi1 = Physical_util.get_optional_item if1
    and oi2 = Physical_util.get_optional_item if2 in
    match (oi1,oi2) with
    (* If both forests are empty, then they are equal *)
    | (None, None) ->
	0
	  (* If only one of the forests is empty, then look at whether
   	     empty is greatest or least *)
    | (None, _) ->
	begin
	  match esk with
	  | EmptyGreatest ->
	      1
	  | EmptyLeast ->
	      -1
	end
    | (_, None) ->
	begin
	  match esk with
	  | EmptyGreatest ->
	      -1
	  | EmptyLeast ->
	      1
	end
	  (* Otherwise, use the provided comparison operator function
             from the data model -- Both arguments should be atomic
             values, because atomization is always applied. *)
    | (Some i1, Some i2) ->
	try
	  if is_nan i1
	  then
	    begin
	      match esk with
	      | EmptyGreatest ->
		  1
	      | EmptyLeast ->
		  -1
	    end
	  else if is_nan i2
	  then
	    begin
	      match esk with
	      | EmptyGreatest ->
		  -1
	      | EmptyLeast ->
		  1
	    end
	  else if op_gt i1 i2 then 1
	  else if op_gt i2 i1 then -1
	  else 0
	with
	| Query(Datamodel(msg)) ->
	    raise_in_forest_compare (((string_value i1)^" it2\n" ^ (string_value i2))^"\n"^msg)
  in
  (* Finally, deal with the sorting kind ascending or descending *)
  match sk with
  | Ascending ->
      ascending_compare
  | Descending ->
      -ascending_compare


(******************************************)
(* Auxiliary functions for function calls *)
(******************************************)

(* promotes_to checks that a value can be promoted to a given
   sequencetype and returns the promoted value. 

  If the value matches the target type, then it is promoted to itself

   Value matches Type
   -----------------------------------
   Value against Type promotes to Value

  If the value does not match the target type, but matches a type
  which can be promoted to the target type, then the value is cast to
  the target type.

  statEnv |- Value1 matches Type1
  statEnv |- Type1 can be promoted to Type2
  statEnv |- Type1 != Type2
  cast as Type2 (Value1) => Value2 
  --------------------------------------------------
  statEnv |- Value1 against Type2 promotes to Value2

  Value promotes to Type as Value'
  ----------------------------------
  Value converts to Type as Value'

  Value : untypedAtomic
  Type <: xs:anySimpleType
  cast Value as (Type) => Value'
  ------------------------------
  Value converts to Type as Value'

*)

let promotes_to stat_ctxt dt =
  let cxschema = schema_from_static_context stat_ctxt in
  (* Extracts the item and occurrence from the sequence type *)
  let (adtk,b1,b2) = Typing_util.factor_asequencetype dt in
  match adtk with
  | AITEmpty ->
      (fun arg ->
	(* The sequence type is empty, check that the input cursor is empty *)
	if (arg = [])
	then (Some arg)
	else None)
  | AITAtomic bt1 ->
      let bt = Schema_judge.atomic_type_of_typename cxschema bt1 in
      (* For base types, promotion may require casting *)
      let norm_ctxt = norm_context_from_stat_context stat_ctxt in
      let nsenv = nsenv_from_norm_context norm_ctxt in
      (fun arg ->
	let b = Occurrence.occurs (List.length arg) in
	if not((Occurrence.le b b2) && (Occurrence.le b1 b))
	then None
	else
	  let result =
	    let promote_item_to item =
	      let av = getAtomicValue item in
	      let at = av#getAtomicValueKind() in
	      (* untypedAtomic data is cast to the target type *)
	      if (at = ATUntypedAtomic) then
		begin
		  try
		    (Item_Atomic (av#cast_to nsenv bt1 bt))
		  with
		  | Query(Cast_Error(_)) -> raise Not_found
		  | Query(Validation(_)) -> raise Not_found
		end
		  (* All other atomic types are promoted *)
	      else
		begin
		  (* debugging_base_type_and_atomic_type bt at; *)
		  let is_same_type, b_can_be_promoted = 
		    bt_can_be_promoted_to bt at
		  in
		  if b_can_be_promoted then
		    if (is_same_type)
		    then item
		    else (Item_Atomic (av#cast_to nsenv bt1 bt))
		  else raise Not_found
		end
	    in
	    try
	      Some (List.map promote_item_to arg)
	    with
	    | Not_found -> None
	  in
	  result)
  | _ ->
      (fun arg ->
	let c = Cursor.cursor_of_list arg in
	try
	  Some
	    (Cursor.list_of_cursor "Cs_util.promotes_to"
	       (dynamic_type_check stat_ctxt dt c))
	with
	| _ ->
	    None)

(* The match_overloaded_function finds the first function signature
   such that the given actual function arguments match the signature,
   after promotion. *)
let match_overloaded_function stat_ctxt fn signatures =
  let make_promotes_to_fun signature =
    match signature with
    | (fn,(input_types, output_type), upd) ->
	(fn,List.map (promotes_to stat_ctxt) input_types)
  in
  let promotes_to_funs =
    List.map make_promotes_to_fun signatures
  in
  (fun eargs_cursor ->
    let eargs =
      List.map (Cursor.list_of_cursor "Cs_util.match_overloaded_function") eargs_cursor
    in
    let rec args_match_signature ptfs =
      match ptfs with
      | [] -> raise Not_found
      | (fn,promotes_to_fun) :: ptfs' ->
	  begin
	    let vts = List.combine eargs promotes_to_fun in
	    let eargs' =
	      Gmisc.unwrap_option_list
		(List.map (fun (arg,ptf) -> ptf arg) vts)
	    in
	    if (List.length eargs = List.length eargs')
	    then
	      begin
		(* debugging_fn fn; *)
		(fn, eargs')
	      end
	    else
	      args_match_signature ptfs'
	  end
    in
    try
      let (fn,args) = args_match_signature promotes_to_funs in
      (fn, List.map Cursor.cursor_of_list args)
    with
      Not_found ->
	raise_type_error_in_function_arguments fn)

let get_computed_node_name nsenv pv1 =
  let av = Physical_util.get_singleton_atomic pv1 in
  match av#getAtomicValueKind() with
  | ATQName -> av#getAtomicQName()
  | ATString -> Datatypes_util.qname_of_untyped nsenv (av#getAtomicString())
  | ATUntypedAtomic -> Datatypes_util.qname_of_untyped nsenv (av#getAtomicUntyped())
  | _ -> raise (Query(Type_Error("Expected xs:QName, xs:string or xs:untypedAtomic value")))

