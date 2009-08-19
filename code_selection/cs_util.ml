(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: cs_util.ml,v 1.38 2007/02/21 21:14:43 simeon Exp $ *)

(* Module: Cs_util
   Description:
     This module contains some auxiliary evaluation functions, notably
     for function calls, type declarations, axis, element and
     attribute construction.
*)

open Format

open Error

(* open Namespace_symbols *)

open Datatypes
open Datatypes_util

open Xquery_ast
open Xquery_algebra_ast
open Xquery_algebra_ast_util
open Xquery_common_ast
open Xquery_algebra_ast_annotation_util

open Physical_xml_value
open Physical_value
open Physical_value_util

open Norm_context
open Typing_context
open Code_selection_context
open Variable_context_manager


(*************)
(* Constants *)
(*************)

(* idiomatic constants that appear in multiple locations *)
let empty_dom_sequence     = (Physical_sequence.materialized_of_list [])
let non_empty_dom_sequence = (Physical_sequence.materialized_of_list ([Item_Atomic Dm_atomic_util.integer_one]))

let empty_sequence     = (DomValue empty_dom_sequence)
let non_empty_sequence = (DomValue non_empty_dom_sequence)

let empty_tuple        = Physical_table.empty_tuple
let empty_tuple_opt    = Some empty_tuple (* Useful in cursor returns *)

(******************)
(* Error messages *)
(******************)

let raise_document_element_singleton () =
  raise (Query (Prototype "document node does not contain a singleton children element"))

let raise_computed_tag_error () =
  raise (Query (Code_Selection "Computed tag not a Qname or string"))

let raise_cast_to_symbol_failure () =
  raise (Query (Code_Selection "Failure in Cs_util.cast_to_symbol"))

let raise_cast_to_bool_failure () =
  raise (Query (Code_Selection "Failure in Cs_util.cast_to_bool"))

let raise_in_forest_compare msg =
  raise (Query(Datamodel("In forest_compare :"^msg)))

let raise_type_error_in_function_arguments fn =
  raise (Query (Type_Error ("Arguments to function '"
				 ^ (Namespace_names.prefixed_string_of_rqname fn) 
				 ^ "' do not match function's signature.\n")))
let get_physical_opname algop = 
  match algop.palgop_expr_eval_sig with
  | None -> raise(Query(Code_Selection("Physical operator missing for logical operator "^
				       Xquery_algebra_ast_util.string_of_algop_expr_name algop.palgop_expr_name)))
  | Some (physop, _, _) -> physop

(*************************************)
(*********** Code Building ***********)
(*************************************)

let build_add_var_xml_value_with_ref code_ctxt vr = 
  let f = build_variable_store_code vr in
    (fun xv ->
     let mv = materialize_xml_value xv in
       f mv)

let build_add_var_item_list code_ctxt vname = 
  let f = build_current_insert_code code_ctxt vname in
  (fun il -> 
    let mv = xml_value_of_item_list il in
     f mv)

(* Code that can also have "unsafe" (i.e. non-materialized versions) *)

(* XML Values *)
let build_unsafe_fn code_ctxt bShould vn f =
  (* Converts a stream into the corresponding item sequence if streams are not
     explicitly allowed. This mimics the former conservative behaviour which was
     enforced at the Ocaml function signature level. - Michael *)
  if (bShould && not(!Conf.force_materialized_variables))
  then
    begin
      let buc =
	(get_bound_use_counts (retrieve_annotation "build_unsafe_fn" code_ctxt))
      in
      try
	match (List.assoc vn buc) with
	| (0,Never) -> (* unused variable *)
	    (fun xv -> ())
	| (1,Once) -> (* used only once, set unsafely *)
	    (fun xv -> f xv)
	| _ -> (* Should materialize *)
	    (fun xv -> f (materialize_xml_value xv))
      with Not_found ->
	raise (Query (Code_Selection ("Variable " 
				      ^ (Namespace_names.prefixed_string_of_rqname vn) ^
				      " not listed in bound during unsafe code building")))
    end
  else
    (fun xv -> f (materialize_xml_value xv))

(* Now binding an arbitrary xml_value here. - Michael *)
let build_add_var_xml_value code_ctxt bunsafe vname =
  let f = build_current_insert_code code_ctxt vname in
  let nf = build_unsafe_fn code_ctxt bunsafe vname f in 
  (fun xv -> nf xv)

(* item cursors *)
let build_add_var_item_cursor code_ctxt bunsafe vname =
  let f = build_current_insert_code code_ctxt vname in
  let f = build_unsafe_fn code_ctxt bunsafe vname f in 

  (fun ic ->
     let cv = xml_value_of_item_cursor ic in
       f cv)


(*********************)
(* These are exposed *)
(*********************)

let build_add_var_xml_value_unsafe_allowed code_ctxt vname =
  build_add_var_xml_value code_ctxt true vname 

let build_add_var_xml_value_safe code_ctxt vname =
  build_add_var_xml_value code_ctxt false vname 


let build_add_var_item_cursor_unsafe_allowed code_ctxt vname =
  build_add_var_item_cursor code_ctxt true vname

let build_add_var_item_cursor_safe code_ctxt vname =
  build_add_var_item_cursor code_ctxt false vname


(**************************************)
(********* RETRIEVING SECTION *********)
(**************************************)

let build_physical_value_retrieve code_ctxt vn =
  build_current_retrieve_code code_ctxt vn

let build_var_xml_value_retrieve code_ctxt vn =
  let retrieve = build_current_retrieve_code code_ctxt vn in
  (fun () -> (retrieve ()))

let build_var_item_list_retrieve code_ctxt vn =
  let retrieve = build_current_retrieve_code code_ctxt vn in
  (fun () -> item_list_of_xml_value (retrieve ()))


(****************)
(* Join Helpers *)
(****************)

let inputs_are_fs_untyped_to_any op =
  (* We know it is binary *)
  let ops = Xquery_algebra_ast_util.access_manysub op.psub_expression in 
  let l,r = (ops.(0), ops.(1)) in
    (is_fs_untyped_to_any l) && 
    (is_fs_untyped_to_any r)

(**********************************************)
(* This promotes according to all options of  *)
(*   fs:untyped-to-any                        *)
(* It has the semantic of value comparison
   type promotion. This is used in 
   distinct value comparisons for example.    *)
(**********************************************)
(* NOTE ABOUT xs:decimal. We promote it to double here. This
   fact is used below in our promotions. *)
let promote_to_highest t = 
  match t with
  | ATInteger | ATDecimal | ATFloat ->
      Some (Namespace_symbols_builtin.xs_double, ATDouble)
  | ATDouble -> None
	(* Untyped converted to double below, if possible *)
  | ATUntypedAtomic -> Some (Namespace_symbols_builtin.xs_string, ATString)
  | _ -> None

let promote_atomicValue_to_highest nsenv av = 
  let at = av#getAtomicValueKind () in
  match at with
  | ATInteger | ATDecimal | ATFloat ->
      av#cast_to nsenv Namespace_symbols_builtin.xs_double ATDouble
  | ATDouble -> av
	(* Untyped converted to double below, if possible *)
  | ATUntypedAtomic -> 
      av#cast_to nsenv Namespace_symbols_builtin.xs_string ATString
  | _ -> av

let promote_atomicValue_to_all nsenv av =
  let at = av#getAtomicValueKind () in
  match at with
  | ATUntypedAtomic ->
      [av;
       av#cast_to nsenv Namespace_symbols_builtin.xs_string ATString]
  | ATInteger ->
      [av;
       av#cast_to nsenv Namespace_symbols_builtin.xs_decimal ATDecimal;
       av#cast_to nsenv Namespace_symbols_builtin.xs_float ATFloat;
       av#cast_to nsenv Namespace_symbols_builtin.xs_double ATDouble]
  | ATDecimal ->
      [av;
       av#cast_to nsenv Namespace_symbols_builtin.xs_float ATFloat;
       av#cast_to nsenv Namespace_symbols_builtin.xs_double ATDouble]
  | ATFloat ->
      [av;
       av#cast_to nsenv Namespace_symbols_builtin.xs_double ATDouble]
  | ATDouble ->
      [av]
  | ATYearMonthDuration | ATDayTimeDuration ->
      [av;av#cast_to nsenv Namespace_symbols_builtin.xs_duration ATDuration]
  | _ ->
      [av]

(* Handles the fs_untyped to any semantic
   
   Returns a pair (v1,v2 option).

   If the value is typed: 

   v1 is the value promoted to its "highest" type.
   v2 is the value in its original type 
         or None if type(v1) = type(v2)

   If the value is untyped:
   v1 is it as a string
   v2 is the value promoted to a double if possible
*)
let handle_fs_untyped_to_any_semantic nsenv av =
  (* Promote numeric types -> double
     promote untyped       -> string *)   
  let orig_at =  (av#getAtomicValueKind ()) in
  let changed = promote_to_highest orig_at in
  let v1 = (* Cast it if necessary *)
    match changed with
    | None -> av
    | Some (new_atn, new_at) ->
	av#cast_to nsenv new_atn new_at
  in
  let v2 =
    (* This is nasty - but ocaml does not 
       provide a simple way to check if this condition
       is valid (essentially whether float_of_string will fail).
       So it makes us use exceptions on the critical path
       of the join - we need to eliminate this *)
    (* This should probably be hand-coded as lexing doubles
       is absurdly expensive for what it is doing *)
    match orig_at with      
    | ATUntypedAtomic -> 
	begin
	  try
	    Some (av#cast_to nsenv Namespace_symbols_builtin.xs_double ATDouble)
	  with  _ ->
	      None
	end
    | _ -> match changed with Some _ -> Some av | None -> None 
  in
    v1, v2

(**********************************************************)
(* RQName demangling for Distributed XQuery dxq: protocol *)
(**********************************************************)
let mangle_prefix = Pcre.regexp "^dxq:"

let dxq_demangle_rqname = function
    (x,Namespace_names.NSUri y,z) ->
      if Pcre.pmatch ~rex:mangle_prefix y then
        let demangled_y = Pcre.replace ~rex:mangle_prefix ~templ:"" y in
        Some (x,Namespace_names.NSUri demangled_y,z)
      else None
  | _ -> None




