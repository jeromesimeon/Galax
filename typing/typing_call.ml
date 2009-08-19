(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: typing_call.ml,v 1.34 2008/03/25 01:47:28 mff Exp $ *)

(* Module: Typing_call
   Description:
     This module implements support for static typing of function
     calls.
 *)

open Format

open Error
open Occurrence

open Namespace_names
open Norm_context
open Processing_context

open Datatypes
open Datatypes_util

open Xquery_common_ast
open Xquery_core_ast
open Xquery_type_core_ast
open Xquery_ast_util

open Xquery_type_core_ast

open Schema_simplification
open Schema_util
open Subtyping_top

open Typing_context
open Typing_errors
open Typing_util

open Print_top

(*

  4.1.5 Function Calls
  
  Static Type Analysis
  
  Different sets of static typing rules are used to type check
  function calls depending on which of the following categories the
  belong to: overloaded internal functions, built-in functions with a
  specific typing rule, and other built-in and user-defined functions.

  The following rule is common to all those categories, and is used to
  bootstrap type inference, by first looking-up the expanded QName for
  the function, then applying the appropriate set of inference rule
  depending on the category in which the function is.

  statEnv |-  QName of func expands to expanded-QName
  statEnv |-  Expr1 : Type1
  ...
  statEnv |-  Exprn : Typen
  statEnv  |-  expanded-QName(Type1,...,Typen) : Type
  ---------------------------------------------------
  statEnv  |-  QName (Expr1,...,Exprn) : Type
  
  The following depends on the kind of function call.
  
  1. If the expanded QName for the function corresponds to one of the
     overloaded internal fs: functions listed in [B.2 Mapping of
     Overloaded Internal Functions], the rules in [B.2 Mapping of
     Overloaded Internal Functions] are applied.

  2. If the expanded QName for the function corresponds to one of the
     built-in functions with a specialized typing rule, listed in [7
     Additional Semantics of Functions], the rules in [7 Additional
     Semantics of Functions] are applied.

  3.  Otherwise, the following general rule is applied.

  The rule looks up the function in the static environment and checks
  that some signature for the function satisfies the following
  constraint: the type of each actual argument is a subtype of some
  type that can be promoted to the type of the corresponding function
  parameter. In this case, the function call is well typed and the
  result type is the return type specified in the function's
  signature.

  statEnv.funcType(expanded-QName,n) = declare function expanded-QName(Type1', ..., Typen') as Type'
  statEnv  |-  Type1 can be promoted to Type1'
  ...
  statEnv  |-  Typen can be promoted to Typen'
  -----------------------------------------------------
  statEnv  |-  expanded-QName(Type1, ..., Typen) : Type'
  
  The function body itself is not analyzed for each invocation: static
  typing of the function definition itself guarantees that the
  function body always returns a value of the declared return type.
  
*)

(*************************)
(* Normal function calls *)
(*************************)

(* Non-overloaded function calls:

   Check that the argument types are subtypes of expected parameter types. 

   Normalization of non-overloaded function calls takes care of
   conversion and promotion, so no extra work is done here.
*)

let build_user_defined_type_rule fn optinput_types =
  (fun static_ctxt fi argument_types return_type -> 
    try
      let schema = schema_from_static_context static_ctxt in
      let type_check_one_argument t1 opt_type2 = 
	begin
	  match opt_type2 with
	  | None ->
	      raise
		(Query (Malformed_Expr
			  ("Malformed call to function "
			   ^ (Namespace_names.prefixed_string_of_rqname fn))))
	  | Some (sequencetype2, t2) ->
	      if (not(Subtyping_top.is_subtype_of schema t1 t2)) then
		let _ = raise_wrong_expected_type_error static_ctxt t1 t2 in false
	      else true
	end
      in
      let _ = (List.for_all2 type_check_one_argument argument_types optinput_types) in
      return_type
    with
    | Invalid_argument _ -> 
	raise (Norm_util.incorrect_arg_count fn (List.length argument_types) (List.length optinput_types)))

let compute_type_normal_function_call typing_ctxt fn arguments_types (optinput_types, output_type) =
  let (out_sequencetype, output_cxtype) = output_type in 
  let type_rule =
    try
      let arity = List.length arguments_types in
      Typing_fn.lookup_bltin_fctn_type_rule (fn,arity)
    with
    | _ ->
	build_user_defined_type_rule fn optinput_types
  in
  type_rule typing_ctxt Finfo.bogus arguments_types output_cxtype

(************************)
(* Overloaded functions *)
(************************)


(* Matches a function signature *)

(* match_function_signature finds the first function signature such
   that the types of the functions arguments match the signature, after
   promotion.  

    Finally, the resulting type is obtained by performing type
    promotion and accessing the operators mapping table (using the
    operator type for judgment defined below).
  
    statEnv  |-  AtomicType1 can be promoted to AtomicType1'
    statEnv  |-  AtomicType2 can be promoted to AtomicType2'
    statEnv |-  operator type for AtomicType1 and AtomicType2 is AtomicType3
    ------------------------------------------------------------------------
    statEnv  |-  expanded-QName(AtomicType1,AtomicType2) : AtomicType3
    
    statEnv  |-  AtomicType1 can be promoted to AtomicType1'
    statEnv |-  operator type for AtomicType1 is AtomicType3
    ------------------------------------------------------
    statEnv  |-  expanded-QName(AtomicType1) : AtomicType3

*)

let wrap_promotion schema actual expected =
  Typing_util.can_be_promoted_to_judge schema actual expected

let match_function_signature typing_ctxt fn arguments_types signatures =
  let schema = schema_from_static_context typing_ctxt in
  let norm_ctxt = (norm_context_from_stat_context typing_ctxt) in
  let proc_ctxt = (processing_context_from_norm_context norm_ctxt) in
  let rec args_match_signature signatures = 
    match signatures with
    | [] -> raise Not_found
    | (fn', (input_types, output_type), opt_fun_kind, upd) :: sigs ->
	begin
	  let (out_sequencetype, output_cxtype) = output_type in 
	  let expected_input_types = List.map (fun (_, expected) -> expected) input_types in
	  let vts = List.combine arguments_types expected_input_types in
	  let targs' = Gmisc.unwrap_option_list(
	    List.map (fun (actual, expected) ->
	      wrap_promotion schema actual expected
		     ) vts)
	  in 
	  if (List.length arguments_types = List.length targs') then
	    (fn', (targs', output_cxtype), upd)
	  else
           (* If we bottom out here, it means that no signature
	      matched.  In the case of weak typing, we are optimistic,
	      and use the most general (least specific) signature of the overloaded
	      function, which is the *???* one tried.
	   *)
            if (sigs = [] && is_weak_typing proc_ctxt) then 
	      begin
(*
print_string("Defaulting to "^(prefixed_string_of_rqname fn')^"\n");
print_string("Output type "^(Print_top.bprintf_cxtype "" output_cxtype)^"\n");
*)
		(fn', (expected_input_types, output_cxtype), upd)
	      end
	    else args_match_signature sigs
	end
  in 
  try
    args_match_signature signatures 
  with
  | Not_found -> 
      let input_types = String.concat "," (List.map (bprintf_cxtype "") arguments_types) in
      raise
	(Query
	   (Static_Type_Error (("Arguments to function '"
				^ (Namespace_names.prefixed_string_of_rqname fn)
				^ "("
				^ input_types
				^ ")' do not match function's signature.\n"))))

let match_signature_on_first_arg typing_ctxt fn actual signatures =
  let schema = schema_from_static_context typing_ctxt in
  let norm_ctxt = (norm_context_from_stat_context typing_ctxt) in
  let proc_ctxt = (processing_context_from_norm_context norm_ctxt) in
  let rec args_match_signature signatures = 
    match signatures with
    | [] -> if (is_weak_typing proc_ctxt) then [] else raise Not_found
    | (fn', (input_types, output_type), opt_fun_kind, upd) :: sigs ->
	begin
	  let (out_sequencetype, output_cxtype) = output_type in 
	  let expected_input_types = List.map (fun (_, expected) -> expected) input_types in
	  let expected = List.hd expected_input_types in
	  let promoted_type_opt = Typing_util.can_be_promoted_to_judge schema actual expected in
	  match promoted_type_opt with
	  | Some promoted_type -> 
	    (fn', (promoted_type :: (List.tl expected_input_types), output_cxtype)) :: (args_match_signature sigs)
	  | None -> args_match_signature sigs
	end
  in 
  try
    args_match_signature signatures 
  with
  | Not_found -> 
      let input_type = (bprintf_cxtype "") actual in
      raise
	(Query
	   (Static_Type_Error (("First argument to function '"
				^ (Namespace_names.prefixed_string_of_rqname fn)
				^ "("
				^ input_type
				^ ")' does not match function's signature.\n"))))

(* Note:
     match_overloaded_function_signature looks up most specific function
     that implements argument function applied to arguments with given
     types.
  - Jerome *)

let match_overloaded_function_signature typing_ctxt fn argument_types overloaded_signature_table =
  (* Pick the first function whose signature matches the types of the
     function arguments, after type promotion *)
  try
    let (fn', (input_types,  output_type), upd) =
      match_function_signature typing_ctxt fn argument_types overloaded_signature_table
    in
    (fn', input_types, output_type, upd)
  with
  | _ -> (fn, argument_types, Schema_builtin.cxtype_item_star, NonUpdating)

(*

  B.2 Mapping of Overloaded Internal Functions

Static Type Analysis

  The following static typing rules apply generically to all the
  overloaded fs: special functions. They do not apply to any other
  function calls, which are treated in [4.1.5 Function Calls].

  Step 1. First, if the static type of one of the expressions passed as
  argument is a union of atomic types, the function call is type
  checked once separately for each atomic type in that union. The
  static type of the entire function call expression is then the union
  of the types computed in each case.

  Type1 = (OptAtomicType1,1|...|OptAtomicTypem,1)
  Type2 = (OptAtomicType1,2|...|OptAtomicTypen,2)
  statEnv  |-  expanded-QName(OptAtomicType1,1, OptAtomicType1,2) : OptAtomicType1'
  ...
  statEnv  |-  expanded-QName(OptAtomicTypem,1, OptAtomicTypem,n,2) : OptAtomicTyper'
  -------------------------------------------------------------------------------------
  statEnv  |-  expanded-QName(Type1, Type2) : (OptAtomicType1'|...|OptAtomicTyper')

  Note that this approach can be used since the type declared for a
  function parameter can never itself be a union.
*)

(* Overloaded function calls *)
let compute_type_overloaded_function_call typing_ctxt fn argument_types overloaded_signature_table =
  let schema = schema_from_static_context typing_ctxt in

  (* If overloaded function has custom typing rule, use that rule *)
  let arity = List.length argument_types in
  if Typing_fn.has_special_type_rule (fn, arity) then 
    let (fn', (input_types,  output_type),  _) =
      match_function_signature typing_ctxt fn argument_types overloaded_signature_table
    in
    let type_rule = Typing_fn.lookup_bltin_fctn_type_rule (fn',arity)
    (* I'm not sure whether we should apply the built_in type rule to
       the original argument types or the input types, after type
       promotion.  The rules for aggregate functions in
       Typing_fn._agg_type assume that the rule is applied to the
       types after promotion.  
    *)
    in
    let ret = type_rule typing_ctxt Finfo.bogus argument_types output_type in
    ret

  else 
    begin
      (* Step 1: Expand out union type of first argument only, to
         identify potential signatures that match on first argument *)
      let (first_unit_args, snd_arg_opt, m, n) =
	expand_first_overloaded_argument schema argument_types
      in
      (* Step 2: Handle empty case  *)
      if (Occurrence.equal n Occurrence.occurs_zero)
      then Schema_util.cxtype_empty
      else
	begin
	  let output_types = 
	    (* Step 3. For each unit type in first input argument type, find
               all signatures that match on the first argument. *)
	    List.concat(List.map (fun first_arg_type -> 
(*	      print_string("Matching on "^(prefixed_string_of_rqname fn)^":"^(Print_top.bprintf_cxtype "" first_arg_type)^"\n"); *)
	      let sig_list =
		match_signature_on_first_arg typing_ctxt fn first_arg_type overloaded_signature_table
	      in

  	     (* We factor the output type here, because the quantifier
                is a function of the input type's quantifier, not the
                output type's.  *)
	      match snd_arg_opt with
	      | None -> 
		  if (List.length sig_list > 0) then
	        (* Step 4a.  For unary functions, the first matching function is the best match. *)
		    let (fn',(_, output_type)) = (List.hd sig_list) in 
		    let (pl, _, _) = factor_with_units output_type in pl
		  else []
	      | Some snd_arg_type ->
                (* Step 4b. For binary function, for each unit type in second input argument type, 
                   select first signature for which unit type matches second expected type.  
    	        *)
		  let (pl1,m1,n1) = factor_with_units snd_arg_type in
		  let rec check_snd_arg_type sig_list p1 = 
		    begin
		      match sig_list with 
		      | [] -> []
		      | (fn',(expected_input_types, output_type))::rest_sigs ->
			  begin
			    let (pl2,m2,n2) = factor_with_units (List.hd(List.tl expected_input_types)) in
			    if (Occurrence.le m2 m1 && Occurrence.le n1 n2)
			    then 
			      (* Quadratic comparison of unit types in pl1 and pl2 !! *)
			      if (List.exists (fun p2 ->
				Gmisc.is_some (can_be_promoted_to_judge schema p1 p2)
				  ) pl2)
			      then let (pl,_,_) = factor_with_units output_type in pl 
			      else check_snd_arg_type rest_sigs p1
			    else check_snd_arg_type rest_sigs p1
			  end
		    end
		  in List.concat(List.map (check_snd_arg_type sig_list) pl1))
		    first_unit_args)
	  in
	  (* Exclude those prime types that are already in the output type *)
          (* Mary (2/2006) See Schema_simplification.simplify_union
             for similar code & comment *)
(*	  print_string ("Output types "^(String.concat " " (List.map (Print_top.bprintf_cxtype "") output_types))^"\n"); *)
	  let output_types = 
	    Gmisc.sort_and_remove_duplicates_revcompare (fun a b -> compare b.pcxtype_desc a.pcxtype_desc) output_types 
	  in
	  (* Step 5: If either one of the types of the operands is
     	     optional, the type obtained by propagating the optional
     	     occurrence indicator. *)
	  if (Occurrence.is_one (m,n)) then
	    choice_of_list output_types
	  else
	    Xquery_type_core_ast_util.fmkcxtype (CBound(choice_of_list output_types, m, n)) Finfo.bogus
	end
    end

