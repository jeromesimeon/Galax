(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: evaluation_expr.ml,v 1.18 2007/03/18 21:38:01 mff Exp $ *)

(* Module: Evaluation_expr
   Description:
     Execute the code associated to each algebraic operation
*)

open Algebra_type
open Xquery_algebra_ast
open Error

(* NOTE:

   The reason we have this first step that instantiate the dependant
   expressions is because those may have changed in the course of
   algebraic rewriting.

   A better strategy would be to change the evaluation to instantiate
   all of the code in the AST *before* starting actual evaluation.

   - J & C *)


let has_delta_update op = 
  let {has_delta_update=hdu} = op.annotation in
    hdu

let has_two_deltas_sexprs sexpr = 
  match sexpr with 
    | NoSub  
    | OneSub _ -> false
    | TwoSub (op1,op2) ->	
	(has_delta_update op1) &&
	(has_delta_update op2)

    | ManySub ops ->
	let count_fold cur op =
	  if has_delta_update op 
	  then (cur+1)
	  else cur
	in
	let counts = Array.fold_left count_fold 0 ops in
	  counts >= 2 
	    
let update_allocate_holder_if_needed alg_ctxt subexpr = 
  match Execution_context.get_current_snap_semantic alg_ctxt with
    | Xquery_common_ast.Snap_Ordered_Deterministic   ->
	if has_two_deltas_sexprs subexpr
	then Execution_context.allocate_update_holder alg_ctxt
	else alg_ctxt

    | Xquery_common_ast.Snap_Unordered_Deterministic 
    | Xquery_common_ast.Snap_Nondeterministic        -> 
	(* Here we do not need to allocate holders because it is done else where *)
	alg_ctxt


(****************************************)
(* Physical typing debug code - Michael *)
(****************************************)

open Xquery_physical_type_ast
open Print_xquery_physical_type
open Physical_value

let counter_check_physical_type algop pv =
  let string_of_physical_value pv =
    match pv with
      | PXMLValue (SaxValue _) -> "PXMLValue (SaxValue _)"
      | PXMLValue (DomValue (CSeq _)) -> "PXMLValue (DomValue (CSeq _))"
      | PXMLValue (DomValue (LSeq _)) -> "PXMLValue (DomValue (LSeq _))"
      | PTable _ -> "PTable _"
  in
  let raise_type_mismatch pt =
    let physop = Cs_util.get_physical_opname algop in
      raise (Query (Prototype ("Physical type mismatch -  " ^ (string_of_physical_type pt) ^ " vs. " ^ (string_of_physical_value pv)
			       ^ " at " ^ (string_of_physop_expr_name physop) ^ "! This should have been compiled out (evaluation_expr)!")))
  in
  let counter_check_physical_sax_type pt =
    match pv with
      | PXMLValue (SaxValue _) -> ()
      | _ -> raise_type_mismatch pt
  in
  let counter_check_physical_dom_cursor_type pt =
    match pv with
      | PXMLValue (DomValue (CSeq _)) -> ()
      | _ -> raise_type_mismatch pt
  in
  let counter_check_physical_dom_list_type pt =
    match pv with
      | PXMLValue (DomValue (LSeq _)) -> ()
      | _ -> raise_type_mismatch pt
  in
  let counter_check_physical_table_type pt =
    match pv with
      | PTable _ -> ()
      | _ -> raise_type_mismatch pt
  in
    match algop.palgop_expr_eval_sig with
      | Some (_, _, pt) ->
	  begin
	    match pt with
	      | PT_Table _ -> counter_check_physical_table_type pt
	      | PT_XML (PT_Sax PT_Stream)-> counter_check_physical_sax_type pt
	      | PT_XML (PT_Sax PT_Discarded)-> raise_type_mismatch pt
	      | PT_XML (PT_Dom PT_CursorSeq) -> counter_check_physical_dom_cursor_type pt
	      | PT_XML (PT_Dom PT_ListSeq) -> counter_check_physical_dom_list_type pt
	  end
      | None -> ()
	  
let memtotal = ref None
let memlive = ref None
(* This function must be tail recursive as well!!!! *)

let recmem msg = 
  let st = (Gc.full_major(); Gc.stat ()) in
  Printf.eprintf "%s %d\n%!" msg st.Gc.live_words

(* Should be compiled out! - Michael *)
let bcounter_check_physical_type = false

let empty_sequence = Physical_value_util.empty_xml_value ()
  

(*
*  Algebra_iterative_evaluate evaluates a top-level algebraic plan or 
*  a user-defined function call, contained in "alg_term".  In both cases,
*  an XQuery "stack frame" (heap-allocated code context) is allocated
*  upon entry.  Any tail-recursive function call in "alg_term" is
*  evaluated in the _same_ stack frame.
*
*  Evaluation continues in the current stack frame as long as
*  tail-recursive calls occur in alg_term.
*
*  See algebra_execute_aux below to see how tail-recursive calls are
*  detected.
*)
(************************************************************)
(* DO NOT TOUCH THIS CODE WITHOUT CONSULTING MARY OR JEROME *)
(************************************************************)
let count = ref 0
let rec algebra_iterative_evaluate alg_ctxt (alg_term: algop_expr) =
  let tail_recursive_call = ref true in
  let tail_alg_term = ref alg_term in
  let result_ref = ref (Physical_value_util.empty_xml_value ()) in 
  while (!tail_recursive_call) do 
    tail_recursive_call := false;
    result_ref := algebra_execute_aux (tail_recursive_call, tail_alg_term) alg_ctxt (!tail_alg_term);
  done; 
  !result_ref

(*
  Algebra_execute_aux recursively evaluates an algebraic plan in "alg_term". 

  Recursion terminates in two ways:
  1. A leaf operator in the algebraic plan is evaluated or
  2. A tail recursive function call is encountered. 

  In case 2, all calls to algebra_execute_aux are exited/unrolled upto
  the last call to algebra_iterative_evaluate, which evaluated the
  containing function.  Evaluation of the tail call continues from
  this point, re-using the same XQuery stack frame as that allocated
  for the containing function.
*)
(************************************************************)
(* DO NOT TOUCH THIS CODE WITHOUT CONSULTING MARY OR JEROME *)
(************************************************************)
and algebra_execute_aux (tail_recursive_call, tail_alg_term) alg_ctxt (alg_term: algop_expr) =
    
  (* The algebraic operator contains the file location that should be
     correlated with errors, so we catch any dynamic exceptions here
     and rewrap the exceptions with the file location. 
  *)
(*  
   Printf.printf ("=> %s@%d\n%!") (Xquery_algebra_ast_util.string_of_algop_expr_name  alg_term.palgop_expr_name) !count; 
   count := !count + 1;
*)
  begin
    let fi = (alg_term).palgop_expr_loc in  
    let v = 
    (try
      begin
	(* 1. Instantiate function "actual_code", which implements the
	      algebraic operator and "tailcode", which implements a
	      tail-recursive function call.  

           If operator is a non-tail-recursive function call, the
           function is evaluated in a new stack frame, and the
           function's body is evaluated by algebra_iterative_evaluate,
           which keeps track of the current frame.
        *)
	let (actual_code, tailcode) =
	  match !(alg_term.palgop_expr_eval) with      
	  | NoDep (f, tailcode) -> 
	      begin
		match alg_term.palgop_expr_name with
		| AOECallUserDefined ((cfname, arity), _, _, _, false) -> 
		    (f algebra_iterative_evaluate, tailcode) 
		| _ -> (f (algebra_execute_aux (tail_recursive_call, tail_alg_term)), tailcode)
	      end
	  | SomeDep (f, tailcode) -> 
	      begin
		match alg_term.palgop_expr_name with
		| AOECallUserDefined ((cfname, arity), _, _, _, false) -> 
		    (f algebra_iterative_evaluate, tailcode) 
		| _ -> (f (algebra_execute_aux (tail_recursive_call, tail_alg_term)), tailcode)
	      end
	in 

	(* 2. Evaluate independent sub-expressions, then call the code
 	      that implements the operator itself.  
	*)
	let sub_alg_terms = (alg_term).psub_expression in
	begin
	  match (sub_alg_terms, actual_code) with
	  | (NoSub,AOECUnit f) ->
	      f alg_ctxt () 
	  | (OneSub alg1,AOECUnary f) ->
	      let ac1    = update_allocate_holder_if_needed alg_ctxt sub_alg_terms in 
	      let input1 = algebra_execute_aux (tail_recursive_call, tail_alg_term) ac1 alg1 in
	      f alg_ctxt input1
	  | (TwoSub (alg1,alg2),AOECBinary f) ->	
	      let ac1    = update_allocate_holder_if_needed alg_ctxt sub_alg_terms in 
	      let ac2    = update_allocate_holder_if_needed alg_ctxt sub_alg_terms in 
	      let input1 = algebra_execute_aux (tail_recursive_call, tail_alg_term) ac1 alg1 in
	      let input2 = algebra_execute_aux (tail_recursive_call, tail_alg_term) ac2 alg2 in
	      f alg_ctxt input1 input2
	  | (ManySub algs, AOECMany f) ->
	      begin
		let acs    = Array.map 
		    (fun _ -> update_allocate_holder_if_needed alg_ctxt sub_alg_terms) algs in 
		let inputs = Array.mapi 
		    (fun i alg -> algebra_execute_aux (tail_recursive_call, tail_alg_term) acs.(i) alg) algs in

    	      (* 3. If the operator is tail-recursive-function call,
                    recursion terminates.  The tail call's "entry code", which
                    updates the current stack frame with the
                    function's actual arguments, is evaluated; the
                    current plan is replaced by the body of the
                    tail-call; and the empty sequence is returned.
	      *)
		match alg_term.palgop_expr_name with
		| AOECallUserDefined ((cfname, arity), _, _, _, true) -> 
		    begin
		      match tailcode with
		      | None -> 
			  raise (Query(Code_Selection("No tail-recursive code for "^
						      Namespace_names.prefixed_string_of_rqname cfname)))
		      | Some (entry_code, exit_code) -> 
			  begin
			    tail_recursive_call := true;
			    let body = entry_code inputs in 
			    tail_alg_term := body; 
			    empty_sequence 
			  end;
		    end
		| _ ->  
		    f alg_ctxt inputs
	      end
	  | _ ->
	      raise (Query (Malformed_Algebra_Expr 
			      "Number of input sub-algebraic operations does not match code arity"))
	end
      end
    with
    | exn -> raise (error_with_file_location fi exn)) in

(*****************************************************************)
(* IF YOU PUT ANY CODE HERE, YOU RISK MAKING algebra_execute_aux *)
(* non-tail-recursive and possibly cause a memory leak.          *)
(*****************************************************************)
(* 
   count := !count - 1;
   Printf.printf ("<= %s@%d\n%!") 
   (Xquery_algebra_ast_util.string_of_algop_expr_name  alg_term.palgop_expr_name) !count; 
*)
    v
  end

(************************************************************)
(* DO NOT TOUCH THIS CODE WITHOUT CONSULTING MARY OR JEROME *)
(************************************************************)
let algebra_execute alg_ctxt (alg_term: algop_expr) = 
  let v =  algebra_iterative_evaluate alg_ctxt alg_term in
  v

  (*****************************************)
  (* Physical typing debug code - Michael *)
  (****************************************)
(*
  if (bcounter_check_physical_type)
  then (counter_check_physical_type alg_term result);
*)
