(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* Module: Xquery_algebra_ast_annotation_util
   Description:
     This module also provides functions access annotations on the algebra AST. 
*)

open Error

open Xquery_common_ast
open Xquery_algebra_ast
open Xquery_algebra_ast_util
open Xquery_physical_type_ast


(************************)
(* Annotation accessors *)
(************************)

let empty_annotation = 
  mk_annotation [] [] [] [] ([], [], NoTable)

let access_annotation x = 
  match x.compile_annotations with
  | None -> empty_annotation 
  | Some s -> s


(**********************)
(* Accessor functions *)
(**********************)

let get_accessed_fields  x       = x.accessed_fields
let get_tuple_field_use_counts x = x.tuple_field_use_counts
let get_returned_fields  x       = x.returned_fields
let get_free_variables   x       = List.map (fun (a,_) -> a) x.use_counts
let get_use_counts       x       = x.use_counts
let get_bound_use_counts x       = x.bound_usage_counts
     
let algop_get_accessed_fields  x       = get_accessed_fields  (access_annotation x)
let algop_get_tuple_field_use_counts x = get_tuple_field_use_counts (access_annotation x)
let algop_get_returned_fields  x       = get_returned_fields  (access_annotation x)
let algop_get_free_variables   x       = get_free_variables   (access_annotation x)
let algop_get_use_counts       x       = get_use_counts       (access_annotation x)
let algop_get_bound_use_counts x       = get_bound_use_counts (access_annotation x)

(* Materialization Check *)

let materialization_check output_sig input_sig msg_constructor = 
  match (output_sig, input_sig) with
  | (PT_XML s1, PT_XML s2) -> 
      begin
	match (s1, s2) with 
	| (PT_Sax PT_Stream, PT_Dom _) -> msg_constructor "Sax to XML Materialization"
	| (PT_Dom s1', PT_Dom s2') -> 
	    begin
	      match s1', s2' with 
	      |  (PT_CursorSeq, PT_ListSeq) -> msg_constructor "Item Cursor to Item List materialization"
	      |  (PT_ListSeq, PT_CursorSeq) -> msg_constructor "Export item list to item cursor"
	      |  (_, _) -> []
	    end
	| (PT_Sax PT_Discarded, _) -> msg_constructor "Converting from Sax Unit"
	| (_, PT_Sax PT_Discarded) -> msg_constructor "Converting to Sax Unit"
	| (_, PT_Sax PT_Stream) -> msg_constructor "Exporting XML to SAX"
      end
  |  _  -> [] (* Must be a list *)
(*  | _ -> msg_constructor "no materialization or export" *)

(* Some more stuff *)

let check_data_signatures msg_constructor output_sig input_sig = 
  match  (input_sig, output_sig) with
    | NoInput, _ -> 
	raise (Query (Algebra_Parsing_Error ("should not be checking for materialization with no input")))
    | OneInput inp, [outp] -> 
	materialization_check outp inp (msg_constructor "Input 1: ")
    | TwoInput (inp1, inp2), [outp1;outp2] ->
	(materialization_check outp1 inp1 (msg_constructor "Input 1: ")) 
	@ (materialization_check outp2 inp2 (msg_constructor "Input 2: "))
    | ManyInput in_many_array, _  ->
	let many_array = Array.of_list output_sig in
	let returned = Array.mapi 
			 (fun index cur_out -> 
			    materialization_check cur_out (Array.get in_many_array index) (msg_constructor ("Input " ^ (string_of_int (index+1)) ^ ":")))
	   many_array in
	  List.concat (Array.to_list returned)
    | _ -> 
	let n_input_str = 
	  match input_sig with
	    | NoInput     -> "No Input"
	    | OneInput _  -> "One Input"
	    | TwoInput _  -> "Two Input"
	    | ManyInput _ -> "Many Input"
	in
	raise (Query (Algebra_Parsing_Error ("Mismatch input signature "  ^ n_input_str ^
				  " to inputs: " ^ (string_of_int (List.length output_sig)))) )


let check_signatures msg_constructor our_signature inputs = 
  let get_output_sig aop = 
    match aop.palgop_expr_eval_sig with
      | None -> raise (Query (Algebra_Parsing_Error ("Signature not filled in - check_signatures")))
      | Some sign ->
	  let (_, _, output_s) =  sign in output_s
  in
    
  let our_input_sig = 
    match our_signature with 
      | None   -> raise (Query (Algebra_Parsing_Error ("Signature not filled in - check_signatures")))
      | Some (_, our_input, _) -> our_input
  in
  match inputs with 
      NoSub -> []
    | OneSub s -> 
	check_data_signatures msg_constructor [(get_output_sig s)] our_input_sig
    | TwoSub (s0, s1) -> 
	check_data_signatures msg_constructor [(get_output_sig s0); (get_output_sig s1)] our_input_sig
    | ManySub s ->
	check_data_signatures msg_constructor (List.map get_output_sig (Array.to_list s)) our_input_sig


let rec strip_annotation op = 
  let strip_subexpr expr = 
    match expr with
      | NoSub   -> NoSub
      | OneSub o       -> OneSub(strip_annotation o)
      | TwoSub (s0,s1) -> TwoSub((strip_annotation s0),
				 (strip_annotation s1))
      | ManySub s      -> 
	  ManySub (Array.map strip_annotation s)
  in
    logical_aalgop_mkop 
      op.palgop_expr_name 
      (strip_subexpr op.psub_expression)
      (strip_subexpr op.pdep_sub_expression)
      op.compile_annotations 
      op.palgop_expr_origin 
      op.palgop_expr_loc 

let rec deep_copy_expr { palgop_expr_name = op_name;
			 palgop_expr_eval = eval_code_ref;
			 palgop_expr_eval_sig = eval_sig;
			 annotation = annot;
			 compile_annotations = comp_annot;
			 psub_expression = indep_expressions;
			 pdep_sub_expression = dep_expressions;
			 palgop_expr_origin = e_handle;  
			 palgop_expr_loc = expr_loc;}  = 
  
  algop_mkop !eval_code_ref eval_sig op_name 
    (deep_copy_sexpr indep_expressions) 
    (deep_copy_sexpr dep_expressions) annot comp_annot e_handle expr_loc

and deep_copy_sexpr sexpr = 
  match sexpr with
    | NoSub -> NoSub
    | OneSub o -> OneSub (deep_copy_expr o)
    | TwoSub (o1,o2) -> 
	TwoSub ((deep_copy_expr o1), (deep_copy_expr o2))
    | ManySub ops ->
	ManySub (Array.map deep_copy_expr ops)

(* Prints an annotation *)

let print_annot a =
  Printf.printf "Annotation\n";
  Printf.printf "----------\n";
  Printf.printf "\tAccessed fields: ";
  List.iter (fun x -> Printf.printf "%s;" (Namespace_names.prefixed_string_of_rqname x)) a.accessed_fields;
  Printf.printf "\n\tReturned fields: ";
  List.iter (fun x -> Printf.printf "%s;" (Namespace_names.prefixed_string_of_rqname x)) a.returned_fields;
  Printf.printf "\n----------\n";
  flush stdout

