(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: typing_top.ml,v 1.19 2007/07/01 15:57:16 mff Exp $ *)

(* Module: Typing_top
   Description:
     This module implements the static typing feature.
 *)

open Format

open Error
open Occurrence

open Namespace_names

open Datatypes

open Xquery_common_ast
open Xquery_common_ast_util

open Xquery_core_ast
open Xquery_core_ast_util

open Xquery_type_core_ast
open Xquery_type_core_ast_util

open Typing_context
open Typing_errors
open Typing_expr

open Print_top


(***********************)
(* Prolog declarations *)
(***********************)

(* Global variables *)

let type_var_decls typing_ctxt var_decls =
  let type_one_variable ctxt var_decl =
    match var_decl.pcvar_decl_desc with
    | (vn, odt, CEVarUser ve) ->
	begin
	  let t1 = type_cexpr ctxt ve in
	  let t1' = check_type_declaration typing_ctxt t1 odt in
(* Print_top.printf_cxtype ("\nAdd var "^(Namespace_names.prefixed_string_of_rqname vn)^" with type ") t1'; *)
	  let ctxt' = add_var_to_static_context ctxt (vn, t1') in
	  ctxt'
	end
    | (vn, Some (dt, t1), cevar_kind) ->
	begin
(*
Print_top.printf_cxschema "\nInput Schema : " schema; 
Print_top.printf_cxtype ("\nAdd var "^(Namespace_names.prefixed_string_of_rqname vn)^" with type ") t1;
*)
	  let ctxt' = add_var_to_static_context ctxt (vn, t1) in
	  ctxt'
	end
    | (vn, None, cevar_kind) ->
	begin
	  let t1 = Schema_builtin.cxtype_item_star in
(* Print_top.printf_cxtype ("\nAdd var "^(Namespace_names.prefixed_string_of_rqname vn)^" with type ") t1; *)
	  let ctxt' = add_var_to_static_context ctxt (vn, t1) in
	  ctxt'
	end
  in
  let typing_ctxt' = List.fold_left type_one_variable typing_ctxt var_decls in
  typing_ctxt'


(* Function declarations *)

let type_function_def typing_ctxt f =    
  match f.pcfunction_def_desc with
  | (_, _, _, CEFunctionInterface, _)
  | (_, _, _, CEFunctionImported, _)
  | (_, _, _, CEFunctionBltIn, _) -> ()
  | ((fname,arity),
     vars,
     (input_types, (output_dt, output_ty)),
     CEFunctionUser e, _) -> 
       if (List.length vars) != (List.length input_types)
       then
	 raise (Query (Wrong_Args
			 ("Incompatible number of elements in function"
			  ^ (Namespace_names.prefixed_string_of_rqname fname))))
       else
	 begin
	   (* adds asumptions about variables for functions in the environment *)
	   let vars_with_types = (List.combine vars input_types) in
           let typing_ctxt' = List.fold_right 
	       (fun (var, (dt, cty)) tenv ->
		 add_var_to_static_context tenv (var,cty)) vars_with_types typing_ctxt
	   in
	   (* then checks the type of the function's expression with the
	      above assumptions *)
           ignore(type_cexpr_with_replace typing_ctxt' e output_ty)
	 end

(* Key declarations *)

let type_cindex_def stat_ctx cindex_def = 
  match cindex_def.pcindex_def_desc with
  | CValueIndex(name, e1, e2) ->
      (* What should be the type for index expressions, notably the index
	 itself? *)
      begin
	ignore(type_cexpr stat_ctx e1);
	ignore(type_cexpr stat_ctx e2)
      end
  | CNameIndex name -> ()


(**************)
(* Statements *)
(**************)

let type_cstatement typing_ctxt cs = 
  let cxtype = type_cexpr typing_ctxt cs in
  if (!Conf.print_type) then 
    begin
      let t = Print_top.bprintf_cxtype "" cxtype in 
      Print_top.print_escaped_output
	!Conf.type_formatter
	!Conf.type_header t
	!Conf.type_footer;
      flush stdout
    end;
  cxtype


(***********)
(* Modules *)
(***********)
let type_cprolog typing_ctxt prolog =
  let typing_ctxt' = type_var_decls typing_ctxt prolog.pcprolog_vars in
  begin
    List.iter (type_function_def typing_ctxt') prolog.pcprolog_functions;
    List.iter (type_cindex_def typing_ctxt') prolog.pcprolog_indices;
    typing_ctxt'
  end

let type_cxmodule typing_ctxt qm =
  let typing_ctxt' = type_cprolog typing_ctxt qm.pcmodule_prolog in
  begin
    List.iter
      (fun cs -> 
	ignore(type_cstatement typing_ctxt' cs))
      qm.pcmodule_statements;
    typing_ctxt'
  end

(**********************)
(* Toplevel functions *)
(**********************)

let typing_type_cexpr static_context cexpr =
  let cxtype = type_cexpr static_context cexpr in
  if (!Conf.print_type) then 
    begin
      let t = Print_top.bprintf_cxtype "" cxtype in 
      Print_top.print_escaped_output
	!Conf.type_formatter
	!Conf.type_header t
	!Conf.type_footer;
      flush stdout
    end;
  static_context

let typing_type_cstatement static_context cstatement =
  let _ = type_cstatement static_context cstatement in
  static_context

let typing_type_cprolog static_context cprolog =
  let new_static_context = type_cprolog static_context cprolog in
  new_static_context

let typing_type_cxmodule static_context cxmodule =
  let new_static_context = type_cxmodule static_context cxmodule in
  new_static_context

