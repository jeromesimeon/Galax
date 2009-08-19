(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: compile_context.ml,v 1.28 2007/05/16 15:32:09 mff Exp $ *)

(* Module: Compile_context
   Description:
     This module contains context information used during algebraic
     compilation.
*)

open Namespace_util
open Namespace_names

open Norm_context
open Typing_context

open Xquery_algebra_ast
open Xquery_common_ast

open Error


(***********************)
(* Compilation context *)
(***********************)

type ('a,'b) compile_context =
    { compiled_static_context    : static_context;
      compiled_functions 	 : (('a,'b) aalgop_function_body) RQNameIntHashtbl.t;
      compiled_variables 	 : (crname) RQNameHashtbl.t; 
      mutable compiled_has_input : bool;
      next_variable              : Namespace_generate.name_gen ref}

(* Creates a new compilation context *)

let build_compile_context stat_ctxt =
  let mod_ctxt = Norm_context.module_context_from_norm_context (Typing_context.norm_context_from_stat_context stat_ctxt) in 
  let ng =
    Processing_context.get_name_generator
      mod_ctxt Namespace_builtin.glx_prefix Namespace_builtin.glx_uri ""
  in
  { compiled_static_context = stat_ctxt;
    compiled_functions 	    = RQNameIntHashtbl.create 167;
    compiled_variables 	    = RQNameHashtbl.create 167;
    compiled_has_input      = false;
    next_variable           = ng }

(* Default compilation context *)

let default_compile_context norm_ctxt =
  let default_stat_ctxt = default_static_context norm_ctxt in
  build_compile_context default_stat_ctxt

(* Replace the static context *)

let replace_static_context_in_compile_context stat_ctxt comp_ctxt =
  { compiled_static_context = stat_ctxt;
    compiled_functions 	    = comp_ctxt.compiled_functions;
    compiled_variables 	    = comp_ctxt.compiled_variables;
    compiled_has_input      = comp_ctxt.compiled_has_input;
    next_variable           = comp_ctxt.next_variable }

(* Accesses parts of the static context from the compilation context *)

let static_context_from_compile_context c = c.compiled_static_context

(* Replace the namespace environment *)

let replace_namespace_env_in_compile_context nsenv comp_ctxt =
  (* Mary : This is gross... *)
  let stat_ctxt = static_context_from_compile_context comp_ctxt in
  let norm_ctxt = norm_context_from_stat_context stat_ctxt in
  let norm_ctxt' = replace_namespace_env_in_norm_context nsenv norm_ctxt in
  let stat_ctxt' = replace_norm_context_in_static_context norm_ctxt' stat_ctxt in
  replace_static_context_in_compile_context stat_ctxt' comp_ctxt

let norm_context_from_compile_context alg_ctxt =
  norm_context_from_stat_context (static_context_from_compile_context alg_ctxt)

(***************************)
(* Treatement of functions *)
(***************************)

let add_function_to_compile_context comp_ctxt (cfname,arity) fb =
  if (RQNameIntHashtbl.mem comp_ctxt.compiled_functions (cfname,arity))
  then
    raise (Query (Symbol_Already_Defined ("Function ", 
					  (prefixed_string_of_rqname cfname))))
  else
    RQNameIntHashtbl.add comp_ctxt.compiled_functions (cfname,arity) fb
  
let get_function_from_compile_context msg comp_ctxt (cfname,arity) =
  try
    RQNameIntHashtbl.find (comp_ctxt.compiled_functions) (cfname,arity) 
  with
  | Not_found ->
      raise (Query (Undefined(msg^"Function "
			      ^ (curly_uri_string_of_rqname cfname)
			      ^ " with arity "
			      ^ (string_of_int arity)
			      ^ " not found in compile context.")))

let mem_function_from_compile_context comp_ctxt (cfname,arity)  =
  RQNameIntHashtbl.mem comp_ctxt.compiled_functions (cfname,arity) 

let update_physical_plan_in_compile_context comp_ctxt (name,arity) body = 
  let func_defn = get_function_from_compile_context "update_physical_plan" comp_ctxt (name,arity) in 
  match !(func_defn.palgop_func_physical_plan) with
  | None ->  func_defn.palgop_func_physical_plan := Some body
  | Some _ -> raise(Query(Code_Selection("Physical plan for "^(Namespace_names.prefixed_string_of_rqname name)^" already defined.")))

(* Built in helper functions *)

let builtin_fn_hash = RQNameIntHashtbl.create 167;;
let register_builtin (cfname,arity) =
  RQNameIntHashtbl.add builtin_fn_hash (cfname,arity) ();;

let is_builtin (cfname,arity) =
  RQNameIntHashtbl.mem builtin_fn_hash (cfname,arity)

(***************************)
(* Treatement of variables *)
(***************************)

let copy_compile_context comp_ctxt =
  { compiled_static_context = comp_ctxt.compiled_static_context;
    compiled_functions 	    = RQNameIntHashtbl.copy comp_ctxt.compiled_functions;
    compiled_variables 	    = RQNameHashtbl.copy comp_ctxt.compiled_variables;
    compiled_has_input      = comp_ctxt.compiled_has_input;
    next_variable           = comp_ctxt.next_variable}

let has_input_set comp_ctxt =
  comp_ctxt.compiled_has_input

let get_new_name comp_ctxt orig_name =
  Namespace_generate.generate_name_with_prefix !(comp_ctxt.next_variable) orig_name

let get_new_group_name comp_ctxt = get_new_name comp_ctxt "group_created"
let get_new_dot_name comp_ctxt = get_new_name comp_ctxt "dot_new"
let get_new_var_name comp_ctxt = get_new_name comp_ctxt "var_new"

let get_new_tuple_name comp_ctxt cvname =
  let (_,_, orig_name) = cvname in
  get_new_name comp_ctxt orig_name
let get_new_variable_name comp_ctxt cvname = get_new_name comp_ctxt cvname

(* Assumes we know it to be a tuple field name.. *)
let get_tuple_field_name comp_ctxt cvname =
  try
    RQNameHashtbl.find comp_ctxt.compiled_variables cvname 
  with Not_found ->
    raise (Query (Compilation ("Looking for tuple field " ^ 
			       (Namespace_names.prefixed_string_of_rqname cvname)
			       ^ " but has not been compiled")))

let add_variable_field_to_compile_context comp_ctxt cvname =
  let comp_ctxt' = copy_compile_context comp_ctxt in
    begin
      let crname = get_new_tuple_name comp_ctxt' cvname in
      RQNameHashtbl.replace comp_ctxt'.compiled_variables cvname crname;
      comp_ctxt'.compiled_has_input <- true;
      comp_ctxt'
    end

let hide_variable_field_from_compile_context comp_ctxt cvname =
  let comp_ctxt' = copy_compile_context comp_ctxt in
  begin
    RQNameHashtbl.remove comp_ctxt'.compiled_variables cvname;
    comp_ctxt'
  end

let get_variable_field_from_compile_context comp_ctxt cvname =
  if (RQNameHashtbl.mem comp_ctxt.compiled_variables cvname)
  then
    Some (RQNameHashtbl.find comp_ctxt.compiled_variables cvname)
  else
    None

let no_more_input comp_ctxt =
  comp_ctxt.compiled_has_input <- false

(* External Build functions *)
let update_compile_context_from_module comp_ctxt m =
  let fns = m.palgop_module_prolog.palgop_prolog_functions in 
  List.iter (fun fdecl  ->
	       let (name, _, body, _) = fdecl.palgop_function_decl_desc in
	       add_function_to_compile_context comp_ctxt name body)
    fns;
    comp_ctxt

let copy_without_functions comp_ctxt = 
  { compiled_static_context = comp_ctxt.compiled_static_context;
    compiled_functions 	    = RQNameIntHashtbl.create 167;
    compiled_variables 	    = RQNameHashtbl.copy comp_ctxt.compiled_variables;
    compiled_has_input      = comp_ctxt.compiled_has_input;
    next_variable           = comp_ctxt.next_variable}


let map_function_bodies comp_ctxt fn = 
  let ht = RQNameIntHashtbl.create 167 in   
    RQNameIntHashtbl.iter (fn ht) comp_ctxt.compiled_functions;
  { compiled_static_context = comp_ctxt.compiled_static_context;
    compiled_functions 	    = ht;
    compiled_variables 	    = RQNameHashtbl.copy comp_ctxt.compiled_variables;
    compiled_has_input      = comp_ctxt.compiled_has_input;
    next_variable           = comp_ctxt.next_variable}
  

(* If a binding from context_2 is in context_1, context_1's binding is replaced *)
let update_compile_context context_1 context_2 = 
  let check_fn ht key binding =
    if RQNameIntHashtbl.mem context_1.compiled_functions key
    then RQNameIntHashtbl.add ht key (RQNameIntHashtbl.find context_1.compiled_functions key)
    else RQNameIntHashtbl.add ht key binding
  in
    map_function_bodies context_2 check_fn 


(* let _ = print_string("Compile_context\n") *)
