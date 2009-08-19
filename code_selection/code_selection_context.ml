(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_selection_context.ml,v 1.54 2007/10/16 01:25:34 mff Exp $ *)

(* Module: Code_selection_context
     This module implements the context used during code selection.
*)

open Error
open Namespace_util

open Xquery_common_ast
open Xquery_algebra_ast

open Compile_context
open Variable_context_manager
open Tuple_context_manager

(* This must walk through only once! *)
(* Association list used for scoping issues *)

(* Contexts must have nesting... *)
type context_name = 
  | GlobalContext
      (* this should be removed - we don't need it *)
      (* Statements are nested inside modules... *)
  | StatementContext
  | PrologContext
  | ClosureContext
  | FunctionContext of cfname_arity

let ctxt_string context_name = 
 match context_name with 
  | GlobalContext -> "global" 
  | StatementContext -> "statement"
  | PrologContext -> "prolog" 
  | ClosureContext -> "closure"
  | FunctionContext cfname -> "function"

type external_var_retrieve_code = (unit -> Physical_value.item list) ref

type cs_function_code =
    { parameter_insertion_code : (Physical_value.xml_value -> unit) array;
      entrance_code  : (unit -> unit);
      exit_code      : (unit -> unit) }


(******************************************************************)
(* Note: If you change these from lists, you must update the copy.*)
(*   These should be used in a functional way.                    *)
(******************************************************************)

type var_map   = (cvname * variable_ref) list
type tuple_map = (cvname * tuple_ref) list

type code_selection_context =
    { mutable compile_ctxt          : Algebra_type.alg_compile_context;
      mutable code_type_ctxt        : Code_typing_context.code_type_context;
      (* To handle contexts *)
      var_context_stack     : (context_name * variable_context_manager * tuple_context_manager) Stack.t;
      function_context_map  : (variable_context_manager * var_map * tuple_context_manager) Namespace_util.RQNameIntHashtbl.t;
      imported_functions   : code_selection_context Namespace_util.RQNameIntHashtbl.t; 

      imported_variables    : (code_selection_context) Namespace_util.RQNameHashtbl.t;
      mutable external_variables    : (cvname * external_var_retrieve_code) list;

      (* to handle scoping of var names *)
      mutable scoped_variables      : var_map;

      (* Tuple related stuff *)
      (* Tuples are not passed between functions so scoping is our issue *)
      mutable scoped_tuples         : tuple_map;
      (* Current annotation *)
      mutable current_annotation    : Xquery_algebra_ast.free_variable_desc option;

      (* For certain kinds of information (such as tuple field use
         counts), local annotations are not quite appropriate, because
         local information has to be combined with information from
         further up in the AST in order to be meaningful.

	 The scope of the global annotation is defined implicitly by
	 store_global_annotation function calls, and should mirror
	 'statements'. A cleaner solution would be to use the
	 var_context_stack (which would then have to allow a more
	 fine-grained treatment of query prologs. - Michael *)
      mutable global_annotation     : Xquery_algebra_ast.free_variable_desc option;
      mutable value_indices : Physical_value_index.value_indices_hash;
      mutable name_indices  : Physical_name_index.name_indices_hash;
      mutable in_remote_execute_operator : bool }

let print_imported_functions code_ctxt =
Debug.print_dxq_debug ("*funs:"); RQNameIntHashtbl.iter (fun (vn,a) c -> Debug.print_dxq_debug  ((Namespace_names.prefixed_string_of_rqname vn)^":"^(string_of_int a))) code_ctxt.imported_functions

let print_imported_variables code_ctxt =
Debug.print_dxq_debug ("*vars:"); RQNameHashtbl.iter (fun vn c -> Debug.print_dxq_debug  ((Namespace_names.prefixed_string_of_rqname vn))) code_ctxt.imported_variables

let print_imported str code_ctxt =
Debug.print_dxq_debug (str^"*imported"); print_imported_functions code_ctxt; print_imported_variables code_ctxt; Debug.print_dxq_debug "\n"

(********************************)
(* Accessors for other contexts *)
(********************************)

let cxschema_from_code_selection_context csc =
  let norm_ctxt = norm_context_from_compile_context csc.compile_ctxt in
  Norm_context.cxschema_from_norm_context norm_ctxt

let annotated_compile_context_from_code_selection_context csc = 
  csc.compile_ctxt

let compile_context_from_code_selection_context csc = 
  Compile_context_util.copy_strip_functions csc.compile_ctxt

let static_context_from_code_selection_context csc = 
  static_context_from_compile_context csc.compile_ctxt

let code_type_context_from_code_selection_context csc = 
  csc.code_type_ctxt

(*************************************)
(* Construct code selection context *)
(*************************************)

let build_code_selection_context ctc comp_ctxt =
  let st = Stack.create () in
  let clean_variable_context = Variable_context_manager.build_context_manager () in  
  let clean_tuple_context    = Tuple_context_manager.build_context_manager () in
  begin
    Stack.push (GlobalContext,  clean_variable_context, clean_tuple_context) st;
    (* Create a global context - this may change*)
    { compile_ctxt         = comp_ctxt;
      code_type_ctxt       = ctc;
      var_context_stack    = st;
      function_context_map = Namespace_util.RQNameIntHashtbl.create 167;
      imported_functions   = Namespace_util.RQNameIntHashtbl.create 7;
      imported_variables   = Namespace_util.RQNameHashtbl.create 7;
      scoped_variables     = [];
      external_variables   = [];
      scoped_tuples        = [];
      current_annotation   = None;
      global_annotation    = None;
      value_indices        = Physical_value_index.init_value_indices_hash ();
      name_indices         = Physical_name_index.init_name_indices_hash ();
      in_remote_execute_operator = false }
  end

(* Keep this local *)
let mk_code_selection_context comp_ctxt ctc vcs fcm ifuns sv ev iv st ca ga vi ni exop =
  { compile_ctxt         = comp_ctxt;
    code_type_ctxt       = ctc;
    var_context_stack    = vcs;
    function_context_map = fcm;
    imported_functions   = ifuns; 
    imported_variables   = iv;
    scoped_variables     = sv;
    external_variables   = ev;
    scoped_tuples        = st;
    current_annotation   = ca;
    global_annotation    = ga;
    value_indices        = vi;
    name_indices         = ni;
    in_remote_execute_operator = exop;
  }

let copy_code_selection_context code_ctxt = 
(* print_imported "copy_code_selection_context" code_ctxt; *)
  mk_code_selection_context 
    (copy_compile_context code_ctxt.compile_ctxt)
    code_ctxt.code_type_ctxt
    (Stack.copy code_ctxt.var_context_stack)
    (Namespace_util.RQNameIntHashtbl.copy code_ctxt.function_context_map)
    code_ctxt.imported_functions
    code_ctxt.scoped_variables
    code_ctxt.external_variables
    code_ctxt.imported_variables
    code_ctxt.scoped_tuples
    code_ctxt.current_annotation
    code_ctxt.global_annotation
    code_ctxt.value_indices
    code_ctxt.name_indices
    code_ctxt.in_remote_execute_operator

let internal_copy_code_selection_context code_ctxt = 
  mk_code_selection_context 
    code_ctxt.compile_ctxt
    code_ctxt.code_type_ctxt
    code_ctxt.var_context_stack
    code_ctxt.function_context_map
    code_ctxt.imported_functions
    code_ctxt.scoped_variables
    code_ctxt.external_variables
    code_ctxt.imported_variables
    code_ctxt.scoped_tuples
    code_ctxt.current_annotation
    code_ctxt.global_annotation
    code_ctxt.value_indices
    code_ctxt.name_indices
    code_ctxt.in_remote_execute_operator

let update_variable_scope csc sv =   
  let csc' = internal_copy_code_selection_context csc in
  csc'.scoped_variables <- sv;
  csc'

let update_tuple_scope csc st =
  let csc' = internal_copy_code_selection_context csc in
  csc'.scoped_tuples <- st;
  csc'

(**************************************)
(********** Variable Context **********)
(**************************************)


let get_current_context_manager code_ctxt =
  try 
    let _, ctxt, tctxt = Stack.top code_ctxt.var_context_stack in
      ctxt, tctxt 
  with Stack.Empty ->
    raise (Query (Code_Selection ("No valid variable context!")))

let get_current_variable_context_manager code_ctxt =
  let vcm, _ = get_current_context_manager code_ctxt in
    vcm

let get_current_tuple_context_manager code_ctxt = 
  let _, tcm = get_current_context_manager code_ctxt in
    tcm

(* When compiling we may need a reference to this context (say f references g and g is not built)
   this causes the convention that the paramters must be in the same order in both
   places and be the first allocated in the context *)

(******************)
(* Variable stuff *)
(******************)

(* List.remove_assoc does not throw exceptions when vn is not present *)

(* functions to access the context *)
let add_variable_with_ref csc vn var_ref =
  let l = csc.scoped_variables in
  let new_list = (vn, var_ref) :: (Namespace_util.remove_rqname_assoc vn l)in
  update_variable_scope csc new_list
    
let add_variable_to_current_context csc vn = 
  let cm = get_current_variable_context_manager csc in
  let vr = get_new_variable_slot cm in
    add_variable_with_ref csc vn vr
      
(* Now returns a context and a variable *)

let get_variable_reference csc v =
  try
    Namespace_util.rqname_assoc v csc.scoped_variables
  with Not_found ->
    raise (Query (Undefined_Variable(Finfo.bogus,Namespace_names.prefixed_string_of_rqname v, "Variable not defined in code selection context")))

(***************)	      
(* Tuple stuff *)
(***************)	      

let add_tuple_reference csc tn = 
  let tm = get_current_tuple_context_manager csc in
  let tr = get_new_tuple_slot tm in
  let new_list = (tn, tr) :: (Namespace_util.remove_rqname_assoc tn csc.scoped_tuples) in
    update_tuple_scope csc new_list

let get_tuple_reference csc tn =
  try
    Namespace_util.rqname_assoc tn csc.scoped_tuples
  with Not_found ->
    raise (Query (Undefined
		    ("Tuple slot not found in context during code selection: " ^ 
		     (Namespace_names.prefixed_string_of_rqname tn))))

let get_input_tuple_fields csc = List.map fst csc.scoped_tuples

(******************)	      
(* Function stuff *)
(******************)	      

let get_function_context csc fname =
  if Namespace_util.RQNameIntHashtbl.mem csc.function_context_map fname then
    let ctxt, _, tctxt = Namespace_util.RQNameIntHashtbl.find csc.function_context_map fname in
    ctxt, tctxt
  else
    begin
      let ctxt  = Variable_context_manager.build_context_manager () in
      let tctxt = Tuple_context_manager.build_context_manager () in 
      Namespace_util.RQNameIntHashtbl.add csc.function_context_map fname (ctxt, [], tctxt);
      ctxt, tctxt
    end

(* Note: code_ctxt contains the prolog context *)
let enter_function_context code_ctxt fname vname_list = 
  (* Allocate a new one or find the previously done one *)
  let vcm, tcm = get_function_context code_ctxt fname in
  (* Need to push down the new context to keep it current *)
  Stack.push ((FunctionContext fname), vcm, tcm) code_ctxt.var_context_stack;
  (* Can functions access global variables? - I assume so...*)
  (* let code_ctxt = update_variable_scope code_ctxt [] in *)
  List.fold_left add_variable_to_current_context code_ctxt (Array.to_list vname_list)

let exit_function_context code_ctxt fname = 
  try
    let fname', ctxt, tctxt = Stack.pop code_ctxt.var_context_stack in
    match fname' with 
    | FunctionContext f when Namespace_names.rqname_int_equal fname f -> 
	(* Instantiate the context *)
	instantiate_variable_context_manager ctxt;
	instantiate_tuple_context_manager tctxt;
	(* Add in the scope *)
	Namespace_util.RQNameIntHashtbl.replace code_ctxt.function_context_map fname (ctxt, code_ctxt.scoped_variables, tctxt)
    | ctxt ->
	raise (Query (Code_Selection ("Function exiting scope incorrectly - Names differ "^(ctxt_string ctxt))))
  with Stack.Empty ->
    raise (Query (Code_Selection ("Function exiting scope incorrectly - Empty Stack")))

let get_context_name csc = 
  try
    let name, _, _ = Stack.top csc.var_context_stack in
    name
  with Stack.Empty ->
    raise (Query (Code_Selection ("Get Context Name on Empty Context")))  

(* Statement compilation *)

let enter_statement_context code_ctxt = 
  let nvcm  = Variable_context_manager.build_context_manager () in
  let ntcm  = Tuple_context_manager.build_context_manager () in
    Stack.push (StatementContext, nvcm, ntcm) code_ctxt.var_context_stack;
    code_ctxt
   (* update_variable_scope code_ctxt []  *)

let exit_statement_context code_ctxt = 
  try
    let name, ctxt, tctxt = Stack.pop code_ctxt.var_context_stack in
      match name with 
	GlobalContext (* MFF: This is a hack... *)
      |	StatementContext -> 
	    instantiate_variable_context_manager ctxt;
	    instantiate_tuple_context_manager tctxt;
	| ctxt ->
	    raise (Query (Code_Selection ("Statement exiting scope incorrectly - Names differ "^(ctxt_string ctxt))))
  with Stack.Empty ->
    raise (Query (Code_Selection ("Statement exiting scope incorrectly - Empty Stack")))  

let enter_prolog_context code_ctxt =  
  let nvcm = Variable_context_manager.build_context_manager () in
  let ntcm = Tuple_context_manager.build_context_manager () in 
    Stack.push (PrologContext, nvcm, ntcm) code_ctxt.var_context_stack;
    code_ctxt

let exit_prolog_context code_ctxt = 
  try
    let name, ctxt, tctxt = Stack.pop code_ctxt.var_context_stack in
    let _ = get_context_name code_ctxt in
    match name with 
    | PrologContext -> 
	instantiate_variable_context_manager ctxt;
	instantiate_tuple_context_manager tctxt
    | ctxt ->
	raise (Query (Code_Selection ("Prolog exiting scope incorrectly - Names differ "^(ctxt_string ctxt)))) 
  with Stack.Empty ->
    raise (Query (Code_Selection ("Prolog exiting scope incorrectly - Empty Stack")))  

(********************)	 
(* Annotation Stuff *)
(********************)

let store_annotation csc c_annot = 
  let csc' = internal_copy_code_selection_context csc in
  csc'.current_annotation <- c_annot;
  csc'
  
let store_global_annotation csc g_annot = 
  let csc' = internal_copy_code_selection_context csc in
  csc'.global_annotation <- g_annot;
  csc'

let retrieve_annotation msg csc = 
  match csc.current_annotation with
  | None ->
      raise (Query
	       (Code_Selection (msg^": Attempt to retrieve annotation from code selection"
				^ " context but, no annotations set")))
  | Some s ->
      s

let retrieve_global_annotation csc = 
  match csc.global_annotation with
  | None ->
      raise (Query (Code_Selection ("Attempt to retrieve global annotation from code selection"
				    ^ " context but, no global annotations set")))
  | Some s -> s

(*****************)
(* Code builders *)
(*****************)

let store_debug = false
let build_current_insert_code code_ctxt vn =
  let vr = get_variable_reference code_ctxt vn in
  if (store_debug) then 
    (fun x ->
      Debug.print_default_debug("Storing " ^ (Namespace_names.prefixed_string_of_rqname vn) ^ " @ " ^ (string_of_variable_ref vr));
      build_variable_store_code vr x)  
  else
    build_variable_store_code vr

let build_current_assign_code code_ctxt vn rhs =
  let vr = get_variable_reference code_ctxt vn in
  if (store_debug) then 
    (fun x ->
      Debug.print_default_debug("Assigning " ^ (Namespace_names.prefixed_string_of_rqname vn) ^ " @ " 
				^ (string_of_variable_ref vr));
      build_variable_assign_code vr rhs x)
  else
      build_variable_assign_code vr rhs


let build_current_retrieve_code code_ctxt vn =
  let vr = get_variable_reference code_ctxt vn in
  if (store_debug) then 
    (fun x ->
      Debug.print_default_debug("Retrieving " ^ (Namespace_names.prefixed_string_of_rqname vn) ^ " @ " 
				^ (string_of_variable_ref vr));
      build_variable_retrieve_code vr x)
  else
      build_variable_retrieve_code vr

let build_retrieve_tuple_code code_ctxt tn =
  let tr = get_tuple_reference code_ctxt tn in
  if (store_debug) then
    (fun x ->
      Debug.print_default_debug("Retrieving tuple " ^ (Namespace_names.prefixed_string_of_rqname tn) ^ " @ " 
				^ (string_of_tuple_ref tr));
    build_tuple_retrieve_code tr x) 
  else
    build_tuple_retrieve_code tr

let build_retrieve_dom_tuple_code code_ctxt tn =
  fun () ->
    (Physical_xml_value.dom_value_of_xml_value (build_retrieve_tuple_code code_ctxt tn ()))


let build_create_tuple_code_unsafe code_ctxt tn =
  (* Printf.printf "[build_create_tuple_code_unsafe]\n"; flush stdout; *)
  let tr = get_tuple_reference code_ctxt tn in
  if (store_debug) then 
    (fun x ->
      Debug.print_default_debug("Storing tuple " ^ (Namespace_names.prefixed_string_of_rqname tn) ^ " @ " 
				^ (string_of_tuple_ref tr));
      if (Debug.default_debug()) then
	begin
	  let l = Physical_sequence.list_of_sequence (Physical_xml_value.dom_value_of_xml_value x) in
	  List.iter (fun y -> Debug.print_default_debug("\t" ^ (Physical_item.string_value y))) l;
	  Debug.print_default_debug(" \t***** ")
	end;
      build_tuple_store_code tr x)
  else
    build_tuple_store_code tr

let build_create_tuple_code code_ctxt tn =
  (* Printf.printf "[build_create_tuple_code]\n"; flush stdout; *)
  (* Now allowing item cursors/XML token streams in tuple fields.
     Materialization is based on tuple field use counts, in complete
     analogy to variable materialization.- Michael *)
  let mat_fun =
    if !Conf.allow_streamed_tuple_fields
    then
      begin
	(* Printf.printf "Retrieving Global annotation\n"; flush stdout; *)
	let global_annotation = retrieve_global_annotation code_ctxt in
	let (tuple_field_use_counts, _, _) = global_annotation.tuple_field_use_counts in
	try
	  match (List.assoc tn tuple_field_use_counts) with
	  | (0,Never) -> (fun x -> x)
	  | (1,Once) -> (fun x -> x)
	  | _ -> (fun x -> Physical_xml_value.materialize_xml_value x)
	with Not_found ->
	  raise (Query (Code_Selection ("Tuple field " 
					^ (Namespace_names.prefixed_string_of_rqname tn) ^
					" not listed in tuple field use counts.")))	   
      end
    else
      begin
	(* Printf.printf "NOT Retrieving Global annotation\n"; flush stdout; *)
	(fun x -> Physical_xml_value.materialize_xml_value x)
      end
  in
  let prev_build = build_create_tuple_code_unsafe code_ctxt tn in
  (fun x ->
    prev_build (mat_fun x))

let build_create_dom_tuple_code_unsafe code_ctxt tn =
  (* Printf.printf "[build_create_dom_tuple_code_unsafe]\n"; flush stdout; *)
  let prev_build = build_create_tuple_code_unsafe code_ctxt tn in
  fun v ->
    (prev_build (Physical_xml_value.xml_value_of_dom_value v))

let build_create_dom_tuple_code code_ctxt tn =
  (* Printf.printf "[build_create_dom_tuple_code]\n"; flush stdout; *)
  let prev_build = build_create_tuple_code code_ctxt tn in
  fun v ->
    (prev_build (Physical_xml_value.xml_value_of_dom_value v))

let dummy_error_code = (fun pv ->
	raise (Query (Code_Selection("User-defined function, no insert built!")))) 

let build_enter_context (vcm,tcm) =
  let v_enter = build_variable_enter_context vcm in
  let t_enter = build_tuple_enter_context tcm in
  (fun () -> v_enter (); t_enter ())

let build_exit_context (vcm,tcm) =
  let v_exit = build_variable_exit_context vcm in
  let t_exit = build_tuple_exit_context tcm in
    (fun () -> t_exit (); v_exit () ) (* This reversal is not necessary right now *)

let build_function_code csc (fn,arity) vars = 
 let fn_context = get_function_context csc (fn,arity) in
 let vcm, tcm   = fn_context in 

 let local_build_code = 
   if (store_debug) then 
     (fun ctxt i v ->
       Debug.print_default_debug("Parameter " ^ (string_of_int i) ^ " Store");
       build_parameter_insert_code ctxt i v)
   else 
       build_parameter_insert_code
 in
 (* Create the insert code array *)
 let insert_code_array = Array.create arity dummy_error_code in
   begin
     for i = 0 to arity - 1 do
       insert_code_array.(i) <- local_build_code vcm i
     done;
   end;
  { parameter_insertion_code = insert_code_array;
    entrance_code = build_enter_context fn_context;
    exit_code     = build_exit_context  fn_context}

(**********************************)
(*** External variable handling ***)
(**********************************)

let invalid_external_variable_fn vn =
  (fun () -> raise (Query (Undefined_Variable (Finfo.bogus,Namespace_names.prefixed_string_of_rqname vn, "External variable declared but not in the external context"))))

let build_ext_variable_access value = (fun () -> value)


let update_external_variables code_ctxt vn uval = 
  let ext_vars = (vn, uval) :: code_ctxt.external_variables in
  let csc' = internal_copy_code_selection_context code_ctxt in
  csc'.external_variables <- ext_vars;
  csc'

(* Exposed Functions *)
let declare_external_variable code_ctxt vn =
  let code_ctxt = add_variable_to_current_context code_ctxt vn in
  let bad_fn = invalid_external_variable_fn vn in
    update_external_variables code_ctxt vn (ref bad_fn)
    
let add_external_variable_value code_ctxt vn value = 
  try 
    let fn_ref = Namespace_util.rqname_assoc vn code_ctxt.external_variables in
      fn_ref := (build_ext_variable_access value);
      code_ctxt
  with Not_found ->
    begin
    (* Silently ignoring those cases now... which is a really bad
       style. we should at least raise a warning 
    *)
      eprintf_warning ("Silently ignoring value for non-declared external variable: $" ^ (Namespace_names.prefixed_string_of_rqname vn));
      code_ctxt
    end

let get_external_variable_fn_value code_ctxt vn =
  try
    Namespace_util.rqname_assoc vn code_ctxt.external_variables
  with _ ->
    (raise (Query (Undefined_Variable(Finfo.bogus,Namespace_names.curly_uri_string_of_rqname vn, " External variable is declared, but not bound in the external context"))))
   
(**********************)
(* Imported Variables *)
(**********************)

let add_imported_variable_context  code_ctxt vn imported_code_ctxt = 
  RQNameHashtbl.add code_ctxt.imported_variables vn imported_code_ctxt

let get_imported_variable_context code_ctxt vn = 
  RQNameHashtbl.find code_ctxt.imported_variables vn 

let add_imported_function_context  code_ctxt vn_arity imported_code_ctxt = 
  RQNameIntHashtbl.add code_ctxt.imported_functions vn_arity imported_code_ctxt

let get_imported_function_context code_ctxt vn_arity = 
  RQNameIntHashtbl.find code_ctxt.imported_functions vn_arity

(************************************)
(* Construct code selection context *)
(************************************)

(* Context handling  *)
let enter_scope code_ctxt = 
(* print_imported ">>" code_ctxt; *)
code_ctxt

(* restore the variable context of the entering context *)
let restore_variables enter_ctxt cur_ctxt =
(* let _ = print_imported "restore_variables" enter_ctxt in *)
  { 
    compile_ctxt         = enter_ctxt.compile_ctxt;
    code_type_ctxt       = enter_ctxt.code_type_ctxt;
    var_context_stack    = enter_ctxt.var_context_stack;
    function_context_map = enter_ctxt.function_context_map;
    imported_functions   = enter_ctxt.imported_functions;
    (* this is the change, the others do not change during execution *)
    scoped_variables     = enter_ctxt.scoped_variables;
(* Mary: external_variables should not be effected by entering or exiting a variable scope,
   so why are they restored? *)
    external_variables   = enter_ctxt.external_variables;
    imported_variables   = enter_ctxt.imported_variables;
    scoped_tuples        = cur_ctxt.scoped_tuples;
    current_annotation   = cur_ctxt.current_annotation;
    global_annotation    = cur_ctxt.global_annotation;
    value_indices        = Physical_value_index.merge_value_indices cur_ctxt.value_indices enter_ctxt.value_indices;
    name_indices         = Physical_name_index.merge_name_indices cur_ctxt.name_indices enter_ctxt.name_indices;
    in_remote_execute_operator  = cur_ctxt.in_remote_execute_operator;}

(* Flush the variables, and if necessary the tuples *)
let exit_scope bflush_tuples enter_ctxt cur_ctxt =
(* print_imported "<<" cur_ctxt; *)
  let scoped_tuples =
    if bflush_tuples then enter_ctxt.scoped_tuples
    else cur_ctxt.scoped_tuples
  in
  { compile_ctxt         = cur_ctxt.compile_ctxt;
    code_type_ctxt       = cur_ctxt.code_type_ctxt;
    var_context_stack    = cur_ctxt.var_context_stack;
    function_context_map = cur_ctxt.function_context_map;
    imported_functions   = enter_ctxt.imported_functions;
    imported_variables   = enter_ctxt.imported_variables;
    scoped_variables     = enter_ctxt.scoped_variables;
    external_variables   = enter_ctxt.external_variables;
    scoped_tuples        = scoped_tuples;
    current_annotation   = cur_ctxt.current_annotation;
    global_annotation    = cur_ctxt.global_annotation;
    value_indices        = Physical_value_index.merge_value_indices cur_ctxt.value_indices enter_ctxt.value_indices;
    name_indices         = Physical_name_index.merge_name_indices cur_ctxt.name_indices enter_ctxt.name_indices;
    in_remote_execute_operator  = cur_ctxt.in_remote_execute_operator; }

let get_in_remote_execute_operator code_ctxt = 
  code_ctxt.in_remote_execute_operator

let set_in_remote_execute_operator code_ctxt in_remote = 
  code_ctxt.in_remote_execute_operator <- in_remote

(* Default code selection context *)

let default_code_selection_context norm_ctxt =
  let default_comp_ctxt = default_compile_context norm_ctxt in
  build_code_selection_context
    Code_typing_context.default_code_type_context default_comp_ctxt

(* Replace the compilation context *)

let replace_compile_context_in_code_selection_context comp_ctxt cs_ctxt =
  { compile_ctxt         = update_compile_context cs_ctxt.compile_ctxt comp_ctxt;
    code_type_ctxt       = cs_ctxt.code_type_ctxt;
    var_context_stack    = cs_ctxt.var_context_stack;
    function_context_map = cs_ctxt.function_context_map;
    imported_functions   = cs_ctxt.imported_functions;
    imported_variables   = cs_ctxt.imported_variables;
    scoped_variables     = cs_ctxt.scoped_variables;
    external_variables   = cs_ctxt.external_variables;
    scoped_tuples        = cs_ctxt.scoped_tuples;
    current_annotation   = cs_ctxt.current_annotation;
    global_annotation    = cs_ctxt.global_annotation;
    value_indices        = cs_ctxt.value_indices;
    name_indices         = cs_ctxt.name_indices;
    in_remote_execute_operator  = cs_ctxt.in_remote_execute_operator; }

let replace_code_type_context_in_code_selection_context ctc cs_ctxt =
  { compile_ctxt         = cs_ctxt.compile_ctxt;
    code_type_ctxt       = ctc;
    var_context_stack    = cs_ctxt.var_context_stack;
    function_context_map = cs_ctxt.function_context_map;
    imported_functions   = cs_ctxt.imported_functions;
    imported_variables   = cs_ctxt.imported_variables;
    scoped_variables     = cs_ctxt.scoped_variables;
    external_variables   = cs_ctxt.external_variables;
    scoped_tuples        = cs_ctxt.scoped_tuples;
    current_annotation   = cs_ctxt.current_annotation;
    global_annotation    = cs_ctxt.global_annotation;
    value_indices        = cs_ctxt.value_indices;
    name_indices         = cs_ctxt.name_indices;
    in_remote_execute_operator  = cs_ctxt.in_remote_execute_operator; }

(* Type for the code_selection main functions, which has to be passed
   as parameter in some parts of the code selection, notably for
   predicate handling in joins *)

type code_selection_function =
    code_selection_context -> Algebra_type.algop_expr -> code_selection_context


(*************************)
(* Operations on indices *)
(*************************)

(* access to name indices *)

let create_new_name_index code_ctxt qname =
  let nh = code_ctxt.name_indices in
  let new_index = Physical_name_index.create_name_index () in
  begin
(*    let _ = Printf.printf "Adding name index for %s\n"  (Namespace_symbols.relem_string qname)  in *)
    Physical_name_index.add_new_name_index nh qname new_index;
    code_ctxt
  end

let get_name_index code_ctxt elem =
  let index_handler = Physical_name_index.get_opt_name_index code_ctxt.name_indices elem in
  match index_handler with
  | None -> None
  | Some index_handler ->
      begin
	if Debug.default_debug() then
	  Debug.print_default_debug "WARNING, finalizing SORTED btree handler into full index (get_name_index)!\n%!";
	Some (Dm_atomic_btree_util.full_btree_index_of_sorted_btree_handler index_handler)
      end

let get_name_index_handler code_ctxt elem =
  Physical_name_index.get_opt_name_index code_ctxt.name_indices elem 

let get_all_name_indices code_ctxt =
  let all_handlers = Physical_name_index.get_all_name_indices code_ctxt.name_indices in
  let _ = 
    if Debug.default_debug() then
      Debug.print_default_debug "WARNING, finalizing SORTED btree handler into full index (get_all_name_indices)!"
  in
  List.map (fun (x,y) -> (x,Dm_atomic_btree_util.full_btree_index_of_sorted_btree_handler y)) all_handlers  

let get_all_name_indices_handler code_ctxt =
  Physical_name_index.get_all_name_indices code_ctxt.name_indices

(* let _ = print_string("Code_selection_context\n") *)

(***********************)
(* Closures            *)
(***********************)
(* Note: code_ctxt contains the closure context *)
let enter_closure_context code_ctxt vname_list tfield_list = 
  let vcm = Variable_context_manager.build_context_manager () in
  let tcm = Tuple_context_manager.build_context_manager () in 
  Stack.push (ClosureContext, vcm, tcm) code_ctxt.var_context_stack;
  let code_ctxt' = List.fold_left add_variable_to_current_context code_ctxt vname_list in
  List.fold_left add_tuple_reference code_ctxt' tfield_list

let exit_closure_context code_ctxt  = 
  try
    let fname, ctxt, tctxt = Stack.pop code_ctxt.var_context_stack in
    match fname with 
    | ClosureContext -> 
	(* Instantiate the context *)
	instantiate_variable_context_manager ctxt;
	instantiate_tuple_context_manager tctxt;
    | _ ->
	raise (Query (Code_Selection ("Closure exiting scope incorrectly")))
  with Stack.Empty ->
    raise (Query (Code_Selection ("Closure exiting scope incorrectly - Empty Stack")))


