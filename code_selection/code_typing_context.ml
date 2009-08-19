(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: code_typing_context.ml,v 1.6 2007/02/01 22:08:45 simeon Exp $ *)


open Error
open Xquery_common_ast
type code_type_context =
    { code_type_context_variables : (cvname * Xquery_physical_type_ast.physical_xml_type) list;
      code_type_context_input     : Xquery_physical_type_ast.physical_tuple_type option }

let default_code_type_context =
  {
   (* The builtin fs: variables always exist and have the type DOM *)
    code_type_context_variables = [(Xquery_common_ast.fs_dot, Xquery_physical_type_ast_util.dom_list_type);
				    (Xquery_common_ast.fs_sequence, Xquery_physical_type_ast_util.dom_list_type);
				    (Xquery_common_ast.fs_last, Xquery_physical_type_ast_util.dom_list_type);
				    (Xquery_common_ast.fs_position, Xquery_physical_type_ast_util.dom_list_type);];
    (* The implicit INPUT tuple always exists and has the type empty table *)
    code_type_context_input     = Some [] }

let add_variable_type code_type_context cvname physical_xml_type =
  { code_type_context_variables = (cvname,physical_xml_type) :: code_type_context.code_type_context_variables;
    code_type_context_input     = code_type_context.code_type_context_input }

let get_variable_type code_type_context cvname =
  try
    let (x, t) = 
      List.find (fun (x,t) -> Namespace_names.rqname_equal x cvname) code_type_context.code_type_context_variables
    in t
  with
  | Not_found ->
      raise (Query (Internal_Error ("Unknown type for variable "^(Namespace_names.prefixed_string_of_rqname cvname)^" during physical type inference")))


(**********************************)
(* Input of dependant expressions *)
(**********************************)

let add_input_type code_type_context physical_tuple_type =
  { code_type_context_variables = code_type_context.code_type_context_variables;
    code_type_context_input     = Some physical_tuple_type }
  
let get_input_type code_type_context =
  match code_type_context.code_type_context_input with
  | None ->
      raise (Query (Internal_Error "Unknown INPUT type during physical type inference"))
  | Some t -> t

