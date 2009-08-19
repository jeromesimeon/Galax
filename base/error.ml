(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: error.ml,v 1.64 2007/08/01 18:06:30 simeon Exp $ *)

(* Module: Error
   Description:
     This module deals with error handling in Galax.
*)

open Format

type error =
(* Lexing *)
  | Lexing of Finfo.finfo * string
(* Parsing *)
  | Parsing of Finfo.finfo * string
  | Algebra_Parsing_Error of string
  | Namespace_Internal of string
  | Namespace_Error of string
(* Normalization *)
  | Static_Error of string
  | Static_Internal of string
  | Module_Import of string
  | Annotation_Error of string
(* Types *)
  | Malformed_Type of string
  | Malformed_Tuple of string
  | Malformed_Expr of string
  | Malformed_Core_Expr of string
  | Malformed_Algebra_Expr of string
(* Static Typing *)
  | Static_Type_Error of string
  | Automata of string
  | Undefined_Variable of Finfo.finfo * string * string
(* Rewriting *)
  | Rewriting of string
  | Streaming_XPath of string
(* Factorization *)
  | Factorization of string
(* Compilation *)
  | Compilation of string
  | Symbol_Already_Defined of (string * string)
(* Optimization *)
  | Optimization of string
(* Code Selection *)
  | Code_Selection of string
  | Expr_Error of string
  | Key_Error of (string * string)
  | KeyRef_Error of (string * string)
  | Physical_Type_Error of string
(* Evaluation *)
  | Constructor_Error of string
  | Type_Error of string
  | Unicode_Error of string
  | Validation of string
(* Schema Normalization *) 
  | Schema of string
  | Schema_Internal of string
  | Schema_Import of string
(* Serialization *)
  | Serialization of string
(* Data Model / Loading *)
  | Datamodel of string
  | URI_Error of string
  | Load_Error of string
  | Cast_Error of string
  | Protocol_Error of string
  | Stream_Error of string
  | Cursor_Error of string
  | Physical_DM_Error of string
  | Malformed_DateTimeValue of string 
  | Jungle_Error of string
  | Shredded_Error of string
(* Projection *)
  | Projection of string
(* WSDL *)
  | Root
(* Toplevel tools *)
  | Toplevel_Error of string
  | Monitor_Error of string
(* Multiple Modules *)
  | Parameter_Mismatch of string (* Norm, Eval *)
  | Unknown of string
  | Internal_Error of string
  | Wrong_Args of string (* PhysicalDM, Code Selection *)
  | Prototype of string
  | Undefined of string (* Namespace, Parsing, Code Selection *)
  | Mapping_Failure of string (* Normalization, Namespaces *)
  | Update_Error of string (* DM, Code Selection *)
(* Distributed XQuery *)
  | DXQ_Error of string
(* Testing *)
  | Testing_Error of string
(* Generic error with file location *)
  | Error of string
(* XQueryX errors *)
  | XQueryX_Error of Finfo.finfo * string
(* Generic error with file location -- used to wrap internal error with a file location *)
  | Wrapped_Error of Finfo.finfo * string

exception Query of error

let fprint_finfo ff finfo =
  fprintf ff " [%s:]" (Finfo.finfo_to_string finfo)

let print_error_verbose ff exn =
  match exn with
(* Lexing *)
  | Query (Lexing (fi,msg)) ->
      fprintf ff  "Lexing Error: %s" msg;
      fprint_finfo ff fi
(* Parsing *)
  | Query (Parsing (fi,msg)) ->
      fprintf ff  "Parsing Error: %s" msg;
      fprint_finfo ff fi
  | Query (Algebra_Parsing_Error msg) ->
      fprintf ff "Algebra Parsing Error: %s" msg
  | Query (Namespace_Error msg) ->
      fprintf ff "Namespace Error: %s" msg
	(* Prints Caml errors *)
  | Query (Namespace_Internal msg) ->
      fprintf ff "Namespace Internal Error: %s" msg
(* Normalization *)
  | Query (Static_Error (msg)) ->
      fprintf ff  "Static Error: %s" msg
  | Query (Static_Internal (msg)) ->
      fprintf ff  "Static Internal Error: %s" msg
  | Query (Module_Import(msg)) -> 
      fprintf ff "Module Import Error: %s" msg
  | Query (Annotation_Error (msg)) ->
      fprintf ff  "Annotation Error: %s" msg
(* Types *)
  | Query (Malformed_Type (msg)) ->
      fprintf ff  "Malformed Type: %s" msg
  | Query (Malformed_Tuple (msg)) ->
      fprintf ff  "Malformed Tuple: %s" msg
  | Query (Malformed_Expr (msg)) ->
      fprintf ff  "Malformed Expr: %s" msg
  | Query (Malformed_Core_Expr (msg)) ->
      fprintf ff  "Malformed Core Expr: %s" msg
  | Query (Malformed_Algebra_Expr (msg)) ->
      fprintf ff  "Malformed Algebra Expr: %s" msg
(* Static Typing *)
  | Query (Static_Type_Error (msg)) ->
      fprintf ff  "Static_Type Error: %s" msg
  | Query (Automata msg) ->
      fprintf ff  "Automata Error: %s" msg
  | Query (Undefined_Variable(fi, vn, msg)) ->
      fprintf ff  "Undefined variable $%s: %s" vn msg;
      fprint_finfo ff fi
(* Rewriting *)
  | Query (Rewriting msg) ->
      fprintf ff  "Rewriting Internal Error: %s" msg
  | Query (Streaming_XPath msg) ->
      fprintf ff  "Rewriting Internal Error: %s" msg
(* Factorization *)
  | Query (Factorization msg) ->
      fprintf ff  "Factorization Internal Error: %s" msg
(* Compilation *)
  | Query (Compilation msg) ->
      fprintf ff  "Compilation Internal Error: %s" msg
  | Query (Symbol_Already_Defined(kind, name)) ->
      fprintf ff "Symbol_Already_Defined Error: %s %s multiply defined" kind name
(* Code Selection *)
  | Query (Code_Selection msg) ->
      fprintf ff  "Code Selection Internal Error: %s" msg
  | Query (Expr_Error (msg)) ->
      fprintf ff  "Expr Error: %s" msg
  | Query (Key_Error (key,msg)) ->
      fprintf ff "Key Error: %s" msg
  | Query (KeyRef_Error (key,msg)) ->
      fprintf ff "KeyRef Error: %s" msg
  | Query (Physical_Type_Error (msg)) ->
      fprintf ff  "Physical Type Error: %s" msg
(* Evaluation *)
  | Query (Constructor_Error (msg)) ->
      fprintf ff  "Constructor Error: %s" msg
  | Query (Type_Error (msg)) ->
      fprintf ff  "Type Error: %s" msg
  | Query (Unicode_Error (msg)) ->
      fprintf ff  "Unicode Error: %s" msg
  | Query (Validation msg) ->
      fprintf ff "Validation Error: %s" msg
(* Schema Normalization *)
  | Query (Schema msg) ->
      fprintf ff "Schema_Error: %s" msg 
  | Query (Schema_Internal msg) ->
      fprintf ff "Schema Internal Error: %s" msg 
  | Query (Schema_Import(msg)) -> 
      fprintf ff "Schema mapping Error: %s" msg
(* Serialization *)
  | Query (Serialization msg) ->
      fprintf ff "Serialization Internal Error: %s" msg 
(* Datamodel *)
  | Query (Datamodel msg) ->
      fprintf ff  "Datamodel Error: %s" msg
  | Query (URI_Error msg) ->
      fprintf ff "URI Error: %s" msg
  | Query (Load_Error msg) ->
      fprintf ff "Load Error: %s" msg
  | Query (Cast_Error msg) ->
      fprintf ff "Cast Error: %s" msg
  | Query (Protocol_Error msg) ->
      fprintf ff "Protocol Error: %s" msg
  | Query (Stream_Error msg) ->
      fprintf ff "Stream Error: %s" msg
  | Query (Cursor_Error msg) ->
      fprintf ff "Cursor Error: %s" msg
  | Query (Physical_DM_Error msg) ->
      fprintf ff "Physical Data Model Error: %s" msg
  | Query (Malformed_DateTimeValue (msg)) ->
      fprintf ff  "Malformed DateTime Value: %s" msg
  | Query (Jungle_Error msg) ->
      fprintf ff "Jungle Error: %s" msg
  | Query (Shredded_Error msg) ->
      fprintf ff "Shredded Error: %s" msg
(* Projection *)
  | Query (Projection msg) ->
      fprintf ff "Projection Internal Error: %s" msg
(* WSDL *)
  | Query Root ->
      fprintf ff  "Root Error: Root node"
(* Toplevel *)
  | Query (Toplevel_Error msg) ->
      fprintf ff "Toplevel Error: %s" msg
  | Query (Monitor_Error msg) ->
      fprintf ff "Monitor Error: %s" msg
(* Multiple Modules *)

  | Query (Parameter_Mismatch (msg)) ->
      fprintf ff  "Parameter_Mismatch Error: %s" msg
  | Query (Unknown msg) ->
      fprintf ff  "Unknown Error: %s" msg
  | Query (Internal_Error msg) ->
      fprintf ff  "Internal Error: %s" msg
  | Query (Wrong_Args s) ->
      fprintf ff  "Wrong_Args Error: %s" s
  | Query (Prototype msg) -> 
      fprintf ff  "Prototype Error: %s" msg
  | Query (Undefined msg) ->
      fprintf ff  "Undefined Error: %s" msg
  | Query (Mapping_Failure msg) ->
      fprintf ff  "Mapping_Failure Error: %s" msg
  | Query (Testing_Error (msg)) ->
      fprintf ff "Testing Error: %s" msg
  | Query (XQueryX_Error (fi, msg)) ->
      fprintf ff "XQueryX Error: %s" msg;
      fprint_finfo ff fi
  | Query (Update_Error (msg)) ->
      fprintf ff "Update Error: %s" msg
  | Query (DXQ_Error (msg)) ->
      fprintf ff "Distributed XQuery Error: %s" msg
(* Top-level error resulting from downgrade_error *)
  | Query (Error(msg)) ->
      fprintf ff  "Error: %s" msg
(* This error is used to re-wrap errors with a file location *)
  | Query (Wrapped_Error(fi, msg)) ->
      fprintf ff  "%s" msg;
      fprint_finfo ff fi
(*
  | Boxed_Error (v, printer) ->
      fprintf ff  "%s" (printer v)
*)
	(* Prints Caml errors *) 
  (* Prints Caml errors -- we have to handle all the predefined Caml
     errors here *)

  | Match_failure (file, startpos, endpos) ->
      fprintf ff "Match Failure at %s %d %d" file startpos endpos
  | Assert_failure(file, startpos, endpos) ->
      fprintf ff "Assert Failure at %s %d %d" file startpos endpos
  | Invalid_argument msg ->
      fprintf ff "Invalid argument: %s" msg
  | Failure msg ->
      fprintf ff "Failure exception: %s" msg 
  | Not_found ->
      fprintf ff "Not_found"
  | Out_of_memory ->
      fprintf ff "Out_of_memory"
  | Stack_overflow ->
      fprintf ff "Stack_overflow"
  | End_of_file ->
      fprintf ff "End_of_file"
  | Division_by_zero ->
      fprintf ff "Division_by_zero"
  | Sys_blocked_io ->
      fprintf ff "Sys_blocked_io"
  | Sys_error msg ->
      fprintf ff "System Error: %s" msg 
  | Unix.Unix_error(unixerr,fname,arg) -> fprintf ff "%s: %s" fname (Unix.error_message unixerr)
	(* If we end up here, we need to ask PXP to print the error *)
  | e ->
      fprintf ff "%s" (Pxp_types.string_of_exn e)

let print_error_code ff exn =
  match exn with

(* Lexing *)
  | Query (Lexing (fi,msg)) ->
      fprintf ff  "Lexing Error";
      fprint_finfo ff fi
(* Parsing *)
  | Query (Parsing (fi,msg)) ->
      fprintf ff  "Parsing Error";
      fprint_finfo ff fi
  | Query (Algebra_Parsing_Error msg) ->
      fprintf ff "Algebra Parsing Error"
  | Query (Namespace_Error msg) ->
      fprintf ff "Namespace Error"
	(* Prints Caml errors *)
  | Query (Namespace_Internal msg) ->
      fprintf ff "Namespace Internal Error"
(* Normalization *)
  | Query (Static_Error (msg)) ->
      fprintf ff  "Static Error"
  | Query (Static_Internal (msg)) ->
      fprintf ff  "Static Internal Error"
  | Query (Module_Import(msg)) -> 
      fprintf ff "Module Import Error"
  | Query (Annotation_Error (msg)) ->
      fprintf ff  "Annotation Error"
(* Types *)
  | Query (Malformed_Type (msg)) ->
      fprintf ff  "Malformed Type"
  | Query (Malformed_Tuple (msg)) ->
      fprintf ff  "Malformed Tuple"
  | Query (Malformed_Expr (msg)) ->
      fprintf ff  "Malformed Expr"
  | Query (Malformed_Core_Expr (msg)) ->
      fprintf ff  "Malformed Core Expr"
  | Query (Malformed_Algebra_Expr (msg)) ->
      fprintf ff  "Malformed Algebra Expr"
(* Static Typing *)
  | Query (Static_Type_Error (msg)) ->
      fprintf ff  "Static_Type Error"
  | Query (Automata msg) ->
      fprintf ff  "Automata Error"
  | Query (Undefined_Variable(fi, vn, msg)) ->
      fprintf ff  "Error: Undefined Variable";
      fprint_finfo ff fi
(* Rewriting *)
  | Query (Rewriting msg) ->
      fprintf ff  "Rewriting Internal Error"
  | Query (Streaming_XPath msg) ->
      fprintf ff  "Rewriting Internal Error"
(* Factorization *)
  | Query (Factorization msg) ->
      fprintf ff  "Factorization Internal Error"
(* Compilation *)
  | Query (Compilation msg) ->
      fprintf ff  "Compilation Internal Error"
  | Query (Symbol_Already_Defined(kind, name)) ->
      fprintf ff "Symbol_Already_Defined Error:"
(* Code Selection *)
  | Query (Code_Selection msg) ->
      fprintf ff  "Code Selection Internal Error"
  | Query (Expr_Error (msg)) ->
      fprintf ff  "Expr Error"
  | Query (Key_Error (key,msg)) ->
      fprintf ff "Key Error"
  | Query (KeyRef_Error (key,msg)) ->
      fprintf ff "KeyRef Error"
  | Query (Physical_Type_Error (msg)) ->
      fprintf ff  "Physical Type Error"
(* Evaluation *)
  | Query (Constructor_Error (msg)) ->
      fprintf ff  "Constructor Error"
  | Query (Type_Error (msg)) ->
      fprintf ff  "Type Error"
  | Query (Unicode_Error (msg)) ->
      fprintf ff  "Unicode Error"
  | Query (Validation msg) ->
      fprintf ff "Validation Error"
(* Schema Normalization *)
  | Query (Schema msg) ->
      fprintf ff "Schema_Error" 
  | Query (Schema_Internal msg) ->
      fprintf ff "Schema Internal Error" 
  | Query (Schema_Import(msg)) -> 
      fprintf ff "Schema mapping Error"
(* Serialization *)
  | Query (Serialization msg) ->
      fprintf ff "Serialization Internal Error" 
(* Datamodel *)
  | Query (Datamodel msg) ->
      fprintf ff  "Datamodel Error"
  | Query (URI_Error msg) ->
      fprintf ff "URI Error"
  | Query (Load_Error msg) ->
      fprintf ff "Load Error"
  | Query (Cast_Error msg) ->
      fprintf ff "Cast Error"
  | Query (Protocol_Error msg) ->
      fprintf ff "Protocol Error"
  | Query (Stream_Error msg) ->
      fprintf ff "Stream Error"
  | Query (Cursor_Error msg) ->
      fprintf ff "Cursor Error"
  | Query (Physical_DM_Error msg) ->
      fprintf ff "Physical Data Model Error"
  | Query (Malformed_DateTimeValue (msg)) ->
      fprintf ff  "Malformed DateTime Value"
  | Query (Jungle_Error msg) ->
      fprintf ff "Jungle Error"
  | Query (Shredded_Error msg) ->
      fprintf ff "Shredded Error"
(* Projection *)
  | Query (Projection msg) ->
      fprintf ff "Projection Internal Error"
(* WSDL *)
  | Query Root ->
      fprintf ff  "Root Error: Root node"
(* Toplevel *)
  | Query (Toplevel_Error msg) ->
      fprintf ff "Toplevel Error"
  | Query (Monitor_Error msg) ->
      fprintf ff "Monitor Error"
(* Multiple Modules *)

  | Query (Parameter_Mismatch (msg)) ->
      fprintf ff  "Parameter_Mismatch Error"
  | Query (Unknown msg) ->
      fprintf ff  "Unknown Error"
  | Query (Internal_Error msg) ->
      fprintf ff  "Internal Error"
  | Query (Wrong_Args s) ->
      fprintf ff  "Wrong_Args Error"
  | Query (Prototype msg) -> 
      fprintf ff  "Prototype Error"
  | Query (Undefined msg) ->
      fprintf ff  "Undefined Error"
  | Query (Mapping_Failure msg) ->
      fprintf ff  "Mapping_Failure Error"
  | Query (Testing_Error (msg)) ->
      fprintf ff "Testing Error"
  | Query (XQueryX_Error (fi,msg)) ->
      fprintf ff "XQueryX Error"
  | Query (Update_Error (msg)) ->
      fprintf ff "Update Error"
  | Query (DXQ_Error (msg)) ->
      fprintf ff "Distributed XQuery Error"
  | Query (Error (msg)) ->
      fprintf ff  "Error"
  | Query (Wrapped_Error (fi, msg)) ->
      fprintf ff  "Error"
  (* Prints Caml errors -- we have to handle all the predefined Caml
     errors here *)
(*
  | Boxed_Error (v, printer) ->
      fprintf ff  "Error" 
*)

  | Match_failure (file, startpos, endpos) ->
      fprintf ff "Match Failure Error at %s %d %d" file startpos endpos
  | Assert_failure(file, startpos, endpos) ->
      fprintf ff "Assert Failure Errorat %s %d %d" file startpos endpos
  | Invalid_argument msg ->
      fprintf ff "Invalid argument Error: %s" msg
  | Failure msg ->
      fprintf ff "Failure exception Error: %s" msg 
  | Not_found ->
      fprintf ff "Not_found Error"
  | Out_of_memory ->
      fprintf ff "Out_of_memory Error"
  | Stack_overflow ->
      fprintf ff "Stack_overflow Error"
  | Sys_error _ ->
      fprintf ff "System Error"
  | End_of_file ->
      fprintf ff "End_of_file Error"
  | Division_by_zero ->
      fprintf ff "Division_by_zero Error"
  | Sys_blocked_io ->
      fprintf ff "Sys_blocked_io Error"
	(* If we end up here, we need to ask PXP to print the error *)
  | e ->
      fprintf ff "%s" (Pxp_types.string_of_exn e)

let print_error_dispatch ff exn =
  if !Conf.verbose_error
  then
    print_error_verbose ff exn
  else
    print_error_code ff exn

let print_error_safe_dispatch ff exn =
  try
    print_error_dispatch ff exn
  with
  | _ ->
      fprintf ff "Unknown Error Occurred"

let print_error ff exn =
  begin
    print_error_dispatch ff exn;
    pp_print_flush ff ()
  end

let print_error_safe ff exn =
  begin
    print_error_safe_dispatch ff exn;
    pp_print_flush ff ()
  end

let printf_error s exn =
  Gmisc.printf_stub s print_error exn

let eprintf_error s error =
  Gmisc.eprintf_stub s print_error error

let bprintf_error s error =
  Gmisc.bprintf_stub s print_error error

let printf_error_safe s error =
  Gmisc.printf_stub s print_error_safe error

let eprintf_error_safe s error =
  Gmisc.eprintf_stub s print_error_safe error

let bprintf_error_safe s error =
  Gmisc.bprintf_stub s print_error_safe error

let print_warning ff s =
  if !Conf.warning then fprintf ff "[WARNING] %s\n" s

let printf_warning s =
  Gmisc.printf_stub "" print_warning s

let eprintf_warning s =
  Gmisc.eprintf_stub "" print_warning s

let bprintf_warning s =
  Gmisc.bprintf_stub "" print_warning s

let error_with_file_location fi exn = 
  let fi_string = "\nAt "^(Finfo.finfo_to_string fi) in 
  match exn with
(* Lexing *)
  | Query (Lexing (_,msg)) -> Query(Lexing(fi,msg))
(* Parsing *)
  | Query (Parsing (_,msg)) -> Query(Parsing(fi,msg))
  | Query (Algebra_Parsing_Error msg) -> Query(Algebra_Parsing_Error(msg^fi_string))
  | Query (Namespace_Error msg) -> Query(Namespace_Error(msg^fi_string))
  | Query (Namespace_Internal msg) -> Query(Namespace_Internal(msg^fi_string))
(* Normalization *)
  | Query (Static_Error (msg)) -> Query(Static_Error(msg^fi_string))
  | Query (Static_Internal (msg)) -> Query(Static_Internal(msg^fi_string))
  | Query (Module_Import(msg)) -> Query(Module_Import(msg^fi_string))
  | Query (Annotation_Error (msg)) -> Query(Annotation_Error(msg^fi_string))
(* Types *)
  | Query (Malformed_Type (msg)) -> Query(Malformed_Type(msg^fi_string))
  | Query (Malformed_Tuple (msg)) -> Query(Malformed_Tuple(msg^fi_string))
  | Query (Malformed_Expr (msg)) ->  Query(Malformed_Expr(msg^fi_string))
  | Query (Malformed_Core_Expr (msg)) ->  Query(Malformed_Core_Expr(msg^fi_string))
  | Query (Malformed_Algebra_Expr (msg)) ->  Query(Malformed_Algebra_Expr(msg^fi_string))
(* Static Typing *)
  | Query (Static_Type_Error (msg)) ->  Query(Static_Type_Error(msg^fi_string))
  | Query (Automata msg) ->  Query(Automata(msg^fi_string))
  | Query (Undefined_Variable(_, vn, msg)) ->  Query(Undefined_Variable(fi, vn, msg^fi_string))
(* Rewriting *)
  | Query (Rewriting msg) ->  Query(Rewriting(msg^fi_string))
  | Query (Streaming_XPath msg) ->  Query(Streaming_XPath(msg^fi_string))
(* Factorization *)
  | Query (Factorization msg) ->  Query(Factorization(msg^fi_string))
(* Compilation *)
  | Query (Compilation msg) ->  Query(Compilation(msg^fi_string))
  | Query (Symbol_Already_Defined(kind, name)) ->  Query(Symbol_Already_Defined(kind, name^fi_string))
(* Code Selection *)
  | Query (Code_Selection msg) -> Query(Code_Selection(msg^fi_string))
  | Query (Expr_Error (msg)) -> Query(Expr_Error(msg^fi_string))
  | Query (Key_Error (key,msg)) -> Query(Key_Error(key, msg^fi_string))
  | Query (KeyRef_Error (key,msg)) -> Query(KeyRef_Error(key, msg^fi_string))
  | Query (Physical_Type_Error msg) -> Query(Physical_Type_Error(msg^fi_string))
(* Evaluation *)
  | Query (Constructor_Error (msg)) -> Query(Constructor_Error(msg^fi_string))
  | Query (Type_Error (msg)) -> Query(Type_Error(msg^fi_string))
  | Query (Unicode_Error (msg)) -> Query(Unicode_Error(msg^fi_string))
  | Query (Validation msg) -> Query(Validation(msg^fi_string))
(* Schema Normalization *)
  | Query (Schema msg) -> Query(Schema(msg^fi_string))
  | Query (Schema_Internal msg) -> Query(Schema_Internal(msg^fi_string))
  | Query (Schema_Import(msg)) ->  Query(Schema_Import(msg^fi_string))
(* Serialization *)
  | Query (Serialization msg) -> Query(Serialization(msg^fi_string))
(* Datamodel *)
  | Query (Datamodel msg) -> Query(Datamodel(msg^fi_string))
  | Query (URI_Error msg) -> Query(URI_Error(msg^fi_string))
  | Query (Load_Error msg) -> Query(Load_Error(msg^fi_string))
  | Query (Cast_Error msg) -> Query(Cast_Error(msg^fi_string))
  | Query (Protocol_Error msg) -> Query(Protocol_Error(msg^fi_string))
  | Query (Stream_Error msg) -> Query(Stream_Error(msg^fi_string))
  | Query (Cursor_Error msg) -> Query(Cursor_Error(msg^fi_string))
  | Query (Physical_DM_Error msg) -> Query(Physical_DM_Error(msg^fi_string))
  | Query (Malformed_DateTimeValue (msg)) -> Query(Malformed_DateTimeValue(msg^fi_string))
  | Query (Jungle_Error msg) -> Query(Jungle_Error(msg^fi_string))
  | Query (Shredded_Error msg) -> Query(Shredded_Error(msg^fi_string))
(* Projection *)
  | Query (Projection msg) -> Query(Projection(msg^fi_string))
(* Toplevel *)
  | Query (Toplevel_Error msg) -> Query(Toplevel_Error(msg^fi_string))
  | Query (Monitor_Error msg) ->  Query(Monitor_Error(msg^fi_string))
(* Multiple Modules *)
  | Query (Parameter_Mismatch (msg)) -> Query(Parameter_Mismatch(msg^fi_string))
  | Query (Unknown msg) -> Query(Unknown(msg^fi_string))
  | Query (Internal_Error msg) -> Query(Internal_Error(msg^fi_string))
  | Query (Wrong_Args msg) -> Query(Wrong_Args(msg^fi_string))
  | Query (Prototype msg) ->  Query(Prototype(msg^fi_string))
  | Query (Undefined msg) -> Query(Undefined(msg^fi_string))
  | Query (Mapping_Failure msg) -> Query(Mapping_Failure(msg^fi_string))
  | Query (Testing_Error (msg)) -> Query(Testing_Error(msg^fi_string))
  | Query (XQueryX_Error (_,msg)) -> Query(XQueryX_Error(fi,msg))
  | Query (Update_Error (msg)) -> Query(Update_Error(msg^fi_string))

  | Query (DXQ_Error (msg)) -> Query(DXQ_Error(msg^fi_string))

(* Top-level error resulting from downgrade_error *)
  | Query (Error(msg)) -> Query(Error(msg^fi_string))
(* Prints Caml errors -- we have to handle all the predefined Caml
     errors here *)
  | Invalid_argument msg -> Invalid_argument(msg^fi_string)
  | Failure msg -> Failure(msg^fi_string)
  | Sys_error msg -> Sys_error(msg^fi_string)
	(* If we end up here, we need to ask PXP to print the error *)
  | e -> e

