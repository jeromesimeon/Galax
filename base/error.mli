(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: error.mli,v 1.40 2007/08/01 18:06:30 simeon Exp $ *)

(* Module: Error
   Description:
     This module deals with error handling in Galax.
*)

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
  | DXQ_Error of string (* Distributed XQuery *)
(* Testing *)
  | Testing_Error of string
(* Top-level error resulting from downgrade_error *)
  | Error of string
(* XQueryX errors *)
  | XQueryX_Error of Finfo.finfo * string
(* Generic error with file location -- used to wrap internal error with a file location *)
  | Wrapped_Error of Finfo.finfo * string

exception Query of error

val printf_warning  : string -> unit
val eprintf_warning : string -> unit
val bprintf_warning : string -> string

val printf_error  : string -> exn -> unit
val eprintf_error : string -> exn -> unit
val bprintf_error : string -> exn -> string

val printf_error_safe  : string -> exn -> unit
val eprintf_error_safe : string -> exn -> unit
val bprintf_error_safe : string -> exn -> string

val error_with_file_location : Finfo.finfo -> exn -> exn

