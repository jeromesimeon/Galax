(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: print_top.ml,v 1.10 2007/08/01 17:06:17 mff Exp $ *)

(* Module Print_top
   Description:
     This module provide the top-level pretty-printing functions for
     the XQuery ASTs.
*)

open Error

open Gmisc

open Occurrence
open Namespace_names
open Datatypes

open Xquery_common_ast
open Print_common

open Xquery_ast
open Print_xquery

open Xquery_core_ast
open Print_xquery_core

open Print_type_core

open Format

(* Toplevel print functions *)

let print_result_type ff m =
  fprintf ff "%a\n@?" print_cxtype m

(***************)
(* Print stubs *)
(***************)

(* Type *)

let printf_sequencetype s m =
  printf_stub s print_sequencetype m

let bprintf_sequencetype s m =
  bprintf_stub s print_sequencetype m

(* core types *)

let printf_cxtype s m =
  printf_stub s print_cxtype m

let bprintf_cxtype s m =
  bprintf_stub s print_cxtype m

let fprintf_cxtype f s m =
  fprintf_stub f s print_cxtype m

let printf_cxschema s m =
  printf_stub s print_cxschema m

let bprintf_cxschema s m =
  bprintf_stub s print_cxschema m

let printf_celem_decl s m =
  printf_stub s print_celem_decl m

let bprintf_celem_decl s m =
  bprintf_stub s print_celem_decl m

let printf_cattr_decl s m =
  printf_stub s print_cattr_decl m

let bprintf_cattr_decl s m =
  bprintf_stub s print_cattr_decl m

let printf_ctype_decl s m =
  printf_stub s print_ctype_decl m

let bprintf_ctype_decl s m =
  bprintf_stub s print_ctype_decl m

(* Sequence type *)

let printf_csequencetype s m =
  printf_stub s print_csequencetype m

let bprintf_csequencetype s m =
  bprintf_stub s print_csequencetype m

let printf_asequencetype s m =
  printf_stub s Print_xquery_algebra.print_asequencetype m

let bprintf_asequencetype s m =
  bprintf_stub s Print_xquery_algebra.print_asequencetype m

(* Full XQuery expression *)

let printf_expr s e =
  printf_stub s print_expr e

let bprintf_expr s e =
  bprintf_stub s print_expr e

let fprintf_expr f s e =
  fprintf_stub f s print_expr e

(* XQuery statement *)

let printf_statement s st =
  printf_stub s print_statement st

let bprintf_statement s st =
  bprintf_stub s print_statement st

let fprintf_statement f s st =
  fprintf_stub f s print_statement st

(* XQuery module *)

let fprintf_library_module f s m =
  fprintf_stub f s print_library_module m

let fprintf_main_module f s m =
  fprintf_stub f s print_main_module m

let fprintf_module f s m =
  fprintf_stub f s print_module m

let fprintf_interface f s m =
  fprintf_stub f s print_interface m

let fprintf_prolog f s p =
  fprintf_stub f s print_prolog p

let bprintf_interface s m =
  bprintf_stub s print_interface m

let bprintf_library_module s m =
  bprintf_stub s print_library_module m

let bprintf_main_module s m =
  bprintf_stub s print_main_module m

let bprintf_module s m =
  bprintf_stub s print_module m

let bprintf_prolog s p =
  bprintf_stub s print_prolog p


(*************************)
(* Typed core expression *)
(*************************)

(* Node kind *)

let printf_cnode_kind s cnk =
  printf_stub s print_ckind_test cnk
let bprintf_cnode_kind s cnk =
  bprintf_stub s print_ckind_test cnk

let print_type_annotation ff m = 
  fprintf ff "[%a] @," print_cxtype m

(* Additional annotations can be printed here *)
let print_annotation ff a =
  let (ta, da, fv, sc, st)  = Xquery_core_ast_annotation.annot_components a in 
  begin
    fprintf ff "[type: ";
    (match ta with
    | None -> fprintf ff "None]"
    | Some m -> fprintf ff "%a] @," print_cxtype m);
    fprintf ff "[sbdo: ";
    (match da with
    | None    -> fprintf ff "None]"
    | Some d  -> fprintf ff "%s]" (Xquery_core_ast_annotation.print_ddo_annot d));
    fprintf ff "[free: ";
    (match fv with
    | None -> fprintf ff "None]"
    | Some fl -> fprintf ff "%s]" (Xquery_core_ast_annotation.print_free_var_annot fl));
    fprintf ff "[sc:%s]" (Xquery_core_ast_annotation.print_scrambling_annot sc);
    fprintf ff "[stream:%s]" (Xquery_core_ast_annotation.print_stream_annot st);
  end

let print_acexpr ff e = 
    print_cexpr ff e print_annotation
let print_acstatement ff e = 
    print_cstatement ff e print_annotation

let print_acprolog ff e = 
    print_cprolog ff e print_annotation
let print_acmodule ff e = 
    print_cmodule ff e print_annotation

let fprintf_acexpr f s e =
  fprintf_stub f s print_acexpr e

let printf_acexpr s e =
  printf_stub s print_acexpr e

let bprintf_acexpr s e =
  bprintf_stub s print_acexpr e

(* Typed core statement *)

let fprintf_acstatement f s st =
  fprintf_stub f s print_acstatement st

let printf_acstatement s st =
  printf_stub s print_acstatement st

let bprintf_acstatement s st =
  bprintf_stub s print_acstatement st

(* Core query prolog *)
let fprintf_acprolog f s qms =
  fprintf_stub f s print_acprolog qms

let printf_acprolog s qms =
  printf_stub s print_acprolog qms

let bprintf_acprolog s qms =
  bprintf_stub s print_acprolog qms

(* Core query module list *)

let fprintf_acmodule f s qms =
  fprintf_stub f s print_acmodule qms

let printf_acmodule s qms =
  printf_stub s print_acmodule qms

let bprintf_acmodule s qms =
  bprintf_stub s print_acmodule qms

(*****************************)
(* Top-level print functions *)
(*****************************)

let fprintf_result_type c s m =
  fprintf_stub c s print_result_type m 

let printf_result_type s m =
  printf_stub s print_result_type m

let bprintf_result_type s m =
  bprintf_stub s print_result_type m

let print_escaped_output ff header str footer =
  if (!Conf.verbose) then pp_print_string ff header;
  pp_print_string ff (!Conf.xml_charescape_fn str);
  if (!Conf.verbose) then pp_print_string ff footer;
  pp_print_flush ff ()


