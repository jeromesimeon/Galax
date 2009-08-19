(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: typing_context.ml,v 1.13 2007/05/16 15:32:13 mff Exp $ *)

(* Module: Typing_context
   Description:
     This module implements support for the static context.
 *)
open Error

open Xquery_common_ast
open Xquery_core_ast
open Xquery_core_ast_util
open Xquery_type_core_ast

open Processing_context
open Norm_context

type static_context =
    { norm_context : norm_context;
      var_type     : (cvname * cxtype) list;
      schema_namer : Schema_namer.t; (* For making anonymous types *)
    }   

(* The default static context has the following bindings: 
   fs:dot : item
*)

let default_var_types = 
  [ (fs_dot, Schema_builtin.cxtype_item) ]

(* Default static context *)

let default_static_context norm_ctxt =
  { norm_context = norm_ctxt;
    var_type     = default_var_types;
    schema_namer = Schema_namer.create();
  }


(* Merge with a previous normalization context *)

let replace_norm_context_in_static_context norm_ctxt stat_ctxt =
  { norm_context = norm_ctxt;
    var_type     = stat_ctxt.var_type;
    schema_namer = stat_ctxt.schema_namer;
  }

let copy_static_context ctxt =
  { norm_context = copy_norm_context ctxt.norm_context;
    var_type     = ctxt.var_type;
    schema_namer = ctxt.schema_namer;
  }

(* Add a variable and its type to static context *)

let add_var_to_static_context ctxt (v, t) =
  { norm_context = ctxt.norm_context;
    var_type     = (v,t) :: ctxt.var_type;
    schema_namer = ctxt.schema_namer;
  }

(* Lookup a variable's type in static context *)

let var_from_static_context stat_ctxt vn fi = 
  try
    Namespace_util.rqname_assoc vn stat_ctxt.var_type
  with
  | Not_found ->
      raise (Query (Undefined_Variable(fi, Namespace_names.prefixed_string_of_rqname vn, "Variable undefined in static context")))

(* Accessors *)

let norm_context_from_stat_context stat_ctxt =
  stat_ctxt.norm_context

let schema_from_static_context stat_ctxt =
  cxschema_from_norm_context stat_ctxt.norm_context

let schema_namer_from_static_context stat_ctxt =
  stat_ctxt.schema_namer

let vars_from_static_context stat_ctxt =
  stat_ctxt.var_type

