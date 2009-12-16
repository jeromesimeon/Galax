(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Module: Top_args
   Description:
     This module contains code for generic processing of galax
     executable command-line arguments.
 *)

type executable_kind =
  | ExecXQuery
  | ExecXQueryCompile
  | ExecXML
  | ExecXMLSchema
  | ExecProject

type gargs = string array

let dispatch_table =
  [ "xquery",ExecXQuery;
    "compile",ExecXQueryCompile;
    "xml",ExecXML;
    "xmlschema",ExecXMLSchema;
    "project",ExecProject ]

let do_dispatch_args actual_args =
  let effective =
    try
      let dispatch_arg = actual_args.(1) in
      let execkind = List.assoc dispatch_arg dispatch_table in
      let effective_args = Array.concat [[|actual_args.(0)|];(Array.sub actual_args 2 ((Array.length actual_args)-2))] in
      execkind,effective_args
    with _ ->
      ExecXQuery,actual_args
  in
  effective

let dispatch_args () =
  do_dispatch_args Sys.argv

