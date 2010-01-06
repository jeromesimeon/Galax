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
  | ExecHelp
  | ExecXQuery
  | ExecXQueryCompile
  | ExecXML
  | ExecXMLSchema
  | ExecProject

type map_executable_kind =
  | MapHelp
  | MapXQuery2XML

type gargs = string array

let dispatch_table =
  [ "help", (ExecHelp,"Command-line help");
    "xquery", (ExecXQuery, "XQuery evaluation");
    "compile", (ExecXQueryCompile, "XQuery compilation");
    "xml", (ExecXML, "XML parser");
    "xmlschema", (ExecXMLSchema, "XML Schema import");
    "project", (ExecProject, "XQuery-based document projection") ]

let dispatch_map_table =
  [ "help", (MapHelp, "Command-line help");
    "xquery2xml", (MapXQuery2XML, "Maps a query to XML form") ]

let do_dispatch_args actual_args =
  let effective =
    try
      let dispatch_arg = actual_args.(1) in
      let (execkind,_) = List.assoc dispatch_arg dispatch_table in
      let effective_args = Array.concat [[|actual_args.(0)|];(Array.sub actual_args 2 ((Array.length actual_args)-2))] in
      execkind,effective_args
    with _ ->
      ExecHelp,actual_args
  in
  effective

let do_dispatch_map_args actual_args =
  let effective =
    try
      let dispatch_map_arg = actual_args.(1) in
      let (execkind,_) = List.assoc dispatch_map_arg dispatch_map_table in
      let effective_args = Array.concat [[|actual_args.(0)|];(Array.sub actual_args 2 ((Array.length actual_args)-2))] in
      execkind,effective_args
    with _ ->
      MapHelp,actual_args
  in
  effective

let dispatch_args () =
  do_dispatch_args Sys.argv

let dispatch_map_args () =
  do_dispatch_map_args Sys.argv

let build_subcommands dt =
  let add_command r (cn,(_,cm)) =
    r ^ "\t" ^ cn ^ " (" ^ cm ^ ")\n"
  in
  List.fold_left add_command "" dt

let exec_help_usage =
  "Galax command-line evaluator, version "^Conf.version^".\n"
  ^ "Type 'glx help <subcommand>' for help on specific subcommand.\n"
  ^ "Type 'glx -version' to see the version and build information.\n"
  ^ "\n"
  ^ "Available subcommands:\n"
  ^ (build_subcommands dispatch_table)
  ^ "Galax is a tool for XML processing.\n"
  ^ "For additional information, see http://galax.sourceforge.net"

let map_help_usage =
  "Galax command-line mapper, version "^Conf.version^".\n"
  ^ "Type 'glx-map help <subcommand>' for help on specific subcommand.\n"
  ^ "Type 'glx-map -version' to see the version and build information.\n"
  ^ "\n"
  ^ "Available subcommands:\n"
  ^ (build_subcommands dispatch_map_table)
  ^ "Galax is a tool for XML processing.\n"
  ^ "For additional information, see http://galax.sourceforge.net"

let usage errmsg =
  let b = Buffer.create 200 in
  Printf.bprintf b "%s\n" errmsg;
  Printf.eprintf "%s" (Buffer.contents b)

let exec_help_go gargs =
  usage exec_help_usage

let map_help_go gargs =
  usage map_help_usage

