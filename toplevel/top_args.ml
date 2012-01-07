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
  | ExecXQueryTokenize
  | ExecXML
  | ExecXMLSchema
  | ExecProject

type map_executable_kind =
  | MapHelp
  | MapXQuery2XML
  | MapXQueryX2XQuery
  | MapXQuery2Plan
  | MapXQuery2SOAP
  | MapXMLPlan2Plan
  | MapWSDL2XQuery

type gargs = string array

let dispatch_table =
  [ "help", (ExecHelp,"Command-line help");
    "xquery", (ExecXQuery, "XQuery evaluation");
    "compile", (ExecXQueryCompile, "XQuery compilation");
    "tokenize", (ExecXQueryTokenize, "XQuery lexing tokens");
    "xml", (ExecXML, "XML parser");
    "xmlschema", (ExecXMLSchema, "XML Schema import");
    "project", (ExecProject, "XQuery-based document projection") ]

let dispatch_map_table =
  [ "help", (MapHelp, "Command-line help");
    "xquery2xml", (MapXQuery2XML, "Maps a query to XML form");
    "xqueryx2xquery", (MapXQueryX2XQuery, "Maps a query in (trivial) XQueryX form into XQuery form");
    "xquery2plan", (MapXQuery2Plan, "Maps a query into a query plan");
    "xquery2soap", (MapXQuery2SOAP, "Maps an XQuery module into a SOAP server");
    "xmlplan2plan", (MapXMLPlan2Plan, "Maps a query plan in XML into a query plan");
    "wsdl2xquery", (MapWSDL2XQuery, "Maps a WSDL file into an XQuery module") ]

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
  let version_args =
    try
      match Sys.argv.(1) with
      | "-version" -> [|Sys.argv.(0);"xquery";"-version"|]
      | _ -> Sys.argv
    with _ ->
      Sys.argv
  in
  do_dispatch_args version_args

let dispatch_map_args () =
  let version_args =
    try
      match Sys.argv.(1) with
      | "-version" -> [|Sys.argv.(0);"xquery2xml";"-version"|]
      | _ -> Sys.argv
    with _ ->
      Sys.argv
  in
  do_dispatch_map_args version_args

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
  ^ "\n"
  ^ "Galax is a tool for XML processing.\n"
  ^ "For additional information, see http://galax.sourceforge.net"
  ^ "\n"

let map_help_usage =
  "Galax command-line mapper, version "^Conf.version^".\n"
  ^ "Type 'glx-map help <subcommand>' for help on specific subcommand.\n"
  ^ "Type 'glx-map -version' to see the version and build information.\n"
  ^ "\n"
  ^ "Available subcommands:\n"
  ^ (build_subcommands dispatch_map_table)
  ^ "\n"
  ^ "Galax is a tool for XML processing.\n"
  ^ "For additional information, see http://galax.sourceforge.net"
  ^ "\n"

let usage errmsg =
  let b = Buffer.create 200 in
  Printf.bprintf b "%s\n" errmsg;
  Printf.eprintf "%s" (Buffer.contents b);
  flush stderr

let process_help_args args =
  match args with
  | [|glx;help;kind|] -> Some kind
  | [|glx;kind|] -> Some kind
  | _ -> None

let exec_help_go gargs =
  begin
    usage exec_help_usage;
    let okind = process_help_args gargs in
    match okind with
    | None ->
	None
    | Some kind ->
	begin
	  try
	    let (execkind,_) =
	      List.assoc kind dispatch_table
	    in
	    begin
	      usage exec_help_usage;
	      Printf.eprintf "\n";
	      Some (execkind, [|"glx";kind;"-help"|])
	    end
	  with _ ->
	    None
	end
  end

let map_help_go gargs =
  begin
    usage map_help_usage;
    let okind = process_help_args gargs in
    match okind with
    | None ->
	None
    | Some kind ->
	begin
	  try
	    let (execkind,_) =
	      List.assoc kind dispatch_map_table
	    in
	    begin
	      usage map_help_usage;
	      Printf.eprintf "\n";
	      Some (execkind, [|"glx-map";kind;"-help"|])
	    end
	  with _ ->
	    None
	end
  end

