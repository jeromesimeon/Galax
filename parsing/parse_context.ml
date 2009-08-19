(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: parse_context.ml,v 1.16 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Parse_context
   Description:
     This module implements the parsing context.
 *)

open Error

(* This module implements the parsing context *)

type parse_context =
    { parse_proc_ctxt          : Processing_context.processing_context;
      parse_general_entities   : (string,string) Hashtbl.t }

(* Note:
      From the XML 1.0 spec:

   	<!ENTITY lt     "&#38;#60;">
   	<!ENTITY gt     "&#62;">
   	<!ENTITY amp    "&#38;#38;">
   	<!ENTITY apos   "&#39;">
   	<!ENTITY quot   "&#34;">

   - Jerome
 *)

(* Internally, entities are resolved to characters and are converted
   back to entity references during serialization. *)
let built_in_general_entities =
  [ ("lt", "<");
    ("gt", ">");
    ("amp", "&");
    ("apos", "'");
    ("quot", "\"") ]

let init_general_entities () =
  let general_entity_table = Hashtbl.create 167 in
  let add_fun (x,y) = Hashtbl.add general_entity_table x y in
  List.iter add_fun built_in_general_entities;
  general_entity_table

let build_xquery_parse_context proc_ctxt =
  let general_entity_table = init_general_entities () in
  { parse_proc_ctxt = proc_ctxt;
    parse_general_entities = general_entity_table }

let add_general_entity_to_parse_context parse_context x y =
  Hashtbl.add parse_context.parse_general_entities x y

let get_processing_context parse_context =
  parse_context.parse_proc_ctxt

let get_general_entity parse_context fi x =
  try
    Hashtbl.find parse_context.parse_general_entities x
  with
  | _ ->
      raise (Query (Parsing (fi,"General entity: '&"^x^";' not found")))

