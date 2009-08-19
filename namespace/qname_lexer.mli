(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: qname_lexer.mli,v 1.5 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Qname_lexer
   Description:
     This module implements a parser for XML QNames.
 *)

type qname_lexer = Lexing.lexbuf -> string option * string

val register_qname_lexer :
    Encoding.encoding -> qname_lexer -> unit


(* Auxiliary lexer used for qualified names *)

val parse_qname   : Lexing.lexbuf -> string option * string

