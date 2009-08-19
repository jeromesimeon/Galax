(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: cursor_descendant.mli,v 1.3 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Cursor_descendant
   Description:
     Generates a cursor from This module exports a data model instance into an XML stream.
*)

(* Creates a stream of the descendants for the input items *)

val cursor_descendant :
    'a Dm_types.access_ops -> 'a Cursor.cursor -> 'a Cursor.cursor

