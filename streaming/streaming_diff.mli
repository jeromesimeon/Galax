(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: streaming_diff.mli,v 1.3 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Streaming_diff
   Description:
     Compares two XML streams
*)

open Streaming_types


(***************)
(* Stream diff *)
(***************)

(* Return the first stream up to the point where it differs with the
   second stream, in which case it raises an error *)


val stream_diff : typed_xml_stream -> typed_xml_stream -> typed_xml_stream

(* Same, but just returns true or false *)

val stream_boolean_diff : typed_xml_stream -> typed_xml_stream -> bool

