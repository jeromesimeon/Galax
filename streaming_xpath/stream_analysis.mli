(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: stream_analysis.mli,v 1.2 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Stream_analysis
   Description:
     This module provides a whole-program analysis function for
     performing a stream analysis on a specified XQuery core module,
     consisting of

   1) building an XQuery core data flow graph for that module
   2) determining for each potential stream source in the graph wether it
      can be streamed or not
   3) adding a corresponding indicator flag annotation to the core ast node.

   At the top level of the analysis, data flow properties of interest are those
   of type 'for each data flow path originating at a certain source, ...', which
   make use of more low level observations on individual nodes in the graph,
   and, in turn, of those nodes' payload (i.e., XQuery core ast handles).

   - Michael *)

val stream_analysis_of_xmodule :
  Xquery_core_ast.acxmodule -> unit
