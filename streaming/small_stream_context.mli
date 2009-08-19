(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: small_stream_context.mli,v 1.6 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Small_stream_context
   Description:
     This module implements the context used when building a small
     stream from an element-construction expression.
*)

open Error


(****************************)
(* The small stream context *)
(****************************)

type ss_context


(**************************************)
(* Creates a new small stream context *)
(**************************************)

val build_ss_context : Small_stream_ast.sexpr list -> ss_context


(******************************************)
(* Operations on the small stream context *)
(******************************************)

val get_current_sexpr_list : ss_context -> Small_stream_ast.sexpr list

val get_remaining_sexpr_list : ss_context -> Small_stream_ast.sexpr list

val replace_current_sexpr_list : ss_context -> Small_stream_ast.sexpr list -> unit

val push_elem_to_ss_context :
    ss_context -> Small_stream_ast.sexpr -> Small_stream_ast.sexpr list -> Streaming_types.resolved_sax_event * Small_stream_ast.sexpr list

val pop_elem_from_ss_context :
    ss_context -> (Streaming_types.resolved_sax_event * Small_stream_ast.sexpr list) option

(******************************)
(* Simple stream constructors *)
(******************************)

val resolved_xml_stream_of_sexpr : Small_stream_ast.sexpr -> Streaming_types.resolved_xml_stream
    (* Builds an XML stream with holes out of a fragment of AST which
       contains element construction operations. *)

val sexpr_of_rsexpr : Namespace_context.nsenv -> Small_stream_ast.rsexpr -> Small_stream_ast.sexpr

