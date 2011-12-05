(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: small_stream_ast.mli,v 1.9 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Small_stream_ast
   Description:
     This *interface* contains type declarations for a fragment of XML
     document used to build a small stream.
*)

(* Describes small fragments of XML with holes in them *)

(*
type sattribute = Namespace_names.rqname * Datatypes.xs_untyped * Namespace_symbols.rattr_symbol option ref
type sattribute_forest = sattribute list
*)

type sattribute_forest = Streaming_types.sax_xml_attribute_forest
type sattribute = Streaming_types.sax_xml_attribute

type sexpr =
  | SDocument of (Dm_atomic.atomicAnyURI option ref * sexpr list)
  | SElem of Namespace_names.rqname * (Namespace_context.binding_table option) * Namespace_context.nsenv * sattribute_forest * Dm_atomic.atomicAnyURI option ref * sexpr list * Namespace_symbols.relem_symbol option ref
  | SText of Datatypes.xs_untyped
  | SPI of (Namespace_names.ncname * Datatypes.xs_untyped)
  | SComment of Datatypes.xs_untyped
  | SHole

