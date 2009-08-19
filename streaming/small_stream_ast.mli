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

type rsattribute = Namespace_names.rqname * Datatypes.xs_untyped

type rsattribute_forest = rsattribute list

type rsexpr =
  | RSDocument of (Dm_atomic.atomicAnyURI option ref * rsexpr list)
  | RSElem of Namespace_names.rqname * Namespace_context.binding_table * rsattribute_forest * Dm_atomic.atomicAnyURI option ref * rsexpr list
  | RSText of Datatypes.xs_untyped
  | RSPI of (Namespace_names.ncname * Datatypes.xs_untyped)
  | RSComment of Datatypes.xs_untyped
  | RSHole

(* The same after namespace resolution *)

type sattribute = Namespace_symbols.rattr_symbol * Datatypes.xs_untyped

type sattribute_forest = sattribute list

type sexpr =
  | SDocument of (Dm_atomic.atomicAnyURI option ref * sexpr list)
  | SElem of Namespace_symbols.relem_symbol * Namespace_context.nsenv * sattribute_forest * Dm_atomic.atomicAnyURI option ref * sexpr list
  | SText of Datatypes.xs_untyped
  | SPI of (Namespace_names.ncname * Datatypes.xs_untyped)
  | SComment of Datatypes.xs_untyped
  | SHole

