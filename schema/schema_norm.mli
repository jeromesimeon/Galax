(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: schema_norm.mli,v 1.8 2007/02/01 22:08:53 simeon Exp $ *)

(* Module: Schema_norm
   Description:
     This module normalizes a schema in the XQuery type system into a
     schema in the 'core' XQuery type system.
*)

(** Normalize a Schema schema to XQuery core.  *)

val normalize : Namespace_context.nsenv -> Xquery_type_ast.xschema -> Xquery_type_core_ast.cxschema

val normalize_type : Namespace_context.nsenv -> Xquery_type_ast.xschema -> Xquery_type_ast.xtype -> Xquery_type_core_ast.cxtype

(** The input source schema is supposed to contain definitions for all
    the components that are referenced in it; in particular it must
    already contain all imported schemas. *)

(** Tasks performed by the normalization: 

  - Flattening the hierarchical schema import structure into a flat
    list of definitions. (TODO)

  - Resolving names w.r.t namespaces.

  - Inventing names for anonymous types.

  - Translating derivations by extension into derivations by restriction
    (relying on the "closed world assumption") (TODO)

  - Eliminating group definitions by inlining them into types (TODO)

*)

(* Other stuff for later (either here or in schema_import): [Jerome]

 * What do we do about wildcards!??????

 * [Maybe for later] What do we do about strict/lax/skip
validation!?????

 * [Maybe for later] I think we should normalize attributes used in
derivation by restrictions (or are we already doing it beforehand?). The
reason is that they are not treated the same way as elements (Yet
another weirdness of XML Schema). In the case of attributes, the absence
of an attribute indicates that it remains the same as in the base type.
If one wants to disallow an attributes which is in the base type, then
the 'use="prohibited"' can be used. I don't know what we do with this
currently, it might well be we have to change the global AST to support
this.


*)
