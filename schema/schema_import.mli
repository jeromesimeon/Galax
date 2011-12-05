(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: schema_import.mli,v 1.12 2007/08/01 17:06:17 mff Exp $ *)

(* Module: Schema_import
   Description:
     This module imports an XML document (in the form of a resolved
     XML SAX stream) in the XQuery type system.
*)

(** Schema import accepts an optional prefix suggestion for the target
  namespace.  This prefix is only suggestive; if the schema associates
  its own prefixe(s) to the target namespace, then one of those is
  used instead; if this prefix conflicts with prefixes in the schema,
  or None is passed, then the target namespace prefix is invented. *)

val  import_schema : Processing_context.processing_context -> 
  (* prefix option *) Namespace_names.prefix option -> (* target uri *) string -> 
    (* location hint option *) string option -> Xquery_type_ast.xschema
  (** Import a schema at the given target URI using the optional
      location hint, or if that fails, the location hints in the
      processing context. *)

val  import_schema_document : Processing_context.processing_context -> Namespace_names.prefix option -> Streaming_types.xml_stream -> Xquery_type_ast.xschema
  (** The stream corresponds to an opened document, i.e. the document
    root node is still present.  The root element must be <schema>. *)

val  import_schema_element :  Processing_context.processing_context -> Namespace_names.prefix option -> Streaming_types.xml_stream -> Xquery_type_ast.xschema
  (** The stream is supposed to start with <schema> element, 
    i.e. the document root or parent elements have already been removed. 
    This is intended for importing embedded schemas. *)

