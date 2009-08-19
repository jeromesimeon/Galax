(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: sax_annot.ml,v 1.4 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Sax_annot
   Description:
     This *module* implements annotations for typed SAX events. 

   Semantics:

     Parsing yields a SAX event stream, namespace resolution yields a
     resolved SAX event stream, and validation yields a typed SAX
     event stream in which all event annotations are None. 

     A stream-annotation phase (e.g., XPath stream analysis, node-id
     addition, etc.) takes a typed SAX event stream in which the
     corresponding annotation components are None, and yields a stream
     in which all the corresponding annotation components have been
     set.

     Setting an annotation component of an annotation destructively
     modifies the annotation.  Accessing an annotation component in an
     annotation that is not set raises an error.  *)

open Error

(** The XPath stream analysis label is a boolean **)
type stream_label_annot = bool

type sax_annot = 
    { mutable stream_label_annot : bool option } 

(** Create an empty AST annotation. 
    @return New AST annotation.*)
let empty_sax_annot () = 
  { stream_label_annot = None }

(** Return all the annotation's components 
    @return Tuple of annotation's components *)
let annot_components sa = (sa.stream_label_annot)

(** Copy an annotation
    @param sax_annotation 
    @return copy of annotation *)
let copy_annot sa =
  let sla = annot_components sa in 
  { stream_label_annot = sla }

(** XPath label annotations *)
(** Set the stream-label annotation
    @param sax_annotation to update
    @param stream-label annotation to add
   *)
let set_stream_label_annot sa sla = 
  sa.stream_label_annot <- Some sla

(** Get the stream-label annotation
    @param sax_annotation 
    @return stream-label annotation
   *)
let get_stream_label_annot sa = 
  match sa.stream_label_annot with 
  | None -> false (* raise (Query(Annotation_Error("In get_stream_label_annot: Annotation does not contain a stream label"))) *)
  | Some sla -> sla

let set_annotation annot1 annot2 =
  annot1.stream_label_annot <- annot2.stream_label_annot;


