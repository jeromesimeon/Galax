(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: sax_annot.mli,v 1.3 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Sax_annot
   Description:
     This *interface* contains annotations for typed SAX events. 

   Semantics:

     Parsing yields a SAX event stream, namespace resolution yields a
     resolved SAX event stream, and validation yields a typed SAX
     event stream in which all event annotations are None. 

     A stream-annotation phase (e.g., XPath stream analysis, document-order
     addition, etc.) takes a typed SAX event stream in which the
     corresponding annotation components are None, and yields a stream
     in which all the corresponding annotation components have been
     set.

     Setting an annotation component of an annotation destructively
     modifies the annotation.  Accessing an annotation component in an
     annotation that is not set raises an error.  *)

type sax_annot

(** The XPath stream analysis label is a boolean **)
type stream_label_annot = bool

(** Create an empty AST annotation. 
    @return New AST annotation.*)
val empty_sax_annot : unit -> sax_annot

(** Return all the annotation's components 
    @return Tuple of annotation's components *)
val annot_components : sax_annot -> stream_label_annot option

(** Copy an annotation
    @param sax_annotation 
    @return copy of annotation *)
val copy_annot  : sax_annot -> sax_annot

(** XPath label annotations *)
(** Set the stream-label annotation
    @param sax_annotation to update
    @param stream-label annotation to add
   *)
val set_stream_label_annot  : sax_annot ->  stream_label_annot -> unit

(** Get the stream-label annotation
    @param sax_annotation 
    @return stream-label annotation
   *)
val get_stream_label_annot  : sax_annot -> stream_label_annot

val set_annotation : sax_annot -> sax_annot -> unit

