(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: streaming_types.mli,v 1.6 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Streaming_types
   Description:
     This module contains data structures for XML streams.
*)


(**********************************)
(* SAX events for well-formed XML *)
(**********************************)

(* XML declaration *)

type sax_xml_decl =
    (Datatypes.xs_untyped * Encoding.encoding option * Datatypes.xs_untyped option)

(* DTD declaration *)

type sax_dtd_decl = Pxp_dtd.dtd option

(* Base URI *)

type sax_base_uri = Dm_atomic.atomicAnyURI option ref

(* Attributes *)

type sax_xml_attribute =
    Namespace_names.uqname * Datatypes.xs_untyped

type sax_xml_attribute_forest =
    sax_xml_attribute list

(* Elements, PIs, Comments, and Characters *)

type has_element_content = bool

type document_desc = sax_xml_decl option * sax_dtd_decl * sax_base_uri
type element_desc  = Namespace_names.uqname * sax_xml_attribute_forest * has_element_content
type pi_desc       = Namespace_names.ncname * Datatypes.xs_untyped
type comment_desc  = Datatypes.xs_untyped
type text_desc     = Datatypes.xs_untyped
type atomic_desc   = Dm_atomic.atomicValue

(* SAX events *)

type sax_event_desc =
  | SAX_startDocument of document_desc
  | SAX_endDocument
  | SAX_startElement of element_desc
  | SAX_endElement
  | SAX_processingInstruction of pi_desc
  | SAX_comment of comment_desc
  | SAX_characters of text_desc
  | SAX_attribute of sax_xml_attribute        (* Addition to the standard SAX events *)
  | SAX_atomicValue of atomic_desc            (* Addition to the standard SAX events *)
  | SAX_hole                                  (* Addition to the standard SAX events *)

type sax_event =
    { se_desc : sax_event_desc;
      se_loc  : Finfo.finfo }
 
(* Note:
     The two last events are added in order to be able to deal
     with top-level attributes and atomic values from the XQuery data
     model.
   - Jerome *)

(* Well-formed XML streams *)

(* Note:
     An XML stream is a Caml stream of SAX events. Please see the
     Stream Caml module for more on operations on streams.
   - Jerome *)

type xml_stream = sax_event Cursor.cursor


(*******************************)
(* SAX events for resolved XML *)
(*******************************)

(* Note:
     The following structures are used to represent 'resolved' XML
     document in a streaming fashion. They are the same as above, but
     the QNames are turned into expanded-names (Symbols in Galax).
   - Jerome *)

(* Resolved attributes *)

type resolved_sax_xml_attribute =
    Namespace_symbols.rattr_symbol              (* QNames are resolved *)
      * Datatypes.xs_untyped

type resolved_sax_xml_attribute_forest =
    resolved_sax_xml_attribute list

(* Typed elements *)

type resolved_element_desc =
    Namespace_symbols.relem_symbol                   (* QNames are resolved *)
      * resolved_sax_xml_attribute_forest
      * has_element_content
      * sax_base_uri
      (* Addition to the standard SAX events *)
      * Namespace_context.nsenv        (* Namespace environment for that element *)

(* Resolved SAX events *)

type resolved_sax_event_desc =
  | RSAX_startDocument of document_desc
  | RSAX_endDocument
  | RSAX_startElement of resolved_element_desc
  | RSAX_endElement
  | RSAX_processingInstruction of pi_desc
  | RSAX_comment of comment_desc
  | RSAX_characters of text_desc
  | RSAX_attribute of resolved_sax_xml_attribute  (* Addition to the standard SAX events *)
  | RSAX_atomicValue of atomic_desc         	  (* Addition to the standard SAX events *)
  | RSAX_hole                                  	  (* Addition to the standard SAX events *)

(* Resolved XML streams *)

type resolved_sax_event = {
   rse_desc : resolved_sax_event_desc;
   rse_loc  : Finfo.finfo;
}

(* Note:
     A resolved XML stream is a Caml stream of resolved SAX
     events. Please see the Stream Caml module for more on operations
     on streams.
   - Jerome *)

type resolved_xml_stream = resolved_sax_event Cursor.cursor


(****************************)
(* SAX events for typed XML *)
(****************************)

(* Note:
     The following structures are used to represent 'typed' XML
     document in a streaming fashion. They support a number of
     additions from the standard SAX events, notably: type annotations
     on elements and attributes, as well as typed values resulting
     from XML Schema validation on top of the corresponding text
     content.
   - Jerome *)

(* Typed attributes *)

type typed_sax_xml_attribute =
    Namespace_symbols.rattr_symbol    	   (* QNames are resolved *)
      * Datatypes.xs_untyped         	   (* Addition to the standard SAX events *)
      * Namespace_symbols.rtype_symbol       	   (* The XML Schema type annotation *)
      * Dm_atomic.atomicValue list  (* The XML Schema simple value *)

type typed_sax_xml_attribute_forest =
    typed_sax_xml_attribute list

(* Typed elements *)

type typed_element_desc =
    Namespace_symbols.relem_symbol                   (* QNames are resolved *)
      * typed_sax_xml_attribute_forest
      * has_element_content
      * sax_base_uri
        (* Addition to the standard SAX events *)
      * Namespace_context.nsenv           (* Namespace environment for that element *)
      * Dm_types.nilled                   (* Has the element been nilled ? *)
      * Namespace_symbols.rtype_symbol    (* The XML Schema type annotation *)
      * Dm_atomic.atomicValue list (* The XML Schema simple value *)

(* SAX events *)

type typed_sax_event_desc =
  | TSAX_startDocument of document_desc
  | TSAX_endDocument
  | TSAX_startElement of typed_element_desc
  | TSAX_endElement
  | TSAX_processingInstruction of pi_desc
  | TSAX_comment of comment_desc
  | TSAX_characters of text_desc
  (* Additions to the standard SAX events: *)
  | TSAX_attribute of typed_sax_xml_attribute
  | TSAX_atomicValue of atomic_desc
  | TSAX_hole
  (* Enclosed expressions only occur in typed streams and are 
     eliminated during erasure *)
  | TSAX_startEncl
  | TSAX_endEncl

(* Annotated typed SAX event 

   As with the annotated Core AST, the annotation type contains
   mutable option fields.  An alternative is to make the events
   polymorphic in the annotations, but this is kind of heavy weight.

   For the moment, we just add annotations to typed SAX events.
   Instead of having three kinds of SAX events (original, resolved,
   typed), we could have just one kind in which the annotations
   include the resolved QNames and the computed type, but this is a
   deeper change.

   - Mary
*)

type typed_annotated_sax_event = 
    { tse_desc  : typed_sax_event_desc;  (* Typed SAX event *)
      tse_annot : Sax_annot.sax_annot;   (* Annotation *)
      tse_loc   : Finfo.finfo            (* Location in stream *) }

(* Typed XML streams *)

(* Note:
     A typed XML stream is a Caml stream of typed SAX events. Please
     see the Stream Caml module for more on operations on streams.
   - Jerome *)

type typed_xml_stream = typed_annotated_sax_event Cursor.cursor

(* Ordered, typed XML streams 

   An ordered, typed XML stream is a typed XML stream in which each
   typed SAX event is annotated with a Node identifier. *)

type ordered_typed_sax_xml_attribute =
    typed_sax_xml_attribute        (* The attribute event content *)
      * Nodeid.prepostint_docorder (* The attribute node id *)

type ordered_typed_sax_xml_attribute_forest =
    ordered_typed_sax_xml_attribute list


(* Ordered typed elements *)

type ordered_typed_element_desc =
    Namespace_symbols.relem_symbol                   (* QNames are resolved *)
      * ordered_typed_sax_xml_attribute_forest
      * has_element_content
      * sax_base_uri
        (* Addition to the standard SAX events *)
      * Namespace_context.nsenv  	     (* Namespace environment for that element *)
      * Dm_types.nilled        	             (* Has the element been nilled ? *)
      * Namespace_symbols.rtype_symbol       (* The XML Schema type annotation *)
      * Dm_atomic.atomicValue list    (* The XML Schema simple value *)

(* SAX events *)

type ordered_typed_sax_event_desc =
  | OTSAX_startDocument of document_desc * Nodeid.preint_docorder
  | OTSAX_endDocument of Nodeid.postint_docorder
  | OTSAX_startElement of ordered_typed_element_desc * Nodeid.preint_docorder
  | OTSAX_endElement of Nodeid.postint_docorder
  | OTSAX_processingInstruction of pi_desc * Nodeid.prepostint_docorder
  | OTSAX_comment of comment_desc * Nodeid.prepostint_docorder
  | OTSAX_characters of text_desc * Nodeid.prepostint_docorder
  (* Additions to the standard SAX events: *)
  | OTSAX_attribute of ordered_typed_sax_xml_attribute
  | OTSAX_atomicValue of atomic_desc
  | OTSAX_hole
  (* Enclosed expressions only occur in typed streams and are 
     eliminated during erasure *)
  | OTSAX_startEncl
  | OTSAX_endEncl

(* Annotated typed SAX event 

   As with the annotated Core AST, the annotation type contains
   mutable option fields.  An alternative is to make the events
   polymorphic in the annotations, but this is kind of heavy weight.

   For the moment, we just add annotations to typed SAX events.
   Instead of having three kinds of SAX events (original, resolved,
   typed), we could have just one kind in which the annotations
   include the resolved QNames and the computed type, but this is a
   deeper change.

   - Mary
*)

type ordered_typed_annotated_sax_event =
    { otse_desc  : ordered_typed_sax_event_desc;  (* Ordered typed SAX event *)
      otse_annot : Sax_annot.sax_annot;   	  (* Annotation *)
      otse_loc   : Finfo.finfo            	  (* Location in stream *) }


type ordered_typed_xml_stream = ordered_typed_annotated_sax_event Cursor.cursor

