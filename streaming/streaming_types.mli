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

type special = bool ref
type typed_sax_xml_attribute =
    Namespace_symbols.rtype_symbol  (* The XML Schema type annotation *)
      * Dm_atomic.atomicValue list  (* The XML Schema simple value *)
type sax_xml_attribute = Namespace_names.uqname * Datatypes.xs_untyped * special * Namespace_symbols.rattr_symbol option ref * typed_sax_xml_attribute option ref
type sax_xml_attribute_forest = sax_xml_attribute list

type sax_special_xml_attribute = Namespace_names.uqname * Datatypes.xs_untyped
type sax_special_xml_attribute_forest = sax_special_xml_attribute list

(* Elements, PIs, Comments, and Characters *)

(* Typed elements *)

type resolved_element_desc =
    Namespace_symbols.relem_symbol                   (* QNames are resolved *)
      * sax_base_uri                                 (* A base URI is available *)
      * Namespace_context.nsenv                      (* A Namespace environment is available *)

type typed_element_desc =
    Dm_types.nilled                       (* Has the element been nilled ? *)
      * Namespace_symbols.rtype_symbol    (* The XML Schema type annotation *)
      * Dm_atomic.atomicValue list        (* The XML Schema simple value *)
    
type has_element_content = bool ref

type document_desc = sax_xml_decl option * sax_dtd_decl * sax_base_uri
type element_desc  = Namespace_names.uqname * sax_xml_attribute_forest * has_element_content * sax_special_xml_attribute_forest ref * resolved_element_desc option ref * typed_element_desc option ref
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
  (* Enclosed expressions are eliminated during erasure *)
  | SAX_startEncl
  | SAX_endEncl

type sax_event =
    { se_desc : sax_event_desc;
      se_annot : Sax_annot.sax_annot;   (* Annotation *)
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

(* Typed XML streams *)

(* Note:
     A typed XML stream is a Caml stream of typed SAX events. Please
     see the Stream Caml module for more on operations on streams.
   - Jerome *)

type typed_xml_stream = sax_event Cursor.cursor

(* Ordered, typed XML streams 

   An ordered, typed XML stream is a typed XML stream in which each
   typed SAX event is annotated with a Node identifier. *)

type ordered_sax_xml_attribute =
    sax_xml_attribute        (* The attribute event content *)
      * Nodeid.prepostint_docorder (* The attribute node id *)

type ordered_sax_xml_attribute_forest =
    ordered_sax_xml_attribute list


(* Ordered typed elements *)

type ordered_element_desc  = Namespace_names.uqname * ordered_sax_xml_attribute_forest * has_element_content * sax_special_xml_attribute_forest ref * resolved_element_desc option ref * typed_element_desc option ref

(* SAX events *)

type ordered_sax_event_desc =
  | OTSAX_startDocument of document_desc * Nodeid.preint_docorder
  | OTSAX_endDocument of Nodeid.postint_docorder
  | OTSAX_startElement of ordered_element_desc * Nodeid.preint_docorder
  | OTSAX_endElement of Nodeid.postint_docorder
  | OTSAX_processingInstruction of pi_desc * Nodeid.prepostint_docorder
  | OTSAX_comment of comment_desc * Nodeid.prepostint_docorder
  | OTSAX_characters of text_desc * Nodeid.prepostint_docorder
  (* Additions to the standard SAX events: *)
  | OTSAX_attribute of ordered_sax_xml_attribute
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

type ordered_annotated_sax_event =
    { otse_desc  : ordered_sax_event_desc;  (* Ordered typed SAX event *)
      otse_annot : Sax_annot.sax_annot;   	  (* Annotation *)
      otse_loc   : Finfo.finfo            	  (* Location in stream *) }


type ordered_xml_stream = ordered_annotated_sax_event Cursor.cursor

