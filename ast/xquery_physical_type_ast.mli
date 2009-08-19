(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xquery_physical_type_ast.mli,v 1.5 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Xquery_physical_type_ast *)

(***********************)
(* Physical XML types  *)
(***********************)

type physical_dom_type =
  | PT_CursorSeq 
  | PT_ListSeq 

type physical_sax_type =
  | PT_Stream
  | PT_Discarded

type physical_xml_type =
  | PT_Dom of physical_dom_type
  | PT_Sax of physical_sax_type

type physical_variable_type =  (Xquery_common_ast.cvname * physical_xml_type)
type physical_tuple_field_type = (Xquery_common_ast.crname * physical_xml_type)
type physical_tuple_type =  physical_tuple_field_type list

type physical_type =
  | PT_XML of   physical_xml_type
  | PT_Table of physical_tuple_type

