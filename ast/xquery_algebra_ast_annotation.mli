(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xquery_algebra_ast_annotation.mli,v 1.7 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Xquery_algebra_ast_annotation *)

(***********************)
(* Physical XML types  *)
(***********************)

type physical_materialized_type =
  | PT_CSeq 
  | PT_LSeq 

type physical_xml_type =
  | PT_Mat of physical_materialized_type
  | PT_Sax     

type physical_tuple_type = (Xquery_common_ast.cvname * physical_xml_type) list    

type  physical_type =
  | PT_XML of   physical_xml_type
  | PT_Table of physical_tuple_type

