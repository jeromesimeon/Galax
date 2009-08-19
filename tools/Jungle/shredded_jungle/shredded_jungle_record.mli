(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: shredded_jungle_record.mli,v 1.2 2007/02/01 22:08:54 simeon Exp $ *)

(* Module : Jungle_record 
   Description :

*)

open Nodeid

exception Shredded_Jungle_Record_Type_Error of string

type shredded_jungle_record
type record = shredded_jungle_record

type preorder        = Shredded_jungle_basetypes.preorder 
type stored_nodeid   = Shredded_jungle_basetypes.stored_nodeid
type record_specific = Shredded_jungle_basetypes.record_specific
type record_kind     = Shredded_jungle_basetypes.record_kind
type eqnameid        = Shredded_jungle_basetypes.eqnameid
type namespaceid     = Shredded_jungle_basetypes.namespaceid
type prefixid        = Shredded_jungle_basetypes.prefixid
type textid          = Shredded_jungle_basetypes.textid
type commentid       = Shredded_jungle_basetypes.commentid
type processingid    = Shredded_jungle_basetypes.processingid   
type symbol          = prefixid * eqnameid

(* Creates a new record *)

val create_record :
  preorder ->stored_nodeid option ->
  Shredded_jungle_basetypes.record_specific 
  -> shredded_jungle_record
 


val encode : shredded_jungle_record -> char array
val decode : char array -> shredded_jungle_record 
 
val get_preorder     : shredded_jungle_record -> preorder
val set_preorder     : shredded_jungle_record -> preorder -> shredded_jungle_record

val get_parent        : shredded_jungle_record -> stored_nodeid option
val set_parent 		  : shredded_jungle_record -> stored_nodeid option -> shredded_jungle_record

val get_name_eqnameid 	  : shredded_jungle_record -> eqnameid
val get_name_prefixid     : shredded_jungle_record -> prefixid
val set_name_symbol       : shredded_jungle_record -> symbol -> shredded_jungle_record
val get_name_symbol       : shredded_jungle_record -> symbol


val get_type_eqnameid 	  : shredded_jungle_record -> eqnameid option
val get_type_prefixid     : shredded_jungle_record -> prefixid option
val set_type_symbol     : shredded_jungle_record -> symbol option -> shredded_jungle_record
val get_type_symbol     : shredded_jungle_record -> symbol option

val get_attribute_value_id : shredded_jungle_record -> textid
val get_textid		  : shredded_jungle_record -> textid
val get_kind		  : shredded_jungle_record ->   Shredded_jungle_basetypes.record_kind
val get_specific      : shredded_jungle_record -> Shredded_jungle_basetypes.record_specific
val get_namespaceid   : shredded_jungle_record -> namespaceid

val get_commentid    : shredded_jungle_record -> commentid
val get_processing_instruction_id : shredded_jungle_record -> processingid


val is_text_record    : shredded_jungle_record -> bool
val is_elem_record    : shredded_jungle_record -> bool
val is_attr_record    : shredded_jungle_record -> bool

val is_fixed_length   : unit -> bool
val get_record_size   : unit -> int
