(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: shredded_file_util.mli,v 1.2 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: shredded_file_util
   Description:
     This util isolates the structure of the database naming on disk
*)


val create_main_db_name                   : string -> string -> string

val create_qnameid_db_name                : string -> string -> string

val create_qname_db_name                  : string -> string -> string

val create_text_db_name                   : string -> string -> string

val create_comment_db_name                : string -> string -> string

val create_typed_element_value_db_name    : string -> string -> string

val create_typed_attribute_value_db_name  : string -> string -> string

val create_processingid_db_name           : string -> string -> string

val create_namespace_db_name              : string -> string -> string

val create_first_child_index_db_name      : string -> string -> string

val create_children_index_db_name         : string -> string -> string

val create_next_sibling_index_db_name     : string -> string -> string

val create_attr_index_db_name             : string -> string -> string

val create_metadata_db_name           	  : string -> string -> string

val create_renumber_db_name               : string -> string -> string
val create_renumber_right_temp_db_name    : string -> string -> string
val create_renumber_left_temp_db_name     : string -> string -> string
