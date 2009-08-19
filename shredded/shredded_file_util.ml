(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: shredded_file_util.ml,v 1.2 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: shredded_file_util
   Description:
     This util isolates the structure of the database naming on disk
*)

     (* Suffix of databases *)
let main_db_suffix = "-main.db"
let children_index_db_suffix = "-Children.db"
let first_child_index_db_suffix = "-FirstChildIndex.db"
let next_sibling_index_db_suffix = "-NextSiblingIndex.db"
let attr_index_db_suffix = "-AttrIndex.db"
let qnameid_db_suffix = "-Qname2QnameID.db"
let qname_db_suffix = "-QnameID2Qname.db"
let text_db_suffix = "-Text.db"
let namespace_db_suffix = "-Namespace.db"
let metadata_db_suffix = "-Metadata.db"
let processingid_db_suffix = "-ProcesingInstructions.db"
let comment_db_suffix      = "-Comment.db"
let typed_element_value_db_suffix  = "-TypedElementValues.db"
let typed_attribute_value_db_suffix  = "-TypedElementValues.db"
let renumber_db_suffix = "-Renumber.db"  
let renumber_right_temp_db_suffix = "Renumber.Right.temp.db"
let renumber_left_temp_db_suffix  = "Renumber.Left.temp.db"
(* Note: isolate naming functions *)
(* Note: Caml library Filename to manipulate file names *)
  
let create_main_db_name dir name = Filename.concat dir (name ^ main_db_suffix)

let create_qnameid_db_name dir name = Filename.concat dir (name ^ qnameid_db_suffix)

let create_qname_db_name dir name = Filename.concat dir (name ^ qname_db_suffix)

let create_text_db_name dir name = Filename.concat dir (name ^ text_db_suffix)

let create_comment_db_name dir name = Filename.concat dir (name ^ comment_db_suffix)

let create_typed_element_value_db_name dir name = Filename.concat dir (name ^ typed_element_value_db_suffix)

let create_typed_attribute_value_db_name dir name = Filename.concat dir (name ^ typed_attribute_value_db_suffix)

let create_processingid_db_name dir name = Filename.concat dir (name ^ processingid_db_suffix)

let create_namespace_db_name dir name = Filename.concat dir (name ^ namespace_db_suffix)

let create_first_child_index_db_name dir name = Filename.concat dir (name ^ first_child_index_db_suffix)

let create_children_index_db_name dir name = Filename.concat dir (name ^ children_index_db_suffix)

let create_next_sibling_index_db_name dir name = Filename.concat dir (name ^ next_sibling_index_db_suffix)

let create_attr_index_db_name dir name =  Filename.concat dir (name ^ attr_index_db_suffix)

let create_metadata_db_name dir name = Filename.concat dir (name ^ metadata_db_suffix)

let create_renumber_db_name dir name = Filename.concat dir (name ^ renumber_db_suffix)
let create_renumber_right_temp_db_name dir name = Filename.concat dir (name ^ renumber_right_temp_db_suffix)
let create_renumber_left_temp_db_name  dir name = Filename.concat dir (name ^ renumber_left_temp_db_suffix)
