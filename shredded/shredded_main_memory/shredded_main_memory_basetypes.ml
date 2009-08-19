(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: shredded_main_memory_basetypes.ml,v 1.4 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Shredded_main_memory_basetypes
   Description:
     This module implements the basetypes for shredded, all it does
     really is match up signatures *)

(* NOTE: There is a relation between the types here and what is 
   stored in the record - if you change something, you should
   check the record. *)

exception Not_implemented of string
module Main_Memory_Basetypes = struct
  let implemid = Nodeid.new_implemid "Shredded_MM"
  let implem_name = "shredmm"

  let is_persistent () = false
  exception Shredded_Error      = Error.Query 

  module Generator_Default = struct
    type handle = Id.id_gen
    type t = int

    (*********************************************************************)
    (* THE seed_value IS RELIED ON! WE USE 0 TO MARK INVALID IN RECORDS! *)
    (*********************************************************************)	
    let seed_value = 1 
    let build_gen s = Id.create s
    let new_value h = Id.next h 
  end

  type docid                  = Nodeid.docid
  module Docid_Module         = Shredded_common.Int_Type

  type store_id               = {directory_name : string; 
				 shredded_mm_name    : string;
				 docid          : docid }
  let mk_store_id dn jn did   = {directory_name = dn; 
				 shredded_mm_name    = jn;
				 docid          = did }
  let docid_of_storeid s      = s.docid

  type docorder               = Nodeid.docorder
  type preorder               = Nodeid.large_preorder
  module Preorder_Module      = Shredded_common.Preorder_Type


  type nodeid                    = Nodeid.nodeid
  type stored_nodeid             = int
  module Stored_Nodeid_Generator = Generator_Default
  module Stored_Nodeid_Module    = Shredded_common.Int_Type

  type stored_preorder_nodeid_pair     = Shredded_common.Preorder_Nodeid_Pair.t
  module Stored_Preorder_Nodeid_Module = Shredded_common.Preorder_Nodeid_Pair

  module Stored_Preorder_Nodeid_Record_Module   = Shredded_common.Preorder_Nodeid_Pair

  (************************)
  (* Declare Public Types *)
  (************************)
  (* Each type consists of a caml type, a type wrapped in a functor with
     encoding/decoding functions (labeled _Module) and possibly a
     generator if it is a uid type *)

  type textid           = int
  module Textid_Generator = Generator_Default 
  module Textid_Module  = Shredded_common.Int_Type


  type text             = string
  module Text_Module    = Shredded_common.Text_Type

  type namespaceid      = int
  module Namespaceid_Generator = Generator_Default
  module Namespaceid_Module    = Shredded_common.Int_Type

  type processingid             = int
  module Processingid_Generator = Generator_Default
  module Processingid_Module    = Shredded_common.Int_Type

  type processing                      = Namespace_names.ncname * Datatypes.xs_untyped
  module Processing_Instruction_Module = Shredded_common.Processing_Type
    
  type stored_atomic_value_list           = namespaceid * (Datatypes.atomic_type * string) list
  module Stored_Atomic_Value_List_Module  = Shredded_common.Stored_Atomic_Value_List_Type 
    
  type stored_atomic_value_list_with_nilled           = bool  * stored_atomic_value_list
  module Stored_Atomic_Value_List_With_Nilled_Module  = Shredded_common.Stored_Atomic_Value_List_With_Nilled_Type

  type commentid             = int
  module Commentid_Generator = Generator_Default
  module Commentid_Module    = Shredded_common.Int_Type

  type comment               = string
  module Comment_Module      = Shredded_common.String_Type

  type prefixid              = int
  module Prefixid_Generator  = Generator_Default
  module Prefixid_Module     = Shredded_common.Int_Type

  type prefix                = string
  module Prefix_Module       = Shredded_common.String_Type

  type eqnameid              = int
  module Eqnameid_Generator  = Generator_Default
  module Eqnameid_Module     = Shredded_common.Int_Type

  type eqname                = Shredded_common.Eqname_Type.t
  module Eqname_Module       = Shredded_common.Eqname_Type
    
  (* namespaceid * Namespace_context.binding_table *)
  type binding               = Shredded_common.Binding_Type.t 
  module Binding_Module      = Shredded_common.Binding_Type 

  type metadata_value          = Shredded_common.String_Type.t 
  module Metadata_Value_Module = Shredded_common.String_Type

  type metadata_key          = Shredded_common.String_Type.t 
  module Metadata_Key_Module = Shredded_common.String_Type


  type cell_id               = Shredded_common.Cell_Id_Type.t
  module Cell_Id             = Shredded_common.Cell_Id_Type

  let string_of_text t           = t

  let text_of_text_desc t = t 
  let text_desc_of_text s = s

  let text_of_xs_untyped    x = x
  let xs_untyped_of_text    t = t 

  let text_of_xml_attribute t = t
  let xml_attribute_of_text x = x
				  
  type nsenv_hash_table_type = (namespaceid, Namespace_context.nsenv) Hashtbl.t

  type elem_eqnameid = eqnameid
  type attr_eqnameid = eqnameid
  type type_eqnameid = eqnameid
  type elem_symbol   = prefixid * elem_eqnameid
  type attr_symbol   = prefixid * attr_eqnameid
  type type_symbol   = prefixid * type_eqnameid


  type record_specific =
    | ElementRecord of (elem_symbol * namespaceid * type_symbol option)
    | AttributeRecord of (attr_symbol * textid * type_symbol option )
    | TextRecord of textid
    | PIRecord of processingid
    | CommentRecord of commentid
    | DocumentRecord

  type record_kind =
    | ElementRecordKind
    | AttributeRecordKind
    | TextRecordKind
    | PIRecordKind
    | CommentRecordKind
    | DocumentRecordKind
	(* Record portion *)
	
end
