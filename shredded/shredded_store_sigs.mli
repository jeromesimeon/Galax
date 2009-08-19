(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: shredded_store_sigs.mli,v 1.11 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Shredded_store_sigs
   Description:
     This module provides the signatures that parameterize a shredded
     store. These are used by shredded_dm.ml (and then by
     shredded_store) to access the various index structures.
 *)

open Nodeid

(* A Note on accessors: 
   This terminology comes from BDB:

   _get_set        : get the (first) value whose key matches the parameter.
   _get_set_range  : get the (first) value whose key is greater than or equal to the parameter
   _get_both       : get the value that matches both on the key and the value
   _get_both_range : get the value whose (key,value) is greater than or equal to the parameter

   This terminology is used in all structures, but only where it makes
   sense.  For example, hashes have no notion of order and so
   _get_set_range doesn't really make sense.
*)  

module type Shredded_Cell_Type = sig
  type t
  type encoded_type = char array
  val encode : t -> encoded_type
  val decode : encoded_type -> t
  val in_same_cell : t -> t -> int -> bool
end

module type Shredded_Type = sig 
  type t 

  (* We need to parameterize the basetype by the encoded types to be
     general and then 'with type...' match each instance *)
    (* For now, just hard code the type *)
  type encoded_type = char array 
  val encode : t -> encoded_type
  val decode : encoded_type -> t
end

module type Shredded_OrderedType = sig
  type t 
  type encoded_type = char array 
      (* encode *must* be encoding in a way that the strings are bit-string comparable -as is done by BDB *)
  val encode : t -> encoded_type
  val decode : encoded_type -> t

  (* currently only used in the main memory *)
  val compare : t -> t -> int  
  val fprintf_value : Format.formatter -> t -> unit
end


module type Record_Type = sig 
  type record
  val is_fixed_length : unit -> bool
  val get_record_size : unit -> int    
  val encode          : record -> char array
  val decode          : char array -> record
end

module type Generator = sig 
  type handle
  type t 
  val seed_value : t 
  val build_gen : t -> handle
  val new_value : handle -> t
end


(* This signature is the list of necessary base types. *)
module type Shredded_Basetypes = sig 
  (* This should be split out *)
  val implemid : Nodeid.implemid
  val implem_name : string

  (* This isn't a great place for this *)
  val is_persistent : unit -> bool


  type store_id

  type docorder               = Nodeid.docorder  

  type preorder               = Nodeid.large_preorder
  module Preorder_Module      : Shredded_OrderedType with type t = preorder

       
  type nodeid        = Nodeid.nodeid
  type stored_nodeid = int (* must be public since store must do the converstion *)
  module Stored_Nodeid_Module : Shredded_OrderedType with type t = stored_nodeid

  type stored_preorder_nodeid_pair     = preorder * stored_nodeid
  module Stored_Preorder_Nodeid_Module : Shredded_OrderedType with type t = stored_preorder_nodeid_pair

  module Stored_Preorder_Nodeid_Record_Module : Record_Type with type record = stored_preorder_nodeid_pair
  (* There is code that needs to be removed then these can these types
     opaque. The code takes each type + 1. Adding a function to
     increment or using the generator in a different way can
     alleviate this problem.

     -- Chris *)
    

  (* These are types and their wrapper modules *)
  type textid           = int
  module Textid_Module           : Shredded_OrderedType with type t = textid

  type namespaceid      = int
  module Namespaceid_Module      : Shredded_Type with type t = namespaceid 

  type binding                   = (namespaceid * Namespace_context.binding_table)
  module Binding_Module          : Shredded_Type with type t = binding

  type comment      = string
  module Comment_Module      : Shredded_Type with type t = comment 

  (**********************************************)
  (* We store expanded qnames and their prefix. *)
  (* These are only ref'd inside the two way    *)
  (* hashes                                     *)
  (**********************************************)
  type eqnameid     = int   
  module Eqnameid_Module     : Shredded_Type with type t = eqnameid

  type eqname       = Namespace_names.uri * Namespace_names.ncname
  module Eqname_Module       : Shredded_Type with type t = eqname

  type prefixid     = int 
  module Prefixid_Module     : Shredded_Type with type t = prefixid

  type prefix       = string
  module Prefix_Module       : Shredded_Type with type t = prefix
		
					       
  (* You need a namespaceid to make sense/get access to a context to
     cast it *)
  type stored_atomic_value_list = namespaceid * (Datatypes.atomic_type * string) list
  module Stored_Atomic_Value_List_Module   : Shredded_Type with type t = stored_atomic_value_list

  type stored_atomic_value_list_with_nilled =  bool * stored_atomic_value_list
  module Stored_Atomic_Value_List_With_Nilled_Module   : Shredded_Type with type t = stored_atomic_value_list_with_nilled
									 
  type commentid    = int
  module Commentid_Module    : Shredded_Type with type t = commentid 

  type processing   = Namespace_names.ncname * Datatypes.xs_untyped
  module Processing_Instruction_Module : Shredded_Type with type t = processing 

  type processingid = int
  module Processingid_Module : Shredded_Type with type t = processingid 


  type text 
  module Text_Module         : Shredded_OrderedType with type t = text


  type metadata_key          = string
  module Metadata_Key_Module   : Shredded_Type with type t = metadata_key

  type metadata_value        = string
  module Metadata_Value_Module : Shredded_Type with type t = metadata_value

  type cell_id               
  module Cell_Id             : Shredded_Cell_Type


  val string_of_text : text -> string
  val text_of_xml_attribute : string -> text
  val xml_attribute_of_text : text -> string

  val text_of_text_desc     : string -> text
  val text_desc_of_text     : text -> string

  val xs_untyped_of_text            : text -> Datatypes.xs_untyped
  val text_of_xs_untyped            : Datatypes.xs_untyped -> text

  (* Union for record type *)
  type elem_eqnameid = eqnameid
  type attr_eqnameid = eqnameid
  type type_eqnameid = eqnameid
  type elem_symbol   = prefixid * elem_eqnameid
  type attr_symbol   = prefixid * attr_eqnameid
  type type_symbol   = prefixid * type_eqnameid
      
  type record_specific =
    | ElementRecord of (elem_symbol * namespaceid * type_symbol option)
    | AttributeRecord of (attr_symbol * textid * type_symbol option)
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
    
  type nsenv_hash_table_type = (namespaceid, Namespace_context.nsenv) Hashtbl.t


  (* Id generators *)

  (* Generators *)
  module Stored_Nodeid_Generator: Generator with type t = stored_nodeid
  module Textid_Generator       : Generator with type t = textid
  module Namespaceid_Generator  : Generator with type t = namespaceid
  module Commentid_Generator    : Generator with type t = commentid
  module Processingid_Generator : Generator with type t = processingid
  module Prefixid_Generator     : Generator with type t = prefixid
  module Eqnameid_Generator     : Generator with type t = eqnameid
end

(* Module signature that deals with the record of a node *)
module type Node_Record = 
sig
  type record
  type preorder   

  type stored_nodeid
  type record_specific
  type record_kind

  type eqnameid
  type namespaceid
  type prefixid
  type textid
  type commentid
  type processingid
  type symbol = prefixid * eqnameid 

  val is_fixed_length : unit -> bool
  val get_record_size : unit -> int    
  val encode          : record -> char array
  val decode          : char array -> record
      
  val create_record     : preorder -> stored_nodeid option -> record_specific -> record

  val get_preorder     : record -> preorder    
  val set_preorder     : record -> preorder -> record    

  val get_parent        : record -> stored_nodeid option 
  val set_parent        : record -> stored_nodeid option -> record

  val get_name_eqnameid : record -> eqnameid
  val get_name_prefixid : record -> prefixid
  val get_name_symbol   : record -> symbol
  val set_name_symbol   : record -> symbol -> record


  val get_type_eqnameid : record -> eqnameid option
  val get_type_prefixid : record -> prefixid option
  val get_type_symbol   : record -> symbol option
  val set_type_symbol   : record -> symbol option -> record


  val get_attribute_value_id : record -> textid
  val get_textid		  : record -> textid
  val get_kind		: record -> record_kind
  val get_specific      : record -> record_specific
  val get_namespaceid   : record -> namespaceid
  val is_text_record    : record -> bool
  val is_elem_record    : record -> bool
  val is_attr_record    : record -> bool    
    
  val get_commentid    : record -> commentid
  val get_processing_instruction_id : record -> processingid
end

(**************************)
(* Recno Module Signature *)
(**************************)

module type Shredded_Recno_Functor_Sig = 
   functor (BaseTypes : Shredded_Basetypes) ->
     functor (Record  : Record_Type) ->
sig
  (*********************)
  (* Handle to storage *)
  (*********************)  
  type recno_handle
  type record = Record.record
  type cursor_direction = Next | Prev

  val recno_open : string -> int -> recno_handle
    (* Takes the name of the file open flags and returns a handle on
       a new btree *)
  val recno_put    : recno_handle -> BaseTypes.stored_nodeid -> record -> unit
  val recno_get    : recno_handle -> BaseTypes.stored_nodeid -> record option
  val recno_get_unsafe    : recno_handle -> BaseTypes.stored_nodeid -> record
  val recno_delete : recno_handle -> BaseTypes.stored_nodeid -> unit

  val recno_close  : recno_handle -> unit
    
  (* recno_close_no_sync is used with temporary storage so that the
     store knows it does not have to make the updates persistant. *)
  val recno_close_no_sync : recno_handle -> unit
  val recno_sync   : recno_handle -> unit
    
  (*****************)
  (* Recno Cursors *)
  (*****************)
  type recno_cursor

  val recno_cursor_to_cursor : recno_cursor -> cursor_direction -> record Cursor.cursor
    
  val recno_cursor_open : recno_handle -> recno_cursor

  (* This interface doesn't look right - Chris *)
  val recno_cursor_put : recno_cursor -> BaseTypes.stored_nodeid -> record -> unit
  val recno_cursor_get_next : recno_cursor -> record option
  val recno_cursor_get_prev : recno_cursor -> record option
  val recno_cursor_get_first: recno_cursor -> record option
  val recno_cursor_get_last : recno_cursor -> record option

  val recno_cursor_get_next_dup : recno_cursor -> record option

  val recno_cursor_get_set : recno_cursor -> BaseTypes.stored_nodeid -> record option

  val recno_cursor_del : recno_cursor -> unit

  val recno_cursor_close : recno_cursor -> unit
end

(**************************)
(* Btree Module Signature *)
(**************************)
(* This right now defaults to taking duplicates, so delete key -> delete_all *)
module type Shredded_Btree_Functor_Sig = 
  functor (Key : Shredded_OrderedType) ->
    functor (Value : Shredded_OrderedType) ->    
sig
  type btree_handle 
  type btree_key    = Key.t 
  type btree_value  = Value.t
  type btree_cursor
  type cursor_direction = Next | Prev
    
  (* Takes the name of the file open flags and returns a handle
     on a new btree. int is the bufffersize. bool is allow duplicates, it 
     may also fail. *)
  val btree_open    : string -> int -> bool -> btree_handle

  val btree_put     : btree_handle -> btree_key -> btree_value -> unit 

  (* btree_get: get the first child *)
  val btree_get     : btree_handle -> btree_key -> btree_value option

  (* btree_get_all: all the children of a given key *)
  val btree_get_all : btree_handle -> btree_key -> btree_value Cursor.cursor

  val btree_delete      : btree_handle -> btree_key -> btree_value -> unit
  val btree_delete_all  : btree_handle -> btree_key -> unit

  val btree_close   : btree_handle -> unit 
  val btree_sync    : btree_handle -> unit 
        
  (* Wrap an existing (and placed) cursor to a dm cursor. *)
  val btree_cursor_to_cursor : btree_cursor -> cursor_direction -> (btree_key * btree_value) Cursor.cursor
  val btree_cursor_open      : btree_handle -> btree_cursor

  val btree_cursor_put       : btree_cursor -> btree_key -> btree_value -> unit
  val btree_cursor_get_first : btree_cursor -> (btree_key * btree_value) option
  val btree_cursor_get_last  : btree_cursor -> (btree_key * btree_value) option

  (******************************)
  (* cursor movement operations *)
  (******************************)
  val btree_cursor_get_next  : btree_cursor -> (btree_key * btree_value) option
  val btree_cursor_get_prev  : btree_cursor -> (btree_key * btree_value) option

  (****************************)
  (* Cursor get_set and range *)
  (****************************)
  val btree_cursor_get_set       : btree_cursor -> btree_key -> (btree_key * btree_value) option 
  val btree_cursor_get_both      : btree_cursor -> btree_key * btree_value -> (btree_key * btree_value) option 
  val btree_cursor_get_set_range : btree_cursor -> btree_key -> (btree_key * btree_value) option
  val btree_cursor_get_both_range: btree_cursor -> btree_key * btree_value -> (btree_key * btree_value) option 

  val btree_cursor_del   : btree_cursor -> unit
  val btree_cursor_close : btree_cursor -> unit
end


module type Shredded_Hash_Functor_Sig =
  functor (Key : Shredded_Type) ->
    functor (Value : Shredded_Type) ->    
sig
  type hash  type hash_key = Key.t
  type hash_value = Value.t

  (* Create a new Hash Table : 
     string -- name of hash table (file name for BDB) 
     int    -- buffer size for caching; 0 gives default behavior
     bool   -- allow duplicates *may* silently overwrite if false and duplicates entered
  *)
  val hash_open : string -> int -> bool -> hash    
  (* Add (key, value) pair to Hash Table *)
  val hash_put : hash -> hash_key -> hash_value -> unit 
  (* Get value associated with key from Hash Table *)
  val hash_get : hash -> hash_key -> hash_value option
    
  (* Get all values associated with a key *)
  val hash_get_all : hash -> hash_key -> hash_value Cursor.cursor

  (* Delete value associated with key from Hash Table *)
  val hash_delete : hash -> hash_key -> unit
  (* Close the Hash Table *)
  val hash_close  : hash -> unit 
    
  (* Sync the hash table to disk *)
  val hash_sync   : hash -> unit 

  type hash_cursor  
  type cursor_direction = Next | Prev

  val hash_cursor_to_cursor : hash_cursor -> cursor_direction -> (hash_key * hash_value) Cursor.cursor

  (*  Open a cursor on Hash Table *)
  val hash_cursor_open : hash -> hash_cursor
  (*  Place cursor at record associated with hash_key *)
  val hash_cursor_put : hash_cursor -> hash_key -> hash_value -> unit
  (*  Get record at cursor's current location *)
  val hash_cursor_get_next : hash_cursor -> (hash_key * hash_value) option
  (*  Following functions are analogous *)
  val hash_cursor_get_prev : hash_cursor -> (hash_key * hash_value) option
  val hash_cursor_get_first : hash_cursor -> (hash_key * hash_value) option 
  val hash_cursor_get_last : hash_cursor -> (hash_key * hash_value) option

  (* Position the cursor via get methods *)
  val hash_cursor_get_set : hash_cursor  -> hash_key -> (hash_key * hash_value) option
  val hash_cursor_get_both : hash_cursor -> (hash_key * hash_value) -> (hash_key * hash_value) option

  val hash_cursor_del : hash_cursor -> unit
  val hash_cursor_close : hash_cursor -> unit
end


