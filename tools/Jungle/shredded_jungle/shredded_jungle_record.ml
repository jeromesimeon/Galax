(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: shredded_jungle_record.ml,v 1.2 2007/02/01 22:08:54 simeon Exp $ *)

(* Module : Shredded_jungle_record 
   Description :

   Records are all fixed Length, all elements are integers. We store them
   as characters because BDB requires a different byte encoding.
   (Records are longer now! - they are 11 integers long)

00 - denotes padding
These are the types of elements we store:

Element:
  |    0     |     1    |   2  |       3        |        4           |       5        |          6         |   7       |
  | preorder | parentid | kind | name_prefix_id | name_uri_ncname_id | type_prefix_id | type_uri_ncname_id | nsid      | 
Attribute:
  |    0     |     1    |   2  |       3        |        4           |       5        |          6         |   7       |
  | preorder | parentid | kind | name_prefix_id | name_uri_ncname_id | type_prefix_id | type_uri_ncname_id |  textid   | 
Text: 
  |    0     |     1    |   2  |       3        |        4           |       5        |          6         |   7       |
  | preorder | parentid | kind |  0000000000000 | 000000000000000000 | 00000000000000 | 000000000000000000 | textid    |
PIRecord: 
  |    0     |     1    |   2  |       3        |        4           |       5        |          6         |   7       |
  | preorder | parentid | kind |  0000000000000 | 000000000000000000 | 00000000000000 | 000000000000000000 | pi_id     |
Comment 
  |    0     |     1    |   2  |       3        |        4           |       5        |          6         |   7       |
  | preorder | parentid | kind |  0000000000000 | 000000000000000000 | 00000000000000 | 000000000000000000 | commentid |
Document
  |    0     |     1    |   2  |       3        |        4           |       5        |          6         |   7       |
  | preorder | 00000000 | kind |  0000000000000 | 000000000000000000 | 00000000000000 | 000000000000000000 | 000000    |

This modules manipulates these records.

*)


open String
open Nodeid

open Shredded_jungle_basetypes
open Shredded_encode

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

exception Shredded_Jungle_Record_Type_Error of string

type shredded_jungle_record = char array
type record = shredded_jungle_record

let size_of_int             = 4
let size_of_int64_pair      = 4 * size_of_int
let preorder_offset         = 0 * size_of_int64_pair
let parent_offset           = size_of_int64_pair 
let kind_offset             = 1 * size_of_int + size_of_int64_pair
let prefix_name_offset      = 2 * size_of_int + size_of_int64_pair
let uri_ncname_offset       = 3 * size_of_int + size_of_int64_pair
let prefix_type_offset      = 4 * size_of_int + size_of_int64_pair
let uri_ncname_type_offset  = 5 * size_of_int + size_of_int64_pair
let textid_offset           = 6 * size_of_int + size_of_int64_pair
let nsid_id_offset          = 7 * size_of_int + size_of_int64_pair
let comment_id_offset       = 7 * size_of_int + size_of_int64_pair
let pi_id_offset            = 7 * size_of_int + size_of_int64_pair
let record_size             = 8 * size_of_int + size_of_int64_pair

let is_fixed_length      () = true
let get_record_size      () = record_size

(* We rely that generators start > 1 *)
let no_parent_marker        = 0
let no_type_eqname          = 0
let no_type_prefix          = 0

(*********************)
(* Private encodings *)
(*********************)

let encoding_of_kind k = 
  match k with
    | ElementRecordKind   -> '1'
    | AttributeRecordKind -> '2'
    | TextRecordKind      -> '3'
    | PIRecordKind        -> '4'
    | CommentRecordKind   -> '5'
    | DocumentRecordKind  -> '6'

let kind_of_encoding i = 
  match i with 
    | '1' -> ElementRecordKind
    | '2' -> AttributeRecordKind  
    | '3' -> TextRecordKind      
    | '4' -> PIRecordKind        
    | '5' -> CommentRecordKind    
    | '6' -> DocumentRecordKind  
    | _ -> 
	let l = " " in l.[0] <- i;
	raise (Shredded_Jungle_Record_Type_Error 
		    ("Found integer: " ^ l ^ " when looking for kind in a shredded_jungle record"))

let encode record = record
let decode record_str = record_str

(*************)
(* Accessors *)
(*************)
let get_preorder record = 
  let g,m = inplace_decode_int64_pair record preorder_offset in
    {gId=g;mId=m}


let set_preorder record new_pre = 
  let {gId=g;mId=m} = new_pre in 
  let () = inplace_encode_int64_pair record preorder_offset (g,m) in record


let get_parent record = 
  let v = inplace_decode_int record parent_offset in 
    if v = no_parent_marker then
      None
    else Some v
    
let set_parent record new_parent = 
  let () = 
  match new_parent with
    | Some new_parent ->
	inplace_encode_int record parent_offset new_parent
    | None -> 
	inplace_encode_int record parent_offset no_parent_marker
  in record

let get_kind record = kind_of_encoding record.(kind_offset)
let set_kind record k = record.(kind_offset) <- encoding_of_kind k

(***********************)
(* Attr/Element Qnames *)
(***********************)
let get_name_eqnameid_unsafe record = inplace_decode_int record uri_ncname_offset
let set_name_eqnameid record v    = inplace_encode_int record uri_ncname_offset v

let get_name_eqnameid record =
  let k = get_kind record in 
    if k = ElementRecordKind || k = AttributeRecordKind then
      get_name_eqnameid_unsafe record
    else
      raise (Shredded_Jungle_Record_Type_Error "Getting Qname not supported on the node")


(* Qname Prefixes *)
let get_name_prefix_unsafe record = inplace_decode_int record prefix_name_offset
let set_name_prefixid record     v= inplace_encode_int record prefix_name_offset v

let get_name_prefixid record =
  let k = get_kind record in 
    if k = ElementRecordKind || k = AttributeRecordKind then
      get_name_prefix_unsafe record
    else
      raise (Shredded_Jungle_Record_Type_Error "Getting Qname Prefix not supported on the node")

let get_name_symbol record = 
  (get_name_prefixid record),
  (get_name_eqnameid record)

let set_name_symbol record (prefixid, eqnameid) =
  set_name_prefixid record prefixid;
  set_name_eqnameid record eqnameid;
  record
    

(***************)
(* Type Qnames *)
(***************)
let type_prefix_valid_or_opt   v = if v = no_type_prefix then None else Some v
let type_eqnameid_valid_or_opt v = if v = no_type_eqname then None else Some v

let get_type_eqnameid_unsafe record = inplace_decode_int record uri_ncname_type_offset

let set_type_eqnameid record v  =   
  inplace_encode_int record uri_ncname_type_offset v

let get_type_eqnameid record =
  let k = get_kind record in 
    if k = ElementRecordKind || k = AttributeRecordKind then
      type_eqnameid_valid_or_opt (get_type_eqnameid_unsafe record)
    else
      raise (Shredded_Jungle_Record_Type_Error "Getting Qname not supported on the node")

(* Qname Prefixes *)
let get_type_prefix_unsafe record = inplace_decode_int record prefix_type_offset
let set_type_prefixid record v    = inplace_encode_int record prefix_type_offset v

let get_type_prefixid record =
  let k = get_kind record in 
    if k = ElementRecordKind || k = AttributeRecordKind then
      type_prefix_valid_or_opt (get_type_prefix_unsafe record)
    else
      raise (Shredded_Jungle_Record_Type_Error "Getting Qname Prefix not supported on the node")


let get_type_symbol record = 
  match get_type_eqnameid record with
    | None   -> None
    | Some v -> 
	begin
	  match get_type_prefixid record with
	    | None -> 
		raise (Shredded_Jungle_Record_Type_Error "Record Corrupt. type prefixid not set but eqnameid sis")
	    | Some p ->
		Some (v, p)
	end

let set_type_symbol record symopt = 
  match symopt with
    | None -> 
	set_type_prefixid record no_type_prefix;
	set_type_eqnameid record no_type_eqname;
	record
    | Some (p,e) ->
	if p = no_type_prefix then
	  raise (Shredded_Jungle_Record_Type_Error "Corrupt Store! Setting an invalid prefix")
	else if e = no_type_eqname then
	  raise (Shredded_Jungle_Record_Type_Error "Corrupt Store! Setting an invalid eqname")
	else
	  begin
	    (* Format.printf "Setting type_symbol %d %d@." p e; *)
	    set_type_prefixid record p;
	    set_type_eqnameid record e;
	      record
	  end

let get_attribute_value_id record = 	
  let k = get_kind record in 
    if k = AttributeRecordKind then
      inplace_decode_int record textid_offset
    else
      raise (Shredded_Jungle_Record_Type_Error "Get attribute value operation not supported on the node")
  
let get_namespaceid record = 	
  let k = get_kind record in
    if k = ElementRecordKind then 
      inplace_decode_int record nsid_id_offset
    else 
      raise (Shredded_Jungle_Record_Type_Error 
		  "Get namespaceid value operation not supported on the node")

let set_namespaceid record nsid = inplace_encode_int record nsid_id_offset nsid

let get_textid record = 
  let k = get_kind record in    
    match k with 
      | TextRecordKind -> 
	  inplace_decode_int record textid_offset
      | _	-> raise (Shredded_Jungle_Record_Type_Error "Get textid operation not supported on the node")

let set_textid record t = inplace_encode_int record textid_offset t 


let get_commentid record  =
    match get_kind record with
      | CommentRecordKind -> 
	  inplace_decode_int record comment_id_offset
      | _ -> raise (Shredded_Jungle_Record_Type_Error "Get comment operation not supported on the node")

let set_commentid record = inplace_encode_int record comment_id_offset


let get_processing_instruction_id record  =
    match get_kind record with
      | PIRecordKind -> 
	  inplace_decode_int record pi_id_offset
      | _ -> raise (Shredded_Jungle_Record_Type_Error "Get comment operation not supported on the node")

let set_pi_id record = inplace_encode_int record pi_id_offset


(*************************)
(* Convenience Functions *)
(*************************)
let is_text_record record = 
  let kind = get_kind record in 
    match kind with 
      | TextRecordKind -> true
      | _ 	-> false

let is_elem_record record = 
  let kind = get_kind record in 
    match kind with 
      | ElementRecordKind -> true
      | _ 	-> false

let is_attr_record record = 
  let kind = get_kind record in 
    match kind with 
      | AttributeRecordKind -> true
      | _ 	-> false	  
	  
 let get_specific record = 
   match get_kind record with
     | ElementRecordKind ->
	 let elem_sym = get_name_symbol record in 
	 let nsid     = get_namespaceid record in
	 let typesym  = get_type_symbol record in 
	   ElementRecord (elem_sym, nsid, typesym)

     | AttributeRecordKind -> 
	 let attr_sym = get_name_symbol record in 
	 let textid   = get_attribute_value_id record in
	 let typesym  = get_type_symbol record in 
	   AttributeRecord (attr_sym, textid, typesym)

     | TextRecordKind -> 
	 TextRecord (get_textid record)

     | PIRecordKind -> 
	 PIRecord (get_textid record)

     | CommentRecordKind -> 
 	 CommentRecord (get_textid record)

     | DocumentRecordKind -> DocumentRecord

(*****************)
(* Create method *)
(*****************)

let padding = char_of_int 0;;

let print_opt_type ff t =
  match t with
    | None -> Format.fprintf ff "None";
    | Some (p,e) -> Format.fprintf ff "(%d,%d)" p e

let create_record pre parent specific = 
  let record  = Array.make record_size padding in 
  let record  = set_preorder record pre in 
  let record  = set_parent record parent in 
  let ()      =  
    match specific with 
      | ElementRecord (name_sym, nsid, tname) ->	  
	  set_kind record ElementRecordKind;
	  ignore (set_name_symbol record name_sym);
	  set_namespaceid record nsid;
	  ignore (set_type_symbol record tname );
	    (* ;
					  Format.printf "Element Type: %a (%d,%d) | Qname: %d %d@." print_opt_type tname
	    (get_type_prefix_unsafe record)
	    (get_type_eqnameid_unsafe record)
	    (get_name_prefixid record)
	    (get_name_eqnameid record) *)
	    

      | AttributeRecord (attr_sym,textid,tname) ->
	  (* Format.printf "Attribute: prefixid: %d qname: %d textid: %d@." 
	    pid qnameid textid; *)
	  set_kind record AttributeRecordKind;	  
	  ignore (set_name_symbol record attr_sym);
	  set_textid record textid;
	  ignore (set_type_symbol record tname) (* ;
	  Format.printf "Attribute Type: %a (%d,%d) | Qname (%d,%d)@." print_opt_type tname
	    (get_type_prefix_unsafe record)
	    (get_type_eqnameid_unsafe record)
	    (get_name_prefixid record)
	    (get_name_eqnameid record) *)


      | TextRecord text ->
	  (* Format.printf "Textid: %d@." text; *)
	  set_kind record TextRecordKind;
	  set_textid record text

      | PIRecord pi_id -> 
	  (* Format.printf "PIRecord: %d@." text; *)
	  set_kind record PIRecordKind;
	  set_pi_id record pi_id

      | CommentRecord commentid ->
	  (* Format.printf "Comment: %d@." text; *)
	  set_kind record CommentRecordKind;
	  set_commentid record commentid

      | DocumentRecord ->
	  set_kind record DocumentRecordKind
  in	  
	  record
