(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: shredded_main_memory_record.ml,v 1.2 2007/02/01 22:08:54 simeon Exp $ *)

module Main_Memory_Record = struct
  type preorder      = Shredded_main_memory_basetypes.Main_Memory_Basetypes.preorder
  type stored_nodeid = Shredded_main_memory_basetypes.Main_Memory_Basetypes.stored_nodeid

  type eqnameid      = Shredded_main_memory_basetypes.Main_Memory_Basetypes.eqnameid
  type namespaceid   = Shredded_main_memory_basetypes.Main_Memory_Basetypes.namespaceid
  type prefixid      = Shredded_main_memory_basetypes.Main_Memory_Basetypes.prefixid
  type textid        = Shredded_main_memory_basetypes.Main_Memory_Basetypes.textid
  type commentid     = Shredded_main_memory_basetypes.Main_Memory_Basetypes.commentid
  type processingid  = Shredded_main_memory_basetypes.Main_Memory_Basetypes.processingid

  type elem_symbol   = Shredded_main_memory_basetypes.Main_Memory_Basetypes.elem_symbol
  type attr_symbol   = Shredded_main_memory_basetypes.Main_Memory_Basetypes.attr_symbol
  type type_symbol   = Shredded_main_memory_basetypes.Main_Memory_Basetypes.type_symbol

  type record_specific = Shredded_main_memory_basetypes.Main_Memory_Basetypes.record_specific
  type record_kind     = Shredded_main_memory_basetypes.Main_Memory_Basetypes.record_kind

  open Shredded_main_memory_basetypes.Main_Memory_Basetypes

  exception Not_Applicable of string
  exception Main_Memory_Record of string
  
  type symbol = prefixid * eqnameid 
  let get_eqnameid = snd
  let get_prefixid = fst
  let get_eqnameid_opt opt = 
    match opt with 
      | None -> None
      | Some v -> Some (get_eqnameid v)

  let get_prefixid_opt opt = 
    match opt with 
      | None -> None
      | Some v -> Some (get_prefixid v)


  type element_record = 
      { elem_name   : symbol ;
	elem_type_name   : symbol option;
	namespaceid : namespaceid; } 

  let mk_elem_record en tnopt n = 
    { elem_name = en; elem_type_name = tnopt; namespaceid = n } 
      
  type attribute_record = 
      { attribute_name : symbol;
	attr_type_name : symbol option;
	attr_content   : textid;
      }

  let mk_attr_record sym topt content = 
    { attribute_name = sym;
      attr_type_name      = topt;
      attr_content        = content
    }
  type text_record = textid

  type pi_record      = processingid
  type comment_record = commentid

  type record_content = 
    | Element of element_record
    | Attribute of attribute_record
    | Text of text_record
    | ProcessingInstruction of pi_record
    | Comment of comment_record
    | Document 

  let set_symbol_elem_name er s = 
    Element (mk_elem_record s er.elem_type_name er.namespaceid)

  let set_symbol_elem_type er s = 
    Element (mk_elem_record er.elem_name s er.namespaceid)
      
  let set_symbol_attr_name a s = 
    Attribute (mk_attr_record s a.attr_type_name a.attr_content)

  let set_symbol_attr_type a t = 
    Attribute (mk_attr_record a.attribute_name t a.attr_content)
      
  type record = 
      { mutable preorder : preorder;
	mutable parent   : stored_nodeid option;
	mutable content  : record_content
	  (* kind ? *) } 
      
  let is_fixed_length () = false

  let get_record_size () =  raise (Not_Applicable ("Record size ill-defined"))
  let encode record      =  raise (Not_Applicable ("Do not encode main-memory records"))
  let decode array       =  raise (Not_Applicable ("Do not decode main-memory records"))
			 

  
  let create_record preorder parent r_spec = 
    let content = 
      match r_spec with
	| ElementRecord (elem,nid,ts_opt) -> 
	    Element (mk_elem_record elem ts_opt nid)
	| AttributeRecord (attr_symbol,textid, type_symbol_opt) ->
	    Attribute (mk_attr_record attr_symbol type_symbol_opt textid) 	  
	| TextRecord textid -> Text textid
	| PIRecord processingid -> ProcessingInstruction processingid	  
	| CommentRecord commentid -> Comment commentid	  
	| DocumentRecord -> Document
    in
      { preorder = preorder; parent = parent; content = content } 

  let get_preorder r = r.preorder
  let set_preorder r p = r.preorder <- p; r
    
  let get_parent r   = r.parent
  let set_parent r p = r.parent <- p; r       
    
  let get_name_eqnameid r = 
    match r.content with
      | Element er  -> get_eqnameid er.elem_name
      | Attribute a -> get_eqnameid a.attribute_name
      | _ -> raise (Main_Memory_Record ("get_name_eqnameid on wrong type of node"))

  let get_name_prefixid r = 
    match r.content with
      | Element er  -> get_prefixid er.elem_name
      | Attribute a -> get_prefixid a.attribute_name
      | _ -> raise (Main_Memory_Record ("get_name_eqnameid on wrong type of node"))

  let get_name_symbol r = 
    match r.content with
      | Element er  -> er.elem_name
      | Attribute a -> a.attribute_name
      | _ -> raise (Main_Memory_Record ("get_name_eqnameid on wrong type of node"))

  let set_name_symbol r s = 
    let content = 
      match r.content with
	| Element er  -> set_symbol_elem_name er s
	| Attribute a -> set_symbol_attr_name a s
	| _ -> raise (Main_Memory_Record ("set_name_symbol on wrong type of node"))
    in 
      r.content <- content; r


  let get_type_eqnameid r = 
    match r.content with
      | Element er  -> get_eqnameid_opt er.elem_type_name
      | Attribute a -> get_eqnameid_opt a.attr_type_name
      | _ -> raise (Main_Memory_Record ("get_type_eqnameid on wrong type of node"))

  let get_type_prefixid r = 
    match r.content with
      | Element er   -> get_prefixid_opt er.elem_type_name
      | Attribute  a -> get_prefixid_opt a.attr_type_name
      | _ -> raise (Main_Memory_Record ("get_type_prefixid on wrong type of node"))
  
  let get_type_symbol r = 
    match r.content with
      | Element er -> er.elem_type_name
      | Attribute a -> a.attr_type_name
      | _ -> raise (Main_Memory_Record ("get_type_symbol wrong type of node"))
	
  let set_type_symbol r t = 
    let content = 
      match r.content with
	| Element er   -> set_symbol_elem_type er t
	| Attribute a -> set_symbol_attr_type a t
	| _ -> raise (Main_Memory_Record ("set_type_symbol wrong type of node"))
    in r.content <- content; r

  let get_attribute_value_id r = 
    match r.content with
      | Attribute {attr_content=a} -> a 
      | _ -> raise (Main_Memory_Record ("get_attribute_value wrong type of node"))
	
  let get_textid r = 
    match r.content with
      | Text tr -> tr 
      | _ -> raise (Main_Memory_Record ("get_textid wrong type of node"))

  let get_processing_instruction_id r = 
    match r.content with
      | ProcessingInstruction pid -> pid
      | _ -> raise (Main_Memory_Record ("get_processing_instruction on wrong type of node"))

  let get_commentid r = 
    match r.content with
      | Comment c -> c 
      | _ -> raise (Main_Memory_Record ("get_commentid wrong type of node"))

  let get_kind r = 
    match r.content with
      | Element   _ -> ElementRecordKind
      | Attribute _ -> AttributeRecordKind
      | Text      _ -> TextRecordKind
      | ProcessingInstruction  _ -> PIRecordKind 
      | Comment   _ -> CommentRecordKind
      | Document    -> DocumentRecordKind


  let get_specific r = 
    match r.content with 
      | Element ({elem_name=en;elem_type_name=et;namespaceid=n}) ->
	  ElementRecord (en,n,et)
      | Attribute ({attribute_name=an;attr_type_name=tn;attr_content=ac}) ->
	  AttributeRecord (an, ac, tn) 

      | Text tr ->  TextRecord tr
      | ProcessingInstruction pi -> PIRecord  pi
      | Comment cid -> CommentRecord  cid
      | Document -> DocumentRecord

  
  let is_text_record r = 
    match r.content with 
      | Text _ -> true
      | _ -> false

  let is_elem_record r = 
    match r.content with
      | Element _ -> true
      | _ -> false

  let is_attr_record r = 
    match r.content with
      | Attribute _ -> true
      | _ -> false

  let get_namespaceid r = 
    match r.content with 
      | Element ({namespaceid=n}) -> n
      | _ -> raise (Main_Memory_Record ("get_namespaceid on wrong type of node"))
    

end
(* Type checking hack *) 
module Restriction = (Main_Memory_Record : Shredded_store_sigs.Node_Record)
