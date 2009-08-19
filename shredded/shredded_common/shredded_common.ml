(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: shredded_common.ml,v 1.7 2007/10/16 01:25:35 mff Exp $ *)
(* This contains the type conversion information.
   Since jungle only stores strings, these modules
   parameterize the btree and hash key-value pairs. *)
 
(* This is still dirty, because we go back and forth with
   strings, we can make this more general and have a signature like:
   module type Shred_Type = 
   sig 
    type t
    val encoding_of_type : t -> encoded_type
    val type_of_encoding : encoded_type -> t
   end

   Where encoded_type is a structure over which the btrees are built.
   A particular store gives its encoded type structure, which is
   responsible for conversions. For example, a native implementation
   could have encoded_type being a union type, while jungle could have
   it a string. You can imagine crazier things, like encrypted and
   compressed encodings blah blah... - Chris
*)
(* REMOVE THIS AND THROW QUERY! *)
exception Shred_Common_Exception of string
(* Try to remove the copies here *)
(********************) 
(* Module signature *)
(********************) 
let jungle_string_of_int i = 
  let a = Array.make 4 '0' in
    Shredded_encode.inplace_encode_int a 0 i;
    a

let jungle_int_of_string s = 
  Shredded_encode.inplace_decode_int s 0 

module type Shred_Type = 
sig 
  type t
  type encoded_type = char array
  val encode : t -> encoded_type
  val decode : encoded_type -> t
end


(* These are intended so that we have fewer copies *)
(* Additionally, these modules should produce output
   that can be nested (i.e. strings must keep track of 
   their length and not rely on the lenght of the remaining
   bits in the array *)
module type Inplace_Type = 
sig
  type t
  type encoded_type = char array  
  val needed_length    : t -> int
  val serialized_space : encoded_type -> int -> int

  val encode_inplace : t -> encoded_type -> int (*offset*) -> encoded_type
  val decode_inplace : encoded_type -> int -> t    
end


(*********************************************)
(* Implementations, base types               *)
(*********************************************)
module Int_Type = struct
  type t = int
  type record = t
  type encoded_type = char array
  let encoded_size = 4
  let encode   t = jungle_string_of_int t
  let decode   s = jungle_int_of_string s
  let is_fixed_length () = true
  let get_record_size () = 4
  let compare a b = 
    if a = b then 0 
    else if (a - b) < 0 then -1 else 1

  let fprintf_value ff v = Format.fprintf ff "%d"  v
end

module Inplace_Int_Type = struct
  type t = int  
  type encoded_type = char array
  let encoded_size    = 4
  let needed_length x = Shredded_encode.needed_length_for_int
  let serialized_space s o = Shredded_encode.needed_length_for_int

  let inplace_encode  = Shredded_encode.inplace_encode_int
  let inplace_decode  = Shredded_encode.inplace_decode_int
end


module Bool_Type = struct
  type t = bool
  type encoded_type = char array
      
  (* C style *)
  let bool_of_int x = if x = 0 then false else true
  let int_of_bool x = if x then 1 else 0

  let needed_length x = Shredded_encode.needed_length_for_int 
  let serialized_space s o = Shredded_encode.needed_length_for_int

  let inplace_encode a o v = Shredded_encode.inplace_encode_int a o (int_of_bool v)
  let inplace_decode a o   = bool_of_int (Shredded_encode.inplace_decode_int a o)
end


module String_Type = struct
  type t = string
  type encoded_type = char array
  let encode   t = Shredded_encode.bits_of_string t
  let decode   s = Shredded_encode.string_of_bits s		     
  let compare    = String.compare
  let fprintf_value ff s = Format.fprintf ff "%s" s
end

module Inplace_String_Type = struct
  open Shredded_encode
  type t = string
  type encoded_type = char array  

  (* To Prevent Copies *)
  let needed_length str = 
    let encoding_length_str = Shredded_encode.length_bits_of_string str in
       Shredded_encode.needed_length_for_int + encoding_length_str

  let serialized_space a offset = 
    let str_length = inplace_decode_int a offset in 
      str_length + Shredded_encode.serialized_int_size 
	
  let inplace_encode output output_offset str = 
    let encoding_length_str = length_bits_of_string str in
    let () = inplace_encode_int output output_offset encoding_length_str in
    let () = inplace_encode_string output (output_offset+Shredded_encode.needed_length_for_int) str in 
      ()

  let inplace_decode input input_offset = 
    let length = inplace_decode_int input input_offset in 
      inplace_decode_string input (input_offset+4) length            

end


(**********************)
(* Encode Native Ints *)
(**********************)
module Inplace_Native_Int_Type = struct
  type t = Int32.t
  type encoded_type = char array
  let needed_length x = 4
  let serialized_space s o = 4
  let char_mask       = 255

  let inplace_encode (* a offset v *) = Shredded_encode.inplace_encode_int32
  let inplace_decode (* a offset   *) = Shredded_encode.inplace_decode_int32
end
(*****************)
(* Encode Int64s *)
(*****************)
module Inplace_Int64_Type = struct
  type t = Int64.t
  type encoded_type = char array
  let needed_length x = 8
  let serialized_space s o = 8
  let char_mask       = 255

  let inplace_encode (* a offset v *) = Shredded_encode.inplace_encode_int64
  let inplace_decode (* a offset   *) = Shredded_encode.inplace_decode_int64
end

(*************************)
(* Encode Pair of Int64s *)
(*************************)
module Pair_of_Int64_Type = struct
  let needed_length x = 16
  let serialized_length s o = 16
		
  (* v0 is the less signifcant of the two *)
  let inplace_encode a o (v0,v1) =
    Inplace_Int64_Type.inplace_encode a o v0;
    Inplace_Int64_Type.inplace_encode a (o+8) v1

  let inplace_decode a o =
    let v0 = Inplace_Int64_Type.inplace_decode a o in 
    let v1 = Inplace_Int64_Type.inplace_decode a (o+8) in 
      (v0,v1)
end

(**************************************************)
(* These are 'more' strongly typed then the above *)
(**************************************************)
(* Convert these to inplace varieties. *)
module Textid_Type = struct
  type t = int
  type encoded_type = char array
  let encode   t = jungle_string_of_int t
  let decode   s = jungle_int_of_string s
end

module Namespaceid_Type = struct
  type t = int
  type encoded_type = char array
  let needed_length x = Shredded_encode.needed_length_for_int
  let serialized_space s o = Shredded_encode.needed_length_for_int
  let encode   t = jungle_string_of_int t
  let decode   s = jungle_int_of_string s

  let inplace_encode  = Shredded_encode.inplace_encode_int
  let inplace_decode  = Shredded_encode.inplace_decode_int
end

module Uri_prefix_mapid = struct
  type t = int
  type encoded_type = char array
  let encode   t = jungle_string_of_int t
  let decode   s = jungle_int_of_string s
end

(* The encoding here should be in *big* endian order,
   because of a quirk in BDB *)
(* module Preorder_Type = struct
  type t = Nodeid.preorder
  type encoded_type = char array
  let encode   t = jungle_string_of_int t
  let decode   s = jungle_int_of_string s
end *)

module Text_Type = String_Type



(*****************************************)
(* This depends on the size of the types *)
(*****************************************)
(* More verbose so can be used in lists *)
module Namespace_Prefix_Type = struct
  type t = Namespace_names.prefix
  type encoded_type = char array 
  open Shredded_encode
  let encode t = 
    let length_desc, prefix_length, str = 
      match t with
	| Namespace_names.NSDefaultElementPrefix  -> -1, 4, None
	| Namespace_names.NSDefaultFunctionPrefix -> -2, 4, None
	| Namespace_names.NSWildcardPrefix        -> -3, 4, None
	| Namespace_names.NSPrefix pref           
	| Namespace_names.NSServerPrefix pref           
	| Namespace_names.NSInterfacePrefix pref           -> 
	    (String.length pref), (String.length pref) + 4, Some pref
    in
    let a = Array.make prefix_length '0' in
    let () = inplace_encode_int a 0 length_desc in 
    let () = match str with Some t -> 
      inplace_encode_string a 4 t | None -> () in
      a

    let decode s = 
      let length_desc = inplace_decode_int s 0 in
	match length_desc with
	  | -1 -> Namespace_names.NSDefaultElementPrefix
	  | -2 -> Namespace_names.NSDefaultFunctionPrefix
	  | -3 -> Namespace_names.NSWildcardPrefix
	  | x when length_desc > 0 ->
	      Namespace_names.NSPrefix (inplace_decode_string s 4 x) 
	  | _ -> raise (Shred_Common_Exception "Unknown Namespace Constant")
	  
end

module Strength_Namespace_Uri_Type = struct
  open Shredded_encode
  let encode nsuri = 
    match nsuri with
      | Namespace_names.NSWildcardUri ->
	  let a = Array.make 4 '0' in
	    inplace_encode_int a 0 (-1);
	    a

      | Namespace_names.NSUri uri ->
	  let uri_l = String.length uri in 
	  let a = Array.make (4+uri_l) '0' in 
	    inplace_encode_int a 0 uri_l;
	    inplace_encode_string a 4 uri;
	    a

  let decode d = 
    let uri_length = inplace_decode_int d 0 in
      if uri_length >= 0 then
	begin
	  let str = inplace_decode_string d 4 uri_length in 
	    Namespace_names.NSUri str
	end
      else if uri_length = -1 then
	Namespace_names.NSWildcardUri
      else raise (Shred_Common_Exception "Unknown URI type")
	      
    
    
end

module Binding_Type = struct
  type t = int * Namespace_context.binding_table
  open Shredded_encode
  type encoded_type = char array
      
  let encode_binding (p,u) = 
    (Namespace_Prefix_Type.encode p)
    :: (Strength_Namespace_Uri_Type.encode u) 
    :: []

  let encode   (n,p_list) =    
    let p_list = List.concat (List.map encode_binding p_list) in
    let n      = copy_encode n in 
    let len    = copy_encode (List.length p_list) in 
      Array.concat ([n;len] @ p_list)

  let decode  s = 
    let n   = inplace_decode_int s 0 in
    (n, [])

end 

(* THESE ARE VERY EXPENSIVE TO ENCODE! *)
module Processing_Type   = struct
  open Shredded_encode
  type t = Namespace_names.ncname * Datatypes.xs_untyped
  type encoded_type = char array
  let encode (ncname,t) = 
    let b1  = bits_of_string ncname in
    let s2  = bits_of_string t in
    let len = jungle_string_of_int (Array.length b1) in 
      Array.concat [len; b1; s2]
      
  let decode s = 
    let len      = Shredded_encode.inplace_decode_int s 0  in
    let name_str = string_of_bits (Array.sub s 4 len) in 
    let text     = string_of_bits (Array.sub s (4+len) ((Array.length s) - (4+len)))  in
      (name_str,text)
end 

(* These are expensive to decode and encode *)
module Eqname_Type = struct
  open Shredded_encode
  type t = Namespace_names.uri * Namespace_names.ncname
  type encoded_type = char array
  (* Layout:
     | 0          | 4   | 4 + uri_length |
     | uri_length | uri | local_name     |
     
      uri_length = -1 <==> uri = NSWildcardUri
  *)    
  let wild_card = -1 
  let decode a = 
    let uri_length = inplace_decode_int a 0 in
    let total_length  = Array.length a in 
      if uri_length < 0 then
	begin	  
	  let ncname = inplace_decode_string a 4 (total_length - 4) in
	    (Namespace_names.NSWildcardUri, ncname)
	end
      else
	begin
	  let uri    = inplace_decode_string a 4 uri_length in 
	  let ncname = inplace_decode_string a (4+uri_length) (total_length - (4+uri_length)) in 
	    (Namespace_names.NSUri uri, ncname)
	end

  let encode (uri,name) = 
    match uri with
      | Namespace_names.NSUri uri ->
	  let uri_l  = String.length uri in
	  let length = 4 + uri_l + (String.length name) in
	  let a      = Array.make length '0' in
	  let ()     = inplace_encode_int a 0 uri_l in 
	  let ()     = inplace_encode_string a 4 uri in
	  let ()    = inplace_encode_string a (4+uri_l) name in
	    a

      | Namespace_names.NSWildcardUri ->
	  let length = 4 + String.length name in
	  let a      = Array.make length '0' in
	  let ()     = inplace_encode_int a 0 wild_card in
	  let ()     = inplace_encode_string a 4 name in
	    a
end

module Atomic_Type_Type = struct
  open Datatypes (* Union Type comes from here *)
  type t = Datatypes.atomic_type
  type encoded_type = char array
  let internal_encode v = 
    match v with 
      | ATString   -> 10
      | ATBoolean  -> 11
      | ATDecimal  -> 12
      | ATFloat    -> 13
      | ATDouble   -> 14
      | ATDuration -> 15
      | ATDateTime -> 16
      | ATTime     -> 17
      | ATDate     -> 18
      | ATGYearMonth -> 19
      | ATGYear      -> 20
      | ATGMonthDay  -> 21
      | ATGDay       -> 22
      | ATGMonth     -> 23
      | ATHexBinary  -> 24
      | ATBase64Binary -> 25
      | ATAnyURI      -> 26
      | ATQName       -> 27
      | ATNOTATION    -> 28

      (* Derived atomic types *)
      | ATInteger     -> 29

      (* xs data types *)
      | ATYearMonthDuration -> 30
      | ATDayTimeDuration   -> 31
      | ATUntypedAtomic     -> 32

      (* built-in Ur atomic type *)
      | ATAnyAtomic         -> 33

  let internal_decode i = 
    match i with 
      | 10 -> ATString   
      | 11 -> ATBoolean  
      | 12 -> ATDecimal  
      | 13 -> ATFloat    
      | 14 -> ATDouble   
      | 15 -> ATDuration 
      | 16 -> ATDateTime 
      | 17 -> ATTime
      | 18 -> ATDate
      | 19 -> ATGYearMonth
      | 20 -> ATGYear
      | 21 -> ATGMonthDay
      | 22 -> ATGDay
      | 23 -> ATGMonth
      | 24 -> ATHexBinary
      | 25 -> ATBase64Binary
      | 26 -> ATAnyURI
      | 27 -> ATQName
      | 28 -> ATNOTATION
      | 29 -> ATInteger
	  (* xs data types *)
      | 30 -> ATYearMonthDuration 
      | 31 -> ATDayTimeDuration   
      | 32 -> ATUntypedAtomic     

      (* built-in Ur atomic type *)
      | 33 -> ATAnyAtomic               
      | _ -> raise (Shred_Common_Exception ("Unknown Atomic Type Code: " ^ (string_of_int i)))
    
  let encode v = Int_Type.encode (internal_encode v)
  let decode l = internal_decode (Int_Type.decode l)

  (*******************)
  (* Inplace portion *)
  (*******************)
  let needed_length x = Inplace_Int_Type.needed_length x
  let serialized_space s o = Inplace_Int_Type.serialized_space s o 
  let inplace_encode a offset v =
    Inplace_Int_Type.inplace_encode a offset (internal_encode v)
  let inplace_decode a offset = 
    internal_decode (Inplace_Int_Type.inplace_decode a offset)
end

module Inplace_Atomic_Type_Type = Atomic_Type_Type

(* We encode the erased value, and the type
   so that we may cast it back *)
(* We need to use an encoding of strings that stores
   its length as well
*)
module Atomic_Value_Type = struct
  open Shredded_encode
  type t = Datatypes.atomic_type * string
  type encoded_type = char array

  let needed_length (atomic_type, erased_string) = 
    let type_length   = Inplace_Atomic_Type_Type.needed_length atomic_type in 
    let str_length    = Inplace_String_Type.needed_length erased_string in
      type_length + str_length 

  let serialized_space s o = 
    let type_space     = Inplace_Atomic_Type_Type.serialized_space s o in 
      type_space + (Inplace_Int_Type.serialized_space s 0)
    
  let encode_inplace encoded_array offset (atomic_type, erased_string) = 
    let type_space     = Inplace_Atomic_Type_Type.needed_length atomic_type in 
      Inplace_Atomic_Type_Type.inplace_encode encoded_array 0 atomic_type;
      Inplace_String_Type.inplace_encode encoded_array type_space erased_string
	
  let decode_inplace d offset = 
    let atomic_type    = Inplace_Atomic_Type_Type.inplace_decode d offset in
    let type_space     = Inplace_Atomic_Type_Type.needed_length atomic_type in 
    let str_style      = Inplace_String_Type.inplace_decode d (offset + type_space) in 
      (atomic_type, str_style)

  let encode v = 
    let space          = needed_length v in 
    let encoded_array  = Array.create space '0' in 
      encode_inplace encoded_array 0 v;
      encoded_array
      
  let decode s = decode_inplace s 0 
  
end


module Stored_Atomic_Value_List_Type = struct
  open Shredded_encode
(* Layout :

   | 0    | 4      | Av list | 
   | nsid | length | ...     | 
*)
  type t = Namespaceid_Type.t * (Datatypes.atomic_type * string) list
  type encoded_type = char array      

  let needed_length (nid, av_list) = 
    let nid_length   = Namespaceid_Type.needed_length nid in 
    let space_length = Inplace_Int_Type.needed_length (List.length av_list) in 
    let av_length  = List.fold_left (fun cur v -> cur + (Atomic_Value_Type.needed_length v)) 0 av_list in 
      nid_length + space_length + av_length

  let serialized_space enct offset = 
    let current_offset  = offset + (Namespaceid_Type.serialized_space enct offset) in 
    let n_items         = Inplace_Int_Type.inplace_decode enct current_offset in 
    let current_offset  = current_offset + (Inplace_Int_Type.serialized_space enct current_offset) in 
    let current_offset  = ref current_offset in 
      for i = 1 to n_items do
	current_offset := !current_offset + Atomic_Value_Type.serialized_space enct !current_offset;
      done;
	!current_offset - offset
      

  let inplace_encode array offset (nid, av_list) = 
      Namespaceid_Type.inplace_encode array offset nid;
    let current_offset = offset + Namespaceid_Type.needed_length nid in
    let list_length    = List.length av_list in 
      Inplace_Int_Type.inplace_encode array current_offset list_length;
      let current_offset = current_offset + (Inplace_Int_Type.needed_length list_length)  in
      let _    = List.fold_left (fun offset v -> 
	let len = Atomic_Value_Type.needed_length v in 
	Atomic_Value_Type.encode_inplace array offset v;
	len + offset) current_offset av_list in 
      ()
      
  let inplace_decode input input_offset = 
    (* Format.printf "Inplace Decode %d@." input_offset; *)
    let nsid           = Namespaceid_Type.inplace_decode input input_offset in 
    let current_offset = input_offset + Namespaceid_Type.serialized_space input input_offset in 
    (* Format.printf "Inplace Decode current %d@." current_offset; *)
    let n_items        = Inplace_Int_Type.inplace_decode input current_offset in 
    (* Format.printf "Inplace Decode current 2 %d %d@." current_offset n_items; *)
    let current_offset = ref (input_offset + Inplace_Int_Type.serialized_space input input_offset) in     
    let value = ref [] in 
      for i = 1 to n_items do
	value := Atomic_Value_Type.decode_inplace input !current_offset :: !value; 
	current_offset := !current_offset + (Atomic_Value_Type.serialized_space input !current_offset)
      done;
      (* Format.printf "Inplace Decode Leave@. %d" input_offset; *)
	(nsid, List.rev !value)
      

  let encode v = 
    let needed = needed_length v in
    let array  = Array.make needed '0' in 
      inplace_encode array 0 v;
      array

  let decode s = 
    inplace_decode s 0
end


module Stored_Atomic_Value_List_With_Nilled_Type = struct
  type t = bool * Stored_Atomic_Value_List_Type.t
  type encoded_type = char array
      
  let needed_length (b,av_list) = 
    (Bool_Type.needed_length b) + 
      (Stored_Atomic_Value_List_Type.needed_length av_list)

  let serialized_space enct offset = 
    let bool_space = Bool_Type.serialized_space enct offset in 
    let av_space   = Stored_Atomic_Value_List_Type.serialized_space enct (offset + bool_space) in
      bool_space + av_space

  let inplace_encode array offset (b,av_list) = 
    Bool_Type.inplace_encode array offset b;
    let current_offset = offset + (Bool_Type.needed_length b) in
      Stored_Atomic_Value_List_Type.inplace_encode array current_offset av_list
      
  let inplace_decode array offset =
    let b = Bool_Type.inplace_decode array offset in
    let current_offset = Bool_Type.serialized_space array offset in 
    let av_list = Stored_Atomic_Value_List_Type.inplace_decode array current_offset in
      (b,av_list)    

  let encode v = 
    let needed = needed_length v in
    let array  = Array.make needed '0' in 
      inplace_encode array 0 v;
      array

  let decode s = 
    inplace_decode s 0
end


(********************************)
(* These are the cell encodings *)
(********************************)
module Cell_Id_Type = struct
  type tree_id = Int32.t array (* To make the divisions easier *)
  type stored_nodeid = int (* Change this dependence *)

  type t = tree_id * stored_nodeid
  type encoded_type = char array
  let bits_per_int  = 32
  let power_per_int = 5
  let bit_mask      = 31
  
  (***********************************)
  (* Layout                          *)
  (* ----------------------          *)
  (* tid | length | cells |          *)
  (* ----------------------          *)
  (***********************************)
  let tid_offset    = 0 
  let length_offset = 4
  let cell_offset   = 8

  let encode (stored_id,tid) = 
    (* space calculate *)
    let len = Array.length stored_id in 
    let needed_space = 4 + 4 + (len * 4) in 
    let s   = Array.make needed_space '0' in
      Inplace_Int_Type.inplace_encode s tid_offset tid;
      Inplace_Int_Type.inplace_encode s length_offset len;
      for i = 0 to (len - 1) do
	Inplace_Native_Int_Type.inplace_encode s (cell_offset + 4*i) stored_id.(i)
      done;
      s

  let decode array = 
    let tid = Inplace_Int_Type.inplace_decode array tid_offset in 
    let len = Inplace_Int_Type.inplace_decode array length_offset in
    let a   = Array.make len Int32.zero in 
      for i = 0 to (len-1) do
	a.(i) <- Inplace_Native_Int_Type.inplace_decode array (cell_offset + 4*i)
      done;
	(a,tid)

  (* Function to make sense of two cells *)
  let in_same_cell (t1,n1) (t2,n2) level =
    let n_ints = Array.length t1 in
    let int_offset = level lsr power_per_int in 
    let int_mask   = level land bit_mask in 
    let b_equal    = ref true in  
      (***************************************)
      (* There are low order bits to compare *)
      (***************************************)
      if int_mask > 0 then
	begin	  
	  let t1_i = Int32.shift_right_logical t1.(int_offset) int_mask in 
	  let t2_i = Int32.shift_right_logical t2.(int_offset) int_mask in 
	    (* Format.printf "Low Order: mask: %d - %d %d@." int_mask (Int32.to_int t1_i) (Int32.to_int t2_i); *)
	    b_equal := t1_i = t2_i
	end 
	else (* Format.printf "No Low Order Compare %d@." int_mask *) ()
      ;

      (*************************)
      (* Compare complete ints *)
      (* Should be faster      *)
      (*************************)
      let i          = 
	if int_mask = 0 
	then ref int_offset 
	else ref (int_offset + 1) 
      in 

	while !i < n_ints  && !b_equal do
	  b_equal := (t1.(!i) = t2.(!i));
	  incr i
	done;
	!b_equal
end

module Preorder_Type = struct
  open Nodeid
  type t = large_preorder
  type encoded_type = char array
  let encoded_size = 16
  let inplace_encode a o {gId=g;mId=m} = 
    Shredded_encode.inplace_encode_int64_pair a o (g,m)

  let inplace_decode a o = 
    let g,m = Shredded_encode.inplace_decode_int64_pair a o in 
      {gId=g;mId=m}

  let encode v = 
    let a = Array.make encoded_size '0' in 
      inplace_encode a 0 v;
      a

  let decode a = inplace_decode a 0     
  let compare {gId=a0;mId=a1} {gId=b0;mId=b1} = 
    if a0 = b0 then
      if a1 = b1 then 0
      else if a1 < b1 then -1 else 1
    else if a0 < b0 then -1 else 1

  let fprintf_value ff {gId=g;mId=m} =
    Format.fprintf ff "{%s%s}"
      (Int64.to_string g)
      (Int64.to_string m) 
end

module Preorder_Nodeid_Pair = struct
  type t = (Preorder_Type.t * Int_Type.t)
  type encoded_type = char array
  type record       = t


  let encoded_size    = Preorder_Type.encoded_size + Inplace_Int_Type.encoded_size
  let get_record_size () = encoded_size
  let is_fixed_length () = true
  let preorder_offset o  = (o + 0)
  let node_offset o      = (o + Preorder_Type.encoded_size)

  let inplace_encode a o (p,n) = 
    Preorder_Type.inplace_encode a (preorder_offset o) p;
    Inplace_Int_Type.inplace_encode a (node_offset o)  n

  let inplace_decode a o = 
    let p = Preorder_Type.inplace_decode a (preorder_offset o) in
    let n = Inplace_Int_Type.inplace_decode a (node_offset o) in
      (p,n)

  let encode v = 
    let a = Array.make encoded_size '0' in 
      inplace_encode a 0 v;
      a

  let decode a = 
    inplace_decode a 0 

  let compare (a0,a1) (b0,b1) = 
    let comp_0 = Preorder_Type.compare a0 b0 in
    let comp_1 = Int_Type.compare a1 b1      in
      if comp_0 = 0 
      then 
	if comp_1 = 0 then 0
	else comp_1
      else comp_0
	
  let fprintf_value ff (p,i) = 
    Format.fprintf ff "(%a,%a)" 
      Preorder_Type.fprintf_value p
      Int_Type.fprintf_value i
end
