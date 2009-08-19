(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* Module: Shredded_encoder
   Description:
     This module implements the string encoding function for integers  
*)

(* Turns a Caml integer into a shredded_jungle string to be used for storage *)
let char_pad = char_of_int 0 
let needed_length_for_int = 4
let serialized_int_size   = 4

(* let shredded_jungle_string_of_int i =
  let s = Array.create 4 (Char.chr 0) in
    shredded_jungle_string_of_int_array s i *)
let inplace_encode_int s offset i =
  let c3 = i land 255 in 
  let c2 = (i lsr 8) land 255 in 
  let c1 = (i lsr 16) land 255 in 
  let c0 = (i lsr 24) land 255 in 
    s.(0+offset) <- Char.chr c0;
    s.(1+offset) <- Char.chr c1;
    s.(2+offset) <- Char.chr c2;
    s.(3+offset) <- Char.chr c3
    

let inplace_decode_int s offset  = 
  if Array.length s < offset + 4 then
    raise (Failure ("string should have 4 characters " ^ (string_of_int (Array.length s)) ^ " " ^ (string_of_int offset)))
  else    
    ((Char.code s.(offset+0)) lsl 24) lor
    ((Char.code s.(offset+1)) lsl 16) lor 
    ((Char.code s.(offset+2)) lsl 8) lor
    (Char.code s.(offset+3))


let copy_encode int = 
  let a = Array.make 4 '0' in 
    inplace_encode_int a 0 int;
    a
    

(* Functions to convert, there is no loss because caml strings are
   equivalent.  char arrays -> strings and back
*)
let length_bits_of_string str = String.length str 

let string_of_bits p =
  let l = String.make (Array.length p) char_pad in
    for i = 0 to (Array.length p) - 1 do
      l.[i] <- p.(i)
    done;
    l

let bits_of_string l =
  let p = Array.make (String.length l) char_pad in 
    for i = 0 to (String.length l) - 1 do
      p.(i) <- l.[i]
    done;
    p


let inplace_decode_string p offset length =
  let l = String.make length char_pad in
    for i = 0 to length - 1 do
      l.[i] <- p.(i+offset)
    done;
    l

let inplace_encode_string p offset l =
  for i = 0 to (String.length l) - 1 do
    p.(i+offset) <- l.[i]
  done



(**********************)
(* Encode Native Ints *)
(**********************)
let char_mask       = 255

let inplace_encode_int32 a offset v =
  let convert x = Char.chr ((Int32.to_int x) land char_mask) in
    a.(offset)   <- convert (Int32.shift_right_logical v 24);
    a.(offset+1) <- convert (Int32.shift_right_logical v 16);
    a.(offset+2) <- convert (Int32.shift_right_logical v 8);
    a.(offset+3) <- convert v 
	
let inplace_decode_int32 a offset = 
  let convert x = Int32.of_int (Char.code x) in 
  let c0 = Int32.shift_left (convert a.(offset)) 24 in 
  let c1 = Int32.shift_left (convert a.(offset+1)) 16 in 
  let c2 = Int32.shift_left (convert a.(offset+2)) 8 in
  let c3 = convert a.(offset+3)  in
    Int32.logor 
      (Int32.logor c0 c1)
      (Int32.logor c2 c3)		
      
  
(*****************)
(* Encode Int64s *)
(*****************)
let powers = [| 0 ; 8 ; 16;  24;  32; 40; 48; 56 |]
let inplace_encode_int64 a offset v = 
  let convert x = Char.chr ((Int64.to_int x) land char_mask) in
    for i = 0 to 7 do
      a.(offset+i) <- convert (Int64.shift_right_logical v powers.(7-i))
    done

(* Marshalling Hack to remove copies, one Int64 native copy *)
let marshal_hack        = Marshal.to_string Int64.zero [] ;;
let mARSHAL_BODY_OFFSET = 24

let inplace_decode_int64 a offset = 
  (* Should be factored *)
  (* let convert x = Int64.of_int (Char.code x) in *)
  try 
    for i = 0 to 7 do
      marshal_hack.[i+mARSHAL_BODY_OFFSET] <- a.(offset+i)
    done;
    Marshal.from_string marshal_hack 0
  with Invalid_argument _ ->
    raise (Failure ("FAILURE IN MARSHALLING CODE [Int64]"))
(* Unrolled the loop by hand *)
(*  let c0 = Int64.shift_left (convert a.(offset))   56 in 
    let c1 = Int64.shift_left (convert a.(offset+1)) 48 in 
    let c2 = Int64.shift_left (convert a.(offset+2)) 40 in
    let c3 = Int64.shift_left (convert a.(offset+3)) 32 in
    let c4 = Int64.shift_left (convert a.(offset+4)) 24 in 
    let c5 = Int64.shift_left (convert a.(offset+5)) 16 in 
  let c6 = Int64.shift_left (convert a.(offset+6)) 8 in
  let c7 = convert a.(offset+7)  in
    Int64.logor
      (Int64.logor
	 (Int64.logor c0 c1)
	 (Int64.logor c2 c3))
      (Int64.logor
	 (Int64.logor c4 c5)
	 (Int64.logor c6 c7)) *)

  (* let i64 = ref (Int64.of_int 0) in
    for i = 0 to 7 do
      i64 := 
      Int64.logor (Int64.shift_left (convert a.(offset+i)) powers.(7-i))
	(!i64)
    done;
      !i64 *)
    

let inplace_encode_int64_pair a offset (v0,v1) =
  inplace_encode_int64 a offset v0;
  inplace_encode_int64 a (offset+8) v1


let inplace_decode_int64_pair a offset =
  let v0 = inplace_decode_int64 a offset in 
  let v1 = inplace_decode_int64 a (offset+8) in
    (v0,v1)

