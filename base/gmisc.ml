(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: gmisc.ml,v 1.47 2007/02/28 18:48:06 mff Exp $ *)

(* Module: Gmisc
   Description:
     This module implements some additions to the Caml standard
     library that appeared to be useful in the process of developing
     Galax.
*)

(* Note:
     All these functions raise standard Caml exceptions, rather than
     Galax exceptions in the rest of the system.
   - Jerome
 *)


(*****************)
(* I/O functions *)
(*****************)

(* Load the content of a file into a string *)

  (* Grab a file and stuff it into a string *)
  let string_of_file file =
    try
      let inchan = open_in_bin file in
      let len = in_channel_length inchan in
      let buf = String.create len in
      really_input inchan buf 0 len;
      close_in inchan;
      (* Windows 95 terminates lines with carriage return as well as
         newline; this screws up the Unix version when it reads in
         files created under Windows.  So, we get rid of the carriage
         returns, not the ideal solution. *)
      if Sys.os_type = "Win32" then buf
      else Str.global_replace (Str.regexp "\r\n") "\n" buf
    with
      Sys_error err ->
        Printf.eprintf
          "galaxd: could not read the file %s, got error Sys_error %s\n@?"
          file
          err;
        raise(Sys_error err)

let load_file_in_buffer nb f =
  let ic = open_in f in
  while
    Netbuffer.add_inplace nb (input ic) > 0 do ()
  done;
  close_in ic

let load_string_in_buffer nb s =
  Netbuffer.add_string nb s

let load_file f =
  let nb = Netbuffer.create 100 in
  begin
    load_file_in_buffer nb f;
    Netbuffer.contents nb
  end

  let get_files_in_directory dir =
    let dirh = Unix.opendir dir in
    let rec get_them l =
      match
        try Some(Unix.readdir dirh) with _ -> None
      with
      | None ->
          Unix.closedir dirh; l
      | Some x ->
          get_them (x::l)
    in
    Sort.list (<=) (get_them [])

  (* Convert a shell-style regular expression, using the special characters,
     ?*[], to a caml-style regular expression. *)

  let convert_regexp s =
    let s = Str.global_replace (Str.regexp "\\+") "\\+" s in
    let s = Str.global_replace (Str.regexp "\\^") "\\^" s in
    let s = Str.global_replace (Str.regexp "\\$") "\\$" s in
    let s = Str.global_replace (Str.regexp "\\.") "\\." s in
    let s = Str.global_replace (Str.regexp "\\?") "." s in
    let s = Str.global_replace (Str.regexp "\\*") ".*" s in
    s ^ "$"

  let ls dir pattern =
    let files = get_files_in_directory dir in
    let re = Str.regexp (convert_regexp pattern) in
    let rec filter l =
      match l with
        [] -> []
      | hd::tl ->
          if Str.string_match re hd 0 then hd::(filter tl)
          else filter tl
    in filter files

(******************)
(* List functions *)
(******************)

(* an additional function to partition list,
   but only getting the first element that satisfy
   the given predicate *)

let partition_first p l =
  let rec part_first remain =
    function
      |	[] ->
	  raise Not_found
      |	x :: l ->
	  let (t,v) = (p x) in
	  if t then (v,(List.rev remain)@ l) else part_first (x :: remain) l in
  part_first [] l

let partition_index f l =
  let index = ref 0 in
  let rec part_aux f l =
    match l with
    | [] -> ([],[])
    | e :: r ->
	if (f !index)
	then
	  begin
	    incr index;
	    let (m1,m2) = part_aux f r in
	    (e :: m1,m2)
	  end
	else
	  begin
	    incr index;
	    let (m1,m2) = part_aux f r in
	    (m1,e :: m2)
	  end
  in
  part_aux f l

let rec partition_pairs l =
  match l with
  | [] -> []
  | x :: [] -> raise (Invalid_argument "[Gmisc.partition_pairs]: not a pair")
  | x1 :: x2 :: r ->
      (x1,x2) :: (partition_pairs r)

(* Filter non existing elements from a list *)

let filter_non_exists g1 g2 =
  let partfun y = not(List.exists (fun x -> x = y) g2) in
  List.filter partfun g1

(* Map concat *)

let map_concat f l =
  List.concat (List.map f l)

(* N first items *)

let rec list_npeek n list =
  if n <= 0
  then []
  else
    match list with
    | [] -> []
    | x :: rest ->
	x :: (list_npeek (n-1) rest)

(* Triple split *)

let rec triple_split l =
  match l with
  | [] -> ([],[],[])
  | (a,b,c)::l' ->
      let (l1,l2,l3)=triple_split l' in
      (a::l1,b::l2,c::l3)

(* Remove duplicates *)

let rec remove_duplicates ps =
  match ps with 
  | [] -> []
  | a::ps' ->
      if List.mem a ps'
      then
	remove_duplicates ps'
      else
	a::remove_duplicates ps'

let sort_and_remove_duplicates_revcompare rev_compare d = 
  if d = [] then []
  else (* has at least one element, so List.hd call is safe *)
    begin
      (* Sort them in opposite order so we can use tail recursive
	 fold_left without reverse or [] *)
      let sorted_list = List.fast_sort rev_compare d in
      let head = List.hd sorted_list in 
      (* now remove duplicates *)
      (* fst because last_seen is already in the list *)
      fst (List.fold_left (fun (cur_list, last_seen) new_value ->
	if ((rev_compare new_value last_seen) = 0) then
	  (cur_list, last_seen)
	else
	  (new_value :: cur_list, new_value)) ([head], (head)) (List.tl sorted_list))
    end

let rec unwrap_option_list l =
  match l with 
  | [] -> []
  | None::l' -> unwrap_option_list l'
  | (Some v)::l' -> v :: (unwrap_option_list l')

let some_list l = 
  let len = List.length l in
  let unwrap_l = (unwrap_option_list l) in
  if (List.length unwrap_l) = len then unwrap_l
  else
    raise Not_found

let is_some opt = 
  match opt with
  | None -> false
  | Some _ -> true

(* X subset Y *)
let is_subset x_set y_set =
  List.fold_left (fun is_subset x ->
		    is_subset && (List.mem x y_set)) true x_set

(* X intersect Y *)
let intersect_list l1 l2 = 
  List.filter (fun x -> List.mem x l1) l2

(* X - Y *)
let difference_list l1 l2 =
  List.filter (fun x -> not (List.mem x l2)) l1


(*********************)
(* Hashtbl functions *)
(*********************)

(* Creates and load a hash table *)

let create_hashtable size init =
  let tbl = Hashtbl.create size in
  List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
  tbl

let all_of_hashtable ht =
  let f a b c = (a, b) :: c in 
  Hashtbl.fold f ht []

let keys_of_hashtable ht =
  let f a b c = (a) :: c in 
  Hashtbl.fold f ht []

let cond_add nh x y =
  if Hashtbl.mem nh x
  then ()
  else Hashtbl.add nh x y

let merge_hashtable h1 h2 =
  Hashtbl.iter (cond_add h1) h2;
  h1


(********************)
(* String functions *)
(********************)

let split_string_on_char s c = 
  let mid = (String.index s c) in
  let sta1 = 0 in
  let len1 = mid in
  let sta2 = mid + 1 in
  let len2 = (String.length s) - sta2 in
  (String.sub s sta1 len1, String.sub s sta2 len2)

let split_right_on_char s c =
  try 
    split_string_on_char s c 
  with
    Not_found -> ("", s)

let split_left_on_char s c =
  try 
    split_string_on_char s c 
  with
    Not_found -> (s, "")

let rec split_on_char s c =
  try
    let (s1, s2) = split_string_on_char s c
    in s1 :: split_on_char s2 c
  with
    Not_found ->
      s :: []

let remove_leading s c =
  let l = String.length s in
  let rest_first =
    let rec loop s i =
      if i >= l then l-1 else
      if s.[i] = c then loop s (i+1)
      else i
    in
    loop s 0
  in
  String.sub s rest_first (l-rest_first)
    
let remove_trailing s c =
  if (String.length s > 0) then
    let rest_last =
      let rec loop s i =
	if i = 0 then 1 else
	if s.[i] = c then loop s (i-1)
	else i+1
      in
      loop s ((String.length s) - 1)
    in
    String.sub s 0 rest_last
  else s

let quote1    = Str.regexp "\""
let quote2    = Str.regexp "'"

let quote_quotes str =
  let str = Str.global_replace quote1 "&quot;" str in
  let str = Str.global_replace quote2 "&apos;" str in
  str

(*********************)
(* Parsing functions *)
(*********************)

let wrap_lexer f s =
  let lexbuf = Lexing.from_string s in
  f lexbuf


(**********************)
(* Printing functions *)
(**********************)

(* Print to stdout *)

let printf_stub s f x =
  Format.printf "%s%a@?" s f x

(* Print to stderr *)

let eprintf_stub s f x =
  Format.fprintf (!Conf.glx_err_formatter) "%s%a@?" s f x

(* Print to output channel *)

let fprintf_stub c s f x =
  Format.fprintf c "%s%a@?" s f x

(* Print to a string buffer *)

let bprintf_stub s f x =
  let buff = Buffer.create 50 in
  Format.bprintf buff "%s%a@?" s f x;
  let result = Buffer.contents buff in
  Buffer.reset buff;
  result


(**********************)
(* Filename functions *)
(**********************)

(* Rename a DOS dir to a UNIX dir *)

let rename_dir wd =
  Str.global_replace (Str.regexp (Str.quote "\\")) "/" wd

(******************)
(* Hash Functions *)
(******************)

let prime_seed = 23 
let string_hash str =
  let hash_code = ref 393 in
  let cur_prime = ref prime_seed in
    
    for i = 0 to (String.length str) - 1 do
      cur_prime := !cur_prime * prime_seed;
      hash_code := !hash_code + ((!cur_prime) * (Char.code str.[i]))
    done;
    !hash_code

(*********************)
(* Integer functions *)
(*********************)

(* Some missing conversions *)

(* Some auxiliary conversion operations between Caml numeric types *)

let max_int32 = Int32.max_int
let min_int32 = Int32.min_int
let max_int31 = Int32.of_int max_int
let min_int31 = Int32.of_int min_int
let bmax_int31 = Big_int.big_int_of_int max_int
let bmin_int31 = Big_int.big_int_of_int min_int
let bmax_int32 = Big_int.big_int_of_int max_int
let bmin_int32 = Big_int.big_int_of_int min_int

let big_int_of_int32 i =
  let div = Big_int.big_int_of_int (Int32.to_int (Int32.div i max_int31)) in
  let rem = Big_int.big_int_of_int (Int32.to_int (Int32.rem i max_int31)) in
  Big_int.add_big_int (Big_int.mult_big_int div bmax_int31) rem

let bmax_int32 = big_int_of_int32 Int32.max_int
let bmin_int32 = big_int_of_int32 Int32.min_int

let int32_of_big_int bi =
  if (Big_int.lt_big_int bmax_int32 bi) || (Big_int.lt_big_int bi bmin_int32)
  then
    raise (Failure "Big_int out of bound for int32 conversion")
  else
    let (bdiv,brem) = Big_int.quomod_big_int bi bmax_int31 in
    let div = Int32.of_int (Big_int.int_of_big_int bdiv) in
    let rem = Int32.of_int (Big_int.int_of_big_int brem) in
    Int32.add (Int32.mul div max_int31) rem

let dmax_int64 = Int64.max_int
let dmin_int64 = Int64.min_int
let dmax_int32 = Int64.of_int32 Int32.max_int
let dmin_int32 = Int64.of_int32 Int32.min_int

let big_int_of_int64 i =
  let div1 = Int64.div i dmax_int32 in
  let rem1 = big_int_of_int32 (Int64.to_int32 (Int64.rem i dmax_int32)) in
  let div2 = big_int_of_int32 (Int64.to_int32 (Int64.div div1 dmax_int32)) in
  let rem2 = big_int_of_int32 (Int64.to_int32 (Int64.rem div1 dmax_int32)) in
  let result1 = Big_int.add_big_int (Big_int.mult_big_int div2 bmax_int32) rem2 in
  Big_int.add_big_int (Big_int.mult_big_int result1 bmax_int32) rem1

let bmax_int64 = big_int_of_int64 Int64.max_int
let bmin_int64 = big_int_of_int64 Int64.min_int

let int64_of_big_int bi =
  if (Big_int.lt_big_int bmax_int64 bi) || (Big_int.lt_big_int bi bmin_int64)
  then
    raise (Failure "Big_int out of bound for int64 conversion")
  else
    let (bdiv1,brem1) = Big_int.quomod_big_int bi bmax_int32 in
    let (bdiv2,brem2) = Big_int.quomod_big_int bdiv1 bmax_int32 in
    let div2 = Int64.of_int32 (int32_of_big_int bdiv2) in
    let rem2 = Int64.of_int32 (int32_of_big_int brem2) in
    let result1 = Int64.add (Int64.mul div2 dmax_int32) rem2 in
    let rem1 = Int64.of_int32 (int32_of_big_int brem1) in
    Int64.add (Int64.mul result1 dmax_int32) rem1

let compose f g = function x -> f(g(x))

let int_of_hex_char c =
  match c with
  | '0' -> 0
  | '1' -> 1
  | '2' -> 2
  | '3' -> 3
  | '4' -> 4
  | '5' -> 5
  | '6' -> 6
  | '7' -> 7
  | '8' -> 8
  | '9' -> 9
  | 'a' | 'A' -> 10
  | 'b' | 'B' -> 11
  | 'c' | 'C' -> 12
  | 'd' | 'D' -> 13
  | 'e' | 'E' -> 14
  | 'f' | 'F' -> 15
  | _ -> raise (Invalid_argument "Invalid hex character")

let hex_char_of_int i =
  match i with
  | 0 -> '0'
  | 1 -> '1'
  | 2 -> '2'
  | 3 -> '3'
  | 4 -> '4'
  | 5 -> '5'
  | 6 -> '6'
  | 7 -> '7'
  | 8 -> '8'
  | 9 -> '9'
  | 10 -> 'A'
  | 11 -> 'B'
  | 12 -> 'C'
  | 13 -> 'D'
  | 14 -> 'E'
  | 15 -> 'F'
  | _ -> raise (Invalid_argument "Invalid integer range in hex")

let hex_char_pair_of_int i =
  let i1 = i / 16 in
  let i2 = i mod 16 in
  (hex_char_of_int i1, hex_char_of_int i2)

let int_of_hex_char_pair c1 c2 =
  let i1 = int_of_hex_char c1 in
  let i2 = int_of_hex_char c2 in
  16 * i1 + i2

let char_of_hex_char_pair c1 c2 =
  Char.chr (int_of_hex_char_pair c1 c2)

(* XML Schema Datatypes 3.2.15 hexBinary

   [Definition:] hexBinary represents arbitrary hex-encoded binary
   data. The ·value space· of hexBinary is the set of finite-length
   sequences of binary octets.  

   3.2.15.1 Lexical Representation

   hexBinary has a lexical representation where each binary octet is
   encoded as a character tuple, consisting of **two(2)** hexadecimal
   digits ([0-9a-fA-F]) representing the octet code. For example,
   "0FB7" is a hex encoding for the 16-bit integer 4023 (whose binary
   representation is 111110110111).  
*)

let binary_of_hexString s =
  if s = "" then "" else
  begin
    let l = String.length s in
    let dv = l / 2 in
    let md = l mod 2 in
    let newl = 
      if md = 0 then dv
      else raise (Invalid_argument("Text: \"" ^ s ^ "\" not a hexBinary value (hex digit pairs)"))
   in
    let news = String.make newl 'x' in
    let (current,c1,c2) =
      if md = 0
      then
	(ref 2, ref (String.get s 0), ref (String.get s 1))
      else
	(ref 1, ref '0', ref (String.get s 0))
    in
    String.set news ((!current-1) / 2) (char_of_hex_char_pair !c1 !c2);
    while (!current < l - 1) do
      c1 := (String.get s !current);
      c2 := (String.get s (!current+1));
      current := !current + 2;
      String.set news ((!current-1) / 2) (char_of_hex_char_pair !c1 !c2)
    done;
    news
  end

let string_of_hexBinary s =
  if s = "" then "" else
  begin
    let l = String.length s in
    let newl = l * 2 in
    let news = String.make newl 'x' in
    let current = ref 0 in
    while (!current < newl) do
      let i = Char.code (String.get s (!current / 2)) in
      let (c1,c2) = hex_char_pair_of_int i in
      String.set news !current c1;
      String.set news (!current+1) c2;
      current := !current+2
    done;
    news
  end

(*
type ct =
  | OpenComment
  | CloseComment
  | Constructor

let ct_of_chars c1 c2 =
  match (c1,c2) with
  | '(',':' -> OpenComment
  | ':',')' -> CloseComment
  | '<',_ ->
      if 

let comment_blit start s =
  let c1 = ref String.get s 0 in
  let c2 = ref String.get s 1 in
  let current = ref start in
  let stop = ref false 
  while not(stop) do
    match 
  done

*)
