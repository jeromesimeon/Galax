(* Note:

     This file is taken 'as-is' from the pxp distribution, but seems
     to have been originally written by Claudio Sacerdoti Coen. There
     is no license recorded for that file.

     PXP can be found at:
     http://www.ocaml-programming.de/programming/pxp.html

   - Jerome

*)

(******************************************************)
(*    Claudio Sacerdoti Coen <sacerdot@cs.unibo.it>   *)
(*                   14/05/2000                       *)
(******************************************************)

(* [14-Jun-2001] Slightly modified by Gerd Stolpmann *)

(* Surrogate Pairs are not accepted in XML files (is it true???) *)
exception SurrogatePairs;;

(* Interval (n,m) where n >m m *)
exception InvalidInterval of int * int;;

(* Given an ucs2 character code, returns it in utf8 *)
(* (as a concatenation of characters)               *)
let char_ucs2_to_utf8 =
 function
    n when n >= 0xD800 && n <= 0xDFFF -> raise SurrogatePairs
  | n when n <= 0x007F -> Types.Char n
  | n when n <= 0x07FF ->
     Types.Concat
      [[Types.Char (n lsr  6 land 0b00011111 lor 0b11000000)] ;
       [Types.Char (n        land 0b00111111 lor 0b10000000)]]
  | n ->
     Types.Concat
      [[Types.Char (n lsr 12 land 0b00001111 lor 0b11100000)] ;
       [Types.Char (n lsr  6 land 0b00111111 lor 0b10000000)] ;
       [Types.Char (n        land 0b00111111 lor 0b10000000)]]
;;

(*CSC: Two functions for debugging pourposes only

let char_ucs2_to_utf8 =
 function
    n when n >= 0xD800 && n <= 0xDFFF -> assert false
  | n when n <= 0x007F -> [[n]]
  | n when n <= 0x07FF ->
     [[(n lsr  6 land 0b00011111 lor 0b11000000)] ;
      [(n        land 0b00111111 lor 0b10000000)]]
  | n ->
     [[(n lsr 12 land 0b00001111 lor 0b11100000)] ;
      [(n lsr  6 land 0b00111111 lor 0b10000000)] ;
      [(n        land 0b00111111 lor 0b10000000)]]
;;

let rec bprint =
 function
    0 -> ""
  | n -> bprint (n / 2) ^ string_of_int (n mod 2)
;;
*)

(* A few useful functions *)
let rec mklist e =
 function
    0 -> []
  | n -> e::(mklist e (n - 1))
;;

let sup =
 let t = Types.Char 0b10111111 in
  function
     1 -> t
   | n -> Types.Concat (mklist [t] n)
;;

let rec inf =
 let b = Types.Char 0b10000000 in
  function
     1 -> [[b]]
   | n -> mklist [b] n
;;

let mysucc =
 function
    [Types.Char n] -> n + 1
  | _ -> assert false
;;

let mypred =
 function
    [Types.Char n] -> n - 1
  | _ -> assert false
;;

(* Given two utf8-encoded extremes of an interval character code      *)
(* whose 'length' is the same, it returns the utf8 regular expression *)
(* matching all the characters in the interval                        *)
let rec same_length_ucs2_to_utf8 =
 let module T = Types in
  function
     (T.Char n, T.Char m) when n = m -> [T.Char n]
   | (T.Char n, T.Char m) -> [T.Interval (n,m)]
   | (T.Concat [hen ; [tln]], T.Concat [hem ; [tlm]]) when hen = hem ->
      [T.Concat [hen ; same_length_ucs2_to_utf8 (tln,tlm)]]
   | (T.Concat [hen ; [tln]], T.Concat ([hem ; [tlm]] as e2)) ->
      (T.Concat [hen ; same_length_ucs2_to_utf8 (tln,sup 1)]) ::
      (let shen = mysucc hen
       and phem = mypred hem in
       let succhen = [T.Char shen] in
        if succhen = hem then
         same_length_ucs2_to_utf8 (T.Concat (succhen::(inf 1)), T.Concat e2)
        else
         (T.Concat [[T.Interval (shen, phem)] ;
          [T.Interval (0b10000000,0b10111111)]])::
           same_length_ucs2_to_utf8 (T.Concat (hem::(inf 1)), T.Concat e2)
      )
    (*same_length_ucs2_to_utf8 (T.Concat ((mysucc hen)::(inf 1)), T.Concat e2)*)
   | (T.Concat (hen::tln), T.Concat (hem::tlm)) when hen = hem ->
      [T.Concat [hen ; same_length_ucs2_to_utf8 (T.Concat tln, T.Concat tlm)]]
   | (T.Concat (hen::tln), T.Concat ((hem::tlm) as e2)) ->
      let n = List.length tln in
       (T.Concat
        [hen ; same_length_ucs2_to_utf8 (T.Concat tln,sup n)]) ::
         (let shen = mysucc hen
          and phem = mypred hem in
          let succhen = [T.Char shen] in
           if succhen = hem then
            same_length_ucs2_to_utf8 (T.Concat (succhen::(inf n)), T.Concat e2)
           else
            (T.Concat [[T.Interval (shen, phem)] ;
             [T.Interval (0b10000000,0b10111111)] ;
             [T.Interval (0b10000000,0b10111111)]]
            )::
             same_length_ucs2_to_utf8 (T.Concat (hem::(inf n)), T.Concat e2)
       )
     (*same_length_ucs2_to_utf8 (T.Concat ((mysucc hen)::(inf n)),T.Concat e2)*)
   | _ -> assert false
;;

(* Given an interval of ucs2 characters, splits *)
(* the list in subintervals whose extremes has  *)
(* the same utf8 encoding length and, for each  *)
(* extreme, calls same_length_ucs2_to_utf8      *)
let rec seq_ucs2_to_utf8 =
 function
    (n,_) when n >= 0xD800 && n <= 0xDFFF -> raise SurrogatePairs
  | (_,n) when n >= 0xD800 && n <= 0xDFFF -> raise SurrogatePairs
  | (n,m) when n > m -> raise (InvalidInterval (n,m))
  | (n,m) when n = m -> [char_ucs2_to_utf8 n]
  | (n,m) when n <= 0x07F && m > 0x07F ->
      (seq_ucs2_to_utf8 (n,0x07F)) @ (seq_ucs2_to_utf8 (0x080,m))
  | (n,m) when n <= 0x07FF && m > 0x07FF ->
      (seq_ucs2_to_utf8 (n,0x07FF)) @ (seq_ucs2_to_utf8 (0x0800,m))
  | (n,m) ->
      let utf8n = char_ucs2_to_utf8 n
      and utf8m = char_ucs2_to_utf8 m in
       same_length_ucs2_to_utf8 (utf8n,utf8m)
;;

(* Given an ucs2 regual expression, returns  *)
(* the corresponding utf8 regular expression *)
let ucs2_to_utf8 { Types.id = id ; Types.rel = rel } =
 let rec aux re l2 =
  match re with
     Types.Char i -> char_ucs2_to_utf8 i :: l2
   | Types.Interval (l,u) -> seq_ucs2_to_utf8 (l,u) @ l2
   | Types.Identifier _ as i -> i :: l2
   | Types.Concat rell ->
      let foo rel = List.fold_right aux rel [] in
       Types.Concat (List.map foo rell) :: l2
 in
  { Types.id = id ; Types.rel = List.fold_right aux rel [] }
;;

(* The function actually used to produce the output *)
let output = ref (fun _ -> ());;      (* modified by GS *)

(* padded_string_of_int i returns the string representing the        *)
(* integer i (i < 256) using exactly 3 digits (example: 13 -> "013") *)
let padded_string_of_int i =
 if i < 10 then
  "00" ^ string_of_int i
 else if i < 100 then
  "0" ^ string_of_int i
 else
  string_of_int i
;;

(* Two functions useful to print a definition *)
let rec print_disjunction ?(first = true) =
 function
    [] -> ()
  | he::tl ->
     if not first then !output " | " ;
     print_re he ;
     print_disjunction ~first:false tl
and print_re =
 function
    Types.Char i -> !output ("'\\" ^ padded_string_of_int i ^ "'")
  | Types.Interval (l,u) ->
     !output ("['\\" ^ padded_string_of_int l ^ "'-'\\" ^
      padded_string_of_int u ^ "']")
  | Types.Identifier i -> !output i
  | Types.Concat rell ->
     let foo rel =
      if List.length rel > 1 then
       (!output "(" ; print_disjunction rel ; !output ")")
      else
       print_disjunction rel
     in
      List.iter foo rell
;;

(* print_definition prints a definition in the format expected by ocamllex *)
let print_definition { Types.id = id ; Types.rel = rel } =
 !output ("let " ^ id ^ " =\n   ") ;
 print_disjunction rel ;
 !output "\n\n"
;;

(* main *)
let _ =
  (* modified by Gerd Stolpmann *)
  let in_filename = ref "" in
  let out_filename = ref "" in
  Arg.parse
      []
      (fun s -> 
	 if !in_filename = "" then
	   in_filename := s
	 else
	   if !out_filename = "" then
	     out_filename := s
	   else raise (Arg.Bad "Too many arguments")
      )
      "usage: ucs2_to_utf8 infile outfile";
   if !in_filename = "" || !out_filename = "" then (
     prerr_endline "Too few arguments";
     exit 1
   );
   try
     let in_file = open_in !in_filename in
     let out_file = open_out !out_filename in
     let lexbuf = Lexing.from_channel in_file in
     let ucs2_result = Parser.main Lexer.token lexbuf in
     output := output_string out_file;
     List.iter print_definition (List.map ucs2_to_utf8 ucs2_result);
     close_in in_file;
     close_out out_file
   with
       any ->
	 (try Sys.remove !out_filename with _ -> ());
	 prerr_endline (Printexc.to_string any);
	 exit 1
;;
