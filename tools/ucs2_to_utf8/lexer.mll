(* Note:

     This file is taken 'as-is' from the pxp distribution, but seems
     to have been originally written by Claudio Sacerdoti Coen. There
     is no license recorded for that file.

     PXP can be found at:
     http://www.ocaml-programming.de/programming/pxp.html

   - Jerome

*)

{
(******************************************************)
(*    Claudio Sacerdoti Coen <sacerdot@cs.unibo.it>   *)
(*                   14/05/2000                       *)
(******************************************************)

open Parser

let comment_depth = ref 0;;

let charint_of_lexeme l =
 String.set l 0 '0' ;
 int_of_string l
;;
}

let digit = ['0'-'9']|['A'-'F']

rule token =
 parse
    [' ' '\t' '\n']                           { token lexbuf }
  | "let"                                     { LET }
  | (['a'-'z']|'_')(['a'-'z']|['A'-'Z']|'_'|['0'-'9']|'\'')*
                                              { IDENT (Lexing.lexeme lexbuf) }
  | '='                                       { EQ }
  | ";;"                                      { END_OF_LET }
  | "|"                                       { PIPE }
  | '['                                       { LBRACKET }
  | ']'                                       { RBRACKET }
  | '-'                                       { RANGE }
  | "(*"                                      { incr comment_depth ;
                                                comment lexbuf
                                              }
  | "#x" digit digit digit digit              { CHAR (charint_of_lexeme (Lexing.lexeme lexbuf)) }
  | eof                                       { EOF }

and comment =
 parse
    "(*" { incr comment_depth ; comment lexbuf }
  | "*)" { decr comment_depth ;
           if !comment_depth = 0 then token lexbuf else comment lexbuf
         }
  | _    { comment lexbuf }
