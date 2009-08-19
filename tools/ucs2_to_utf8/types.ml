(* Note:

     This file is taken 'as-is' from the PXP distribution, but seems
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

type regexp =
   Char of int
 | Interval of int * int      (* lower bound, upper bound *)
 | Identifier of string
 | Concat of regexp list list (* concatenation of disjunctions *)
;;

type definition = { id : string ; rel : regexp list } ;;
