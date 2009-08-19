(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: occurrence.ml,v 1.7 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Occurrence
   Description:
     Manipulations of occurrences, i.e., an integer or
     'unbounded'. Used for XML Schema minOccurs, maxOccurs, and XQuery
     occurrence indicators.
*)

(* The type 'occurs' corresponds to minOccurs and maxOccurs *)

type occurs =
  | UP_INT of int
  | UNBOUNDED

(* An occurrence indicator is a pair of minOccurs, maxOccurs *)

type occurrence_indicator =
    (occurs * occurs)

(* operations on occurs *)

let occurs i = UP_INT i

let unbounded   = UNBOUNDED
let occurs_zero = UP_INT 0
let occurs_one  = UP_INT 1

let unbounded_operation op b1 b2 =
  match (b1,b2) with
  | (UNBOUNDED,_) -> UNBOUNDED
  | (_,UNBOUNDED) -> UNBOUNDED
  | (UP_INT(i1),UP_INT(i2)) -> UP_INT(op i1 i2)

let ub_lt b1 b2 =
  match (b1,b2) with
  | (UNBOUNDED,UNBOUNDED) -> false
  | (_,UNBOUNDED) -> true
  | (UNBOUNDED,_) -> false
  | (UP_INT i1, UP_INT i2) -> i1 < i2

let ub_gt b1 b2 =
  match (b1,b2) with
  | (UNBOUNDED,UNBOUNDED) -> false
  | (UNBOUNDED,_) -> true
  | (_,UNBOUNDED) -> false
  | (UP_INT i1, UP_INT i2) -> i1 > i2

let ub_max ma1 ma2 = unbounded_operation max ma1 ma2
let ub_min ma1 ma2 =
  match (ma1,ma2) with
  | (UNBOUNDED,UP_INT(i)) -> UP_INT(i)
  | (UP_INT(i),UNBOUNDED) -> UP_INT(i)
  | (UP_INT(i1),UP_INT(i2)) -> UP_INT(min i1 i2)
  | _ -> UNBOUNDED

let ub_add ma1 ma2 = unbounded_operation (+) ma1 ma2
let ub_mult ma1 ma2 =
  match (ma1,ma2) with
  | (_,UP_INT 0) -> UP_INT 0
  | (UP_INT 0,_) -> UP_INT 0
  | (UNBOUNDED,_) -> UNBOUNDED
  | (_,UNBOUNDED) -> UNBOUNDED
  | (UP_INT(i1),UP_INT(i2)) -> UP_INT(i1*i2)

let minus i =
  function
    | UNBOUNDED -> UNBOUNDED
    | (UP_INT i') ->
	let i'' = i'-i in
	if i'' < 0 then (UP_INT 0) else (UP_INT i'')

let b_equal mi ma =
  match (mi,ma) with
  | (UNBOUNDED,UNBOUNDED) -> true
  | (_,UNBOUNDED) -> false
  | (UNBOUNDED,_) -> false
  | (UP_INT i1,UP_INT i2) -> i1=i2

let equal mi ma =
  match (mi,ma) with
  | (UNBOUNDED,UNBOUNDED) -> true
  | (_,UNBOUNDED) -> false
  | (UNBOUNDED,_) -> false
  | (UP_INT i1,UP_INT i2) -> i1=i2

let le mi ma =
  match (mi,ma) with
  | (_,UNBOUNDED) -> true
  | (UNBOUNDED,_) -> false
  | (UP_INT i1,UP_INT i2) -> i1 <= i2

(* Prints bounds *)

let string_of_occurs ub =
  match ub with
  | UP_INT i -> (string_of_int i)
  | UNBOUNDED -> "unbounded"


(* Approximate occurrence indicators as used in XQuery *)

let one       = ((UP_INT 1), (UP_INT 1))  (* Exactly one *)
let optional  = ((UP_INT 0), (UP_INT 1))  (* Zero or one '?' *)
let star      = ((UP_INT 0), UNBOUNDED)   (* Zero or more '*' *)
let plus      = ((UP_INT 1), UNBOUNDED)   (* One or more '+' *)

let is_one      oi = oi = one
let is_optional oi = oi = optional
let is_star     oi = oi = star
let is_plus     oi = oi = plus

(* Computes an approximate occurrence indicator *)

(*
 · |	 1 	 ? 	 + 	 * 
 ---------------------------------
 1 |	 1 	 ? 	 + 	 * 
 ? |	 ? 	 ? 	 * 	 * 
 + |	 + 	 * 	 + 	 * 
 * |	 * 	 * 	 * 	 * 
*)
let mult mi ma =
  match (mi,ma) with
  | (UNBOUNDED,_) -> UNBOUNDED
  | (_,UNBOUNDED) -> UNBOUNDED
  | (UP_INT(i1),UP_INT(i2)) -> UP_INT(i1 * i2)

let mult_occurrences (l1,u1) (l2,u2) = 
  (mult l1 l2, mult u1 u2)
  
(*
 , |	 1 	 ? 	 + 	 * 
 ---------------------------------
 1 |	 + 	 + 	 + 	 + 
 ? |	 + 	 * 	 + 	 * 
 + |	 + 	 + 	 + 	 + 
 * |	 + 	 * 	 + 	 * 
*)
let seq_occurrences (l1,u1) (l2,u2) = 
  (ub_max l1 l2, UNBOUNDED)
  
(*	    	
 | |	 1 	 ? 	 + 	 * 
 ---------------------------------
 1 |	 1 	 ? 	 + 	 * 
 ? |	 ? 	 ? 	 * 	 * 
 + |	 + 	 * 	 + 	 * 
 * |	 * 	 * 	 * 	 * 
*)
	    	
let approximate_occurrences (b1,b2) =
  let b1' = ub_min b1 (UP_INT 1)
  and b2' =
    if le b2 (UP_INT 1)
    then
      b2
    else
      unbounded
  in
  (b1',b2')

