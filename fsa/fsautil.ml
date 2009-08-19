(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: fsautil.ml,v 1.3 2007/02/01 22:08:46 simeon Exp $ *)

(* Module Fsautil
   Description:
     This module contains a few function used in the rest of the FSA
     package.
*)

open Error

(* Utilities used in the FSA package *)

let rec list_of_self len elm =
  if len < 0 then raise (Query (Automata "Computing cross product of a negative size set")) 
  else if len = 0 then []
  else (elm :: (list_of_self (len-1) elm))

let product_of_list_single l single =
  let len = List.length l in
  let l2 = list_of_self len single in
  List.combine l l2

(* given two sets, return the set of the cross product of the two sets *)

let cross_product statelist1 statelist2 =
  let statelist' = List.flatten (List.map (product_of_list_single statelist1) statelist2) in
  statelist'

(* Create a new ordered type whose base type is a pair of a given
   original ordered type *)

module MakeOrderedSetPair (Ordered : Set.OrderedType) =
  struct
    type t = (Ordered.t * Ordered.t)
    let compare (e1,e2) (e3,e4) =
      let c1 = Ordered.compare e1 e3 in
      if c1 = 0 then Ordered.compare e2 e4
      else c1
  end

