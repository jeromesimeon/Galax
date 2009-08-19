(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: namespace_util.ml,v 1.6 2007/10/16 01:25:34 mff Exp $ *)

(* Module: Namespace_util
   Description:
     This module implements some basic utilities over XML names.
*)

open Error

open Namespace_names


(***********************************)
(* Hashtables over resolved QNames *)
(***********************************)
(* These hash functions are not good
   but better ones that involve 
   concatenation or casting are too slow. *)
module RQName =
  struct
    type t = rqname
    let equal = rqname_equal
    let hash (p,x,y) = 
      (match x with
      | NSUri s ->
	  (Gmisc.string_hash s) +
	    (Gmisc.string_hash y)
      | NSWildcardUri ->
	  Gmisc.string_hash y) 
	+ 
	(match p with
	| NSInterfacePrefix nc -> (Gmisc.string_hash ("I"))
	| NSServerPrefix nc -> (Gmisc.string_hash ("S"^nc))
	| _ -> 0)
  end

module RQNameHashtbl = Hashtbl.Make(RQName)

module RQNameInt =
  struct
    type t = (rqname * int)
    let equal = rqname_int_equal
    let hash ((p,x,y),i) = 
      let h = 
	match x with
	| NSUri s ->
	    (Gmisc.string_hash s) + (Gmisc.string_hash y)
	| NSWildcardUri ->
	    Gmisc.string_hash y 
      in
      let ph = 
	match p with
	| NSInterfacePrefix nc -> (Gmisc.string_hash ("I"))
	| NSServerPrefix nc -> (Gmisc.string_hash ("S"^nc))
	| _ -> 0
      in
      (h * i) + (h * i * i) + ph + h + i
  end

module RQNameIntHashtbl = Hashtbl.Make(RQNameInt)


(******************************************)
(* Association lists over resolved QNames *)
(******************************************)

type 'a rqname_assoc_list = (rqname * 'a) list

let rec remove_rqname_assoc rqname0 assoc_list =
  match assoc_list with
  | [] -> []
  | (rqname, x as pair) :: assoc_list' ->
      if rqname_equal rqname rqname0
      then assoc_list'
      else pair :: remove_rqname_assoc rqname0 assoc_list'

let rec rqname_assoc rqname0 assoc_list =
  match assoc_list with
  | [] -> raise Not_found
  | (rqname,x) :: assoc_list' ->
      if rqname_equal rqname rqname0
      then x
      else rqname_assoc rqname0 assoc_list'

let rec mem_rqname_assoc rqname0 assoc_list =
  match assoc_list with
  | [] -> false
  | (rqname,x) :: assoc_list' ->
      if rqname_equal rqname rqname0
      then true
      else mem_rqname_assoc rqname0 assoc_list'


