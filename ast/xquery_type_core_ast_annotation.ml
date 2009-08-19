(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xquery_type_core_ast_annotation.ml,v 1.3 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Xquery_type_core_ast_annotation
   Description:
     This module implements some annotations on the type AST.
*)

open Namespace_symbols
open Namespace_symbols_util
open Xquery_common_ast

type atomic_letter_mapping =  (int ref) * int SQNameHashtbl.t
type cename_letter_mapping =  (int ref) * (rtype_symbol option * nillable * int) SQNameHashtbl.t
type caname_letter_mapping =  (int ref) * (rtype_symbol option * int) SQNameHashtbl.t
type piname_letter_mapping =  (int ref) * (string option, int) Hashtbl.t

type letter_mappings =
    atomic_letter_mapping * cename_letter_mapping * caname_letter_mapping * piname_letter_mapping

let create_at_mapping () = (ref 0, SQNameHashtbl.create 7)
let create_ce_mapping () = (ref 0, SQNameHashtbl.create 7)
let create_ca_mapping () = (ref 0, SQNameHashtbl.create 7)
let create_pi_mapping () = (ref 0, Hashtbl.create 7)
let create_letter_mappings () = 
  (create_at_mapping(), create_ce_mapping (), create_ca_mapping (), create_pi_mapping ())

let type_letter_map (tm,_,_,_) = tm
let elem_letter_map (_,em,_,_) = em
let attr_letter_map (_,_,am,_) = am
let pi_letter_map   (_,_,_,pi) = pi

let copy_letter_mappings mappings = 
  let (tref, tm) = type_letter_map mappings in 
  let (eref, em) = elem_letter_map mappings in 
  let (aref, am) = attr_letter_map mappings in 
  let (pref, pm) = pi_letter_map mappings in 
  ((ref !tref, SQNameHashtbl.copy tm),
   (ref !eref, SQNameHashtbl.copy em),
   (ref !aref, SQNameHashtbl.copy am),
   (ref !pref, Hashtbl.copy pm))

