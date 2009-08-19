(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: print_common.ml,v 1.17 2007/02/01 22:08:45 simeon Exp $ *)

(* Module Print_common
   Description:
     This module implements pretty-printing for some common parts of
     the ASTs.
*)

open Gmisc
open Error

open Occurrence

open Namespace_names
open Namespace_symbols

open Datatypes
open Datatypes_util

open Xquery_common_ast
open Xquery_common_ast_util

open Format


(**************)
(* Occurrence *)
(**************)

let print_occurence ff occ =
  match occ with
  | None ->
      ()
  | Some (l,h) ->
      begin
	match (l,h) with
	| (UP_INT 0, UNBOUNDED) ->
	    fprintf ff "*"
	| (UP_INT 1, UNBOUNDED) ->
	    fprintf ff "+"
	| (UP_INT 0, UP_INT 1) ->
	    fprintf ff "?"
	| _ ->
	    raise (Query (Malformed_Type ("Cannot have complex minOccurs and maxOccurs in a Datatype")))
      end


(************************)
(* Names and namesapces *)
(************************)

(* Unresolved QNames *)

let print_uqname ff qn =
  let simplified_s =
    let s = (string_of_uqname qn) in
    match s with
    | "*:*" -> "*"
    | _ -> s
  in
  fprintf ff "%s" simplified_s

(* Resolved QNames *)

let print_rqname ff qn =
  fprintf ff "%s" (prefixed_string_of_rqname qn)


let print_uri_rqname ff qn =
  fprintf ff "%s" (quoted_uri_string_of_rqname qn)


(* Symbols *)

let print_symbol ff rsy =
  let string_fun = symbol_prefix_string in
  fprintf ff "%s" (string_fun rsy)


(***************************)
(* Print a nillable marker *)
(***************************)

let print_nillable ff = function
  | Nillable -> fprintf ff "@ nillable"
  | NonNillable -> ()


(********************************)
(* Print a mixed content marker *)
(********************************)

let print_mixed ff = function 
  | Mixed -> fprintf ff "mixed@ "
  | NonMixed -> ()


(***************************)
(* Atomic types and values *)
(***************************)

let print_atomic_type ff at =
  fprintf ff "%s" (string_of_atomic_type at)

(* Atomic values *)

(* TO FIX: PRINTING FOR ATOMIC VALUE SHOULD USE XQUERY SYNTAX, not CAML SYNTAX ! - JEROME *)

let print_literal ff l =
  fprintf ff "%s" (delimited_string_of_literal "\"" l)

let string_of_proto_value l =
  match l with
  | ATInteger -> "0"
  | ATDecimal -> "0.0"
  | ATFloat -> "xs:float(\"0\")"
  | ATDouble -> "0.0e0"
  | ATString -> "\"\""
  | ATUntypedAtomic -> "xs:untypedAtomic(\"\")"
  | ATBoolean -> "false"
  | ATQName -> "\"\""
  | ATAnyURI -> "\"http://example.com\""
  | ATDateTime -> "\"2004-05-24T14:42:00\""
  | ATDate -> "\"2004-05-24\""
  | ATTime -> "\"14:42:00\""
  | ATDayTimeDuration -> "\"P1DT2H3M\""
  | ATYearMonthDuration -> "\"P1Y1M\""
  | _ -> raise (Query(Malformed_Type("Dynamic type of value is of a wrong atomic type")))

let print_proto_value ff l =
  fprintf ff "%s" (string_of_proto_value l)


(***************************)
(* Common XPath structures *)
(***************************)

(* Binary operators *)

let string_of_bop bop =
  match bop with
  | BEIntersect ->
      "intersect"
  | BEUnion ->
      "union"
  | BEExcept ->
      "except"
  | BEBar ->
      "|"
  | BEAnd ->
      "and"
  | BEOr ->
      "or"
  | BEPrecedes ->
      "<<"
  | BEFollows ->
      ">>"
  | BEEq ->
      "eq"
  | BEEqual ->
      "="
  | BENEq -> 
      "ne"
  | BENEqual -> 
      "!="
  | BEIs ->
      "is"
  | BELtOp ->
      "lt"
  | BELt ->
      "<"
  | BEGtOp ->
      "gt"
  | BEGt ->
      ">"
  | BELte ->
      "le"
  | BELteq ->
      "<="
  | BEGteq ->
      ">="
  | BEGte ->
      "ge"
  | BEMult ->
      "*"
  | BEPlus ->
      "+"
  | BEMinus ->
      "-"
  | BEDiv ->
      "div"
  | BEIDiv ->
      "idiv"
  | BEMod ->
      "mod"

let print_bop ff bop =
  fprintf ff "%s" (string_of_bop bop)


(* Unary operators *)

let string_of_uop uop =
  match uop with
  | UEPlus ->
      "+"
  | UEMinus ->
      "-"

let print_uop ff uop =
  fprintf ff "%s" (string_of_uop uop)


(* XPath Axis *)

let string_of_axis a =
  match a with
  | Ancestor ->
      "ancestor"
  | Ancestor_or_self ->
      "ancestor-or-self"
  | Attribute ->
      "attribute"
  | Child ->
      "child"
  | Descendant ->
      "descendant"
  | Descendant_or_self ->
      "descendant-or-self"
  | Following_sibling ->
      "following-sibling"
  | Preceding_sibling ->
      "preceding-sibling"
  | Following ->
      "following"
  | Preceding ->
      "preceding"
  | Parent ->
      "parent"
  | Self ->
      "self"

let print_axis ff a =
  fprintf ff "%s" (string_of_axis a)


(****************************)
(* Common XQuery structures *)
(****************************)

(* Validation mode *)

let print_validation_mode ff vmode =
  match vmode with
  | Lax -> fprintf ff "lax"
  | Strict -> fprintf ff "strict"

(* Sort kinds *)

let print_sortkind ff sk =
  match sk with
  | Ascending -> fprintf ff "ascending"
  | Descending -> fprintf ff "descending"

let print_emptysortkind ff esk =
  match esk with
  | EmptyGreatest -> fprintf ff "empty greatest"
  | EmptyLeast -> fprintf ff "empty least"

let print_stablekind ff sk =
  match sk with
  | Stable -> fprintf ff "stable "
  | NonStable -> ()

let print_snap_modifier ff sm = 
  match sm with 
  | Snap_Unordered_Deterministic -> fprintf ff ""
  | Snap_Nondeterministic -> fprintf ff "nondeterministic "
  | Snap_Ordered_Deterministic -> fprintf ff "ordered "

let updating_flag_to_string upd = 
  if upd = Updating then "updating " else ""
