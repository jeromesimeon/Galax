(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: namespace_symbols.mli,v 1.10 2007/08/20 13:12:40 simeon Exp $ *)

(* Module: Namespace_symbols
   Description:
     This module implements symbol tables for QNames.
*)

open Namespace_names
open Namespace_context

type prefix_symbol = int
type uri_symbol    = int
type ncname_symbol = int
type symbol = (prefix_symbol * uri_symbol * ncname_symbol)

val symbol_equal : symbol -> symbol -> bool
val symbol_hash  : symbol -> int
val symbol_prefix_string : symbol -> string

(*********************************)
(* Operations on element symbols *)
(*********************************)

(* Operations on resolved element symbols *)

type relem_symbol = symbol

val relem_symbol : rqname -> relem_symbol

val relem_name              : relem_symbol -> rqname
val relem_uname             : nsenv -> relem_symbol -> uqname
val relem_name_with_binding : nsenv -> relem_symbol -> (uqname * binding option * binding)

val relem_equal : symbol -> symbol -> bool
val relem_subtag : symbol -> symbol -> bool

val relem_prefix        : relem_symbol -> prefix (* prints the original prefix *)
val relem_string        : relem_symbol -> string (* prints the resolved QName *)
val relem_prefix_string : relem_symbol -> string (* prints the original QName *)

(* Special symbols *)

val anyrelem : relem_symbol


(************************************)
(* Operations on attributes symbols *)
(************************************)

(* Operations on resolved attribute symbols *)

type rattr_symbol = symbol

val rattr_symbol : rqname -> rattr_symbol

val rattr_name              : rattr_symbol -> rqname
val rattr_uname             : nsenv -> rattr_symbol -> uqname
val rattr_name_with_binding : nsenv -> rattr_symbol -> (uqname * binding option * binding)

val rattr_equal : symbol -> symbol -> bool
val rattr_subtag : symbol -> symbol -> bool

val rattr_prefix        : rattr_symbol -> prefix (* prints the original prefix *)
val rattr_string        : rattr_symbol -> string (* prints the resolved QName *)
val rattr_prefix_string : rattr_symbol -> string (* prints the original QName *)

(* Special symbols *)

val anyrattr : rattr_symbol


(******************************)
(* Operations on type symbols *)
(******************************)

(* Operations on resolved type symbols *)

type rtype_symbol = symbol

val rtype_name           : rtype_symbol -> rqname

val rtype_symbol 	 : rqname -> rtype_symbol
val rtype_prefix 	 : rtype_symbol -> prefix

val rtype_equal : symbol -> symbol -> bool
val rtype_subtag : symbol -> symbol -> bool

val rtype_string         : rtype_symbol -> string (* prints the resolved QName *)
val rtype_prefix_string  : rtype_symbol -> string (* prints the original QName *)

(* Special symbols *)

val anytype              : rtype_symbol
val anysimpletype        : rtype_symbol

(* Special symbols *)

val stringsym            : rtype_symbol
val booleansym           : rtype_symbol
val decimalsym           : rtype_symbol
val floatsym             : rtype_symbol
val doublesym            : rtype_symbol
val durationsym          : rtype_symbol
val dateTimesym          : rtype_symbol
val timesym              : rtype_symbol
val datesym              : rtype_symbol
val gYearMonthsym        : rtype_symbol
val gYearsym             : rtype_symbol
val gMonthDaysym         : rtype_symbol
val gDaysym              : rtype_symbol
val gMonthsym            : rtype_symbol
val hexBinarysym         : rtype_symbol
val base64Binarysym      : rtype_symbol
val anyURIsym            : rtype_symbol
val qnamesym             : rtype_symbol
val notationsym          : rtype_symbol

val integersym           : rtype_symbol
val intsym               : rtype_symbol

(* Symbols in xs namespace *)
val dayTimeDurationsym   : rtype_symbol
val yearMonthDurationsym : rtype_symbol
val untypedAtomicsym     : rtype_symbol
val untypedsym           : rtype_symbol
val anyAtomicTypesym     : rtype_symbol


(* Attribute symbols *)
val idsym                : rattr_symbol
val idrefsym             : rattr_symbol

(***********************************)
(* Operations on anonymous symbols *)
(***********************************)

type anon_symbol = symbol

val anon_symbol        : rqname -> anon_symbol
val anon_name          : anon_symbol -> rqname
val anon_prefix        : anon_symbol -> prefix

val anon_equal : symbol -> symbol -> bool
val anon_subtag : symbol -> symbol -> bool

val anon_string        : anon_symbol -> string
val anon_prefix_string : anon_symbol -> string


(************************)
(* Generic symbol types *)
(************************)

(* Generic resolved symbols *)

type resolved_unit_symbol =
  | RUnitTupleSymbol                      (* for tuples *)
  | RUnitDocSymbol                        (* for documents *)
  | RUnitElemSymbol   of relem_symbol     (* for elements *)
  | RUnitAttrSymbol   of rattr_symbol     (* for attributes *)
  | RUnitTextSymbol                       (* for text nodes *)
  | RUnitSimpleSymbol of rtype_symbol     (* for simple types *)
  | RUnitAnonSymbol   of anon_symbol      (* for anonymous symbols *)

val subtag   : resolved_unit_symbol -> resolved_unit_symbol -> bool
val equaltag : resolved_unit_symbol -> resolved_unit_symbol -> bool

