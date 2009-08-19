(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: namespace_symbols.ml,v 1.12 2007/08/20 13:12:40 simeon Exp $ *)

(* Module: Namespace_symbols
   Description:
     This module implements symbol tables for QNames.
*)

open Error
open Pool

open Namespace_names
open Namespace_builtin
open Namespace_context


(****************)
(* Symbol pools *)
(****************)

(* Pool for namespace prefixes *)

module Prefix =
  struct
    type t = prefix
    let equal = (=)
    let hash = prefix_hash
  end

module PrefixPool = MakeNamePool (Prefix)

type prefix_symbol = PrefixPool.symbol

let prefix_pool = PrefixPool.create_pool ()


(* Pool for namespace URIs *)

module Uri =
  struct
    type t = uri
    let equal = (=)
    let hash = uri_hash
  end

module UriPool = MakeNamePool (Uri)

type uri_symbol = UriPool.symbol

let uri_pool    = UriPool.create_pool ()


(* Namepool for local names *)

module Ncname =
  struct
    type t = ncname
    let equal = (=)
    let hash = ncname_hash
  end

module NcnamePool = MakeNamePool (Ncname)

type ncname_symbol = NcnamePool.symbol

let ncname_pool = NcnamePool.create_pool ()


(***********)
(* Symbols *)
(***********)

(* Note:
      Symbols are triples now.
   - Jerome *)

type symbol = (prefix_symbol * uri_symbol * ncname_symbol)

(* Symbol creation *)

let symbol_create (prefix,uri,ncname) =
  (PrefixPool.add_name prefix_pool prefix,
   UriPool.add_name uri_pool uri,
   NcnamePool.add_name ncname_pool ncname)

let wildcard_symbol =
  symbol_create Namespace_builtin.wild_rqname
let (wildcard_prefix_symbol,wildcard_uri_symbol,wildcard_ncname_symbol) =
  wildcard_symbol


(* Symbol lookup *)

(* Note: Those look-ups are expensive and should be avoided - Jerome *)
let symbol_name (ps,uris,ncnames) =
  (PrefixPool.get_name prefix_pool ps,
   UriPool.get_name uri_pool uris,
   NcnamePool.get_name ncname_pool ncnames)

let symbol_prefix (ps,_,_) =
  PrefixPool.get_name prefix_pool ps

let symbol_string s =
  let rs = symbol_name s in
    quoted_uri_string_of_rqname rs

let symbol_prefix_string s =
  let rqname = symbol_name s in
    prefixed_string_of_rqname rqname


(* Equality between symbols *)

let symbol_equal (_,uris1,ncnames1) (_,uris2,ncnames2) =
   (uris1 = uris2) && (ncnames1 = ncnames2)

let symbol_hash (_,uris1,ncnames1) =
  Hashtbl.hash (uris1,ncnames1)

(* Subtagging *)

let symbol_subtag (_,uris1,ncnames1) (_,uris2,ncnames2) =
  if (uris2 = wildcard_uri_symbol)
  then
    if (ncnames2 = wildcard_ncname_symbol)
    then true
    else (ncnames2 = ncnames1)
  else
    if (ncnames2 = wildcard_ncname_symbol)
    then (uris2 = uris1)
    else (ncnames2 = ncnames1) && (uris2 = uris1)


(*********************************)
(* Operations on element symbols *)
(*********************************)

type relem_symbol = symbol

(* Creates a new symbol *)

let relem_symbol rqname = symbol_create rqname

let relem_equal  = symbol_equal
let relem_subtag = symbol_subtag


(* Note: Those look-ups are expensive *)
let relem_name s = symbol_name s

let relem_name_with_binding nsenv s =
  let rqname = relem_name s in
  make_binding nsenv rqname

let relem_uname nsenv s =
  let rqname = relem_name s in
  let (uqname,_,_) = make_binding nsenv rqname in
  uqname


let relem_prefix s = symbol_prefix s
let relem_string s = symbol_string s
let relem_prefix_string s = symbol_prefix_string s

(* Special symbols *)

let anyrelem = wildcard_symbol


(************************************)
(* Operations on attributes symbols *)
(************************************)

type rattr_symbol = symbol

(* Creates/accesses symbols *)

let rattr_symbol rqname = symbol_create rqname

let rattr_equal  = symbol_equal
let rattr_subtag = symbol_subtag

let rattr_name s = symbol_name s
let rattr_name_with_binding nsenv s =
  let rqname = symbol_name s in
  make_attribute_binding nsenv rqname
let rattr_uname nsenv s =
  let rqname = symbol_name s in
  let (uqname,_,_) = make_attribute_binding nsenv rqname in
  uqname

let rattr_prefix s =
  let (prefix,uri,_) = rattr_name s in
  match uri with
  | NSUri "" ->
      NSDefaultElementPrefix
  | _ ->
      prefix

(* Converts a symbol to a string *)

let rattr_string s = symbol_string s
let rattr_prefix_string s = symbol_prefix_string s

(* Special symbols *)

let anyrattr = wildcard_symbol


(******************************)
(* Operations on type symbols *)
(******************************)

type rtype_symbol = symbol

(* Creates/accesses symbols *)

let rtype_symbol rqname = symbol_create rqname

let rtype_name s = symbol_name s
let rtype_prefix s = symbol_prefix s

let rtype_equal  = symbol_equal
let rtype_subtag = symbol_subtag

let rtype_string s = symbol_string s
let rtype_prefix_string s = symbol_prefix_string s

(* Special formal semantics symbols *)

let anysimpletype = rtype_symbol xs_anySimpleType
let anytype       = rtype_symbol xs_anyType

(* Special symbols *)

let stringsym       = rtype_symbol xs_string
let booleansym      = rtype_symbol xs_boolean
let decimalsym      = rtype_symbol xs_decimal
let floatsym        = rtype_symbol xs_float
let doublesym       = rtype_symbol xs_double
let durationsym     = rtype_symbol xs_duration
let dateTimesym     = rtype_symbol xs_dateTime
let timesym         = rtype_symbol xs_time
let datesym         = rtype_symbol xs_date
let gYearMonthsym   = rtype_symbol xs_gYearMonth
let gYearsym        = rtype_symbol xs_gYear
let gMonthDaysym    = rtype_symbol xs_gMonthDay
let gDaysym         = rtype_symbol xs_gDay
let gMonthsym       = rtype_symbol xs_gMonth
let hexBinarysym    = rtype_symbol xs_hexBinary
let base64Binarysym = rtype_symbol xs_base64Binary
let anyURIsym       = rtype_symbol xs_anyURI
let qnamesym        = rtype_symbol xs_QName
let notationsym     = rtype_symbol xs_NOTATION

let integersym      = rtype_symbol xs_integer
let intsym          = rtype_symbol xs_int

(* Predefined types in xs namespace: *)
let dayTimeDurationsym   = rtype_symbol xs_dayTimeDuration
let yearMonthDurationsym = rtype_symbol xs_yearMonthDuration
let untypedAtomicsym     = rtype_symbol xs_untypedAtomic
let untypedsym           = rtype_symbol xs_untyped
let anyAtomicTypesym     = rtype_symbol xs_anyAtomicType

(* Attribute symbols *)
let idsym                = rattr_symbol empty_ID
let idrefsym             = rattr_symbol empty_IDREF


(***********************************)
(* Operations on anonymous symbols *)
(***********************************)

(* Operations on resolved anonymous symbols *)

type anon_symbol = symbol

(* Creates/accesses symbols *)

let anon_symbol rqname = symbol_create rqname

let anon_equal  = symbol_equal
let anon_subtag = symbol_subtag

let anon_name s   = symbol_name s
let anon_prefix s = symbol_prefix s

(* Converts a symbol to a string *)

let anon_string s = symbol_string s
let anon_prefix_string s = symbol_prefix_string s


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

let equaltag s1 s2 =
  match (s1,s2) with
  | (RUnitDocSymbol, RUnitDocSymbol) ->
      true
  | (RUnitElemSymbol es1, RUnitElemSymbol es2) ->
      symbol_equal es1 es2
  | (RUnitAttrSymbol as1, RUnitAttrSymbol as2) ->
      symbol_equal as1 as2
  | (RUnitSimpleSymbol ss1, RUnitSimpleSymbol ss2) ->
      symbol_equal ss1 ss2
  | (RUnitAnonSymbol ss1, RUnitAnonSymbol ss2) ->
      symbol_equal ss1 ss2
  | (RUnitTextSymbol, RUnitTextSymbol) ->
      true
  | _ ->
      false

let subtag s1 s2 =
  match (s1,s2) with
  | (RUnitDocSymbol, RUnitDocSymbol) ->
      true
  | (RUnitElemSymbol es1, RUnitElemSymbol es2) ->
      symbol_subtag es1 es2
  | (RUnitAttrSymbol as1, RUnitAttrSymbol as2) ->
      symbol_subtag as1 as2
  | (RUnitTextSymbol, RUnitTextSymbol) ->
      true
  | (RUnitSimpleSymbol ss1, RUnitSimpleSymbol ss2) ->
      (symbol_equal ss1 ss2) (* || (symbol_equal ss2 anyatomic) ??? *)
  | (RUnitAnonSymbol ss1, RUnitAnonSymbol ss2) ->
      symbol_equal ss1 ss2
  | _ ->
      false

