(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: physical_util.ml,v 1.24 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Physical_util
   Description:
     This module supports utility functions on the abstract
     implementation of XQuery 1.0 and XPath 2.0 data model sequence.

*)

open Error

open Datatypes

open Dm_atomic
open Dm

open Physical_value
open Physical_item

(* Some useful value accessors *)

let get_string i =
  try
    let i = Cursor.cursor_get_singleton i in
    (getAtomicValue i)#getAtomicString()
  with
  | _ ->
      raise (Query (Parameter_Mismatch("Parameter is not a string")))

let get_hexBinary n =
  try
    let n = Cursor.cursor_get_singleton n in
    (getAtomicValue n)#getAtomicHexBinary()
  with
  | _ ->
      raise (Query (Parameter_Mismatch("Parameter is not an hexBinary")))

let get_base64Binary n =
  try
    let n = Cursor.cursor_get_singleton n in
    (getAtomicValue n)#getAtomicBase64Binary()
  with
  | _ ->
      raise (Query (Parameter_Mismatch("Parameter is not an base64Binary")))

let get_integer n =
  try
    let n = Cursor.cursor_get_singleton n in
    (getAtomicValue n)#getAtomicInteger()
  with
  | _ ->
      raise (Query (Parameter_Mismatch("Parameter is not an integer")))

let get_decimal_aux n = 
  let a = getAtomicValue n in
  let k = a#getAtomicValueKind() in
  let d = 
    match k with
    | ATDecimal -> a
    | ATInteger -> a#cast_to Namespace_context.empty_nsenv Namespace_symbols_builtin.xs_decimal ATDecimal 
    | _ -> raise (Query (Parameter_Mismatch("Parameter is not a decimal or derived from decimal")))
  in d#getAtomicDecimal()

let get_decimal n =
  try
    let n = Cursor.cursor_get_singleton n in
    get_decimal_aux n
  with
  | _ ->
      raise (Query (Parameter_Mismatch("Parameter is not a decimal")))

let get_float n =
  try
    let n = Cursor.cursor_get_singleton n in
    (getAtomicValue n)#getAtomicFloat()
  with
  | _ ->
      raise (Query (Parameter_Mismatch("Parameter is not a float")))

let get_double n =
  try
    let n = Cursor.cursor_get_singleton n in
    (getAtomicValue n)#getAtomicDouble()
  with
  | _ ->
      raise (Query (Parameter_Mismatch("Parameter is not a double")))

let get_anyURI n =
  try
    let n = Cursor.cursor_get_singleton n in
   (getAtomicValue n)#getAtomicAnyURI()
  with
  | _ ->
      raise (Query (Parameter_Mismatch("Parameter is not a URI")))

let get_QName n =
  try
    let n = Cursor.cursor_get_singleton n in
    (getAtomicValue n)#getAtomicQName()
  with
  | _ ->
      raise (Query (Parameter_Mismatch("Parameter is not a QName")))

let get_boolean n =
  try
    let n = Cursor.cursor_get_singleton n in
    (getAtomicValue n)#getAtomicBoolean()
  with
  | _ ->
      raise (Query (Parameter_Mismatch("Parameter is not a boolean")))

let get_date n =
  try
    let n = Cursor.cursor_get_singleton n in
    (getAtomicValue n)#getAtomicDate()
  with
  | _ ->
      raise (Query (Parameter_Mismatch("Parameter is not a date")))

let get_gYearMonth n =
  try
    let n = Cursor.cursor_get_singleton n in
    (getAtomicValue n)#getAtomicGYearMonth()
  with
  | _ ->
      raise (Query (Parameter_Mismatch("Parameter is not a gYearMonth")))

let get_gYear n =
  try
    let n = Cursor.cursor_get_singleton n in
    (getAtomicValue n)#getAtomicGYear()
  with
  | _ ->
      raise (Query (Parameter_Mismatch("Parameter is not a gYear")))

let get_gMonthDay n =
  try
    let n = Cursor.cursor_get_singleton n in
    (getAtomicValue n)#getAtomicGMonthDay()
  with
  | _ ->
      raise (Query (Parameter_Mismatch("Parameter is not a gMonthDay")))

let get_gDay n =
  try
    let n = Cursor.cursor_get_singleton n in
    (getAtomicValue n)#getAtomicGDay()
  with
  | _ ->
      raise (Query (Parameter_Mismatch("Parameter is not a gDay")))

let get_gMonth n =
  try
    let n = Cursor.cursor_get_singleton n in
    (getAtomicValue n)#getAtomicGMonth()
  with
  | _ ->
      raise (Query (Parameter_Mismatch("Parameter is not a gMonth")))

let get_time n =
  try
    let n = Cursor.cursor_get_singleton n in
    (getAtomicValue n)#getAtomicTime()
  with
  | _ ->
      raise (Query (Parameter_Mismatch("Parameter is not a time")))

let get_dateTime n =
  try
    let n = Cursor.cursor_get_singleton n in
    (getAtomicValue n)#getAtomicDateTime()
  with
  | _ ->
      raise (Query (Parameter_Mismatch("Parameter is not a dateTime")))

let get_yearMonthDuration n =
  try
  let n = Cursor.cursor_get_singleton n in
    (getAtomicValue n)#getAtomicYearMonthDuration()
  with
  | _ ->
      raise (Query (Parameter_Mismatch("Parameter is not a yearMonthDuration")))

let get_dayTimeDuration n =
  try
    let n = Cursor.cursor_get_singleton n in
    (getAtomicValue n)#getAtomicDayTimeDuration()
  with
  | _ ->
      raise (Query (Parameter_Mismatch("Parameter is not a dayTimeDuration")))

let get_duration n =
  try
    let n = Cursor.cursor_get_singleton n in
    let a = getAtomicValue n in
    let k = a#getAtomicValueKind() in
    let d = 
      match k with
      | ATDuration -> a
      | ATYearMonthDuration -> a#cast_to Namespace_context.empty_nsenv Namespace_symbols_builtin.xs_duration ATDuration
      | ATDayTimeDuration -> a#cast_to Namespace_context.empty_nsenv Namespace_symbols_builtin.xs_duration ATDuration
      | _ -> raise (Query (Parameter_Mismatch("Parameter is not a duration or derived from duration")))
    in d#getAtomicDuration()
  with
  | _ ->
      raise (Query (Parameter_Mismatch("Parameter is not a duration")))

let get_dtd_from_duration n =
  try
    let n = Cursor.cursor_get_singleton n in
    let a = getAtomicValue n in
    let k = a#getAtomicValueKind() in
    let d = 
      match k with
      | ATDuration -> a#cast_to Namespace_context.empty_nsenv Namespace_symbols_builtin.xs_dayTimeDuration ATDayTimeDuration
      | ATYearMonthDuration -> a#cast_to Namespace_context.empty_nsenv Namespace_symbols_builtin.xs_dayTimeDuration ATDayTimeDuration
      | ATDayTimeDuration -> a
      | _ -> raise (Query (Parameter_Mismatch("Parameter is not a duration or derived from duration")))
    in d#getAtomicDayTimeDuration()
  with
  | _ ->
      raise (Query (Parameter_Mismatch("Parameter is not a duration")))

let get_ymd_from_duration n =
  try
    let n = Cursor.cursor_get_singleton n in
    let a = getAtomicValue n in
    let k = a#getAtomicValueKind() in
    let d = 
      match k with
      | ATDuration -> a#cast_to Namespace_context.empty_nsenv Namespace_symbols_builtin.xs_yearMonthDuration ATYearMonthDuration
      | ATYearMonthDuration -> a
      | ATDayTimeDuration -> a#cast_to Namespace_context.empty_nsenv Namespace_symbols_builtin.xs_yearMonthDuration ATYearMonthDuration
      | _ -> raise (Query (Parameter_Mismatch("Parameter is not a duration or derived from duration")))
    in d#getAtomicYearMonthDuration()
  with
  | _ ->
      raise (Query (Parameter_Mismatch("Parameter is not a duration")))

let get_string_cursor n =
  let get_string_in_cursor i =
    try
      (getAtomicValue i)#getAtomicString()
    with
    | _ ->
	raise (Query (Parameter_Mismatch("Parameter is not a string")))
  in
  Cursor.cursor_map get_string_in_cursor n

let get_dayTimeDuration_cursor n =
  let get_dayTimeDuration_in_cursor i =
    try
      (getAtomicValue i)#getAtomicDayTimeDuration()
    with
    | _ ->
	raise (Query (Parameter_Mismatch("Parameter is not an dayTimeDuration")))
  in
  Cursor.cursor_map get_dayTimeDuration_in_cursor n

let get_yearMonthDuration_cursor n =
  let get_yearMonthDuration_in_cursor i =
    try
      (getAtomicValue i)#getAtomicYearMonthDuration()
    with
    | _ ->
	raise (Query (Parameter_Mismatch("Parameter is not an yearMonthDuration")))
  in
  Cursor.cursor_map get_yearMonthDuration_in_cursor n

let get_date_cursor n =
  let get_date_in_cursor i =
    try
      (getAtomicValue i)#getAtomicDate()
    with
    | _ ->
	raise (Query (Parameter_Mismatch("Parameter is not an date")))
  in
  Cursor.cursor_map get_date_in_cursor n

let get_time_cursor n =
  let get_time_in_cursor i =
    try
      (getAtomicValue i)#getAtomicTime()
    with
    | _ ->
	raise (Query (Parameter_Mismatch("Parameter is not an time")))
  in
  Cursor.cursor_map get_time_in_cursor n

let get_dateTime_cursor n =
  let get_dateTime_in_cursor i =
    try
      (getAtomicValue i)#getAtomicDateTime()
    with
    | _ ->
	raise (Query (Parameter_Mismatch("Parameter is not an dateTime")))
  in
  Cursor.cursor_map get_dateTime_in_cursor n

let get_integer_cursor n =
  let get_integer_in_cursor i =
    try
      (getAtomicValue i)#getAtomicInteger()
    with
    | _ ->
	raise (Query (Parameter_Mismatch("Parameter is not an integer")))
  in
  Cursor.cursor_map get_integer_in_cursor n

let get_decimal_cursor n =
  let get_decimal_in_cursor i =
    try
      get_decimal_aux i 
    with
    | _ ->
	raise (Query (Parameter_Mismatch("Parameter is not a decimal")))
  in
  Cursor.cursor_map get_decimal_in_cursor n

let get_float_cursor n =
  let get_float_in_cursor i =
    try
      (getAtomicValue i)#getAtomicFloat()
    with
    | _ ->
	raise (Query (Parameter_Mismatch("Parameter is not a float")))
  in
  Cursor.cursor_map get_float_in_cursor n

let get_double_cursor n =
  let get_double_in_cursor i =
    try
      (getAtomicValue i)#getAtomicDouble()
    with
    | _ ->
	raise (Query (Parameter_Mismatch("Parameter is not a double")))
  in
  Cursor.cursor_map get_double_in_cursor n

let get_node n =
  if (item_kind n) = NodeKind
  then
    getNode n
  else
    raise (Query (Parameter_Mismatch("Parameter '"^(string_value n)^"' is not a single node")))

let get_singleton_node n =
  try
    get_node (Cursor.cursor_get_singleton n)
  with _ ->
      raise (Query (Parameter_Mismatch("Parameter is not a single node")))

let get_atomic n =
  try
    getAtomicValue n
  with
  | _ ->
      raise (Query (Parameter_Mismatch("Parameter is not an atomic value")))

let get_singleton_atomic n =
  get_atomic (Cursor.cursor_get_singleton n)

let get_optional_atomic n =
  match Cursor.cursor_get_optional n with
  | None -> None
  | Some i -> Some (get_atomic i)

let get_optional_string n =
  match Cursor.cursor_get_optional n with
  | None -> None
  | Some i -> Some
	(try
	  (getAtomicValue i)#getAtomicString()
	with
	| _ ->
	    raise (Query (Parameter_Mismatch("Parameter is not a string"))))

let get_optional_double n =
  match Cursor.cursor_get_optional n with
  | None -> None
  | Some i -> Some
	(try
	  (getAtomicValue i)#getAtomicDouble()
	with
	| _ ->
	    raise (Query (Parameter_Mismatch("Parameter is not a double"))))

let get_optional_float n =
  match Cursor.cursor_get_optional n with
  | None -> None
  | Some i -> Some 
	(try
	  (getAtomicValue i)#getAtomicFloat()
	with
	| _ ->
	    raise (Query (Parameter_Mismatch("Parameter is not a float"))))

let get_optional_decimal n =
  match Cursor.cursor_get_optional n with
  | None -> None
  | Some i -> 
      try
	Some (get_decimal_aux i)
      with
      | _ ->
	  raise (Query (Parameter_Mismatch("Parameter is not a decimal")))

let get_optional_integer n =
  match Cursor.cursor_get_optional n with
  | None -> None
  | Some i -> Some
	(try
	  (getAtomicValue i)#getAtomicInteger()
	with
	| _ ->
	    raise (Query (Parameter_Mismatch("Parameter is not a integer"))))

let get_optional_date n =
  match Cursor.cursor_get_optional n with
  | None -> None
  | Some i -> Some
	(try
	  (getAtomicValue i)#getAtomicDate()
	with
	| _ ->
	    raise (Query (Parameter_Mismatch("Parameter is not a date"))))

let get_optional_time n =
  match Cursor.cursor_get_optional n with
  | None -> None
  | Some i -> Some
	(try
	  (getAtomicValue i)#getAtomicTime()
	with
	| _ ->
	    raise (Query (Parameter_Mismatch("Parameter is not a time"))))

let get_item n =
  try 
    Cursor.cursor_get_singleton n
  with
  | _ -> 
      raise (Query (Parameter_Mismatch("Parameter is not a single item")))

let get_optional_item n =
  try 
    Cursor.cursor_get_optional n
  with
  | _ -> 
      raise (Query (Parameter_Mismatch("Parameter is not empty or a single item")))

let get_atomic_cursor n =
  let get_atomic_in_cursor i =
    try
      (getAtomicValue i)
    with
    | _ ->
	raise (Query (Parameter_Mismatch("Parameter is not an atomic value")))
  in
  Cursor.cursor_map get_atomic_in_cursor n

let get_node_cursor n =
  let get_node_in_cursor n =
    try
      (getNode n)
    with
    | _ ->
	raise (Query (Parameter_Mismatch("Parameter is not a node")))
  in
  Cursor.cursor_map get_node_in_cursor n

let get_node_list_of_item_list n =
  List.map (get_node) n

let get_atomic_list_of_item_list n =
  List.map (get_atomic) n

let _integer_cursor il = 
  Cursor.cursor_map (fun i ->(Item_Atomic (new atomicInteger(i)))) il

let _node_cursor nl =
  Cursor.cursor_map (fun n -> Item_Node n) nl

let _node_list nl =
  _node_cursor (Cursor.cursor_of_list nl)

let _atomic_cursor nl =
  Cursor.cursor_map (fun n -> Item_Atomic n) nl

let _atomic_list nl =
  _atomic_cursor (Cursor.cursor_of_list nl)

let _atomic_value av = 
  Cursor.cursor_of_singleton (Item_Atomic av)

