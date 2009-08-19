(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: dm_atomic.ml,v 1.15 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Datatypes_atomic
   Description:
     Galax's abstract data model interface for atomic values.
*)

open Error

open Namespace_builtin
open Namespace_context
open Namespace_symbols

open Datatypes
open Datatypes_util
open DateTime
open Decimal
open AnyURI


(************************)
(* Atomic value objects *)
(************************)

class virtual atomicValue type_annotation =
  object (self)
    (* Note:
         Although given in the 'virtual' interface, the atomicValues
         are in fact part of the Galax implementation.
       - Jerome *)

    val at = type_annotation

    method virtual erase_atomic_value : unit -> xs_untyped
    method string_value () = self#erase_atomic_value()
    method virtual getAtomicValueKind : unit -> Datatypes.atomic_type
    method virtual cast_to : nsenv -> Namespace_symbols.rtype_symbol -> atomic_type -> atomicValue
    method atomic_type () = at

    method atomic_value_eq v2 = 
      if (self#getAtomicValueKind() = v2#getAtomicValueKind()) then
	self#eq(v2)
      else false
    method private virtual eq : atomicValue -> bool 
    method atomic_value_lteq v2 = 
      if (self#getAtomicValueKind() = v2#getAtomicValueKind()) then
	  self#lteq(v2)
	else false
      method private virtual lteq : atomicValue -> bool
      method atomic_value_lt v2 = 
	if (self#getAtomicValueKind() = v2#getAtomicValueKind()) then
	  self#lt(v2)
	else false
      method private virtual lt : atomicValue -> bool
      method atomic_value_gteq v2 = 
	if (self#getAtomicValueKind() = v2#getAtomicValueKind()) then
	  self#gteq(v2)
	else false
      method private virtual gteq : atomicValue -> bool
      method atomic_value_gt v2 = 
	if (self#getAtomicValueKind() = v2#getAtomicValueKind()) then
	  self#gt(v2)
	else false
      method private virtual gt : atomicValue -> bool

      method atomic_value_compare v2 =
	if not(self#getAtomicValueKind() = v2#getAtomicValueKind())
	then
	  raise (Query (Datamodel "Comparison between two incompatible values"))
	else
	  if self#atomic_value_eq(v2) then  0
	  else
	    if self#atomic_value_gt(v2) then 1
	    else -1
      method getAtomicString () : xs_string = raise (Query (Datamodel "Not an atomic string"))
      method getAtomicBoolean () : xs_boolean = raise (Query (Datamodel "Not an atomic boolean"))
      method getAtomicDecimal () : xs_decimal = raise (Query (Datamodel "Not an atomic decimal"))
      method getAtomicFloat () : xs_float = raise (Query (Datamodel "Not an atomic float"))
      method getAtomicDouble () : xs_double = raise (Query (Datamodel "Not an atomic double"))
      method getAtomicInteger () : xs_integer = raise (Query (Datamodel "Not an atomic integer"))
      method getAtomicDuration () : xs_duration = raise (Query (Datamodel "Not an atomic duration"))
      method getAtomicDateTime () : xs_dateTime = raise (Query (Datamodel "Not an atomic date time"))
      method getAtomicTime () : xs_time = raise (Query (Datamodel "Not an atomic time"))
      method getAtomicDate () : xs_date = raise (Query (Datamodel "Not an atomic date"))
      method getAtomicGYearMonth () : xs_gYearMonth = raise (Query (Datamodel "Not an atomic gYearMonth"))
      method getAtomicGYear () : xs_gYear = raise (Query (Datamodel "Not an atomic gYear"))
      method getAtomicGMonthDay () : xs_gMonthDay = raise (Query (Datamodel "Not an atomic gMonthDay"))
      method getAtomicGDay () : xs_gDay = raise (Query (Datamodel "Not an atomic gDay"))
      method getAtomicGMonth () : xs_gMonth = raise (Query (Datamodel "Not an atomic gMonth"))
      method getAtomicYearMonthDuration () : xs_yearMonthDuration = raise (Query (Datamodel "Not an atomic yearMonthDuration"))
      method getAtomicDayTimeDuration () : xs_dayTimeDuration = raise (Query (Datamodel "Not an atomic dayTimeDuration"))
      method getAtomicHexBinary ()    : xs_hexBinary = raise (Query (Datamodel "Not an atomic hexBinary"))
      method getAtomicBase64Binary () : xs_base64Binary = raise (Query (Datamodel "Not an atomic base64Binary"))
      method getAtomicAnyURI ()       : xs_anyURI = raise (Query (Datamodel "Not an atomic anyURI"))
      method getAtomicQName ()        : xs_QName = raise (Query (Datamodel "Not an atomic QName"))
      method getAtomicNotation ()     : xs_NOTATION = raise (Query (Datamodel "Not an atomic NOTATION"))
      method getAtomicUntyped ()      : xs_untyped = raise (Query (Datamodel "Not an atomic untyped"))
  end

and atomicString ?ta:type_annotation v = 
  let ta =
    match type_annotation with
    | None -> Namespace_symbols_builtin.xs_string
    | Some x -> x
  in
  object (self)
    inherit atomicValue ta
    val v' = v

    method erase_atomic_value () = v
    method getAtomicValueKind () = ATString
    method getAtomicString () = v'
    method private eq a = 
      string_equal v' (a#getAtomicString())
    method private lteq a = 
      string_lteq v' (a#getAtomicString())
    method private lt a = 
      string_lt v' (a#getAtomicString())
    method private gteq a = 
      string_gteq v' (a#getAtomicString())
    method private gt a = 
      string_gt v' (a#getAtomicString())
    method cast_to nsenv at bt : atomicValue = 
      match bt with 
      | ATString -> ((new atomicString  ~ta:at v') :> atomicValue) (* (self :> atomicValue) *)
      | ATBoolean ->
	  (new atomicBoolean ~ta:at (boolean_of_untyped v'))
      | ATDecimal -> 
	  (new atomicDecimal ~ta:at (decimal_of_untyped v'))
      | ATFloat -> 
	  (new atomicFloat ~ta:at (float_of_untyped v'))
      | ATDouble -> 
	  (new atomicDouble ~ta:at (double_of_untyped v'))
      | ATInteger ->
	  (new atomicInteger ~ta:at (integer_of_untyped at v'))
      | ATDuration ->
	  (new atomicDuration ~ta:at (duration_of_untyped v'))
      | ATDateTime ->
	  (new atomicDateTime ~ta:at (dateTime_of_untyped v'))
      | ATTime ->
	  (new atomicTime ~ta:at (time_of_untyped v'))
      | ATDate ->
	  (new atomicDate ~ta:at (date_of_untyped v'))
      | ATGYearMonth ->
	  (new atomicGYearMonth ~ta:at (gYearMonth_of_untyped v'))
      | ATGYear ->
	  (new atomicGYear ~ta:at (gYear_of_untyped v'))
      | ATGMonthDay ->
	  (new atomicGMonthDay ~ta:at (gMonthDay_of_untyped v'))
      | ATGDay ->
	  (new atomicGDay ~ta:at (gDay_of_untyped v'))
      | ATGMonth ->
	  (new atomicGMonth ~ta:at (gMonth_of_untyped v'))
      | ATHexBinary ->
	  ((new atomicHexBinary ~ta:at (hexBinary_of_untyped v')) :> atomicValue)
      | ATBase64Binary ->
	  ((new atomicBase64Binary ~ta:at (base64Binary_of_untyped v')) :> atomicValue)
      | ATAnyURI ->
	  ((new atomicAnyURI ~ta:at (anyURI_of_untyped v')) :> atomicValue)
      | ATQName -> 
	  ((new atomicQName ~ta:at (qname_of_untyped nsenv v')) :> atomicValue)
      | ATNOTATION -> 
	  raise (Query (Prototype "Cast to a NOTATION not supported"))
      | ATYearMonthDuration ->
	  (new atomicYearMonthDuration ~ta:at (yearMonthDuration_of_untyped v'))
      | ATDayTimeDuration ->
	  (new atomicDayTimeDuration ~ta:at (dayTimeDuration_of_untyped v'))
      | ATUntypedAtomic -> (new atomicUntyped ~ta:at (v'))
      | ATAnyAtomic -> (self :> atomicString)
  end


and atomicBoolean ?ta:type_annotation v = 
  let ta =
    match type_annotation with
    | None -> Namespace_symbols_builtin.xs_boolean
    | Some x -> x
  in
  object (self)
    inherit atomicValue ta
    val v' = v

    method erase_atomic_value () = string_of_bool v
    method getAtomicValueKind () = ATBoolean
    method getAtomicBoolean () = v'
    method private eq a = 
	  bool_equal v' (a#getAtomicBoolean())
    method private lteq a = 
	  bool_lteq v' (a#getAtomicBoolean())
    method private lt a = 
	  bool_lt v' (a#getAtomicBoolean())
    method private gteq a = 
	  bool_gteq v' (a#getAtomicBoolean())
    method private gt a = 
	  bool_gt v' (a#getAtomicBoolean())
    method cast_to nsenv at bt = 
      match bt with 
      | ATString ->
 	  ((new atomicString ~ta:at (if v' then "true" else "false")) :> atomicValue)
      | ATBoolean -> (self :> atomicBoolean)
      | ATDecimal -> 
	  if v' then (new atomicDecimal ~ta:at Decimal._decimal_one) else (new atomicDecimal ~ta:at Decimal._decimal_zero)
      | ATFloat -> 
	  if v' then (new atomicFloat ~ta:at 1.0) else (new atomicFloat ~ta:at 0.0)
      | ATDouble -> 
	  if v' then (new atomicDouble ~ta:at 1.0) else (new atomicDouble ~ta:at 0.0)
      | ATInteger -> 
	  if v' then (new atomicInteger ~ta:at Decimal._integer_one) else (new atomicInteger ~ta:at Decimal._integer_zero)
      | ATDuration ->
	  raise (Query (Prototype "Cast to a Duration not supported"))
      | ATDateTime -> 
	  raise (Query (Cast_Error "Cannot cast a Boolean value to a DateTime"))
      | ATTime ->
	  raise (Query (Prototype "Cast to a Time not supported"))
      | ATDate ->
	  raise (Query (Prototype "Cast to a Date not supported"))
      | ATGYearMonth ->
	  raise (Query (Prototype "Cast to a GYearMonth not supported"))
      | ATGYear ->
	  raise (Query (Prototype "Cast to a GYear not supported"))
      | ATGMonthDay ->
	  raise (Query (Prototype "Cast to a GMonthDay not supported"))
      | ATGDay ->
	  raise (Query (Prototype "Cast to a GDay not supported"))
      | ATGMonth ->
	  raise (Query (Prototype "Cast to a GMonth not supported"))
      | ATHexBinary ->
	  raise (Query (Cast_Error "Cannot cast to a HexBinary"))
      | ATBase64Binary ->
	  raise (Query (Cast_Error "Cannot cast to a Base64Binary"))
      | ATAnyURI ->
	  raise (Query (Cast_Error "Cannot cast a Boolean value to a AnyURI"))
      | ATQName -> 
	  raise (Query (Cast_Error "Cannot cast a Boolean value to a QName"))
      | ATNOTATION -> 
	  raise (Query (Prototype "Cast to a NOTATION not supported"))
      | ATYearMonthDuration ->
	  raise (Query (Prototype "Cast to a yearMonthDuration not supported"))
      | ATDayTimeDuration ->
	  raise (Query (Prototype "Cast to a dateTimeDuration not supported"))
      | ATUntypedAtomic -> (new atomicUntyped ~ta:at (if v' then "true" else "false"))
      | ATAnyAtomic -> (self :> atomicBoolean)
  end

and atomicDecimal ?ta:type_annotation v = 
  let ta =
    match type_annotation with
    | None -> Namespace_symbols_builtin.xs_decimal
    | Some x -> x
  in
  object (self)
    inherit atomicValue ta
    val v' = v 

    method erase_atomic_value () = Decimal._string_of_decimal v'
    method getAtomicValueKind () = ATDecimal
    method getAtomicDecimal () = v'
    method private eq a = 
	  _decimal_eq v' (a#getAtomicDecimal())
    method private lteq a = 
	  _decimal_le v' (a#getAtomicDecimal())
    method private lt a = 
	  _decimal_lt v' (a#getAtomicDecimal())
    method private gteq a = 
	  _decimal_ge v' (a#getAtomicDecimal())
    method private gt a = 
	  _decimal_gt v' (a#getAtomicDecimal())
    method cast_to nsenv at bt = 
      match bt with 
      | ATString ->
	  ((new atomicString ~ta:at (Decimal._string_of_decimal v')) :> atomicValue)
      | ATBoolean -> 
	  (new atomicBoolean ~ta:at (if Decimal._decimal_eq v' _decimal_zero then false else true))
      | ATDecimal -> 
	  (new atomicDecimal ~ta:at v') (* self *)
      | ATFloat -> 
	  (new atomicFloat ~ta:at (float_of_untyped (Decimal._string_of_decimal v')))
      | ATDouble -> 
	  (new atomicDouble ~ta:at (double_of_untyped (Decimal._string_of_decimal v')))
      | ATInteger -> 
	  (new atomicInteger ~ta:at (Decimal._cast_decimal_to_integer v'))
      | ATDuration ->
	  raise (Query (Prototype "Cast to a Duration not supported"))
      | ATDateTime -> 
	  raise (Query (Cast_Error "Cannot cast a Decimal value to a DateTime"))
      | ATTime ->
	  raise (Query (Prototype "Cast to a Time not supported"))
      | ATDate ->
	  raise (Query (Prototype "Cast to a Date not supported"))
      | ATGYearMonth ->
	  raise (Query (Prototype "Cast to a GYearMonth not supported"))
      | ATGYear ->
	  raise (Query (Prototype "Cast to a GYear not supported"))
      | ATGMonthDay ->
	  raise (Query (Prototype "Cast to a GMonthDay not supported"))
      | ATGDay ->
	  raise (Query (Prototype "Cast to a GDay not supported"))
      | ATGMonth ->
	  raise (Query (Prototype "Cast to a GMonth not supported"))
      | ATHexBinary ->
	  raise (Query (Cast_Error "Cannot cast to a HexBinary"))
      | ATBase64Binary ->
	  raise (Query (Cast_Error "Cannot cast to a Base64Binary"))
      | ATAnyURI ->
	  raise (Query (Cast_Error "Cannot cast a Decimal value to a AnyURI"))
      | ATQName -> 
	  raise (Query (Cast_Error "Cannot cast a Decimal value to a QName"))
      | ATNOTATION -> 
	  raise (Query (Prototype "Cast to a NOTATION not supported"))
      | ATYearMonthDuration ->
	  raise (Query (Prototype "Cast to a yearMonthDuration not supported"))
      | ATDayTimeDuration ->
	  raise (Query (Prototype "Cast to a dateTimeDuration not supported"))
      | ATUntypedAtomic -> 
	  (new atomicUntyped ~ta:at (Decimal._string_of_decimal v'))
      | ATAnyAtomic -> (self :> atomicDecimal)
  end

and atomicFloat ?ta:type_annotation v = 
  let ta =
    match type_annotation with
    | None -> Namespace_symbols_builtin.xs_float
    | Some x -> x
  in
  object (self)
    inherit atomicValue ta
    val v' = v 

    method erase_atomic_value () = serialize_float v'
    method getAtomicValueKind () = ATFloat
    method getAtomicFloat () = v'
    method private eq a = 
	  float_equal v' (a#getAtomicFloat())
    method private lteq a = 
	  float_lteq v' (a#getAtomicFloat())
    method private lt a = 
	  float_lt v' (a#getAtomicFloat())
    method private gteq a = 
	  float_gteq v' (a#getAtomicFloat())
    method private gt a = 
	  float_gt v' (a#getAtomicFloat())

    method cast_to nsenv at bt = 
      match bt with 
      | ATString ->
	  ((new atomicString ~ta:at (serialize_float v')) :> atomicValue)
      | ATBoolean -> 
	  (new atomicBoolean ~ta:at (if v' = 0.0 or Decimal.is_nan v' then false else true))
      | ATDecimal -> 
	  (new atomicDecimal ~ta:at (Decimal._decimal_of_float v'))
      | ATFloat -> 
	  (new atomicFloat ~ta:at v') (* self *)
      | ATDouble -> 
	  (new atomicDouble ~ta:at v')
      | ATInteger -> 
	  (new atomicInteger ~ta:at (Decimal._cast_float_to_integer v'))
      | ATDuration ->
	  raise (Query (Prototype "Cast to a Duration not supported"))
      | ATDateTime -> 
	  raise (Query (Cast_Error "Cannot cast a Float value to a DateTime"))
      | ATTime ->
	  raise (Query (Prototype "Cast to a Time not supported"))
      | ATDate ->
	  raise (Query (Prototype "Cast to a Date not supported"))
      | ATGYearMonth ->
	  raise (Query (Prototype "Cast to a GYearMonth not supported"))
      | ATGYear ->
	  raise (Query (Prototype "Cast to a GYear not supported"))
      | ATGMonthDay ->
	  raise (Query (Prototype "Cast to a GMonthDay not supported"))
      | ATGDay ->
	  raise (Query (Prototype "Cast to a GDay not supported"))
      | ATGMonth ->
	  raise (Query (Prototype "Cast to a GMonth not supported"))
      | ATHexBinary ->
	  raise (Query (Cast_Error "Cannot cast to a HexBinary"))
      | ATBase64Binary ->
	  raise (Query (Cast_Error "Cannot cast to a Base64Binary"))
      | ATAnyURI ->
	  raise (Query (Cast_Error "Cannot cast a Float value to a AnyURI"))
      | ATQName -> 
	  raise (Query (Cast_Error "Cannot cast a Float value to a QName"))
      | ATNOTATION -> 
	  raise (Query (Prototype "Cast to a NOTATION not supported"))
      | ATYearMonthDuration ->
	  raise (Query (Prototype "Cast to a yearMonthDuration not supported"))
      | ATDayTimeDuration ->
	  raise (Query (Prototype "Cast to a dateTimeDuration not supported"))
      | ATUntypedAtomic -> 
	  (* NB: Decimal._string_of_float implements lexical rules from F&O 17.1.2 *)
	  (new atomicUntyped ~ta:at (Decimal._string_of_float v'))
      | ATAnyAtomic -> (self :> atomicFloat)
  end

and atomicDouble ?ta:type_annotation v = 
  let ta =
    match type_annotation with
    | None -> Namespace_symbols_builtin.xs_double
    | Some x -> x
  in
  object (self)
    inherit atomicValue ta
    val v' = v 

    method erase_atomic_value () = serialize_double v'
    method getAtomicValueKind () = ATDouble
    method getAtomicDouble () = v'
    method private eq a = 
	  double_equal v' (a#getAtomicDouble())
    method private lteq a = 
	  double_lteq v' (a#getAtomicDouble())
    method private lt a = 
	  double_lt v' (a#getAtomicDouble())
    method private gt a = 
	  double_gt v' (a#getAtomicDouble())
    method private gteq a = 
	  double_gteq v' (a#getAtomicDouble())
    method cast_to nsenv at bt = 
      match bt with 
      | ATString ->
	  ((new atomicString ~ta:at (serialize_double v') :> atomicValue))
      | ATBoolean -> 
	  (new atomicBoolean ~ta:at (if v' = 0.0 or Decimal.is_nan v' then false else true))
      | ATDecimal ->
	  (new atomicDecimal ~ta:at (Decimal._cast_double_to_decimal v'))
      | ATFloat -> 
	  (new atomicFloat ~ta:at (Decimal._cast_double_to_float v'))
      | ATDouble -> 
	  (new atomicDouble ~ta:at v') (* self *)
      | ATInteger -> 
	  (new atomicInteger ~ta:at (Decimal._cast_float_to_integer v'))
      | ATDuration ->
	  raise (Query (Prototype "Cast to a Duration not supported"))
      | ATDateTime -> 
	  raise (Query (Cast_Error "Cannot cast a Double value to a DateTime"))
      | ATTime ->
	  raise (Query (Prototype "Cast to a Time not supported"))
      | ATDate ->
	  raise (Query (Prototype "Cast to a Date not supported"))
      | ATGYearMonth ->
	  raise (Query (Prototype "Cast to a GYearMonth not supported"))
      | ATGYear ->
	  raise (Query (Prototype "Cast to a GYear not supported"))
      | ATGMonthDay ->
	  raise (Query (Prototype "Cast to a GMonthDay not supported"))
      | ATGDay ->
	  raise (Query (Prototype "Cast to a GDay not supported"))
      | ATGMonth ->
	  raise (Query (Prototype "Cast to a GMonth not supported"))
      | ATHexBinary ->
	  raise (Query (Cast_Error "Cannot cast to a HexBinary"))
      | ATBase64Binary ->
	  raise (Query (Cast_Error "Cannot cast to a Base64Binary"))
      | ATAnyURI ->
	  raise (Query (Cast_Error "Cannot cast a Double value to a AnyURI"))
      | ATQName -> 
	  raise (Query (Cast_Error "Cannot cast a Double value to a QName"))
      | ATNOTATION -> 
	  raise (Query (Prototype "Cast to a NOTATION not supported"))
      | ATYearMonthDuration ->
	  raise (Query (Prototype "Cast to a yearMonthDuration not supported"))
      | ATDayTimeDuration ->
	  raise (Query (Prototype "Cast to a dateTimeDuration not supported"))
      | ATUntypedAtomic -> 
	  (* NB: Decimal._string_of_float implements lexical rules from F&O 17.1.2 *)
	  (new atomicUntyped ~ta:at (Decimal._string_of_float v'))
      | ATAnyAtomic -> (self :> atomicDouble)
  end

and atomicInteger ?ta:type_annotation v = 
  let ta =
    match type_annotation with
    | None -> Namespace_symbols_builtin.xs_integer
    | Some x -> x
  in
  object (self)
    inherit atomicValue ta
    val v' = v

    method erase_atomic_value () = _string_of_integer v'
    method getAtomicValueKind () = ATInteger
    method getAtomicInteger () = v'
    method private eq a = 
	  _integer_eq v' (a#getAtomicInteger())
    method private lteq a = 
	  _integer_le v' (a#getAtomicInteger())
    method private lt a = 
	  _integer_lt v' (a#getAtomicInteger())
    method private gteq a = 
	  _integer_ge v' (a#getAtomicInteger())
    method private gt a = 
	  _integer_gt v' (a#getAtomicInteger())
    method cast_to nsenv at bt = 
      match bt with 
      | ATString ->
	  ((new atomicString ~ta:at (_string_of_integer v')) :> atomicValue)
      | ATBoolean -> 
	  (new atomicBoolean ~ta:at (if Decimal._integer_eq v' _integer_zero then false else true))
      | ATDecimal -> 
	  (new atomicDecimal ~ta:at (Decimal._decimal_of_integer v'))
      | ATFloat -> 
	  (new atomicFloat ~ta:at (float_of_untyped (Decimal._string_of_integer v')))
      | ATDouble -> 
	  (new atomicDouble ~ta:at (double_of_untyped (Decimal._string_of_integer v')))
      | ATInteger -> 
	  (new atomicInteger ~ta:at v') (* self *)
      | ATDuration ->
	  raise (Query (Prototype "Cast to a Duration not supported"))
      | ATDateTime -> 
	  raise (Query (Cast_Error "Cannot cast a Integer value to a DateTime"))
      | ATTime ->
	  raise (Query (Prototype "Cast to a Time not supported"))
      | ATDate ->
	  raise (Query (Prototype "Cast to a Date not supported"))
      | ATGYearMonth ->
	  raise (Query (Prototype "Cast to a GYearMonth not supported"))
      | ATGYear ->
	  raise (Query (Prototype "Cast to a GYear not supported"))
      | ATGMonthDay ->
	  raise (Query (Prototype "Cast to a GMonthDay not supported"))
      | ATGDay ->
	  raise (Query (Prototype "Cast to a GDay not supported"))
      | ATGMonth ->
	  raise (Query (Prototype "Cast to a GMonth not supported"))
      | ATHexBinary ->
	  raise (Query (Cast_Error "Cannot cast to a HexBinary"))
      | ATBase64Binary ->
	  raise (Query (Cast_Error "Cannot cast to a Base64Binary"))
      | ATAnyURI ->
	  raise (Query (Cast_Error "Cannot cast a Integer value to a AnyURI"))
      | ATQName -> 
	  raise (Query (Cast_Error "Cannot cast a Integer value to a QName"))
      | ATNOTATION -> 
	  raise (Query (Prototype "Cast to a NOTATION not supported"))
      | ATYearMonthDuration ->
	  raise (Query (Prototype "Cast to a yearMonthDuration not supported"))
      | ATDayTimeDuration ->
	  raise (Query (Prototype "Cast to a dateTimeDuration not supported"))
      | ATUntypedAtomic -> 
	  (new atomicUntyped ~ta:at (_string_of_integer v'))
      | ATAnyAtomic -> (self :> atomicInteger)
  end

and atomicDuration ?ta:type_annotation v = 
  let ta =
    match type_annotation with
    | None -> Namespace_symbols_builtin.xs_duration
    | Some x -> x
  in
  object (self)
    inherit atomicValue ta

    val v' = v 

    method erase_atomic_value () = DateTime.string_of_duration v'
    method getAtomicValueKind () = ATDuration
    method getAtomicDuration () = v'
    method private eq a   = duration_equal v' (a#getAtomicDuration())
    method private lteq a = duration_lteq v' (a#getAtomicDuration())
    method private lt a   = duration_lt v' (a#getAtomicDuration())
    method private gteq a = duration_gteq v' (a#getAtomicDuration())
    method private gt a   = duration_gt v' (a#getAtomicDuration())
    method cast_to nsenv at bt = 
      match bt with 
      | ATString ->
	  ((new atomicString ~ta:at (DateTime.string_of_duration v')) :> atomicValue)
      | ATBoolean -> 
	  raise (Query (Cast_Error "Cannot cast a Duration value to a Boolean"))
      | ATDecimal -> 
	  raise (Query (Cast_Error "Cannot cast a Duration value to a Decimal"))
      | ATFloat -> 
	  raise (Query (Cast_Error "Cannot cast a Duration value to a Float"))
      | ATDouble -> 
	  raise (Query (Cast_Error "Cannot cast a Duration value to a Double"))
      | ATInteger ->
	  raise (Query (Cast_Error "Cannot cast a Duration value to an Integer"))
      | ATDuration ->
	  (new atomicDuration ~ta:at v') (* self *)
      | ATDateTime ->
	  raise (Query (Cast_Error "Cannot cast a Duration value to a DateTime"))
      | ATTime ->
	  raise (Query (Prototype "Cast to a Time not supported"))
      | ATDate ->
	  raise (Query (Prototype "Cast to a Date not supported"))
      | ATGYearMonth ->
	  raise (Query (Prototype "Cast to a GYearMonth not supported"))
      | ATGYear ->
	  raise (Query (Prototype "Cast to a GYear not supported"))
      | ATGMonthDay ->
	  raise (Query (Prototype "Cast to a GMonthDay not supported"))
      | ATGDay ->
	  raise (Query (Prototype "Cast to a GDay not supported"))
      | ATGMonth ->
	  raise (Query (Prototype "Cast to a GMonth not supported"))
      | ATHexBinary ->
	  raise (Query (Cast_Error "Cannot cast to a HexBinary"))
      | ATBase64Binary ->
	  raise (Query (Cast_Error "Cannot cast to a Base64Binary"))
      | ATAnyURI ->
	  raise (Query (Cast_Error "Cannot cast a Duration value to a AnyURI"))
      | ATQName -> 
	  raise (Query (Cast_Error "Cannot cast a Duration value to a QName"))
      | ATNOTATION -> 
	  raise (Query (Prototype "Cast to a NOTATION not supported"))
      | ATYearMonthDuration ->
	  (new atomicYearMonthDuration ~ta:at (DateTime.yearMonthDuration_of_duration v'))
      | ATDayTimeDuration ->
	  (new atomicDayTimeDuration ~ta:at (DateTime.dayTimeDuration_of_duration v'))
      | ATUntypedAtomic -> 
	  ((new atomicUntyped ~ta:at (DateTime.string_of_duration v')) :> atomicValue)
      | ATAnyAtomic -> (self :> atomicDuration)
  end

and atomicDateTime ?ta:type_annotation v = 
  let ta =
    match type_annotation with
    | None -> Namespace_symbols_builtin.xs_dateTime
    | Some x -> x
  in
  object (self)
    inherit atomicValue ta
    val v' = v 

    method erase_atomic_value () = DateTime.string_of_dateTime v'
    method getAtomicValueKind () = ATDateTime
    method getAtomicDateTime () = v'
    method private eq a = 
	  dateTime_equal None v' (a#getAtomicDateTime())
    method private eq a = 
	  dateTime_equal None v' (a#getAtomicDateTime())
    method private lteq a = 
	  dateTime_lteq None v' (a#getAtomicDateTime())
    method private lt a = 
	  dateTime_lt None v' (a#getAtomicDateTime())
    method private gteq a = 
	  dateTime_gteq None v' (a#getAtomicDateTime())
    method private gt a = 
	  dateTime_gt None v' (a#getAtomicDateTime())
    method cast_to nsenv at bt = 
      match bt with 
      | ATString ->
	  ((new atomicString ~ta:at (DateTime.string_of_dateTime v')) :> atomicValue)
      | ATBoolean -> 
	  raise (Query (Cast_Error "Cannot cast a DateTime value to a Boolean"))
      | ATDecimal -> 
	  raise (Query (Cast_Error "Cannot cast a DateTime value to a Decimal"))
      | ATFloat -> 
	  raise (Query (Cast_Error "Cannot cast a DateTime value to a Float"))
      | ATDouble -> 
	  raise (Query (Cast_Error "Cannot cast a DateTime value to a Double"))
      | ATInteger ->
	  raise (Query (Cast_Error "Cannot cast a DateTime value to an Integer"))
      | ATDuration ->
	  raise (Query (Cast_Error "Cannot cast a DateTime value to a Duration"))
      | ATDateTime -> 
	  (new atomicDateTime ~ta:at v') (* self *)
      | ATTime ->
	  (new atomicTime ~ta:at (DateTime.time_from_dateTime v'))
      | ATDate ->
	  (new atomicDate ~ta:at (DateTime.date_from_dateTime v'))

      (*
	 When a value of any primitive type is cast as xs:gYearMonth,
	 the xs:gYearMonth value TV is derived from ST and SV as
	 follows:
	 *  If ST is xs:dateTime, then 
	 let SYR be eg:convertYearToString( fn:year-from-dateTime( SV )), 
	 let SMO be eg:convertTo2CharString( fn:month-from-dateTime( SV )) and 
	 let STZ be eg:convertTZtoString( fn:timezone-from-dateTime( SV )); 
	 TV is xs:gYearMonth( fn:concat( SYR , '-', SMO, STZ ) ).
      *)
      | ATGYearMonth ->
	  let syr = DateTime.year_from_dateTime v' in 
	  let smo = DateTime.month_from_dateTime v' in 
	  let ostz = DateTime.opt_timezone_from_dateTime v' in 
	  (new atomicGYearMonth ~ta:at (mkgYearMonth(syr,smo,ostz)))
      | ATGYear ->
      (* 

	 When a value of any primitive type is cast as xs:gYear, the
	 xs:gYear value TV is derived from ST and SV as follows:

	 * If ST is xs:dateTime, 
	 let SYR be eg:convertYearToString( fn:year-from-dateTime( SV )) and 
	 let STZ be eg:convertTZtoString( fn:timezone-from-dateTime( SV )); 
	 TV is xs:gYear(fn:concat( SYR, STZ )).
	 *)
	  let syr = DateTime.year_from_dateTime v' in 
	  let ostz = DateTime.opt_timezone_from_dateTime v' in 
	  (new atomicGYear ~ta:at (mkgYear(syr,ostz)))
      | ATGMonthDay ->
	  (*

	     When a value of any primitive type is cast as
	     xs:gMonthDay, the xs:gMonthDay value TV is derived from
	     ST and SV as follows:

	     *  If ST is xs:dateTime, then 
	     let SMO be eg:convertTo2CharString( fn:month-from-dateTime( SV )), 
	     let SDA be eg:convertTo2CharString( fn:day-from-dateTime( SV )) and 
	     let STZ be eg:convertTZtoString( fn:timezone-from-dateTime( SV )); 
	     TV is xs:gYearMonth( fn:concat( '--', SMO '-', SDA, STZ ) ).

	  *)
	  let smo = DateTime.month_from_dateTime v' in 
	  let sda = DateTime.day_from_dateTime v' in 
	  let ostz = DateTime.opt_timezone_from_dateTime v' in 
	  (new atomicGMonthDay ~ta:at (mkgMonthDay(smo,sda,ostz)))
      | ATGDay ->
      (*

	 When a value of any primitive type is cast as xs:gDay, the
	 xs:gDay value TV is derived from ST and SV as follows:

	 * If ST is xs:dateTime, then 
	 let SDA be eg:convertTo2CharString( fn:day-from-dateTime( SV )) and 
	 let STZ be eg:convertTZtoString( fn:timezone-from-dateTime( SV )); 
	 TV is xs:gDay( fn:concat( '---', SDA, STZ )).

      *)
	  let sda = DateTime.day_from_dateTime v' in 
	  let ostz = DateTime.opt_timezone_from_dateTime v' in 
	  (new atomicGDay ~ta:at (mkgDay(sda,ostz)))
      | ATGMonth ->
      (* 

	 When a value of any primitive type is cast as xs:gMonth, the
	 xs:gMonth value TV is derived from ST and SV as follows:

         * If ST is xs:dateTime, then 
	 let SMO be eg:convertTo2CharString( fn:month-from-dateTime( SV )) and 
	 let STZ be eg:convertTZtoString( fn:timezone-from-dateTime( SV )); 
	 TV is xs:gMonth( fn:concat( '--' , SMO, STZ )).

      *)
	  let smo = DateTime.month_from_dateTime v' in 
	  let ostz = DateTime.opt_timezone_from_dateTime v' in 
	  (new atomicGMonth ~ta:at (mkgMonth(smo,ostz)))
      | ATHexBinary ->
	  raise (Query (Cast_Error "Cannot cast to a HexBinary"))
      | ATBase64Binary ->
	  raise (Query (Cast_Error "Cannot cast to a Base64Binary"))
      | ATAnyURI ->
	  raise (Query (Cast_Error "Cannot cast a DateTime value to a AnyURI"))
      | ATQName -> 
	  raise (Query (Cast_Error "Cannot cast a DateTime value to a QName"))
      | ATNOTATION -> 
	  raise (Query (Prototype "Cast to a NOTATION not supported"))
      | ATYearMonthDuration ->
	  raise (Query (Prototype "Cast to a yearMonthDuration not supported"))
      | ATDayTimeDuration ->
	  raise (Query (Prototype "Cast to a dateTimeDuration not supported"))
      | ATUntypedAtomic -> 
	  (new atomicUntyped ~ta:at (DateTime.string_of_dateTime v'))
      | ATAnyAtomic -> (self :> atomicDateTime)
  end

and atomicTime ?ta:type_annotation v = 
  let ta =
    match type_annotation with
    | None -> Namespace_symbols_builtin.xs_time
    | Some x -> x
  in
  object (self)
    inherit atomicValue ta
    val v' = v 

    method erase_atomic_value () = DateTime.string_of_time v'
    method getAtomicValueKind () = ATTime
    method getAtomicTime () = v'
    method private eq a = 
	  time_equal None v' (a#getAtomicTime())
    method private eq a = 
	  time_equal None v' (a#getAtomicTime())
    method private lteq a = 
	  time_lteq None v' (a#getAtomicTime())
    method private lt a = 
	  time_lt None v' (a#getAtomicTime())
    method private gteq a = 
	  time_gteq None v' (a#getAtomicTime())
    method private gt a = 
	  time_gt None v' (a#getAtomicTime())
    method cast_to nsenv at bt = 
      match bt with 
      | ATString ->
	  ((new atomicString ~ta:at (self#erase_atomic_value())) :> atomicValue)
      | ATBoolean -> 
	  raise (Query (Cast_Error "Cannot cast a Time value to a Boolean"))
      | ATDecimal -> 
	  raise (Query (Cast_Error "Cannot cast a Time value to a Decimal"))
      | ATFloat -> 
	  raise (Query (Cast_Error "Cannot cast a Time value to a Float"))
      | ATDouble -> 
	  raise (Query (Cast_Error "Cannot cast a Time value to a Double"))
      | ATInteger ->
	  raise (Query (Cast_Error "Cannot cast a Time value to an Integer"))
      | ATDuration ->
	  raise (Query (Cast_Error "Cannot cast a Time value to an Duration"))
      | ATDateTime -> 
	  raise (Query (Cast_Error "Cannot cast a Time value to an DateTime"))
      | ATTime ->
	  (new atomicTime ~ta:at v') (* self *)
      | ATDate ->
	  raise (Query (Prototype "Cast to a Date not supported"))
      | ATGYearMonth ->
	  raise (Query (Prototype "Cast to a GYearMonth not supported"))
      | ATGYear ->
	  raise (Query (Prototype "Cast to a GYear not supported"))
      | ATGMonthDay ->
	  raise (Query (Prototype "Cast to a GMonthDay not supported"))
      | ATGDay ->
	  raise (Query (Prototype "Cast to a GDay not supported"))
      | ATGMonth ->
	  raise (Query (Prototype "Cast to a GMonth not supported"))
      | ATHexBinary ->
	  raise (Query (Cast_Error "Cannot cast to a HexBinary"))
      | ATBase64Binary ->
	  raise (Query (Cast_Error "Cannot cast to a Base64Binary"))
      | ATAnyURI ->
	  raise (Query (Cast_Error "Cannot cast a Time value to a AnyURI"))
      | ATQName -> 
	  raise (Query (Cast_Error "Cannot cast a Time value to a QName"))
      | ATNOTATION -> 
	  raise (Query (Prototype "Cast to a NOTATION not supported"))
      | ATYearMonthDuration ->
	  raise (Query (Prototype "Cast to a yearMonthDuration not supported"))
      | ATDayTimeDuration ->
	  raise (Query (Prototype "Cast to a dateTimeDuration not supported"))
      | ATUntypedAtomic -> 
	  (new atomicUntyped ~ta:at (self#erase_atomic_value()))
      | ATAnyAtomic -> (self :> atomicTime)
  end

and atomicDate ?ta:type_annotation v = 
  let ta =
    match type_annotation with
    | None -> Namespace_symbols_builtin.xs_date
    | Some x -> x
  in
  object(self)
    inherit atomicValue ta
    val v' = v 

    method erase_atomic_value () = DateTime.string_of_date v'
    method getAtomicValueKind () = ATDate
    method getAtomicDate () = v'
    method private eq a = 
	  date_equal None v' (a#getAtomicDate())
    method private lteq a = 
	  date_lteq None v' (a#getAtomicDate())
    method private lt a = 
	  date_lt None v' (a#getAtomicDate())
    method private gteq a = 
	  date_gteq None v' (a#getAtomicDate())
    method private gt a = 
	  date_gt None v' (a#getAtomicDate())
    method cast_to nsenv at bt = 
      match bt with 
      | ATString ->
	  ((new atomicString ~ta:at (DateTime.string_of_date v')) :> atomicValue)
      | ATBoolean -> 
	  raise (Query (Cast_Error "Cannot cast a Time value to a Boolean"))
      | ATDecimal -> 
	  raise (Query (Cast_Error "Cannot cast a Time value to a Decimal"))
      | ATFloat -> 
	  raise (Query (Cast_Error "Cannot cast a Time value to a Float"))
      | ATDouble -> 
	  raise (Query (Cast_Error "Cannot cast a Time value to a Double"))
      | ATInteger ->
	  raise (Query (Cast_Error "Cannot cast a Time value to an Integer"))
      | ATDuration ->
	  raise (Query (Cast_Error "Cannot cast a Time value to an Duration"))
      | ATDateTime -> 
	  let zero_time = DateTime.mktime(0,0,Decimal._decimal_zero,None) in 
	  (new atomicDateTime ~ta:at (DateTime.mkdateTime(v', zero_time, (DateTime.opt_timezone_from_date v'))))
      | ATTime ->
	  raise (Query (Prototype "Cast to a Time not supported"))
      | ATDate ->
	  (new atomicDate ~ta:at v') (* self *)
      | ATGYearMonth ->
      (*
	 When a value of any primitive type is cast as xs:gYearMonth, the
	 xs:gYearMonth value TV is derived from ST and SV as follows:

	 *  If ST is xs:date, then 
	 let SYR be eg:convertYearToString( fn:year-from-date( SV )), 
	 let SMO be eg:convertTo2CharString( fn:month-from-date( SV )) and 
	 let STZ be eg:convertTZtoString( fn:timezone-from-date( SV )); 
	 TV is xs:gYearMonth( fn:concat( SYR , '-', SMO, STZ ) ).
      *)
	  let syr = DateTime.year_from_date v' in 
	  let smo = DateTime.month_from_date v' in 
	  let ostz = DateTime.opt_timezone_from_date v' in 
	  (new atomicGYearMonth ~ta:at (mkgYearMonth(syr,smo,ostz)))
      | ATGYear ->
      (* 

	 When a value of any primitive type is cast as xs:gYear, the
	 xs:gYear value TV is derived from ST and SV as follows:

	 * If ST is xs:date, 
	 let SYR be eg:convertYearToString( fn:year-from-date( SV )); and 
	 let STZ be eg:convertTZtoString( fn:timezone-from-date( SV )); 
	 TV is xs:gYear(fn:concat( SYR, STZ )).
	 *)
	  let syr = DateTime.year_from_date v' in 
	  let ostz = DateTime.opt_timezone_from_date v' in 
	  (new atomicGYear ~ta:at (mkgYear(syr,ostz)))
      | ATGMonthDay ->
      (*

	 When a value of any primitive type is cast as xs:gMonthDay,
	 the xs:gMonthDay value TV is derived from ST and SV as
	 follows:

	 * If ST is xs:date, then 
	 let SMO be eg:convertTo2CharString(fn:month-from-date( SV )), 
	 let SDA be eg:convertTo2CharString( fn:day-from-date( SV )) and 
	 let STZ be eg:convertTZtoString( fn:timezone-from-date( SV )); 
	 TV is xs:gMonthDay( fn:concat( '--', SMO , '-', SDA, STZ ) ).
      *)
	  let smo = DateTime.month_from_date v' in 
	  let sda = DateTime.day_from_date v' in 
	  let ostz = DateTime.opt_timezone_from_date v' in 
	  (new atomicGMonthDay ~ta:at (mkgMonthDay(smo,sda,ostz)))
      | ATGDay ->
      (*

	 When a value of any primitive type is cast as xs:gDay, the
	 xs:gDay value TV is derived from ST and SV as follows:

	 * If ST is xs:date, then 
	 let SDA be eg:convertTo2CharString( fn:day-from-date( SV )) and 
	 let STZ be eg:convertTZtoString( fn:timezone-from-date( SV )); 
	 TV is xs:gDay( fn:concat( '---', SDA, STZ )).
      *)
	  let sda = DateTime.day_from_date v' in 
	  let ostz = DateTime.opt_timezone_from_date v' in 
	  (new atomicGDay ~ta:at (mkgDay(sda,ostz)))
      | ATGMonth ->
      (* 

	 When a value of any primitive type is cast as xs:gMonth, the
	 xs:gMonth value TV is derived from ST and SV as follows:

         * If ST is xs:date, then 
	 let SMO be eg:convertTo2CharString( fn:month-from-date( SV )) and 
	 let STZ be eg:convertTZtoString( fn:timezone-from-date( SV )); 
	 TV is xs:gMonth( fn:concat( '--', SMO, STZ )).
      *)
	  let smo = DateTime.month_from_date v' in 
	  let ostz = DateTime.opt_timezone_from_date v' in 
	  (new atomicGMonth ~ta:at (mkgMonth(smo,ostz)))
      | ATHexBinary ->
	  raise (Query (Cast_Error "Cannot cast to a HexBinary"))
      | ATBase64Binary ->
	  raise (Query (Cast_Error "Cannot cast to a Base64Binary"))
      | ATAnyURI ->
	  raise (Query (Cast_Error "Cannot cast a Time value to a AnyURI"))
      | ATQName -> 
	  raise (Query (Cast_Error "Cannot cast a Time value to a QName"))
      | ATNOTATION -> 
	  raise (Query (Prototype "Cast to a NOTATION not supported"))
      | ATYearMonthDuration ->
	  raise (Query (Prototype "Cast to a yearMonthDuration not supported"))
      | ATDayTimeDuration ->
	  raise (Query (Prototype "Cast to a dateTimeDuration not supported"))
      | ATUntypedAtomic -> 
	  (new atomicUntyped ~ta:at (DateTime.string_of_date v'))
      | ATAnyAtomic -> (self :> atomicDate)
  end

and atomicGYearMonth ?ta:type_annotation v = 
  let ta =
    match type_annotation with
    | None -> Namespace_symbols_builtin.xs_gYearMonth
    | Some x -> x
  in
  object(self)
    inherit atomicValue ta
    val v' = v 

    method erase_atomic_value () = DateTime.string_of_gYearMonth v'
    method getAtomicValueKind () = ATGYearMonth
    method getAtomicGYearMonth () = v'
    method private eq a = 
	  gYearMonth_equal None v' (a#getAtomicGYearMonth())
    method private lteq a = 
	  gYearMonth_lteq None v' (a#getAtomicGYearMonth())
    method private lt a = 
	  gYearMonth_lteq None v' (a#getAtomicGYearMonth())
    method private gteq a = 
	  gYearMonth_gteq None v' (a#getAtomicGYearMonth())
    method private gt a = 
	  gYearMonth_gt None v' (a#getAtomicGYearMonth())
    method cast_to nsenv at bt = 
      match bt with 
      | ATString ->
	  (new atomicString ~ta:at (DateTime.string_of_gYearMonth v') :> atomicValue)
      | ATBoolean -> 
	  raise (Query (Cast_Error "Cannot cast a GYearMonth value to a Boolean"))
      | ATDecimal -> 
	  raise (Query (Cast_Error "Cannot cast a GYearMonth value to a Decimal"))
      | ATFloat -> 
	  raise (Query (Cast_Error "Cannot cast a GYearMonth value to a Float"))
      | ATDouble -> 
	  raise (Query (Cast_Error "Cannot cast a GYearMonth value to a Double"))
      | ATInteger ->
	  raise (Query (Cast_Error "Cannot cast a GYearMonth value to an Integer"))
      | ATDuration ->
	  raise (Query (Cast_Error "Cannot cast a GYearMonth value to an Duration"))
      | ATDateTime -> 
	  raise (Query (Cast_Error "Cannot cast a GYearMonth value to an DateTime"))
      | ATTime ->
	  raise (Query (Cast_Error "Cannot cast a GYearMonth value to an Time"))
      | ATDate ->
	  raise (Query (Prototype "Cast to a Date not supported"))
      | ATGYearMonth ->
	  (new atomicGYearMonth ~ta:at v') (* self *)
      | ATGYear ->
	  raise (Query (Prototype "Cast to a GYear not supported"))
      | ATGMonthDay ->
	  raise (Query (Prototype "Cast to a GMonthDay not supported"))
      | ATGDay ->
	  raise (Query (Prototype "Cast to a GDay not supported"))
      | ATGMonth ->
	  raise (Query (Prototype "Cast to a GMonth not supported"))
      | ATHexBinary ->
	  raise (Query (Cast_Error "Cannot cast to a HexBinary"))
      | ATBase64Binary ->
	  raise (Query (Cast_Error "Cannot cast to a Base64Binary"))
      | ATAnyURI ->
	  raise (Query (Cast_Error "Cannot cast a GYearMonth value to a AnyURI"))
      | ATQName -> 
	  raise (Query (Cast_Error "Cannot cast a GYearMonth value to a QName"))
      | ATNOTATION -> 
	  raise (Query (Prototype "Cast to a NOTATION not supported"))
      | ATYearMonthDuration ->
	  raise (Query (Prototype "Cast to a yearMonthDuration not supported"))
      | ATDayTimeDuration ->
	  raise (Query (Prototype "Cast to a dateTimeDuration not supported"))
      | ATUntypedAtomic -> 
	  (new atomicUntyped ~ta:at (DateTime.string_of_gYearMonth v'))
      | ATAnyAtomic -> (self :> atomicGYearMonth)
  end

and atomicGYear ?ta:type_annotation v = 
  let ta =
    match type_annotation with
    | None -> Namespace_symbols_builtin.xs_gYear
    | Some x -> x
  in
  object (self)
    inherit atomicValue ta
    val v' = v 

    method erase_atomic_value () = DateTime.string_of_gYear v'
    method getAtomicValueKind () = ATGYear
    method getAtomicGYear () = v'
    method private eq a = 
	  gYear_equal None v' (a#getAtomicGYear())
    method private lteq a = 
	  gYear_lteq None v' (a#getAtomicGYear())
    method private lt a = 
	  gYear_lt None v' (a#getAtomicGYear())
    method private gteq a = 
	  gYear_gteq None v' (a#getAtomicGYear())
    method private gt a = 
	  gYear_gt None v' (a#getAtomicGYear())
    method cast_to nsenv at bt = 
      match bt with 
      | ATString ->
	  ((new atomicString ~ta:at (DateTime.string_of_gYear v')) :> atomicValue)
      | ATBoolean -> 
	  raise (Query (Cast_Error "Cannot cast a GYear value to a Boolean"))
      | ATDecimal -> 
	  raise (Query (Cast_Error "Cannot cast a GYear value to a Decimal"))
      | ATFloat -> 
	  raise (Query (Cast_Error "Cannot cast a GYear value to a Float"))
      | ATDouble -> 
	  raise (Query (Cast_Error "Cannot cast a GYear value to a Double"))
      | ATInteger ->
	  raise (Query (Cast_Error "Cannot cast a GYear value to an Integer"))
      | ATDuration ->
	  raise (Query (Cast_Error "Cannot cast a GYear value to an Duration"))
      | ATDateTime -> 
	  raise (Query (Cast_Error "Cannot cast a GYear value to an DateTime"))
      | ATTime ->
	  raise (Query (Cast_Error "Cannot cast a GYear value to an Time"))
      | ATDate ->
	  raise (Query (Prototype "Cast to a Date not supported"))
      | ATGYearMonth ->
	  raise (Query (Cast_Error "Cannot cast a GYear value to an GYearMonth"))
      | ATGYear ->
	  (new atomicGYear ~ta:at v') (* self *)
      | ATGMonthDay ->
	  raise (Query (Prototype "Cast to a GMonthDay not supported"))
      | ATGDay ->
	  raise (Query (Prototype "Cast to a GDay not supported"))
      | ATGMonth ->
	  raise (Query (Prototype "Cast to a GMonth not supported"))
      | ATHexBinary ->
	  raise (Query (Cast_Error "Cannot cast to a HexBinary"))
      | ATBase64Binary ->
	  raise (Query (Cast_Error "Cannot cast to a Base64Binary"))
      | ATAnyURI ->
	  raise (Query (Cast_Error "Cannot cast a GYear value to a AnyURI"))
      | ATQName -> 
	  raise (Query (Cast_Error "Cannot cast a GYear value to a QName"))
      | ATNOTATION -> 
	  raise (Query (Prototype "Cast to a NOTATION not supported"))
      | ATYearMonthDuration ->
	  raise (Query (Prototype "Cast to a yearMonthDuration not supported"))
      | ATDayTimeDuration ->
	  raise (Query (Prototype "Cast to a dateTimeDuration not supported"))
      | ATUntypedAtomic -> 
	  (new atomicUntyped ~ta:at (DateTime.string_of_gYear v'))
      | ATAnyAtomic -> (self :> atomicGYear)
  end

and atomicGMonthDay ?ta:type_annotation v = 
  let ta =
    match type_annotation with
    | None -> Namespace_symbols_builtin.xs_gMonthDay
    | Some x -> x
  in
  object(self)
    inherit atomicValue ta
    val v' = v 

    method erase_atomic_value () = DateTime.string_of_gMonthDay v'
    method getAtomicValueKind () = ATGMonthDay
    method getAtomicGMonthDay () = v'
    method private eq a = 
	  gMonthDay_equal None v' (a#getAtomicGMonthDay())
    method private lteq a = 
	  gMonthDay_lteq None v' (a#getAtomicGMonthDay())
    method private lt a = 
	  gMonthDay_lt None v' (a#getAtomicGMonthDay())
    method private gteq a = 
	  gMonthDay_gteq None v' (a#getAtomicGMonthDay())
    method private gt a = 
	  gMonthDay_gt None v' (a#getAtomicGMonthDay())
    method cast_to nsenv at bt = 
      match bt with 
      | ATString ->
	  ((new atomicString ~ta:at (DateTime.string_of_gMonthDay v')) :> atomicValue)
      | ATBoolean -> 
	  raise (Query (Cast_Error "Cannot cast a GMonthDay value to a Boolean"))
      | ATDecimal -> 
	  raise (Query (Cast_Error "Cannot cast a GMonthDay value to a Decimal"))
      | ATFloat -> 
	  raise (Query (Cast_Error "Cannot cast a GMonthDay value to a Float"))
      | ATDouble -> 
	  raise (Query (Cast_Error "Cannot cast a GMonthDay value to a Double"))
      | ATInteger ->
	  raise (Query (Cast_Error "Cannot cast a GMonthDay value to an Integer"))
      | ATDuration ->
	  raise (Query (Cast_Error "Cannot cast a GMonthDay value to an Duration"))
      | ATDateTime -> 
	  raise (Query (Cast_Error "Cannot cast a GMonthDay value to an DateTime"))
      | ATTime ->
	  raise (Query (Cast_Error "Cannot cast a GMonthDay value to an Time"))
      | ATDate ->
	  raise (Query (Prototype "Cast to a Date not supported"))
      | ATGYearMonth ->
	  raise (Query (Cast_Error "Cannot cast a GMonthDay value to an GYearMonth"))
      | ATGYear ->
	  raise (Query (Cast_Error "Cannot cast a GMonthDay value to an GYear"))
      | ATGMonthDay ->
	  (new atomicGMonthDay ~ta:at v') (* self *)
      | ATGDay ->
	  raise (Query (Prototype "Cast to a GDay not supported"))
      | ATGMonth ->
	  raise (Query (Prototype "Cast to a GMonth not supported"))
      | ATHexBinary ->
	  raise (Query (Cast_Error "Cannot cast to a HexBinary"))
      | ATBase64Binary ->
	  raise (Query (Cast_Error "Cannot cast to a Base64Binary"))
      | ATAnyURI ->
	  raise (Query (Cast_Error "Cannot cast a GMonthDay value to a AnyURI"))
      | ATQName -> 
	  raise (Query (Cast_Error "Cannot cast a GMonthDay value to a QName"))
      | ATNOTATION -> 
	  raise (Query (Prototype "Cast to a NOTATION not supported"))
      | ATYearMonthDuration ->
	  raise (Query (Prototype "Cast to a yearMonthDuration not supported"))
      | ATDayTimeDuration ->
	  raise (Query (Prototype "Cast to a dateTimeDuration not supported"))
      | ATUntypedAtomic -> 
	  (new atomicUntyped ~ta:at (DateTime.string_of_gMonthDay v'))
      | ATAnyAtomic -> (self :> atomicGMonthDay)
  end

and atomicGDay ?ta:type_annotation v = 
  let ta =
    match type_annotation with
    | None -> Namespace_symbols_builtin.xs_gDay
    | Some x -> x
  in
  object (self)
    inherit atomicValue ta
    val v' = v 

    method erase_atomic_value () = DateTime.string_of_gDay v'
    method getAtomicValueKind () = ATGDay
    method getAtomicGDay () = v'
    method private eq a = 
	  gDay_equal None v' (a#getAtomicGDay())
    method private lteq a = 
	  gDay_lteq None v' (a#getAtomicGDay())
    method private lt a = 
	  gDay_lt None v' (a#getAtomicGDay())
    method private gteq a = 
	  gDay_gteq None v' (a#getAtomicGDay())
    method private gt a = 
	  gDay_gt None v' (a#getAtomicGDay())
    method cast_to nsenv at bt = 
      match bt with 
      | ATString ->
	  ((new atomicString ~ta:at (DateTime.string_of_gDay v')) :> atomicValue)
      | ATBoolean -> 
	  raise (Query (Cast_Error "Cannot cast a GDay value to a Boolean"))
      | ATDecimal -> 
	  raise (Query (Cast_Error "Cannot cast a GDay value to a Decimal"))
      | ATFloat -> 
	  raise (Query (Cast_Error "Cannot cast a GDay value to a Float"))
      | ATDouble -> 
	  raise (Query (Cast_Error "Cannot cast a GDay value to a Double"))
      | ATInteger ->
	  raise (Query (Cast_Error "Cannot cast a GDay value to an Integer"))
      | ATDuration ->
	  raise (Query (Cast_Error "Cannot cast a GDay value to an Duration"))
      | ATDateTime -> 
	  raise (Query (Cast_Error "Cannot cast a GDay value to an DateTime"))
      | ATTime ->
	  raise (Query (Cast_Error "Cannot cast a GDay value to an Time"))
      | ATDate ->
	  raise (Query (Prototype "Cast to a Date not supported"))
      | ATGYearMonth ->
	  raise (Query (Cast_Error "Cannot cast a GDay value to an GYearMonth"))
      | ATGYear ->
	  raise (Query (Cast_Error "Cannot cast a GDay value to an GYear"))
      | ATGMonthDay ->
	  raise (Query (Cast_Error "Cannot cast a GDay value to an GMonthDay"))
      | ATGDay ->
	  (new atomicGDay ~ta:at v') (* self *)
      | ATGMonth ->
	  raise (Query (Prototype "Cast to a GMonth not supported"))
      | ATHexBinary ->
	  raise (Query (Cast_Error "Cannot cast to a HexBinary"))
      | ATBase64Binary ->
	  raise (Query (Cast_Error "Cannot cast to a Base64Binary"))
      | ATAnyURI ->
	  raise (Query (Cast_Error "Cannot cast a GDay value to a AnyURI"))
      | ATQName -> 
	  raise (Query (Cast_Error "Cannot cast a GDay value to a QName"))
      | ATNOTATION -> 
	  raise (Query (Prototype "Cast to a NOTATION not supported"))
      | ATYearMonthDuration ->
	  raise (Query (Prototype "Cast to a yearMonthDuration not supported"))
      | ATDayTimeDuration ->
	  raise (Query (Prototype "Cast to a dateTimeDuration not supported"))
      | ATUntypedAtomic -> 
	  (new atomicUntyped ~ta:at (DateTime.string_of_gDay v'))
      | ATAnyAtomic -> (self :> atomicGDay)
  end

and atomicGMonth ?ta:type_annotation v = 
  let ta =
    match type_annotation with
    | None -> Namespace_symbols_builtin.xs_gMonth
    | Some x -> x
  in
  object (self)
    inherit atomicValue ta
    val v' = v 

    method erase_atomic_value () = DateTime.string_of_gMonth v'
    method getAtomicValueKind () = ATGMonth
    method getAtomicGMonth () = v'
    method private eq a = 
	  gMonth_equal None v' (a#getAtomicGMonth())
    method private lteq a = 
	  gMonth_lteq None v' (a#getAtomicGMonth())
    method private lt a = 
	  gMonth_lt None v' (a#getAtomicGMonth())
    method private gteq a = 
	  gMonth_gteq None v' (a#getAtomicGMonth())
    method private gt a = 
	  gMonth_gt None v' (a#getAtomicGMonth())
    method cast_to nsenv at bt = 
      match bt with 
      | ATString ->
	  ((new atomicString ~ta:at (DateTime.string_of_gMonth v')) :> atomicValue)
      | ATBoolean -> 
	  raise (Query (Cast_Error "Cannot cast a GMonth value to a Boolean"))
      | ATDecimal -> 
	  raise (Query (Cast_Error "Cannot cast a GMonth value to a Decimal"))
      | ATFloat -> 
	  raise (Query (Cast_Error "Cannot cast a GMonth value to a Float"))
      | ATDouble -> 
	  raise (Query (Cast_Error "Cannot cast a GMonth value to a Double"))
      | ATInteger ->
	  raise (Query (Cast_Error "Cannot cast a GMonth value to an Integer"))
      | ATDuration ->
	  raise (Query (Cast_Error "Cannot cast a GMonth value to an Duration"))
      | ATDateTime -> 
	  raise (Query (Cast_Error "Cannot cast a GMonth value to an DateTime"))
      | ATTime ->
	  raise (Query (Cast_Error "Cannot cast a GMonth value to an Time"))
      | ATDate ->
	  raise (Query (Prototype "Cast to a Date not supported"))
      | ATGYearMonth ->
	  raise (Query (Cast_Error "Cannot cast a GMonth value to an GYearMonth"))
      | ATGYear ->
	  raise (Query (Cast_Error "Cannot cast a GMonth value to an GYear"))
      | ATGMonthDay ->
	  raise (Query (Cast_Error "Cannot cast a GMonth value to an GMonthDay"))
      | ATGDay ->
	  raise (Query (Cast_Error "Cannot cast a GMonth value to an GDay"))
      | ATGMonth ->
	  (new atomicGMonth ~ta:at v') (* self *)
      | ATHexBinary ->
	  raise (Query (Cast_Error "Cannot cast to a HexBinary"))
      | ATBase64Binary ->
	  raise (Query (Cast_Error "Cannot cast to a Base64Binary"))
      | ATAnyURI ->
	  raise (Query (Cast_Error "Cannot cast a GMonth value to a AnyURI"))
      | ATQName -> 
	  raise (Query (Cast_Error "Cannot cast a GMonth value to a QName"))
      | ATNOTATION -> 
	  raise (Query (Prototype "Cast to a NOTATION not supported"))
      | ATYearMonthDuration ->
	  raise (Query (Prototype "Cast to a yearMonthDuration not supported"))
      | ATDayTimeDuration ->
	  raise (Query (Prototype "Cast to a dateTimeDuration not supported"))
      | ATUntypedAtomic -> 
	  (new atomicUntyped ~ta:at (DateTime.string_of_gMonth v'))
      | ATAnyAtomic -> (self :> atomicGMonth)
  end

and atomicYearMonthDuration ?ta:type_annotation v = 
  let ta =
    match type_annotation with
    | None -> Namespace_symbols_builtin.xs_yearMonthDuration
    | Some x -> x
  in
  object (self)
    inherit atomicValue ta
    val v' = v 

    method erase_atomic_value () = DateTime.canonical_of_yearMonthDuration v'
    method getAtomicValueKind () = ATYearMonthDuration
    method getAtomicYearMonthDuration () = v'
    method private eq a = 
	  yearMonthDuration_equal v' (a#getAtomicYearMonthDuration())
    method private lteq a = 
	  yearMonthDuration_lteq v' (a#getAtomicYearMonthDuration())
    method private lt a = 
	  yearMonthDuration_lt v' (a#getAtomicYearMonthDuration())
    method private gteq a = 
	  yearMonthDuration_gteq v' (a#getAtomicYearMonthDuration())
    method private gt a = 
	  yearMonthDuration_gt v' (a#getAtomicYearMonthDuration())
    method cast_to nsenv at bt = 
      match bt with 
      | ATString ->
	  ((new atomicString ~ta:at (self#erase_atomic_value())) :> atomicValue)
      | ATBoolean -> 
	  raise (Query (Cast_Error "Cannot cast a yearMonthDuration value to a Boolean"))
      | ATDecimal -> 
	  raise (Query (Cast_Error "Cannot cast a yearMonthDuration value to a Decimal"))
      | ATFloat -> 
	  raise (Query (Cast_Error "Cannot cast a yearMonthDuration value to a Float"))
      | ATDouble -> 
	  raise (Query (Cast_Error "Cannot cast a yearMonthDuration value to a Double"))
      | ATInteger ->
	  raise (Query (Cast_Error "Cannot cast a yearMonthDuration value to an Integer"))
      | ATDuration ->
	  (new atomicDuration ~ta:at (DateTime.duration_of_yearMonthDuration v'))
      | ATDateTime -> 
	  raise (Query (Cast_Error "Cannot cast a yearMonthDuration value to an DateTime"))
      | ATTime ->
	  raise (Query (Cast_Error "Cannot cast a yearMonthDuration value to an Time"))
      | ATDate ->
	  raise (Query (Prototype "Cast to a Date not supported"))
      | ATGYearMonth ->
	  raise (Query (Cast_Error "Cannot cast a yearMonthDuration value to an GYearMonth"))
      | ATGYear ->
	  raise (Query (Cast_Error "Cannot cast a yearMonthDuration value to an GYear"))
      | ATGMonthDay ->
	  raise (Query (Cast_Error "Cannot cast a yearMonthDuration value to an GMonthDay"))
      | ATGDay ->
	  raise (Query (Cast_Error "Cannot cast a yearMonthDuration value to an GDay"))
      | ATGMonth ->
	  raise (Query (Cast_Error "Cannot cast a yearMonthDuration value to an GMonth"))
      | ATHexBinary ->
	  raise (Query (Cast_Error "Cannot cast to a HexBinary"))
      | ATBase64Binary ->
	  raise (Query (Cast_Error "Cannot cast to a Base64Binary"))
      | ATAnyURI ->
	  raise (Query (Cast_Error "Cannot cast a yearMonthDuration value to a AnyURI"))
      | ATQName -> 
	  raise (Query (Cast_Error "Cannot cast a yearMonthDuration value to a QName"))
      | ATNOTATION -> 
	  raise (Query (Prototype "Cast to a NOTATION not supported"))
      | ATYearMonthDuration ->
	  (new atomicYearMonthDuration ~ta:at v') (* self *)
      | ATDayTimeDuration ->
      (* 
	 If ST is xs:yearMonthDuration and TT is xs:dayTimeDuration,
	 the cast is permitted and returns a xs:dayTimeDuration with value 0.0
	 seconds.  
      *)
          (new atomicDayTimeDuration ~ta:at zero_dayTimeDuration)
      | ATUntypedAtomic -> 
	  (new atomicUntyped ~ta:at (self#erase_atomic_value()))
      | ATAnyAtomic -> (self :> atomicYearMonthDuration)
  end

and atomicDayTimeDuration ?ta:type_annotation v = 
  let ta =
    match type_annotation with
    | None -> Namespace_symbols_builtin.xs_dayTimeDuration
    | Some x -> x
  in
  object (self)
    inherit atomicValue ta
    val v' = v 

    method erase_atomic_value () = DateTime.canonical_of_dayTimeDuration v'
    method getAtomicValueKind () = ATDayTimeDuration
    method getAtomicDayTimeDuration () = v'
    method private eq a = 
	  dayTimeDuration_equal v' (a#getAtomicDayTimeDuration())
    method private lteq a = 
	  dayTimeDuration_lteq v' (a#getAtomicDayTimeDuration())
    method private lt a = 
	  dayTimeDuration_lt v' (a#getAtomicDayTimeDuration())
    method private gteq a = 
	  dayTimeDuration_gteq v' (a#getAtomicDayTimeDuration())
    method private gt a = 
	  dayTimeDuration_gt v' (a#getAtomicDayTimeDuration())
    method cast_to nsenv at bt = 
      match bt with 
      | ATString ->
	  ((new atomicString ~ta:at (self#erase_atomic_value())) :> atomicValue)
      | ATBoolean -> 
	  raise (Query (Cast_Error "Cannot cast a dayTimeDuration value to a Boolean"))
      | ATDecimal -> 
	  raise (Query (Cast_Error "Cannot cast a dayTimeDuration value to a Decimal"))
      | ATFloat -> 
	  raise (Query (Cast_Error "Cannot cast a dayTimeDuration value to a Float"))
      | ATDouble -> 
	  raise (Query (Cast_Error "Cannot cast a dayTimeDuration value to a Double"))
      | ATInteger ->
	  raise (Query (Cast_Error "Cannot cast a dayTimeDuration value to an Integer"))
      | ATDuration ->
	  (new atomicDuration ~ta:at (DateTime.duration_of_dayTimeDuration v'))
      | ATDateTime -> 
	  raise (Query (Cast_Error "Cannot cast a dayTimeDuration value to an DateTime"))
      | ATTime ->
	  raise (Query (Cast_Error "Cannot cast a dayTimeDuration value to an Time"))
      | ATDate ->
	  raise (Query (Prototype "Cast to a Date not supported"))
      | ATGYearMonth ->
	  raise (Query (Cast_Error "Cannot cast a dayTimeDuration value to an GYearMonth"))
      | ATGYear ->
	  raise (Query (Cast_Error "Cannot cast a dayTimeDuration value to an GYear"))
      | ATGMonthDay ->
	  raise (Query (Cast_Error "Cannot cast a dayTimeDuration value to an GMonthDay"))
      | ATGDay ->
	  raise (Query (Cast_Error "Cannot cast a dayTimeDuration value to an GDay"))
      | ATGMonth ->
	  raise (Query (Cast_Error "Cannot cast a dayTimeDuration value to an GMonth"))
      | ATHexBinary ->
	  raise (Query (Cast_Error "Cannot cast to a HexBinary"))
      | ATBase64Binary ->
	  raise (Query (Cast_Error "Cannot cast to a Base64Binary"))
      | ATAnyURI ->
	  raise (Query (Cast_Error "Cannot cast a dayTimeDuration value to a AnyURI"))
      | ATQName -> 
	  raise (Query (Cast_Error "Cannot cast a dayTimeDuration value to a QName"))
      | ATNOTATION -> 
	  raise (Query (Prototype "Cast to a NOTATION not supported"))
      | ATYearMonthDuration ->
      (*
         If ST is xs:dayTimeDuration and TT is xs:yearMonthDuration,
	 the cast is permitted and returns a xs:yearMonthDuration with value 0
	 months.
      *)
          (new atomicYearMonthDuration ~ta:at zero_yearMonthDuration)
      | ATDayTimeDuration ->
	  (new atomicDayTimeDuration ~ta:at v') (* self *)
      | ATUntypedAtomic -> 
	  (new atomicUntyped ~ta:at (self#erase_atomic_value()))
      | ATAnyAtomic -> (self :> atomicDayTimeDuration)
  end

and atomicHexBinary ?ta:type_annotation v = 
  let ta =
    match type_annotation with
    | None -> Namespace_symbols_builtin.xs_hexBinary
    | Some x -> x
  in
  object (self)
    inherit atomicValue ta
    val v' = v

    method erase_atomic_value () = serialize_hexBinary v'
    method getAtomicValueKind () = ATHexBinary
    method getAtomicHexBinary () = v'
    method private eq a = 
	  hexBinary_equal v' (a#getAtomicHexBinary())
    method private lteq a = 
	  hexBinary_lteq v' (a#getAtomicHexBinary())
    method private lt a = 
	  hexBinary_lt v' (a#getAtomicHexBinary())
    method private gteq a = 
	  hexBinary_gteq v' (a#getAtomicHexBinary())
    method private gt a = 
	  hexBinary_gt v' (a#getAtomicHexBinary())
    method cast_to nsenv at bt = 
      match bt with 
      | ATString ->
	  ((new atomicString ~ta:at (serialize_hexBinary v')) :> atomicValue)
      | ATBoolean -> 
	  (new atomicBoolean ~ta:at (
	   begin
	     match v' with
	     | "0" -> false
	     | "1" -> true
	     | _ ->
	 raise (Query (Cast_Error "Cannot cast a HexBinary value to a Boolean"))
	   end))
      | ATDecimal -> 
	  raise (Query (Cast_Error "Cannot cast a hexBinary value to a Decimal"))
      | ATFloat -> 
	  raise (Query (Cast_Error "Cannot cast a hexBinary value to a Float"))
      | ATDouble -> 
	  raise (Query (Cast_Error "Cannot cast a hexBinary value to a Double"))
      | ATInteger ->
	  raise (Query (Cast_Error "Cannot cast a hexBinary value to an Integer"))
      | ATDuration ->
	  raise (Query (Cast_Error "Cannot cast a hexBinary value to an Duration"))
      | ATDateTime -> 
	  raise (Query (Cast_Error "Cannot cast a hexBinary value to an DateTime"))
      | ATTime ->
	  raise (Query (Cast_Error "Cannot cast a hexBinary value to an Time"))
      | ATDate ->
	  raise (Query (Prototype "Cast to a Date not supported"))
      | ATGYearMonth ->
	  raise (Query (Cast_Error "Cannot cast a hexBinary value to an GYearMonth"))
      | ATGYear ->
	  raise (Query (Cast_Error "Cannot cast a hexBinary value to an GYear"))
      | ATGMonthDay ->
	  raise (Query (Cast_Error "Cannot cast a hexBinary value to an GMonthDay"))
      | ATGDay ->
	  raise (Query (Cast_Error "Cannot cast a hexBinary value to an GDay"))
      | ATGMonth ->
	  raise (Query (Cast_Error "Cannot cast a hexBinary value to an GMonth"))
      | ATHexBinary ->
	  (new atomicHexBinary ~ta:at v') (* self *)
      | ATBase64Binary ->
	  (new atomicBase64Binary ~ta:at v') (* casting *)
      | ATAnyURI ->
	  raise (Query (Cast_Error "Cannot cast a hexBinary value to a AnyURI"))
      | ATQName -> 
	  raise (Query (Cast_Error "Cannot cast a hexBinary value to a QName"))
      | ATNOTATION -> 
	  raise (Query (Prototype "Cast to a NOTATION not supported"))
      | ATYearMonthDuration ->
	  raise (Query (Cast_Error "Cannot cast a hexBinary value to a YearMonthDuratoin"))
      | ATDayTimeDuration ->
	  raise (Query (Cast_Error "Cannot cast a hexBinary value to an DayTimeDuration"))
      | ATUntypedAtomic -> 
	  (new atomicUntyped ~ta:at (serialize_hexBinary v'))
      | ATAnyAtomic -> (self :> atomicHexBinary)
  end

and atomicBase64Binary ?ta:type_annotation v = 
  let ta =
    match type_annotation with
    | None -> Namespace_symbols_builtin.xs_base64Binary
    | Some x -> x
  in
  object (self)
    inherit atomicValue ta
    val v' = v

    method erase_atomic_value () = serialize_base64Binary v'
    method getAtomicValueKind () = ATBase64Binary
    method getAtomicBase64Binary () = v'
    method private eq a   = base64Binary_equal v' (a#getAtomicBase64Binary())
    method private lteq a = base64Binary_lteq v' (a#getAtomicBase64Binary())
    method private lt a   = base64Binary_lt v' (a#getAtomicBase64Binary())
    method private gteq a = base64Binary_gteq v' (a#getAtomicBase64Binary())
    method private gt a   = base64Binary_gt v' (a#getAtomicBase64Binary())
    method cast_to nsenv at bt = 
      match bt with
      | ATString ->
	  ((new atomicString ~ta:at (serialize_base64Binary v')) :> atomicValue)
      | ATBoolean -> 
	  (new atomicBoolean ~ta:at (
	   begin
	     match v' with
	     | "0" -> false
	     | "1" -> true
	     | _ ->
		 raise (Query (Cast_Error "Cannot cast a base64Binary value to a Boolean"))
	   end))
      | ATDecimal -> 
	  raise (Query (Cast_Error "Cannot cast a base64Binary value to a Decimal"))
      | ATFloat -> 
	  raise (Query (Cast_Error "Cannot cast a base64Binary value to a Float"))
      | ATDouble -> 
	  raise (Query (Cast_Error "Cannot cast a base64Binary value to a Double"))
      | ATInteger ->
	  raise (Query (Cast_Error "Cannot cast a base64Binary value to an Integer"))
      | ATDuration ->
	  raise (Query (Cast_Error "Cannot cast a base64Binary value to an Duration"))
      | ATDateTime -> 
	  raise (Query (Cast_Error "Cannot cast a base64Binary value to an DateTime"))
      | ATTime ->
	  raise (Query (Cast_Error "Cannot cast a base64Binary value to an Time"))
      | ATDate ->
	  raise (Query (Prototype "Cast to a Date not supported"))
      | ATGYearMonth ->
	  raise (Query (Cast_Error "Cannot cast a base64Binary value to an GYearMonth"))
      | ATGYear ->
	  raise (Query (Cast_Error "Cannot cast a base64Binary value to an GYear"))
      | ATGMonthDay ->
	  raise (Query (Cast_Error "Cannot cast a base64Binary value to an GMonthDay"))
      | ATGDay ->
	  raise (Query (Cast_Error "Cannot cast a base64Binary value to an GDay"))
      | ATGMonth ->
	  raise (Query (Cast_Error "Cannot cast a base64Binary value to an GMonth"))
      | ATHexBinary ->
	  (new atomicHexBinary ~ta:at v') (* casting *)
      | ATBase64Binary ->
	  (new atomicBase64Binary ~ta:at v') (* self *)
      | ATAnyURI ->
	  raise (Query (Cast_Error "Cannot cast a base64Binary value to a AnyURI"))
      | ATQName -> 
	  raise (Query (Cast_Error "Cannot cast a base64Binary value to a QName"))
      | ATNOTATION -> 
	  raise (Query (Prototype "Cast to a NOTATION not supported"))
      | ATYearMonthDuration ->
	  raise (Query (Cast_Error "Cannot cast a base64Binary value to a YearMonthDuratoin"))
      | ATDayTimeDuration ->
	  raise (Query (Cast_Error "Cannot cast a base64Binary value to an DayTimeDuration"))
      | ATUntypedAtomic -> 
	  (new atomicUntyped ~ta:at (serialize_base64Binary v'))
      | ATAnyAtomic -> (self :> atomicBase64Binary)
  end

and atomicAnyURI ?ta:type_annotation v = 
  let ta =
    match type_annotation with
    | None -> Namespace_symbols_builtin.xs_anyURI
    | Some x -> x
  in
  object (self)
    inherit atomicValue ta
    val v' = v

    method erase_atomic_value () = _string_of_uri v'
    method getAtomicValueKind () = ATAnyURI
    method getAtomicAnyURI () = v'
    method private eq a = 
	  anyURI_equal v' (a#getAtomicAnyURI())
    method private lteq a = 
	  raise (Query (Undefined "le not defined on xs:anyURI"))
    method private lt a = 
	  raise (Query (Undefined "lt not defined on xs:anyURI"))
    method private gteq a = 
	  raise (Query (Undefined "ge not defined on xs:anyURI"))
    method private gt a = 
	  raise (Query (Undefined "gt not defined on xs:anyURI"))
    method cast_to nsenv at bt = 
      match bt with 
      | ATString ->
	  ((new atomicString ~ta:at (AnyURI._string_of_uri v'))  :> atomicValue)
      | ATBoolean -> 
	  raise (Query (Cast_Error "Cannot cast a anyURI value to a Boolean"))
      | ATDecimal -> 
	  raise (Query (Cast_Error "Cannot cast a anyURI value to a Decimal"))
      | ATFloat -> 
	  raise (Query (Cast_Error "Cannot cast a anyURI value to a Float"))
      | ATDouble -> 
	  raise (Query (Cast_Error "Cannot cast a anyURI value to a Double"))
      | ATInteger ->
	  raise (Query (Cast_Error "Cannot cast a anyURI value to an Integer"))
      | ATDuration ->
	  raise (Query (Cast_Error "Cannot cast a anyURI value to an Duration"))
      | ATDateTime -> 
	  raise (Query (Cast_Error "Cannot cast a anyURI value to an DateTime"))
      | ATTime ->
	  raise (Query (Cast_Error "Cannot cast a anyURI value to an Time"))
      | ATDate ->
	  raise (Query (Prototype "Cast to a Date not supported"))
      | ATGYearMonth ->
	  raise (Query (Cast_Error "Cannot cast a anyURI value to an GYearMonth"))
      | ATGYear ->
	  raise (Query (Cast_Error "Cannot cast a anyURI value to an GYear"))
      | ATGMonthDay ->
	  raise (Query (Cast_Error "Cannot cast a anyURI value to an GMonthDay"))
      | ATGDay ->
	  raise (Query (Cast_Error "Cannot cast a anyURI value to an GDay"))
      | ATGMonth ->
	  raise (Query (Cast_Error "Cannot cast a anyURI value to an GMonth"))
      | ATHexBinary ->
	  raise (Query (Cast_Error "Cannot cast to a HexBinary"))
      | ATBase64Binary ->
	  raise (Query (Cast_Error "Cannot cast to a Base64Binary"))
      | ATAnyURI ->
	  ((new atomicAnyURI ~ta:at v') (* self *) :> atomicValue)
      | ATQName -> 
	  raise (Query (Cast_Error "Cannot cast a anyURI value to a QName"))
      | ATNOTATION -> 
	  raise (Query (Prototype "Cast to a NOTATION not supported"))
      | ATYearMonthDuration ->
	  raise (Query (Cast_Error "Cannot cast a anyURI value to a YearMonthDuration"))
      | ATDayTimeDuration ->
	  raise (Query (Cast_Error "Cannot cast a anyURI value to a dayTimeDuration"))
      | ATUntypedAtomic -> 
	  ((new atomicUntyped ~ta:at (AnyURI._string_of_uri v')) :> atomicValue)
      | ATAnyAtomic -> (self :> atomicAnyURI)
  end

and atomicQName ?ta:type_annotation v = 
  let ta =
    match type_annotation with
    | None -> Namespace_symbols_builtin.xs_QName
    | Some x -> x
  in
  object (self)
    inherit atomicValue ta
    val v' = v

    method erase_atomic_value () = anon_prefix_string v'
    method getAtomicValueKind () = ATQName
    method getAtomicQName ()     = v'

    (* Operations on QNames *)
    method private eq (a : atomicQName) = 
      qname_equal v' (a#getAtomicQName())
    method private lteq a =
      qname_lteq v' (a#getAtomicQName())
    method private lt a =
      qname_lt v' (a#getAtomicQName())
    method private gteq a =
      qname_gteq v' (a#getAtomicQName())
    method private gt a =
      qname_gt v' (a#getAtomicQName())

    method cast_to nsenv at bt = 
      match bt with 
      | ATString ->
	  ((new atomicString ~ta:at (anon_prefix_string v')) :> atomicValue)
      | ATBoolean -> 
	  raise (Query (Cast_Error "Cannot cast a QName value to a Boolean"))
      | ATDecimal -> 
	  raise (Query (Cast_Error "Cannot cast a QName value to a Decimal"))
      | ATFloat -> 
	  raise (Query (Cast_Error "Cannot cast a QName value to a Float"))
      | ATDouble -> 
	  raise (Query (Cast_Error "Cannot cast a QName value to a Double"))
      | ATInteger ->
	  raise (Query (Cast_Error "Cannot cast a QName value to an Integer"))
      | ATDuration ->
	  raise (Query (Cast_Error "Cannot cast a QName value to an Duration"))
      | ATDateTime -> 
	  raise (Query (Cast_Error "Cannot cast a QName value to an DateTime"))
      | ATTime ->
	  raise (Query (Cast_Error "Cannot cast a QName value to an Time"))
      | ATDate ->
	  raise (Query (Prototype "Cast to a Date not supported"))
      | ATGYearMonth ->
	  raise (Query (Cast_Error "Cannot cast a QName value to an GYearMonth"))
      | ATGYear ->
	  raise (Query (Cast_Error "Cannot cast a QName value to an GYear"))
      | ATGMonthDay ->
	  raise (Query (Cast_Error "Cannot cast a QName value to an GMonthDay"))
      | ATGDay ->
	  raise (Query (Cast_Error "Cannot cast a QName value to an GDay"))
      | ATGMonth ->
	  raise (Query (Cast_Error "Cannot cast a QName value to an GMonth"))
      | ATHexBinary ->
	  raise (Query (Cast_Error "Cannot cast to a HexBinary"))
      | ATBase64Binary ->
	  raise (Query (Cast_Error "Cannot cast to a Base64Binary"))
      | ATAnyURI ->
	  raise (Query (Cast_Error "Cannot cast a QName value to a anyURI"))
      | ATQName ->
	  ((self :> atomicQName) :> atomicValue)
      | ATNOTATION -> 
	  raise (Query (Prototype "Cast to a NOTATION not supported"))
      | ATYearMonthDuration ->
	  raise (Query (Cast_Error "Cannot cast a QName value to a YearMonthDuration"))
      | ATDayTimeDuration ->
	  raise (Query (Cast_Error "Cannot cast a QName value to a dayTimeDuration"))
      | ATUntypedAtomic -> 
	  ((new atomicUntyped ~ta:at (anon_prefix_string v')) :> atomicValue)
      | ATAnyAtomic -> (self :> atomicQName)
  end

and atomicNotation ?ta:type_annotation v = 
  let ta =
    match type_annotation with
    | None -> Namespace_symbols_builtin.xs_NOTATION
    | Some x -> x
  in
  object(self)
    inherit atomicValue ta
    val v' = v 

    method erase_atomic_value () = v'
    method getAtomicValueKind () = ATNOTATION
    method getAtomicNotation () = v'
    method private eq a = 
	  notation_equal v' (a#getAtomicNotation())
    method private lteq a = 
	  notation_lteq v' (a#getAtomicNotation())
    method private lt a = 
	  notation_lt v' (a#getAtomicNotation())
    method private gteq a = 
	  notation_gteq v' (a#getAtomicNotation())
    method private gt a = 
	  notation_gt v' (a#getAtomicNotation())

    method cast_to nsenv at bt = 
      match bt with 
      | ATString ->
	  raise (Query (Cast_Error ("Cannot cast NOTATION value: \"" ^ v' ^ "\" to a string")))
      | ATBoolean -> 
	  raise (Query (Cast_Error "Cannot cast a NOTATION value to a Boolean"))
      | ATDecimal -> 
	  raise (Query (Cast_Error "Cannot cast a NOTATION value to a Decimal"))
      | ATFloat -> 
	  raise (Query (Cast_Error "Cannot cast a NOTATION value to a Float"))
      | ATDouble -> 
	  raise (Query (Cast_Error "Cannot cast a NOTATION value to a Double"))
      | ATInteger ->
	  raise (Query (Cast_Error "Cannot cast a NOTATION value to an Integer"))
      | ATDuration ->
	  raise (Query (Cast_Error "Cannot cast a NOTATION value to an Duration"))
      | ATDateTime -> 
	  raise (Query (Cast_Error "Cannot cast a NOTATION value to an DateTime"))
      | ATTime ->
	  raise (Query (Cast_Error "Cannot cast a NOTATION value to an Time"))
      | ATDate ->
	  raise (Query (Prototype "Cast to a Date not supported"))
      | ATGYearMonth ->
	  raise (Query (Cast_Error "Cannot cast a NOTATION value to an GYearMonth"))
      | ATGYear ->
	  raise (Query (Cast_Error "Cannot cast a NOTATION value to an GYear"))
      | ATGMonthDay ->
	  raise (Query (Cast_Error "Cannot cast a NOTATION value to an GMonthDay"))
      | ATGDay ->
	  raise (Query (Cast_Error "Cannot cast a NOTATION value to an GDay"))
      | ATGMonth ->
	  raise (Query (Cast_Error "Cannot cast a NOTATION value to an GMonth"))
      | ATHexBinary ->
	  raise (Query (Cast_Error "Cannot cast to a HexBinary"))
      | ATBase64Binary ->
	  raise (Query (Cast_Error "Cannot cast to a Base64Binary"))
      | ATAnyURI ->
	  raise (Query (Cast_Error "Cannot cast a NOTATION value to a anyURI"))
      | ATQName -> 
	  raise (Query (Cast_Error "Cannot cast a NOTATION value to a QName"))
      | ATNOTATION -> 
	  (new atomicNotation ~ta:at v') (* self *)
      | ATYearMonthDuration ->
	  raise (Query (Cast_Error "Cannot cast a NOTATION value to a YearMonthDuration"))
      | ATDayTimeDuration ->
	  raise (Query (Cast_Error "Cannot cast a NOTATION value to a dayTimeDuration"))
      | ATUntypedAtomic -> 
	  (new atomicUntyped ~ta:at v')
      | ATAnyAtomic -> (self :> atomicNotation)
  end

and atomicUntyped ?ta:type_annotation v = 
  let ta =
    match type_annotation with
    | None -> Namespace_symbols_builtin.xs_untypedAtomic
    | Some x -> x
  in
  object (self)
    inherit atomicValue ta
    val v' = v

    method erase_atomic_value () = v'
    method getAtomicValueKind () = ATUntypedAtomic
    method getAtomicUntyped () = v'
    method private eq a = 
	  untyped_equal v' (a#getAtomicUntyped())
    method private lteq a = 
	  untyped_lteq v' (a#getAtomicUntyped())
    method private lt a = 
	  untyped_lt v' (a#getAtomicUntyped())
    method private gteq a = 
	  untyped_gteq v' (a#getAtomicUntyped())
    method private gt a = 
	  untyped_gt v' (a#getAtomicUntyped())

    method cast_to nsenv at bt = 
      match bt with 
      | ATString ->
	  ((new atomicString ~ta:at v') :> atomicValue)
      | ATBoolean -> 
	  let us = String.lowercase v' in
	  let nab = (new atomicBoolean ~ta:at (boolean_of_untyped us)) in
	  (nab :> atomicValue)
      | ATDecimal -> 
	  ((new atomicDecimal ~ta:at (decimal_of_untyped v')) :> atomicValue)
      | ATFloat -> 
	  ((new atomicFloat ~ta:at (float_of_untyped v')) :> atomicValue)
      | ATDouble -> 
	  ((new atomicDouble ~ta:at (double_of_untyped v')) :> atomicValue)
      | ATInteger ->
	  ((new atomicInteger ~ta:at (integer_of_untyped at v')) :> atomicValue)
      | ATDuration ->
	  ((new atomicDuration ~ta:at (duration_of_untyped v')) :> atomicValue)
      | ATDateTime -> 
	  ((new atomicDateTime ~ta:at (dateTime_of_untyped v')) :> atomicValue)
      | ATTime ->
	  ((new atomicTime ~ta:at (time_of_untyped v')) :> atomicValue)
      | ATDate ->
	  ((new atomicDate ~ta:at (date_of_untyped v')) :> atomicValue)
      | ATGYearMonth ->
	  (new atomicGYearMonth ~ta:at (gYearMonth_of_untyped v'))
      | ATGYear ->
	  (new atomicGYear ~ta:at (gYear_of_untyped v'))
      | ATGMonthDay ->
	  (new atomicGMonthDay ~ta:at (gMonthDay_of_untyped v'))
      | ATGDay ->
	  (new atomicGDay ~ta:at (gDay_of_untyped v'))
      | ATGMonth ->
	  (new atomicGMonth ~ta:at (gMonth_of_untyped v'))
      | ATHexBinary ->
	  ((new atomicHexBinary ~ta:at (hexBinary_of_untyped v')) :> atomicValue)
      | ATBase64Binary ->
	  ((new atomicBase64Binary ~ta:at (base64Binary_of_untyped v')) :> atomicValue)
      | ATAnyURI ->
	  ((new atomicAnyURI ~ta:at (anyURI_of_untyped v')) :> atomicValue)
      | ATQName -> 
	  raise (Query (Cast_Error "Cannot cast an Untyped value to a QName"))
      | ATNOTATION -> 
	  raise (Query (Cast_Error "Cannot cast a Untyped value to a NOTATION"))
      | ATYearMonthDuration ->
	  ((new atomicYearMonthDuration ~ta:at (yearMonthDuration_of_untyped v')) :> atomicValue)
      | ATDayTimeDuration ->
	  ((new atomicDayTimeDuration ~ta:at (dayTimeDuration_of_untyped v')) :> atomicValue)
      | ATUntypedAtomic -> (* Why can't we just do this: self :> atomicUntyped *)
  	  ((new atomicUntyped ~ta:at v') :> atomicValue)
      | ATAnyAtomic -> (self :> atomicUntyped)
  end


(********************************)
(* Hashtables for atomic values *)
(********************************)

(* Replicated *)
(* AtomicValue Hash *)
(* This bizarre looking thing is because
   ocaml's hash -> takes union types to their
	  enumerated value.. 
   
   It is the identity function on integers.   
*)

(* as of ocaml 3.08, the polymorphic hash 
   function was pretty bad. It is not a hash
   function at all: It is the identity
   function on integers.
   On strings -> it writes them in base 19.

   If we need better performance, rewrite the hash function. 
*)
module AtomicValueHashType = 
 struct
   type t = atomicValue
   let equal x y = x#atomic_value_eq y
   let hash x =
     let kind_hash =
       (Hashtbl.hash (x#getAtomicValueKind ()) + 17) * 23
     in
     let ret =
       (Hashtbl.hash (x#erase_atomic_value ())) *
	 kind_hash
     in
     ret 
 end

module AtomicValueHash = Hashtbl.Make(AtomicValueHashType)

