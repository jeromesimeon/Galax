(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: dm_atomic.mli,v 1.4 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Datatypes_atomic
   Description:
     Galax's abstract data model interface for atomic values
*)

open Datatypes
open Namespace_context


(************************)
(* Atomic value objects *)
(************************)

class virtual atomicValue :
    Namespace_symbols.rtype_symbol ->
  object
    method virtual erase_atomic_value : unit -> xs_untyped
    method string_value               : unit -> xs_untyped
    method virtual getAtomicValueKind : unit -> atomic_type
    method virtual atomic_type 	      : unit -> Namespace_symbols.rtype_symbol
    method virtual cast_to     	      : nsenv -> Namespace_symbols.rtype_symbol -> atomic_type -> atomicValue

    (* Equality/inequality/comparison operators *)
    method atomic_value_eq   	: atomicValue -> bool
    method atomic_value_lteq 	: atomicValue -> bool
    method atomic_value_lt   	: atomicValue -> bool
    method atomic_value_gteq 	: atomicValue -> bool
    method atomic_value_gt   	: atomicValue -> bool
    method atomic_value_compare : atomicValue -> int

    (* Downcasts *)
    method getAtomicString     	      : unit -> xs_string 
    method getAtomicBoolean    	      : unit -> xs_boolean 
    method getAtomicDecimal    	      : unit -> xs_decimal 
    method getAtomicFloat      	      : unit -> xs_float 
    method getAtomicDouble     	      : unit -> xs_double 
    method getAtomicInteger    	      : unit -> xs_integer 
    method getAtomicDuration   	      : unit -> xs_duration 
    method getAtomicDateTime   	      : unit -> xs_dateTime 
    method getAtomicTime       	      : unit -> xs_time 
    method getAtomicDate       	      : unit -> xs_date 
    method getAtomicGYearMonth 	      : unit -> xs_gYearMonth 
    method getAtomicGYear      	      : unit -> xs_gYear 
    method getAtomicGMonthDay  	      : unit -> xs_gMonthDay 
    method getAtomicGDay       	      : unit -> xs_gDay 
    method getAtomicGMonth     	      : unit -> xs_gMonth 
    method getAtomicYearMonthDuration : unit -> xs_yearMonthDuration
    method getAtomicDayTimeDuration   : unit -> xs_dayTimeDuration
    method getAtomicHexBinary         : unit -> xs_hexBinary
    method getAtomicBase64Binary      : unit -> xs_base64Binary
    method getAtomicAnyURI   	      : unit -> xs_anyURI
    method getAtomicQName    	      : unit -> xs_QName
    method getAtomicNotation 	      : unit -> xs_NOTATION
    method getAtomicUntyped  	      : unit -> xs_untyped

    method private virtual eq   : atomicValue -> bool
    method private virtual lteq : atomicValue -> bool
    method private virtual lt   : atomicValue -> bool
    method private virtual gteq : atomicValue -> bool
    method private virtual gt   : atomicValue -> bool
  end

and atomicString :
  ?ta:Namespace_symbols.rtype_symbol ->
  xs_string ->
  object
    inherit atomicValue

    method erase_atomic_value : unit -> xs_untyped
    method getAtomicValueKind : unit -> atomic_type

    method cast_to     : nsenv -> Namespace_symbols.rtype_symbol -> atomic_type -> atomicValue
    method atomic_type : unit -> Namespace_symbols.rtype_symbol

    method private eq   : atomicValue -> bool
    method private lteq : atomicValue -> bool
    method private lt   : atomicValue -> bool
    method private gteq : atomicValue -> bool
    method private gt   : atomicValue -> bool

(*    method get_string : unit -> xs_string  *)
  end

and atomicBoolean : 
  ?ta:Namespace_symbols.rtype_symbol ->
  xs_boolean -> 
  object
    inherit atomicValue

    method erase_atomic_value : unit -> xs_untyped
    method getAtomicValueKind : unit -> atomic_type
    method cast_to     	      : nsenv -> Namespace_symbols.rtype_symbol -> atomic_type -> atomicValue
    method atomic_type 	      : unit -> Namespace_symbols.rtype_symbol

    method private eq   : atomicValue -> bool
    method private lteq : atomicValue -> bool
    method private lt   : atomicValue -> bool
    method private gteq : atomicValue -> bool
    method private gt   : atomicValue -> bool
  end

and atomicDecimal : 
  ?ta:Namespace_symbols.rtype_symbol ->
  xs_decimal -> 
  object
    inherit atomicValue

    method erase_atomic_value : unit -> xs_untyped
    method getAtomicValueKind : unit -> atomic_type
    method cast_to     	      : nsenv -> Namespace_symbols.rtype_symbol -> atomic_type -> atomicValue
    method atomic_type 	      : unit -> Namespace_symbols.rtype_symbol

    method private eq   : atomicValue -> bool
    method private lteq : atomicValue -> bool
    method private lt   : atomicValue -> bool
    method private gteq : atomicValue -> bool
    method private gt   : atomicValue -> bool
  end

and atomicFloat : 
  ?ta:Namespace_symbols.rtype_symbol ->
  xs_float -> 
  object
    inherit atomicValue

    method erase_atomic_value : unit -> xs_untyped
    method getAtomicValueKind : unit -> atomic_type
    method cast_to     	      : nsenv -> Namespace_symbols.rtype_symbol -> atomic_type -> atomicValue
    method atomic_type 	      : unit -> Namespace_symbols.rtype_symbol

    method private eq   : atomicValue -> bool
    method private lteq : atomicValue -> bool
    method private lt   : atomicValue -> bool
    method private gteq : atomicValue -> bool
    method private gt   : atomicValue -> bool
  end

and atomicDouble : 
  ?ta:Namespace_symbols.rtype_symbol ->
  xs_double -> 
  object
    inherit atomicValue

    method erase_atomic_value : unit -> xs_untyped
    method getAtomicValueKind : unit -> atomic_type
    method cast_to     	      : nsenv -> Namespace_symbols.rtype_symbol -> atomic_type -> atomicValue
    method atomic_type 	      : unit -> Namespace_symbols.rtype_symbol

    method private eq   : atomicValue -> bool
    method private lteq : atomicValue -> bool
    method private lt   : atomicValue -> bool
    method private gteq : atomicValue -> bool
    method private gt   : atomicValue -> bool
  end

and atomicInteger :
  ?ta:Namespace_symbols.rtype_symbol ->
  xs_integer -> 
  object
    inherit atomicValue

    method erase_atomic_value : unit -> xs_untyped
    method getAtomicValueKind : unit -> atomic_type
    method cast_to     	      : nsenv -> Namespace_symbols.rtype_symbol -> atomic_type -> atomicValue
    method atomic_type 	      : unit -> Namespace_symbols.rtype_symbol

    method private eq   : atomicValue -> bool
    method private lteq : atomicValue -> bool
    method private lt   : atomicValue -> bool
    method private gteq : atomicValue -> bool
    method private gt   : atomicValue -> bool
  end

and atomicDuration : 
  ?ta:Namespace_symbols.rtype_symbol ->
  xs_duration -> 
  object
    inherit atomicValue

    method erase_atomic_value : unit -> xs_untyped
    method getAtomicValueKind : unit -> atomic_type
    method cast_to     	      : nsenv -> Namespace_symbols.rtype_symbol -> atomic_type -> atomicValue
    method atomic_type 	      : unit -> Namespace_symbols.rtype_symbol

    method private eq   : atomicValue -> bool
    method private lteq : atomicValue -> bool
    method private lt   : atomicValue -> bool
    method private gteq : atomicValue -> bool
    method private gt   : atomicValue -> bool
  end

and atomicDateTime : 
  ?ta:Namespace_symbols.rtype_symbol ->
  xs_dateTime -> 
  object
    inherit atomicValue

    method erase_atomic_value : unit -> xs_untyped
    method getAtomicValueKind : unit -> atomic_type
    method cast_to     	      : nsenv -> Namespace_symbols.rtype_symbol -> atomic_type -> atomicValue
    method atomic_type 	      : unit -> Namespace_symbols.rtype_symbol

    method private eq   : atomicValue -> bool
    method private lteq : atomicValue -> bool
    method private lt   : atomicValue -> bool
    method private gteq : atomicValue -> bool
    method private gt   : atomicValue -> bool
  end

and atomicTime : 
  ?ta:Namespace_symbols.rtype_symbol ->
  xs_time -> 
  object
    inherit atomicValue

    method erase_atomic_value : unit -> xs_untyped
    method getAtomicValueKind : unit -> atomic_type
    method cast_to     	      : nsenv -> Namespace_symbols.rtype_symbol -> atomic_type -> atomicValue
    method atomic_type 	      : unit -> Namespace_symbols.rtype_symbol

    method private eq   : atomicValue -> bool
    method private lteq : atomicValue -> bool
    method private lt   : atomicValue -> bool
    method private gteq : atomicValue -> bool
    method private gt   : atomicValue -> bool
  end

and atomicDate : 
  ?ta:Namespace_symbols.rtype_symbol ->
  xs_date -> 
  object
    inherit atomicValue

    method erase_atomic_value : unit -> xs_untyped
    method getAtomicValueKind : unit -> atomic_type
    method cast_to     	      : nsenv -> Namespace_symbols.rtype_symbol -> atomic_type -> atomicValue
    method atomic_type 	      : unit -> Namespace_symbols.rtype_symbol

    method private eq   : atomicValue -> bool
    method private lteq : atomicValue -> bool
    method private lt   : atomicValue -> bool
    method private gteq : atomicValue -> bool
    method private gt   : atomicValue -> bool
  end

and atomicGYearMonth : 
  ?ta:Namespace_symbols.rtype_symbol ->
  xs_gYearMonth -> 
  object
    inherit atomicValue

    method erase_atomic_value : unit -> xs_untyped
    method getAtomicValueKind : unit -> atomic_type
    method cast_to     	      : nsenv -> Namespace_symbols.rtype_symbol -> atomic_type -> atomicValue
    method atomic_type 	      : unit -> Namespace_symbols.rtype_symbol

    method private eq   : atomicValue -> bool
    method private lteq : atomicValue -> bool
    method private lt   : atomicValue -> bool
    method private gteq : atomicValue -> bool
    method private gt   : atomicValue -> bool
  end

and atomicGYear : 
  ?ta:Namespace_symbols.rtype_symbol ->
  xs_gYear -> 
  object
    inherit atomicValue

    method erase_atomic_value : unit -> xs_untyped
    method getAtomicValueKind : unit -> atomic_type
    method cast_to     	      : nsenv -> Namespace_symbols.rtype_symbol -> atomic_type -> atomicValue
    method atomic_type 	      : unit -> Namespace_symbols.rtype_symbol

    method private eq   : atomicValue -> bool
    method private lteq : atomicValue -> bool
    method private lt   : atomicValue -> bool
    method private gteq : atomicValue -> bool
    method private gt   : atomicValue -> bool
  end

and atomicGMonthDay : 
  ?ta:Namespace_symbols.rtype_symbol ->
  xs_gMonthDay -> 
  object
    inherit atomicValue

    method erase_atomic_value : unit -> xs_untyped
    method getAtomicValueKind : unit -> atomic_type
    method cast_to            : nsenv -> Namespace_symbols.rtype_symbol -> atomic_type -> atomicValue
    method atomic_type        : unit -> Namespace_symbols.rtype_symbol

    method private eq   : atomicValue -> bool
    method private lteq : atomicValue -> bool
    method private lt   : atomicValue -> bool
    method private gteq : atomicValue -> bool
    method private gt   : atomicValue -> bool
  end

and atomicGDay : 
  ?ta:Namespace_symbols.rtype_symbol ->
  xs_gDay -> 
  object
    inherit atomicValue

    method erase_atomic_value : unit -> xs_untyped
    method getAtomicValueKind : unit -> atomic_type
    method cast_to     	      : nsenv -> Namespace_symbols.rtype_symbol -> atomic_type -> atomicValue
    method atomic_type 	      : unit -> Namespace_symbols.rtype_symbol

    method private eq   : atomicValue -> bool
    method private lteq : atomicValue -> bool
    method private lt   : atomicValue -> bool
    method private gteq : atomicValue -> bool
    method private gt   : atomicValue -> bool
  end

and atomicGMonth : 
  ?ta:Namespace_symbols.rtype_symbol ->
  xs_gMonth -> 
  object
    inherit atomicValue

    method erase_atomic_value : unit -> xs_untyped
    method getAtomicValueKind : unit -> atomic_type
    method cast_to     	      : nsenv -> Namespace_symbols.rtype_symbol -> atomic_type -> atomicValue
    method atomic_type 	      : unit -> Namespace_symbols.rtype_symbol

    method private eq   : atomicValue -> bool
    method private lteq : atomicValue -> bool
    method private lt   : atomicValue -> bool
    method private gteq : atomicValue -> bool
    method private gt   : atomicValue -> bool
  end

and atomicYearMonthDuration : 
  ?ta:Namespace_symbols.rtype_symbol ->
  xs_yearMonthDuration ->
  object
    inherit atomicValue

    method erase_atomic_value : unit -> xs_untyped
    method getAtomicValueKind : unit -> atomic_type
    method cast_to     	      : nsenv -> Namespace_symbols.rtype_symbol -> atomic_type -> atomicValue
    method atomic_type 	      : unit -> Namespace_symbols.rtype_symbol

    method private eq   : atomicValue -> bool
    method private lteq : atomicValue -> bool
    method private lt   : atomicValue -> bool
    method private gteq : atomicValue -> bool
    method private gt   : atomicValue -> bool
  end

and atomicDayTimeDuration : 
  ?ta:Namespace_symbols.rtype_symbol ->
  xs_dayTimeDuration ->
  object
    inherit atomicValue

    method erase_atomic_value : unit -> xs_untyped
    method getAtomicValueKind : unit -> atomic_type
    method cast_to     	      : nsenv -> Namespace_symbols.rtype_symbol -> atomic_type -> atomicValue
    method atomic_type 	      : unit -> Namespace_symbols.rtype_symbol

    method private eq   : atomicValue -> bool
    method private lteq : atomicValue -> bool
    method private lt   : atomicValue -> bool
    method private gteq : atomicValue -> bool
    method private gt   : atomicValue -> bool
  end

and atomicHexBinary : 
  ?ta:Namespace_symbols.rtype_symbol ->
  xs_hexBinary ->
  object
    inherit atomicValue

    method erase_atomic_value : unit -> xs_untyped
    method getAtomicValueKind : unit -> atomic_type
    method cast_to            : nsenv -> Namespace_symbols.rtype_symbol -> atomic_type -> atomicValue
    method atomic_type        : unit -> Namespace_symbols.rtype_symbol

    method private eq   : atomicValue -> bool
    method private lteq : atomicValue -> bool
    method private lt   : atomicValue -> bool
    method private gteq : atomicValue -> bool
    method private gt   : atomicValue -> bool
  end

and atomicBase64Binary : 
  ?ta:Namespace_symbols.rtype_symbol ->
  xs_base64Binary ->
  object
    inherit atomicValue

    method erase_atomic_value : unit -> xs_untyped
    method getAtomicValueKind : unit -> atomic_type
    method cast_to     	      : nsenv -> Namespace_symbols.rtype_symbol -> atomic_type -> atomicValue
    method atomic_type 	      : unit -> Namespace_symbols.rtype_symbol

    method private eq   : atomicValue -> bool
    method private lteq : atomicValue -> bool
    method private lt   : atomicValue -> bool
    method private gteq : atomicValue -> bool
    method private gt   : atomicValue -> bool
  end

and atomicAnyURI : 
  ?ta:Namespace_symbols.rtype_symbol ->
  xs_anyURI ->
  object
    inherit atomicValue

    method erase_atomic_value : unit -> xs_untyped
    method getAtomicValueKind : unit -> atomic_type
    method cast_to            : nsenv -> Namespace_symbols.rtype_symbol -> atomic_type -> atomicValue
    method atomic_type        : unit -> Namespace_symbols.rtype_symbol

    method private eq   : atomicValue -> bool
    method private lteq : atomicValue -> bool
    method private lt   : atomicValue -> bool
    method private gteq : atomicValue -> bool
    method private gt   : atomicValue -> bool
  end

and atomicQName : 
  ?ta:Namespace_symbols.rtype_symbol ->
  xs_QName ->
  object
    inherit atomicValue

    method erase_atomic_value : unit -> xs_untyped
    method getAtomicValueKind : unit -> atomic_type
    method cast_to            : nsenv -> Namespace_symbols.rtype_symbol -> atomic_type -> atomicValue
    method atomic_type        : unit -> Namespace_symbols.rtype_symbol

    method private eq   : atomicValue -> bool
    method private lteq : atomicValue -> bool
    method private lt   : atomicValue -> bool
    method private gteq : atomicValue -> bool
    method private gt   : atomicValue -> bool

  end

and atomicNotation : 
  ?ta:Namespace_symbols.rtype_symbol ->
  xs_NOTATION ->
  object
    inherit atomicValue

    method erase_atomic_value : unit -> xs_untyped
    method getAtomicValueKind : unit -> atomic_type
    method cast_to            : nsenv -> Namespace_symbols.rtype_symbol -> atomic_type -> atomicValue
    method atomic_type        : unit -> Namespace_symbols.rtype_symbol

    method private eq   : atomicValue -> bool
    method private lteq : atomicValue -> bool
    method private lt   : atomicValue -> bool
    method private gteq : atomicValue -> bool
    method private gt   : atomicValue -> bool
  end

and atomicUntyped :       
  ?ta:Namespace_symbols.rtype_symbol ->
  xs_untyped -> 
  object
    inherit atomicValue

    method erase_atomic_value : unit -> xs_untyped
    method getAtomicValueKind : unit -> atomic_type
    method cast_to     	      : nsenv -> Namespace_symbols.rtype_symbol -> atomic_type -> atomicValue
    method atomic_type 	      : unit -> Namespace_symbols.rtype_symbol

    method private eq   : atomicValue -> bool
    method private lteq : atomicValue -> bool
    method private lt   : atomicValue -> bool
    method private gteq : atomicValue -> bool
    method private gt   : atomicValue -> bool
  end


(********************************)
(* Hashtables for atomic values *)
(********************************)

module AtomicValueHash : Hashtbl.S with type key = atomicValue

