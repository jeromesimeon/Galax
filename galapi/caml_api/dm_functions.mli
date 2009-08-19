(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: dm_functions.mli,v 1.10 2007/02/01 22:08:47 simeon Exp $ *)

(* Module: Dm_functions
   Description:

   This module implements a number of short-cut functions to construct
   and access the XQuery 1.0 and XPath 2.0 data model.

*)

open Dm
open Dm_atomic

open Physical_value


(*****************************)
(* Atomic value constructors *)
(*****************************)

val atomicString     	    : string 	     	       -> atomicString
val atomicBoolean    	    : bool   	     	       -> atomicBoolean
val atomicDecimal    	    : Num.num 	               -> atomicDecimal
val atomicInteger    	    : Big_int.big_int	       -> atomicInteger
val atomicFloat      	    : float   	     	       -> atomicFloat
val atomicDouble     	    : float   	     	       -> atomicDouble
val atomicAnyURI            : AnyURI._uri              -> atomicAnyURI
val atomicQName             : Namespace_context.nsenv * string -> atomicQName
val atomicUntyped           : string                   -> atomicUntyped
val atomicDate              : string                   -> atomicDate
val atomicTime              : string                   -> atomicTime
val atomicDateTime          : string                   -> atomicDateTime
val atomicDayTimeDuration   : string                   -> atomicDayTimeDuration
val atomicYearMonthDuration : string                   -> atomicYearMonthDuration

(*********************)
(* Node constructors *)
(*********************)

val documentNode  	      : (string * node list) -> document
val elementNode   	      : atomicQName * attribute list * node list * atomicQName -> element
val attributeNode 	      : atomicQName * atomicString * atomicQName -> attribute
val textNode      	      : atomicString -> text
val commentNode   	      : atomicString -> comment
val processingInstructionNode : atomicString * atomicString ->  processingInstruction


(**********************)
(* Accessors on items *)
(**********************)

val string_value    : item -> string
val item_kind       : item -> string
val get_node        : item -> node
val get_atomicValue : item -> atomicValue
val get_element     : item -> element
val get_attribute   : item -> attribute


(**********************)
(* Accessors on nodes *)
(**********************)

val parent   	: node -> node list
val children 	: node -> node list
val base_uri  	: node -> atomicString list
val node_kind 	: node -> string
val node_name   : node -> atomicQName list
val typed_value : node -> atomicValue list
val attributes  : node -> attribute list

(************************) 
(* Conversion functions *)
(************************) 

(* "Up-cast" functions *)

val to_node     	: #node -> node
val to_node_item   	: #node -> item
val to_atomicValue      : #atomicValue -> atomicValue
val to_atomicValue_item : #atomicValue -> item
val to_node_item_list   : #node list -> item list
val to_atomicValue_item_list : #atomicValue list -> item list

(* "Down cast" functions *)

val string_of_atomicValue    : atomicValue   -> string 
val boolean_of_atomicBoolean : atomicBoolean -> bool
val int_of_atomicInteger     : atomicInteger -> int
val integer_of_atomicInteger : atomicInteger -> Big_int.big_int
val decimal_of_atomicDecimal : atomicDecimal -> Num.num
val float_of_atomicFloat     : atomicFloat   -> float
val float_of_atomicDouble    : atomicDouble  -> float


