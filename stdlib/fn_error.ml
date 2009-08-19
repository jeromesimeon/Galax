(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: fn_error.ml,v 1.15 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Fn_error
   Description:
     This module implements support for the user-level fn:error()
     function.
*)

open Error

open Dm_atomic

open Physical_value
open Physical_item

type xquery_error_msg = Physical_value.item * Physical_value.item option * Physical_value.item list 

exception Xquery_error of xquery_error_msg

let default_error_item = Item_Atomic (new atomicQName (Namespace_symbols.anon_symbol Namespace_builtin.err_default_error) :> atomicValue)

(********************************************************************)
(* fn:error function                                                *)
(* If an invocation provides $description and $error-object, then   *)
(* these values may also be returned to the external processing     *)
(* environment. The means by which these values are provided to a host *)
(* environment is ·implementation dependent·.                       *)

(* An error xs:QName with namespace URI NS and local part LP will be *)
(* returned as the URI NS#LP.                                       *)

let raise_error proc_ctxt v = 

  (* If fn:error is invoked with no arguments, then its behavior is the same as the invocation of the following expression: *)
  (* fn:error(fn:expanded-QName('http://www.w3.org/2004/07/xqt-errors', 'FOER0000')) *)
  let arity = List.length v in 
  let qname = 
    if (arity < 1) then default_error_item 
    (* fn:error($error as xs:QName) as none *)
    else List.hd v 
  in
  let description = 
    (* fn:error($error as xs:QName, $description as xs:string) as none *)
    if (arity <= 1) then None
    else
      Some (List.hd (List.tl v))
  in
  let error_objs = 
    (* fn:error($error as xs:QName, $description as xs:string, $error-object as item()* *)
    if (arity <= 1) then []
    else List.tl (List.tl v)
  in
(*
  let fn_error_printer (qname, desc, error_objs) = 
    (Serialization.bserialize_datamodel proc_ctxt (Cursor.cursor_of_singleton qname))^":"^
    (Serialization.bserialize_datamodel proc_ctxt (Cursor.cursor_of_singleton desc))^"\n"^
     (Serialization.bserialize_datamodel proc_ctxt (Cursor.cursor_of_list error_objs))
  in
  raise (Boxed_error ((qname, description, error_objs), fn_error_printer))*)
  raise (Xquery_error (qname, description, error_objs)) 

let downgrade_error proc_ctxt msg =
  let errstr =
    match msg with
    | (qnameitem, None, _) -> 
	(getAtomicValue qnameitem)#erase_atomic_value()
    | (qnameitem, Some stritem, objs) ->
	(getAtomicValue qnameitem)#erase_atomic_value() ^
	(getAtomicValue stritem)#erase_atomic_value() ^
	(if objs = []
	then ""
	else
	  (Serialization.bserialize_datamodel proc_ctxt (Cursor.cursor_of_list objs)))
  in
  raise (Query (Error (errstr)))

