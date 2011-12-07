(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: namespace_resolve.ml,v 1.9 2007/10/16 01:25:34 mff Exp $ *)

(* Module: Namespace_resolve
   Description:
     This module implements namespace resolution.
*)


open Namespace_names
open Namespace_context
open Namespace_symbols

open Error


(**********************************************)
(* Resolving QNames to Expanded QNames        *)
(* Expanded QNames are represented by symbols *)
(**********************************************)

(* Note:
     Given namespace environment an unresolved symbol, returns the
     resolved symbol for the resolved qname
   - J&B
*)

let resolve_element_qname nsr uqname =
  match uqname with
  | (NSWildcardPrefix, localname) ->
      let uri = get_ns_of_prefix nsr NSWildcardPrefix in
      (NSWildcardPrefix, uri, localname)
  | (NSDefaultElementPrefix, localname) ->
      let uri = get_ns_of_prefix nsr NSDefaultElementPrefix in
      (NSDefaultElementPrefix, uri, localname)
  | (NSPrefix nc, localname) ->
      let uri = get_ns_of_prefix nsr (NSPrefix nc) in
      (NSPrefix nc, uri, localname)
  | (NSInterfacePrefix _, localname)
  | (NSServerPrefix _, localname) ->
      raise (Query (Namespace_Error "Trying to resolve an element name as an interface or server name"))
  | (NSDefaultFunctionPrefix, localname) ->
      raise (Query (Namespace_Error "Trying to resolve an element name as a function name"))

let resolve_element_qname_default nsr uqname =
  match uqname with
  | (NSWildcardPrefix, localname) ->
      let uri = get_ns_of_prefix nsr NSWildcardPrefix in
      (NSWildcardPrefix, uri, localname),false
  | (NSDefaultElementPrefix, localname) ->
      let uri =
	try
	  get_ns_of_prefix nsr NSDefaultElementPrefix
	with
	| _-> NSUri ""
      in
      if (uri = NSUri "")
      then (NSDefaultElementPrefix, uri, localname),true
      else (NSDefaultElementPrefix, uri, localname),false
  | (NSPrefix nc, localname) ->
      let uri = get_ns_of_prefix nsr (NSPrefix nc) in
      (NSPrefix nc, uri, localname),false
  | (NSInterfacePrefix _, localname)
  | (NSServerPrefix _, localname) ->
      raise (Query (Namespace_Error "Trying to resolve an element name as an interface or server name"))
  | (NSDefaultFunctionPrefix, localname) ->
      raise (Query (Namespace_Error "Trying to resolve an element name as a function name"))

let resolve_pragma_qname nsr uqname =
  match uqname with
  | (NSWildcardPrefix, localname) ->
      raise (Query (Namespace_Error "Pragma QName name must have a prefix"))
  | (NSDefaultElementPrefix, localname) ->
      raise (Query (Namespace_Error "Pragma QName name must have a prefix"))
  | (NSPrefix nc, localname) ->
      let uri = get_ns_of_prefix nsr (NSPrefix nc) in
      (NSPrefix nc, uri, localname)
  | (NSInterfacePrefix _, localname)
  | (NSServerPrefix _, localname) ->
      raise (Query (Namespace_Error "Trying to resolve a pragma name as an interface or server name"))
  | (NSDefaultFunctionPrefix, localname) ->
      raise (Query (Namespace_Error "Trying to resolve a pragma name as a function name"))

let check_non_empty_prefix uri =
  match uri with
  | NSUri "" ->
      raise (Query (Namespace_Error "Function name in no namespace"))
  | _ -> ()

let resolve_function_qname nsr uqname =
  match uqname with
  | (NSWildcardPrefix, localname) ->
      raise (Query (Namespace_Error ("Resolution of wildcard namespace not supported!")))
  | (NSDefaultElementPrefix, localname) ->
      raise (Query (Namespace_Error "Trying to resolve a function name as an element name"))
  | (NSPrefix nc, localname) ->
      (* The prefix of an unresolved variable or function qname can refer to 
	 a generic namespace prefix, an interface prefix or a server prefix.
         We check for all 3 here. *) 
      (try 
	let uri = get_ns_of_prefix nsr (NSPrefix nc) in 
	(check_non_empty_prefix uri;
	(NSPrefix nc, uri, localname))
      with Query(Namespace_Internal _) -> 
	(try 
	  Debug.print_dxq_debug ("Function "^(string_of_uqname uqname)^" not matched\n");
	  let uri = get_ns_of_prefix nsr (NSInterfacePrefix nc) in 
	  (check_non_empty_prefix uri; 
	  let rqname = (NSInterfacePrefix nc, uri, localname) in
	  Debug.print_dxq_debug ("Function interface "^(prefixed_string_of_rqname rqname)^" matched\n");
	  rqname)
	with Query(Namespace_Internal _) -> 
	  let uri = get_ns_of_prefix nsr (NSServerPrefix nc) in 
	  (check_non_empty_prefix uri; 
	  (NSServerPrefix nc, uri, localname))))
  | (NSInterfacePrefix nc, localname) ->
      let uri = get_ns_of_prefix nsr (NSInterfacePrefix nc) in (NSInterfacePrefix nc, uri, localname)
  | (NSServerPrefix nc, localname) ->
      let uri = get_ns_of_prefix nsr (NSServerPrefix nc) in (NSServerPrefix nc, uri, localname)
(*      raise (Query (Namespace_Error "Trying to resolve a function name as an interface or server name"))*)
  | (NSDefaultFunctionPrefix, localname) ->
      let uri = get_ns_of_prefix nsr NSDefaultFunctionPrefix in
      (check_non_empty_prefix uri;
      (NSDefaultFunctionPrefix, uri, localname))

let resolve_variable_qname nsr uqname =
  match uqname with
  | (NSWildcardPrefix, localname) ->
      raise (Query (Namespace_Error ("Resolution of wildcard namespace not supported!")))
  | (NSDefaultElementPrefix, localname) ->
      (* Note:
	   Variables with no prefix are always in no namespace. See
	   XQuery spec [Section 3.1.2 Variable References].
         - Jerome *)
      (NSDefaultElementPrefix,NSUri "", localname)
  | (NSPrefix nc, localname) ->
      (* The prefix of an unresolved variable or function qname can refer to 
	 a generic namespace prefix, an interface prefix or a server prefix.
         We check for all 3 here. 
      *) 
      (try let uri = get_ns_of_prefix nsr (NSPrefix nc) in (NSPrefix nc, uri, localname)
      with Query(Namespace_Internal _) -> 
	(try let uri = get_ns_of_prefix nsr (NSInterfacePrefix nc) in (NSInterfacePrefix nc, uri, localname)
	with Query(Namespace_Internal _) -> 
	  let uri = get_ns_of_prefix nsr (NSServerPrefix nc) in (NSServerPrefix nc, uri, localname)))
  | (NSInterfacePrefix nc, localname) ->
      let uri = get_ns_of_prefix nsr (NSInterfacePrefix nc) in (NSInterfacePrefix nc, uri, localname)
  | (NSServerPrefix nc, localname) ->
      let uri = get_ns_of_prefix nsr (NSServerPrefix nc) in (NSServerPrefix nc, uri, localname)
(*      raise (Query (Namespace_Error "Trying to resolve a variable name as an interface or server name")) *)
  | (NSDefaultFunctionPrefix, localname) ->
      raise (Query (Namespace_Error "Trying to resolve a variable name as a function name"))

let resolve_type_qname nsr uqname =
  match uqname with
  | (NSWildcardPrefix, localname) ->
      let uri = get_ns_of_prefix nsr NSWildcardPrefix in
      (NSWildcardPrefix, uri, localname)
  | (NSDefaultElementPrefix, localname) ->
      let uri = get_ns_of_prefix nsr NSDefaultElementPrefix in
      (NSDefaultElementPrefix, uri, localname)
  | (NSPrefix nc, localname) ->
      let uri = get_ns_of_prefix nsr (NSPrefix nc) in
      (NSPrefix nc, uri, localname)
  | (NSInterfacePrefix _, localname)
  | (NSServerPrefix _, localname) ->
      raise (Query (Namespace_Error "Trying to resolve a type name as an interface or server name"))
  | (NSDefaultFunctionPrefix, localname) ->
      raise (Query (Namespace_Error "Trying to resolve a type name as a function name"))

let resolve_group_qname nsr uqname =
  match uqname with
  | (NSWildcardPrefix, localname) ->
      let uri = get_ns_of_prefix nsr NSWildcardPrefix in
      (NSWildcardPrefix, uri, localname)
  | (NSDefaultElementPrefix, localname) ->
      let uri = get_ns_of_prefix nsr NSDefaultElementPrefix in
      (NSDefaultElementPrefix, uri, localname)
  | (NSPrefix nc, localname) ->
      let uri = get_ns_of_prefix nsr (NSPrefix nc) in
      (NSPrefix nc, uri, localname)
  | (NSInterfacePrefix _, localname)
  | (NSServerPrefix _, localname) ->
      raise (Query (Namespace_Error "Trying to resolve a group name as an interface or server name"))
  | (NSDefaultFunctionPrefix, localname) ->
      raise (Query (Namespace_Error "Trying to resolve a group name as a function name"))

let resolve_attrGroup_qname nsr uqname =
  match uqname with
  | (NSWildcardPrefix, localname) ->
      let uri = get_ns_of_prefix nsr NSWildcardPrefix in
      (NSWildcardPrefix, uri, localname)
  | (NSDefaultElementPrefix, localname) ->
      let uri = get_ns_of_prefix nsr NSDefaultElementPrefix in
      (NSDefaultElementPrefix, uri, localname)
  | (NSPrefix nc, localname) ->
      let uri = get_ns_of_prefix nsr (NSPrefix nc) in
      (NSPrefix nc, uri, localname)
  | (NSInterfacePrefix _, localname)
  | (NSServerPrefix _, localname) ->
      raise (Query (Namespace_Error "Trying to resolve an attrGroup name as an interface or server name"))
  | (NSDefaultFunctionPrefix, localname) ->
      raise (Query (Namespace_Error "Trying to resolve an attrGroup name as a function name"))

let resolve_attribute_qname nsr uqname =
  match uqname with 
  | (NSWildcardPrefix, localname) ->
      let uri = get_ns_of_prefix nsr NSWildcardPrefix in
      (NSWildcardPrefix, uri, localname)
  | (NSDefaultElementPrefix, localname) ->
      (NSDefaultElementPrefix, NSUri "", localname)
  | (NSPrefix prefix, localname) -> 
      let uri = get_ns_of_prefix nsr (NSPrefix prefix) in 
      (NSPrefix prefix,uri,localname)
  | (NSInterfacePrefix _, localname)
  | (NSServerPrefix _, localname) ->
      raise (Query (Namespace_Error "Trying to resolve an attribute name as an interface or server name"))
  | (NSDefaultFunctionPrefix, _) ->
      raise (Query (Namespace_Error "Trying to resolve an attribute name as a function name"))

let resolve_element_qname_to_symbol nsr ename = 
  relem_symbol (resolve_element_qname nsr ename)

let resolve_attribute_qname_to_symbol nsr aname = 
  rattr_symbol (resolve_attribute_qname nsr aname)

let resolve_type_qname_to_symbol nsr tname = 
  rtype_symbol (resolve_type_qname nsr tname)

let resolve_anon_qname_to_symbol nsr tname = 
  anon_symbol (resolve_element_qname nsr tname)

