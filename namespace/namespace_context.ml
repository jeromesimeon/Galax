(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: namespace_context.ml,v 1.14 2007/10/16 01:25:34 mff Exp $ *)

(* Module: Namespace_context
   Description:
     This module implements support for the namespace context.
*)

open Error

open Namespace_names
open Namespace_builtin


(**********************)
(* Namespace bindings *)
(**********************)

(* Namespace binding *)

type binding = prefix * uri

(* Type for binding tables *)

(*
module OrderedBinding =
  struct
    type t = binding
    let compare = compare
  end

module BindingSet = Set.Make(OrderedBinding)
*)

type binding_table = binding list

(* Create a binding for a uri with a fresh prefix *)

let create_binding uri =
  (new_prefix "glx",uri)

(* Filters the built-in prefixes and the default function prefix *)

let cleanup_bindings bindings required_prefixes =
  let real_required_prefixes =
    List.filter 
      (fun (pref,uri) -> 
	not (is_xml_built_in_namespace (pref,uri))) 
      required_prefixes
  in
  List.filter 
    (fun (pref,uri) -> 
      not(is_built_in_namespace (pref,uri) || pref = NSDefaultFunctionPrefix) || 
      (List.exists (fun y -> (pref,uri) = y) real_required_prefixes))
    bindings

let cleanup_out_bindings bindings required_prefixes =
  let real_required_prefixes =
    List.filter 
      (fun (pref,uri) ->
	(* (Printf.printf "Required prefix: %s\n" (string_of_prefix pref);flush stdout); *)
	not (is_xml_out_built_in_namespace (pref,uri))) 
      required_prefixes
  in
  List.filter 
    (fun (pref,uri) -> 
      not(is_xml_out_built_in_namespace (pref,uri) || pref = NSDefaultFunctionPrefix) || 
      (List.exists (fun y -> (pref,uri) = y) real_required_prefixes))
    bindings

let cleanup_actual_out_bindings bindings required_prefixes =
  let real_required_prefixes =
    List.filter 
      (fun (pref,uri) ->
	(* (Printf.printf "Required prefix: %s\n" (string_of_prefix pref);flush stdout); *)
	not (is_xml_out_built_in_namespace (pref,uri))) 
      required_prefixes
  in
  List.filter 
    (fun (pref,uri) -> 
      not(is_xml_actual_out_built_in_namespace (pref,uri) || pref = NSDefaultFunctionPrefix) || 
      (List.exists (fun y -> (pref,uri) = y) real_required_prefixes))
    bindings


(*************************)
(* Namespace environment *)
(*************************)

(* type for namespace environments *)

type nsenv_content =
    { nsenv_bindings       : binding_table;
      mutable nsenv_reused : bool;
      nsenv_previous       : nsenv }

and nsenv = nsenv_content option ref

let rec dump_nsenv nsenv = 
  match !nsenv with 
  | None -> ()
  | Some nc -> (dump_nsenv (nc.nsenv_previous);
  List.iter (fun (p,u) -> Debug.print_default_debug ((Namespace_names.string_of_prefix p)^":"^(Namespace_names.string_of_uri u)^"\n")) nc.nsenv_bindings)


(* creates an empty environment *)

let empty_nsenv = ref None
let make_empty_nsenv () = ref None

let default_xquery_nsenv =
  let nsenv_content =
    { nsenv_bindings = xquery_built_in_namespaces;
      nsenv_reused   = false;
      nsenv_previous = empty_nsenv }
  in
  ref (Some nsenv_content)

let default_ext_xquery_nsenv =
  let nsenv_content =
    { nsenv_bindings = ext_xquery_built_in_namespaces;
      nsenv_reused   = false;
      nsenv_previous = empty_nsenv }
  in
  ref (Some nsenv_content)

let default_xml_nsenv =
  let nsenv_content =
    { nsenv_bindings = xml_built_in_namespaces;
      nsenv_reused   = false;
      nsenv_previous = empty_nsenv }
  in
  ref (Some nsenv_content)

let default_xml_out_nsenv () =
  let nsenv_content =
    { nsenv_bindings = xml_out_built_in_namespaces;
      nsenv_reused   = false;
      nsenv_previous = empty_nsenv }
  in
  ref (Some nsenv_content)

let default_all_nsenv =
  let nsenv_content =
    { nsenv_bindings = all_built_in_namespaces;
      nsenv_reused   = false;
      nsenv_previous = empty_nsenv }
  in
  ref (Some nsenv_content)

(* adds a new prefix,uri binding in a namespace environment *)

let check_ns pref uri =
  begin
    match (pref,uri) with
      (* Hack for the 'empty' prefix which is used in the XML
	 Schema to types mapping *)
    | (NSPrefix "empty", NSUri "") ->
	()
    | (NSPrefix pre, NSUri "") ->
	raise (Query (Namespace_Error ("Namespace name cannot be empty when using the namespace prefix "^pre^" [See Namespace REC Section 2. Declaring Namespaces]")))
    | (NSWildcardPrefix, uri) ->
	Error.eprintf_warning (Error.bprintf_error "" (Query (Namespace_Internal ("Cannot redefine the wildcard prefix to "^(string_of_uri uri)))));
	  ()
(*	raise (Query (Namespace_Internal ("Cannot redefine the wildcard prefix")))*)
    | _ ->
	()
  end

let is_reused nsenv =
  match !nsenv with
  | None -> true
  | Some nsenv_content ->
      nsenv_content.nsenv_reused

let check_reused nsenv =
  match !nsenv with
  | None -> ()
  | Some nsenv_content ->
      nsenv_content.nsenv_reused <- true

let add_all_ns nsenv binding_table =
  (* Note: Important! if there is no new bindings,
     just point to the previous namespace environment.
     - Jerome *)
  if binding_table = []
  then
    begin
      check_reused nsenv;
      nsenv
    end
  else
    begin
      (* Checking some namespace consistency *)
      List.iter (fun (x,y) -> check_ns x y) binding_table;
      (* Creates a new namespace environment, pointing to the previous context *)
      let new_nsenv_content =
	{ nsenv_bindings = binding_table;
	  nsenv_reused   = false;
	  nsenv_previous = nsenv }
      in
      ref (Some new_nsenv_content)
    end

(* gets the namespace of a given prefix *)

let rec get_ns_of_prefix nsr pref =
  match !nsr with
  | None -> 
      begin
	match pref with
	| NSDefaultElementPrefix ->
	    raise (Query (Namespace_Internal ("Default element namespace not found.")))
	| NSDefaultFunctionPrefix ->
	    raise (Query (Namespace_Internal ("Default function namespace not found.")))
	| NSWildcardPrefix ->
	    raise (Query (Namespace_Internal ("Wildcard namespace not found.")))
        | NSInterfacePrefix nc ->
	    raise (Query (Namespace_Internal ("Namespace interface prefix "^ nc^ " not found.")))
        | NSServerPrefix nc ->
	    raise (Query (Namespace_Internal ("Namespace server prefix "^ nc^ " not found.")))
        | NSPrefix nc ->
	    raise (Query (Namespace_Internal ("Namespace prefix "^ nc^ " not found.")))
      end
  | Some nsenv_content ->
      begin
	try
	  List.assoc pref nsenv_content.nsenv_bindings
	with
	| Not_found ->
	    get_ns_of_prefix nsenv_content.nsenv_previous pref
      end

let binding_in_scope nsenv (prefix,uri) =
  try
    let buri = get_ns_of_prefix nsenv prefix in
    if buri = uri
    then false
    else true
  with
  | _ -> true

let filter_in_scope nsenv binding_table = List.filter (binding_in_scope nsenv) binding_table

let add_all_ns_test nsenv binding_table =
  let patched_bindings = filter_in_scope nsenv binding_table in
  add_all_ns nsenv patched_bindings

let filter_nsenv_in_scope nsenv binding_table =
  let patched_bindings = filter_in_scope nsenv binding_table in
  add_all_ns nsenv patched_bindings,patched_bindings

let remove_empty_ns nsenv binding_table =
  let check_empty_ns_aux nsenv (prefix,uri) =
    match uri with
    | NSUri "" ->
	begin
	  try
	    ignore(get_ns_of_prefix nsenv prefix);
	    true
	  with
	  | _ ->
	      false
	end
    | _ -> true
  in
  List.filter (check_empty_ns_aux nsenv) binding_table

let add_all_ns_xquery nsenv binding_table =
  let binding_table = remove_empty_ns nsenv binding_table in
  add_all_ns nsenv binding_table

let rec get_prefix_of_uri nsenv uri =
  match !nsenv with
  | None -> 
      begin
	match uri with
	| NSUri uri ->
	    raise (Query (Namespace_Internal ("Default prefix in scope for uri: " ^ uri)))
	| NSWildcardUri ->
	    raise (Query (Namespace_Internal ("Wildcard uri never has a prefix.")))
      end
  | Some nsenv_content ->
      begin
	try
	  fst(List.find
		(fun (y,x) -> x = uri && (y != NSDefaultFunctionPrefix)) 
		(nsenv_content.nsenv_bindings))
	with
	| Not_found ->
	    get_prefix_of_uri nsenv_content.nsenv_previous uri
      end

let is_conflict nsenv prefix uri =
  try
    let buri = get_ns_of_prefix nsenv prefix in
    if buri = uri
    then false
    else true
  with
  | _ -> false

let make_binding nsenv (orig_prefix,uri,ncname) =
  (* 1st. Look in the in-scope namespaces *)
(*
  let orig_pref_string = Namespace_names.string_of_prefix orig_prefix in
  let orig_uri_string = Namespace_names.string_of_uri uri in
*)
  try
    (* Printf.printf "Looking for prefix of URI {%s} in in-scope namespaces\n" orig_uri_string; flush stdout; *)
    let pref = get_prefix_of_uri nsenv uri in
    ((pref,ncname), None, (pref,uri))
  with
  | _ ->
      (* 2. Use the original binding unless there is a conflict *)
      if not(is_conflict nsenv orig_prefix uri)
      then
	begin
	  (* Printf.printf "Using original prefix '%s' for URI {%s}\n" orig_pref_string orig_uri_string; flush stdout; *)
	  ((orig_prefix,ncname),Some(orig_prefix,uri),(orig_prefix,uri))
	end
      else
	begin
	  (* 3. Create a new binding from scratch which does not conflict with anyone *)
	  (* Printf.printf "Conflict with original prefix '%s' for URI {%s}, using new binding\n" orig_pref_string orig_uri_string; flush stdout; *)
	  let (pref,uri) = create_binding uri in
	  ((pref,ncname),Some (pref,uri), (pref,uri))
	end

let make_attribute_binding nsenv (orig_prefix,uri,ncname) =
  if uri = NSUri ""
  then
    ((NSDefaultElementPrefix,ncname),None,(NSDefaultElementPrefix,uri))
  else
    begin
      (* 1st. Look in the in-scope namespaces *)
      try
	let pref = get_prefix_of_uri nsenv uri in
	((pref,ncname), None, (pref,uri))
      with
      | _ ->
	  (* 2. Use the original binding unless there is a conflict *)
	  if not(is_conflict nsenv orig_prefix uri)
	  then
	    begin
	      ((orig_prefix,ncname),Some(orig_prefix,uri),(orig_prefix,uri))
	    end
	  else
	    begin
	      (* 3. Create a new binding from scratch which does not conflict with anyone *)
	      let (pref,uri) = create_binding uri in
	      ((pref,ncname),Some (pref,uri), (pref,uri))
	    end
    end

(* Returns the new bindings for the current context *)

let active_bindings nsenv =
  match !nsenv with
  | None -> []
  | Some nsenv_content ->
      nsenv_content.nsenv_bindings

let rec delta_bindings nsenv1 nsenv2 =
  if nsenv1 == nsenv2
  then []
  else
    match !nsenv1 with
    | None -> []
    | Some nsenv_content ->
	(* Must remove duplicates here *)
	let previous_deltas = (delta_bindings (nsenv_content.nsenv_previous) nsenv2) in
	(List.filter (fun y -> not(List.mem y previous_deltas)) (nsenv_content.nsenv_bindings)) @ (previous_deltas)

let replace_bindings nsenv bt =
  begin
    match !nsenv with
    | None ->
	let newnsenv =
	  { nsenv_bindings = bt;
	    nsenv_reused   = false;
	    nsenv_previous = ref None }
	in
      nsenv := Some newnsenv
    | Some nsenv_content ->
	let newnsenv =
	  { nsenv_bindings = bt;
	    nsenv_reused   = nsenv_content.nsenv_reused;
	    nsenv_previous = nsenv_content.nsenv_previous }
	in
	nsenv := Some newnsenv
  end; nsenv

let patch_bindings nsenv bt =
  if (is_reused nsenv)
  then
    add_all_ns nsenv bt
  else
    replace_bindings nsenv (bt @ (active_bindings nsenv))

(* Returns all of the bindings *)

let rec flatten_bindings nsenv =
  match !nsenv with
  | None -> []
  | Some nsenv_content ->
      (nsenv_content.nsenv_bindings) @ (flatten_bindings nsenv_content.nsenv_previous)

(* Are the nsenvs the identical ?*)

let same_nsenv nsenv1 nsenv2 = (nsenv1 == nsenv2)

(* Print a binding table *)


open Format

let print_bd i ff (prefix,uri) =
  fprintf ff "%i: {%s}{%s}" i (string_of_prefix prefix) (string_of_uri uri)

let rec print_bds i ff bds  =
  match bds with
  | [] -> ()
  | [e] -> fprintf ff "%a" (print_bd i) e
  | e :: es' ->
      fprintf ff "%a@ %a" (print_bd i) e (print_bds (i+1)) es'

let print_binding_table s ff bds =
  fprintf ff "%s@\n" s;
  fprintf ff "@[<hv 2>bindings {@,%a@;<0 -2>}@]@." (print_bds 0) bds

let print_sa i ff (uqname,content) =
  fprintf ff "%i: {%s}{%s}" i (string_of_uqname uqname) content

let rec print_sas i ff sas  =
  match sas with
  | [] -> ()
  | [e] -> fprintf ff "%a" (print_sa i) e
  | e :: es' ->
      fprintf ff "%a@ %a" (print_sa i) e (print_sas (i+1)) es'

let print_special_attributes s ff sas =
  fprintf ff "%s@\n" s;
  fprintf ff "@[<hv 2>special {@,%a@;<0 -2>}@]@." (print_sas 0) sas

