(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: print_type_core.ml,v 1.11 2007/08/15 18:53:48 mgreenberg Exp $ *)

(* Module: Print_core_type
   Description:
     This module implements pretty-printing for the core variant of
     the XQuery type system.

     The printer here tries to print the type out in the XQuery type
     syntax, though there is no guarantee of completeness.

     TODO

     * Namespaces should be collated from a traversal of the core type
       and printed out in the beginning.  Right now, the XSchema namespace
       is defined by default.
*)

open Format

open Error
open Occurrence

open Xquery_type_core_ast
open Print_common

open Namespace_symbols_util

(**********)
(* Errors *)
(**********)

let raise_simple_type_with_attribute_content_error () =
  raise (Query (Malformed_Type "Found a simple type with attribute content: should never happen!"))


(***************************)
(* Print a type derivation *)
(***************************)

let print_restricts ff ctname =
  fprintf ff "@ restricts %a" print_symbol ctname
let print_extends ff ctname =
  fprintf ff "@ extends %a" print_symbol ctname


(******************************)
(* Print a substitution group *)
(******************************)

let print_substitutes_for ff = function
  | CSubstitutesFor cename -> 
      fprintf ff "@ substitutes for %a@ " print_symbol cename
  | CNonSubstitutesFor -> () 


(*****************************)
(* Print a union simple type *)
(*****************************)

let rec print_csimple_type_union ff stl =
  match stl with
  | [] ->
      fprintf ff "none"
  | ctname::[] ->
      fprintf ff "%a" print_symbol ctname
  | ctname::stl' ->
      fprintf ff "%a |@ %a"
	print_symbol ctname print_csimple_type_union stl'


(****************)
(* Print a type *)
(****************)

(* Note: 
     Here is, for each type constructor, the code used for precedence
     in the pretty-printer.

   4 - TAttributeRef, TAttributeLocal, TElementRef, TElementLocal, 
       TGroupRef, TEmpty, TNone, TAtomicRef, TDocument, TText
   3 - TBound
   2 - TInterleave
   1 - TSequence
   0 - TChoice

     Higher number indicates higher precedence.

   - Jerome and Phil

 *)

let rec print_cxtype ff m =
  print_cxtype_prec 0 ff m

and print_cxtype_prec p ff xt =
  match xt.pcxtype_desc with
  | CAtomicRef ctname ->
      fprintf ff "%a" print_symbol ctname
  | CElementRef cename -> 
      fprintf ff "element %a" print_symbol cename
  | CElementLocal(cename,nillable,ctname) ->
      fprintf ff "@[<hv 2>element %a%a%a@]"
	print_symbol cename print_nillable nillable print_xtype_spec ctname
  | CAttributeRef caname -> 
      fprintf ff "attribute %a" 
	print_symbol caname
  | CAttributeLocal(caname,ctname) -> 
      fprintf ff "@[<hv 2>attribute %a %a@]" 
	print_symbol caname print_xtype_spec ctname
  | CDocument cxtype ->
      fprintf ff "@[<hv 2>document {@,%a@;<0 -2>}@]"
	(print_cxtype_prec 0) cxtype
  | CText ->
      fprintf ff "text" 
  | CPI None ->
      fprintf ff "processing-instruction()"
  | CPI (Some name) ->
      fprintf ff "processing-instruction(%s)" name
  | CComment ->
      fprintf ff "comment" 
  | CBound(cxtype,minocc,maxocc) -> 
      begin
	match (minocc,maxocc) with
	| (UP_INT 0, UNBOUNDED) ->
	    fprintf ff "%a*" (print_cxtype_prec 3) cxtype
	| (UP_INT 1, UNBOUNDED) ->
	    fprintf ff "%a+" (print_cxtype_prec 3) cxtype
	| (UP_INT 0, UP_INT 1) ->
	    fprintf ff "%a?" (print_cxtype_prec 3) cxtype
	| (UNBOUNDED, _) ->
	    fprintf ff "none"
	| _ ->
	    fprintf ff "%a (minoccurs %s) (maxoccurs %s)" (print_cxtype_prec 3) cxtype (string_of_occurs minocc) (string_of_occurs maxocc)
      end
  | CSequence(cxtype1,cxtype2) ->
      if p > 1 then
        fprintf ff "@[<hv 1>(%a,@ %a)@]"
	  (print_cxtype_prec 1) cxtype1 (print_cxtype_prec 1) cxtype2
      else
        fprintf ff "%a,@ %a"
	  (print_cxtype_prec 1) cxtype1 (print_cxtype_prec 1) cxtype2
  | CEmpty ->
      fprintf ff "()"
  | CChoice(cxtype1,cxtype2) -> 
      if p > 0 then
	fprintf ff "@[<hv 1>(%a |@ %a)@]"
	  (print_cxtype_prec 0) cxtype1 (print_cxtype_prec 0) cxtype2
      else
	fprintf ff "%a |@ %a"
	  (print_cxtype_prec 0) cxtype1 (print_cxtype_prec 0) cxtype2
  | CNone ->
      fprintf ff "none"
  | CInterleave(cxtype1,cxtype2) ->
      if p > 2 then
	fprintf ff "@[<hv 1>(%a &@ %a)@]" 
	  (print_cxtype_prec 2) cxtype1 (print_cxtype_prec 2) cxtype2
      else
	fprintf ff "%a &@ %a" 
	  (print_cxtype_prec 2) cxtype1 (print_cxtype_prec 2) cxtype2

(******************************)
(* Print a type specification *)
(******************************)

and print_xtype_spec ff ctname =
  fprintf ff " of type %a@;<0 -2>" print_symbol ctname

and print_cxtype_kind ff = 
  let simple () = fprintf ff "simple" in
  let complex () = fprintf ff "complex" in
  function
  | (_, _, CComplexTypeRestriction _) -> complex ()
  | (_, _, CComplexTypeExtension _) -> complex ()
  | (_, _, CAtomicTypeRestriction) -> simple ()
  | (_, _, CSimpleTypeList _) -> simple ()
  | (_, _, CSimpleTypeUnion _) -> simple ()

and print_cxtype_derivation ff = function
  | (der, cattr_content, CComplexTypeRestriction (cmixed, cxtype)) ->
      fprintf ff "%a@ %a {%a@,%a@;<0 -2>}"
        print_restricts der
	print_mixed cmixed
	print_cattribute_content cattr_content
	print_cxtype cxtype
  | (der, cattr_content, CComplexTypeExtension (cmixed, cxtype)) ->
      fprintf ff "%a@ %a {%a@,%a@;<0 -2>}"
        print_extends der
	print_mixed cmixed
	print_cattribute_content cattr_content
	print_cxtype cxtype
  | (der, None, CAtomicTypeRestriction) ->
      fprintf ff "%a@;<0 -2>"
        print_restricts der
  | (der, None, CSimpleTypeList ctname) ->
      fprintf ff "%a@ list of %a@;<0 -2>"
        print_restricts der
	print_symbol ctname
  | (der, None, CSimpleTypeUnion ctname_list) ->
      fprintf ff "%a@ union of {@,%a@;<0 -2>}"
        print_restricts der
	print_csimple_type_union ctname_list
  | _ ->
      raise_simple_type_with_attribute_content_error ()

and print_cattribute_content ff = function 
  | None -> ()
  | Some cxtype -> fprintf ff "@,%a;" print_cxtype cxtype

let print_cattr_derivation ff ctname =
  print_xtype_spec ff ctname

let print_celem_derivation ff (substfor, nillable, xtypespec) = 
  fprintf ff 
    "%a%a%a"
    print_substitutes_for substfor
    print_nillable nillable
    print_xtype_spec xtypespec  

let bprint_cxtype t = Gmisc.bprintf_stub "" (fun ff -> print_cxtype ff) t

(****************************)
(* Print a type declaration *)
(****************************)

let print_celem_decl ff (cename, celemder) =
  fprintf ff
    "@[<hv 2>declare element %a%a;@]"
    print_symbol cename
    print_celem_derivation celemder

let print_cattr_decl ff (caname,cattrder) =
  fprintf ff
    "@[<hv 2>declare attribute %a%a;@]"
    print_symbol caname
    print_cattr_derivation cattrder

let print_ctype_decl ff decl =
  fprintf ff "@[<hv 2>declare %a type %a%a;@]"
	print_cxtype_kind decl.ctypedecl_deriv
    print_symbol decl.ctypedecl_name
    print_cxtype_derivation decl.ctypedecl_deriv

(**********************************************)
(* Calculate and print namespace declarations *)
(**********************************************)

let extract_namespace (prefix, uri, _) =
  (Namespace_names.string_of_prefix prefix, Namespace_names.string_of_uri uri)

let relem_namespace relem = extract_namespace (Namespace_symbols.relem_name relem)
let rtype_namespace rtype = extract_namespace (Namespace_symbols.rtype_name rtype)
let rattr_namespace rattr = extract_namespace (Namespace_symbols.rattr_name rattr)

let cxtype_namespaces cxtype =
  let extract_namespaces cxtype =
	match cxtype.pcxtype_desc with
		CAtomicRef rtype -> [rtype_namespace rtype]
	  | CElementRef relem -> [relem_namespace relem]
	  | CAttributeRef rattr -> [rattr_namespace rattr]
	  | CElementLocal (relem, _, rtype) -> [relem_namespace relem; rtype_namespace rtype]
	  | CAttributeLocal (rattr, rtype) -> [rattr_namespace rattr; rtype_namespace rtype]
	  | _ -> []
  in
  Ast_walker_fold.fold_over_cxtype extract_namespaces List.append [] cxtype

let ctype_decl_namespaces decl =
  let namespaces = [rtype_namespace decl.ctypedecl_name] in
  let (rtype, _, children) = decl.ctypedecl_deriv in
  let namespaces = (rtype_namespace rtype)::namespaces in
  let child_namespaces = 
	match children with
		CAtomicTypeRestriction -> []
	  | CSimpleTypeList rtype -> [rtype_namespace rtype]
	  | CComplexTypeRestriction (_, cxtype)
	  | CComplexTypeExtension (_, cxtype) -> cxtype_namespaces cxtype
	  | CSimpleTypeUnion rtypes ->
		  (* this may produce duplicates, but uappend will weed them
			 out since child_namespaces is passed in as the first argument*)
		  (List.map rtype_namespace rtypes)
  in
	List.append child_namespaces namespaces

let celem_decl_namespaces (relem, (_, _, rtype)) =
  [relem_namespace relem; rtype_namespace rtype]
	
let cattr_decl_namespaces (rattr, rtype) =
  [rattr_namespace rattr; rtype_namespace rtype]

let calculate_namespaces cxs = 
  let ns = ref [] in
  let add_namespaces_for process_decl decls =
	let list_union new_ls base_ls =
	  List.fold_left 
		(fun base_ls new_elt ->
		  if List.mem new_elt base_ls
		  then base_ls
		  else new_elt::base_ls)
		base_ls 
		new_ls
	in
	SQNameHashtbl.iter 
	  (fun kind td -> 
		let nss = process_decl td in 
		  ns := list_union nss !ns)
	  decls
  in
	add_namespaces_for celem_decl_namespaces cxs.cxschema_element_declarations;
	add_namespaces_for cattr_decl_namespaces cxs.cxschema_attribute_declarations;
	add_namespaces_for ctype_decl_namespaces cxs.cxschema_type_declarations;
	!ns

let print_namespace_decl ff (prefix, uri) =
  fprintf ff "@[<hv 2>declare namespace %s = \"%s\";@]" prefix uri

let print_namespace_decls ff namespaces =
  List.iter (fun ns -> fprintf ff "%a@\n" print_namespace_decl ns) namespaces

(************************************)
(* Print a set of type declarations *)
(************************************)

let print_celem_decls ff celemdecls = 
  SQNameHashtbl.iter (fun kind td -> fprintf ff "%a@\n" print_celem_decl td) celemdecls

let print_cattr_decls ff cattrdecls = 
  SQNameHashtbl.iter (fun kind td -> fprintf ff "%a@\n" print_cattr_decl td) cattrdecls

let print_ctype_decls ff ctypedecls = 
  SQNameHashtbl.iter (fun kind td -> fprintf ff "%a@\n" print_ctype_decl td) ctypedecls

let print_letter_mappings ff letmap = 
  let (_, tmap) = Xquery_type_core_ast_annotation.type_letter_map letmap in 
  let (_, emap) = Xquery_type_core_ast_annotation.elem_letter_map letmap in 
  let (_, amap) = Xquery_type_core_ast_annotation.attr_letter_map letmap in 
  fprintf ff "Type Map@\n"; 
  SQNameHashtbl.iter (fun sym letter -> fprintf ff "%s@ ->@ %s\n" (Namespace_symbols.symbol_prefix_string sym) (string_of_int letter)) tmap;
  fprintf ff "Element Map@\n"; 
  SQNameHashtbl.iter (fun sym (_, _, letter) -> fprintf ff "%s@ ->@ %s\n" (Namespace_symbols.symbol_prefix_string sym) (string_of_int letter)) emap;
  fprintf ff "Attribute Map@\n"; 
  SQNameHashtbl.iter (fun sym (_, letter) -> fprintf ff "%s@ ->@ %s\n" (Namespace_symbols.symbol_prefix_string sym) (string_of_int letter)) amap

(************************)
(* Print a whole schema *)
(************************)

let print_cxschema ff cxs =
  fprintf ff "declare schema {@[<v 2>@,%a@,%a@,%a@,%a@]@;<0 -2>};@.@?"
	print_namespace_decls (calculate_namespaces cxs)
    print_celem_decls cxs.cxschema_element_declarations
    print_cattr_decls cxs.cxschema_attribute_declarations
	print_ctype_decls cxs.cxschema_type_declarations
(*   fprintf ff "%a\n@?"
     print_letter_mappings cxs.cxschema_letter_mappings *)

