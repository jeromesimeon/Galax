(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: print_type.ml,v 1.5 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Print_type
   Description:
     This module implements pretty-printing for the XQuery type
     system.
*)

open Format

open Occurrence

open Xquery_type_ast
open Print_common


(***********************)
(* Print a simple type *)
(***********************)

let rec print_stype_spec ff spec = 
  match spec.pstype_specifier_desc with 
  | STypeRef tname    -> fprintf ff "of simple type %a@;<0 -2>" print_uqname tname
  | SAnonymous sderiv -> print_stype_derivation ff sderiv

and print_nested_stype_spec ff spec =
  match spec.pstype_specifier_desc with 
  | STypeRef tname    -> fprintf ff "%a@;<0 -2>" print_uqname tname
  | SAnonymous sderiv -> fprintf ff "{@,%a@;<0 -2>}" print_stype_derivation sderiv

and print_stype_derivation ff = function 
  | SRestriction sspec -> 
      fprintf ff "@[<hv 2>restricts %a@]" print_nested_stype_spec sspec
  | SList sspec -> 
      fprintf ff "@[<hv 2>list of %a@]" print_nested_stype_spec sspec
  | SUnion sspecs -> 
      fprintf ff "@[<hv 2>union of {@,%a@;<0 -2>}@]" print_stype_union sspecs

and print_stype_union ff = function 
  | [] -> ()
  | [s] -> print_nested_stype_spec ff s 
  | s :: ss -> 
      fprintf ff "@[%a |@,@ %a@]" print_nested_stype_spec s print_stype_union ss


(***************************)
(* Print a type derivation *)
(***************************)

let print_deriv ff = function
  | TRestriction tname -> fprintf ff "restricts %a@ " print_uqname tname
  | TExtension tname -> fprintf ff "extends %a@ " print_uqname tname

let print_deriv_opt ff = function
  | Some dr -> print_deriv ff dr
  | None -> ()


(******************************)
(* Print a substitution group *)
(******************************)

let print_substitutes_for ff = function
  | TSubstitutesFor ename -> 
      fprintf ff "substitutes for %a@ " print_uqname ename
  | TNonSubstitutesFor -> () 


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

let rec print_xtype ff m =
  print_xtype_prec 0 ff m

and print_xtype_prec p ff xt = 
  match xt.pxtype_desc with
  | TAtomicRef tname -> 
      fprintf ff "%a" print_uqname tname
  | TElementRef ename -> 
      fprintf ff "element %a" print_uqname ename
  | TElementLocal(ename,nillable,xtypespec) ->
      fprintf ff "@[<hv 2>element %a%a%a@]"
	print_uqname ename print_nillable nillable print_xtype_spec xtypespec
  | TAttributeRef aname -> 
      fprintf ff "attribute %a" 
	print_uqname aname
  | TAttributeLocal(aname,stypespec) -> 
      fprintf ff "@[<hv 2>attribute %a %a@]" 
	print_uqname aname print_stype_spec stypespec
  | TDocument xtype ->
      fprintf ff "@[<hv 2>document {@,%a@;<0 -2>}@]" 
	(print_xtype_prec 0) xtype
  | TText -> 
      fprintf ff "text" 
  | TComment -> 
      fprintf ff "comment" 
  | TProcessingInstruction -> 
      fprintf ff "processing-instruction" 
  | TGroupRef gname -> 
      fprintf ff "group %a" print_uqname gname
  | TAttrGroupRef gname -> 
      fprintf ff "attrGroup %a" print_uqname gname
  | TBound(xtype,minocc,maxocc) -> 
      begin
	match (minocc,maxocc) with
	| (UP_INT 0, UNBOUNDED) ->
	    fprintf ff "%a*" (print_xtype_prec 3) xtype
	| (UP_INT 1, UNBOUNDED) ->
	    fprintf ff "%a+" (print_xtype_prec 3) xtype
	| (UP_INT 0, UP_INT 1) ->
	    fprintf ff "%a?" (print_xtype_prec 3) xtype
	| (UNBOUNDED, _) ->
	    fprintf ff "none"
	| _ ->
	    fprintf ff "%a (minoccurs %s) (maxoccurs %s)" (print_xtype_prec 3) xtype (string_of_occurs minocc) (string_of_occurs maxocc)
      end
  | TSequence(xtype1,xtype2) ->
      if p > 1 then
        fprintf ff "@[<hv 1>(%a,@ %a)@]"
	  (print_xtype_prec 1) xtype1 (print_xtype_prec 1) xtype2
      else
        fprintf ff "%a,@ %a"
	  (print_xtype_prec 1) xtype1 (print_xtype_prec 1) xtype2
  | TEmpty ->
      fprintf ff "()"
  | TChoice(xtype1,xtype2) -> 
      if p > 0 then
	fprintf ff "@[<hv 1>(%a |@ %a)@]"
	  (print_xtype_prec 0) xtype1 (print_xtype_prec 0) xtype2
      else
	fprintf ff "%a |@ %a"
	  (print_xtype_prec 0) xtype1 (print_xtype_prec 0) xtype2
  | TNone ->
      fprintf ff "none"
  | TInterleave(xtype1,xtype2) ->
      if p > 2 then
	fprintf ff "@[<hv 1>(%a &@ %a)@]" 
	  (print_xtype_prec 2) xtype1 (print_xtype_prec 2) xtype2
      else
	fprintf ff "%a &@ %a" 
	  (print_xtype_prec 2) xtype1 (print_xtype_prec 2) xtype2


(**************************************)
(* Print a complex type specification *)
(**************************************)

and print_ctype_spec ff ctypespec = 
  match ctypespec.pctype_specifier_desc with
  | TTypeRef tname ->   
      fprintf ff " of type %a@;<0 -2>" print_uqname tname
  | TAnonymous xtderiv -> 
      fprintf ff " %a" print_ctype_derivation xtderiv


(******************************)
(* Print a type specification *)
(******************************)

and print_xtype_spec ff = function
  | TSpecSimple sspec -> fprintf ff " %a" print_stype_spec sspec
  | TSpecComplex cspec -> print_ctype_spec ff cspec


and print_ctype_derivation ff = function
  | (der, None, mixed, cxtype) ->
      fprintf ff "%a%a{%a@;<0 -2>}"
        print_deriv_opt der
	print_mixed mixed
	print_xtype cxtype
  | (der, Some axtype, mixed, cxtype) ->
      fprintf ff "%a%a{%a@,;%a@;<0 -2>}"
        print_deriv_opt der
	print_mixed mixed
	print_xtype cxtype
        print_xtype axtype

let print_xtype_derivation ff = function 
  | TSimpleDerivation sder -> print_stype_derivation ff sder
  | TComplexDerivation cder -> print_ctype_derivation ff cder

let print_xelem_derivation ff (substfor, nillable, xtypespec) = 
  fprintf ff 
    "%a%a%a"
    print_substitutes_for substfor print_nillable nillable
    print_xtype_spec xtypespec  


(****************************)
(* Print a type declaration *)
(****************************)

let print_type_decl ff td = 
  match td.pxtype_declaration_desc with
  | TAttributeDecl(aname,stypspec) -> 
      fprintf ff 
	"@[<hv 2>declare attribute %a @,%a;@]" 
	print_uqname aname
	print_stype_spec stypspec
  | TElementDecl(ename, elemder) -> 
      fprintf ff 
	"@[<hv 2>declare element %a%a;@]" 
	print_uqname ename
	print_xelem_derivation elemder
  | TTypeDecl(tname,TSimpleDerivation sder) ->
      fprintf ff "@[<hv 2>declare simple type %a %a;@]" 
	print_uqname tname
	print_stype_derivation sder
  | TTypeDecl(tname,TComplexDerivation cder) ->
      fprintf ff "@[<hv 2>declare complex type %a %a;@]"
	print_uqname tname
	print_ctype_derivation cder
  | TGroupDecl(gname,xtype) -> 
      fprintf ff "@[<hv 2>declare group %a {@,%a@;<0 -2>};@]"
	print_uqname gname
	print_xtype xtype
  | TAttrGroupDecl(gname,xtype) -> 
      fprintf ff "@[<hv 2>declare attrGroup %a {@,%a@;<0 -2>};@]"
	print_uqname gname
	print_xtype xtype


(************************************)
(* Print a set of type declarations *)
(************************************)

let print_issds ff typedecls = 
  List.iter (fun td -> fprintf ff "%a@\n" print_type_decl td) typedecls


(************************)
(* Print a whole schema *)
(************************)

let print_namespace_decls ff nsdecls = 
  let print_one ff = function
    | ("", uri) ->
	fprintf ff "declare default element namespace = %s;@\n" (Namespace_names.quoted_string_of_uri uri)
    | (ncname, uri) ->
	fprintf ff "declare namespace %s = %s;@\n" ncname (Namespace_names.quoted_string_of_uri uri)
  in
  let rec print_all ff = function
    | nsd :: [] -> fprintf ff "%a@\n" print_one nsd
    | nsd :: rest -> fprintf ff "%a%a" print_one nsd print_all rest
    | [] -> ()
  in print_all ff nsdecls

let rec print_xschema ff xs =
  fprintf ff "@[<hv 2>declare schema {@,%a%a%a@;<0 -2>};@]\n@?"
    print_namespace_decls xs.xschema_namespace_declarations
    print_xschemas xs.xschema_imported_schemas
    print_issds xs.xschema_type_declarations

and print_xschemas ff xss =
  match xss with
  | [] -> ()
  | xs :: xss' ->
      print_xschema ff xs;
      print_xschemas ff xss'

let bprintf_xschema s xs = 
  Gmisc.bprintf_stub s print_xschema xs 

