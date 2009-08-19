(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xquery_type_ast.mli,v 1.11 2007/02/01 22:08:45 simeon Exp $ *)

(* Module Xquery_type_ast
   Description:
     This *interface* contains type declarations for the XQuery type
     system abstract syntax tree.
*)

open Namespace_names

open Xquery_common_ast


(*************)
(* Types AST *)
(*************)

(* Simple types *)

type stype_specifier = 
    { pstype_specifier_desc : stype_specifier_desc;
      pstype_specifier_loc : Finfo.finfo }

and stype_specifier_desc = 
  | STypeRef of tname
  | SAnonymous of stype_derivation

and stype_derivation = 
  | SRestriction of stype_specifier
  | SList of stype_specifier
  | SUnion of stype_specifier list 



(* Derivation *)

type deriv =
  | TRestriction of tname
  | TExtension   of tname

(* Content model *)

type xtype =
    { pxtype_desc : xtype_desc;
      pxtype_loc  : Finfo.finfo }

and xtype_desc =
    (* Item types *)
  | TAtomicRef      of tname
  | TAttributeRef   of aname                     	   (* Global attribute reference *)
  | TAttributeLocal of aname * stype_specifier   	   (* Local attribute declaration *)
  | TElementRef     of ename                     	   (* Global element reference *)
  | TElementLocal   of ename * nillable * xtype_specifier  (* Local element declaration *)
  | TDocument       of xtype
  | TText
  | TProcessingInstruction
  | TComment

    (* Groups *)
  | TGroupRef       of gname                     	   (* Group reference *)
  | TAttrGroupRef   of gname                     	   (* Attribute group reference *)

    (* Content models *)
  | TBound          of xtype * Occurrence.occurs * Occurrence.occurs
  | TSequence       of xtype * xtype
  | TEmpty
  | TChoice         of xtype * xtype
  | TNone
  | TInterleave     of xtype * xtype

(* Type specifier *)

and xtype_specifier =
  | TSpecSimple of stype_specifier
  | TSpecComplex of ctype_specifier

and ctype_specifier =
    { pctype_specifier_desc : ctype_specifier_desc;
      pctype_specifier_loc  : Finfo.finfo }

and ctype_specifier_desc = 
  | TTypeRef   of tname               (* Type reference  : of type TypeName *)
  | TAnonymous of ctype_derivation


(* Complex type derivation *)

and ctype_derivation = 
    deriv option * xtype option * mixed * xtype
                 (* attrs *)       (* children *)


(* Type derivation *)

type xtype_derivation = 
  | TComplexDerivation of ctype_derivation
  | TSimpleDerivation of stype_derivation

(* Substitution group *)

type substitutes_for =
 | TSubstitutesFor of ename
 | TNonSubstitutesFor

type xelem_derivation = substitutes_for * nillable * xtype_specifier

(* Global declarations *)

type xtype_declaration =
    { pxtype_declaration_desc : xtype_declaration_desc;
      pxtype_declaration_loc  : Finfo.finfo }

and xtype_declaration_desc =
  | TAttributeDecl of aname * stype_specifier
  | TElementDecl   of ename * xelem_derivation
  | TTypeDecl      of tname * xtype_derivation
  | TGroupDecl     of gname * xtype
  | TAttrGroupDecl of gname * xtype   (*--VG++  is gname reuse ok? *)

(* Schema *)

type xschema =
    { xschema_imported_schemas : xschema list;  (* imported schemas *)
      xschema_namespace_declarations : namespace_declaration list;
      xschema_type_declarations : xtype_declaration list }

