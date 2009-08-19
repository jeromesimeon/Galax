(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xquery_type_core_ast.mli,v 1.14 2007/02/01 22:08:45 simeon Exp $ *)

(* Module Xquery_type_ast
   Description:
     This *interface* contains type declarations for the XQuery type
     system abstract syntax tree.
*)

open Namespace_symbols
open Namespace_symbols_util

open Xquery_common_ast


(*************)
(* Types AST *)
(*************)

(* Substitution group *)

type csubstitutes_for =
  | CSubstitutesFor of relem_symbol
  | CNonSubstitutesFor

(* Content model *)

type cxtype =
    { mutable pcxtype_desc : cxtype_desc;
      mutable pcxtype_simplified : bool;
      pcxtype_loc  : Finfo.finfo }

and cxtype_desc =
  | CAtomicRef      of rtype_symbol
  | CElementRef     of relem_symbol                (* Global element reference *)
  | CAttributeRef   of rattr_symbol                (* Global attribute reference *)
  | CElementLocal   of relem_symbol * nillable * rtype_symbol  (* Local element declaration *)
  | CAttributeLocal of rattr_symbol * rtype_symbol (* Local attribute declaration *)
  | CDocument       of cxtype                      (* Document type *)
  | CText
  | CPI             of string option
  | CComment
  | CBound          of cxtype * Occurrence.occurs * Occurrence.occurs
  | CSequence       of cxtype * cxtype
  | CEmpty
  | CChoice         of cxtype * cxtype
  | CNone
  | CInterleave     of cxtype * cxtype


(* Type derivation *)

(*                    derived-from-type * attribute-content * child-content *)
and cxtype_derivation = rtype_symbol * cattribute_content * cchildren_content

and celem_derivation = csubstitutes_for * nillable * rtype_symbol

and cattr_derivation = rtype_symbol

and cattribute_content = cxtype option

and cchildren_content =
  | CComplexTypeRestriction of mixed * cxtype
  | CComplexTypeExtension of mixed * cxtype
  | CAtomicTypeRestriction
  | CSimpleTypeList of rtype_symbol
  | CSimpleTypeUnion of rtype_symbol list

(* Global declarations *)

                        (* Type-name  * Derivation * Expanded-type option *)
type celem_declaration = relem_symbol * celem_derivation (* * expanded-type option *)
type cattr_declaration = rattr_symbol * cattr_derivation (* * expanded-type option *)
type ctype_declaration = 
  {
    ctypedecl_name   : rtype_symbol;                (* Type name *)
    ctypedecl_deriv  : cxtype_derivation;           (* Derivation *)
    (* mutable ctype_id   : (int, int) option;   (* Unique id *) *)
    mutable ctypedecl_cxtype : cxtype option;       (* Expanded-type expression *)
  }

(* rtype_symbol * (* Unique-type identifier pre,post *) cxtype_derivation (* * expanded-type option *) *)

(*
   Schema:
   A Core schema contains element, attribute, and type declarations,
   each of which are keyed on the (uri, ncname) of the declared schema
   component.
*)
type cxschema =
    { cxschema_letter_mappings        : Xquery_type_core_ast_annotation.letter_mappings;
      cxschema_element_declarations   : celem_declaration SQNameHashtbl.t;
      cxschema_attribute_declarations : cattr_declaration SQNameHashtbl.t;
      cxschema_type_declarations      : ctype_declaration SQNameHashtbl.t }

(* Mapping from type to its directly derived types 
   rtype_symbol -> rtype_symbol list 
*)

