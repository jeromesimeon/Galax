(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xquery_common_ast.mli,v 1.22 2007/02/01 22:08:45 simeon Exp $ *)

(* Module Xquery_common_ast
   Description:
     This *interface* contains some common type declarations for the
     XQuery abstract syntax tree.
*)

open Namespace_names


(*********)
(* Names *)
(*********)

(* Unresolved QNames are used in the AST's *)

type tname       = uqname (* type name *)
type gname       = uqname (* group name *)
type ename       = uqname (* element name *)
type aname       = uqname (* attribute name *)
type sname       = uqname (* schema name *)
type vname       = uqname (* variable name *)
type fname       = uqname (* function name *)
type fname_arity = uqname * int (* function name with a given arity *)

(* Resolved QNames are used in the core AST's *)

type ctname       = rqname (* type name *)
type cename       = rqname (* element name *)
type caname       = rqname (* attribute name *)
type cvname       = rqname (* variable name *)
type cfname       = rqname (* function name *)
type cfname_arity = rqname * int (* function name with a given arity *)

(* This should be fixed later *)

type crname     = cvname (* row name in a tuple *)


(**************************)
(* Namespace declarations *)
(**************************)

type namespace_declaration = (ncname * uri)


(*********)
(* Types *)
(*********)

(* Nillable *)

type nillable =
  | Nillable
  | NonNillable

(* Mixed *)

type mixed =
  | Mixed
  | NonMixed


(*********************)
(* XPath expressions *)
(*********************)

(* Principal node kind *)

type principal =
  | PrincipalElement
  | PrincipalAttribute

(* XPath 2.0 operators *)

type unaryop =
  | UEPlus                                          (* +e1 *)
  | UEMinus                                         (* -e1 *)

type binop =
  | BEIntersect                                     (* e1 intersect e2 *)
  | BEUnion                                         (* e1 union e2 *)
  | BEExcept                                        (* e1 except e2 *)
  | BEBar                                           (* e1 | e2 *)

  | BEAnd                                           (* e1 and e2 *)
  | BEOr                                            (* e1 or e2 *)

  | BEPrecedes                                      (* e1 precedes e2 *)
  | BEFollows                                       (* e1 follows e2 *)

  | BEEq                                            (* e1 eq e2 *)
  | BENEq                                           (* e1 ne e2 *)
  | BELtOp                                          (* e1 lt e2 *)
  | BELte                                           (* e1 le e2 *)
  | BEGtOp                                          (* e1 gt e2 *)
  | BEGte                                           (* e1 ge e2 *)

  | BEEqual                                         (* e1 = e2 *)
  | BENEqual                                        (* e1 != e2 *)
  | BEIs                                            (* e1 is e2 *)
  | BELt                                            (* e1 < e2 *)
  | BEGt                                            (* e1 > e2 *)
  | BELteq                                          (* e1 <= e2 *)
  | BEGteq                                          (* e1 >= e2 *)

  | BEPlus                                          (* e1 + e2 *)
  | BEMinus                                         (* e1 - e2 *)

  | BEMult                                          (* e1 * e2 *)
  | BEDiv                                           (* e1 div e2 *)
  | BEIDiv                                          (* e1 idiv e2 *)
  | BEMod                                           (* e1 mod e2 *)


(* XPath axis *)

type axis =
  | Ancestor
  | Ancestor_or_self
  | Attribute
  | Child
  | Descendant
  | Descendant_or_self
  | Following_sibling
  | Preceding_sibling
  | Parent
  | Self
  (* full axis feature -- Philippe *)
  | Following
  | Preceding




(**********************)
(* XQuery expressions *)
(**********************)

(* Literal values *)

type literal =
  | IntegerLiteral of Decimal._integer
  | DecimalLiteral of Decimal._decimal
  | DoubleLiteral of float
  | StringLiteral of string
  | BooleanLiteral of bool
  | URILiteral of AnyURI._uri

val delimited_string_of_literal : string -> literal -> string
val string_of_literal : literal -> string
val literal_of_string : Datatypes.atomic_type -> string -> literal
val atomic_type_of_literal : literal -> Datatypes.atomic_type

(* Validation mode *)

type validation_mode =
  | Lax
  | Strict

(* Sort criteria used in orderby *)

type sortkind =
  | Ascending
  | Descending

type emptysortkind =
  | EmptyGreatest
  | EmptyLeast

type stablekind =
  | Stable
  | NonStable


(***************************)
(* XML updates expressions *)
(***************************)

type value_of_flag =
  | Normal_Replace
  | Value_Of_Replace


type snap_modifier = 
  | Snap_Ordered_Deterministic 
  | Snap_Unordered_Deterministic
  | Snap_Nondeterministic
 
type updating_modifier = Updating | NonUpdating


(**********)
(* Prolog *)
(**********)

type strip_or_preserve =
  | Strip
  | Preserve

type ordered_or_unordered =
  | Ordered
  | Unordered

type preserve_or_no_preserve =
  | NSPreserve
  | NSNoPreserve

type inherit_or_no_inherit =
  | NSInherit
  | NSNoInherit

(* Built-in names *)

(* Special 'fs:' variables: $fs:dot, $fs:sequence, $fs:position, and
   $fs:last *)

val fs_dot      : cvname
val fs_sequence : cvname
val fs_position : cvname
val fs_last     : cvname


