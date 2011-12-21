(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xquery_ast.mli,v 1.73 2007/11/16 21:16:52 mff Exp $ *)

(* Module: Xquery_ast
   Description:
     This *interface* contains type declarations for the XQuery
     language abstract syntax tree.
*)

open Namespace_names

open Xquery_common_ast
open Xquery_type_ast

(**************)
(* Node kinds *)
(**************)

type element_test =
  | SchemaElementTest of ename
  | ElementTest of (ename * tname option) option

type attribute_test =
  | SchemaAttributeTest of aname
  | AttributeTest of (aname * tname option) option

type kind_test =
  | DocumentKind of element_test option     (* ./document-node() *)
  | ElementKind of element_test             (* ./element() *)
  | AttributeKind of attribute_test         (* ./attribute() *)
  | PIKind of string option  	 	    (* ./pi("xxx") *)
  | CommentKind              	 	    (* ./comment () *)
  | TextKind                 	 	    (* ./text() *)
  | AnyKind                  	 	    (* ./node() *)


(******************)
(* Sequence types *)
(******************)

type itemtype =
  | ITKindTest of kind_test
  | ITTypeRef of tname                               (* type(QName) *)
  | ITItem
  | ITNumeric
  | ITAnyString
  | ITAtomic of tname
  | ITEmpty

type sequencetype_desc = itemtype * (Occurrence.occurs * Occurrence.occurs) option

type sequencetype = 
    { psequencetype_desc: sequencetype_desc;
      psequencetype_loc: Finfo.finfo }


(*****************************)
(* XPath 2.0 data structures *)
(*****************************)

(* Node tests *)

type node_test =
  | PNameTest of uqname           (* ./a or ./*:b or ./b:* or ./* *)
  | PNodeKindTest of kind_test


(**********************)
(* XQuery expressions *)
(**********************)

(* Patterns used in typeswitch *)

type pattern =
    { ppattern_desc: pattern_desc;
      ppattern_loc: Finfo.finfo }

and pattern_desc =
  | Case of sequencetype
  | Default

type expr =
    { pexpr_desc: expr_desc;
      pexpr_loc: Finfo.finfo }

and expr_desc =
  | EUnordered of expr list                              (* unordered { Expr } *)
  | EOrdered of expr list                                (* ordered { Expr } *)

  | EBinaryOp of expr * binop * expr                     (* Binary operators: 3 + 2 *)
  | EFLWOR of fl_expr list *
	expr option * order_by option * expr             (* FLWORs *)
  | EIf of expr * expr * expr                            (* if (e1) then e2 else e3 *)
  | ESome of (sequencetype option * vname * expr) list * expr
                                                         (* existential *)
  | EEvery of (sequencetype option * vname * expr) list * expr
                                                         (* universal *)
  | ETypeswitch of expr *                                (* typeswitch e1 case ... *)
	(pattern * vname option * expr) list
  | EInstanceOf of expr * sequencetype                   (* e1 instanceof t1 *)
  | ERange of expr * expr                                (* e1 to e2 *)
  | EUnaryOp of unaryop * expr                           (* +2, -$v, ... *)
  | ERoot                                                (* / *)
  | EPath of path_expr                                   (* e1/e2 ... *)
  | ESelf                                                (* Self           : . *)
  | EParent                                              (* Parent : .. *)
  | EVar of vname                                        (* Variables : $v, $a:b *)
  | EScalar of literal                                   (* Literals: 2.1 "John" *)
  | EApp of fname * (expr list)                          (* FunctionCall   : f($v) *)
  | EList of expr list                                   (* (e1,...,en) *)
  | EEnclosed of expr                                    (* Enclosed expression *)
  | ECast of expr * sequencetype                         (* cast as *)
  | ECastable of expr * sequencetype                     (* castable as *)
  | ETreat of expr * sequencetype                        (* downcast - no conversion *)
  | EValidate of validation_mode option * expr           (* XML Schema validation *)
  | EElemFixed of ename * expr list * expr list          (* <a b1="">...</a> *)
  | EElemComputed of expr list * expr list                    (* element { e1 } { e2 } *)
  | EAttrFixed of aname * expr list                      (* attribute a { Expr } *)
  | EAttrComputed of expr list * expr list                    (* attribute { e1 } { e2 } *)
  | EDocument of expr list                               (* document { ... } *)
  | EText of string                                      (* text { "This is text" } *)
  | ECharRef of int                                      (* &#x00; *)
  | ETextComputed of expr list                           (* text { e1, ..., ek } *)
  | EPI of ncname * string                               (* <? ... ?> *)
  | EPIComputed of expr list * expr list                 (* processing-instruction (NCNAME|{ e }) { e } *)
  | EComment of string                                   (* <!-- ... --> *)
  | ECommentComputed of expr list                        (* comment { e1, ..., ek } *)
  | EPragma of uqname * string * expr list               (* (# S? QName Content #) { Expr } *)
  (* DXQ expressions *)
  | ELetServerImplement of ncname * ncname * expr * expr (* let server NCName implement NCName at Expr return { e } *)
  | EForServerClose of ncname * ncname * expr       (* for server NCName implement NCName box { e } *)
  | EExecute of bool * ncname * expr                (* at server NCName do { e } or from server NCName return { e } *)
  | EEvalClosure of expr                            (* eval box { e } *)

  (* Update-related expressions *)
  | ECopy of expr
  | ESnap of snap_modifier * expr
  (* Update expressions *)
  | EDelete of snap * expr
  | EInsert of snap * expr * insert_location
  | ERename of snap * expr * expr
  | EReplace of snap * value_of_flag * expr * expr
  | ERevalidate of snap * validation_mode option * expr  (* XML Schema validation *)
  | ETransform of copyvar_expr list *
	  expr * expr             (* transform copy $v1:=e1,... modify ... return ... *)
  | EWhile of expr * expr     (* while E1 return E2 *)
  | ELetvar of sequencetype option * vname * expr * expr     (* letvar $v := E1 return E2 *)
  | ESet of vname * expr (* set  $v := E *)
  | EBlock of (block_decl_expr list)  * (expr list)

and path_expr =
  | PSlash of expr * expr                                (* e1/e2 *)
  | PSlashSlash of expr * expr                           (* e1//e2 *)
  | PAxis of axis * node_test                            (* axis::nodetest *)
  | PStepQualifiers of bool * expr * step_qualifier list (* e1[e2][e3]=>e1... *)

and step_qualifier =
    { pstep_qualifier_desc: step_qualifier_desc;
      pstep_qualifier_loc: Finfo.finfo }

and step_qualifier_desc =
  | PredicateQualifier of expr

and fl_expr =
    { pfl_desc: fl_expr_desc;
      pfl_loc: Finfo.finfo }

and fl_expr_desc =
  | ELet of sequencetype option * vname * expr
  | EFor of sequencetype option * vname * vname option * expr
       (* Second optional vname is for the 'at $i' index variable - Jerome *)

and order_by =
    stablekind * order_spec list

and order_spec = expr * sortkind * emptysortkind option

and insert_location =
  | EAsLastInto of expr
  | EAsFirstInto of expr
  | EInto of expr
  | EAfter of expr
  | EBefore of expr

and snap =
  | Snap
  | NoSnap

and copyvar_expr_desc = vname * expr

and copyvar_expr = 
    { pcopyvar_desc: copyvar_expr_desc;
      pcopyvar_loc: Finfo.finfo }

and block_decl_expr = 
    { bl_decl_desc: block_decl_desc;
      bl_decl_loc: Finfo.finfo }
      
and block_decl_desc = sequencetype option * vname * expr

(**************)
(* Statements *)
(**************)

type statement = expr


(****************)
(* Query module *)
(****************)

(* Global functions *)

type function_body =
  | EFunctionBltIn
  | EFunctionInterface
  | EFunctionImported
  | EFunctionUser of expr

type function_signature = sequencetype option list * sequencetype option

type function_def =
    { pfunction_def_desc : function_def_desc;
      pfunction_loc      : Finfo.finfo }

and function_def_desc = fname * (vname list) * function_signature * function_body * updating_modifier
    (* if the last flag is true, then it is an updating function *) 

(* Global variable declaration *)

type var_body = 
  | EVarExternal
  | EVarInterface
  | EVarImported
  | EVarUser of expr

type var_decl =
    { pvar_decl_desc : var_decl_desc;
      pvar_decl_loc  : Finfo.finfo }

and var_decl_desc = vname * sequencetype option * var_body 

(* DXQ Server declaration *)

type server_decl =
    { pserver_decl_desc : server_decl_desc;
      pserver_decl_loc  : Finfo.finfo }

and server_decl_desc = ncname * ncname * expr

(* Index definitions *)

type index_def =
    { pindex_def_desc : index_def_desc;
      pindex_def_loc  : Finfo.finfo }

and index_def_desc =
  | ValueIndex of string * expr * expr
  | NameIndex of ename

(* Context declarations *)

type context_decl =
    { pcontext_decl_desc : context_decl_desc;
      pcontext_decl_loc  : Finfo.finfo }

and context_decl_desc =
  | EBaseURIDecl of string
  | ENamespaceDecl of namespace_declaration
  | EDefaultElementNamespaceDecl of uri
  | EDefaultFunctionNamespaceDecl of uri
  | ESchemaDecl of (prefix option * string * string option)
  | EImportServiceDecl of (ncname * string * string option)
  | EImportModuleDecl of (ncname * string * string option)
  | EXmlSpaceDecl of strip_or_preserve
  | EDefaultCollationDecl of string
  | EConstructionDecl of strip_or_preserve
  | EOrderingDecl of ordered_or_unordered
  | EDefaultEmptyOrderDecl of emptysortkind
  | ECopyNamespacesDecl of (preserve_or_no_preserve * inherit_or_no_inherit)
(* DXQ declarations *)
  | EImportInterfaceDecl of (ncname * string * string option)

(********************)
(* XQuery toplevels *)
(********************)

(* Modules *)

type funcvar_def =
  | OptionDecl of ename * string
  | FunDef of function_def
  | VarDef of var_decl
  | ServerDef of server_decl

type prolog =
    { mutable pprolog_xschemas 	: xschema list;
      mutable pprolog_contexts 	: context_decl list;
      mutable pprolog_funcvars  : funcvar_def list;
      mutable pprolog_indices  	: index_def list }

type interface_prolog =
    { mutable iprolog_xschemas 	: xschema list;
      mutable iprolog_contexts 	: context_decl list;
      mutable iprolog_funcvars  : funcvar_def list }

type interface = 
    { pinterface_decl : (ncname * string);
      pinterface_prolog : interface_prolog;
    } 

type library_module =
                             (* Prefix, URI, optional Interface URI & Location *)
    { plibrary_module_decl : (ncname * string * (string * string option) option );
      plibrary_module_prolog : prolog }

type main_module =
    { pmain_module_prolog     : prolog;
      pmain_module_statements : statement list }

type xmodule =
  | ELibraryModule of library_module
  | EMainModule of main_module

(**************************)
(* For use in other AST's *)
(**************************)

(* Handle to the original expression *)

type expr_handle = expr option

