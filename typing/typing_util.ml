(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: typing_util.ml,v 1.25 2008/03/21 19:02:32 simeon Exp $ *)

(* Module: Typing_util
   Description:
     This modules implements some basic operations used during static
     typing.
 *)

open Error

open Namespace_symbols_builtin

open Xquery_common_ast
open Xquery_core_ast
open Xquery_type_core_ast
open Xquery_type_core_ast_util

open Xquery_algebra_ast

open Schema_builtin
open Schema_util

(***************************)
(* Sequence type factoring *)
(***************************)

let factor_sequencetype dt =
  match dt.pcsequencetype_desc with
  | (CITEmpty, _) ->
      (CITEmpty, Occurrence.occurs 0, Occurrence.occurs 0)
  | (dtk, None) ->
      (dtk,Occurrence.occurs 1,Occurrence.occurs 1)
  | (dtk,(Some (m,n))) ->
      (dtk,m,n)

let factor_asequencetype dt =
  match dt.pasequencetype_desc with
  | (AITEmpty, _) ->
      (AITEmpty, Occurrence.occurs 0, Occurrence.occurs 0)
  | (dtk, None) ->
      (dtk,Occurrence.occurs 1,Occurrence.occurs 1)
  | (dtk,(Some (m,n))) ->
      (dtk,m,n)

let is_itemstar_sequencetype dt =
  match dt.pcsequencetype_desc with
  | (CITItem, Some (m,n)) ->
      (Occurrence.equal m (Occurrence.occurs 0)) && 
      (Occurrence.equal n (Occurrence.UNBOUNDED)) 
  | _ -> false
      

let is_just_a_complex_type cxt = 
  match cxt.pcxtype_desc with 
  | CBound      _
  | CSequence   _
  | CChoice     _ 
  | CInterleave _ -> true
  | _ -> false

(******************)
(* Type promotion *)
(******************)

(*
  [FS Section 8.5.1]

  can_be_promoted_to_judge determines whether t1 can be promoted to t2
  and returns (Some t2) if it can be promoted; None, otherwise. 
*)
let rec can_be_promoted_to_judge schema t1 t2 =
  (* Printf.printf "\t\t[DEBUG] Type %s can be promoted to Type %s:" (Print_top.bprintf_cxtype "" t1) (Print_top.bprintf_cxtype "" t2); *)
  let promote_type = 
    try
      (* 
	 These two rules are implemented together:
	 
	 statEnv |- prime(Type1) can be promoted to prime(Type2)      
	 quantifier(Type1) <= quantifier(Type2)
	 -------------------------------------------------------
	 statEnv |- Type1 can be promoted to Type2
	 
	 Type promotion distributes over occurrence and union
	 constructors:
	 
	 statEnv |- prime(Type1) can be promoted to prime(Type1')      
	 prime(Type2) can be promoted to prime(Type2')
	 ---------------------------------------------------------------
	 statEnv |- (Type1 | Type2) can be promoted to (Type1' | Type2')
	 
	 This case is only applied when neither t1 nor t2 is an item type.
       *)
      if (is_just_a_complex_type t1 || is_just_a_complex_type t2) then 
	begin
	  let (pl1,m1,n1) = factor_with_units t1 in
	  let (pl2,m2,n2) = factor_with_units t2 in
	  if (Occurrence.le m2 m1 && Occurrence.le n1 n2) then 
	    if (Subtyping_top.is_subtype_of_empty_sequence schema t1)
	    then
	      Some t2
	    else
	      if (List.for_all (fun p1 ->
		List.exists (fun p2 ->
		  Gmisc.is_some (can_be_promoted_to_judge schema p1 p2)
			    ) pl2) pl1)
	      then
(* ?! EEK!! This is returning the original union type, instead
   of the subset of types to which the input type can be
   promoted. !! Should it? *)
		Some t2 
	      else
		None
	  else
	  None 
	end
    else
	begin
	  match (t1.pcxtype_desc, t2.pcxtype_desc) with 
	  | (CAtomicRef r1, CAtomicRef r2) -> 
	      begin
		(*  
	           statEnv |- Type1 <: Type2
		   -----------------------------------------
		   statEnv |- Type1 can be promoted to Type2
		 *)
		if (Schema_judge.derives_from schema r1 r2) then
		  Some t2
		else if (Schema_judge.derives_from schema r2 r1) then
		  None
		    (* statEnv |- xs:untypedAtomic can be promoted to Type *)
		else if (Schema_judge.derives_from schema r1 Namespace_symbols_builtin.xs_untypedAtomic) then 
		  Some t2
		    (* statEnv |- xs:integer can be promoted to xs:decimal or xs:float or xs:double *)
		else if (Schema_judge.derives_from schema r1 Namespace_symbols_builtin.xs_integer
			   && (Schema_judge.derives_from schema r2 Namespace_symbols_builtin.xs_decimal
			     || Schema_judge.derives_from schema r2 Namespace_symbols_builtin.xs_float
			     || Schema_judge.derives_from schema r2 Namespace_symbols_builtin.xs_double)) then 
		  Some t2
		    (* statEnv |- xs:decimal can be promoted to xs:float or xs:double *)
		else if (Schema_judge.derives_from schema r1 Namespace_symbols_builtin.xs_decimal
			   && (Schema_judge.derives_from schema r2 Namespace_symbols_builtin.xs_float
			     || Schema_judge.derives_from schema r2 Namespace_symbols_builtin.xs_double)) then 
		  Some t2
		    
		    (* statEnv |- xs:float can be promoted to xs:double *)
		else if (Schema_judge.derives_from schema r1 Namespace_symbols_builtin.xs_float
			   && Schema_judge.derives_from schema r2 Namespace_symbols_builtin.xs_double) then 
		  Some t2
		    (* statEnv |- xs:anyURI can be promoted to xs:string *)
		else if (Schema_judge.derives_from schema r1 Namespace_symbols_builtin.xs_anyURI
			   && Schema_judge.derives_from schema r2 Namespace_symbols_builtin.xs_string) then 
		  Some t2
		else
		  None
	      end
	  |	(CEmpty, CEmpty) -> Some t2
	  |	(CNone, CNone) -> Some t2
	  |	_ -> None
	end	    
    with
    | exn -> raise(Query(Internal_Error(bprintf_error ("In can_be_promoted_to_judge t1 = "^(Print_top.bprintf_cxtype "" t1)^" t2 = "^(Print_top.bprintf_cxtype "" t2)) exn)))
  in
(*
  begin
    match promote_type with
    | None -> 
	Printf.printf " NONE\n"
    | Some typ ->
	Printf.printf " %s\n" (Print_top.bprintf_cxtype "" typ)
  end;
*)
  promote_type

(*
   This function can be implemented very efficiently when the type
   hierarchy is encoded in (pre,post) symbols.

   For the moment, it just finds the least common type, with promotion, in the list.
*) 
let least_common_promoted_type cxschema ctype_list = 
  let lct_aux lct t = 
    if (Gmisc.is_some(can_be_promoted_to_judge cxschema t lct)) then lct
    else if (Gmisc.is_some(can_be_promoted_to_judge cxschema lct t)) then t
    else raise Not_found
  in 
  match ctype_list with 
  | [] -> cxtype_anyAtomic
  | _ -> List.fold_left lct_aux (List.hd ctype_list) (List.tl ctype_list)

(*  
   [FS Section 7.2.6]  data_on Judgment   
*)
let rec data_on_judge schema t = 
  let data_on_type = 
  (* statEnv |- data_on empty : empty *)
    if (Subtyping_top.is_subtype_of_empty_sequence schema t) then 
      cxtype_empty
  (* statEnv |- data_on none : none *)
    else if (Subtyping_top.is_subtype_of_empty_choice schema t) then 
      cxtype_none
    else 
      match t.pcxtype_desc with 
      (*
         The general rule for fn:data is to apply the filter data_on to
         the prime type of its argument type, then apply the quantifier
         to the result:
	 
         statEnv |- Expr : Type
         statEnv |- data on prime(Type) : Type1
         --------------------------------------
         statEnv |- fn:data(Expr) : Type1 · quantifier(Type)
      *)
      | CBound _ 
      | CSequence _ 
      | CInterleave _ -> 
	  let (p, m, n) = factor t in
	  let t1 = data_on_judge schema p in
	  (fmkcxtype (CBound(t1, m, n)) t.pcxtype_loc)
      (*
         When applied to the union of two types, data_on is applied to
         each of the two types. The resulting type is computed using
         prime and quantifier, which are defined in [8.4 Judgments for
         FLWOR and other expressions on sequences]. This rule is
         necessary because data_on may return a sequence of atomic
         types.

         statEnv |- data on Type1 : Type1'
         statEnv |- data on Type2 : Type2'
         ----------------------------------------------------------------------------------
         statEnv |- data on (Type1|Type2) : prime(Type1'|Type2') · quantifier(Type1'|Type2')
      *)
      | CChoice(t1, t2) -> 
	  let t1' = data_on_judge schema t1 in
	  let t2' = data_on_judge schema t2 in
	  let (p, m, n) = factor (fmkcxtype (CChoice (t1', t2')) t.pcxtype_loc) in
	  (fmkcxtype (CBound (p, m, n)) t.pcxtype_loc)
      (* 
	 statEnv |- Type <: xs:anyAtomicType
	 ------------------------------------
	 statEnv |- data on Type : Type
      *)
      | CAtomicRef ctname -> t
     (* 
	When applied to an attribute node type, the data_on filter returns
	the attribute's simple type.

	statEnv |- AttributeType type lookup of type TypeName
	statEnv |- (of type TypeName) expands to Type
	-----------------------------------------------------
	statEnv |- data on AttributeType : Type
     *)
      | CAttributeRef   caname ->
          let (_, ctname) = Schema_judge.lookup_attribute_type schema t in Schema_judge.expands_attribute_to schema ctname 
      | CAttributeLocal (caname, ctname) ->
	  Schema_judge.expands_attribute_to schema ctname 
     (*
	When applied to text, and document node types, data_on returns xs:untypedAtomic
	
	statEnv |- Type <: text | document
	------------------------------------------
	statEnv |- data on Type : xs:untypedAtomic
     *)
      | CDocument _
      | CText ->
	  Schema_builtin.cxtype_untypedAtomic
     (* 
	When applied to comment or processing instruction node types, data_on returns xs:string
	
	statEnv |- Type <: comment | processing-instruction
	---------------------------------------------------
	statEnv |- data on Type : xs:string
     *)
      | CPI _
      | CComment -> Schema_builtin.cxtype_string
      | CElementRef cename ->
	  let (cename, csubstitutes_for, nillable, ctname) = Schema_judge.lookup_element schema cename in 
	  data_on_element schema t (nillable, ctname)
      | CElementLocal (cename, nillable, ctname) ->
	  data_on_element schema t (nillable, ctname)
      |	_ -> 
	  raise (Query (Prototype ("In data_on: Incompletely implemented for :"^(Print_top.bprintf_cxtype "" t))))
  in Schema_simplification.simplify_ty schema data_on_type

and data_on_element schema t (nillable, ctname) = 
(* 
   When applied to element node types with type annotation
   xs:untyped, the data_on filter returns xs:untypedAtomic.
   
   statEnv |- ElementType type lookup of type xs:untyped
   ------------------------------------------------------
   statEnv |- data on ElementType : xs:untypedAtomic
*)
  if (Namespace_symbols.symbol_equal ctname Namespace_symbols_builtin.xs_untyped) then 
    Schema_builtin.cxtype_untypedAtomic
  else
    let (cxtype, content_kind, mixed) = Schema_judge.expands_to schema (nillable, ctname) in
    (*
       When applied to an element type whose type annotationXQ
       denotes a complex type of mixed content, the data_on
       filter returns xs:untypedAtomic.
       
       statEnv |- ElementType type lookup of type TypeName
       statEnv |- TypeName of elem/type expands to expanded-QName      
       statEnv.typeDefn(expanded-QName) = define type TypeName Derivation? mixed { Type1? }
       ------------------------------------------------------------------------------------
       statEnv |- data on ElementType : xs:untypedAtomic
    *)
    if (mixed = Mixed) then Schema_builtin.cxtype_untypedAtomic
    (*
       When applied to an element type whose type
       annotationXQ denotes a simple type or a complex type
       of simple content, data_on returns the element's
       simple type.
       
       statEnv |- ElementType type lookup TypeReference
       statEnv |- TypeReference expands to Type
       statEnv |- Type <: (attribute *, Type1)      
       statEnv |- Type1 <: xs:anyAtomicType*
       ------------------------------------------------
       statEnv |- data on ElementType : Type1
    *)
    else 
      begin
	match content_kind with
	| SimpleKind sk -> Schema_judge.atomic_type_of_simple_kind schema sk
	| _ ->
        (*
	   The data_on filter is not defined on any element type
	   whose type annotation denotes a complex type of
	   complex content and therefore apply data_on to such a
	   node raises a static error.  
	*)
	    raise (Query (Static_Type_Error ("fn:data() not defined on type :"^(Print_top.bprintf_cxtype "" t))))
      end

(*
   [FS 8.6.1 Elements in validation mode]

   If the element name is globally declared in the schema, it resolves
   to the element type of the corresponding global element
   declaration, independently of the validation mode.

   statEnv |- ElementName of elem/type expands to expanded-QName
   statEnv.elemDecl(expanded-QName) = define ElementType
   -----------------------------------------------------------------------
   statEnv |- ElementName with mode ValidationMode resolves to ElementType

   If an element name is not globally defined and the validation mode
   is lax, then the element name resolves to the element type with the
   given element name with any content type.

   statEnv |- ElementName of elem/type expands to expanded-QName
   statEnv.elemDecl(expanded-QName) undefined
   ---------------------------------------------------------------------------------------
   statEnv |- ElementName with mode lax resolves to element ElementName of type xs:anyType

*)

let validate_element_resolves_to schema mode elemtype = 
  match elemtype.pcxtype_desc with
  | CElementRef cename -> elemtype
  | CElementLocal (cename, nillable, ctname) ->
      begin
	try
	  let (cename', _, _, _) = Schema_judge.lookup_element schema cename in 
	  fmkcxtype (CElementRef cename') elemtype.pcxtype_loc
	with 
	| Not_found -> 
	    if (mode = Lax) then 
		fmkcxtype (CElementLocal(cename,nillable,Namespace_symbols_builtin.xs_anyType)) elemtype.pcxtype_loc 
	    else
	      raise(Query(Static_Type_Error("Validation mode is Strict but element not globally declared: "^(Print_top.bprintf_cxtype "" elemtype))))
      end
  | CDocument _ -> elemtype
  | _ -> 
      raise(Query(Internal_Error("In validate_element_resolves_to: Expected element or document type, found: "^(Print_top.bprintf_cxtype "" elemtype))))

(* Expand all possible combinations of argument types to an overloaded function *)
let expand_overloaded_arguments schema argument_types = 
  if List.length argument_types = 1 then   
    begin
      let argument_type = Args.get_param1 argument_types in
      let (ul,m,n) = factor_with_units argument_type in
      (List.map (fun u -> [u]) ul, m, n)
    end
  else if List.length argument_types = 2 then   
    begin
      let (argument_type1, argument_type2) = Args.get_param2 argument_types in
      let (ul1,m1,n1) = factor_with_units argument_type1 in
      let (ul2,m2,n2) = factor_with_units argument_type2 in
      let m = Occurrence.ub_min m1 m2
      in
      let n = Occurrence.ub_min n1 n2
      in
      (List.concat (List.map (fun u1 -> List.map (fun u2 -> [u1; u2]) ul2) ul1), m, n)
    end
  else raise(Query(Internal_Error("Overloaded function call has more than two arguments")))

(* Expand all possible combinations of argument types to an overloaded function *)
let expand_first_overloaded_argument schema argument_types = 
  if List.length argument_types = 1 then   
    begin
      let argument_type = Args.get_param1 argument_types in
      let (ul,m,n) = factor_with_units argument_type in
      (ul, None, m, n)
    end
  else if List.length argument_types = 2 then   
    begin
      let (argument_type1, argument_type2) = Args.get_param2 argument_types in
      let (ul1,m1,n1) = factor_with_units argument_type1 in
      let (ul2,m2,n2) = factor_with_units argument_type2 in
      let m = Occurrence.ub_min m1 m2
      in
      let n = Occurrence.ub_min n1 n2
      in
      (ul1, Some argument_type2, m, n)
    end
  else
    raise 
      (Query(Internal_Error("Overloaded function call has more than two arguments")))


