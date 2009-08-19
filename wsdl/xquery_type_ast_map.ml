(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xquery_type_ast_map.ml,v 1.9 2007/07/05 14:58:44 ndonose Exp $ *)

(* Module Xquery_type_ast_map
   Description:
    Helping functions to build @see xquery_type_ast.mli type AST 
    using information from element and type declarations.
   
   Nicola: not yet enough tested
*)

open Xquery_ast
open Xquery_ast_util
open Xquery_type_ast
open Xquery_type_core_ast
open Namespace_names
open Error

open Wsdl_ast

let make_sequencetype d = 
  fmksequencetype 
    (d, 
     Some(Occurrence.occurs 0,
	  Occurrence.unbounded)) 
    Finfo.bogus  

let anytype = make_sequencetype ITItem


(**
  takes a list of strings representing (local) variable
  names and generates a name that is not in the list
*)
let create_new_var vlist init_name =
  let rec new_var v i = function
    | [] -> v
    | x::rest -> 
	if v=x then 
	  new_var (init_name ^ (string_of_int (i+1))) (i+1) rest
	else
	  new_var v i rest
  in
    new_var init_name 0 (List.sort (Pervasives.compare) vlist)
      


type kind_of_WSDL_type = 
    Kind_reference of uqname
  | Kind_atomic of uqname
  | Kind_list of uqname
  | Kind_complex of uqname
  | Kind_anyType of uqname

(** find the prefix for a given URI in an xschema *)
let rec lookup_prefix_for_uri schema uri = 
  try    
    (match (List.find (fun (name,u) -> u=uri) schema.xschema_namespace_declarations)
     with name,_ -> NSPrefix name)
  with Not_found -> 
    search_prefix_in_list_of_schemas schema.xschema_imported_schemas uri
and search_prefix_in_list_of_schemas schs uri =
  match schs with
    | [] -> raise Not_found
    | x::rest -> 
	try lookup_prefix_for_uri x uri 
	with Not_found -> search_prefix_in_list_of_schemas rest uri


let rec lookup_uri_for_str_prefix schema str_prefix = 
  try    
    (match (List.find (fun (name,u) -> name=str_prefix) schema.xschema_namespace_declarations)
     with _,uri -> uri)
  with Not_found -> 
    search_uri_in_list_of_schemas schema.xschema_imported_schemas str_prefix
and search_uri_in_list_of_schemas schs str_prefix =
  match schs with
    | [] -> raise Not_found
    | x::rest -> 
	try lookup_uri_for_str_prefix x str_prefix
	with Not_found -> search_uri_in_list_of_schemas rest str_prefix


(** find the URI for a given prefix in an xschema *)
let lookup_uri_for_prefix schema prefix = 
  match prefix with
      NSPrefix pref -> lookup_uri_for_str_prefix schema pref
    | _ -> raise Not_found	  


(** find the corresponding type declaration in the schema or raises Not_found *)
let rec lookup_type_in_xschema schema typ_name =
  let rec find_type_in_decls t = function
    | [] -> raise Not_found
    | x::decls' -> 
	begin
	  match x.pxtype_declaration_desc with
	    | TTypeDecl(tn, t_deriv) -> 
		if tn=t then t_deriv
		else find_type_in_decls t decls'
	    | _ -> find_type_in_decls t decls' 
	end
  in    
    try 
      find_type_in_decls typ_name schema.xschema_type_declarations
    with Not_found -> 
      find_type_in_schemas schema.xschema_imported_schemas typ_name
and find_type_in_schemas schs t =
  match schs with
    | [] -> raise Not_found
    | x::rest -> 
	try  lookup_type_in_xschema x t 
	with Not_found -> find_type_in_schemas rest t
	  

let rec wsdl_type_from_s_derivation = function
  | SRestriction specifier ->  (* atomic type *)
      begin
	match specifier.pstype_specifier_desc with
	  | STypeRef t_name -> Some (Kind_reference t_name)
	  | SAnonymous deriv -> wsdl_type_from_s_derivation deriv
      end
  | SList specif -> (* list of atomic *)
      begin
	match specif.pstype_specifier_desc with
	  | STypeRef t_name ->  Some (Kind_list t_name)
	  | SAnonymous _ -> None
      end
  | SUnion _ -> None


let get_wsdl_type_from_derivation = function
  | TComplexDerivation _ -> Some (Kind_complex Namespace_builtin.wild_uqname)
  | TSimpleDerivation s_deriv -> wsdl_type_from_s_derivation s_deriv


(** the 3 mappings defined by the WSDL/XQuery binding *)

(** atomic     ---> atomic *)
let mk_regular_atomic typ_name =
  Xquery_ast_util.mksequencetype (ITAtomic typ_name, None)
    
(** list       ---> atomic\* *)
let mk_elem_for_complex_type typ_name =
  Xquery_ast_util.mksequencetype (ITKindTest (ElementKind (ElementTest (Some (Namespace_builtin.wild_uqname, Some typ_name)))), None)

(** complexType ---> element(\*,complexType) *)
let mk_list_of_atomic typ_name =
  Xquery_ast_util.mksequencetype (ITAtomic typ_name, 
				  Some (Occurrence.UP_INT 0, Occurrence.UNBOUNDED))
    
let mk_anyType t_name =
  Xquery_ast_util.mksequencetype (ITTypeRef t_name, 
				  Some (Occurrence.UP_INT 1, Occurrence.UP_INT 1))

    
let rec get_itemtype_for_wsdl_type ((prefix, uri, name) as rqname) schema =
  let rsymbol  = Namespace_symbols.rtype_symbol rqname in
  let uq_name = Namespace_names.uqname_of_rqname rqname in
      (* if it's a builtin type*)
      (*       try  *)
      (*	let _ = Datatypes_util.lookup_bltin_type (Namespace_symbols.rtype_symbol rqname) *)
      
      try 
	let _ = List.find (fun d -> Namespace_symbols.rtype_equal rsymbol d.ctypedecl_name) 
	  Schema_builtin.built_in_xml_schema_type_decls
	in Some (Kind_atomic uq_name)
      with 
	| _ ->
	    (* not an atomic type *)
	    if name = "anyType" && 
	      (uri = Namespace_builtin.xs_uri || uri = Namespace_builtin.xsd_uri)  then
		Some (Kind_anyType uq_name)
	    else 
	      try 
		begin
		  let deriv = lookup_type_in_xschema schema uq_name in
		    match get_wsdl_type_from_derivation deriv with
		      | None -> None
		      | Some (Kind_anyType _) -> Some (Kind_anyType uq_name)
		      | Some (Kind_complex _) -> Some (Kind_complex uq_name)
		      | Some (Kind_atomic _) -> Some (Kind_atomic uq_name)
		      | Some (Kind_list atom_typ) as tlist -> tlist
		      | Some (Kind_reference ((pref,_) as ref_typ)) -> 
			  try
			    begin
			      let u = lookup_uri_for_str_prefix schema (string_of_prefix pref) in
				begin
				  match ref_typ with pref,n ->
				    get_itemtype_for_wsdl_type (pref,u,n) schema 
				end
			    end
			  with _ -> None			    
		end
	      with Not_found -> None			  
		
(**  
     @param type_name: the (resolved) name of the type we are looking for
     @param schema: the schema we use
*)
let xquery_type_from_wsdl_type type_name schema = 
  match type_name with prefix, pref_uri, name ->
    let prefix = lookup_prefix_for_uri schema pref_uri in      
      match get_itemtype_for_wsdl_type (prefix, pref_uri, name) schema with
	| Some (Kind_atomic t) -> Some (mk_regular_atomic t)
	| Some (Kind_list t) -> Some (mk_list_of_atomic t)
	| Some (Kind_complex t) -> Some (mk_elem_for_complex_type t)
	| Some (Kind_anyType t) -> Some (mk_anyType t)
	| Some (Kind_reference _) -> assert false (*  ... should never arrive here !*)
	| None -> None


let xquery_element_from_wsdl_element elem_name schema = 
  match elem_name with prefix, pref_uri, name ->
    try 
      let pref = lookup_prefix_for_uri schema pref_uri 
      in Some
	(Xquery_ast_util.mksequencetype 
	   (ITKindTest (ElementKind (ElementTest (Some ((pref, name), None)))), None))
    with Not_found -> None


let rec lookup_prefix_in_schemas pref schemas = match schemas with
    [] -> raise Not_found
  | x::rest -> 
      try
	lookup_uri_for_prefix x pref 
      with Not_found -> lookup_prefix_in_schemas pref rest
	

let get_prefix_from_env uri wmod =	
  let p,_ =
    List.find (fun (_,x) -> x = uri) wmod.global_xmlns
  in p


let get_or_create_xsd_pref wmod = 
  let get_xsd_prefix = get_prefix_from_env Namespace_builtin.xsd_uri
  in
    try 
      get_xsd_prefix wmod
    with Not_found ->
      let str = create_new_var (List.map (fun (p,_) -> string_of_prefix p) wmod.global_xmlns) "xsd" 
      in 
      let nspref = NSPrefix str 
      in
	wmod.global_xmlns <- (nspref,Namespace_builtin.xsd_uri)::wmod.global_xmlns;
	nspref


(* It returns a WSDL type and it may have as side-effect the addition
   of a namespace declaration, if needed for the that type *)
let get_wsdl_type_for_xquery_type opt_seqtype wmod =
  let seqtype =
    match opt_seqtype with
    | None -> anytype
    | Some t -> t
  in
  let search_for_nms pref =
    try 
      let _,u =
	List.find (fun (p,_) -> p = pref) wmod.global_xmlns
      in u 
    with Not_found -> raise (Query (Namespace_Error
				      ("Could not find namespace for prefix " ^ 
				       (string_of_prefix pref) ^ "\n" ^
				       (Finfo.finfo_to_string seqtype.psequencetype_loc))))
  in
  let raise_err() = 
    raise (Query (Type_Error("Could not map given type into a WSDL type")))
  in
  match fst seqtype.psequencetype_desc with
  | ITKindTest (ElementKind (ElementTest et)) -> 
      begin
	match et with
	| None -> raise_err()
	| Some (e, _) -> PartElement (search_for_nms (fst e), e)
      end
  | ITKindTest (ElementKind (SchemaElementTest e)) ->
      PartElement (search_for_nms (fst e), e)
  | ITTypeRef ((pref,_) as tn) -> PartType (search_for_nms pref, tn)
  | ITKindTest TextKind  | ITKindTest CommentKind -> 
      begin
	let pref = get_or_create_xsd_pref wmod in
	PartType (Namespace_builtin.xsd_uri, (pref,"string"))
      end
  | ITAtomic  ((pref,_) as tn)                -> PartType (search_for_nms pref, tn)
  | _ -> raise_err()

