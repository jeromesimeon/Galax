(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: schema_norm.ml,v 1.23 2007/02/01 22:08:53 simeon Exp $ *)

(* Module: Schema_norm
   Description:
     This module normalizes a schema in the XQuery type system into a
     schema in the 'core' XQuery type system.
*)

(*## see raise_nyi calls for not-yet-implemented features *)

(*TODO: 

  - Topological ordering of element declarations by substitution group
  hierarchies, so that inheritance of an element type from the head
  element of the group can be supported, as in
  <xs:element name="umbrella" substitutionGroup="product"/>
  Alternatively, organize backpatching in norm_decls.
*)
open Error
open Xquery_common_ast
open Xquery_type_ast
open Xquery_type_core_ast
open Xquery_type_core_ast_util
open Xquery_ast_util
open Occurrence

open Namespace_symbols_util

type cgname = Namespace_symbols.symbol

type fi = Finfo.finfo 

type deriv_kind = Restriction | Extension

(*******************   Helper functions ********************************)

let raise_error msg = 
  raise (Query(Schema_Import(msg)))

let raise_nyi msg = 
  raise (Query(Prototype ("Not implemented: "^msg)))

(* Resolvers for uqnames *)

let resolve_attr ns aname = 
  let qn = Namespace_resolve.resolve_attribute_qname ns aname in
  Namespace_symbols.rattr_symbol qn

let resolve_elem ns ename = 
  let qn = Namespace_resolve.resolve_element_qname ns ename in
  Namespace_symbols.relem_symbol qn

let resolve_type ns tname = 
  let qn = Namespace_resolve.resolve_type_qname ns tname in
  Namespace_symbols.rtype_symbol qn

let resolve_egroup ns gname = 
  let qn = Namespace_resolve.resolve_group_qname ns gname in
  Namespace_symbols.relem_symbol qn

let resolve_agroup ns gname = 
  let qn = Namespace_resolve.resolve_attrGroup_qname ns gname in
  Namespace_symbols.rattr_symbol qn


(************** Tables for the non-core schema defs ************************)

type xtable = {
  xt_attrs   : (fi * stype_specifier) SQNameHashtbl.t;
  xt_elems   : (fi * (substitutes_for*nillable*xtype_specifier)) SQNameHashtbl.t;
  xt_types   : (fi *  xtype_derivation) SQNameHashtbl.t; 
}

type gtable= {
  gt_egroups : (fi * xtype) SQNameHashtbl.t;
  gt_agroups : (fi * xtype) SQNameHashtbl.t; 
}

let xtable_make_empty () = {
  xt_attrs = SQNameHashtbl.create 31;
  xt_elems = SQNameHashtbl.create 31;
  xt_types = SQNameHashtbl.create 31; }

let gtable_make_empty () = {
  gt_egroups = SQNameHashtbl.create 31;
  gt_agroups = SQNameHashtbl.create 31;
}

let xgtables_add_from_decls xtbl gtbl nsenv decls =
  let adder decl = 
    let fi = decl.pxtype_declaration_loc in
    match decl.pxtype_declaration_desc with 
    | TAttributeDecl(n,d) -> 
	SQNameHashtbl.add xtbl.xt_attrs (resolve_attr nsenv n) (fi,d)
    | TElementDecl(n,d) -> 
	SQNameHashtbl.add xtbl.xt_elems (resolve_elem nsenv n) (fi, d)
    | TTypeDecl(n, d) -> 
	SQNameHashtbl.add xtbl.xt_types (resolve_type nsenv n) (fi, d)
    | TGroupDecl(n, d) -> 
	SQNameHashtbl.add gtbl.gt_egroups (resolve_egroup nsenv n) (fi, d)
    | TAttrGroupDecl(n, d) -> 
	SQNameHashtbl.add gtbl.gt_agroups (resolve_agroup nsenv n) (fi, d)
  in
  List.iter adder decls


(************* A table for core schema defs ***********)

type ctable = {
  ct_attrs : (fi * Namespace_symbols.rtype_symbol) SQNameHashtbl.t;
  ct_elems : (fi * (csubstitutes_for * nillable * Namespace_symbols.rtype_symbol)) SQNameHashtbl.t;
  ct_types : (fi * ctype_declaration) SQNameHashtbl.t;
}

let ctable_make_empty () = {
  ct_attrs = SQNameHashtbl.create 31;
  ct_elems = SQNameHashtbl.create 31;
  ct_types = SQNameHashtbl.create 31; 
}

let ctable_add_from_cdecls ctbl celem_decls cattr_decls ctype_decls =
  let adder_elem (n, d) = SQNameHashtbl.add ctbl.ct_elems n (Finfo.bogus,d) in
  let adder_attr (n, d) = SQNameHashtbl.add ctbl.ct_attrs n (Finfo.bogus,d) in
  let adder_type decl = SQNameHashtbl.add ctbl.ct_types decl.ctypedecl_name (Finfo.bogus,decl) in
  begin
    List.iter adder_elem celem_decls;
    List.iter adder_attr cattr_decls;
    List.iter adder_type ctype_decls
  end


(************** A table for names defined in a schema *)
type ntable =
    { nt_attrs   : (unit) SQNameHashtbl.t;
      nt_elems   : (unit) SQNameHashtbl.t;
      nt_types   : (unit) SQNameHashtbl.t;
      nt_egroups : (unit) SQNameHashtbl.t;
      nt_agroups : (unit) SQNameHashtbl.t }

let ntable_make_empty () = {
  nt_attrs = SQNameHashtbl.create 31;
  nt_elems = SQNameHashtbl.create 31;
  nt_types = SQNameHashtbl.create 31; 
  nt_egroups = SQNameHashtbl.create 31;
  nt_agroups = SQNameHashtbl.create 31;
} 

let ntable_destructive_merge ntbl1 ntbl2 = 
  SQNameHashtbl.iter (fun a b -> SQNameHashtbl.add ntbl1.nt_attrs a b) ntbl2.nt_attrs;
  SQNameHashtbl.iter (fun a b -> SQNameHashtbl.add ntbl1.nt_elems a b) ntbl2.nt_elems;
  SQNameHashtbl.iter (fun a b -> SQNameHashtbl.add ntbl1.nt_types a b) ntbl2.nt_types;
  SQNameHashtbl.iter (fun a b -> SQNameHashtbl.add ntbl1.nt_egroups a b) ntbl2.nt_egroups;
  SQNameHashtbl.iter (fun a b -> SQNameHashtbl.add ntbl1.nt_agroups a b) ntbl2.nt_agroups;
  ntbl1

let ntable_add_from_cdecls ntbl celem_decls cattr_decls ctype_decls = 
  let adder_elem (n, _) = SQNameHashtbl.add ntbl.nt_elems n () in
  let adder_attr (n, _) = SQNameHashtbl.add ntbl.nt_attrs n () in
  let adder_type decl = SQNameHashtbl.add ntbl.nt_types decl.ctypedecl_name () in
  begin
    List.iter adder_elem celem_decls;
    List.iter adder_attr cattr_decls;
    List.iter adder_type ctype_decls
  end

let ntable_add_from_xtable ntbl xtbl = 
  SQNameHashtbl.iter (fun n _ -> SQNameHashtbl.add ntbl.nt_attrs n ()) xtbl.xt_attrs;
  SQNameHashtbl.iter (fun n _ -> SQNameHashtbl.add ntbl.nt_elems n ()) xtbl.xt_elems;
  SQNameHashtbl.iter (fun n _ -> SQNameHashtbl.add ntbl.nt_types n ()) xtbl.xt_types

let ntable_add_from_gtable ntbl gtbl = 
  SQNameHashtbl.iter (fun n _ -> SQNameHashtbl.add ntbl.nt_egroups n ()) gtbl.gt_egroups;
  SQNameHashtbl.iter (fun n _ -> SQNameHashtbl.add ntbl.nt_agroups n ()) gtbl.gt_agroups



(********** Name resolvers that check existence of a referent *******) 

let string_of_urqname uqname rqname = 
  let rqname = Namespace_symbols.relem_name rqname in
  (Namespace_names.string_of_uqname uqname) ^ " = " ^
  (Namespace_names.quoted_uri_string_of_rqname rqname)

   (*--VG## It is hard at this point to provide a line number in the
     error messages in these functions...  
     Ideally, ename should carry a Finfo. *)

let resolve_referred_attr (nsenv,ntbl) fi aname = 
  let caname = resolve_attr nsenv aname in
    if SQNameHashtbl.mem ntbl.nt_attrs caname then caname
    else raise_error 
      ("No definition found for the attribute referenced by " ^
       (string_of_urqname aname caname))

let resolve_referred_elem (nsenv,ntbl) fi ename = 
  let cename = resolve_elem nsenv ename in
    if SQNameHashtbl.mem ntbl.nt_elems cename then cename
    else raise_error 
      ("No definition found for the element referenced by " ^
       (string_of_urqname ename cename))

let resolve_referred_type (nsenv,ntbl) fi tname = 
  let ctname = resolve_type nsenv tname in
    if SQNameHashtbl.mem ntbl.nt_types ctname then ctname
    else raise_error 
      ("No definition found for the type referenced by " ^
       (string_of_urqname tname ctname))

let resolve_referred_egroup (nsenv,ntbl) fi egname = 
  let cegname = resolve_egroup nsenv egname in
    if SQNameHashtbl.mem ntbl.nt_egroups cegname then cegname
    else raise_error 
      ("No definition found for the element group referenced by " ^
       (string_of_urqname egname cegname))

let resolve_referred_agroup (nsenv,ntbl) fi agname = 
  let cagname = resolve_agroup nsenv agname in
    if SQNameHashtbl.mem ntbl.nt_agroups cagname then cagname
    else raise_error 
      ("No definition found for the element group referenced by " ^
       (string_of_urqname agname cagname))



(*--??++ 
(********** Related to imported sub-schemas  *************)

(* Currently, this creates a new table.  But it won't hurt if one of the
   argument tables (e.g., the largest) was modified and returned. *)
let combine_ctype_tables (tables: ctype_table list) : ctype_table = 
  let newtable = make_ctype_table () in 
  let add_one_table tbl = 
    begin
      SQNameHashtbl.iter (SQNameHashtbl.add newtable.ctt_attrs) tbl.ctt_attrs;
      SQNameHashtbl.iter (SQNameHashtbl.add newtable.ctt_elems) tbl.ctt_elems;
      SQNameHashtbl.iter (SQNameHashtbl.add newtable.ctt_types) tbl.ctt_types;
    end
  in 
    List.iter add_one_table tables; 
    newtable

let combine_cgroup_tables (tables: cgroup_table list) : cgroup_table = 
  let newtable = make_cgroup_table () in 
  let add_one_table tbl = 
    begin
      SQNameHashtbl.iter (SQNameHashtbl.add newtable.cgt_egroups) tbl.cgt_egroups;
      SQNameHashtbl.iter (SQNameHashtbl.add newtable.cgt_agroups) tbl.cgt_agroups;
    end
  in 
    List.iter add_one_table tables; 
    newtable

let ctx_of_ctt (tbl: ctype_table) : ctype_ctx = 
  make_ctype_ctx ()   (*## temporary *)
*)


(************** Checking groups for no cycles ******************)

(** A "graph of dependencies" contains, for each group name, the list
  of the group names used in its definition. *)
type deps = cgname list              
type graph = (cgname * deps) list

let extract_egrefs nsenv xtype : deps = 
  let refs = ref [] in
  let rec extract xt : unit = 
    match xt.pxtype_desc with 
      (* Interesting case: *)
      | TGroupRef gname -> refs := (resolve_egroup nsenv gname) :: !refs
      (* "Go deeper" cases: *)
      | TElementLocal(_,_,TSpecSimple xs) ->  ()
      | TElementLocal(_,_,TSpecComplex xc) ->  
	  (match xc.pctype_specifier_desc with
	     | TTypeRef _ -> ()
	     | TAnonymous(_,_,_,x) -> extract x)
      | TDocument(x) -> extract x 
      | TBound(x,_,_) -> extract x
      | TSequence(x1,x2) | TChoice(x1,x2) | TInterleave(x1,x2) ->
	  extract x1; extract x2
      | _ -> ()
  in extract xtype; !refs


let extract_agrefs nsenv xtype : deps = 
  let refs = ref [] in
  let rec extract xt : unit = 
    match xt.pxtype_desc with
      (* Interesting case: *)
      | TAttrGroupRef gname -> refs := (resolve_egroup nsenv gname) :: !refs
      (* "Go deeper" cases: *)
      | TElementLocal(_,_,TSpecSimple xs) ->  ()
      | TElementLocal(_,_,TSpecComplex xc) ->  
	  (match xc.pctype_specifier_desc with
	     | TTypeRef _ -> ()
	     | TAnonymous(_,None,_,_) -> ()
	     | TAnonymous(_,Some x,_,_) -> extract x)
      | TDocument(x) -> extract x 
      | TBound(x,_,_) -> extract x
      | TSequence(x1,x2) | TChoice(x1,x2) | TInterleave(x1,x2) ->
	  extract x1; extract x2
      | _ -> ()
  in extract xtype; !refs


let get_groups_graph nsenv ghash refs_extractor : graph = 
  let g = ref [] in 
    SQNameHashtbl.iter (fun n (_,d) -> g := (n, (refs_extractor nsenv d)) :: !g) ghash;
    !g
      

(** Takes a graph where every edge participates in a cycle, 
and returns a string representing a cycle. *)
let show_cycle graph : string = 
  (* For simplicity, start with a node that appears first in a 
     dependency list; e.g. in the first one. *)
  let start = match graph with
    | (_, d::_) :: _ -> d
    | _ -> assert false (* guaranteed not to happen *)
  in
  let rec chase_cycle str curr = 
    let next = match List.assoc curr graph with
      | d::_ -> d
      | _ -> assert false  (* guaranteed not to happen *) in
    let str' = str ^ " <- \n" ^ (Namespace_symbols.symbol_prefix_string next) in
      if next = start then str'
      else chase_cycle str' next
  in
    chase_cycle (Namespace_symbols.symbol_prefix_string start) start
    

(* The graph obtained from get_groups_graph is searched for possible
   cycles using the "zero fan-in elimination" algorithm of topological
   sorting.  *)
let check_graph_is_dag group_kind (graph : graph)  = 
  let gr = ref graph in
    while !gr != [] do 
      (* Find all names without dependencies *)
      match List.partition (fun (n,ds) -> ds = []) !gr with
	| _,  [] ->  (* everything checked, no cycles *)
	    gr := []
	| [], gr' -> (* there must be cycles in the remaining graph *)
	    raise_error ("There are cyclic dependencies in " ^ 
			      group_kind ^ ", for example: \n" ^ 
			      (show_cycle gr'))  
	| indeps, gr' -> (* remove indeps dependencies from gr' *)
	    let names = List.map (fun (n,_) -> n) indeps in
	    let gr'' = 
	      List.map (fun (n, ds) -> 
			  let ds' = 
			    List.filter (fun d -> not(List.mem d names)) ds
			  in (n, ds')) 
		gr'
	    in gr := gr''
    done
  

let check_group_cycles nsenv gtbl = 
  (let egroups_graph = get_groups_graph nsenv gtbl.gt_egroups extract_egrefs in
    check_graph_is_dag "element groups" egroups_graph); 
  (let agroups_graph = get_groups_graph nsenv gtbl.gt_agroups extract_agrefs in
     check_graph_is_dag "attribute groups" agroups_graph)




(************** Normalization of schema components *******************)

(* Most norm_XXX functions, in addition to returning a result, also
   update their ctbl argument. *)

let norm_substitutes_for (nsenv,ntbl) fi = function 
  | TSubstitutesFor ename -> 
      CSubstitutesFor (resolve_referred_elem (nsenv,ntbl) fi ename)
  | TNonSubstitutesFor -> CNonSubstitutesFor

let rec norm_stype_specifier ctbl (nsenv,namer,ntbl,gtbl) stypespec = 
  let fi = stypespec.pstype_specifier_loc in
  match stypespec.pstype_specifier_desc with 
    | STypeRef tname -> resolve_referred_type (nsenv,ntbl) fi tname
    | SAnonymous stypeder -> 
	let cxtypeder = 
	  norm_stype_derivation ctbl (nsenv,namer,ntbl,gtbl) stypeder in
	let ctname = Schema_namer.fresh_name namer in
	let decl = fmkctype_decl ctname cxtypeder None in 
	  SQNameHashtbl.add ctbl.ct_types ctname (fi, decl);
	  ctname

and norm_stype_derivation ctbl (nsenv,namer,ntbl,gtbl) stypeder 
  : cxtype_derivation = 
  match stypeder with 
    | SRestriction stypespec -> 
	let ctname = norm_stype_specifier ctbl (nsenv,namer,ntbl,gtbl) stypespec in
	  (ctname, None, CAtomicTypeRestriction)
    | SList stypespec -> 
	let ctname = norm_stype_specifier ctbl (nsenv,namer,ntbl,gtbl) stypespec in
	  (Namespace_symbols_builtin.xs_anyType, None, CSimpleTypeList ctname)
    | SUnion stypespecs -> 
	let ctnames = 
	  List.map (norm_stype_specifier ctbl (nsenv,namer,ntbl,gtbl)) stypespecs in
	  (Namespace_symbols_builtin.xs_anyType, None, CSimpleTypeUnion ctnames)

(*--VG-- reverted back to CBounds...
(*--VG  adapted from Glushkov.normalize_bound; see explanation there  *)
let rec apply_bounds fi cxtype min max : cxtype = 
  fmkcxtype (apply_bounds_desc fi cxtype min max) fi

and apply_bounds_desc fi cxtype min max : cxtype_desc = 
  let cxempty = fmkcxtype CEmpty fi in
  match min, max with
  | UP_INT 0, UP_INT 0 -> CEmpty                      (*--VG was missing *)
  | UP_INT 1, UP_INT 1 -> cxtype.pcxtype_desc         (*--VG was missing *)
  | UP_INT 0, UNBOUNDED -> CStar cxtype                       (* (I) *)
  | UP_INT 1, UNBOUNDED -> CPlus cxtype                    (* (II) *)
  | UP_INT min', UNBOUNDED ->                                 (* (III) *)
      CSequence (cxtype, apply_bounds fi cxtype (UP_INT (min'-1)) UNBOUNDED)
  | UP_INT 0, UP_INT 1 ->                                 (* (IV) *)
      CChoice (cxempty, cxtype)
      (*--VG?? why not   COption(cxtype)   ? *)
  | UP_INT 0, UP_INT max' when max' > 0 ->      (* (IV') *)
      CChoice (cxempty, 
	       fmkcxtype (CSequence(cxtype, 
				    apply_bounds fi cxtype (UP_INT 0) (UP_INT(max'-1)))) fi)
  | UP_INT min', UP_INT max' when max' > 0 ->            (* (V) *)
      CSequence (cxtype, 
		 apply_bounds fi cxtype (UP_INT (min'-1)) (UP_INT (max'-1)))
  | _ -> 
      raise_error "Error during normaliznig bound type: Invalid bounds"
*)

let rec norm_xtype ctbl (nsenv,namer,ntbl,gtbl) xtype : cxtype = 
  let fi = xtype.pxtype_loc in
  let rewrap cxtype_desc = fmkcxtype cxtype_desc fi
  in
    match xtype.pxtype_desc with 
      | TAtomicRef tname -> 
	  rewrap (CAtomicRef(resolve_referred_type (nsenv,ntbl) fi tname))
      | TElementRef ename ->   
	  rewrap (CElementRef(resolve_referred_elem (nsenv,ntbl) fi ename))
      | TElementLocal(ename, nillable, xtypespec) -> 
	  let cename = resolve_elem nsenv ename in
	  let cnillable = nillable in
	  let ctname = 
	    norm_xtype_specifier ctbl (nsenv,namer,ntbl,gtbl) xtypespec in
	    rewrap(CElementLocal(cename, cnillable, ctname))
      | TAttributeRef aname ->
	  rewrap (CAttributeRef (resolve_referred_attr  (nsenv,ntbl) fi aname))
      | TAttributeLocal(aname, stypespec) ->
	  let caname = resolve_attr nsenv aname in
	  let ctname = norm_stype_specifier ctbl (nsenv,namer,ntbl,gtbl) stypespec in
	    rewrap(CAttributeLocal(caname, ctname))
      | TDocument xtype -> 
	  rewrap(CDocument (norm_xtype ctbl (nsenv,namer,ntbl,gtbl) xtype))
      | TText -> 
	  rewrap(CText)
      | TProcessingInstruction -> 
	  rewrap(CPI None)
      | TComment -> 
	  rewrap(CComment)

      | TGroupRef gname  ->
	  let cgname = resolve_referred_egroup (nsenv,ntbl) fi gname in
	  let (_, g_xtype) = 
	    try SQNameHashtbl.find gtbl.gt_egroups cgname 
	    with Not_found -> raise_error 
	      ("Undefined element group reference: <group ref=" ^ 
	       (Namespace_names.string_of_uqname gname) ^ ">")
	  in
	    norm_xtype ctbl (nsenv,namer,ntbl,gtbl) g_xtype
 (*--VG## Something suboptimal manifests above (and below):
   resolve_referred_group and SQNameHashtbl.find should succeed or fail under the
   same conditions.  The treatment of {type,attrr,elem} refs and
   {egrop,agroup} refs should differ: the first are not need to be
   resolved into the definitions, while the second are! *)

      | TAttrGroupRef gname ->
	  let cgname = resolve_referred_agroup (nsenv,ntbl) fi gname in
	  let (_, g_xtype) = 
	    try SQNameHashtbl.find gtbl.gt_agroups cgname 
	    with Not_found -> raise_error 
	      ("Undefined attribute group reference: <attributeGroup ref=" ^ 
	       (Namespace_names.string_of_uqname gname) ^ ">")
	  in
	    norm_xtype ctbl (nsenv,namer,ntbl,gtbl) g_xtype


      | TBound(xtype, min, max) -> 
	  let cxtype = norm_xtype ctbl (nsenv,namer,ntbl,gtbl) xtype in
	    rewrap(CBound(cxtype, min, max))
	      (*--	apply_bounds_desc fi cxtype min max *)

      | TSequence (xtype1, xtype2) -> 
	  rewrap(CSequence(norm_xtype  ctbl (nsenv,namer,ntbl,gtbl) xtype1, 
			   norm_xtype  ctbl (nsenv,namer,ntbl,gtbl) xtype2))
      | TEmpty -> 
	  rewrap(CEmpty)
      | TChoice(xtype1, xtype2) -> 
	  rewrap(CChoice(norm_xtype  ctbl (nsenv,namer,ntbl,gtbl) xtype1, 
			 norm_xtype  ctbl (nsenv,namer,ntbl,gtbl) xtype2))
      | TNone -> 
	  rewrap(CNone)
      | TInterleave(xtype1, xtype2) -> 
	  rewrap(CInterleave(norm_xtype  ctbl (nsenv,namer,ntbl,gtbl) xtype1, 
			     norm_xtype  ctbl (nsenv,namer,ntbl,gtbl) xtype2))


and norm_xtype_opt ctbl (nsenv,namer,ntbl,gtbl) xtypeopt : cxtype option = 
  match xtypeopt with 
    | None -> None
    | Some xtype -> Some (norm_xtype ctbl (nsenv,namer,ntbl,gtbl) xtype)

and norm_xtype_specifier ctbl (nsenv,namer,ntbl,gtbl) xtypespec = 
  match xtypespec with
    | TSpecSimple stypespec  -> 
	norm_stype_specifier ctbl (nsenv,namer,ntbl,gtbl) stypespec
    | TSpecComplex ctypespec -> 
	norm_ctype_specifier ctbl (nsenv,namer,ntbl,gtbl) ctypespec

and norm_ctype_specifier ctbl (nsenv,namer,ntbl,gtbl) ctypespec = 
  let fi = ctypespec.pctype_specifier_loc in
    match ctypespec.pctype_specifier_desc with 
      | TTypeRef tname -> resolve_referred_type (nsenv,ntbl) fi tname
      | TAnonymous ctypeder -> 
	  let cxtypeder = 
	    norm_ctype_derivation ctbl (nsenv,namer,ntbl,gtbl) ctypeder in
	  let ctname = Schema_namer.fresh_name namer in
	  let decl = fmkctype_decl ctname cxtypeder None in 
	    SQNameHashtbl.add ctbl.ct_types ctname (fi, decl);
	    ctname

and norm_ctype_derivation ctbl (nsenv,namer,ntbl,gtbl) ctypeder 
  : cxtype_derivation = 
  let ctypeder' = 
    match ctypeder with 
      | (Some(TExtension base_tname), a, m, c) -> 
	  let base_ctname = 
	    resolve_referred_type (nsenv,ntbl) Finfo.bogus base_tname in
	    (*## ^^^^ *)
	    (Extension, base_ctname, a, m, c)
      | (Some(TRestriction base_tname), a, m, c) -> 
	  let base_ctname = 
	    resolve_referred_type (nsenv,ntbl) Finfo.bogus base_tname in
	    (*## ^^^ *)
	    (Restriction, base_ctname, a, m, c)
      | (None, a, m, c) -> 
	  (Restriction, Namespace_symbols_builtin.xs_anyType, a, m, c) in
    match ctypeder' with 
      | (Restriction, base_ctname, attr_xtype_opt, mixed, chld_xtype) ->
	  let cmixed = mixed in
	  let attr_cxtype_opt = 
	    norm_xtype_opt ctbl (nsenv,namer,ntbl,gtbl) attr_xtype_opt in
	  let chld_cxtype = 
	    norm_xtype ctbl (nsenv,namer,ntbl,gtbl) chld_xtype in
	    (base_ctname, attr_cxtype_opt, 
	     CComplexTypeRestriction(cmixed, chld_cxtype))
      | (Extension, base_ctname, attr_xtype_opt, mixed, chld_xtype) ->
	  let cmixed = mixed in
	  let attr_cxtype_opt = 
	    norm_xtype_opt ctbl (nsenv,namer,ntbl,gtbl) attr_xtype_opt in
	  let chld_cxtype = 
	    norm_xtype ctbl (nsenv,namer,ntbl,gtbl) chld_xtype in
	    (base_ctname, attr_cxtype_opt, 
	     CComplexTypeExtension(cmixed, chld_cxtype))


let norm_element_derivation ctbl (nsenv,namer,ntbl,gtbl) elemder = 
  let (substfor, nill, xtypespec) = elemder in
  let csubstfor = norm_substitutes_for (nsenv,ntbl) Finfo.bogus substfor in
                                                   (*?? ^^^^ ??*)
  let cnill = nill in
  let tname = norm_xtype_specifier ctbl (nsenv,namer,ntbl,gtbl) xtypespec in
    (csubstfor, cnill, tname)


let norm_xtype_derivation ctbl (nsenv,namer,ntbl,gtbl) xtypeder 
  : cxtype_derivation = 
  match xtypeder with 
    | TComplexDerivation ctypeder -> 
	norm_ctype_derivation ctbl (nsenv,namer,ntbl,gtbl) ctypeder
    | TSimpleDerivation stypeder -> 
	norm_stype_derivation ctbl (nsenv,namer,ntbl,gtbl) stypeder


(************** Normalization of schema as a whole *******************)

let norm_decls ctbl (nsenv,namer,ntbl,gtbl) xtbl : unit = 
  SQNameHashtbl.iter (fun caname (fi, stypespec) -> 
    let ctname = norm_stype_specifier ctbl (nsenv,namer,ntbl,gtbl) stypespec 
    in SQNameHashtbl.add ctbl.ct_attrs caname (fi, ctname) )
    xtbl.xt_attrs;
  SQNameHashtbl.iter (fun cename (fi, elemder) -> 
    let celemder = 
      norm_element_derivation ctbl (nsenv,namer,ntbl,gtbl) elemder
    in SQNameHashtbl.add ctbl.ct_elems cename (fi, celemder) )
    xtbl.xt_elems;
  SQNameHashtbl.iter (fun ctname (fi, xtypeder) -> 
    let cxtypeder = 
      norm_xtype_derivation ctbl (nsenv,namer,ntbl,gtbl) xtypeder in
    let decl = fmkctype_decl ctname cxtypeder None in 
    SQNameHashtbl.add ctbl.ct_types ctname (fi, decl))
    xtbl.xt_types


(*--??++ postpone any treatment of imports...
let rec 
  (* Get class and group tables for the imported schemas: *)
  let (imported_ctts, imported_cgts) = 
    List.split (List.map (norm_xschema namer) xs.xschema_imported_schemas) in
  let ctt = combine_ctype_tables imported_ctts in  (* imported ctt *)
  let cgt = combine_cgroup_tables imported_cgts in (* imported cgt *)
  (* Get all the type names, imported and defined in this schema:  *)  
  let ctx =    (* imported ctx *)
    ctx_of_ctt ctt in 
  let _ =    (* full ctx *)
    ctx_addfrom_decls ctx nsenv xs.xschema_type_declarations in 
*)


let flatten_decls ctable = 
  let elem_decls =
    SQNameHashtbl.fold (fun n (fi,d) tbl -> (n, d) :: tbl) ctable.ct_elems []
  in
  let attr_decls =
    SQNameHashtbl.fold (fun n (fi,d) tbl -> (n,d) :: tbl) ctable.ct_attrs []
  in
  let type_decls =
    SQNameHashtbl.fold (fun n (fi,d) tbl -> d :: tbl) ctable.ct_types []
  in
  (elem_decls,attr_decls,type_decls)

(* From Vladimir: 

   Keeping track of the names during normalizing a schema S:

xtbl : (cname -> xdef)  corresponds to the contents of S
    (maps resolved names into non-core definitions)

ntbl : (cname -> unit) contains all the names that can be legally
   referenced in S definitions; i.e., the names defined by S, schemas
   imported by S, and the names from Schema_builtin

ctbl : (cname -> cdef) corresponds to normalized contents of S.
     (maps names defined in S to their defs in core syntax)
     This table is essentially the result of the normalization.

Schema normalization order (no schema imports so far):

1. Map input schema into xtbl.

2. Check group definitions for the absence of cycles and build a
   fast-access table of them.  If practical, expand group references.

4. Build ntbl from xtbl and Schema_builtin.

X. (? Restate extension-derivations as restriction-derivations)

X. (? Handle specifics of attribute inheritance in type derivations)

Traverse xtbl, normalizing each def into an entry in ctbl.

  Normalization of an xtbl entry can:

  - nsenv -- refer to, to resolve uqnames;

  - ntbl -- refer to, to check existence of names for types, elements,
    and attributes;

  - gtbl -- refer to, to expand group references;

  - namer -- refer to and mutate, to create names for anonymous types

  - ctbl -- mutate,  to enter a new normalized entry.

  In this design, ctbl is mutated during the process.  A (less clean,
  I think) alternative would be to return, _several_ normalized defs
  as the result of a xtbl entry normalization, some of them
  corresponding to the anonymous types -- then it could be possible to
  frame the whole process as an effect-free mapping.
*)

let norm_xschema init_nsenv namer ctbl inputxs = 

   let rec norm_xschema_aux xs = 
     (* 1. Recursively normalize imported schemas and merge name tables *)
   let ntbl_list = List.map (fun x -> fst (norm_xschema_aux x)) xs.xschema_imported_schemas in
   let ntbl = List.fold_left ntable_destructive_merge (ntable_make_empty ()) ntbl_list in 

     (* 2. Add new namespace bindings *)
   let ns =     
   List.map (function (ncname,uri) -> (Namespace_names.NSPrefix ncname, uri)) 
   xs.xschema_namespace_declarations in

   let nsenv = Namespace_context.add_all_ns init_nsenv ns in 

   let xtbl = xtable_make_empty () in
   let gtbl = gtable_make_empty () in

   (* 3. Map input schema into xtbl *)
   let _ = xgtables_add_from_decls xtbl gtbl nsenv xs.xschema_type_declarations in

   (* 4. Check group definitions for cycles and build a fast-access table to them. *) 
   let _ = check_group_cycles nsenv gtbl in

   (* 5. Build ntbl from xtbl *)
   let _ = 
       (* The following is redundant ... Should probably only be done once. *)
   (ntable_add_from_cdecls ntbl
      Schema_builtin.built_in_xml_schema_elem_decls
      Schema_builtin.built_in_xml_schema_attr_decls
      Schema_builtin.built_in_xml_schema_type_decls;
    ntable_add_from_xtable ntbl xtbl;
    ntable_add_from_gtable ntbl gtbl)
   in
   
     (* Normalize types of this schema into ctbl' -- additively/destructively modifies ctbl' *)
   let _ = norm_decls ctbl (nsenv,namer,ntbl,gtbl) xtbl in 
   (ntbl,gtbl)

in
   let (ntbl,gtbl) = norm_xschema_aux inputxs in
   (init_nsenv,namer,ntbl,gtbl)

let normalize init_nsenv (xschema: xschema) : cxschema = 
  let namer = Schema_namer.create () in
  let ctbl = ctable_make_empty () in
  let _ =
    ctable_add_from_cdecls ctbl
      Schema_builtin.built_in_xml_schema_elem_decls
      Schema_builtin.built_in_xml_schema_attr_decls
      Schema_builtin.built_in_xml_schema_type_decls
  in
  let _ = norm_xschema init_nsenv namer ctbl xschema in 
  let (elem_decls,attr_decls,type_decls) = flatten_decls ctbl in
  Xquery_type_core_ast_util.fmkcxschema elem_decls attr_decls type_decls
       (* print_string ("After norm_xschema\n");
       Print_type_core.print_cxschema Format.std_formatter (Schema_util.fmkcxschema(flatten_decls ctbl')); 
       *) 

let normalize_type init_nsenv (xschema: xschema) xtype : cxtype = 
  let namer = Schema_namer.create () in
  let ctbl = ctable_make_empty () in
  let _ =
    ctable_add_from_cdecls ctbl
      Schema_builtin.built_in_xml_schema_elem_decls
      Schema_builtin.built_in_xml_schema_attr_decls
      Schema_builtin.built_in_xml_schema_type_decls
  in
  let (nsenv,namer,ntbl,gtbl) = norm_xschema init_nsenv namer ctbl xschema in 
  norm_xtype ctbl (nsenv,namer,ntbl,gtbl) xtype
  
       (* print_string ("After norm_xschema\n");
       Print_type_core.print_cxschema Format.std_formatter (Schema_util.fmkcxschema(flatten_decls ctbl')); 
       *) 

