(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* Module: Compile_annotate
   Description:
     This module implements the walker to calculate annotations on the
     algebra AST. 
*)
open Error

open Xquery_algebra_ast
open Xquery_algebra_ast_util
open Xquery_algebra_ast_annotation_util

(******************)
(* Free Variables *)
(******************)

let empty_fields = []

(*************************************************************)
(*** TO EASE PROCESSING THE LISTS ARE KEPT IN SORTED ORDER ***)
(*************************************************************)

let cv_compare i j = 
  String.compare 
    (Namespace_names.curly_uri_string_of_rqname i) 
    (Namespace_names.curly_uri_string_of_rqname j)

let use_count_compare (i,_) (j,_) = cv_compare i j

(* Used to merge variable counts (which are sorted individually *)
let variable_count_merge l =
  (* Merge all the lists so they are now in order by cv name *)
  let merged = List.fold_left (List.merge use_count_compare) [] l in
    (* Now, scan the list and merge it *)
  let rec merge_helper (completed_items, left_over) new_item =
    let new_v, (new_count,old_usage) = new_item in
      match left_over with
	| None ->
	    (completed_items, (Some new_item))
	| Some (v,(count, usage)) ->
	    begin
	      if v = new_v then
		begin
		  let count' = count + new_count in 
		    (* Check to see if it is once used or never used *)
		    if (((old_usage = Never) && (usage = Once)) ||
			  ((usage = Never) && (old_usage = Once)))
		      && (count' = 1) 
		    then (completed_items, (Some (v, (count', Once))))
		    else (completed_items, (Some (v, (count', Many))))
		end
	      else
		(completed_items @ [v,(count,usage)]), (Some new_item)
	    end
  in
  let cl, lo = List.fold_left merge_helper ([], None) merged in
  let cl =
    match lo with
	None   -> cl
      | Some it -> cl @ [ it ]
  in
    cl

(* Must list all bound variables here *)
(* Return must be sorted by cv_compare *)
let extract_bound_free_variables aname =   
  match aname with
  | AOEMapFromItem(vn)
  | AOELetvar(_,vn)
  | AOESome (_,vn)
  | AOEEvery (_, vn) ->
      ((vn,(0,Never)) :: [], [])

  | AOESeq      | AOEEmpty             | AOEDocument
  | AOEPI _     | AOEPIComputed        | AOEComment _         | AOECommentComputed
  | AOEText _   | AOECharRef _         | AOETextComputed      | AOEElem _
  | AOEAnyElem _| AOEAttr _            | AOEAnyAttr _ 
  | AOEError    | AOETreat _           | AOEValidate _
  | AOECast _   | AOECastable _        |  AOEIf | AOEWhile
  | AOEScalar _ | AOEInputTuple
  | AOECallBuiltIn _     | AOECallOverloaded _ 
  | AOEConvertSimple _ | AOEPromoteNumeric _ | AOEPromoteAnyString | AOEUnsafePromoteNumeric _
  | AOEServerImplements _ | AOEExecute _ | AOEForServerClose _ | AOEEvalClosure
  | AOEASyncExecute _ | AOECallUserDefined _
  | AOEOuterMapConcat _ | AOEMapConcat | AOEGroupBy _ 
  | AOENullMap _ | AOEParse _ | AOETreeJoin _ 
  | AOETupleTreePattern (_,_) (* | AOEPrune _ | AOEDistinct _ *)
  | AOEImperativeSeq  ->
      ([],[])

  | AOEVar(vn) ->
      ([], (vn,(1,Once)) :: [])

  | AOESet(vn) ->
      ([], (vn,(1,Many)) :: [])   (* Nicola: The variable is not actually "used", but "defined" *)
  
  | AOETypeswitch _ ->
      raise (Query (Internal_Error ("SHOULD NOT BE HERE - Typeswitch must be handled differently")))

  (* Function Calls *)
  (* Galax extensions *)

  (* Tuple operations *)
  | AOEAccessTuple _              (* Tuple field access *)
  | AOECreateTuple _        (* Tuple creation *)
  | AOEConcatTuples                        (* Tuple concatenation *)
  | AOEProject _
  | AOEMapToItem                        (* Tuple to item iteration *)
  | AOEMap                                 (* Tuple iteration *)
  | AOEProduct                             (* Cartesian product *)
  | AOEJoin _                                (* Join *)
  | AOELeftOuterJoin _
  | AOESelect _                              (* Selection *)
  | AOEOrderBy _
  | AOEMapIndex _
  | AOEMapIndexStep _
      -> 
      ([], [])

  (* Update operations *)

  | AOECopy
  | AOEDelete
  | AOEInsert _
  | AOERename _
  | AOEReplace _
  | AOESnap _ -> ([], [])

let is_typeswitch cur = match cur with AOETypeswitch __ -> true | _ -> false

let handle_typeswitch cur dep =
  let ovn_list = 
    match cur with
	AOETypeswitch l ->
	  List.map (fun (_,ovn) -> ovn) (Array.to_list l)
      | _ -> raise (Query (Internal_Error ("Typeswitch handle called on non-typeswitch coreexp")))
  in
    (* Must be removed individually from the scopes.
       case $v 
         ...
       case $x  ($v is still free)
         $v 
    *)
  let scope_handle (ovn,dep) =
    let fvs = algop_get_use_counts dep in
    match ovn with 
	None -> fvs, []
      | Some v -> 
	  (* Odds are it will be used so, no .mem *)
	  (* This is stable *)
	  List.partition (fun (x,_) -> (not (v = x))) fvs
  in
    try
        List.split (List.map scope_handle (List.combine ovn_list dep))
    with Invalid_argument a ->
      raise (Query (Malformed_Algebra_Expr("There are differing numbers of dependent expressions and typeswitch cases.."^
				   a )))

(* If you appear as a dependent expression of one of the below ops,
   and are not bound by it, then you are used many times.
   Think: 
   let $y ..
   for $x in ..
     $y
   return ..
*)
let calculate_usage cur dep_free =
  match cur with 
  | AOESome _ | AOEEvery _
  | AOEMapFromItem _ | AOEMapToItem                          
  | AOEMap           | AOEMapConcat 
  | AOEOuterMapConcat _
    -> 
      List.map (fun (vn,(c,_)) -> (vn, (c, Many))) dep_free
  | _ -> dep_free
    
(* returns lists in sorted order (by cv_compare) *)
let compute_free_and_bound_vars cur indep dep =
  let bound_filter bv =
    List.partition (fun (x,_) -> not (List.mem_assoc x bv)) 
  in
  (* Get all the variables bound by this one *)
  let dep_free, bound_variables = 
    if is_typeswitch cur then
      handle_typeswitch cur dep 
    else
      begin
	let bound_variables,free_variables = extract_bound_free_variables cur in
	let fv_lists      = (List.map algop_get_use_counts dep) @ [free_variables] in
	let bound_removed,bound = List.split (List.map (bound_filter bound_variables) fv_lists) in
	  (* Returns a list of lists, each list in sorted order *)
	  (* If it is unused, it will not appear in bound, we want it to show up with useage 0 *)
	let bound_with_cur = List.map (List.merge use_count_compare bound_variables) bound in
	  bound_removed, bound_with_cur
      end
  in

    (* Usage Count: 
       If vn is bound outside a loop and we cross a loop, then it is a many time access
       for example:
       let $y := ..
       for $x in ..
           return $y/id *)

  let indep_free = List.map algop_get_use_counts indep in

  (* dep_free should be those variables which are free in the dep and not 
     used in this expression *)
  let dep_usage       = List.map (calculate_usage cur) dep_free in
  (* We couldn't have bound anything in the indeps and remove duplicates *)
  let free_variables  = variable_count_merge (indep_free @ dep_usage) in
  let bound_variables = variable_count_merge bound_variables in  
    free_variables, bound_variables
     

(* Tuple Access Rules:

   NOTE:
     This function assumes the indep_expr is already properly annotated. 
     
   TupleAccess if there is an access tuple
   TupleReturn if there is a  tuple return
   If TupleConcat just put in an ordered list and remove with duplicates

*)

let rec handle_returned_fields_and_dep be cur_op input_tuple_fields input_candidate_fields = 

  let indep          = cur_op.psub_expression in
  let dep            = cur_op.pdep_sub_expression in
  let indep_list     = subexpr_to_list indep in
  let indep_returned = List.concat (List.map algop_get_returned_fields indep_list) in

  let returned_tuple_fields, bdep_annotated  = 
    match cur_op.palgop_expr_name with 
        (* let-server-return and for-server-return 
	   propagates the tuple fields produced by its indep subexpr 
	*)
    | AOEServerImplements _
    | AOEExecute _ -> indep_returned, false 
    | AOEASyncExecute _ 
    | AOEForServerClose _ 
    | AOEEvalClosure -> indep_returned, false 
    | AOECreateTuple crname_array -> 
	  (* Just want to keep the names *)	  
	let crname_list = Array.to_list crname_array in
	
	  (* Don't want to use fst in case it changes *)
	let crnames     = List.map (fun (odt, name) -> name) crname_list in
	crnames, false

    | AOEConcatTuples -> 
	indep_returned, false 

    | AOEAccessTuple _ -> [], false               (* Tuple field access *)
    | AOENullMap v ->
	(v :: indep_returned), false

    | AOETupleTreePattern (input_field, pt) ->

	let outputs = get_all_outputs_from_twig_pattern pt in
(* 	  let remove_field fieldname fields = *)
(* 	    List.filter (fun (x) -> not (Namespace_names.rqname_equal fieldname x)) fields *)
(* 	  in *)
	    (* (outputs @ (remove_field input_field indep_returned)), false  *)
	[input_field] @ outputs @ indep_returned, false

    | AOEProject f_arr ->
        
	let is_project_field pfields field =
	  List.fold_left 
	    (fun b f -> (Namespace_names.rqname_equal field f) || b) false pfields
	in
	List.filter (fun f -> is_project_field (Array.to_list f_arr) f) indep_returned, false

    | AOEOuterMapConcat vn ->
	let dep_expression = access_onesub dep in
	let input_tuple    = algop_get_returned_fields (access_onesub indep) in
	let (_, input_tuple_candidates, _) = algop_get_tuple_field_use_counts (access_onesub indep) (*cur_op*) in
	annotate_compilation be input_tuple input_tuple_candidates dep_expression;
	    (* Dependent has precedent *)
	let return_fields = (algop_get_returned_fields dep_expression) @ 
	  input_tuple in
	(vn ::return_fields), true

    | AOEMapConcat ->
	let dep_expression = access_onesub dep in
	let input_tuple    = algop_get_returned_fields (access_onesub indep) in
	let (_, input_tuple_candidates, _) = algop_get_tuple_field_use_counts (access_onesub indep) (*cur_op*) in
	annotate_compilation be input_tuple input_tuple_candidates dep_expression;
	    (* Dependent has precedent *)
	let return_fields = (algop_get_returned_fields dep_expression) @ 
	  input_tuple in
	return_fields, true

    | AOEMapToItem -> (* unwraps tuples *)	  
	let dep_expression = access_onesub dep in
	let input_tuple    = algop_get_returned_fields (access_onesub indep) in
	let (_, input_tuple_candidates, _) = algop_get_tuple_field_use_counts (access_onesub indep) (*cur_op*) in
	annotate_compilation be input_tuple input_tuple_candidates dep_expression;
	[], true

    | AOEMapFromItem _ -> (* Item to tuple iteration *)
	  (* Does not introdcue a context tuple *)
	let dep_expression = access_onesub dep in
	annotate_compilation be input_tuple_fields input_candidate_fields dep_expression;
	algop_get_returned_fields dep_expression, true
	      (* Actual Tuple maps *)

    | AOEMapIndex iname               
    | AOEMapIndexStep iname ->
	(iname :: indep_returned), false

    | AOEMap          -> 
	let dep_expression = access_onesub dep in
	let input_tuple    = algop_get_returned_fields (access_onesub indep) in
	let (_, input_tuple_candidates, _) = algop_get_tuple_field_use_counts (access_onesub indep) (*cur_op*) in
	annotate_compilation be input_tuple input_tuple_candidates dep_expression;
	algop_get_returned_fields dep_expression, true

      (* ***
	 
	 Join, Select etc. bind the INPUT tuple - must reflect that here
	 for tuple field use count analysis!?
	 
	 - Michael
	 
	 *** *)
    | AOEJoin _ -> indep_returned, false
    | AOELeftOuterJoin (vn,pred_desc) -> (vn :: indep_returned), false

    | AOEProduct -> 
	let i1, i2 = access_twosub indep in
	
	let returned_fields  = (algop_get_returned_fields i1) @
	  (algop_get_returned_fields i2) in	 
	returned_fields, false
          
    | AOESelect _ (* Does not change the fields *)

    | AOEOrderBy _  -> indep_returned, false

      (* The index groups are stripped out,
         The returned_fields are those that are not stripped out *)
      (* This is not always the case... the scoping for groups needs work *)
      (* You can group on the same attribute in different group clauses *)
    | AOEGroupBy gd_list -> 
	  (* Should remove duplicates to speed the process *)
	  (*	  let group_names = List.flatten (List.map Xquery_algebra_ast_util.get_group_names gd_list) in          
	     let returned_names = List.map Xquery_algebra_ast_util.get_aggregate_name gd_list in
	     let returned_fields = (List.filter (fun x -> not (List.mem x group_names))
	     indep_returned) @ returned_names in	    
	     (Gmisc.remove_duplicates returned_fields), false	   
	  *)
	  (* The first one is the last applied *)
	let returned_names = 
	  begin
	    match gd_list with
	    | [] -> raise (Query (Malformed_Algebra_Expr ("Empty group description")))
	    | gd :: rest -> 
		(get_aggregate_name gd) (* The aggregate name and the induced names *)
		:: (get_induced_group gd)
	  end
	in

(*	  let returned_names = List.map Xquery_algebra_ast_util.get_aggregate_name gd_list in *)
(*	    (indep_returned @ returned_names), false (* There should not be any duplicates *) *)
	returned_names, false

    | AOEInputTuple -> 	  
	input_tuple_fields, false
	    (* This should return the fields of the indep expression of the map... *)

(*
   | AOEPrune _ | AOEDistinct _ ->
   [], false
*)
	  
    | _ -> (* Non tuple operations *)
	[], false
  in
    (* If the dependents were not annotated above, then do it now *)
  if (not bdep_annotated) then
    annotate_subexpr be input_tuple_fields input_candidate_fields dep
      ;
    (* On exit, annotated and returning return fields *)
  Gmisc.remove_duplicates returned_tuple_fields

(*************************************************)    
(* This function assumes the indep's annotations *)
(*    are set correctly.                         *)
(*************************************************)

and annotate_op_and_dep be input_tuple_fields input_candidate_fields cur =
  let cur_name        = cur.palgop_expr_name    in
  let indep           = cur.psub_expression     in
  let dep             = cur.pdep_sub_expression in 
  let indep_list      = subexpr_to_list indep   in


  let indep_accessed  = List.concat (List.map algop_get_accessed_fields indep_list) in
    (* This sets up the dep expressions too *)
  let returned_fields = handle_returned_fields_and_dep be cur input_tuple_fields input_candidate_fields in

  (* This needs the above to have setup the dep. expressions *)
  let dep_list        = subexpr_to_list dep   in
  let dep_accessed    = List.concat (List.map algop_get_accessed_fields dep_list)   in

  (* calculate tuples accessed *)
  let tuples_accessed = 
    match cur_name with
      | AOEAccessTuple (vn) ->	   
	  vn :: [] (* Should not have any indep expressions *)
      | AOETupleTreePattern (i, p) ->
          Gmisc.remove_duplicates (i :: indep_accessed @ dep_accessed) (* TupleTreePattern actually accesses a field ! *)
      | AOEGroupBy gd_list ->
	  let group_accessed  = List.concat (List.map get_group_names gd_list) in
	  let group_accessed2 = List.concat (List.map get_valid_names gd_list) in
	  let group_accessed3 = List.concat (List.map get_induced_group gd_list) in 
	    Gmisc.remove_duplicates (indep_accessed @ dep_accessed @ group_accessed @ group_accessed2 @ group_accessed3)	      
      | _ -> Gmisc.remove_duplicates (indep_accessed @ dep_accessed)
  in

  (************************************)
  (* Tuple field use counts - Michael *)
  (************************************)

  let (tfuc, cf, c) =
    if (!Conf.allow_streamed_tuple_fields)
    then
      (* Accessors *)
      let get_tuple_field_use_counts op = let (tfuc, _, _) = algop_get_tuple_field_use_counts op in tfuc in
      let get_candidate_fields op = let (_, cf, _) = algop_get_tuple_field_use_counts op in cf in
      let get_cardinality op = let (_, _, c) = algop_get_tuple_field_use_counts op in c in
      (* Cardinality checks *)
      let is_cardinality_many c =
	match c with
	| Table CMany -> true
	| Table COne -> false
	| _ -> raise (Query (Prototype "Expected Table cardinality!"))
      in
      (* Cardinality of cross product *)
      let cross_product_cardinality c1 c2 =
	let raise_no_table_cardinality () =
	  raise (Query (Prototype "Expected Table cardinality!"))
	in
	match c1 with
	| Table COne ->
	    begin
	      match c2 with
	      | Table COne -> Table COne
	      | Table CMany -> Table CMany
	      | _ -> raise_no_table_cardinality ()
	    end
	| Table CMany ->
	    begin
	      match c2 with
	      | Table _ -> Table CMany
	      | _ -> raise_no_table_cardinality ()
	    end
	| _ -> raise_no_table_cardinality ()
      in
      (* Use count construction *)
      let set_use_count_many tuple_fields = List.map (fun f -> (f, (0, Many))) tuple_fields in
      let set_use_count_once tuple_fields = List.map (fun f -> (f, (1, Once))) tuple_fields in
      (* Independent use count information *)
      let indep_tfuc = List.fold_left (List.merge use_count_compare) [] (List.map get_tuple_field_use_counts indep_list) in
      let indep_cf = List.fold_left (List.merge cv_compare) [] (List.map  get_candidate_fields indep_list) in
      let indep_rf  = List.fold_left (List.merge cv_compare) [] (List.map algop_get_returned_fields indep_list) in
      (* Dependent use count information *)
      let dep_tfuc = List.fold_left (List.merge use_count_compare) [] (List.map get_tuple_field_use_counts dep_list) in
      let dep_cf = List.fold_left (List.merge cv_compare) [] (List.map  get_candidate_fields dep_list) in	
      let dep_af  = List.fold_left (List.merge cv_compare) [] (List.map algop_get_accessed_fields dep_list) in

      (******************************************************************)
      (* Contribution of the current operator to tuple field use counts *)
      (******************************************************************)

      let (current_tfuc, current_cf, current_c) =
	match cur_name with
	    
	  (*
	    STATUS
	    ------

	     Aligned inference rules/code; several operators are not covered
	     properly, marked '(* *** OPEN ISSUE! *** *)'.


	    General observations and principles:
	    ------------------------------------
	    
	    1) Tuple field use count analysis uses the existing infrastructure
	       for determining the fields that are accessed and returned by
	       algebraic expressions. These are assumed to be available as
	       functions (s.b.), and are excluded from inference rules for
	       the sake of simplicity.

	    2) Besides counting literal accesses (AccessTuple operator), the
	       analysis takes 'iterating operators' (e.g., MapConcat) into
	       account.

	    2a) Iterating operators may not access tuple fields themselves, but
	        they can create output tuple streams that contain 'virtual' copies
	        of input tuples, although these copies are never actually made.
	        As a consequence, the notion of 'candidate tuple fields' will
	        be used to memorize those field names that are known to exist
	        'virtually' many times. Subsequent access to such a field later
	        on in the plan must be counted as multiple accesses.

	    2b) The notion of 'free' variables does also exist for tuple fields:
	        whenever a field is accessed in an expression expr, but not created
	        in expr (because it is created elsewhere and communicated via the
	        INPUT tuple), it is free in expr.

	    3) Some operators need to know the cardinality of a tuple stream in order
	       to drive iteration (a special case is to test wether a tuple stream is
               empty). No explicit access is counted for cardinality assessment.

	       NOTE: With XML token streams, the cardinality of a tuple stream that
	             results from mapping a streamed item sequence to tuple fields
	             does not 'come for free'. In case these fields are explicitly
	             accessed at some point, the cardinality information falls out
	             as a by-product (without actually being an additional access),
	             but in case there is no explicit access, item boundaries remain
	             'undiscovered'. As a consequence, zero use count tuple fields
	             are associated dummy-consumers during code selection.

	     4) Some operators materialize input tuple fields (e.g., Product).
	        This must be counted as a single access to each of those fields.

	     5) *** What about the Cursor.cursor_peek in NullMap etc.??? ***

	     6) *** Could get rid of dependency of [rf(indep)/]af(dep) completely
	            (model everything by means of candidates, see Join rules; need
	            then correct propagation of input tuple candidates above!)?! ***


	    Assumptions
	    -----------
	    
	    1) Tuple field names are unique throughout the query plan.
	    
	    2) A tuple operator always processes all tuples in its input in the same
	       fashion. In particular, tuple field accesses affect potentially all
	       'virtual' copies of a field.

	    
	    Implementation
	    --------------
	    
	    The following approach is set-based. '+' stands for set-union, 
	    '*' for intersection. 'm(s)' sets the use count of all the
	    tuple fields in s to '>1', o(s) to '1'. rf(e) gets the returned
	    fields of expression e, (af) the accessed fields, and cf(e) the
	    candiate fields (s.b.). c(e) is its cardinality (1 for a single
	    tuple, >1 for a table of multiple tuples, and _ in case e does
	    not return a table). Cardinality is needed in order to identify
	    non-iterating map concat operations resulting from let-bindings
	    inside for-loops.
	    
	    The algorithm proceeds by first calculating the contribution
	    of the current operator. This is then combined in a uniform
	    fashion (same for each kind of operator) with the analysis
	    results from indep./dep. subexpressions (component-wise set 
	    union, where use counts are merged implicitly).

	    The environment 'env' consists of a single slot for communicating
	    candidate fields in analogy to data flow through the INPUT tuple
	    (icf, INPUT Candidate Fields).

	    [Augmentation  of the environment 'env' actually happens in
	     handle_returned_fields_and_dep.]

	    Analysis result triple:
	    -----------------------

	     ('tuple field use count information (accessed fields)', -- tfuc
               Name of tuple field, (None|Once|Many) accesses

	      'candidates fields (names of fields that for which     -- cf
	                          any subsequent access implies
	                          repeated access)',
	      'operator output cardinality')                         -- c
 
            Mary's notes:

              Operator output cardinality: conservative estimate of
              the number of tuples produced by the operator.
              Necessary for let bindings, which gets compiled into
              MapConcat.

              Analysis takes iteration into account.  

              

	  *)
	  (*************************)
	  (* Polymorphic operators *)
	  (*************************)

	  | AOEExecute _ ->
              (* Execute is a polymorphic operator, which inherits the
                 annotation of its independent operator. *)
	      let (indep1, indep2) = access_twosub indep in
	      let _ = access_nosub dep in
		(get_tuple_field_use_counts indep2, get_candidate_fields indep2, get_cardinality indep2)

	  (*******************)
	  (* Tuple operators *)
	  (*******************)

	  (* InputTuple *)

	  (* We just have to place the candidate fields of the INPUT tuple
	     from the context to the corresponding field in the result. *)
	    
	  (*
	    env : icf
	    -------------------------------------
	    env |- InputTuple{}() -> ({}, icf, 1)
	  *)

	  (* FINAL *)
	  (*
	    UCC = (CF_Input, RF_Input)
	    -------------------------------------------------------------
	    UCC |- InputTuple{}() uses ({}, CF_Input, 1) returns RF_Input
	  *)

	  | AOEInputTuple ->
	      let _ = access_nosub indep in
	      let _ = access_nosub dep in
		([], input_candidate_fields, Table COne)


	  (* MapFromItem *)

	  (* MapFromItem does not bind the INPUT tuple. That is, INPUT tuple
	     fields are 'free' - accesses to INPUT tuple fields in the dep.
	     branch must be counted as repeated accesses. *)

	  (*
	    tfuc = m(rf(IN) * af(dep))
	    -------------------------------------------------
	    env' |- MapFromItem{dep}(indep) -> (tfuc, {}, >1)
	  *)
		
	  (* FINAL *)
	  (*
	    UCC = (CF_Input, RF_Input)
	    UCC |- Indep uses (UF_Indep, CF_Indep, C_Indep) returns RF_Indep

	    UCC' = (CF_Input + RF_Input, RF_Input)
	    UCC' |- Dep uses (UF_Dep, CF_Dep, C_Dep) returns RF_Dep
	    ----------------------------------------------------------------------------------------------
	    UCC' |- MapFromItem{Dep}(Indep) uses (UF_Indep + UF_Dep, CF_Indep + CF_Dep, >1) returns RF_Dep
	  *)
		
	  | AOEMapFromItem _ ->
	      let _ = access_onesub indep in
	      let _ = access_onesub dep in

	      let tfuc = Gmisc.intersect_list input_tuple_fields dep_af in
		(set_use_count_many tfuc, [], Table CMany)


	  (* AccessTuple *)

	  (* We have to count at least a single access for q. In case it is
	     included in the INPUT tuple candidates, we must count multiple
	     accesses. 
             +: adds the use counts for a particular tuple field. 
          *)

	  (*
	    env : icf
	    ------------------------------------------------------------
	    env |- AccessTuple[q]{}() -> ({(q,1)} + m({q} * icf), {}, _)
	  *)

	  (* FINAL *)
	  (*
	    UCC = (CF_Input, RF_Input)
	    -----------------------------------------------------------------------------
	    UCC |- AccessTuple[q]{}() uses (o({q}) + m({q} * CF_Input), {}, _) returns {}
	  *)

	  | AOEAccessTuple crname ->
	      let _ = access_nosub indep in
	      let _ = access_nosub dep in

	      (* AOEAccessTuple implicitly accesses the input tuple. *)
	      let qualified_candidates = set_use_count_many (Gmisc.intersect_list [crname] input_candidate_fields) in
		((crname, (1, Once)) :: qualified_candidates, [], NoTable)


	  (* CreateTuple *)

	  (* We must add q1,...,qn with zero accesses for the analysis
	     to properly merge use counts, later. *)

	  (*
	    -----------------------------------------------------------------------
	    env |- CreateTuple[q1,...,qn]{}(e) -> ({(q,0)} + ... + {(qn,0)}, {}, 1)
	  *)

	  (* FINAL *)
	  (*
	    UCC |- Indep_1 uses (UF_Indep_1, CF_Indep_1, C_Indep_1) returns RF_Indep_1 
	    ...
	    UCC |- Indep_n uses (UF_Indep_n, CF_Indep_n, C_Indep_n) returns RF_Indep_n
	    -------------------------------------------------------------------------------------------------------------
	    UCC |- CreateTuple[q_1,...q_n]{}(Indep_1,...,Indep_n) uses ({(q_1,0),...(q_n,0)}, {}, 1) returns {q_1,...q_n}
	  *)

	  | AOECreateTuple crname_array ->
	      let _ = access_manysub indep in
	      let _ = access_nosub dep in

	      let crname_list = Array.to_list crname_array in
	      let fresh_fields = List.map (fun (odt, name) -> (name, (0, Never))) crname_list in
		(fresh_fields, [], Table COne)


	  (* MapConcat *)

	  (*       
		       a 1
		       b 1
		       c 1
	    	       ...

		       a'2
		       b'2
		       c'2
		       ...

		        |              
		    MapConcat
		       / \
                dep.  /   \  indep.

		     a     1
		     b     2
		     c     3
		    ...   ...

	     In case the dep. branch has cardinality >1, we know that, should
	     any field returned by indep. ever be accessed later on (as part of
	     the result tuple stream of the MapConcat, this access would have to
	     be counted as multiple accesses (iteration). This is achieved by
	     memorizing the corresponding field names as 'candidates'.

	     What about the other direction? If the cardinality of the indep. branch
	     is >1, then we know that the dep. expression is evaluated multiple times,
	     once for each tuple of indep., that is, once for each possible state of
	     the INPUT tuple. Subsequent accesses to tuple fields do not target one and
	     the same, but distinct values, and do not have to be counted as multiple
	     accesses, thus.

	     Do we have to take into account that fields may have been injected
	     somewhere in the dep. branch by means of the INPUT tuple from an
	     operator above? No, because MapConcat binds INPUT itself. That is,
	     there can't be multiple accesses on 'free' input tuple fields in
	     the dep. branch.
	  *)

	  (*
	    env' = cf(indep)
	    
	    cf   = if c(dep) > 1 then rf(indep) else {}
	    
	    c    = if c(dep) = c(indep) = 1 then 1 else >1
	    ----------------------------------------------
	    env' |- MapConcat{dep}(indep) -> ({}, cf, c)
	  *)

	  (* FINAL *)
	  (*
	    UCC = (CF_Input, RF_Input)
	    UCC |- Indep uses (UF_Indep, CF_Indep, C_Indep) returns RF_Indep

	    UCC' = (CF_Indep, RF_Indep)
	    UCC' |- Dep uses (UF_Dep, CF_Dep, C_Dep) returns RF_Dep

	    CF = if (C_Dep > 1) then RF_Indep else {}

	    C = max(C_Indep, C_Dep)
	    -----------------------------------------------------------------------------------------------------------
	    UCC' |- MapConcat{Dep}(Indep) uses (UF_Dep + UF_Indep, CF + CF_Dep + CF_Indep, C) returns RF_Indep + RF_Dep
	  *)

	  | AOEMapConcat ->
	      let indep_c = get_cardinality (access_onesub indep) in
	      let dep_c = get_cardinality (access_onesub dep) in

	      let cf = if (is_cardinality_many dep_c) then indep_rf else [] in

	      let c = cross_product_cardinality indep_c dep_c in
		([], cf, c)


	  (* OuterMapConcat *)

	  (* What about the Cursor.cursor_peek in the null test? - Michael *)

	  (*
	    env' = cf(indep)
	    
	    cf   = if c(dep) > 1 then rf(indep) else {}
	    
	    c    = if c(dep) = c(indep) = 1 then 1 else >1
	    ----------------------------------------------------------
	    env' |- OuterMapConcat[q]{dep}(indep) -> ({(q,0)}, cf, >1)
	  *)

	  | AOEOuterMapConcat crname ->
	      let indep_c = get_cardinality (access_onesub indep) in
	      let dep_c = get_cardinality (access_onesub dep) in

	      let cf = if (is_cardinality_many dep_c) then indep_rf else [] in

	      let c = cross_product_cardinality indep_c dep_c in
		([(crname, (0, Never))], cf, c)
	      

	  (* Product *)

	  (* A Product is (semantically) fully symmetric. The implementation does only
	     materialize the right side, though. This has to be taken care of during
	     physical typing. 

          *)

	  (* OLD *)
	  (*
	    cf1  = if c(i1) > 1 then rf(i2) else {}
	    cf2  = if c(i2) > 1 then rf(i1) else {}
	    
	    c = if c(i1) = c(i2) = 1 then 1 else >1
	    ---------------------------------------------
	    env |- Product{}(i1,i2) -> ({}, cf1 + cf2, c)
	  *)

	  (* NEW *)
	  (* This is tricky!
	     The materialization of a streamed tuple field on the right hand side
	     consumes the stream cursor, and must be counted as a single access.

	     E.g.   for $x in $xs,
	                $y in $ys    =>    Product(MapFromItem{...}(...),
	            let $z := $y                   MapConcat{[z:IN#y]}(...))
                    return $z

	     If $y was bound to a stream (which was the case if only one explicit
	     access was counted), that stream would be consumed when $z is bound
	     (and materialized, because it is obviously accessed repeatedly).
	     Additional materialization by the Product would then access an
	     exhausted stream cursor in field $y.
	  *)

	  (*
	    cf1  = if c(i1) > 1 then rf(i2) else {}
	    cf2  = if c(i2) > 1 then rf(i1) else {}
	    
	    tfuc = o(rf(i2))
	    
	    c = if c(i1) = c(i2) = 1 then 1 else >1
	    -----------------------------------------------
	    env |- Product{}(i1,i2) -> (tfuc, cf1 + cf2, c)
	  *)

	  (* FINAL *)
	  (*
	    UCC |- Indep_1 uses (UF_Indep_1, CF_Indep_1, C_Indep_1) returns RF_Indep_1 
	    UCC |- Indep_2 uses (UF_Indep_2, CF_Indep_2, C_Indep_2) returns RF_Indep_2 

	    CF_1 = if (C_Indep1 > 1) then RF_Indep2 else {}
	    CF_2 = if (C_Indep2 > 1) then RF_Indep1 else {}
	    CF = CF_1 + CF_2 + CF_Indep_1 + CF_Indep_2

	    UF = o(RF_Indep2)

	    C = max(C_Indep_1, C_Indep_2)
	    ------------------------------------------------------------------------------------------------------------
	    UCC |- Product{}(Indep_1,Indep_2) uses (UF + UF_Indep_1 + UF_Indep_2, CF, C) returns RF_Indep_1 + RF_Indep_2
	  *)

 	  | AOEProduct ->
	      let _ = access_nosub dep in
	      let (indep1, indep2) = access_twosub indep in
	      let (rf1, rf2) = (algop_get_returned_fields indep1, algop_get_returned_fields indep2) in
	      let (c1, c2) = (get_cardinality indep1, get_cardinality indep2) in

	      let cf1 = if (is_cardinality_many c1) then rf2 else [] in
	      let cf2 = if (is_cardinality_many c2) then rf1 else [] in

	      let tfuc = set_use_count_once rf2 in

	      let c = cross_product_cardinality c1 c2 in
		(tfuc, cf1 @ cf2, c)


	  (* Join *)

	  (* For joins, it actually matters what physical instantiation is
	     chosen.
	     
	     A Nested Loop Join is implemented as a Product, followed by a
	     Select.The right hand side is materialized into an array, and the
	     left side scanned just once. Each tuple field used in a predicate
	     is potentially accessed multiple times (true for both left and right
	     hand sides). This corresponds to memorizing the affected fields
	     as candidates (Product) and counting multiple accesses later on
	     (Select).
	     
	     For a Hash Join, the right side is materialized to a hash table,
	     and the left side scanned just once. Probing for matches requires
	     only one access per tuple field on the left side; right side tuple
	     fields may be probed multiple times.

	     In both cases, there are two 'components' that have to be considered:
	     1) multiple accesses inside the predicates
	     2) potential multiple accesses later (candidate generation in analogy
	        to Products) *)

	  (* OLD *)
	  (*
	    env' = cf(i1 + i2)
	    
	    tfuc1 = if c(i1) > 1 then rf(i2) * af(d1) + ... + af(dn)
	    tfuc2 = if c(i2) > 1 then rf(i1) * af(d1) + ... + af(dn)
	    
	    cf1   = if c(i1) > 1 then rf(i2) else {}
	    cf2   = if c(i2) > 1 then rf(i1) else {}
	    
	    c = if c(i1) = c(i2) = 1 then 1 else >1
	    ----------------------------------------------------------------------------
	    env' |- NestedLoopJoin{d1,...,dn}(i1,i2) -> (m(tfuc1 + tfuc2), cf1 + cf2, c)
	  *)
		  
	  (* This assumes the hash table bucket size is 1. - Michael *)

	  (* OLD *)		  
	  (*
	    env' = cf(i1 + i2)                                            
	    
	    tfuc = if c(i1) > 1 then rf(i2) * af(d1) + ... + af(dn)
	    
	    cf1  = if c(i1) > 1 then rf(i2) else {}
	    cf2  = if c(i2) > 1 then rf(i1) else {}
	    
	    c = if c(i1) = c(i2) = 1 then 1 else >1
	    -------------------------------------------------------------
	    env' |- HashJoin{d1,...,dn}(i1,i2) -> (m(tfuc), cf1 + cf2, c)
	  *)


	  (* This conservatively assumes a Nested Loop Join (= Select{...}(Product(...))). *)

	  (* NEW *)
	  (*
	    We have to count a single access for each tuple field of the right hand side
	    independent subexpression (that side is always materialized; see Product).
	  *)

	  (*
	    env' = cf(i1) + cf(i2)

	    // tfuc1, tfuc2 should be modeled via candidate fields cf1, cf2, as follows:
	    //cf1   = if c(i1) > 1 then rf(i2) else {}
	    //cf2   = if c(i2) > 1 then rf(i1) else {}
	    //env' = cf(i1) + cf(i2) + cf1 + cf2
	    //Can the current candidate fields be uniformly bound to the input candidate
	    //fields of indep. expressions whenever the current op. binds INPUT? NO (MapConcat)!


	    tfuc2 = if c(i2) > 1 then rf(i1) * af(d1) + ... + af(dn) else {}
	    tfuc3 = rf(i2)
	    
	    cf1   = if c(i1) > 1 then rf(i2) else {}
	    cf2   = if c(i2) > 1 then rf(i1) else {}
	    
	    c = if c(i1) = c(i2) = 1 then 1 else >1
	    ---------------------------------------------------------------------------------------
	    env' |- NestedLoopJoin{d1,...,dn}(i1,i2) -> (m(tfuc1 + tfuc2) + o(tfuc3), cf1 + cf2, c)
	  *)

	  (* FINAL *)
	  (* For a HashJoin, don't have to propagate left branch returned fields as candidates to
	     the dependent expressions. There is at least one access though?! But it's not table
	     materialization as for the right branch. *)
	  (*
	    UCC = (CF_Input, RF_Input)
	    UCC |- Indep_1 uses (UF_Indep_1, CF_Indep_1, C_Indep_1) returns RF_Indep_1
	    UCC |- Indep_2 uses (UF_Indep_2, CF_Indep_2, C_Indep_2) returns RF_Indep_2

	    // candidate generation as in a Product
	    CF_1 = if (C_Indep1 > 1) then RF_Indep2 else {}
	    CF_2 = if (C_Indep2 > 1) then RF_Indep1 else {}
	    CF = CF_1 + CF_2 + CF_Indep_1 + CF_Indep_2

	    // candidates must be propagated to the dep. branches
	    UCC' = (CF, RF_Indep1 + RF_Indep2)
	    UCC' |- Dep_1 uses (UF_Dep_1, CF_Dep_1, C_Dep_1) returns RF_Dep_1 
	    ...
	    UCC' |- Dep_n uses (UF_Dep_n, CF_Dep_n, C_Dep_n) returns RF_Dep_n

	    // materialization of the right hand side corresponds to one access
	    UF = o(RF_Indep2) + UF_Dep_1 + ... + UF_Dep_n + UF_Indep_1 + UF_Indep_2

	    C = max(C_Indep_1, C_Indep_2)
	    -------------------------------------------------------------------------------------------------------------------------------------
	    UCC' |- NestedLoopJoin{Dep_1,...,Dep_n}(Indep_1,Indep_2) uses (UF, CF + CF_Dep_1 + ... + CF_Dep_n, C) returns RF_Indep_1 + RF_Indep_2
	  *)

	  | AOEJoin _ ->
	      let _ = access_manysub dep in
	      let (indep1, indep2) = access_twosub indep in

	      let (c1, c2) = (get_cardinality indep1, get_cardinality indep2) in
	      let (rf1, rf2) = (algop_get_returned_fields indep1, algop_get_returned_fields indep2) in


	      let tfuc1 = if (is_cardinality_many c1) then (Gmisc.intersect_list rf2 dep_af) else [] in
	      let tfuc2 = if (is_cardinality_many c2) then (Gmisc.intersect_list rf1 dep_af) else [] in
	      let tfuc3 = rf2 in

	      let cf1 = if (is_cardinality_many c1) then rf2 else [] in
	      let cf2 = if (is_cardinality_many c2) then rf1 else [] in

	      let c = cross_product_cardinality c1 c2 in
		((set_use_count_many (tfuc1 @ tfuc2)) @ (set_use_count_once tfuc3), cf1 @ cf2, c)


	  (* FINAL *)

	  (* Does a boolean field exist virtually many times in the result stream? - Michael *)

	  (*
	    UCC = (CF_Input, RF_Input)
	    UCC |- Indep_1 uses (UF_Indep_1, CF_Indep_1, C_Indep_1) returns RF_Indep_1
	    UCC |- Indep_2 uses (UF_Indep_2, CF_Indep_2, C_Indep_2) returns RF_Indep_2

	    // candidate generation as in a Product
	    CF_1 = if (C_Indep1 > 1) then RF_Indep2 else {}
	    CF_2 = if (C_Indep2 > 1) then RF_Indep1 else {}
	    CF = CF_1 + CF_2 + CF_Indep_1 + CF_Indep_2

	    // candidates must be propagated to the dep. branches
	    UCC' = (CF, RF_Indep1 + RF_Indep2)
	    UCC' |- Dep_1 uses (UF_Dep_1, CF_Dep_1, C_Dep_1) returns RF_Dep_1 
	    ...
	    UCC' |- Dep_n uses (UF_Dep_n, CF_Dep_n, C_Dep_n) returns RF_Dep_n

	    // materialization of the right hand side corresponds to one access
	    UF = {(q,0)} + o(RF_Indep2) + UF_Dep_1 + ... + UF_Dep_n + UF_Indep_1 + UF_Indep_2

	    C = max(C_Indep_1, C_Indep_2)
	    -------------------------------------------------------------------------------------------------------------------------------------------------
	    UCC' |- NestedLoopLeftOuterJoin[q]{Dep_1,...,Dep_n}(Indep_1,Indep_2) uses (UF, CF + CF_Dep_1 + ... + CF_Dep_n, C) returns RF_Indep_1 + RF_Indep_2
	  *)

	  | AOELeftOuterJoin (crname, _) ->
	      let dep = access_manysub dep in
	      let _ = Array.to_list dep in
	      let (indep1, indep2) = access_twosub indep in

	      let (c1, c2) = (get_cardinality indep1, get_cardinality indep2) in
	      let (rf1, rf2) = (algop_get_returned_fields indep1, algop_get_returned_fields indep2) in
	      let tfuc1 = if (is_cardinality_many c1) then (Gmisc.intersect_list rf2 dep_af) else [] in
	      let tfuc2 = if (is_cardinality_many c2) then (Gmisc.intersect_list rf1 dep_af) else [] in
	      let tfuc3 = rf2 in
	      let cf1 = if (is_cardinality_many c1) then rf2 else [] in
	      let cf2 = if (is_cardinality_many c2) then rf1 else [] in

	      let c = cross_product_cardinality c1 c2 in
		((crname, (0, Never)) :: [] @ (set_use_count_many (tfuc1 @ tfuc2)) @ (set_use_count_once tfuc3), cf1 @ cf2, c)


	  (* Map, Select *)

	  (* Cardinalities could be refined! - Michael *)

	  (*
	    env' |- cf(indep)
	    -----------------------------------------
	    env' |- Map{dep}(indep)    -> ({}, {}, >1)
	    env' |- Select{dep}(indep) -> ({}, {}, >1)
	  *)

	  (* FINAL *)
	  (*
	    UCC = (CF_Input, RF_Input)
	    UCC |- Indep uses UF_Indep cand CF_Indep ret RF_Indep cand C_Indep

	    UCC' = (CF_Indep, RF_Indep)
	    UCC' |- Dep_1 uses UF_Dep_1 cand CF_Dep_1 ret RF_Dep_1 cand C_Dep_1
	    --------------------------------------------------------------------------------------
	    UCC' |- Map{Dep}(Indep) uses (UF_Dep + UF_Indep, CF_Dep + CF_Indep, >1) returns RF_Dep
	  *)

	  | AOEMap ->
	      let _ = access_onesub indep in
	      let _ = access_onesub dep in
		([], [], Table CMany)

	  (* FINAL *)
	  (*
	    UCC = (CF_Input, RF_Input)
	    UCC |- Indep uses (UF_Indep, CF_Indep, C_Indep) returns RF_Indep 

	    UCC' = (CF_Indep, RF_Indep)
	    UCC' |- Dep_1 uses (UF_Dep_1, CF_Dep_1, C_Dep_1) returns RF_Dep_1
	    ...
	    UCC' |- Dep_n uses (UF_Dep_n, CF_Dep_n, C_Dep_n) returns RF_Dep_n 
	    ------------------------------------------------------------------
	    UCC' |- Select{Dep_1,...,Dep_n}(Indep) uses 
                    (UF_Dep_1 + ... + UF_Dep_n + UF_Indep, CF_Dep_1 + ... + CF_Dep_n + CF_Indep, >1) 
                    returns RF_Indep
	  *)

	  | AOESelect _  ->
	      let _ = access_onesub indep in
	      let _ = access_manysub dep in
		([], [], Table CMany)


	  (* NullMap *)

	  (* *** OPEN ISSUE! *** *)
	  (* Cursor.cursor_is_empty does a peek, implications? - Michael *)

	  (* The null field is 'virtually' repeated for each tuple in
	     the indep. tuple stream. *)

	  (*
	    cf = if (c(indep) > 1) then q else {}
	    
	    c = c(indep)
	    ---------------------------------------------------
	    env |- NullMap[q]{}(indep)      -> ({(q,0)}, cf, c)
	  *)

	  (* FINAL *)
	  (*
	    UCC |- Indep uses (UF_Indep, CF_Indep, C_Indep) returns RF_Indep
	    CF = if (C_Indep > 1) then {q} else {}
	    -------------------------------------------------------------------------------------------------
	    UCC |- NullMap[q]{}(Indep) uses ({(q, 0)} + UF_Indep, CF + CF_Indep, >1) returns ({q} + RF_Indep)
	  *)

	  | AOENullMap crname ->
	      let indep_c = get_cardinality (access_onesub indep) in
	      let _ = access_nosub dep in

	      let cf = if (is_cardinality_many indep_c) then [crname] else [] in
		((crname, (0, Never)) :: [], cf, indep_c)


	  (* MapIndex, MapIndexStep *)

	  (* *** OPEN ISSUE! *** *)
	  (* Cursor.cursor_is_empty does a peek, implications? - Michael *)

	  (*
	    c = c(indep)
	    ---------------------------------------------------
	    env |- MapIndex[q]{}(indep)     -> ({(q,0)}, {}, c)
	    env |- MapIndexStep[q]{}(indep) -> ({(q,0)}, {}, c)
	  *)

	  (* FINAL *)
	  (*
	    UCC |- Indep uses (UF_Indep, CF_Indep, C_Indep) returns RF_Indep
	    ------------------------------------------------------------------------------------------------------
	    UCC |- MapIndexStep[q]{}(Indep) uses ({(q, 0)} + UF_Indep, CF_Indep, C_Indep) returns (RF_Indep + {q})
	    UCC |- MapIndex[q]{}(Indep) uses ({(q, 0)} + UF_Indep, CF_Indep, C_Indep) returns (RF_Indep + {q})
	  *)

	  | AOEMapIndex crname
	  | AOEMapIndexStep crname ->
	      let indep_c = get_cardinality (access_onesub indep) in
	      let _ = access_nosub dep in
		((crname, (0, Never)) :: [], [], indep_c)


	  (* TreeJoin *)

	  (* Must return Table cardinality here in case of sort joins! - Michael *)
		  
	  (*
	    ----------------------------------------------
	    env |- Treejoin[a::nt]{}(indep) -> ({}, {}, _)
	  *)

	  (* FINAL *)
	  (*
	    UCC |- Indep uses (UF_Indep, CF_Indep, C_Indep) returns RF_Indep
	    -----------------------------------------------------------------------
	    UCC |- TreeJoin[a::nt]{}(Indep) uses (UF_Indep, CF_Indep, _) returns {}
	  *)

	  | AOETreeJoin _ ->
	      let _ = access_onesub indep in
	      let _ = access_nosub dep in
		([], [], NoTable)


	  (* FINAL *)

	  (* Overly conservative? Here, the strategy is diffenent than, e.g., OrderBy where
	     materialization is enforced by the operator itself and not by conservative use
	     counts. - Michael *)

	  (*
	    UCC |- Indep uses (UF_Indep, CF_Indep, C_Indep) returns RF_Indep
	    RF = {q_1,...,q_n} + RF_Indep
	    UF = m(RF)
	    --------------------------------------------------------------------------------
	    UCC |- TupleTreePattern[q_1,...,q_n]{}(Indep) uses (UF, CF_Indep, >1) returns RF
	  *)

	  | AOETupleTreePattern (input, pattern) ->
	      let _ = access_onesub indep in
	      let _ = access_nosub dep in

	      let outputs = get_all_outputs_from_twig_pattern pattern in
		(set_use_count_many (input :: outputs), [], Table CMany)


	  (* ConcatTuples *)

	  (*
	    -------------------------------------------
	    env |- ConcatTuples{}(i1,i2) -> ({}, {}, 1)
	  *)

	  (* FINAL *)
	  (*
	    UCC |- Indep_1 uses (UF_Indep_1, CF_Indep_1, C_Indep_1) returns RF_Indep_1
	    UCC |- Indep_2 uses (UF_Indep_2, CF_Indep_2, C_Indep_2) returns RF_Indep_2
	    ---------------------------------------------------------------------------------------------------------------------------------
	    UCC |- ConcatTuples{}(Indep_1,Indep_2) uses (UF_Indep_1 + UF_Indep_2, CF_Indep_1 + CF_Indep_1, 1) returns RF_Indep_1 + FR_Indep_2
	  *)

	  | AOEConcatTuples ->
	      let _ = access_twosub indep in
	      let _ = access_nosub dep in
		([], [], Table COne)


	  (* Order By *)

	  (* This assumes repeated evalutation of repeatedly needed values. *)

	  (* OLD *)
	  (*
	    tfuc = rf(indep) * (af(d1) + ... + af(dn))
	    ----------------------------------------------------
	    env |- OrderBy{d1,...dn}(indep) -> (m(tfuc), {}, >1)
	  *)

	  (* NEW *)
	  (*
	    OrderBy materializes all input tuple fields. This has to be
	    counted as a single access (see Product)!
	  *)

	  (*
	    tfuc1 = o(rf(indep))
	    tfuc2 = m(rf(indep) * (af(d1) + ... + af(dn)))
	    ----------------------------------------------------------
	    env |- OrderBy{d1,...dn}(indep) -> (tfuc1 + tfuc2, {}, >1)
	  *)

	  (* FINAL *)

	  (* Can make use of C_Indep = 1 - Michael *)

	  (*
	    UCC |- Indep uses (UF_Indep, CF_Indep, C_Indep) returns RF_Indep

	    // candidate generation equivalent to UF' = RF_Indep * (AF_Dep_1 + ... + AF_Ddep_n)
	    UCC' = (CF_Indep + RF_Indep, RF_Indep)
	    UCC' |- Dep_1 uses (UF_Dep_1, CF_Dep_1, C_Dep_1) returns RF_Dep_1
	    ...
	    UCC' |- Dep_n uses (UF_Dep_n, CF_Dep_n, C_Dep_n) returns RF_Dep_n

	    // materialization corresponds to one access
	    UF = o(RF_Indep) + UF_Dep_1 + ... + UF_Dep_n + UF_Indep
	    CF = CF_Dep_1 + ... + CF_Dep_n + CF_Indep
	    ------------------------------------------------------------------------------------
	    UCC' |- OrderBy[osl]{Dep_1,...,Dep_n}(Indep) uses (UF, CF, C_Indep) returns RF_Indep
	  *)

	  | AOEOrderBy _ ->
	      let _ = access_onesub indep in
	      let _ = access_manysub dep in

	      let tfuc1 = set_use_count_once indep_rf in
	      let tfuc2 = set_use_count_many (Gmisc.intersect_list indep_rf dep_af) in
		(tfuc1 @ tfuc2, [], Table CMany)


	  (* Group By *)

	  (* *** OPEN ISSUE! *** *)
	  (* This does internal sorting (and materialization?), finish this! - Michael *)

	  (* cf = {a1,...,an} *)
	  (* -------------------------------------------------- *)
	  (* env |- GroupBy[[g1_1,...,g1_k][v1_1,...v1_l][a1]   *)
	  (*                   ...                              *)
	  (*                [gn_1,...,gn_k][vn_1,...,vn_l][an]] *)
	  (*                {d1,...,dm}(indep) -> ({}, cf, >1)  *)

	  | AOEGroupBy gds ->
	      let _ = access_onesub indep in
	      let _ = access_manysub dep in

	      let cf = List.map (fun gd -> let (odt, an) = gd.aggregate_name in an) gds in

	      (* Added 07|03|2006 - Michael *)
	      let tfuc1 = set_use_count_once indep_rf in
		(*([], cf, Table CMany)*)
		(tfuc1, cf, Table CMany)


	  (* *** OPEN ISSUE! *** *)
	  (* handle these! *)
	  | AOEProject _ ->
	      (* this is a guess -- Ph *)
	      let _ = access_onesub indep in
	      let _ = access_nosub dep in
	      let tfuc1 = set_use_count_once indep_rf in
		(tfuc1, indep_rf, Table CMany)

(*
	  | AOEDistinct _
	  | AOEPrune _
*)

	  (* raise (Query (Prototype "Cannot handle this kind of operator in tuple field use count analysis!")) *)


	  (***********************)
	  (* Non-tuple operators *)
	  (***********************)

	  (* Some, Every *)

	  (* The returned fields of the input tuple have been created 
	     elsewhere; as soon as they are accessed in the dep. branch
	     of a quantifier, it is those fields that are accessed
	     repeatedly ('free accessed fields'). *)

	  (*
	    tfuc = m(rf(IN) * af(dep))
	    --------------------------------------------
	    env |- Some[q]{dep}(indep)  -> (tfuc, {}, _)
	    env |- Every[q]{dep}(indep) -> (tfuc, {}, _)
	  *)

	  (* FINAL *)
	  (*
	    UCC = (CF_Input, RF_Input)
	    UCC |- Indep uses (UF_Indep, CF_Indep, C_Indep) returns RF_Indep

	    // candidate generation equivalent to UF' = RF_Indep * (AF_Dep)
	    UCC' = (CF_Input + RF_Input, RF_Input)
	    UCC' |- Dep uses (UF_Dep, CF_Dep, C_Dep) returns RF_Dep
	    ------------------------------------------------------------------------------------
	    UCC' |-  Some{Dep}(Indep) uses (UF_Dep + UF_Indep, CF_Dep + CF_Indep, >1) returns {}
	    UCC' |- Every{Dep}(Indep) uses (UF_Dep + UF_Indep, CF_Dep + CF_Indep, >1) returns {}
	  *)

	  | AOESome _
	  | AOEEvery _ ->
             (* this will throw an exception for the matches above !!
	      let _ = access_onesub indep in
	      let _ = access_onesub dep in *)
	      let free_accessed = Gmisc.intersect_list input_tuple_fields dep_af in
		(set_use_count_many free_accessed, [], NoTable)



	  | _ -> ([], [], NoTable)
		
      in

      (* Need to sort everything for succesful merge. *)
      let current_tfuc = List.fold_left (List.merge use_count_compare) [] (List.map (fun uc -> [uc]) current_tfuc) in
      let current_cf = List.fold_left (List.merge cv_compare) [] (List.map (fun uc -> [uc]) current_cf) in
	((variable_count_merge ([current_tfuc] @ [indep_tfuc] @ [dep_tfuc])),
	 Gmisc.remove_duplicates (List.fold_left (List.merge cv_compare) [] ([current_cf] @ [indep_cf] @ [dep_cf])),
	 current_c)
    else
      ([], [], NoTable)
  in
    
  (***********************************************************************************)

  let free_variables, bound_variables =
    compute_free_and_bound_vars cur_name indep_list dep_list in
  (* calculate returned tuple fields *)
  mk_annotation free_variables bound_variables tuples_accessed returned_fields (tfuc, cf, c)


(*************************)
(* Walks a Subexpression *)
(*************************)

and annotate_subexpr be input_tuple input_candidate_fields sub =
  match sub with
  | NoSub -> ()
  | OneSub s -> annotate_compilation be input_tuple input_candidate_fields s
  | TwoSub (s0,s1) -> 
      annotate_compilation be input_tuple input_candidate_fields s0;
      annotate_compilation be input_tuple input_candidate_fields s1
  | ManySub s_array -> Array.iter (annotate_compilation be input_tuple input_candidate_fields) s_array

(* cur is a cexpr_desc *)
and annotate_compilation bshould_be_empty input_tuple_fields input_candidate_fields cur =
  let indep = cur.psub_expression in
  annotate_subexpr bshould_be_empty input_tuple_fields input_candidate_fields indep;
  if (bshould_be_empty && (not (cur.compile_annotations = None)))
  then
    raise (Query (Internal_Error ("Compile_annotation error: Algop '"^(Xquery_algebra_ast_util.string_of_algop_expr_name cur.palgop_expr_name)^"' is already annotated")));
  begin
    let fvd = annotate_op_and_dep bshould_be_empty input_tuple_fields input_candidate_fields cur in
    cur.compile_annotations <- (Some fvd)
  end

(* Called externally at the head of the tree *)
let annotate_algebraic_expression root =
  annotate_compilation true empty_fields empty_fields root

(* Called to reannotate the algebraic tree *)
let reannotate_algebraic_expression root = 
  annotate_compilation false empty_fields empty_fields root

(****************************************************)
(* 
   These are all done by side-effects. This should 
   probably be switched since it is not as nice.

   These are not for reannotation - they insist
   that the annotations be blank.

*)
(****************************************************)
let annotate_algebraic_function fn =
  let (cfname, arity), _, fn_body, _ = fn.palgop_function_decl_desc in
print_string ("annotate function "^(Namespace_names.prefixed_string_of_rqname cfname)^"\n");
  match !(fn_body.palgop_func_optimized_logical_plan) with
  | AOEFunctionImported -> () 
  | AOEFunctionUser userfn_body -> annotate_algebraic_expression userfn_body

let annotate_algebraic_decl dcl = 
  annotate_subexpr true empty_fields empty_fields dcl.alg_decl_indep;
  annotate_subexpr true empty_fields empty_fields dcl.alg_decl_dep
 
let annotate_algebraic_prolog p =
  List.iter annotate_algebraic_function p.palgop_prolog_functions;
  List.iter annotate_algebraic_decl p.palgop_prolog_vars;
  List.iter annotate_algebraic_decl p.palgop_prolog_indices
  
let annotate_algebraic_module m = 
  annotate_algebraic_prolog m.palgop_module_prolog;
  List.iter annotate_algebraic_expression 
    (m.palgop_module_statements) 

