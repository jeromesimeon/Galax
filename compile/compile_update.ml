(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: compile_update.ml,v 1.10 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Compile_update
   Description:
     This module compiles an XQuery core update into the XQuery
     algebra.
*)

open Xquery_core_ast
open Xquery_core_ast_util

open Xquery_algebra_ast
open Xquery_algebra_ast_util

open Compile_context
open Compile_expr
open Compile_util

open Namespace_builtin

(* Clean this out *)
let get_fresh_var_name comp_ctxt = 
  let v = Compile_context.next_var comp_ctxt in
    (Namespace_builtin.glx_prefix, Namespace_builtin.glx_uri, ("comp" ^ (string_of_int v)))

(***********)
(* Updates *)
(***********)

let compile_cinsert_location compile_ctxt cinsert_location =
  match cinsert_location with
  | CUAsLastInto cexpr1 ->
      let ao1 = compile_cexpr compile_ctxt cexpr1 in
      (ao1,AOUAsLastInto)
  | CUAsFirstInto cexpr1 ->
      let ao1 = compile_cexpr compile_ctxt cexpr1 in
      (ao1,AOUAsFirstInto)
  | CUInto cexpr1 ->
      let ao1 = compile_cexpr compile_ctxt cexpr1 in
      (ao1,AOUInto)
  | CUAfter cexpr1 ->
      let ao1 = compile_cexpr compile_ctxt cexpr1 in 
      (ao1,AOUAfter)
  | CUBefore cexpr1 ->
      let ao1 = compile_cexpr compile_ctxt cexpr1 in
      (ao1,AOUBefore)

let compile_csimple_update_desc compile_ctxt csimple_update_desc eh fi =
  match csimple_update_desc with
  | CUInsert (cexpr1,cinsert_location) ->
      let ao1 = compile_cexpr compile_ctxt cexpr1 in
      let (ao2,insert_flag) = compile_cinsert_location compile_ctxt cinsert_location in
      let indep = TwoSub (ao1,ao2) in
      let dep = NoSub in
      let op_name = AOEInsert insert_flag in
      annotate_algop op_name indep dep eh fi
  | CUReplace (value_of_flag,cexpr1,cexpr2) ->
      let ao1 = compile_cexpr compile_ctxt cexpr1 in
      let ao2 = compile_cexpr compile_ctxt cexpr2 in
      let indep = TwoSub(ao1,ao2) in
      let dep = NoSub in
      let op_name = AOEReplace value_of_flag in
      annotate_algop op_name indep dep eh fi
  | CUDelete cexpr1 ->
      let ao1 = compile_cexpr compile_ctxt cexpr1 in
      let indep = OneSub ao1 in
      let dep = NoSub in
      let op_name = AOEDelete in
      annotate_algop op_name indep dep eh fi
  | CUEmpty ->
      let indep = NoSub in
      let dep = NoSub in
      let op_name = AOEEmpty in
      annotate_algop op_name indep dep eh fi    

let compile_csimple_update compile_ctxt csimple_update eh fi =
  let csimple_update_desc = csimple_update.pcsimple_update_desc in
  let fi                  = csimple_update.pcsimple_update_loc in
  compile_csimple_update_desc compile_ctxt csimple_update_desc eh fi

(************************************)
(* Compilation of flwor expressions *)
(************************************)

(* HACK FOR NOW: WE DUPLICATE CODE FROM compile_expr
    this should be removed and factorized Avinash & Jerome *)

type 'a compile_select = 
  | Cs_Disjunction     of 'a compile_select * 'a compile_select
  | Cs_ComplexConjunct of 'a compile_select * 'a compile_select
  | Cs_SimpleConjunct  of ('a option, unit) aalgop_expr list
let is_fn_true op =
  match op.pcexpr_desc with
    | CECall (x, [], _, _, _) when x = fn_true ->
	true
    | _ -> false

let is_fn_false op =  
  match op.pcexpr_desc with
    | CECall (x, [], _, _, _) when x = fn_false ->
	true
    | _ -> false

let is_fn_boolean op = 
  match op.pcexpr_desc with
    | CECall (x, _, _, _, _) when x = fn_boolean -> 
	true
    | _ -> false



let rec compile_csimple_update_list compile_ctxt op3 csimple_update_list eh fi =
  (* [[ return Expr1 ]](Op3)
     ==
     Map_(i -> [[ Expr1 ]])(Op3) *)
  let oplist =
    List.map (fun x -> compile_csimple_update compile_ctxt x eh fi)
      csimple_update_list
  in
  let oparray = Array.of_list oplist in
  let apply_updates_op =
    annotate_algop AOESequencing (ManySub oparray) NoSub eh fi
  in
  let table_op = 
    annotate_algop AOEMaterializeTable (OneSub op3) NoSub eh fi
  in
  let map =
    annotate_algop AOEMapToItem (OneSub table_op) (OneSub apply_updates_op) eh fi
  in
  map

and compile_order_by_clause_update compile_ctxt op3
  (order_by_clause,csimple_update_list) eh fi =
  match order_by_clause with
    | None ->
	compile_csimple_update_list compile_ctxt op3 csimple_update_list eh fi
    | Some (stablekind,corder_spec_list) ->
	begin
	  let rec split_fun corder_spec_list =
	    match corder_spec_list with
	      | [] -> ([],[])
	      | (cexpr,sortkind,emptysortkind) :: corder_spec_list' ->
		  let (cexpr_list,rest_list) = split_fun corder_spec_list' in
		    (cexpr :: cexpr_list, (sortkind,emptysortkind) :: rest_list)
	  in
	  let splited = split_fun corder_spec_list in
	  let op_name = AOEOrderBy (stablekind,snd splited) in
	  let op1_list = List.map (compile_cexpr compile_ctxt) (fst splited) in
	  let op1_array = Array.of_list op1_list in
	  let new_op3 = aalgop_mkop op_name (OneSub op3) (ManySub op1_array) None eh fi in
	    compile_csimple_update_list compile_ctxt new_op3 csimple_update_list eh fi
	end

and compile_where_clause_update compile_ctxt op3
  (where_clause,order_by_clause,csimple_update_list) eh fi =
  match where_clause with
    | None ->
	compile_order_by_clause_update compile_ctxt op3
	  (order_by_clause,csimple_update_list) eh fi
    | Some where_expr ->
	let select_predicate =
	  compile_cexpr compile_ctxt where_expr
	in
	let select =
	  compile_select compile_ctxt where_expr op3 eh fi
	in
	  compile_order_by_clause_update compile_ctxt select
	    (order_by_clause,csimple_update_list) eh fi

and compile_fl_clauses_update compile_ctxt op3
  fl_clauses (where_clause,order_by_clause,csimple_update_list) eh fi =
  match fl_clauses with
    | [] ->
	(* Calls compilation for the rest of the FLWOR block *)
	begin
	  compile_where_clause_update compile_ctxt op3
	    (where_clause,order_by_clause,csimple_update_list) eh fi
	end
    | (CEFOR (odt,vname,None,cexpr1)) :: fl_clauses' ->
	(* [[ for $v in Expr1 return Expr2 ]](Op3)
           ==
           [ [[ Expr2 ]](Map_(i -> Map_(j -> [ v : j ] o i)([[ Expr1 ]]))(Op3))
           / $v --> #v ] 
	   
	   Changed to:

           [ [[Expr2]] 
           (MapConcat( i -> MapItemTuple{[v : j]}([[Expr1]]) o i)(Op3)
           / $v --> #v ]
	*)
	(* 1. Compile Expr1 *)      
	begin
	  let op1 = compile_cexpr compile_ctxt cexpr1 in	  
	    (* 2. Build the new Op3 *)
	  let fresh_name = get_fresh_var_name compile_ctxt in
	  let input_j = aalgop_mkop (AOEVar fresh_name) NoSub NoSub None eh fi in

	  let compile_ctxt'  = add_variable_field_to_compile_context compile_ctxt vname   in 
	  let tname          = get_tuple_field_name compile_ctxt' vname  in

	  let tuple_construct =
	    aalgop_mkop (AOECreateTuple [|tname|]) (ManySub [|input_j|]) NoSub None eh fi
	  in	     
	  let map_inner = 
	    aalgop_mkop (AOEMapFromItem fresh_name) (OneSub op1) (OneSub tuple_construct) None eh fi
	  in
	  let map_outer =
	    aalgop_mkop AOEMapConcat (OneSub op3) (OneSub map_inner) None eh fi
	  in
      	    
	    (* 3. Apply compilation to the rest of the FLWOR block, passing the new Op3 *)
	    compile_fl_clauses_update compile_ctxt' map_outer
	      fl_clauses' (where_clause,order_by_clause,csimple_update_list) eh fi
	end

    | (CEFOR (odt,vname,Some vname',cexpr1)) :: fl_clauses' ->
   (* [[ for $v at $i in Expr1 return Expr2 ]](Op3)
      ==
      [ [[ Expr2 ]](Map_(i -> MapIndex(i)(Map_(j -> [ v : j ] o i)([[ Expr1 ]])))(Op3))
      / $v --> #v ] 

      ChangedTo:
      
      [ [[ Expr2 ]](MapConcat( t -> MapIndex(i)(MapItemTuple{[v: j]} ([[ Expr1 ]]))) (Op3))
      / $v --> #v ] 
   *)
   (* 1. Compile Expr1 *)
   begin
     let op1 = compile_cexpr compile_ctxt cexpr1 in
       (* 2. Build the new Op3 *)
     let fresh_name =  get_fresh_var_name compile_ctxt in
     let input_j = aalgop_mkop (AOEVar fresh_name) NoSub NoSub None eh fi in

     (***************************)
     (* Compile the tuple names *)
     (***************************)
     let compile_ctxt'  = add_variable_field_to_compile_context compile_ctxt vname   in 
     let compile_ctxt'' = add_variable_field_to_compile_context compile_ctxt' vname' in

     let tname         = get_tuple_field_name compile_ctxt'' vname  in
     let tname'        = get_tuple_field_name compile_ctxt'' vname' in

     let tuple_construct =
       aalgop_mkop (AOECreateTuple [|tname|]) (ManySub [|input_j|]) NoSub None eh fi
     in

     let input_i = annotate_inputtuple eh fi in
     let tuple_append =
       aalgop_mkop AOEConcatTuples (TwoSub (tuple_construct,input_i)) NoSub None eh fi
     in
     let map_inner =
       aalgop_mkop (AOEMapFromItem fresh_name) (OneSub op1) (OneSub tuple_append) None eh fi
     in
     let map_index =
       aalgop_mkop (AOEMapIndex(tname')) (OneSub map_inner) NoSub None eh fi
     in

     let map_outer = 
       aalgop_mkop AOEMapConcat (OneSub op3) (OneSub map_index) None eh fi
     in

       (* 3. Apply compilation to the rest of the FLWOR block, passing the new Op3 *)
       
       compile_fl_clauses_update compile_ctxt'' map_outer
	 fl_clauses' (where_clause,order_by_clause,csimple_update_list) eh fi
   end

| (CELET (odt,vname,cexpr1)) :: fl_clauses' ->
    (* [[ let $v in Expr1 return Expr2 ]](Op3)
       ==
       [ [[ Expr2 ]](Map_(i -> [ v : [[ Expr1 ]] ] o i)(Op3)) / $v --> #v ] 
       Changed To:
       [ [[ Expr2 ]](MapConcat(t -> [ v : [[ Expr1 ]] ]))(Op3) / $v --> #v
    *)
    (* 1. Compile Expr1 *)
    begin
      let op1 = compile_cexpr compile_ctxt cexpr1 in
      let compile_ctxt' = add_variable_field_to_compile_context compile_ctxt vname in 
      let tname         = get_tuple_field_name compile_ctxt' vname in

      (* 2. Build the new Op3 *)
      let tuple_construct =
	aalgop_mkop (AOECreateTuple [|tname|]) (ManySub [|op1|]) NoSub None eh fi
      in	  
      let map = aalgop_mkop AOEMapConcat (OneSub op3) (OneSub tuple_construct) None eh fi in
	
	compile_fl_clauses_update compile_ctxt' map
	  fl_clauses' (where_clause,order_by_clause,csimple_update_list) eh fi
    end


(*

let compile_order_by_clause_update compile_ctxt op3
    (order_by_clause,csimple_update_list) eh fi =
  match order_by_clause with
  | None ->
      compile_csimple_update_list compile_ctxt op3 csimple_update_list eh fi
  | Some (stablekind,corder_spec_list) ->
      begin
	let rec split_fun corder_spec_list =
	  match corder_spec_list with
	  | [] -> ([],[])
	  | (cexpr,sortkind,emptysortkind) :: corder_spec_list' ->
	      let (cexpr_list,rest_list) = split_fun corder_spec_list' in
	      (cexpr :: cexpr_list, (sortkind,emptysortkind) :: rest_list)
	in
	let splited = split_fun corder_spec_list in
	let op_name = AOEOrderBy (stablekind,snd splited) in
	let op1_list = List.map (compile_cexpr compile_ctxt) (fst splited) in
	let op1_array = Array.of_list op1_list in
	let new_op3 = annotate_algop op_name (OneSub op3) (ManySub op1_array) eh fi in
	compile_csimple_update_list compile_ctxt new_op3 csimple_update_list eh fi
      end

let compile_where_clause_update compile_ctxt op3
    (where_clause,order_by_clause,csimple_update_list) eh fi =
  match where_clause with
  | None ->
      compile_order_by_clause_update compile_ctxt op3
	(order_by_clause,csimple_update_list) eh fi
  | Some where_expr ->
      raise (Error.Query (Error.Internal_Error ("Selects not yet compiled for updates")))
(*      let select_predicate =
	compile_cexpr compile_ctxt where_expr
      in
      let select =
	annotate_algop AOESelect (OneSub op3) (OneSub select_predicate) eh fi
      in
      compile_order_by_clause_update compile_ctxt select
	(order_by_clause,csimple_update_list) eh fi
*)
let rec compile_fl_clauses_update compile_ctxt op3
  fl_clauses (where_clause,order_by_clause,csimple_update_list) eh fi =
  match fl_clauses with
    | [] ->
	(* Calls compilation for the rest of the FLWOR block *)
	begin
	  compile_where_clause_update compile_ctxt op3
	    (where_clause,order_by_clause,csimple_update_list) eh fi
	end
    | (CEFOR (odt,vname,None,cexpr1)) :: fl_clauses' ->
	(* [[ for $v in Expr1 return Expr2 ]](Op3)
           ==
           [ [[ Expr2 ]](Map_(i -> Map_(j -> [ v : j ] o i)([[ Expr1 ]]))(Op3))
           / $v --> #v ] *) 
	
	(* 1. Compile Expr1 *)
	begin
	  let op1 = compile_cexpr compile_ctxt cexpr1 in
	    (* 2. Build the new Op3 *)
	  let fresh_var_name =  get_fresh_var_name compile_ctxt in
	  let input_j = annotate_algop (AOEVar fresh_var_name) NoSub NoSub eh fi in

	  let tuple_construct =
	    annotate_algop (AOECreateTuple [|vname|]) (ManySub [|input_j|]) NoSub eh fi
	  in
	  let input_i = annotate_algop AOEInputTuple NoSub NoSub eh fi in
	  let tuple_append =
	    annotate_algop AOEConcatTuples (TwoSub (tuple_construct,input_i)) NoSub eh fi
	  in
	  let map_inner =
	    annotate_algop (AOEMapFromItem fresh_var_name) (OneSub op1) (OneSub tuple_append) eh fi
	  in
	  let map_outer =
	    annotate_algop AOEMap (OneSub op3) (OneSub map_inner) eh fi
	  in
	    (* 3. Apply compilation to the rest of the FLWOR block, passing the new Op3 *)
	  let compile_ctxt' = add_variable_field_to_compile_context compile_ctxt vname in 

	    compile_fl_clauses_update compile_ctxt' map_outer
	      fl_clauses' (where_clause,order_by_clause,csimple_update_list) eh fi
	end

    | (CEFOR (odt,vname,Some vname',cexpr1)) :: fl_clauses' ->
	(* [[ for $v at $i in Expr1 return Expr2 ]](Op3)
           ==
           [ [[ Expr2 ]](Map_(i -> MapIndex(i)(Map_(j -> [ v : j ] o i)([[ Expr1 ]])))(Op3))
           / $v --> #v ] *) 
	
	(* 1. Compile Expr1 *)
	begin
	  let op1 = compile_cexpr compile_ctxt cexpr1 in
	    (* 2. Build the new Op3 *)
	  let fresh_var_name = get_fresh_var_name compile_ctxt in
	  let input_j = annotate_algop (AOEVar fresh_var_name) NoSub NoSub eh fi in
	  let tuple_construct =
	    annotate_algop (AOECreateTuple [|vname|]) (ManySub [|input_j|]) NoSub eh fi
	  in
	  let input_i = annotate_algop AOEInputTuple NoSub NoSub eh fi in
	  let tuple_append =
	    annotate_algop AOEConcatTuples (TwoSub (tuple_construct,input_i)) NoSub eh fi
	  in
	  let map_inner =
	    annotate_algop (AOEMapFromItem fresh_var_name) (OneSub op1) (OneSub tuple_append) eh fi
	  in
	  let rname' = vname' in (* For now, the tuple field name is the variable name *)
	  let map_index =
	    annotate_algop (AOEMapIndex(rname')) (OneSub map_inner) NoSub eh fi
	  in
	  let map_outer =
	    annotate_algop AOEMap (OneSub op3) (OneSub map_index) eh fi
	  in
	    (* 3. Apply compilation to the rest of the FLWOR block, passing the new Op3 *)

	  let compile_ctxt'  = add_variable_field_to_compile_context compile_ctxt vname   in 
	  let compile_ctxt'' = add_variable_field_to_compile_context compile_ctxt' vname' in
	    
	    compile_fl_clauses_update compile_ctxt'' map_outer
	      fl_clauses' (where_clause,order_by_clause,csimple_update_list) eh fi
	end

    | (CELET (odt,vname,cexpr1)) :: fl_clauses' ->
	(* [[ let $v in Expr1 return Expr2 ]](Op3)
           ==
           [ [[ Expr2 ]](Map_(i -> [ v : [[ Expr1 ]] ] o i)(Op3)) / $v --> #v ] *) 

	(* 1. Compile Expr1 *)
	begin
	  let op1 = compile_cexpr compile_ctxt cexpr1 in
	  let compile_ctxt' = add_variable_field_to_compile_context compile_ctxt vname in 
	  let tname         = get_tuple_field_name compile_ctxt' vname in

          (* 2. Build the new Op3 *)
	  let tuple_construct =
	    annotate_algop (AOECreateTuple [|tname|]) (ManySub [|op1|]) NoSub eh fi
	  in
	  let input_i = annotate_algop AOEInputTuple NoSub NoSub eh fi in
	  let tuple_append =
	    annotate_algop AOEConcatTuples (TwoSub (tuple_construct,input_i)) NoSub eh fi
	  in
	  let map =
	    annotate_algop AOEMap (OneSub op3) (OneSub tuple_append) eh fi
	  in
	    (* 3. Apply compilation to the rest of the FLWOR block, passing the new Op3 *)
	  let compile_ctxt' = add_variable_field_to_compile_context compile_ctxt vname in
	    
	    compile_fl_clauses_update compile_ctxt' map
	      fl_clauses' (where_clause,order_by_clause,csimple_update_list) eh fi
	end
*)


and compile_flwor_update compile_ctxt fl_clauses
  (where_clause,order_by_clause,csimple_update_list) eh fi =
  (* The initial input is an empty table *)
  let init_op3 =
    if has_input_set compile_ctxt
    then
      annotate_algop AOEInputTuple NoSub NoSub eh fi
    else
      annotate_algop (AOECreateTuple [||]) (ManySub [||]) NoSub eh fi
  in
    compile_fl_clauses_update compile_ctxt init_op3
      fl_clauses (where_clause,order_by_clause,csimple_update_list) eh fi

 and compile_select compile_ctxt select_clause op3 eh fi =
   let rec compile_select_helper cur_expr =
     match cur_expr.pcexpr_desc with
       | CEIf(cexpr1, cexpr2, cexpr3) ->
	   (***************************************)
	   (* Restricted form                     *)
	   (* Or  -> if (e1) then true else (e2)  *)
	   (* And -> if (e1) then (e2) else false *)
	   (***************************************)
	   let ae1 = compile_select_helper cexpr1 in
	     if (is_fn_true cexpr2) then
	       begin
		 Cs_Disjunction( ae1, (compile_select_helper cexpr3))	  
	       end
	     else if (is_fn_false cexpr3) then
	       begin
		 let rhs = compile_select_helper cexpr2 in
		   match (ae1, rhs) with
		     | (Cs_SimpleConjunct( c1 ), Cs_SimpleConjunct (c2)) -> 
			 Cs_SimpleConjunct ( c1 @ c2 ) 
		     | _ -> 
			 Cs_ComplexConjunct( ae1, rhs )
	       end
	     else 
	       begin
	       (* An introduced if statement... can not compile it *)
		 let ae1 = compile_cexpr compile_ctxt cur_expr in
		   Cs_SimpleConjunct ( ae1 :: [] )
 	       end
       | CECall(_,[cexpr],_, _, _) when (is_fn_boolean cur_expr) ->
	   compile_select_helper cexpr
       | _ -> 	   
	   let ae1 = compile_cexpr compile_ctxt cur_expr in
	     Cs_SimpleConjunct ( ae1 :: [] )
   in
   let rec compile_conjuncts conjunct index = 
     match conjunct with
       | Cs_SimpleConjunct ( op_list ) ->
	   let length = List.length op_list in 
	   let o_list = Array.of_list op_list in
	     SimpleConjunct( index, (index + length -1)), o_list, (index + length)

       | Cs_ComplexConjunct(c1, c2) ->
	   let op1, op_l1, index = compile_conjuncts c1 index in
	   let op2, op_l2, index = compile_conjuncts c1 index in	     
	   let ops = Array.append op_l1 op_l2 in
	     ComplexConjunct(op1,op2), ops, index

       | Cs_Disjunction (d1,d2) ->
	   let c1, op1, cur_index = compile_conjuncts d1 index in
	   let c2, op2, cur_index = compile_conjuncts d2 cur_index in
	   let ops = Array.append op1 op2 in
	   Disjunct( c1,c2 ), ops, cur_index
   in
   let internal_conjunct_desc = compile_select_helper select_clause in
   let conjunct_desc,ops,_    = compile_conjuncts internal_conjunct_desc 0 in
     aalgop_mkop (AOESelect conjunct_desc) (OneSub op3) (ManySub ops) None eh fi
   
let compile_update_desc compile_ctxt ccomplex_update_desc eh fi =
  match ccomplex_update_desc with
  | CUCond (cexpr1,csimple_update1,csimple_update2) ->
      (* Conditional updates compiled into an IF-THEN-ELSE *)
      let ao1 = compile_cexpr compile_ctxt cexpr1 in
      let ao2 = compile_csimple_update compile_ctxt csimple_update1 eh fi in
      let ao3 = compile_csimple_update compile_ctxt csimple_update2 eh fi in
      let indep = OneSub ao1 in
      let dep = TwoSub (ao2,ao3) in
      let op_name = AOEIf in
      annotate_algop op_name indep dep eh fi
  | CUFLWOR (fl_clauses,where_clause,order_by_clause,csimple_update_list) ->
      (* FLWOR update compiled as FLWOR expressions, except for the
	 simple updates part. *)
      compile_flwor_update compile_ctxt fl_clauses
	(where_clause,order_by_clause,csimple_update_list) eh fi

let compile_cupdate compile_ctxt cupdate =
  let cupdate_desc = cupdate.pcupdate_desc in
  let cupdate_orig = cupdate.pcupdate_origin in
  let fi           = cupdate.pcupdate_loc in
  compile_update_desc compile_ctxt cupdate_desc None fi

