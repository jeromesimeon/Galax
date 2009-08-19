(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: parse_xqueryx.ml,v 1.3 2007/08/09 20:21:22 simeon Exp $ *)

(* Module: Norm_xqueryx
   Description:
     This module implements normalization for XQueryX expressions
     using the trivial embedding.
*)

open Xquery_ast

open Error

let check_attributes attlist fi =
  begin
    match attlist with
    | [] -> ()
    | att_e :: _ ->
	begin
	  match att_e.pexpr_desc with
	  | EAttrFixed (s, _) ->
	      begin
		let n = Namespace_names.string_of_uqname s in
		raise (Query (XQueryX_Error (fi,("Found attribute '"^n^"' in XQueryX element"))))
	      end
	  | _ ->
	      raise (Query (Malformed_Expr("Invalid attribute in element")))
	end
  end

let rec grow_text childlist fi =
  match childlist with
  | [] -> ""
  | e1 :: childlist' ->
      begin
	match e1.pexpr_desc with
	| EText s ->
	    s ^ (grow_text childlist' fi)
	| ECharRef i ->
	    (Galax_camomile.utf8_string_of_code_point i) ^ (grow_text childlist' fi)
	| _ ->
	    raise (Query (Malformed_Expr("Invalid content in XQueryX element")))
      end

let normalize_xqx_xquery_expr attlist childlist fi =
  begin
    (* Check that there is no attribute *)
    check_attributes attlist fi;
    let text = grow_text childlist fi in
    (* Printf.printf "FOUND XQUERY TEXT \"%s\"" text; flush stdout; *)
    Parse_top.parse_statement_from_string text
  end

let normalize_xqx_xquery_main_module_aux attlist childlist fi =
  begin
    (* Check that there is no attribute *)
    check_attributes attlist fi;
    let text = grow_text childlist fi in
    (* Printf.printf "FOUND XQUERY TEXT \"%s\"" text; flush stdout; *)
    try
      Parse_top.parse_main_module_from_string text
    with
    | _ ->
	raise (Query (Malformed_Expr("Invalid XQuery Content Inside XQueryX")))
  end

let normalize_xqx_xquery_library_module_aux attlist childlist fi =
  begin
    (* Check that there is no attribute *)
    check_attributes attlist fi;
    let text = grow_text childlist fi in
    (* Printf.printf "FOUND XQUERY TEXT \"%s\"" text; flush stdout; *)
(*    try *)
      Parse_top.parse_library_module_from_string text
(*    with
    | _ ->
	raise (Query (Malformed_Expr("Invalid XQuery Module Content Inside XQueryX"))) *)
  end

let normalize_xqx_xquery_interface_module_aux attlist childlist fi =
  begin
    (* Check that there is no attribute *)
    check_attributes attlist fi;
    let text = grow_text childlist fi in
    (* Printf.printf "FOUND XQUERY TEXT \"%s\"" text; flush stdout; *)
    try
      Parse_top.parse_interface_from_string text
    with
    | _ ->
	raise (Query (Malformed_Expr("Invalid XQuery Interface Content Inside XQueryX")))
  end

let normalize_xqx_xquery_main_module attlist childlist fi =
  snd(normalize_xqx_xquery_main_module_aux attlist childlist fi)

let normalize_xqx_xquery_library_module attlist childlist fi =
  snd(normalize_xqx_xquery_library_module_aux attlist childlist fi)

let normalize_xqx_xquery_interface_module attlist childlist fi =
  snd(normalize_xqx_xquery_interface_module_aux attlist childlist fi)

let unwrap_module ast =
  try
    begin
      match ast.pmain_module_statements with
      | [] -> raise Not_found
      | e::_ ->
	  begin
	    match e.pexpr_desc with
	    | EList (e::_) ->
		begin
		  match e.pexpr_desc with
		  | EElemFixed (ename, attlist, childlist) ->
		      let fi = e.pexpr_loc in 
		      let (new_nss, other_atts) = Xquery_ast_util.get_ns_attributes attlist in
		      let nsenv = Namespace_context.add_all_ns Namespace_context.default_xquery_nsenv new_nss in
		      let cename = Namespace_resolve.resolve_element_qname nsenv ename in
		      if (Namespace_names.rqname_equal Namespace_builtin.xqx_xquery cename)
		      then
			(other_atts,childlist,fi)
		      else
			raise Not_found
		  | _ -> raise Not_found
		end
	    | _ -> raise Not_found
	  end
    end
  with
  | _ ->
      raise (Query (Malformed_Expr("Invalid content in XQueryX file")))

let parse_xqx_xquery_main_module_from_io pac io =
  (* Printf.printf "Calling XQueryX parser\n"; flush stdout; *)
  let ast =
    try
      snd (Parse_top.parse_main_module_from_io pac io)
    with
    | _ ->
	raise (Query (Malformed_Expr("Invalid XQueryX Markup")))
  in
  let attlist,childlist,fi =
    unwrap_module ast
  in
  normalize_xqx_xquery_main_module_aux attlist childlist fi

let parse_xqx_xquery_library_module_from_io pac io =
  (* Printf.printf "Calling XQueryX parser\n"; flush stdout; *)
  let ast =
    try
      snd (Parse_top.parse_main_module_from_io pac io)
    with
    | _ ->
	raise (Query (Malformed_Expr("Invalid XQueryX Markup")))
  in
  let attlist,childlist,fi =
    unwrap_module ast
  in
  normalize_xqx_xquery_library_module_aux attlist childlist fi

let parse_xqx_xquery_interface_module_from_io pac io =
  (* Printf.printf "Calling XQueryX parser\n"; flush stdout; *)
  let ast =
    try
      snd (Parse_top.parse_main_module_from_io pac io)
    with
    | _ ->
	raise (Query (Malformed_Expr("Invalid XQueryX Markup")))
  in
  let attlist,childlist,fi =
    unwrap_module ast
  in
  normalize_xqx_xquery_interface_module_aux attlist childlist fi

