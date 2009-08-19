(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: namespace_generate.ml,v 1.1 2007/02/12 21:15:33 simeon Exp $ *)

(* Module: Namespace_generate
   Description:
     This module contains support for QName creation.
*)

(* Note:
     The compiler makes intensive use of this, during normalization,
     algebraic compilation, andoptimization.
     This module consolidate bits of pieces of code scattered accross
     the compiler, and making sure the result of compilation is stable.
  - Jerome
*)

open Namespace_names

type name_gen =
    { namespace_uri    : uri;
      namespace_prefix : prefix;
      local_prefix     : string;
      mutable counter  : int }

let create_name_generator prefix uri locprefix =
  { namespace_prefix = prefix;
    namespace_uri    = uri;
    local_prefix     = locprefix;
    counter          = 0 }

let reset_name_generator ng =
  ng.counter <- 0

let generate_name ng =
  begin
    ng.counter <- ng.counter+1;
    let localname = ng.local_prefix ^ "_" ^ (string_of_int ng.counter) in
    (ng.namespace_prefix,ng.namespace_uri,localname)
  end

let generate_name_with_prefix ng local_prefix =
  begin
    ng.counter <- ng.counter+1;
    let localname = local_prefix ^ "_" ^ (string_of_int ng.counter) in
    (ng.namespace_prefix,ng.namespace_uri,localname)
  end

