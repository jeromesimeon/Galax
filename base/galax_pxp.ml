(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: galax_pxp.ml,v 1.3 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Galax_pxp
   Description:
     This module contains useful hooks to the PXP parser.
*)


open Error

open Pxp_ev_parser
open Pxp_types


(*********************)
(* PXP configuration *)
(*********************)

(* Internal stuff used to set-up Pxp *)

let glx_default_config =
  let _ = new drop_warnings in
  { default_config with
    encoding = Encoding.get_internal_encoding ();

(* We want PI's and comments enabled, don't we? *)

    enable_comment_nodes = true;
    enable_pinstr_nodes = true;
    enable_super_root_node = true;

(* If we store element positions, then positions are stored for
   elements and wrapped processing instructions.  See Pxp_types.mli *)

    store_element_positions = true }

(* default entry *)

let glx_default_entry =
  `Entry_document[`Extend_dtd_fully]

let empty_dtd () =
  let _ = glx_default_config in
  Pxp_dtd_parser.create_empty_dtd glx_default_config
  
let default_dtd      = empty_dtd ()
let default_lfactory = default_dtd # lexer_factory
let default_lexobj   = default_lfactory # open_string ""
 
