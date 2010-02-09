(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2009.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Module: Galax_main
   Description:
     Main for the galax executable.
 *)


(*************)
(* Let's go! *)
(*************)

open Top_args
open Top_util

let rec dispatch_go ek gargs =
  match ek with
  | MapHelp ->
      (* Makes sure it maps glx-map help <subcommand> back to glx-map <subcommand> -help *)
      begin
	match Top_args.map_help_go gargs with
	| None -> ()
	| Some (rek,rgargs) ->
	    dispatch_go rek rgargs 
      end
  | MapXQuery2XML ->
      Top_xquery2xmlplan.go gargs
  | MapXQueryX2XQuery ->
      Top_xqueryx2xquery.go gargs
  | MapXQuery2Plan ->
      Top_xquery2plan.go gargs
  | MapXQuery2SOAP ->
      Top_xquery2soap.go gargs
  | MapXMLPlan2Plan ->
      Top_xmlplan2plan.go gargs
  | MapWSDL2XQuery ->
      Top_wsdl2xquery.go gargs

let go() =
  (* 1. Pre-process the arguments *)
  let ek,gargs = dispatch_map_args () in

  (* 2. Call the proper dispatcher *)
  dispatch_go ek gargs

let _ =
  low_exec go ()

