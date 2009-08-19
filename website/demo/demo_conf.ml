(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: demo_conf.ml,v 1.8 2008/03/12 22:30:58 simeon Exp $ *)

let rec pair_up_items il = 
  match il with
  | [] -> []
  | first :: second :: rest ->
      (first,second) :: (pair_up_items rest)
  | _ -> 
      failwith "Expecting an even number of items"

(* config parameters *)

let document_dir = ref ""
let cgibin_prefix = ref ""

(* process the definition of each config parameters, one clause for each *)

let process_definition defn =
  match defn with
  | ("DOCUMENT_DIR", dir) -> document_dir := dir
  | ("CGIBIN_PREFIX", prefix) -> cgibin_prefix := prefix
  | _ -> failwith "Undeclared parameter defined" (* better ignore quietly? *)

let demo_init config_file = 
  let config_content = Gmisc.load_file config_file in
  let alist = pair_up_items (Str.split (Str.regexp "=\\|\n") config_content) in
  let _ = List.map process_definition alist in ()

