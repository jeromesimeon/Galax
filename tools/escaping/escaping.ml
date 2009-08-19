(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: escaping.ml,v 1.2 2007/02/01 22:08:54 simeon Exp $ *)

let output_all = ref false

let usage_msg = 
  Format.sprintf "Usage: %s file" Sys.argv.(0)

let input_file = ref ""
let output_file = ref ""

let process_args () =
  let args = ref [] in
  Arg.parse
    []
    (fun arg -> args := arg :: !args) usage_msg;
  match !args with
  | [] ->
      failwith "Input file not specified"
  | fname :: [] ->
      begin
	input_file := fname;
	output_file := (Filename.chop_extension fname) ^ ".ml"
      end
  | _ ->
      failwith "Too many imput files"

(* Dump the content of a file into a string *)

let load_file f =
  let s = ref "" in
  let ic = open_in f in
  try
    while true do
      s := !s ^ (input_line ic) ^ "\n"
    done;
    !s
  with
  | End_of_file ->
      begin
	close_in ic;
	!s
      end

let intro_text =
  "(***********************************************************************)\n(*                                                                     *)\n(*                                 GALAX                               *)\n(*                             XQuery Engine                           *)\n(*                                                                     *)\n(*  Copyright 2001-2007.                                               *)\n(*  Distributed only by permission.                                    *)\n(*                                                                     *)\n(***********************************************************************)\n\n(* $Id: escaping.ml,v 1.2 2007/02/01 22:08:54 simeon Exp $ *)\n\nlet pervasive = \"\n"

let closing_text =
  "\"\n"

let main () =
  process_args();
  let pervasive_text = load_file !input_file in
  let escaped_pervasive = String.escaped pervasive_text in
  let full_pervasive_ml_text =
    intro_text ^ escaped_pervasive ^ closing_text
  in
  let oc = open_out !output_file in
  begin
    output_string oc full_pervasive_ml_text;
    close_out oc
  end

let _ = main()


