(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: unix2dos.ml,v 1.3 2007/02/01 22:08:55 simeon Exp $ *)

let output_all = ref false

let usage_msg = 
  Format.sprintf "Usage: %s file" Sys.argv.(0)

let process_args () =
  let args = ref [] in
  Arg.parse
    []
    (fun arg -> args := arg :: !args) usage_msg;
  !args

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

let convert_file fname =
  let input = load_file fname in
  let oc = open_out_bin fname in  (* load *)
  begin
    output_string oc input;          (* print back -- should put the right end of lines *)
    close_out oc
  end

let main () =
  let fnames = process_args() in
  List.iter convert_file fnames

let _ = main()


