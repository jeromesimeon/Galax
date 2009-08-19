(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: shredded-jungle-load.ml,v 1.2 2007/02/01 22:08:54 simeon Exp $ *)

open Format
open Error

(* Eventually even this could become a functor.... *)
module Shredded_Store = Shredded_jungle_store.Jungle_Store_Module
  

exception Shredded_Load_Error of string

let shredded_version = "0.1"

let store_dir_name = ref "." 


let store_name = ref "Shredded"

let buff_size = ref 262144
let typed_load = ref false

let print_version() = 
	begin
		printf "\n";
		printf " This is Shredded load utility, version %s\n" shredded_version;
	end 

let usage_msg = 
	sprintf "Usage: %s [-store_dir path] [-store_name string] [-buff_size sizeinbytes] input-xml-file" Sys.argv.(0)

let check_store_dir() = 
  if (Sys.file_exists !store_dir_name) 
  then
    begin
      printf " Store directory %s found ....... Ok \n" !store_dir_name;
      flush stdout
    end
  else
    begin
      printf "\n\n*******************************************************\n\n";
      printf " Please check the specified store directory %s \n" !store_dir_name;
      printf " and make sure that directory exists \n";
      printf "\n*******************************************************\n";
      raise (Shredded_Load_Error "Loading Error")
    end

let check_xml_file input_file = 
  if (Sys.file_exists input_file) 
  then
    begin
      printf "\n Input file %s found ....... Ok \n" input_file;
      flush stdout
    end
  else
    begin
      printf "\n\n*******************************************************\n\n";
      printf " Please check the specified XML filename %s \n" input_file;
      printf " and make sure that file exists \n";
      printf "\n*******************************************************\n";
      raise (Shredded_Load_Error "Loading Error")
    end

let process_args input_file = 
  begin
    check_xml_file input_file;
    check_store_dir()
  end

let get_args() = 
  let args = ref [] in
  begin
    Arg.parse
      [
       "-version",Arg.Unit (fun() -> print_version(); exit 0), " Prints the Shredded loader version";
       "-store_dir", Arg.String (fun f -> store_dir_name := f), " Directory Path where store is to be created";
       "-store_name", Arg.String (fun f -> store_name := f), " Logical name of the store";
       "-buff_size", Arg.Int (fun f -> buff_size := f), " Size of the buffer to be used";
       "-typed", Arg.String(fun s -> typed_load := Top_config.bool_of_onoff s), "Used typed or regular loading";
     ]
      (fun arg -> args := arg :: !args) usage_msg;
    match !args with 
    | [] ->
	printf "\n\n*********************************************\n\n";
	printf "\t Please specify XML file to be stored\n";
	printf "%s\n" usage_msg;
	printf "\n*********************************************\n";
	raise (Shredded_Load_Error "Loading Error")
    | fname -> 
	if((List.length fname) = 1) 
	then
	  List.hd fname
	else
	  raise (Shredded_Load_Error "Loading Error : Please Specify only one XML file")
  end

let load_file input_file =
  let (_, xml_stream) = Streaming_parse.open_xml_stream_from_io (Galax_io.File_Input input_file) in 
  let nodeid_context  = Nodeid_context.default_nodeid_context() in
  let shredded_handle =
    let resolved_xml_stream = Streaming_ops.resolve_xml_stream xml_stream in     	  
    if !typed_load then
      begin
	let typed_stream = Streaming_ops.typed_of_resolved_xml_stream resolved_xml_stream in 
	let docid_gen    = (* Shouldn't this be in the store? *)Nodeid.build_docid_gen () in 
	let ot_stream    = Streaming_ops.ordered_typed_of_typed_stream docid_gen nodeid_context typed_stream in 
	  Shredded_Store.load_shredded_store_from_ordered_typed_stream nodeid_context ot_stream !store_dir_name !store_name !buff_size
      end
    else
      begin
	  Shredded_Store.load_shredded_store_from_resolved_stream 
	    nodeid_context resolved_xml_stream !store_dir_name !store_name !buff_size 
      end
  in
    Shredded_Store.close_store shredded_handle

let main() = 
	let input_file = get_args() in 
	process_args input_file;
	printf "\n\n*********************************************\n\n";
	printf "  Loading file %s in shredded store\n" input_file;
	printf "  Store is created at %s with prefix-name %s\n" !store_dir_name !store_name;
	printf "\n*********************************************\n";
	load_file input_file

let _ = 
  try
    main()
  with
  | e ->
      begin
	eprintf_error "  " e;
	fprintf err_formatter "@."
      end

