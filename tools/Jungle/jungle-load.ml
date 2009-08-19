(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: jungle-load.ml,v 1.2 2007/02/01 22:08:54 simeon Exp $ *)

open Format
open Error

exception Jungle_Load_Error of string

let jungle_version = "0.1"

let store_dir_name = ref "." 

let store_name = ref "Jungle"

let buff_size = ref 262144

let print_version() = 
	begin
		printf "\n";
		printf " This is Jungle load utility, version %s\n" jungle_version;
		printf " Copyright. 2001-2007\n";
		printf " Distributed only by permission\n\n";
		printf " For any question, please contact:\n";
		printf " \tJerome Simeon (simeon@@research.bell-labs.com)\n";
		printf " \tAvinash Vyas (vyas@@research.bell-labs.com)\n";
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
      raise (Jungle_Load_Error "Loading Error")
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
      raise (Jungle_Load_Error "Loading Error")
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
       "-version",Arg.Unit (fun() -> print_version(); exit 0), " Prints the Jungle loader version";
       "-store_dir", Arg.String (fun f -> store_dir_name := f), " Directory Path where store is to be created";
       "-store_name", Arg.String (fun f -> store_name := f), " Logical name of the store";
       "-buff_size", Arg.Int (fun f -> buff_size := f), " Size of the buffer to be used";
     ]
      (fun arg -> args := arg :: !args) usage_msg;
    match !args with 
    | [] ->
	printf "\n\n*********************************************\n\n";
	printf "\t Please specify XML file to be stored\n";
	printf "%s\n" usage_msg;
	printf "\n*********************************************\n";
	raise (Jungle_Load_Error "Loading Error")
    | fname -> 
	if((List.length fname) = 1) 
	then
	  List.hd fname
	else
	  raise (Jungle_Load_Error "Loading Error : Please Specify only one XML file")
  end

let load_file input_file =
  let (_, xml_stream) = Streaming_parse.open_xml_stream_from_io (Galax_io.File_Input input_file) in 
  let resolved_xml_stream = Streaming_ops.resolve_xml_stream xml_stream in 
  let nodeid_context = Nodeid_context.default_nodeid_context() in
  let jungle_handle = Jungle_load.load_jungle_store_from_resolved_stream nodeid_context resolved_xml_stream !store_dir_name !store_name !buff_size in 
  (*
     printf "\n\n*********************************************\n";
     print_string (" Nos of Total Gets in jungle store = " ^ string_of_int(Jungle.get_get_count()) ^ "\n");
     print_string (" Nos of Total Puts in jungle store = " ^ string_of_int(Jungle.get_put_count()) ^ "\n");
     print_string (" Nos of Puts in Main_db jungle store = " ^ string_of_int(Jungle_store.get_main_db_put_count()) ^ "\n");
     print_string (" Nos of Puts in Text_db jungle store = " ^ string_of_int(Jungle_store.get_text_db_put_count()) ^ "\n");
     print_string (" Nos of Puts in Qname_db jungle store = " ^ string_of_int(Jungle_store.get_qname_db_put_count()) ^ "\n");
     print_string (" Nos of Puts in PrePost_db jungle store = " ^ string_of_int(Jungle_store.get_prepost_db_put_count()) ^ "\n");
     print_string (" Nos of Puts in Child_db jungle store = " ^ string_of_int(Jungle_store.get_child_db_put_count()) ^ "\n");
     print_string (" Nos of Puts in Attr_db jungle store = " ^ string_of_int(Jungle_store.get_attr_db_put_count()) ^ "\n");
     print_string (" Nos of Puts in Uri-Prefix_db jungle store = " ^ string_of_int(Jungle_store.get_uri_prefix_db_put_count()) ^ "\n");
     print_string (" Nos of Gets in Qnameid_db jungle store = " ^ string_of_int(Jungle_store.get_qname_db_get_count()) ^ "\n");
     printf "\n*********************************************\n";
   *)
  Jungle_store.close_jungle_store jungle_handle

let main() = 
	let input_file = get_args() in 
	process_args input_file;
	printf "\n\n*********************************************\n\n";
	printf "  Loading file %s in jungle store\n" input_file;
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

