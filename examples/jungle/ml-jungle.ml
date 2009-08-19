open Galax
open Galax_io
open Dm
open Dm_functions

(* 
   Example 1 

   Build a query context from a file.
   Load a document.
   Add a global variable to query context, bound to the loaded document.
   Evaluate a query contained in a string.
   Serialize result to standard output

*)
let example1 prepared_prolog =
  try
    (* Evaluate global variables *)
    let pc = procctxt_from_prepared_prolog prepared_prolog in
    let query_input = File_Input "jungle1.xq" in
    let res = eval_statement prepared_prolog query_input in  
    serialize pc (Formatter_Output Format.std_formatter) res
  with
  | exn -> Error.printf_error "Example 1:" exn

let _ =
  try 
    let pc = Processing_context.default_processing_context() in
    let compiled_prolog = load_standard_library pc in
    let prepared_prolog = eval_prolog compiled_prolog None in 
    example1 prepared_prolog   
(*
    let _ = load_document pc (Http_Input "jungle:///home/mff/galax/examples/jungle/tmp#XMark")
    in () 
*)
  with
  | exn -> Error.printf_error "" exn

