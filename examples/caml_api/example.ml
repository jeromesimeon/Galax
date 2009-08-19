open Galax
open Galax_io
open Dm
open Dm_functions

(* 
   Example 1 

   Import a main module.
   Evaluate a query contained in a string.
   Serialize result to standard output.

*)
let example1 compiled_program exc =
  try
    let (compiled_program, stmts) = import_main_module false compiled_program (File_Input "../docs/seq.xq") in

    let prepared_program = prepare_program compiled_program (Some exc) in 
    let pc = procctxt_of_prepared_program prepared_program in
    let query_input = String_Input "for $s in $seq_context:report//section[section.title = \"Procedure\"] return ($s//incision)[2]/instrument" in
    let res = eval_statement prepared_program query_input in  
    serialize pc (Formatter_Output Format.std_formatter) res
  with
  | exn -> Error.printf_error "Example 1:" exn

(* 
   Example 2

   Import a main module.
   Evaluate the first statement in that module.
   Serialize result to standard output.

*)
let example2 compiled_program exc =

  try
    (* Load main module *)
    let (compiled_program', stmts) = import_main_module false compiled_program (File_Input "../docs/seq.xq") in
    let prepared_program = prepare_program compiled_program' (Some exc) in 
    let pc = procctxt_of_prepared_program prepared_program in
    serialize pc (Formatter_Output Format.std_formatter) (eval_compiled_statement prepared_program (List.nth stmts 0))
  with
    (* Any errors raised by Galax are printed to stderr *)
  | exn -> Error.printf_error "Example 2" exn

(* 
   Example 3

   Import a main module.
   Bind external variables to values.
   Prepare the program.
   Evaluate a query function contained in the library module by applying it to the bound variables.
   Serialize result to a string & print. 

*)
let example3 pc compiled_program xmldoc exc =
  try
    (* Load main module *)
    let (compiled_program, stmts) = import_main_module false compiled_program (File_Input "../docs/xq-example2.xq") in

    (* Evaluate global variables *)
    let prepared_program = prepare_program compiled_program (Some exc) in 
    let pc = procctxt_of_prepared_program prepared_program in
    let res = eval_statement prepared_program (String_Input "xq-example1:test2($y,$x)") in  
    (* Serialize the result to a string *)
    print_string(serialize_to_string pc res);
    prepared_program
  with
    (* Any errors raised by Galax are printed to stderr *)
  | exn -> (Error.printf_error "Example 3" exn; raise exn)

(* 
   Example 4

   Evaluate a query contained in a string.
   Serialize result to standard output

*)
let example4 prepared_program =

  try
    let pc = procctxt_of_prepared_program prepared_program in
    let query_input = String_Input "<foo1:a xmlns:foo1='http://www.foo.org/'/>" in
    let res = eval_statement prepared_program query_input in  
    serialize pc (Formatter_Output Format.std_formatter) res
  with

    (* Any errors raised by Galax are printed to stderr *)
  | exn -> Error.printf_error "Example 4:" exn


(* 
    Load a document 
    Load a library module from a file    
    Prepare/evaluate the prolog 
    Evaluate each example using the prepared or compiled prolog
*)
let _ =
  try 
    let pc = Processing_context.default_processing_context() in
    let xmlint = to_atomicValue_item(atomicInteger (Big_int.big_int_of_int 12)) in
    let xmlstr = to_atomicValue_item(atomicString "a string") in
    let exc = build_external_context pc None None 
	[("x",[xmlstr]); 
	  ("y", [xmlint])] in

    let compiled_program = load_standard_library pc in
    example1 compiled_program exc; 

    let compiled_program = load_standard_library pc in
    example2 compiled_program exc;

    let doc_input = File_Input "../docs/report.xml" in
    let xmldoc = load_document pc doc_input in
    let compiled_program = load_standard_library pc in
    let exc = build_external_context pc None None 
	[("x",[xmlstr]); 
	  ("y", [xmlint]); 
	  ("v",[xmlstr])] in
    let prepared_program = example3 pc compiled_program xmldoc exc in 

    example4 prepared_program

  with
  | exn -> Error.printf_error "" exn

