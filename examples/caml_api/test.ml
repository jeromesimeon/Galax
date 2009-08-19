open Galax
open Galax_io
open Dm
open Dm_functions
open Dm_atomic
open Physical_item
open Physical_item_util

(*
  
    This snippet of code illustrates how to call an XQuery function 
    from ML. 

    IF YOU ADD TESTS TO THIS FILE, YOU MUST ALSO ADD THEM TO: 
      examples/c_api/test.c
      examples/java_api/Test.java

 *)

let example1 () =
  let pc = Processing_context.default_processing_context () in
  let _ = Monitoring_context.set_monitor_time pc.Processing_context.monitor_context true in  
  let _ = Monitor.start_monitor_external_call pc "caml_api/Test" in
  let _ = Processing_context.set_serialization_kind pc Processing_context.Serialize_As_Well_Formed in  
  try 
    (* CONSTRUCTORS *)
    (* xs:integer *)
      let i' = atomicInteger (Big_int.big_int_of_int 20) in
      let s = serialize_to_string pc [to_atomicValue_item i'] in
      let _ = if (not(Big_int.eq_big_int (integer_of_atomicInteger i') (Big_int.big_int_of_int 20))) then print_string "Error! roundtripping integer\n" in
      let _ = print_string ("Test Integer:\n"^s^"\n") in
      
    (* xs:decimal *)
      let i'' = atomicDecimal (Num.num_of_int 20) in
      let s = serialize_to_string pc [to_atomicValue_item i''] in
      let _ = if (not(Num.eq_num (decimal_of_atomicDecimal i'') (Num.num_of_int 20))) then print_string "Error! roundtripping decimal\n" in
      let _ = print_string ("Test Decimal:\n"^s^"\n") in

    (* xs:string *)
      let st = atomicString "user test" in
      let s =  serialize_to_string pc [to_atomicValue_item st] in
      let _ = if (not(string_of_atomicValue st = "user test")) then print_string "Error! roundtripping string\n" in
      let _ = print_string ("Test String:\n"^s^"\n") in
      
    (* xs:boolean *)
      let b = atomicBoolean false in
      let s =  serialize_to_string pc [to_atomicValue_item b] in
      let _ = if (not(boolean_of_atomicBoolean b = false)) then print_string "Error! roundtripping boolean\n" in
      let _ = print_string ("Test Boolean:\n"^s^"\n") in
      
    (* xs:float *)
      let d = atomicFloat 98.549 in
      let s =  serialize_to_string pc [to_atomicValue_item d] in
      let _ = if (not(float_of_atomicFloat d = 98.549)) then print_string "Error! roundtripping float\n" in
      let _ = print_string ("Test Float:\n"^s^"\n") in

    (* xs:double *)
      let db = atomicDouble 98.549 in
      let s =  serialize_to_string pc [to_atomicValue_item db] in
      let _ = if (not(float_of_atomicDouble db = 98.549)) then print_string "Error! roundtripping double\n" in
      let _ = print_string ("Test Double:\n"^s^"\n") in

    (* xs:dateTime *)
      let dt = atomicDateTime "1991-01-02T01:01:01.01Z" in
      let s =  serialize_to_string pc [to_atomicValue_item dt] in
      let _ = print_string ("Test DateTime:\n"^s^"\n") in

    (* xs:date *)
      let date = atomicDate "2000-03-12" in
      let s =  serialize_to_string pc [to_atomicValue_item date] in
      let _ = print_string ("Test Date:\n"^s^"\n") in

    (* xs:time *)
      let time = atomicTime "03:04:05.06" in
      let s =  serialize_to_string pc [to_atomicValue_item time] in
      let _ = print_string ("Test Time:\n"^s^"\n") in

    (* xs:dayTimeDuration *)
      let dtd = atomicDayTimeDuration "P1DT2H3M1.1S" in
      let s =  serialize_to_string pc [to_atomicValue_item dtd] in
      let _ = print_string ("Test DayTimeDuration:\n"^s^"\n") in

    (* xs:yearMonthDuration *)
      let ymd = atomicYearMonthDuration "P2Y3M" in
      let s =  serialize_to_string pc [to_atomicValue_item ymd] in
      let _ = print_string ("Test YearMonthDuration:\n"^s^"\n") in

    (* serialization *)
      let docitems = load_document pc (File_Input "../docs/xml-example.xml") in
      let docitem = (to_node_item(List.hd docitems)) in
      let _  = serialize pc (Formatter_Output Format.std_formatter) (to_node_item_list docitems) in
      let s =  serialize_to_string pc (to_node_item_list docitems) in
      print_string ("Test load_document:\n"^s^"\n"); 

    (* external context with an implicit timezone *)
      let tz = atomicDayTimeDuration "-P0DT5H" in
      let j = atomicInteger (Big_int.big_int_of_int 20) in
      let exc = build_external_context pc (Some docitem) (Some tz) 
	  [("v", [docitem]); 
	    ("x",to_node_item_list docitems); 
	    ("y", [to_atomicValue_item j])] in

    (* import_main_module with an external context *)
      let sc = load_standard_library pc in
      let (cn, stmts) = import_main_module true sc (File_Input "../docs/xq-example2.xq") in 
      let pp = prepare_program cn (Some exc) in
      let v = eval_compiled_statement pp  (List.nth stmts 0) in
      let s = serialize_to_string pc v in
      print_string ("Test import_main_module:\n"^s^"\n");
      
    (* xs:QName *)
      let q = atomicQName (nsenv_of_main_module cn, "xq-example1:bar") in
      let s = serialize_to_string pc [to_atomicValue_item q] in
    (* No round trip for QName yet *)
      let _ = print_string ("Test QName:\n"^s^"\n") in
      
    (* ROUND TRIPPING *)
    (* comment *)
      let c = commentNode(atomicString "This is a comment") in
      let s = serialize_to_string pc [to_node_item c] in
      let _ = print_string("Test node_kind:\n"^node_kind(c)^"\n") in
      let _ = if not(node_kind(c) = "comment") then print_string "Error! node kind != comment\n" in
      let _ = print_string ("Test commentNode:\n"^s^"\n") in

    (* PI *)
      let p = processingInstructionNode(atomicString "target", atomicString("This is a processing instruction")) in

      let s =  serialize_to_string pc [(to_node_item p)] in
      let _ = if not(node_kind (to_node p) = "processing-instruction") then print_string "Error! node kind != processing-instruction\n" in
      let _ = print_string ("Test processingInstructionNode:\n"^s^"\n") in
    (* element *)
      let e = elementNode (q, [], [], q) in
      let s = serialize_to_string pc [(to_node_item e)] in
      let _ = if not(node_kind(to_node e) = "element") then print_string "Error! node kind != element\n" in
      let _ = print_string ("Test elementNode:\n"^s^"\n") in
      
    (* QName *)
      let q1 = atomicQName (nsenv_of_main_module cn, "baz") in
      let s =  serialize_to_string pc [to_atomicValue_item q1] in
    (* No round trip for QName yet *)
      let _ = print_string ("Test QName:\n"^s^"\n") in
      
    (* document *)
      let doc = documentNode ("http://db.bell-labs.com/galax",
			      [to_node p; 
				to_node c; 
				to_node e; 
				to_node e]) in
      let s = serialize_to_string pc [(to_node_item doc)] in
      let _ = if not(node_kind((to_node doc)) = "document") then print_string "Error! node kind != document\n" in
      let _ = print_string ("Test documentNode:\n"^s^"\n") in
      
    (* attribute *)
      let a = attributeNode (q1, atomicString("attribute value"), q) in
      let s = serialize_to_string pc [(to_node_item a)] in
      let _ = if not(node_kind((to_node a)) = "attribute") then print_string "Error! node kind != attribute\n" in
      let _ = print_string ("Test attributeNode:\n"^s^"\n") in
      
    (* element *)
      let e = elementNode (q, [a], [(to_node e); (to_node e)], q) in
      let s = serialize_to_string pc [(to_node_item e)] in
      let _ = print_string ("Test elementNode children:\n"^s^"\n") in

    (* node name *)
      let q1opt = node_name (to_node e) in
      let _ = 
	match q1opt with 
	| [] -> print_string "Error! node name is empty\n" 
	| [q1] -> 
	    begin
	      let s = serialize_to_string pc [to_atomicValue_item q1] in
	      print_string ("Test node_name:\n"^s^"\n") 
	    end
	|	_ -> raise(Failure("Node has more than one parent"))
      in

    (* children  *)
      let k = children (to_node e) in 
      let s = serialize_to_string pc (List.map (fun n -> Physical_value.Item_Node n) k) in
      let _ = print_string ("Test children:\n"^s^"\n") in
      
    (* parent *)
      let pr = parent (List.hd k) in 
      let s  = serialize_to_string pc (to_node_item_list pr) in
      let _  = print_string ("Test parent:\n"^s^"\n") in
      
    (* attributes *)
      let ats = attributes (to_node e) in 
      let s   = serialize_to_string pc (to_node_item_list ats) in
      let _   = print_string ("Test attributes:\n"^s^"\n") in
      
    (* text node *)
      let t = textNode (atomicString("this is a text node")) in
      let s = serialize_to_string pc [to_node_item t] in
      let _ = if not(node_kind(t) = "text") then print_string "Error! node kind != text\n" in
      let _ = print_string ("Test text:\n"^s^"\n") in
      
    (* base_uri of a document node *)
    (* Base URIs are not portable. *)
(*      let b = base_uri (List.hd docitems) in
      let _ =
	match b with
	| [] -> print_string "Error! base uri is empty\n"
	| [u] -> ()
	| _ -> raise(Failure("Node has more than one base uri"))
      in
*)
    (* base_uri of an element node *)
      let b = base_uri(List.hd(children (List.hd docitems))) in
      let _  =
	match b with
	| [] -> print_string "Error! base uri is empty\n"
	| [u] -> 
	    begin
	      let s =  serialize_to_string pc [to_atomicValue_item u] in
	      print_string ("Test base_uri:\n"^s^"\n")
	    end
	| _ -> raise(Failure("Node has more than one base uri"))
      in

    (* eval_statement_from_string *)
      let query_input = String_Input "xq-example1:test0()" in
      let v = eval_statement pp query_input in 
      let s = serialize_to_string pc v in
      print_string ("Test eval_statement 1:\n"^s^"\n"); 

    (* eval_statement_from_string *)
      let query_input = String_Input "20.0 * 3.3"  in
      let v = eval_statement pp query_input in 
      let s = serialize_to_string pc v in
      print_string ("Test eval_statement 2:\n"^s^"\n"); 

    (* eval_statement_from_string *)
      let query_input = String_Input "$x" in
      let v = eval_statement pp query_input in
      let s = serialize_to_string pc v in
      print_string ("Test variable $x:\n"^s^"\n"); 

    (* eval_statement_from_string *)
      let query_input = String_Input "$y"  in
      let v = eval_statement pp query_input in
      let s = serialize_to_string pc v in
      print_string ("Test variable $y:\n"^s^"\n"); 

    (* Test the implicit timezone *)
      let query_input = String_Input "xs:dateTime('1991-12-31T23:00:00.0') = xs:dateTime('2000-01-01T04:00:00.0Z')"  in
      let v = eval_statement pp query_input in
      let s = serialize_to_string pc v in
      print_string ("Test dateTime equality:\n"^s^"\n"); 

      (* Path expression *)
      begin
	let query_input = String_Input "./html/ul/li"  in
	let v = eval_statement pp query_input in
	let s = serialize_to_string pc v in
	print_string ("Test eval_statement 3:\n"^s^"\n")
      end;

      begin
	let query_input = File_Input "../docs/htmlpath.xq" in
	let v = eval_statement pp query_input in
	let s = serialize_to_string pc v in
	print_string ("Test eval_statement 4:\n"^s^"\n")
      end;

      begin
	let query_input = String_Input "$v/html/ul/li" in
	let v = eval_statement pp query_input in
	let s = serialize_to_string pc v in
	print_string ("Test eval_statement 5:\n"^s^"\n")
      end; 
      
      (* Test rebinding variables *)
      begin
	let i = atomicInteger (Big_int.big_int_of_int 30) in
	let exc = build_external_context pc (Some docitem) (Some tz) [("v", [to_atomicValue_item i]); ("x",to_node_item_list docitems); ("y", [to_atomicValue_item i])] in
	let pp = prepare_program cn (Some exc) in
	let query_input = String_Input "($v, $z)" in
	let v = eval_statement pp query_input in
	let s = serialize_to_string pc v in
	print_string ("Test rebinding of variables:\n"^s^"\n")
      end;

    (* import_library_module *)
      let sc = load_standard_library pc in
      let cn2 = import_prolog_only sc false (File_Input "../docs/xmlschema_context.xq") in
      let pp2 = prepare_program cn2 None in
      let query_input = String_Input "$xmlschema:po/HisPo:purchaseOrder/items/item/quantity" in
      let v = eval_statement pp2 query_input in
      let s = serialize_to_string pc v in
      print_string ("Test import_library_module:\n"^s^"\n"); 
      
    (* typed_value *)
      let atv = typed_value(get_node(List.hd(v))) in 
      let s = serialize_to_string pc (to_atomicValue_item_list atv) in
      print_string ("Test typed_value:\n"^s^":"^(string_of_item_kind(item_kind (List.hd (to_atomicValue_item_list atv))) ^"\n")); 

    (* Raise an error intentionally -- always do this last *)
      print_string("Raise an error intentionally:\n"); 
      let _ = load_document pc (File_Input "no-such-file.xml") in
      print_string("We shouldn't be here!\n");

    with
    | exn -> 
	begin
	  (* First, dump the monitor info  -- test both calls *)
(*
	  let mv = Monitor.monitor_of_last_call pc in
	  let s = serialize_to_string pc mv in
	  print_string (s^"\n"); 

	  let mv = Monitor.monitor_of_all_calls pc in 
	  let s = serialize_to_string pc mv in
	  print_string (s^"\n"); 
*)
          (* Any errors raised by Galax are printed to stderr *)
	  Error.printf_error "" exn; print_string("\n"); 
	  Error.printf_warning "This is an intensional error\n"; exit 1;
	end

let _ =
  try
    example1 ()
  with
  | exn -> Error.printf_error "" exn; print_string("\n");

