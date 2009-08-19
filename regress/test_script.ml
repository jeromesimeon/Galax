open Galax_io
open Streaming_constructors
open Serialization
open Test_core

let file = "/home/simeon/NEW/TMP/galax/regress/testconfig.xml"

let config = top_make_test_config file

let all_bugs = get_all_bugs config

let (name,unit_config) = List.nth config.units 0

let _ = 
  try
    make_catalog unit_config ()
  with
  | e ->
      begin
	eprintf_error "  " e;
	fprintf (!Conf.glx_err_formatter) "@."
      end

let all_groups = 
  try
    get_all_groups unit_config ()
  with
  | e ->
      begin
	eprintf_error "  " e;
	fprintf (!Conf.glx_err_formatter) "@."
      end

let cs = 
  try
    cases_of_group_name all_groups "TraceFunc"
  with
  | e ->
      begin
	eprintf_error "  " e;
	fprintf (!Conf.glx_err_formatter) "@."
      end

let run x = 
  run_case unit_config all_bugs (List.nth cs x)

let gcs x = List.nth cs x

let _ =
  let n = ref 0 in
  try
    for i = 0 to 1000 do
      n := i;
      Format.printf "%i;" i; flush stdout; ignore(run i);
    done;
    gcs !n
  with
  | _ ->
      gcs !n

let find_test name =
  let i = ref 0 in
  let x =
    let find x = (incr i ; x.test_name = name) in
    begin
      if (List.exists find cs)
      then !i - 1
      else raise Not_found
    end
  in
  gcs x

(*
let _ = run 31
let case = gcs 31
*)

let case = find_test "Literals057"

let case_dir = Filename.concat unit_config.query_dir case.test_path
let output_dir = Filename.concat unit_config.expected_dir case.test_path
let query_file = Filename.concat case_dir case.test_qfil
let output_file = Filename.concat output_dir (List.nth case.test_ofil 0);;
let vars = List.map (create_var unit_config) case.test_ifil
let result = wrap_result all_bugs case eval_case query_file vars

let _ = compare_results case result [output_file]

let result =
  match result with
  | Eval_Succeeded result ->
      result
  | _ -> raise Not_found



let cs1 = List.nth cs 31

