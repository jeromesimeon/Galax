let make_conf file =
  let test_config = top_make_test_config file in
  let all_bugs = get_all_bugs test_config in
  let (name,unit) = List.nth test_config.units 0 in
  let config = unit in
  let _ = begin make_catalog unit (); Conf.warning := false end in
  let all_groups = get_all_groups config () in
  (config,all_groups,all_bugs)

let find_case t group_name case_name =
  let find_case_from_name cn cases = List.find (fun x -> x.test_name = cn) cases in
  let (config,all_groups,all_bugs) = t in
  let cases = cases_of_group_name config all_groups group_name in
  find_case_from_name case_name cases

let run_case t case =
  let (config,all_groups,all_bugs) = t in
  run_case config all_bugs case

let file = "/home/simeon/GalaxNEW/regress/testconfig.xml"
let group_name = "Literals"
let case_name = "Literals056"

let t = make_conf file
let case = find_case t group_name case_name
let result = run_case t case

let case_dir = Filename.concat config.query_dir case.test_path
let output_dir = Filename.concat config.expected_dir case.test_path
let query_file = Filename.concat case_dir case.test_qfil
let result = run_case_eval config all_bugs case eval_case
let test_comps = case.test_comp
let compared_plan = None
let output_dir = Filename.concat config.expected_dir case.test_path
let sresult = match result with Eval_Succeeded result -> result | _ -> raise Not_found

let result_string =
    let result_stream1 =
      Physical_export.typed_xml_stream_of_datamodel (Cursor.cursor_of_list sresult)
    in
    let result_stream2 = sequence_normalization result_stream1 in
    Serialization.bserialize_typed_xml_stream proc_ctxt result_stream2

