(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: galax_copy_node.ml,v 1.11 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Galax_copy_node
   Description:
     This module implements deep copy of Galax nodes.
*)

(*************)
(* Deep copy *)
(*************)

let deep_copy nodeid_context docid node_sequence =
  let item_sequence = Cursor.cursor_map (fun n -> (Physical_value.Item_Node n)) node_sequence in

  (* 1. Exports the data model as a stream *)
  let input_stream          = Physical_export.typed_xml_stream_of_datamodel item_sequence in

  (* 2. Do the erasure *)
  let resolved_input_stream = Streaming_ops.erase_xml_stream_section_3_7_1 input_stream in

  (* 3. Add type annotations back *)
  let typed_input_stream    = Streaming_ops.typed_of_resolved_xml_stream resolved_input_stream in

  (* 4. Load the data model back *)
  (Physical_load.load_xml_node_sequence_from_typed_stream_for_docid nodeid_context docid typed_input_stream)

(* Registration hack! *)

let _ =
  try
    Galax_dm.register_deep_copy_fun deep_copy
  with
  | e ->
      begin
	Error.eprintf_error "  " e;
	Format.fprintf (!Conf.glx_err_formatter) "@."
      end

