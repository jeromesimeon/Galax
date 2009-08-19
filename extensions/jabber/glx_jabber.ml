(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: glx_jabber.ml,v 1.5 2007/02/01 22:08:46 simeon Exp $ *)

open Error
open Jabber_buddies
open Physical_util

let get_presence_information s_jid s_passwd timeout debug = 
  let (errcode, xmlstr) = presence_information s_jid s_passwd timeout debug 
  in 
  if (errcode = 0) then xmlstr
  else raise (Query(Error("Jabber: "^xmlstr)))

(*
  "<soapenv:Fault xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\"><faultcode>Server</faultcode><faultstring>" ^
  "In order to access Jabber presence, you should compile the Galax Jabber wrapper tool with libiksemel support" ^
  "</faultstring><faultactor>Galax Jabber wrapper</faultactor><detail/></soapenv:Fault>\n"
*)

let _glx_jabber_buddies comp_ctxt alg_ctxt n =

  let proc_ctxt = Compile_context.processing_context_from_compile_context comp_ctxt in
  let (p1,p2,p3,p4) = Args.get_array_param4 n in
p1
(*
  let jid =  get_string p1 in
  let passwd =  get_string p2 in
  let timeout = Decimal._int_of_decimal(Decimal._decimal_of_integer(get_integer p3)) in
  let debugmode = get_boolean p4 in
  let s_out = get_presence_information jid passwd timeout debugmode in
  Physical_util._node_list(Galax.load_document proc_ctxt (Galax_io.String_Input s_out))*)

(*
  let xml_stream = Sax_parser.open_xml_stream_from_io proc_ctxt (Galax_io.Buffer_Input s_out) in
  (* First resolve namespaces *)
  let resolved_xml_stream = Stream_ops.resolve_xml_stream xml_stream in
  (* Then apply type annotations *)
  let typed_xml_stream = Stream_ops.typed_of_resolved_xml_stream resolved_xml_stream in
  (* Finally, load the document *)
  let nodeid_context = Nodeid_context.default_nodeid_context () in
  Cursor.cursor_of_list (Galax_load.load_xml_document_from_typed_stream nodeid_context typed_xml_stream)
*)

(* Dynamic registration of the the additional built-in function.
   - Jerome *)

let glx_jabber_buddies = (Namespace_builtin.glx_prefix, Namespace_builtin.glx_uri, "jabber-buddies") 

let _ =
(*  print_string "Load glx:jabber_buddies\n"; *)
  Code_fn.add_bltin_fctn ((glx_jabber_buddies, 4), _glx_jabber_buddies)
