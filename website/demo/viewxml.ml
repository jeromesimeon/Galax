(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: viewxml.ml,v 1.9 2007/02/01 22:08:55 simeon Exp $ *)
open Demo_conf
open Error
open Top_util

open Netcgi
open Netcgi_types

let main (cgiobj  : cgi_activation) =
  demo_init "galax_demo_config";
  (* Sys.chdir !document_dir; *)
  let file = cgiobj # argument_value "file" in
(*  cgiobj # output # output_string ("<b>File parsed is: " ^ file ^"</b><br/>\n");  *)
  let content = Gmisc.load_file file in
(*  cgiobj # output # output_string "<pre>"; *)
(*  cgiobj # output # output_string (Netencoding.Html.encode_from_latin1 content) *)
  cgiobj # output # output_string content
(*  cgiobj # output # output_string "</pre><br/>\n"*)

let beg_html =
  "<html><head><title>Galax</title><link href=\"../galax.css\" type=\"text/css\" rel=\"stylesheet\"></head>\n<body>\n"

let end_html =
  "</pre>\n\
</body></html>\n"

let exec func (cgiobj : cgi_activation) =
   try 
    cgiobj # set_header ~status:`Ok ~content_type:"text/plain" ();
(*    cgiobj # output # output_string beg_html; *)
    func cgiobj;  
(*    cgiobj # output # output_string end_html; *)
    cgiobj # output # commit_work()
  with
    e ->
      begin
	cgiobj # output # rollback_work();
	cgiobj # set_header ~status:`Internal_server_error ();
	cgiobj # output # output_string beg_html;
        cgiobj # output # output_string "<H2>An Error Was Raised:</H2>\n<pre>\n";
        cgiobj # output # output_string (bprintf_error_safe "  " e);
        cgiobj # output # output_string ("\n</pre>\n"^(!separator));
        cgiobj # output # output_string end_html;
        cgiobj # output # commit_work()
      end


let _ = 
  let operating_type = Netcgi.buffered_transactional_optype in
  let cgiobj = ((new Netcgi.std_activation ~operating_type ()) :> cgi_activation) in 
  exec main cgiobj
