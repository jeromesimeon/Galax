(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)
(*******************************)
(* Http protocol handler       *)
(*******************************)

val get_inet_socket_address : string -> int -> Unix.sockaddr

val decode_url :
            (* scheme * host * port option * cmd-path option *)
  string -> (string * string * string option * string option) option

module HTTP :
  sig
    exception Break
    val buflen : int

(* HTTP header *)
                       (* content-type -> header string *)
    val header_content_type : string -> string 
    val header_set_session  : int -> string 

                   (* host -> port -> cmd -> URL *)
    val make_url : string -> int option -> string -> (* URL *) string

(* HTTP Client *)
    (* We should probably restrict this interface : one function for each method *)

    (* Client-side functions only return HTTP message payloads (no
      status or headers) and an raise exception if any status other
      than 200/"OK" is received.  *)

    (* gen_request uses our home-grown code *)
        (* host -> port -> cmd -> method -> (extra headers * msg body) -> http-response payload *)
    val gen_request :
	string -> int -> string -> string -> (string * string) option -> string

    (* get uses Http_client *)
             (* URL as http://host:port/cmd -> http-response payload *)
    val get : string -> string

(* HTTP Server *)
    type http_method = Get of string | Post of string
    type header =
        ContentLength of int
      | Header of string * string
      | Other of string

    module Echo :
      sig
        val line_of_method : http_method -> string
        val line_of_header : header -> string
        val echo : http_method * header list * string option -> unit
      end
    val method_of_line : string -> http_method
    val header_of_line : string -> header

    val get_payload : header list -> in_channel -> string option
    val get_headers : in_channel -> header list
    val get_method  : in_channel -> http_method
    val get_http_request : in_channel -> http_method * header list * string option

                                  (* Key *)
    val get_header : header list -> string -> string 

    (* All the send_*http_response functions send an HTTP response; 
       The latter two set Content-type to "text/html" and 
       "application/xhtml+xml", respectively 
                                  out_channel -> extra headers -> payload *)
    val send_http_response      : out_channel -> string -> string -> unit
    val send_html_http_response : out_channel -> string -> string -> unit
    val send_xml_http_response  : out_channel -> string -> string -> unit 

    val ok_response : string -> bool

(* XQuery RPC *)
    val http_query_response :
      string -> string -> int -> string -> string -> string -> string
    val http_query_async :
      string -> string -> int -> string -> string -> string -> unit
  end
