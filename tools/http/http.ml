(* Module added by Trevor, for http get *)
(* Modified by Nicola Onose, May 2003 *)
(* Modified by Jerome Simeon, July 2003 *)

(* open Neturl;; *)

let get_inet_socket_address host port = 
  try
    Unix.ADDR_INET((Unix.gethostbyname host).Unix.h_addr_list.(0), port)
  with Not_found ->
    raise (Failure
             (Printf.sprintf
                "Can't find the IP address of the server (%s)" host))

let decode_url s =
   let len = String.length s in
   try
     let i = String.index s ':' in (* might raise Not_found *)
     let scheme = String.sub s 0 i in
     if i+3 >= len then raise Not_found;
     if String.get s (i+1) <> '/' then raise Not_found;
     if String.get s (i+2) <> '/' then raise Not_found;
     let colon_position =
       try Some(String.index_from s (i+3) ':')
       with Not_found -> None in
     let slash_position =
       try Some(String.index_from s (i+3) '/')
       with Not_found -> None in
     let host,port,after_port =
       match colon_position,slash_position with
         None,None ->
           String.sub s (i+3) (len-(i+3)),
           None,
           len
       | Some j,None ->
           String.sub s (i+3) (j-(i+3)),
           Some(String.sub s (j+1) (len-(j+1))),
           len
       | None,Some j ->
           String.sub s (i+3) (j-(i+3)),
           None,
           (j+1)
       | Some j,Some k ->
           String.sub s (i+3) (j-(i+3)),
           Some(String.sub s (j+1) (k-(j+1))),
           (k+1) in
     let rest =
       if after_port >= len then None else
       Some(String.sub s after_port (len-after_port)) in
     Some(scheme,host,port,rest)
   with Not_found -> None
          
module HTTP = struct
  exception Break
  let buflen = 8192

  let make_url host portopt path = 
    "http://"^host^
    (match portopt with
      None -> ""
    | Some port -> (":"^(string_of_int port)))^
    (match path with 
    | "" -> ""
    | _ -> ("/"^path))

  let header_content_type x = Printf.sprintf "Content-Type: %s\r\n" x
  let header_set_session x = Printf.sprintf "Set-Cookie: session=%d\r\n" x

  (* Handling HTTP responses *)

  (* Scan past "\r\n\r\n" *)
  let rec after_crlfcrlf result start =
    let cr = String.index_from result start '\r' in
    if String.get result (cr+1) = '\n' &
      String.get result (cr+2) = '\r' &
      String.get result (cr+3) = '\n'
    then cr+4
    else after_crlfcrlf result (cr+1) 

  (* Scan past "\r\n" *)
  let rec after_crlf result start =
    let cr = String.index_from result start '\r' in
    if String.get result (cr+1) = '\n'
    then cr+2
    else after_crlf result (cr+1) 

  let process_status status_line =
    try
      Scanf.sscanf status_line "HTTP/1.%d %d %s\r" 
	(fun version code phrase -> 
	  if (code < 100 || code >= 600) then 
	    failwith "[HTTP: Unknown code in status line]"
	  else if code = 200 && phrase = "OK" then ()
	  else if (code >= 100 && code < 200) then
	    failwith "[HTTP: Incomplete request]"
	  else if (code >= 300 && code < 400) then
	    failwith "[HTTP: Cannot process redirection]"
	  else if (code >= 400 && code < 500) then
	    failwith ("[HTTP: " ^ (string_of_int code) ^ " " ^ phrase ^ "]")
	  else if (code >= 500 && code < 600) then
	    failwith ("[HTTP: " ^ (string_of_int code) ^ " " ^ phrase ^ "]")
	  else
	    ())
    with
    | _ ->
	failwith "[HTTP: Could not process status line]"

  let process_response result =
    try
      let header_end = after_crlfcrlf result 0 in
      let header = String.sub result 0 header_end in
      let status_end = after_crlf header 0 in
      let status_line = String.sub header 0 status_end in
      begin
	process_status status_line;
	String.sub result header_end (String.length result - header_end)
      end
    with
    | _ -> failwith "[HTTP: Cannot process server response]"

  let ok_response line =
    try 
      process_status line; 
      true
    with _ -> false

(********** HTTP Client **************)
  let gen_request h p s meth extra = (* host port resource *)
    let buf = String.create buflen in
    let longbuf = Buffer.create buflen in
    let targetInetAddr = get_inet_socket_address h p in
    let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let s' = if (String.length s > 0) then s else "/" in
      Unix.connect sock targetInetAddr;      
      let request =
        match extra with 
          | Some (headers, msg) ->
	          Printf.sprintf
	            "%s %s HTTP/1.0\r\nHost: %s:%d\r\nConnection: close\r\nContent-Length: %d\r\n%s\r\n%s\r\n\r\n" 
                meth s' h p (String.length msg) headers msg
          | None ->
	          Printf.sprintf
	            "%s %s HTTP/1.0\r\nHost: %s:%d\r\nConnection: close\r\n\r\n" 
                meth s' h p
    in
    (*  print_string ("Request:\n"^request); flush(stdout); *)
    let _ = Unix.write sock request 0 (String.length request) in
    let total_len = ref 0 in 
    (try
      while true do
        (* Printf.printf "Receiving...\n"; flush(stdout); *)
        let len = Unix.read sock buf 0 buflen in
        if len <= 0 then raise Break;
        Buffer.add_substring longbuf buf 0 len;
	total_len := len + !total_len 
      done
    with 
      Break -> ()
    | Failure msg ->
	let error = Printf.sprintf ": Buffer len = %d" !total_len in                                                       
	raise (Failure("In Http.HTTP.gen_request: "^(msg)^error)));
    Unix.close sock;
    let result = Buffer.contents longbuf in
    Buffer.reset longbuf;
    process_response result
   
  let get url = 
    let call = new Http_client.get url in
    let pipeline = new Http_client.pipeline in
    pipeline # add call;
    pipeline#run();
    match call#status with
      `Successful -> (call#response_body#value)         
    | `Unserved -> failwith "Unserved HTTP call"
    | `Client_error -> failwith "Client error"
    | `Server_error -> failwith "Server error"
    | `Http_protocol_error e -> raise e
    | `Redirection -> failwith "Redirection not supported" 

(* We would prefer to use the standard url parsing routines, but
    (1) base/uri.mli makes the type of _uri abstract so we can't
        use the functions in tools/netstring/neturl.ml
    (2) those functions don't seem to decode ldap urls properly,
        e.g., they complain on
     
ldap://ldap.itd.umich.edu:389/ 
o=University%20of%20Michigan,c=US??sub?sn=john
*)

(********** HTTP Server **************)

  type http_method =
      Get of string
    | Post of string

  type header =
      ContentLength of int
    | Header of string * string
    | Other of string

  (* For debugging *) 
  module Echo = struct            
    let line_of_method h =
      match h with
        Get s -> Printf.sprintf "GET %s HTTP/1.1\r\n" s
      | Post s ->  Printf.sprintf "POST %s HTTP/1.0\r\n" s

    let line_of_header h =
      match h with
        ContentLength s -> Printf.sprintf "Content-Length: %d\r\n" s
      | Header(x,y) -> Printf.sprintf "%s: %s\r\n" x y
      | Other s -> ">>>> "^s^"\n"

    let echo (meth,headers,payload) =
      Printf.eprintf "You said\n\n";
      output_string stderr (line_of_method meth);
      List.iter (fun x -> output_string stderr (line_of_header x)) headers;
      begin
        match payload with None -> ()
        | Some s ->
            output_string stderr "\r\n";
            output_string stderr s;
            output_string stderr "\n"
      end;
      output_string stderr "\n(that's all)\n";
      flush stderr;
  end (* module Echo *)

  let method_of_line line =
    try
      Scanf.sscanf line "GET %s HTTP/1.1\r" (fun s -> Get s)
    with _ -> try
      Scanf.sscanf line "GET %s HTTP/1.0\r" (fun s -> Get s)
    with _ -> try
      Scanf.sscanf line "POST %s HTTP/1.1\r" (fun s -> Post s)
    with _ ->
      (* galax is sending 1.0 *)
      Scanf.sscanf line "POST %s HTTP/1.0\r" (fun s -> Post s)

  let header_of_line line =
    try
      Scanf.sscanf line "Content-Length: %d\r" (fun s -> ContentLength s)
    with _ -> try 
      Scanf.sscanf line "%[^:]: %[^\r]\r" (fun x y -> Header(x,y))
    with _ ->
      Other line

  let get_payload headers inchan =
    let get_len x y =
      match x,y with
        None,ContentLength s -> Some s
      | Some s1,ContentLength s2 ->
          if s1<>s2 then raise(Failure "Two content lengths") else x
      | _,_ -> x in
    let len = List.fold_left get_len None headers in
    match len with
      None -> None
    | Some l ->
        let buf = String.create l in
        really_input inchan buf 0 l;
        Some(buf)

   let get_header headers n =
     let rec loop x =
       match x with
         Header(x,y)::tl -> if x = n then y else loop tl
       | _::tl -> loop tl
       | [] -> ""
     in
     loop headers

  let rec get_headers inchan =
    try
      let line = input_line inchan in
      if (line = "" or line = "\r") then [] else
      (header_of_line line)::(get_headers inchan)
    with End_of_file -> []

  let get_method inchan = method_of_line(input_line inchan)

  let get_http_request inchan =
    let meth = get_method inchan in
    let headers = get_headers inchan in
    let payload = get_payload headers inchan in
    (meth,headers,payload)

  let send_http_response outchan extra_headers payload =
    let len = String.length payload in
    output_string outchan
      ("HTTP/1.1 200 OK\r\n"^
       "Connection: close\r\n"^
       extra_headers ^
       "Content-Length: ");
    output_string outchan (string_of_int len);
    output_string outchan
      ("\r\n"^
       "\r\n");
    output_string outchan payload;
    flush outchan (* Necessary in thread case, Thread.exit does not flush *)

  let send_html_http_response outchan headers payload =
    send_http_response outchan 
      ((header_content_type "text/html")^headers)
      payload 

  let send_xml_http_response outchan headers payload =
    send_http_response outchan 
      ((header_content_type "application/xhtml+xml")^headers)
      payload 

(************ XQuery RPC ****************)
  let http_query_response symbolic_name host port resource meth query =
    let inetaddr = get_inet_socket_address host port in
    let (inchan,outchan) = Unix.open_connection(inetaddr) in
    let len = String.length query in
    Printf.fprintf outchan
      ("%s %s HTTP/1.1\r\nHost: %s:%d\r\nConnection: close\r\nContent-Length: %d\r\nConnected-Host: %s\r\n\r\n")
      meth resource host port len symbolic_name;
    output_string outchan query;
    flush outchan; (* Necessary *)

    if (ok_response(input_line inchan)) then
      let headers = get_headers inchan in
      let payload = get_payload headers inchan in
      Unix.shutdown_connection inchan;
      close_in inchan;
(* (* This results in Sys_error("Bad file descriptor") *)
      Printexc.print close_out outchan;
 *)
      match payload with
        None -> raise(Failure("http_query_response: no response"))
      | Some res -> res
    else raise(Failure("http_query_response: error"))

  let http_query_async symbolic_name host port resource meth query =
    let inetaddr = get_inet_socket_address host port in
    let (inchan,outchan) = Unix.open_connection(inetaddr) in
    let len = String.length query in
    (* TODO: choose a Content-Type.  text/xquery? *)
    Printf.fprintf outchan
      ("%s %s HTTP/1.1\r\nHost: %s:%d\r\nConnection: close\r\nContent-Length: %d\r\nConnected-Host: %s\r\n\r\n")
      meth resource host port len symbolic_name;
    output_string outchan query;
    flush outchan; (* Necessary *)
    (* Wait for OK even though this is async.  Change to match XMLHTTPRequest ?? *)
    if (ok_response(input_line inchan)) then begin
      Unix.shutdown_connection inchan; close_in inchan
    end
    else begin
      Unix.shutdown_connection inchan;
      close_in inchan;
      raise(Failure("http_query_async: error"))
    end

end (* module HTTP *)
