(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: cursor.ml,v 1.6 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: Cursor
   Description:
     This module implements XQuery 1.0 and XPath 2.0 data model
     sequences as lazy, but destructive cursors.
*)

open Error


(******************)
(* Error messages *)
(******************)

let raise_invalid_cursor () =
  raise (Query (Cursor_Error "Processing an invalid cursor"))

let raise_stream_operation_on_materialized_cursor msg =
  raise (Query (Cursor_Error ("Should not use stream operation \"" ^ msg ^ "\" on materialized cursor")))

let raise_materialized_operation_on_streamed_cursor msg =
  raise (Query (Cursor_Error ("Should not use materialized operation \"" ^ msg ^ "\" on streamed cursor")))


(***********)
(* Cursors *)
(***********)

type 'a cursor_gen =
    (unit -> 'a option) option

type 'a cursor =
    ('a list * 'a cursor_gen) option ref


(********************)
(* Basic operations *)
(********************)

let cursor_next seq =
  incr Conf.countnext;
  match !seq with
  | None          -> raise_invalid_cursor ()
  | Some ([], None) -> 
      seq := None ; raise Stream.Failure
  | Some ([], Some s)  ->
      begin
	let current = s () in
	match current with
	| None ->
	    seq := None ; raise Stream.Failure
	| Some x ->
	    x
      end
  | Some (x :: rest,s) -> seq := Some (rest,s); x

let cursor_junk seq =
  match !seq with
  | None               -> raise_invalid_cursor ()
  | Some ([],None)     -> seq := None
  | Some ([],Some s)   ->
      begin
	match (s ()) with
	| None -> seq := None
	| _ -> ()
      end
  | Some (x :: rest,s) -> seq := Some (rest,s)

let cursor_peek seq =
  let empty_seq = Some ([],None) in
    match !seq with
      | None            -> raise_invalid_cursor ()
      | Some ([],None)  -> None
      | Some ([],Some s)     ->
	  begin
	    let s_ret = s () in
	      begin
		match s_ret with
		  | None ->
		      seq := empty_seq; 
		  | Some x ->
		      seq := Some ([x],Some s)
	      end;
		s_ret
	  end
      | Some (x :: _,_) -> Some x

(* Note:
      The following function is tail-recursive.
      Keep internal for now to make sure we do not use it outside of
      this module.
      - Jerome *)

let rec streamed_npeek_generate n s =
  if n <= 0
  then ([],Some s)
  else
    let current = s () in
    begin
      match current with
      | None -> ([],None)
      | Some x ->
	  let (rest,s') = streamed_npeek_generate (n-1) s in
	  (x :: rest,s')
    end

let rec streamed_npeek n (list,s) =
  if n <= 0
  then (list,[],s)
  else
    begin
      match (list,s) with
      | ([],None) ->
	  (list,[],None)
      | ([],Some s) ->
	  let (new_buffer,s') = streamed_npeek_generate n s in
          (list@new_buffer,new_buffer,s')
      | (x :: rest,_) ->
	  let (new_list,buffer,s') = streamed_npeek (n-1) (rest,s) in
	  (x :: new_list, x :: buffer,s')
    end

let cursor_npeek n seq =
  match !seq with
  | None          -> raise_invalid_cursor ()
  | Some (list,s) ->
      let (newlist,buffer,s') = streamed_npeek n (list,s) in
      begin
	seq := Some (newlist,s');
	buffer
      end

(***********************)
(* Cursor Constructors *)
(***********************)

(* GENERAL *)

let cursor_of_list l     = ref (Some (l, None))
let cursor_of_function f = ref (Some ([], Some f))
let cursor_of_stream s =
  let next_fun () =
    match Stream.peek s with
    | None -> None
    | Some x -> Stream.junk s; Some x
  in
  cursor_of_function next_fun

(* BASIC *)

let cursor_empty ()       = cursor_of_list []
let cursor_of_singleton x = cursor_of_list [x]
let cursor_of_option oo   =
  match oo with
  | None -> cursor_empty ()
  | Some i -> cursor_of_singleton i

(* Switched from/to materialized and streamed cursors *)

(* Note: The following seems more efficient, eventhough it iterates
   over the list twice through the rev function. One advantage is that
   rev is tail recursive. - Jerome
*)

(* For debugging
let counter = ref 0
*)

let list_size_counter = ref 0

let list_of_cursor msg seq =
  match !seq with
  | None -> raise_invalid_cursor ()
  | Some (list,None) ->
      seq := None; list
  | Some (list,Some s) ->
      begin
	let current = s () in
	match current with
          (* Optimization in case the list is already materialized *)
	  (* Note:
               This is an important optimization as in many cases, we
               build a cursor from a list. That avoids doing
               streaming/rematerialization of the same thing.
	     - Jerome
	   *)
	| None ->
	    seq := None; list
	      (* Default case *)
	| Some x ->
	    if !Conf.print_materialize then
	      begin
		list_size_counter := 1;
		Printf.printf "[MATERIALIZE] FROM A CURSOR TO A LIST "
	      end;
	    let rec mat l =
	      match s () with
	      | None -> seq := None ; l
	      | Some x ->
		  if !Conf.print_materialize then
		    incr list_size_counter;
		  mat (x :: l)
	    in
	    if !Conf.print_materialize
	    then
	      begin
		Printf.printf "OF SIZE %i\n" !list_size_counter;
		Printf.printf "\t--> IN FUNCTION %s\n" msg;
		flush stdout
	      end;
	    List.rev (mat (x :: (List.rev list)))
      end

let rev_list_of_cursor seq =
  match !seq with
  | None -> raise_invalid_cursor ()
  | Some (list,None) ->
      seq := None ; List.rev list
  | Some (list,Some s) ->
      let rec mat l =
	match s () with
	| None -> seq := None ; l
	| Some x ->
	    mat (x :: l)
      in
      mat (List.rev list)

(********************)
(* Cursor Accessors *)
(********************)

(* BASIC / NON DESTRUCTIVE *)

let cursor_is_empty seq =
  match cursor_peek seq with
  | None -> true
  | _ -> false

let cursor_get_singleton seq =
  match cursor_npeek 2 seq with
  | [x] ->
      x
  | _ ->
      raise (Query (Cursor_Error ("Was expecting a singleton cursor")))

let cursor_is_singleton seq =
  match cursor_npeek 2 seq with
  | [x] ->
      true
  | _ ->
      false

let cursor_get_optional seq =
  match cursor_npeek 2 seq with
  | [] ->
      None
  | [x] ->
      Some x
  | _ ->
      raise (Query (Cursor_Error "Was expecting a singleton cursor or empty"))

let cursor_is_optional seq =
  match cursor_npeek 2 seq with
  | [] ->
      true
  | [x] ->
      true
  | _ ->
      false

let cursor_cons first rest =
  match !rest with
  | None          -> raise_invalid_cursor ()
  | Some (list,s) -> ref (Some (first :: list, s))

(* ITERATORS / DESTRUCTIVE *)

let cursor_append seq1 seq2 =
  match (!seq1,!seq2) with
  | (None,_) | (_,None) -> raise_invalid_cursor ()
  | _ ->
      let current_cursor = ref seq1 in
      let next_cursor = ref (Some seq2) in
      let rec append_fun () =
	match cursor_peek (!current_cursor) with
	| None ->
	    begin
	      cursor_junk !current_cursor;
	      match !next_cursor with
	      | None -> None
	      | Some c ->
		  begin
		    current_cursor := c;
		    next_cursor := None;
		    append_fun ()
		  end
	    end
	| Some x ->
	    begin
	      cursor_junk !current_cursor;
	      Some x
	    end
      in
      cursor_of_function append_fun

let cursor_map f seq =
  match !seq with
  | None -> raise_invalid_cursor ()
  | Some (list,s) ->
      let map_fun x =
	begin
	  match cursor_peek seq with
	  | None -> cursor_junk seq; None
	  | Some x -> cursor_junk seq; Some (f x)
	end
      in
      cursor_of_function map_fun

let rec stream_iter f s =
  match s() with
  | None -> ()
  | Some x -> f x; stream_iter f s

let cursor_iter f seq =
  match !seq with
  | None -> raise_invalid_cursor ()
  | Some (list,None) ->
      begin
	List.iter f list;
	seq := None
      end
  | Some (list,Some s) ->
      begin
	List.iter f list;
	stream_iter f s;
	seq := None
      end

let cursor_map_concat f seq =
  match cursor_peek seq with
  | None ->
      cursor_empty ()
  | Some c ->
      cursor_junk seq;
      let current_stream =
	ref (f c)
      in
      let rec map_concat_fun () =
	match cursor_peek !current_stream with
	| None ->
	    begin
	      match cursor_peek seq with
	      | None -> cursor_junk seq ; None
	      | Some x ->
		  begin
		    cursor_junk seq;
		    current_stream := f x;
		    map_concat_fun ()
		  end
	    end
	| x ->
	    begin
	      cursor_junk !current_stream;
	      x
	    end
      in
      cursor_of_function map_concat_fun

let cursor_filter f seq =
  let filter_fun () =
    let res = ref None in
    while
      match cursor_peek seq with
      | None ->
	  begin
	    cursor_junk seq;
	    res := None;
	    false
	  end
      | Some next_item ->
	  if (f next_item)
	  then
	    begin
	      cursor_junk seq;
	      res := (Some next_item);
	      false
	    end
	  else
	    true
    do
      cursor_junk seq
    done;
    !res
  in
  cursor_of_function filter_fun

(* Note:
     The following is tail-recursive.
   - Jerome *)

let rec cursor_fold_left f root seq =
  match cursor_peek seq with
  | None -> root
  | Some n ->
      begin
	cursor_junk seq;
	cursor_fold_left f (f root n) seq
      end

(* Note:
     The following is tail-recursive.
   - Jerome *)

let rec cursor_exists f seq =
  match cursor_peek seq with
  | None -> false
  | _ ->
      if (f (cursor_next seq))
      then
	true
      else
	cursor_exists f seq

let rec cursor_find f seq =
  match cursor_peek seq with
  | None -> raise Not_found
  | _ ->
      let i = cursor_next seq in
      if (f i)
      then
	i
      else
	cursor_find f seq

(* Note:
     The following is tail-recursive.
   - Jerome *)

let rec cursor_for_all f seq =
  match cursor_peek seq with
  | None -> true
  | _ ->
      if (f (cursor_next seq))
      then
	cursor_for_all f seq
      else
	false

(* Note:
     The following is tail-recursive.
   - Jerome *)

let rec cursor_for_all2 f seq1 seq2 =
  match (cursor_peek seq1,cursor_peek seq2) with
  | (None,None) -> true
  | (Some _, Some _) ->
      if (f (cursor_next seq1) (cursor_next seq2))
      then
	cursor_for_all2 f seq1 seq2
      else
	false
  | _ ->
      raise (Invalid_argument "Cursor.cursor_for_all2")

let cursor_length seq =
  let counter = ref 0 in
  cursor_iter (fun x -> incr counter) seq;
  !counter

let cursor_first seq =
  match cursor_peek seq with
  | None ->
      cursor_empty()
  | Some n ->
      cursor_of_singleton n

let cursor_subsequence2 c startingLoc =
  if startingLoc > 0
  then
    begin
      try
	for i = 1 to startingLoc-1 do
	  match cursor_peek c with
	  | None -> raise Not_found
	  | Some _ ->
	      cursor_junk c
	done
      with
      | Not_found -> ()
    end
  else
    ();
  c

let cursor_subsequence3 c startingLoc length =
  let (startingLoc,length) =
    if startingLoc >= 1
    then (startingLoc,length)
    else (1,length+startingLoc-1)
  in
  let _ = cursor_subsequence2 c startingLoc in
  let current_item = ref length in
  let cursor_subsequence3_fun x =
    if !current_item > 0
    then
      begin
	match cursor_peek c with
	| None -> None
	| Some x ->
	    begin
	      decr current_item;
	      Some (cursor_next c)
	    end
      end
    else
      None
  in
  cursor_of_function cursor_subsequence3_fun

(* Note:
     The following is tail-recursive.
   - Jerome *)

let cursor_last seq =
  match cursor_peek seq with
  | None ->
      cursor_empty()
  | Some first ->
      let current = ref (Some first) in
      let rec last_fun x =
	match !current with
	| None -> None
	| Some n ->
	    begin
	      match cursor_peek seq with
	      | None -> current := None ; Some n
	      | Some n -> cursor_junk seq ; current := Some n ; last_fun x
	    end
      in
      cursor_of_function last_fun

(* Once a cursor returns None it should be dropped from the list. *)
(*
let cursor_list_fold sl =
  let rec poll_streams sl ct = 
    match sl with 
    | [] -> None
    | s :: sl' -> 
	begin
	  match cursor_peek s with 
	  | None -> poll_streams sl' ct
	  | Some e -> Some (cursor_next s)
	end
  in cursor_of_function (poll_streams sl) 


let cursor_array_fold a = 
  let a_len = Array.length a in  
  let poll_streams ct =
    let result = ref a.(0) in (* just hold it *)
    let i = ref 0 in
    let bFound = ref false in
      while (!i < a_len) && (not !bFound) do
	begin
	  match cursor_peek a.(!i) with
	    | None -> ()
	    | Some e -> 
		result := a.(!i);
		bFound := true
	end;
	  incr i
      done;

      if (!bFound) then
	Some (cursor_next !result)
      else
	None
  in
    if a_len < 1 then
      cursor_of_function (fun x -> None)
    else
      cursor_of_function poll_streams 
*)	

let cursor_array_fold a = 
  let a_len        = Array.length a in  
  let current_stream_index = ref 0 in

  let poll_streams ct =
    let bFound = ref false in
      while (!current_stream_index < a_len) 
	&& (not !bFound) do
	begin
	  match cursor_peek a.(!current_stream_index) with
	    | None -> incr current_stream_index 
	    | Some e -> bFound := true (* terminate, and return *)
	end;	  
      done;      
      if (!bFound) 
      then Some (cursor_next a.(!current_stream_index))
      else None
  in
    cursor_of_function poll_streams 

let cursor_list_fold sl = cursor_array_fold (Array.of_list sl)


(* Caml stream of a cursor *)

let stream_of_cursor c =
  let f n =
    let x = cursor_peek c in
    cursor_junk c; x
  in
  Stream.from f

