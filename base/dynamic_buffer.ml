(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: dynamic_buffer.ml,v 1.6 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Dynamic_buffer
   Description:

     This module implements a buffer that automatically grows (if need
     be) when adding new elements. The buffer itself is composed of an
     array of arrays (chunks) the index space of which is exposed to
     the outside world as being linear.

     The buffer maintains a single internal cursor for keeping track
     of the current position; usage should adhere to the following
     protocol:

     - create the buffer
     - add values to the buffer
     - position the cursor, read values etc.
     - reset the buffer, then start again

     The allocated buffer space will never shrink (even in case of a
     reset). This behaviour is intentional.

   - Michael *)


(* The structure holding the actual data. *)
type 'a t = {
  mutable buffer : 'a array array;
  mutable chunk_size : int;
  mutable num_chunks : int;
  mutable increment : int;
  mutable buffer_index : int;
  mutable chunk_index : int;
  mutable buffer_last_index : int;
  mutable chunk_last_index : int;
}


(* Signals that the end of the buffer has been reached. *)
exception Exhausted

(* Creates a new buffer, using the specified (in that order)
   - initial number of chunks
   - chunk size
   - increment
   - initial value. *)
let make n s inc x =
(*
  let chunk = Array.make s x in
  let buff = Array.make n chunk in
    for i = 0 to n - 1 do
      let chunk = Array.make s x in
	buff.(i) <- chunk
    done;
*)
  (* Michael 05|14|2006 *)
  let chunk = [||] in
  let buff = Array.make n chunk in
    for i = 0 to n - 1 do
      let chunk = Array.make s x in
	buff.(i) <- chunk
    done;
    {
      buffer = buff;
      chunk_size = s;
      num_chunks = n;
      increment = inc;
      buffer_index = 0;
      chunk_index = 0;
      buffer_last_index = -1;
      chunk_last_index = -1;
    }

(* Resets the internal cursor to position 0. *)
let reset t =
  t.buffer_index <- 0;
  t.chunk_index <- 0;
  t.buffer_last_index <- -1;
  t.chunk_last_index <- -1

(* Initializes the buffer with one blank chunk. *)
let init t x =
  let s = t.chunk_size in
(*
  let _ = print_string "init buffer with nchunks, csize, inc:\n " in
  let _ = print_int t.num_chunks in
  let _ = print_string " " in
  let _ = print_int s in
  let _ = print_string " " in
  let _ = print_int t.increment in
  let _ = print_string "\n" in
*)
  let chunk = Array.make s x in
  let buff = Array.make 1 chunk in
    t.buffer <- buff;
    t.num_chunks <- 1;
    reset t

let is_physical_index_valid t bi ci =
  ((bi < t.buffer_last_index) && (ci < t.chunk_size)) || ((bi = t.buffer_last_index) && (ci <= t.chunk_last_index))

let get_physical_index t i =
  let bi = i / t.chunk_size in
  let ci = i mod t.chunk_size in
    if is_physical_index_valid t bi ci
    then
      (bi, ci)
    else
      raise (Invalid_argument ("In Dynamic_buffer.get_physical_index: index "^(string_of_int i)^" out of bounds"))

(* Sets the internal cursor to the specified index position. *)
let position t i =
    let (bi, ci) = get_physical_index t i in
      t.buffer_index <- bi;
      t.chunk_index <- ci

let increment_physical_index t =
  if t.chunk_index < (t.chunk_size - 1)
  then
    t.chunk_index <- (t.chunk_index + 1)
  else
    (* unsafe *)
    begin
      t.buffer_index <- (t.buffer_index + 1);
      t.chunk_index <- 0
    end
 
(* Returns the value currently pointed at by the internal cursor,
   then increments that cursor. *)
let next t =
  let bi = t.buffer_index in
  let ci = t.chunk_index in
    if is_physical_index_valid t bi ci
    then
      begin
	let result = t.buffer.(bi).(ci) in
	let _ = increment_physical_index t in
	  result
      end
    else
      raise Exhausted
      
let resize t x =
  (* Remember to initialize every single element of the array with a distinct value! *)
  let chunk = Array.make t.chunk_size x in

  (* Michael 05|14|2006 *)
  let num_chunks = t.num_chunks * t.increment in
  (*let num_chunks = t.num_chunks + t.increment in*)
(*
  let _ = print_string "resize buffer with nchunks, csize, inc:\n " in
  let _ = print_int num_chunks in
  let _ = print_string " " in
  let _ = print_int t.chunk_size in
  let _ = print_string " " in
  let _ = print_int t.increment in
  let _ = print_string "\n" in
*)
  let buff = Array.make num_chunks chunk in
    begin
      for i = t.num_chunks to (num_chunks - 1)
      do
	let chunk = Array.make t.chunk_size x in
	  buff.(i) <- chunk
      done;
      Array.blit t.buffer 0 buff 0 t.num_chunks;
      t.num_chunks <- num_chunks;
      t.buffer <- buff
    end
     
(* Adds a value at the end of the buffer. *)
let add t a =
  (* Michael 05|14|2006 *)
  if (t.num_chunks = 0)
  then init t a;

  let bi = t.buffer_index in
  let ci = t.chunk_index in
    t.buffer.(bi).(ci) <- a;
    t.buffer_last_index <- bi;
    t.chunk_last_index <- ci;
    begin
      if t.chunk_index < (t.chunk_size - 1)
      then
	t.chunk_index <- (ci + 1)
      else
	begin
	  if not (t.buffer_index < (t.num_chunks - 1))
	  then
	    resize t a;
	  t.buffer_index <- (bi + 1);
	  t.chunk_index <- 0
	end
    end   
      
(* Resets the internal cursor to position 0. *)
let reset t =
  t.buffer_index <- 0;
  t.chunk_index <- 0;
  t.buffer_last_index <- -1;
  t.chunk_last_index <- -1

(* True just after creation or after reset has been applied. *)
let is_empty t =
  (t.buffer_last_index < 0) && (t.chunk_last_index < 0)

(* Returns the index position currently pointed at by the internal cursor. *)
let get_position t =
  (t.buffer_index * t.chunk_size) + t.chunk_index

