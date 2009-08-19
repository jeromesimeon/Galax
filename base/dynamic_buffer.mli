(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: dynamic_buffer.mli,v 1.2 2007/02/01 22:08:45 simeon Exp $ *)

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
type 'a t


(* Signals that the end of the buffer has been reached. *)
exception Exhausted

(* Creates a new buffer, using the specified (int that order)
   - number of chunks
   - chunk size
   - increment
   - initial value. *)
val make :
  int -> int -> int -> 'a -> 'a t

(* Sets the internal cursor to the specified index position. *)
val position :
  'a t -> int -> unit

(* Returns the value currently pointed at by the internal cursor,
   then increments that cursor. *)
val next :
  'a t -> 'a

(* Adds a value at the end of the buffer. *)
val add :
  'a t -> 'a -> unit

(* Resets the internal cursor to position 0. *)
val reset :
  'a t -> unit

(* True just after creation or after reset has been applied. *)
val is_empty :
  'a t -> bool

(* Returns the index position currently pointed at by the internal cursor. *)
val get_position :
  'a t -> int
