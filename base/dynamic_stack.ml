(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: dynamic_stack.ml,v 1.3 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Dynamic_stack
     Implements a dynamic stack with direct access to the elements
     needed for e.g. twig joins

 *)

open Error

let initial_stack_size = 16 (* in twigjoin, the size of the stack is bounded by the depth of the tree *)

type 'a dynamic_stack = {
    mutable capacity: int;
    mutable size : int;
    mutable stack : 'a array;
    default : 'a;
  }

let make c x =
  {capacity = c;
   size = 0;
   stack = Array.make initial_stack_size x;
   default = x;
  }

let grow_stack s =
  if s.capacity = 0 then
    begin
      s.capacity <- initial_stack_size;
      s.stack <- Array.make initial_stack_size s.default;
    end
  else
    begin
      s.capacity <- s.capacity * 2;
      let new_stack = Array.make s.capacity s.default in
      Array.blit s.stack 0 new_stack 0 s.size;
      s.stack <- new_stack;
    end
  
let push stack item =
    if stack.size = stack.capacity then
      grow_stack stack;
    stack.stack.(stack.size) <- item;
    stack.size <- stack.size +1

let empty stack = (stack.size = 0)

let pop stack =
  if empty stack then
    raise (Query(Internal_Error("Empty stack exception (dynamic_stack)")))
  else
    begin
      let return_item = stack.stack.(stack.size -1) in
      stack.size <- stack.size -1;
      return_item
    end

let top stack =
  if empty stack then
    raise (Query(Internal_Error("Empty stack exception (dynamic_stack)")))
  else
    stack.stack.(stack.size -1)

