(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: args.ml,v 1.10 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Args
   Description:
     This module implements extraction of function arguments for
     various arities.
*)

open Error

(************************)
(* Arguments extraction *)
(************************)

let get_param0 =
  function
    | [] -> ()
    | _ -> raise (Query (Parameter_Mismatch("Expected zero arguments")))

let get_param1 =
  function
    | [x1] -> x1
    | _ -> raise (Query (Parameter_Mismatch("Expected one argument")))

let get_param2 =
  function
    | [x1;x2] -> (x1,x2)
    | _ -> raise (Query (Parameter_Mismatch("Expected two arguments")))

let get_param3 =
  function
    | [x1;x2;x3] -> (x1,x2,x3)
    | _ -> raise (Query (Parameter_Mismatch("Expected three arguments")))

let get_param4 =
  function
    | [x1;x2;x3;x4] -> (x1,x2,x3,x4)
    | _ -> raise (Query (Parameter_Mismatch("Expected four arguments")))

let get_array_param0 x =
  if (Array.length x) != 0 then
    raise (Query (Parameter_Mismatch("Expected array argument of length zero")))

let get_array_param1 x =
  if (Array.length x != 1) then
    raise (Query (Parameter_Mismatch("Expected array argument of length one")))
  else
    x.(0)

let get_array_param2 x =
  if (Array.length x != 2) then
    raise (Query (Parameter_Mismatch("Expected array argument of length two")))
  else
    x.(0), x.(1)


let get_array_param3 x =
  if (Array.length x != 3) then
    raise (Query (Parameter_Mismatch("Expected array argument of length three")))
  else
    x.(0), x.(1), x.(2)


let get_array_param4 x =
  if (Array.length x != 4) then
    raise (Query (Parameter_Mismatch("Expected array argument of length four")))
  else
    x.(0), x.(1), x.(2), x.(3)
