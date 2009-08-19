(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: glx_jabber.mli,v 1.2 2007/02/01 22:08:46 simeon Exp $ *)

(**
  * Arguments : 
  *             - jid (Jabber ID)
  *             - password (Jabber ID)
  *             - timeout
  *             - verbose mode flag
*)
val get_presence_information : 
  string -> string -> int -> bool -> string
  

