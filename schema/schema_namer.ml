(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: schema_namer.ml,v 1.4 2007/02/01 22:08:53 simeon Exp $ *)

(* Module: Schema_namer
   Description:
     This module gives names to anonymous names.
*)

(* Note: 
    - The fresh names are created in the FS namespace to make sure it
      does not clash with similarly defined user names.
*)

let name_base     = "Anon"
let anonymous_uri = Namespace_builtin.fs_uri
let anonymous_prefix = Namespace_builtin.fs_prefix

type t = Id.id_gen

let create () = Id.create 0

let fresh_name namer = 
  begin
    ignore(Id.next namer);
    let qn =
      (anonymous_prefix, anonymous_uri, name_base ^ (string_of_int (Id.top namer)))
    in
    Namespace_symbols.relem_symbol qn
  end

