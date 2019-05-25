(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: pool.ml,v 1.10 2007/02/01 22:08:45 simeon Exp $ *)

(* Module: Pool
   Description:
     This module implements string pools. Those are bidirectional
     mappings from strings to integers, used to save space in the
     representation of XML documents.
*)

(* Note:
     Name pools are higher order modules parameterized by a
     Hashable type.
   - Jerome and Byron *)

(* Signature of the NamePool modules *)

module type NamePool =
  sig
    type name
    type symbol = int

    type namepool

    val create_pool : unit -> namepool
    val init_pool : namepool -> unit

    val get_name  : namepool -> symbol -> name
    val add_name  : namepool -> name -> symbol

    val exists_name : namepool -> name -> bool
    val symbol_equals : namepool -> symbol -> symbol -> bool

    val pool_size : namepool -> (int * int * int)
  end

(* Functor to create new name pool modules for an given hashed type *)

(* Note:
     The input of the function is just a hashble type and it returns a
     new name pool whose name type is the original hashable type.
   - J&B *)

module MakeNamePool (H: Hashtbl.HashedType) : (NamePool with type name = H.t) =
  struct
    type name = H.t            (* Type of names *)
    type name_o = name option  (* Type of optional names *)
    type symbol = int          (* Type of symbols in the name pool *)

    type namepool =
	{ name_table : (name, symbol) Hashtbl.t;      (* Table from names to symbols *)
	  mutable inverse_table : name_o array;       (* Table from symbols to names *)
	  sym_counter : Id.id_gen }                   (* Symbol counter *)

    let create_pool () =
      { name_table = Hashtbl.create 1439;
	inverse_table = (Array.make 100 None);
	sym_counter = Id.create 0 }

    let init_pool np =
      Hashtbl.clear np.name_table;
      np.inverse_table <- (Array.make 100 None);
      Id.init np.sym_counter 0

    let put_inverse_mapping np v = (* Adds a new symbol in the inverse table *)
      let sym = Id.next np.sym_counter in
      if sym >= Array.length np.inverse_table
      then
	np.inverse_table <- Array.append np.inverse_table (Array.make 100 None)
      else
	();
      (np.inverse_table).(sym) <- Some v;
      sym

    let get_name np sym =                     (* Returns a name from a symbol *)
      match (np.inverse_table).(sym) with
      |	None ->
	  raise Not_found
      |	Some nm ->
	  nm

    let add_name np v =                       (* Adds a new name,symbol binding *)
      try
	Hashtbl.find np.name_table v
      with
      | _ ->
	  begin
	    let sym = put_inverse_mapping np v in
	    Hashtbl.add np.name_table v sym;
	    sym
	  end

    let exists_name np v =                    (* Returns true is a name exists in the name pool *)
      Hashtbl.mem np.name_table v

    let symbol_equals np s1 s2 = (s1 = s2)    (* Compares two symbols *)

    let pool_size np =
      let hashtbl_size ht =
	let size = ref 0 in
	Hashtbl.iter (fun x y -> size := !size+1) ht;
	!size
      in
      (Id.top np.sym_counter,
       hashtbl_size np.name_table,
       Array.length np.inverse_table)
  end
