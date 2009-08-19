(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: nfa.mli,v 1.3 2007/02/01 22:08:46 simeon Exp $ *)

(* Module Nfa
   Description:
     This module implements Non-deterministic finite States
     Automatas. NFA is a functor parameterized by an letter and a
     state types.
*)

open Printf
open Error

module type NFA =
  sig
    type state
    type letter

    module StateSet : Set.S

    module Alphabet : Set.S
    module StateToTransitionMap : Map.S

    module TransitionMap : Map.S

    type nfa = {
        mutable nfa_states : StateSet.t;
        mutable nfa_alphabet : Alphabet.t;
        mutable nfa_start_state : StateSet.elt option;
        mutable nfa_final_states : StateSet.t;
        mutable nfa_transitions :
          StateSet.t TransitionMap.t StateToTransitionMap.t;
      } 

    val nfa_empty : nfa
    val fresh_nfa_empty : unit -> nfa
    val get_nfa_alphabet : nfa -> Alphabet.t
    val add_letter : nfa -> Alphabet.elt -> unit
    val add_all_letters : nfa -> Alphabet.t -> unit
    val get_nfa_states : nfa -> StateSet.t
    val get_nfa_start_state : nfa -> StateSet.elt option
    val get_nfa_final_states : nfa -> StateSet.t
    val add_state : nfa -> StateSet.elt -> unit
    val set_start_state : nfa -> StateSet.elt -> unit
    val set_final_states : nfa -> StateSet.t -> unit
    val add_final_state : nfa -> StateSet.elt -> unit
    val get_nfa_transitions :
        nfa -> StateSet.t TransitionMap.t StateToTransitionMap.t
    val get_TransitionMap :
        nfa -> StateToTransitionMap.key -> StateSet.t TransitionMap.t
    val get_destStateSet : 'a TransitionMap.t -> TransitionMap.key -> 'a
    val get_destStateSet_s :
        nfa -> StateToTransitionMap.key -> TransitionMap.key -> StateSet.t
    val add_transitions_aux :
        nfa ->
          StateToTransitionMap.key -> Alphabet.elt * StateSet.elt -> unit
    val add_transitions :
        nfa -> StateSet.elt -> (Alphabet.elt * StateSet.elt) list -> unit
    val print_automata :
        nfa -> (Alphabet.elt -> unit) -> (StateSet.elt -> unit) -> unit

    val empty     : nfa -> bool
  end

module MakeNFA (State : Set.OrderedType) (Letter : Set.OrderedType) :
    NFA with
       type state = State.t
       and type letter = Letter.t
       and type StateSet.elt = State.t
       and type Alphabet.elt = Letter.t
       and type StateToTransitionMap.key = State.t
       and type TransitionMap.key = Letter.t


