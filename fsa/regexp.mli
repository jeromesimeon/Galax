(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: regexp.mli,v 1.5 2007/02/01 22:08:46 simeon Exp $ *)

(* Module Regexp
   Description:
     This module implements basic regular expressions over an alphabet
     of letters, and the Glushkov construction for both NFA's and
     DFA's over the same alphabet. The Glushkov module is a functor
     parameterized by a letter types.
*)

open Occurrence
open Error
open Nfa
open Dfa


(***********************)
(* Regular expressions *)
(***********************)

module type RegexpType =
  sig
    (* Positions within a regular expression *)
    type position = int    (* a symbol is denoted with its position
			      - each symbol as a unique position *)

    module Position : Set.OrderedType with type t = position

    module PositionSet : Set.S with type elt = position

    type letter

    (* Regular expressions *)

    type regexp =
      | RSym of (letter * position)
      | RSeq of (regexp * regexp)
      | REmpty
      | RChoice of (regexp * regexp)
      | RNone
      | RPlus of regexp
      | RStar of regexp
      | RAmp of (regexp * regexp)

    (* helper function on Glushkov construction *)

    val nullable : regexp -> bool

    (* returns a set of position of the expression *)

    val pos : regexp -> PositionSet.t

    val first : regexp -> PositionSet.t
    val last : regexp -> PositionSet.t
    val follow : regexp -> position -> PositionSet.t

    (* the chi function: it takes a regular expression and a position (integer). *)
    (* returns the label at that position. *)

    val chi : regexp -> position -> letter

    (* Creates a regular expression in star normal form *)
    (* it's a two step process *)

    val star_normalize_aux : regexp -> regexp
    val star_normalize : regexp -> regexp

  end

module MakeRegexpType (Letter : Set.OrderedType) :
    RegexpType with type letter = Letter.t

module type GlushkovType =
  sig
    module Regexp : RegexpType
    module NFA : NFA
    module DFA : DFA

    module NFAPair : NFA

    val build_glushkov : Regexp.regexp -> NFA.nfa
    val build_dtm_glushkov : Regexp.regexp -> DFA.dfa

    val negates_in_alphabet_space : DFA.dfa -> DFA.Alphabet.t -> DFA.dfa

    val intersects : NFA.nfa -> NFA.nfa -> NFAPair.nfa
    val intersects_dtm : NFA.nfa -> DFA.dfa -> NFAPair.nfa
    val print_intersection :
	(NFAPair.Alphabet.elt -> unit) ->
	  (NFAPair.StateSet.elt -> unit) -> NFAPair.nfa -> unit
  end

module MakeGlushkovType (Letter : Set.OrderedType) :
    GlushkovType with type Regexp.letter = Letter.t
       and type NFA.letter = Letter.t
       and type NFA.Alphabet.elt = Letter.t
       and type NFA.StateToTransitionMap.key = int
       and type NFA.TransitionMap.key = Letter.t
       and type NFA.StateSet.elt = int
       and type DFA.letter = Letter.t
       and type DFA.Alphabet.elt = Letter.t
       and type DFA.StateToTransitionMap.key = int
       and type DFA.TransitionMap.key = Letter.t
       and type DFA.StateSet.elt = int
       and type NFAPair.letter = Letter.t
       and type NFAPair.Alphabet.elt = Letter.t
       and type NFAPair.StateToTransitionMap.key = int * int
       and type NFAPair.TransitionMap.key = Letter.t
       and type NFAPair.StateSet.elt = int * int

