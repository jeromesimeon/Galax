(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                             XQuery Engine                           *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: subtyping_glushkov.mli,v 1.3 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Subtyping_glushkov
   Description:
     This module implements type inclusion and intersection, by
     compiling regular expressions into automatas using the glushkov
     construction.
*)

open Xquery_common_ast

module OrderedNamePairs : Set.OrderedType with type t = Subtyping_letter.actual_letter

module TypeName_Glushkov :
    Regexp.GlushkovType with type Regexp.letter = OrderedNamePairs.t
    and type NFA.letter = OrderedNamePairs.t
    and type NFA.Alphabet.elt = OrderedNamePairs.t
    and type NFA.StateToTransitionMap.key = int
    and type NFA.TransitionMap.key = OrderedNamePairs.t
    and type NFA.StateSet.elt = int
    and type DFA.letter = OrderedNamePairs.t
    and type DFA.Alphabet.elt = OrderedNamePairs.t
    and type DFA.StateToTransitionMap.key = int
    and type DFA.TransitionMap.key = OrderedNamePairs.t
    and type DFA.StateSet.elt = int
    and type NFAPair.letter = OrderedNamePairs.t
    and type NFAPair.Alphabet.elt = OrderedNamePairs.t
    and type NFAPair.StateToTransitionMap.key = int * int
    and type NFAPair.TransitionMap.key = OrderedNamePairs.t
    and type NFAPair.StateSet.elt = int * int

