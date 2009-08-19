(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: optimization_rules_sbdo_automaton.ml,v 1.3 2007/02/01 22:08:51 simeon Exp $ *)

(* Module: Optimization_rules_sbdo_automaton
   Description:
     This module implements the 3 automata that annotate the AST with
     DDO annotations for the benefits of the corresponding rewriting
     rules.
*)

open Printf
open Processing_context

(* The first integer is the state, the second integer is the substate i *)

type state_number = int
type state        = state_number * (int * int)

type action_needed =
| Nothing
| Docorder
| Distinct
| DistinctDocorder

(*****************************************************************************)
(* The axis constants as used in the automata                                *)
(*****************************************************************************)

type axis_number = int

let axis_child               = 0
let axis_descendant          = 1
let axis_descendant_or_self  = 2
let axis_following           = 3
let axis_following_sibling   = 4
let axis_parent              = 5
let axis_ancestor            = 6
let axis_ancestor_or_self    = 7
let axis_preceding           = 8
let axis_preceding_sibling   = 9
let axis_self                = 10

let axis_adapter (a:Xquery_common_ast.axis) =
  match a with
  | Xquery_common_ast.Ancestor            -> axis_ancestor
  | Xquery_common_ast.Ancestor_or_self    -> axis_ancestor_or_self
  | Xquery_common_ast.Attribute           -> axis_child
  | Xquery_common_ast.Child               -> axis_child
  | Xquery_common_ast.Descendant          -> axis_descendant
  | Xquery_common_ast.Descendant_or_self  -> axis_descendant_or_self
  | Xquery_common_ast.Following_sibling   -> axis_following_sibling
  | Xquery_common_ast.Preceding_sibling   -> axis_preceding_sibling
  | Xquery_common_ast.Parent              -> axis_parent
  | Xquery_common_ast.Self                -> axis_self

  | Xquery_common_ast.Preceding           -> axis_preceding
  | Xquery_common_ast.Following           -> axis_following


type index_change =
| Preserve
| IncrementFirst
| DecrementFirst
| IncrementSecond
| DecrementSecond
| IncrementDecrement
| IncrementBoth
| DecrementBoth


(*****************************************************************************)
(*********                 TIDY AUTOMATON                         ************)
(*****************************************************************************)

(*****************************************************************************)
(* The constants for the different states of the tidy automaton              *)
(*                                                                           *)
(*   0: lin(i+1), ~no2d(>=0), ~unrel, nsib(<=i), nhat(<=i)                   *)
(*   1: lin(i+1), nolc(i), ~no2d(>=0), ~unrel, nsib(i), ~norc(<=i),          *)
(*             [i > 0] nhat(<=i-1)                                           *)
(*   2: lin(i+1), nolc(i), unrel(i), ~no2d(>=0), nsib(<=i), ~norc(i),        *)
(*             [i > 0] norc(<=i-1)                                           *)
(*   3: lin(i+1), norc(i), nolc(i), ~unrel, ~no2d(>=0), nsib(i),             *)
(*             [i > 0] nhat(<=i-1)                                           *)
(*   4: lin(i+1), norc(<=i), unrel(i), ~no2d(>=0), nsib(<=i), ~nolc(i)       *)
(*   5: lin(i+1), norc(i), nolc(<=i), ~unrel, ~no2d(>=0), nsib(i), ~nolc(i)  *)
(*             [i > 0] nhat(<=i-1)                                           *)
(*   6: no2d(i+1), nsib(<=i)                                                 *)
(*   7: lin, ~unrel, ~no2d(>=0)                                              *)
(*   8: no2d                                                                 *)
(*   9: ntree                                                                *)
(*****************************************************************************)

let state_t_l     = 0
let state_t_lnl   = 1
let state_t_lnlu  = 2
let state_t_lnrl  = 3
let state_t_lnru  = 4
let state_t_lnr   = 5
let state_t_nn    = 6
let state_t_lin   = 7
let state_t_no2d  = 8
let state_t_ntree = 9

(*****************************************************************************)
(* Two transition tables for the automaton, which one is used depends on     *)
(* the substate index i                                                      *)
(*                                                                           *)
(* States of the automaton: see state constants                              *)
(*                                                                           *)
(* Rows in transition_matrices are in this order                             *)
(* Note: States 8-10 do not appear in transition matrix for i>0, since they  *)
(*       have no substates                                                   *)
(* Note: 9 is the initial state                                              *)
(*                                                                           *)
(* Axes: see axis constants                                                  *)
(*                                                                           *)
(* Columns in transition_matrices are in this order                          *)
(*                                                                           *)
(* [i = 0] Use transition_matrix_0                                           *)
(* [i > 0] Use transition_matrix_1                                           *)
(*                                                                           *)
(* Note: the elements on position (m,n) of the transition matrix are         *)
(*       3-tuples: (q, di, op)                                               *)
(*           - q:  State that we reach when coming from state m with axis n  *)
(*           - di: What we must add to i when coming from state m with axis  *)
(*                 n (remark: 0 is the absolute minimum for i)               *)
(*           - op: What operation is needed when coming from state m with    *)
(*                 axis n                                                    *)
(*****************************************************************************)

let tidy_transition_matrix_0 =
  [|
    [| (* State state_t_l     *) (* Child              *) (state_t_l,      IncrementFirst, Docorder);
                                 (* Descendant         *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Descendant-or-Self *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Following          *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_t_l,      Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_t_lin,    Preserve,       DistinctDocorder);
                                 (* Ancestor           *) (state_t_lin,    Preserve,       DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_t_l,      Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_t_l,      Preserve,       DistinctDocorder) 
    |];
    [| (* State state_t_lnl   *) (* Child              *) (state_t_lnl,    IncrementFirst, Docorder);
                                 (* Descendant         *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Descendant-or-Self *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Following          *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_t_lnlu,   Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_t_lin,    Preserve,       DistinctDocorder);
                                 (* Ancestor           *) (state_t_lin,    Preserve,       DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_t_lnl,    Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_t_l,      Preserve,       DistinctDocorder) 
    |];
    [| (* State state_t_lnlu  *) (* Child              *) (state_t_lnlu,   IncrementFirst, Nothing);
                                 (* Descendant         *) (state_t_ntree,  Preserve,       Nothing);
                                 (* Descendant-or-Self *) (state_t_ntree,  Preserve,       Nothing);
                                 (* Following          *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_t_lnlu,   Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_t_lin,    Preserve,       DistinctDocorder);
                                 (* Ancestor           *) (state_t_lin,    Preserve,       DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_t_lnl,    Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_t_l,      Preserve,       DistinctDocorder) 
    |];
    [| (* State state_t_lnrl  *) (* Child              *) (state_t_lnrl,   IncrementFirst, Docorder);
                                 (* Descendant         *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Descendant-or-Self *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Following          *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_t_lnlu,   Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_t_lin,    Preserve,       Distinct);
                                 (* Ancestor           *) (state_t_lin,    Preserve,       DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_t_lnrl,   Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_t_lnru,   Preserve,       DistinctDocorder) 
    |];
    [| (* State state_t_lnru  *) (* Child              *) (state_t_lnru,   IncrementFirst, Nothing);
                                 (* Descendant         *) (state_t_ntree,  Preserve,       Nothing);
                                 (* Descendant-or-Self *) (state_t_ntree,  Preserve,       Nothing);
                                 (* Following          *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_t_l,      Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_t_lin,    Preserve,       Distinct);
                                 (* Ancestor           *) (state_t_lin,    Preserve,       DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_t_lnr,    Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_t_lnru,   Preserve,       DistinctDocorder) 
    |];
    [| (* State state_t_lnr   *) (* Child              *) (state_t_lnr,    IncrementFirst, Docorder);
                                 (* Descendant         *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Descendant-or-Self *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Following          *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_t_l,      Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_t_lin,    Preserve,       Distinct);
                                 (* Ancestor           *) (state_t_lin,    Preserve,       DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_t_lnr,    Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_t_lnru,   Preserve,       DistinctDocorder) 
    |];
    [| (* State state_t_nn   *)  (* Child              *) (state_t_nn,     IncrementFirst, Nothing);
                                 (* Descendant         *) (state_t_ntree,  Preserve,       Nothing);
                                 (* Descendant-or-Self *) (state_t_ntree,  Preserve,       Nothing);
                                 (* Following          *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_t_nn,     Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_t_no2d,   Preserve,       Distinct);
                                 (* Ancestor           *) (state_t_lin,    Preserve,       DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_t_lnrl,   Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_t_nn,     Preserve,       DistinctDocorder) 
    |];
    [| (* State state_t_lin   *) (* Child              *) (state_t_l,      Preserve,       Docorder);
                                 (* Descendant         *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Descendant-or-Self *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Following          *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_t_lnlu,   Preserve,       Docorder);
                                 (* Parent             *) (state_t_lin,    Preserve,       Nothing);
                                 (* Ancestor           *) (state_t_lin,    Preserve,       DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_t_lin,    Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_t_lnru,   Preserve,       Nothing) 
    |];
    [| (* State state_t_no2d  *) (* Child              *) (state_t_nn,     Preserve,       Nothing);
                                 (* Descendant         *) (state_t_ntree,  Preserve,       Nothing);
                                 (* Descendant-or-Self *) (state_t_ntree,  Preserve,       Nothing);
                                 (* Following          *) (state_t_ntree,  Preserve,       Nothing);
                                 (* Following-Sibling  *) (state_t_nn,     Preserve,       Nothing);
                                 (* Parent             *) (state_t_no2d,   Preserve,       Nothing);
                                 (* Ancestor           *) (state_t_lin,    Preserve,       Nothing);
                                 (* Ancestor-or-Self   *) (state_t_lin,    Preserve,       Nothing);
                                 (* Preceding          *) (state_t_ntree,  Preserve,       Nothing);
                                 (* Preceding-Sibling  *) (state_t_nn,     Preserve,       Nothing) 
    |];
    [| (* State state_t_ntree *) (* Child              *) (state_t_ntree,  Preserve,       Docorder);
                                 (* Descendant         *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Descendant-or-Self *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Following          *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Ancestor           *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_t_ntree,  Preserve,       DistinctDocorder) 
    |]
  |]

let tidy_transition_matrix_1 =
  [|
    [| (* State state_t_l     *) (* Child              *) (state_t_l,      IncrementFirst, Docorder);
                                 (* Descendant         *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Descendant-or-Self *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Following          *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_t_l,      Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_t_l,      DecrementFirst, DistinctDocorder);
                                 (* Ancestor           *) (state_t_l,      DecrementFirst, DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_t_l,      Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_t_l,      Preserve,       DistinctDocorder)
    |];
    [| (* State state_t_lnl   *) (* Child              *) (state_t_lnl,    IncrementFirst, Docorder);
                                 (* Descendant         *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Descendant-or-Self *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Following          *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_t_lnl,    Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_t_lnl,    DecrementFirst, DistinctDocorder);
                                 (* Ancestor           *) (state_t_lnl,    DecrementFirst, DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_t_lnl,    Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_t_lnl,    Preserve,       DistinctDocorder)
    |];
    [| (* State state_t_lnlu  *) (* Child              *) (state_t_lnlu,   IncrementFirst, Nothing);
                                 (* Descendant         *) (state_t_ntree,  Preserve,       Nothing);
                                 (* Descendant-or-Self *) (state_t_ntree,  Preserve,       Nothing);
                                 (* Following          *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_t_lnlu,   Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_t_lnlu,   DecrementFirst, Distinct);
                                 (* Ancestor           *) (state_t_lnl,    DecrementFirst, DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_t_lnl,    Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_t_lnlu,   Preserve,       DistinctDocorder)
    |];
    [| (* State state_t_lnrl  *) (* Child              *) (state_t_lnrl,   IncrementFirst, Docorder);
                                 (* Descendant         *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Descendant-or-Self *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Following          *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_t_lnrl,   Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_t_lnrl,   DecrementFirst, DistinctDocorder);
                                 (* Ancestor           *) (state_t_lnrl,   DecrementFirst, DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_t_lnrl,   Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_t_lnrl,   Preserve,       DistinctDocorder)
    |];
    [| (* State state_t_lnru  *) (* Child              *) (state_t_lnru,   IncrementFirst, Nothing);
                                 (* Descendant         *) (state_t_ntree,  Preserve,       Nothing);
                                 (* Descendant-or-Self *) (state_t_ntree,  Preserve,       Nothing);
                                 (* Following          *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_t_lnru,   Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_t_lnru,   DecrementFirst, Distinct);
                                 (* Ancestor           *) (state_t_lnr,    DecrementFirst, DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_t_lnr,    Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_t_lnru,   Preserve,       DistinctDocorder)
    |];
    [| (* State state_t_lnr   *) (* Child              *) (state_t_lnr,    IncrementFirst, Docorder);
                                 (* Descendant         *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Descendant-or-Self *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Following          *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_t_lnr,    Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_t_lnr,    DecrementFirst, DistinctDocorder);
                                 (* Ancestor           *) (state_t_lnr,    DecrementFirst, DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_t_lnr,    Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_t_lnr,    Preserve,       DistinctDocorder)
    |];
    [| (* State state_t_nn   *) (* Child              *) (state_t_nn,     IncrementFirst, Nothing);
                                (* Descendant         *) (state_t_ntree,  Preserve,       Nothing);
                                (* Descendant-or-Self *) (state_t_ntree,  Preserve,       Nothing);
                                (* Following          *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                (* Following-Sibling  *) (state_t_nn,     Preserve,       DistinctDocorder);
                                (* Parent             *) (state_t_nn,     DecrementFirst, Distinct);
                                (* Ancestor           *) (state_t_lnrl,   DecrementFirst, DistinctDocorder);
                                (* Ancestor-or-Self   *) (state_t_lnrl,   Preserve,       DistinctDocorder);
                                (* Preceding          *) (state_t_ntree,  Preserve,       DistinctDocorder);
                                (* Preceding-Sibling  *) (state_t_nn,     Preserve,       DistinctDocorder) 
    |]
  |]

let tidy_needs_sorting state: bool =
  false

let tidy_needs_dupelim state: bool =
  false

let tidy_init_state = (state_t_no2d,  (0, 0))
let tidy_sink_state = (state_t_ntree, (0, 0))

let tidy_final_state_after_ddo (st: state) = st


(*****************************************************************************)
(**********                     DUPTIDY AUTOMATON                   **********)
(*****************************************************************************)

(*****************************************************************************)
(* The constants for the different states of the duptidy automaton           *)
(*                                                                           *)
(*   0: lin(i+1), ~no2d(>=0), ~unrel, nsib(<=i), nhat(<=i), ord              *)
(*   1: lin(i+1), ~no2d(>=0), ~unrel, nsib(<=i), nhat(<=i), ~ord, ord1       *)
(*   2: lin(i+1), ~no2d(>=0), ~unrel, nsib(<=i), nhat(<=i), ~ord, ~ord1      *)
(*   3: lin(i+1), nolc(i), ~no2d(>=0), ~unrel, nsib(i), ~norc(<=i), ord,     *)
(*             [i > 0] nhat(<=i-1)                                           *)
(*   4: lin(i+1), nolc(i), ~no2d(>=0), ~unrel, nsib(i), ~norc(<=i), ~ord,    *)
(*             ord1, [i > 0] nhat(<=i-1)                                     *)
(*   5: lin(i+1), nolc(i), ~no2d(>=0), ~unrel, nsib(i), ~norc(<=i), ~ord,    *)
(*             ~ord1, [i > 0] nhat(<=i-1)                                    *)
(*   6: lin(i+1), nolc(i), unrel(i), ~no2d(>=0), nsib(<=i), ~norc(i), ord    *)
(*             [i > 0] norc(<=i-1)                                           *)
(*   7: lin(i+1), nolc(i), unrel(i), ~no2d(>=0), nsib(<=i), ~norc(i), ~ord,  *)
(*             ord1, [i > 0] norc(<=i-1)                                     *)
(*   8: lin(i+1), nolc(i), unrel(i), ~no2d(>=0), nsib(<=i), ~norc(i), ~ord,  *)
(*             ~ord1, [i > 0] norc(<=i-1)                                    *)
(*   9: lin(i+1), norc(i), nolc(i), ~unrel, ~no2d(>=0), nsib(i), ord,        *)
(*             [i > 0] nhat(<=i-1)                                           *)
(*  10: lin(i+1), norc(i), nolc(i), ~unrel, ~no2d(>=0), nsib(i), ~ord,       *)
(*             ord1, [i > 0] nhat(<=i-1)                                     *)
(*  11: lin(i+1), norc(i), nolc(i), ~unrel, ~no2d(>=0), nsib(i), ~ord,       *)
(*             ~ord1, [i > 0] nhat(<=i-1)                                    *)
(*  12: lin(i+1), norc(<=i), unrel(i), ~no2d(>=0), nsib(<=i), ~nolc(i), ord  *)
(*  13: lin(i+1), norc(i), nolc(<=i), ~unrel, ~no2d(>=0), nsib(i), ~nolc(i)  *)
(*             ord, [i > 0] nhat(<=i-1)                                      *)
(*  14: lin(i+1), norc(i), nolc(<=i), ~unrel, ~no2d(>=0), nsib(i), ~nolc(i)  *)
(*             ~ord, ord1, [i > 0] nhat(<=i-1)                               *)
(*  15: lin(i+1), norc(i), nolc(<=i), ~unrel, ~no2d(>=0), nsib(i), ~nolc(i)  *)
(*             ~ord, ~ord1, [i > 0] nhat(<=i-1)                              *)
(*  16: no2d(i+1), nsib(<=i)                                                 *)
(*  17: lin, ~unrel, ~no2d(>=0)                                              *)
(*  18: no2d                                                                 *)
(*  19: ntree, ord                                                           *)
(*  20: ntree, ~ord, ord1                                                    *)
(*  21: ntree, ~ord, ~ord1                                                   *)
(*****************************************************************************)

let state_d_l      = 0
let state_d_l1     = 1
let state_d_l2     = 2
let state_d_lnl    = 3
let state_d_lnl1   = 4
let state_d_lnl2   = 5
let state_d_lnlu   = 6
let state_d_lnlu1  = 7
let state_d_lnlu2  = 8
let state_d_lnrl   = 9
let state_d_lnrl1  = 10
let state_d_lnrl2  = 11
let state_d_lnru   = 12
let state_d_lnr    = 13
let state_d_lnr1   = 14
let state_d_lnr2   = 15
let state_d_nn     = 16
let state_d_lin    = 17
let state_d_no2d   = 18
let state_d_ntree  = 19
let state_d_ntree1 = 20
let state_d_ntree2 = 21
(* Philippe, undefined state for handling unsupported syntactical constructs *) 
let state_d_undefined = 22
(*****************************************************************************)
(* Two transition tables for the automaton, which one is used depends on     *)
(* the substate index i                                                      *)
(*                                                                           *)
(* States of the automaton: see state constants                              *)
(*                                                                           *)
(* Rows in transition_matrices are in this order                             *)
(* Note: States 15-19 do not appear in transition matrix for i>0, since they *)
(*       have no substates                                                   *)
(* Note: 16 is the initial state                                             *)
(*                                                                           *)
(* Axes: see axis constants                                                  *)
(*                                                                           *)
(* Columns in transition_matrices are in this order                          *)
(*                                                                           *)
(* [i = 0] Use transition_matrix_0                                           *)
(* [i > 0] Use transition_matrix_1                                           *)
(*                                                                           *)
(* Note: the elements on position (m,n) of the transition matrix are         *)
(*       3-tuples: (q, di, op)                                               *)
(*           - q:  State that we reach when coming from state m with axis n  *)
(*           - di: What we must add to i when coming from state m with axis  *)
(*                 n (remark: 0 is the absolute minimum for i)               *)
(*           - op: What operation is needed when coming from state m with    *)
(*                 axis n                                                    *)
(*****************************************************************************)

let duptidy_transition_matrix_0 =
  [|
    [| (* State state_d_l     *) (* Child              *) (state_d_l1,     IncrementFirst, Nothing);
                                 (* Descendant         *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Descendant-or-Self *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_d_l,      Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_d_lin,    Preserve,       DistinctDocorder);
                                 (* Ancestor           *) (state_d_lin,    Preserve,       DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_d_l,      Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_d_l,      Preserve,       DistinctDocorder) 
    |];
    [| (* State state_d_l1    *) (* Child              *) (state_d_l2,     IncrementFirst, Nothing);
                                 (* Descendant         *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Descendant-or-Self *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_d_l,      Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_d_lin,    Preserve,       Distinct);
                                 (* Ancestor           *) (state_d_lin,    Preserve,       DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_d_l,      Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_d_l,      Preserve,       DistinctDocorder) 
    |];
    [| (* State state_d_l2    *) (* Child              *) (state_d_l2,     IncrementFirst, Nothing);
                                 (* Descendant         *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Descendant-or-Self *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_d_l,      Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_d_lin,    Preserve,       DistinctDocorder);
                                 (* Ancestor           *) (state_d_lin,    Preserve,       DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_d_l,      Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_d_l,      Preserve,       DistinctDocorder) 
    |];
    [| (* State state_d_lnl   *) (* Child              *) (state_d_lnl1,   IncrementFirst, Nothing);
                                 (* Descendant         *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Descendant-or-Self *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_d_lnlu,   Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_d_lin,    Preserve,       DistinctDocorder);
                                 (* Ancestor           *) (state_d_lin,    Preserve,       DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_d_lnl,    Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_d_l,      Preserve,       DistinctDocorder) 
    |];
    [| (* State state_d_lnl1  *) (* Child              *) (state_d_lnl2,   IncrementFirst, Nothing);
                                 (* Descendant         *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Descendant-or-Self *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_d_lnlu,   Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_d_lin,    Preserve,       Distinct );
                                 (* Ancestor           *) (state_d_lin,    Preserve,       DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_d_lnl,    Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_d_l,      Preserve,       DistinctDocorder) 
    |];
    [| (* State state_d_lnl2  *) (* Child              *) (state_d_lnl2,   IncrementFirst, Nothing);
                                 (* Descendant         *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Descendant-or-Self *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_d_lnlu,   Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_d_lin,    Preserve,       DistinctDocorder);
                                 (* Ancestor           *) (state_d_lin,    Preserve,       DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_d_lnl,    Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_d_l,      Preserve,       DistinctDocorder) 
    |];
    [| (* State state_d_lnlu  *) (* Child              *) (state_d_lnlu,   IncrementFirst, Nothing);
                                 (* Descendant         *) (state_d_ntree,  Preserve,       Nothing);
                                 (* Descendant-or-Self *) (state_d_ntree,  Preserve,       Nothing);
                                 (* Following          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_d_lnlu,   Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_d_lin,    Preserve,       DistinctDocorder);
                                 (* Ancestor           *) (state_d_lin,    Preserve,       DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_d_lnl,    Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_d_l,      Preserve,       DistinctDocorder) 
    |];
    [| (* State state_d_lnlu1 *) (* Child              *) (state_d_lnlu2,  IncrementFirst, Nothing);
                                 (* Descendant         *) (state_d_ntree2, Preserve,       Nothing);
                                 (* Descendant-or-Self *) (state_d_ntree2, Preserve,       Nothing);
                                 (* Following          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_d_lnlu,   Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_d_lin,    Preserve,       Distinct);
                                 (* Ancestor           *) (state_d_lin,    Preserve,       DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_d_lnl,    Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_d_l,      Preserve,       DistinctDocorder) 
    |];
    [| (* State state_d_lnlu2 *) (* Child              *) (state_d_lnlu2,  IncrementFirst, Nothing);
                                 (* Descendant         *) (state_d_ntree2, Preserve,       Nothing);
                                 (* Descendant-or-Self *) (state_d_ntree2, Preserve,       Nothing);
                                 (* Following          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_d_lnlu,   Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_d_lin,    Preserve,       DistinctDocorder);
                                 (* Ancestor           *) (state_d_lin,    Preserve,       DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_d_lnl,    Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_d_l,      Preserve,       DistinctDocorder) 
    |];
    [| (* State state_d_lnrl  *) (* Child              *) (state_d_lnrl1,  IncrementFirst, Nothing);
                                 (* Descendant         *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Descendant-or-Self *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_d_lnlu,   Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_d_lin,    Preserve,       Distinct);
                                 (* Ancestor           *) (state_d_lin,    Preserve,       DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_d_lnrl,   Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_d_lnru,   Preserve,       DistinctDocorder) 
    |];
    [| (* State state_d_lnrl1 *) (* Child              *) (state_d_lnrl2,  IncrementFirst, Nothing);
                                 (* Descendant         *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Descendant-or-Self *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_d_lnlu,   Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_d_lin,    Preserve,       Distinct);
                                 (* Ancestor           *) (state_d_lin,    Preserve,       DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_d_lnrl,   Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_d_lnru,   Preserve,       DistinctDocorder) 
    |];
    [| (* State state_d_lnrl2 *) (* Child              *) (state_d_lnrl2,  IncrementFirst, Nothing);
                                 (* Descendant         *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Descendant-or-Self *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_d_lnlu,   Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_d_lin,    Preserve,       DistinctDocorder);
                                 (* Ancestor           *) (state_d_lin,    Preserve,       DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_d_lnrl,   Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_d_lnru,   Preserve,       DistinctDocorder) 
    |];
    [| (* State state_d_lnru  *) (* Child              *) (state_d_lnru,   IncrementFirst, Nothing);
                                 (* Descendant         *) (state_d_ntree,  Preserve,       Nothing);
                                 (* Descendant-or-Self *) (state_d_ntree,  Preserve,       Nothing);
                                 (* Following          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_d_l,      Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_d_lin,    Preserve,       Distinct);
                                 (* Ancestor           *) (state_d_lin,    Preserve,       DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_d_lnr,    Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_d_lnru,   Preserve,       DistinctDocorder) 
    |];
    [| (* State state_d_lnr   *) (* Child              *) (state_d_lnr1,   IncrementFirst, Nothing);
                                 (* Descendant         *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Descendant-or-Self *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_d_l,      Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_d_lin,    Preserve,       Distinct);
                                 (* Ancestor           *) (state_d_lin,    Preserve,       DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_d_lnr,    Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_d_lnru,   Preserve,       DistinctDocorder) 
    |];
    [| (* State state_d_lnr1  *) (* Child              *) (state_d_lnr2,   IncrementFirst, Nothing);
                                 (* Descendant         *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Descendant-or-Self *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_d_l,      Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_d_lin,    Preserve,       Distinct);
                                 (* Ancestor           *) (state_d_lin,    Preserve,       DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_d_lnr,    Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_d_lnru,   Preserve,       DistinctDocorder) 
    |];
    [| (* State state_d_lnr2  *) (* Child              *) (state_d_lnr2,   IncrementFirst, Nothing);
                                 (* Descendant         *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Descendant-or-Self *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_d_l,      Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_d_lin,    Preserve,       DistinctDocorder);
                                 (* Ancestor           *) (state_d_lin,    Preserve,       DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_d_lnr,    Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_d_lnru,   Preserve,       DistinctDocorder) 
    |];
    [| (* State state_d_nn   *)  (* Child              *) (state_d_nn,     IncrementFirst, Nothing);
                                 (* Descendant         *) (state_d_ntree,  Preserve,       Nothing);
                                 (* Descendant-or-Self *) (state_d_ntree,  Preserve,       Nothing);
                                 (* Following          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_d_nn,     Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_d_no2d,   Preserve,       Distinct);
                                 (* Ancestor           *) (state_d_lin,    Preserve,       DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_d_lnrl,   Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_d_nn,     Preserve,       DistinctDocorder) 
    |];
    [| (* State state_d_lin   *) (* Child              *) (state_d_l1,     Preserve,       Nothing);
                                 (* Descendant         *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Descendant-or-Self *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_d_lnlu1,  Preserve,       Nothing);
                                 (* Parent             *) (state_d_lin,    Preserve,       Nothing);
                                 (* Ancestor           *) (state_d_lin,    Preserve,       DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_d_lin,    Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_d_lnru,   Preserve,       Nothing) 
    |];
    [| (* State state_d_no2d  *) (* Child              *) (state_d_nn,     Preserve,       Nothing);
                                 (* Descendant         *) (state_d_ntree,  Preserve,       Nothing);
                                 (* Descendant-or-Self *) (state_d_ntree,  Preserve,       Nothing);
                                 (* Following          *) (state_d_ntree,  Preserve,       Nothing);
                                 (* Following-Sibling  *) (state_d_nn,     Preserve,       Nothing);
                                 (* Parent             *) (state_d_no2d,   Preserve,       Nothing);
                                 (* Ancestor           *) (state_d_lin,    Preserve,       Nothing);
                                 (* Ancestor-or-Self   *) (state_d_lin,    Preserve,       Nothing);
                                 (* Preceding          *) (state_d_ntree,  Preserve,       Nothing);
                                 (* Preceding-Sibling  *) (state_d_nn,     Preserve,       Nothing) 
    |];
    [| (* State state_d_ntree *) (* Child              *) (state_d_ntree1, Preserve,       Nothing);
                                 (* Descendant         *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Descendant-or-Self *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Ancestor           *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_d_ntree,  Preserve,       DistinctDocorder) 
    |];
    [| (* State state_d_ntree1*) (* Child              *) (state_d_ntree2, Preserve,       Nothing);
                                 (* Descendant         *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Descendant-or-Self *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_d_ntree,  Preserve,       Distinct);
                                 (* Ancestor           *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_d_ntree,  Preserve,       DistinctDocorder) 
    |];
    [| (* State state_d_ntree2*) (* Child              *) (state_d_ntree2, Preserve,       Nothing);
                                 (* Descendant         *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Descendant-or-Self *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Ancestor           *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_d_ntree,  Preserve,       DistinctDocorder) 
    |];
    [| (* State state_d_undef *) (* Child              *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Descendant         *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Descendant-or-Self *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Ancestor           *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_d_ntree,  Preserve,       DistinctDocorder) 
    |]
  |]

let duptidy_transition_matrix_1 =
  [|
    [| (* State state_d_l     *) (* Child              *) (state_d_l1,     IncrementFirst, Nothing);
                                 (* Descendant         *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Descendant-or-Self *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_d_l,      Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_d_l,      DecrementFirst, DistinctDocorder);
                                 (* Ancestor           *) (state_d_l,      DecrementFirst, DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_d_l,      Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_d_l,      Preserve,       DistinctDocorder)
    |];
    [| (* State state_d_l1    *) (* Child              *) (state_d_l2,     IncrementFirst, Nothing);
                                 (* Descendant         *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Descendant-or-Self *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_d_l,      Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_d_l,      DecrementFirst, Distinct);
                                 (* Ancestor           *) (state_d_l,      DecrementFirst, DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_d_l,      Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_d_l,      Preserve,       DistinctDocorder)
    |];
    [| (* State state_d_l2    *) (* Child              *) (state_d_l2,     IncrementFirst, Nothing);
                                 (* Descendant         *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Descendant-or-Self *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_d_l,      Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_d_l,      DecrementFirst, DistinctDocorder);
                                 (* Ancestor           *) (state_d_l,      DecrementFirst, DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_d_l,      Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_d_l,      Preserve,       DistinctDocorder)
    |];
    [| (* State state_d_lnl   *) (* Child              *) (state_d_lnl1,   IncrementFirst, Nothing);
                                 (* Descendant         *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Descendant-or-Self *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_d_lnl,    Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_d_lnl,    DecrementFirst, DistinctDocorder);
                                 (* Ancestor           *) (state_d_lnl,    DecrementFirst, DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_d_lnl,    Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_d_lnl,    Preserve,       DistinctDocorder)
    |];
    [| (* State state_d_lnl1  *) (* Child              *) (state_d_lnl2,   IncrementFirst, Nothing);
                                 (* Descendant         *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Descendant-or-Self *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_d_lnl,    Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_d_lnl,    DecrementFirst, Distinct);
                                 (* Ancestor           *) (state_d_lnl,    DecrementFirst, DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_d_lnl,    Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_d_lnl,    Preserve,       DistinctDocorder)
    |];
    [| (* State state_d_lnl2  *) (* Child              *) (state_d_lnl2,   IncrementFirst, Nothing);
                                 (* Descendant         *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Descendant-or-Self *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_d_lnl,    Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_d_lnl,    DecrementFirst, DistinctDocorder);
                                 (* Ancestor           *) (state_d_lnl,    DecrementFirst, DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_d_lnl,    Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_d_lnl,    Preserve,       DistinctDocorder)
    |];
    [| (* State state_d_lnlu  *) (* Child              *) (state_d_lnlu,   IncrementFirst, Nothing);
                                 (* Descendant         *) (state_d_ntree,  Preserve,       Nothing);
                                 (* Descendant-or-Self *) (state_d_ntree,  Preserve,       Nothing);
                                 (* Following          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_d_lnlu,   Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_d_lnlu,   DecrementFirst, Distinct);
                                 (* Ancestor           *) (state_d_lnl,    DecrementFirst, DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_d_lnl,    Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_d_lnlu,   Preserve,       DistinctDocorder)
    |];
    [| (* State state_d_lnlu1 *) (* Child              *) (state_d_lnlu1,  IncrementFirst, Nothing);
                                 (* Descendant         *) (state_d_ntree2, Preserve,       Nothing);
                                 (* Descendant-or-Self *) (state_d_ntree2, Preserve,       Nothing);
                                 (* Following          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_d_lnlu,   Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_d_lnlu,   DecrementFirst, Distinct);
                                 (* Ancestor           *) (state_d_lnl,    DecrementFirst, DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_d_lnl,    Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_d_lnlu,   Preserve,       DistinctDocorder)
    |];
    [| (* State state_d_lnlu2 *) (* Child              *) (state_d_lnlu2,  IncrementFirst, Nothing);
                                 (* Descendant         *) (state_d_ntree2, Preserve,       Nothing);
                                 (* Descendant-or-Self *) (state_d_ntree2, Preserve,       Nothing);
                                 (* Following          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_d_lnlu,   Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_d_lnlu,   DecrementFirst, DistinctDocorder);
                                 (* Ancestor           *) (state_d_lnl,    DecrementFirst, DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_d_lnl,    Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_d_lnlu,   Preserve,       DistinctDocorder)
    |];
    [| (* State state_d_lnrl  *) (* Child              *) (state_d_lnrl1,  IncrementFirst, Nothing);
                                 (* Descendant         *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Descendant-or-Self *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_d_lnrl,   Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_d_lnrl,   DecrementFirst, DistinctDocorder);
                                 (* Ancestor           *) (state_d_lnrl,   DecrementFirst, DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_d_lnrl,   Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_d_lnrl,   Preserve,       DistinctDocorder)
    |];
    [| (* State state_d_lnrl1 *) (* Child              *) (state_d_lnrl2,  IncrementFirst, Nothing);
                                 (* Descendant         *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Descendant-or-Self *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_d_lnrl,   Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_d_lnrl,   DecrementFirst, Distinct);
                                 (* Ancestor           *) (state_d_lnrl,   DecrementFirst, DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_d_lnrl,   Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_d_lnrl,   Preserve,       DistinctDocorder)
    |];
    [| (* State state_d_lnrl2 *) (* Child              *) (state_d_lnrl2,  IncrementFirst, Nothing);
                                 (* Descendant         *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Descendant-or-Self *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_d_lnrl,   Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_d_lnrl,   DecrementFirst, DistinctDocorder);
                                 (* Ancestor           *) (state_d_lnrl,   DecrementFirst, DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_d_lnrl,   Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_d_lnrl,   Preserve,       DistinctDocorder)
    |];
    [| (* State state_d_lnru  *) (* Child              *) (state_d_lnru,   IncrementFirst, Nothing);
                                 (* Descendant         *) (state_d_ntree,  Preserve,       Nothing);
                                 (* Descendant-or-Self *) (state_d_ntree,  Preserve,       Nothing);
                                 (* Following          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_d_lnru,   Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_d_lnru,   DecrementFirst, Distinct);
                                 (* Ancestor           *) (state_d_lnr,    DecrementFirst, DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_d_lnr,    Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_d_lnru,   Preserve,       DistinctDocorder)
    |];
    [| (* State state_d_lnr   *) (* Child              *) (state_d_lnr1,   IncrementFirst, Nothing);
                                 (* Descendant         *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Descendant-or-Self *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_d_lnr,    Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_d_lnr,    DecrementFirst, DistinctDocorder);
                                 (* Ancestor           *) (state_d_lnr,    DecrementFirst, DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_d_lnr,    Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_d_lnr,    Preserve,       DistinctDocorder)
    |];
    [| (* State state_d_lnr1  *) (* Child              *) (state_d_lnr2,   IncrementFirst, Nothing);
                                 (* Descendant         *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Descendant-or-Self *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_d_lnr,    Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_d_lnr,    DecrementFirst, Distinct);
                                 (* Ancestor           *) (state_d_lnr,    DecrementFirst, DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_d_lnr,    Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_d_lnr,    Preserve,       DistinctDocorder)
    |];
    [| (* State state_d_lnr2  *) (* Child              *) (state_d_lnr2,   IncrementFirst, Nothing);
                                 (* Descendant         *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Descendant-or-Self *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Following-Sibling  *) (state_d_lnr,    Preserve,       DistinctDocorder);
                                 (* Parent             *) (state_d_lnr,    DecrementFirst, DistinctDocorder);
                                 (* Ancestor           *) (state_d_lnr,    DecrementFirst, DistinctDocorder);
                                 (* Ancestor-or-Self   *) (state_d_lnr,    Preserve,       DistinctDocorder);
                                 (* Preceding          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                 (* Preceding-Sibling  *) (state_d_lnr,    Preserve,       DistinctDocorder)
    |];
    [| (* State state_d_nn   *) (* Child              *) (state_d_nn,     IncrementFirst, Nothing);
                                (* Descendant         *) (state_d_ntree,  Preserve,       Nothing);
                                (* Descendant-or-Self *) (state_d_ntree,  Preserve,       Nothing);
                                (* Following          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                (* Following-Sibling  *) (state_d_nn,     Preserve,       DistinctDocorder);
                                (* Parent             *) (state_d_nn,     DecrementFirst, Distinct);
                                (* Ancestor           *) (state_d_lnrl,   DecrementFirst, DistinctDocorder);
                                (* Ancestor-or-Self   *) (state_d_lnrl,   Preserve,       DistinctDocorder);
                                (* Preceding          *) (state_d_ntree,  Preserve,       DistinctDocorder);
                                (* Preceding-Sibling  *) (state_d_nn,     Preserve,       DistinctDocorder) 
    |]
  |]

let duptidy_needs_sorting state: bool =
  let state = fst state
  in
    if ((state = state_d_l1)     || (state = state_d_l2)    ||
        (state = state_d_lnl1)   || (state = state_d_lnl2)  ||
	(state = state_d_lnlu1)  ||
	(state = state_d_lnrl1)  || (state = state_d_lnrl2) ||
	(state = state_d_lnr1)   || (state = state_d_lnr2)  ||
	(state = state_d_ntree1) || (state = state_d_ntree2))
    then true
    else false

let duptidy_needs_dupelim state: bool =
  let state = fst state in
  if (state = state_d_undefined)
  then true 
  else false

let duptidy_init_state = (state_d_no2d,   (0,0))
let duptidy_sink_state = (state_d_ntree2, (0,0))

let duptidy_final_state_after_ddo (st: state) =
  if not(duptidy_needs_sorting st) || (duptidy_needs_dupelim st) 
  then st
  else (
    let i = fst (snd st) 
    and j = snd (snd st)
    in 
    let st = fst st
    in
         if (st = state_d_l1) || (st = state_d_l2)       then (state_d_l, (i,j))
    else if (st = state_d_lnl1) || (st = state_d_lnl2)   then (state_d_lnl, (i,j))
    else if (st = state_d_lnlu1) || (st = state_d_lnlu2) then (state_d_lnlu, (i,j))
    else if (st = state_d_lnrl1) || (st = state_d_lnrl2) then (state_d_lnrl, (i,j))
    else if (st = state_d_lnr1) || (st = state_d_lnr2)   then (state_d_lnr, (i,j))
    else                                                      (state_d_ntree, (0,0))
  )


(*****************************************************************************)
(**********                     SLOPPY AUTOMATON                    **********)
(*****************************************************************************)

(*****************************************************************************)
(* The constants for the different states of the (merged) sloppy automaton   *)
(*                                                                           *)
(*   0: [i = 0] ord(>=0), lin, nodup                                         *)
(*      [i >= 1, j = 0] ord(>=i), lin(i), nodup, ~ord(<=i-1), nsib(<=i-1)    *)
(*      [i >= 1, j >= 1] ord(>=0), lin(j), unrel(j-1), nodup, nsib           *)
(*   1: ord(>=i), lin(i+j), ~nodup, [i >= 1] ~ord(<=i-1)                     *)
(*                                  [j >= 1] unrel(i+j-1)                    *)
(*   2: ord(i), ~ord(>=i+1), ~nodup, ntree, [i >= 1] ~ord(<=i-1)             *)
(*   3: ord(i), ~ord(>=i+1), nodup, ntree, [i >= 1] ~ord(<=i-1)              *)
(*   4: ord(>=i), lin(i), nodup, ~unrel, [i >= 1] ~ord(<=i-1), nsib(<=i-1)   *)
(*   5: ord(i), gen, ~nodup, [i >= 1] ~ord(<=i-1)                            *)
(*   6: ord, nodup, gen, nsib                                                *)
(*   7: ord, nodup, no2d                                                     *)
(*   8: nodup, ~ord(>= 0), ntree                                             *)
(*   9: ~ord(>=0), ~nodup                                                    *)
(*****************************************************************************)

let state_s_olun  = 0
let state_s_ol    = 1
let state_s_o     = 2
let state_s_on    = 3
let state_s_oln   = 4
let state_s_og    = 5
let state_s_ogn   = 6
let state_s_no2d  = 7
let state_s_ntree = 8
let state_s_sink  = 9

(*****************************************************************************)
(* Three transition tables for the automaton, which one is used depends on   *)
(* the substate index i and j                                                *)
(*                                                                           *)
(* States of the automaton: see state constants                              *)
(*                                                                           *)
(* Rows in transition_matrices are in this order                             *)
(* Note: States 6-9 do not appear in transition matrix for i>0, since they   *)
(*       have no substates;                                                  *)
(*       States 2-9 do not appear in transition matrix for j > 0, since they *)
(*       have no substates with j > 0                                        *)
(* Note: 7 is the initial state                                              *)
(*                                                                           *)
(* Axes: see axis constants                                                  *)
(*                                                                           *)
(* Columns in transition_matrices are in this order                          *)
(*                                                                           *)
(* [i = 0]        Use transition_matrix_0                                    *)
(* [i > 0, j = 0] Use transition_matrix_1                                    *)
(* [i > 0, j > 0] Use transition_matrix_2                                    *)
(*                                                                           *)
(* Note: the elements on position (m,n) of the transition matrix are         *)
(*       3-tuples: (q, di, op)                                               *)
(*           - q:  State that we reach when coming from state m with axis n  *)
(*           - di: What we must add to i or j when coming from state m with  *)
(*                 axis n (remark: 0 is the absolute minimum for i and j)    *)
(*           - op: What operation is needed when coming from state m with    *)
(*                 axis n, for this automaton this is always nothing         *)
(*****************************************************************************)
let sloppy_transition_matrix_0 =
  [|
    [| (* State state_s_olun  *) (* Child              *) (state_s_oln,    IncrementFirst,     Nothing);
                                 (* Descendant         *) (state_s_sink,   Preserve,           Nothing);
                                 (* Descendant-or-Self *) (state_s_sink,   Preserve,           Nothing);
                                 (* Following          *) (state_s_sink,   Preserve,           Nothing);
                                 (* Following-Sibling  *) (state_s_olun,   IncrementDecrement, Nothing);
                                 (* Parent             *) (state_s_olun,   Preserve,           Nothing);
                                 (* Ancestor           *) (state_s_sink,   Preserve,           Nothing);
                                 (* Ancestor-or-Self   *) (state_s_sink,   Preserve,           Nothing);
                                 (* Preceding          *) (state_s_sink,   Preserve,           Nothing);
                                 (* Preceding-Sibling  *) (state_s_olun,   IncrementBoth,      Nothing);
    |];
    [| (* State state_s_ol    *) (* Child              *) (state_s_ol,     IncrementFirst,     Nothing);
                                 (* Descendant         *) (state_s_sink,   Preserve,           Nothing);
                                 (* Descendant-or-Self *) (state_s_sink,   Preserve,           Nothing);
                                 (* Following          *) (state_s_sink,   Preserve,           Nothing);
                                 (* Following-Sibling  *) (state_s_ol,     IncrementDecrement, Nothing);
                                 (* Parent             *) (state_s_ol,     DecrementSecond,    Nothing);
                                 (* Ancestor           *) (state_s_sink,   Preserve,           Nothing);
                                 (* Ancestor-or-Self   *) (state_s_sink,   Preserve,           Nothing);
                                 (* Preceding          *) (state_s_sink,   Preserve,           Nothing);
                                 (* Preceding-Sibling  *) (state_s_ol,     IncrementDecrement, Nothing);
    |];
    [| (* State state_s_o     *) (* Child              *) (state_s_o,      IncrementFirst,     Nothing);
                                 (* Descendant         *) (state_s_sink,   Preserve,           Nothing);
                                 (* Descendant-or-Self *) (state_s_sink,   Preserve,           Nothing);
                                 (* Following          *) (state_s_sink,   Preserve,           Nothing);
                                 (* Following-Sibling  *) (state_s_sink,   Preserve,           Nothing);
                                 (* Parent             *) (state_s_sink,   Preserve,           Nothing);
                                 (* Ancestor           *) (state_s_sink,   Preserve,           Nothing);
                                 (* Ancestor-or-Self   *) (state_s_sink,   Preserve,           Nothing);
                                 (* Preceding          *) (state_s_sink,   Preserve,           Nothing);
                                 (* Preceding-Sibling  *) (state_s_sink,   Preserve,           Nothing);
    |];
    [| (* State state_s_on    *) (* Child              *) (state_s_on,     IncrementFirst,     Nothing);
                                 (* Descendant         *) (state_s_sink,   Preserve,           Nothing);
                                 (* Descendant-or-Self *) (state_s_sink,   Preserve,           Nothing);
                                 (* Following          *) (state_s_sink,   Preserve,           Nothing);
                                 (* Following-Sibling  *) (state_s_sink,   Preserve,           Nothing);
                                 (* Parent             *) (state_s_sink,   Preserve,           Nothing);
                                 (* Ancestor           *) (state_s_sink,   Preserve,           Nothing);
                                 (* Ancestor-or-Self   *) (state_s_sink,   Preserve,           Nothing);
                                 (* Preceding          *) (state_s_sink,   Preserve,           Nothing);
                                 (* Preceding-Sibling  *) (state_s_sink,   Preserve,           Nothing);
    |];
    [| (* State state_s_oln   *) (* Child              *) (state_s_oln,    IncrementFirst,     Nothing);
                                 (* Descendant         *) (state_s_sink,   Preserve,           Nothing);
                                 (* Descendant-or-Self *) (state_s_sink,   Preserve,           Nothing);
                                 (* Following          *) (state_s_sink,   Preserve,           Nothing);
                                 (* Following-Sibling  *) (state_s_olun,   IncrementDecrement, Nothing);
                                 (* Parent             *) (state_s_oln,    Preserve,           Nothing);
                                 (* Ancestor           *) (state_s_sink,   Preserve,           Nothing);
                                 (* Ancestor-or-Self   *) (state_s_sink,   Preserve,           Nothing);
                                 (* Preceding          *) (state_s_sink,   Preserve,           Nothing);
                                 (* Preceding-Sibling  *) (state_s_olun,   IncrementBoth,      Nothing);
    |];
    [| (* State state_s_og    *) (* Child              *) (state_s_og,     IncrementFirst,     Nothing);
                                 (* Descendant         *) (state_s_sink,   Preserve,           Nothing);
                                 (* Descendant-or-Self *) (state_s_sink,   Preserve,           Nothing);
                                 (* Following          *) (state_s_sink,   Preserve,           Nothing);
                                 (* Following-Sibling  *) (state_s_og,     IncrementFirst,     Nothing);
                                 (* Parent             *) (state_s_og,     Preserve,           Nothing);
                                 (* Ancestor           *) (state_s_sink,   Preserve,           Nothing);
                                 (* Ancestor-or-Self   *) (state_s_sink,   Preserve,           Nothing);
                                 (* Preceding          *) (state_s_sink,   Preserve,           Nothing);
                                 (* Preceding-Sibling  *) (state_s_og,     IncrementFirst,     Nothing);
    |];
    [| (* State state_s_ogn   *) (* Child              *) (state_s_ogn,    Preserve,          Nothing);
                                 (* Descendant         *) (state_s_on,     Preserve,           Nothing);
                                 (* Descendant-or-Self *) (state_s_on,     Preserve,           Nothing);
                                 (* Following          *) (state_s_sink,   Preserve,           Nothing);
                                 (* Following-Sibling  *) (state_s_og,     IncrementFirst,     Nothing);
                                 (* Parent             *) (state_s_og,     Preserve,           Nothing);
                                 (* Ancestor           *) (state_s_sink,   Preserve,           Nothing);
                                 (* Ancestor-or-Self   *) (state_s_sink,   Preserve,           Nothing);
                                 (* Preceding          *) (state_s_sink,   Preserve,           Nothing);
                                 (* Preceding-Sibling  *) (state_s_og,     IncrementFirst,     Nothing);
    |];
    [| (* State state_s_no2d  *) (* Child              *) (state_s_ogn,    Preserve,           Nothing);
                                 (* Descendant         *) (state_s_on,     Preserve,           Nothing);
                                 (* Descendant-or-Self *) (state_s_on,     Preserve,           Nothing);
                                 (* Following          *) (state_s_on,     Preserve,           Nothing);
                                 (* Following-Sibling  *) (state_s_ogn,    Preserve,           Nothing);
                                 (* Parent             *) (state_s_no2d,   Preserve,           Nothing);
                                 (* Ancestor           *) (state_s_oln,    Preserve,           Nothing);
                                 (* Ancestor-or-Self   *) (state_s_oln,    Preserve,           Nothing);
                                 (* Preceding          *) (state_s_on,     Preserve,           Nothing);
                                 (* Preceding-Sibling  *) (state_s_ogn,    Preserve,           Nothing);
    |];
    [| (* State state_s_no2d  *) (* Child              *) (state_s_ntree,  Preserve,           Nothing);
                                 (* Descendant         *) (state_s_sink,   Preserve,           Nothing);
                                 (* Descendant-or-Self *) (state_s_sink,   Preserve,           Nothing);
                                 (* Following          *) (state_s_sink,   Preserve,           Nothing);
                                 (* Following-Sibling  *) (state_s_sink,   Preserve,           Nothing);
                                 (* Parent             *) (state_s_sink,   Preserve,           Nothing);
                                 (* Ancestor           *) (state_s_sink,   Preserve,           Nothing);
                                 (* Ancestor-or-Self   *) (state_s_sink,   Preserve,           Nothing);
                                 (* Preceding          *) (state_s_sink,   Preserve,           Nothing);
                                 (* Preceding-Sibling  *) (state_s_sink,   Preserve,           Nothing);
    |];
    [| (* State state_s_sink  *) (* Child              *) (state_s_sink,   Preserve,           Nothing);
                                 (* Descendant         *) (state_s_sink,   Preserve,           Nothing);
                                 (* Descendant-or-Self *) (state_s_sink,   Preserve,           Nothing);
                                 (* Following          *) (state_s_sink,   Preserve,           Nothing);
                                 (* Following-Sibling  *) (state_s_sink,   Preserve,           Nothing);
                                 (* Parent             *) (state_s_sink,   Preserve,           Nothing);
                                 (* Ancestor           *) (state_s_sink,   Preserve,           Nothing);
                                 (* Ancestor-or-Self   *) (state_s_sink,   Preserve,           Nothing);
                                 (* Preceding          *) (state_s_sink,   Preserve,           Nothing);
                                 (* Preceding-Sibling  *) (state_s_sink,   Preserve,           Nothing);
    |]
  |]

let sloppy_transition_matrix_1 =
  [|
    [| (* State state_s_olun  *) (* Child              *) (state_s_olun,   IncrementFirst,     Nothing);
                                 (* Descendant         *) (state_s_ntree,  Preserve,           Nothing);
                                 (* Descendant-or-Self *) (state_s_ntree,  Preserve,           Nothing);
                                 (* Following          *) (state_s_sink,   Preserve,           Nothing);
                                 (* Following-Sibling  *) (state_s_ol,     DecrementSecond,    Nothing);
                                 (* Parent             *) (state_s_ol,     DecrementBoth,      Nothing);
                                 (* Ancestor           *) (state_s_sink,   Preserve,           Nothing);
                                 (* Ancestor-or-Self   *) (state_s_sink,   Preserve,           Nothing);
                                 (* Preceding          *) (state_s_sink,   Preserve,           Nothing);
                                 (* Preceding-Sibling  *) (state_s_ol,     DecrementSecond,    Nothing);
    |];
    [| (* State state_s_ol    *) (* Child              *) (state_s_ol,     IncrementFirst,     Nothing);
                                 (* Descendant         *) (state_s_sink,   Preserve,           Nothing);
                                 (* Descendant-or-Self *) (state_s_sink,   Preserve,           Nothing);
                                 (* Following          *) (state_s_sink,   Preserve,           Nothing);
                                 (* Following-Sibling  *) (state_s_ol,     Preserve,           Nothing);
                                 (* Parent             *) (state_s_ol,     DecrementFirst,     Nothing);
                                 (* Ancestor           *) (state_s_sink,   Preserve,           Nothing);
                                 (* Ancestor-or-Self   *) (state_s_sink,   Preserve,           Nothing);
                                 (* Preceding          *) (state_s_sink,   Preserve,           Nothing);
                                 (* Preceding-Sibling  *) (state_s_ol,     Preserve,           Nothing);
    |];
    [| (* State state_s_o     *) (* Child              *) (state_s_o,      IncrementFirst,     Nothing);
                                 (* Descendant         *) (state_s_sink,   Preserve,           Nothing);
                                 (* Descendant-or-Self *) (state_s_sink,   Preserve,           Nothing);
                                 (* Following          *) (state_s_sink,   Preserve,           Nothing);
                                 (* Following-Sibling  *) (state_s_o,      Preserve,           Nothing);
                                 (* Parent             *) (state_s_o,      DecrementFirst,     Nothing);
                                 (* Ancestor           *) (state_s_sink,   Preserve,           Nothing);
                                 (* Ancestor-or-Self   *) (state_s_sink,   Preserve,           Nothing);
                                 (* Preceding          *) (state_s_sink,   Preserve,           Nothing);
                                 (* Preceding-Sibling  *) (state_s_o,      Preserve,           Nothing);
    |];
    [| (* State state_s_on    *) (* Child              *) (state_s_on,     IncrementFirst,     Nothing);
                                 (* Descendant         *) (state_s_sink,   Preserve,           Nothing);
                                 (* Descendant-or-Self *) (state_s_sink,   Preserve,           Nothing);
                                 (* Following          *) (state_s_sink,   Preserve,           Nothing);
                                 (* Following-Sibling  *) (state_s_o,      Preserve,           Nothing);
                                 (* Parent             *) (state_s_o,      DecrementFirst,     Nothing);
                                 (* Ancestor           *) (state_s_sink,   Preserve,           Nothing);
                                 (* Ancestor-or-Self   *) (state_s_sink,   Preserve,           Nothing);
                                 (* Preceding          *) (state_s_sink,   Preserve,           Nothing);
                                 (* Preceding-Sibling  *) (state_s_o,      Preserve,           Nothing);
    |];
    [| (* State state_s_oln   *) (* Child              *) (state_s_oln,    IncrementFirst,     Nothing);
                                 (* Descendant         *) (state_s_sink,   Preserve,           Nothing);
                                 (* Descendant-or-Self *) (state_s_sink,   Preserve,           Nothing);
                                 (* Following          *) (state_s_sink,   Preserve,           Nothing);
                                 (* Following-Sibling  *) (state_s_ol,     DecrementSecond,    Nothing);
                                 (* Parent             *) (state_s_ol,     DecrementBoth,      Nothing);
                                 (* Ancestor           *) (state_s_sink,   Preserve,           Nothing);
                                 (* Ancestor-or-Self   *) (state_s_sink,   Preserve,           Nothing);
                                 (* Preceding          *) (state_s_sink,   Preserve,           Nothing);
                                 (* Preceding-Sibling  *) (state_s_ol,     DecrementSecond,    Nothing);
    |];
    [| (* State state_s_og    *) (* Child              *) (state_s_og,     IncrementFirst,     Nothing);
                                 (* Descendant         *) (state_s_sink,   Preserve,           Nothing);
                                 (* Descendant-or-Self *) (state_s_sink,   Preserve,           Nothing);
                                 (* Following          *) (state_s_sink,   Preserve,           Nothing);
                                 (* Following-Sibling  *) (state_s_og,     Preserve,           Nothing);
                                 (* Parent             *) (state_s_og,     DecrementFirst,     Nothing);
                                 (* Ancestor           *) (state_s_sink,   Preserve,           Nothing);
                                 (* Ancestor-or-Self   *) (state_s_sink,   Preserve,           Nothing);
                                 (* Preceding          *) (state_s_sink,   Preserve,           Nothing);
                                 (* Preceding-Sibling  *) (state_s_og,     Preserve,           Nothing);
    |]
  |]

let sloppy_transition_matrix_2 =
  [|
    [| (* State state_s_olun  *) (* Child              *) (state_s_olun,   IncrementSecond,    Nothing);
                                 (* Descendant         *) (state_s_on,     DecrementFirst,     Nothing);
                                 (* Descendant-or-Self *) (state_s_on,     DecrementFirst,     Nothing);
                                 (* Following          *) (state_s_sink,   Preserve,           Nothing);
                                 (* Following-Sibling  *) (state_s_ol,     DecrementSecond,    Nothing);
                                 (* Parent             *) (state_s_ol,     DecrementBoth,      Nothing);
                                 (* Ancestor           *) (state_s_sink,   Preserve,           Nothing);
                                 (* Ancestor-or-Self   *) (state_s_sink,   Preserve,           Nothing);
                                 (* Preceding          *) (state_s_sink,   Preserve,           Nothing);
                                 (* Preceding-Sibling  *) (state_s_ol,     DecrementSecond,    Nothing);
    |];
    [| (* State state_s_ol    *) (* Child              *) (state_s_ol,     IncrementFirst,     Nothing);
                                 (* Descendant         *) (state_s_sink,   Preserve,           Nothing);
                                 (* Descendant-or-Self *) (state_s_sink,   Preserve,           Nothing);
                                 (* Following          *) (state_s_sink,   Preserve,           Nothing);
                                 (* Following-Sibling  *) (state_s_ol,     Preserve,           Nothing);
                                 (* Parent             *) (state_s_ol,     DecrementFirst,     Nothing);
                                 (* Ancestor           *) (state_s_sink,   Preserve,           Nothing);
                                 (* Ancestor-or-Self   *) (state_s_sink,   Preserve,           Nothing);
                                 (* Preceding          *) (state_s_sink,   Preserve,           Nothing);
                                 (* Preceding-Sibling  *) (state_s_ol,     Preserve,           Nothing);
    |]
  |]

let sloppy_needs_sorting state: bool =
  let i = fst (snd state) 
  and j = snd (snd state)
  in 
  let state = fst state
  in
    if (((state = state_s_o)    && (i > 0))            ||
        ((state = state_s_on)   && (i > 0))            ||
        ((state = state_s_oln)  && (i > 0))            ||
        ((state = state_s_olun) && (i > 0) && (j = 0)) ||
        ((state = state_s_ol)   && (i > 0))            ||
        ((state = state_s_og)   && (i > 0))            ||
         (state = state_s_ntree)                       ||
         (state = state_s_sink))
    then true
    else false

let sloppy_needs_dupelim state: bool =
  let state = fst state
  in
    if ((state = state_s_o)   || (state = state_s_ol)   ||
        (state = state_s_og)  || (state = state_s_sink))
  then true
  else false

let sloppy_init_state = (state_s_no2d, (0,0))
let sloppy_sink_state = (state_s_sink, (0,0))

let sloppy_final_state_after_ddo (st: state) =
  if not(sloppy_needs_sorting st) && not(sloppy_needs_dupelim st)
  then st
  else (
    let st = fst st
    in
    if (st = state_s_og) then (state_s_ogn, (0,0))
    else                      (state_s_on, (0,0))
  )


(*****************************************************************************)
(* Common functions for doing transitions using one of the automata of above *)
(*****************************************************************************)

let needs_final_action sbdo action state: action_needed =
  let needs_dupelim = (
    match sbdo with 
    | SBDO_Tidy    -> tidy_needs_dupelim
    | SBDO_DupTidy -> duptidy_needs_dupelim
    | SBDO_Sloppy  -> sloppy_needs_dupelim
    | _            -> tidy_needs_dupelim
  ) and needs_sorting = (
    match sbdo with 
    | SBDO_Tidy    -> tidy_needs_sorting
    | SBDO_DupTidy -> duptidy_needs_sorting
    | SBDO_Sloppy  -> sloppy_needs_sorting
    | _            -> tidy_needs_sorting
  ) in 
  if ((needs_sorting state) && (needs_dupelim state)) then (
    DistinctDocorder
  ) else if (not (needs_sorting state) && (needs_dupelim state)) then (
    match action with
    | Nothing  | Distinct         -> Distinct
    | Docorder | DistinctDocorder -> DistinctDocorder
  ) else if ((needs_sorting state) && not (needs_dupelim state)) then (
    match action with
    | Nothing  | Docorder         -> Docorder
    | Distinct | DistinctDocorder -> DistinctDocorder
  ) else (
    action
  )

(* do a transition for beginstate with axis *)
let do_transition 
  (sbdo: sbdo_kind)
  (beginstate: state)
  (axis: axis_number)
  (laststep: bool)
  : (state * action_needed) =
    if (Debug.default_debug())
    then
      begin
	let msg =
	  Format.sprintf  "-> Doing transition for state %i [i=%i, j=%i] with axis %i\n" (fst beginstate) (fst (snd beginstate)) (snd (snd beginstate)) axis in
	Debug.print_default_debug msg
      end;
    let transition_matrix_0 = (
      match sbdo with 
      | SBDO_Tidy    -> tidy_transition_matrix_0
      | SBDO_DupTidy -> duptidy_transition_matrix_0
      | SBDO_Sloppy  -> sloppy_transition_matrix_0
      | _            -> tidy_transition_matrix_0
    ) and transition_matrix_1 = (
      match sbdo with 
      | SBDO_Tidy    -> tidy_transition_matrix_1
      | SBDO_DupTidy -> duptidy_transition_matrix_1
      | SBDO_Sloppy  -> sloppy_transition_matrix_1
      | _            -> tidy_transition_matrix_1
    )  and transition_matrix_2 = (
      match sbdo with 
      | SBDO_Tidy    -> tidy_transition_matrix_1
      | SBDO_DupTidy -> duptidy_transition_matrix_1
      | SBDO_Sloppy  -> sloppy_transition_matrix_2
      | _            -> tidy_transition_matrix_1
    ) in
    let result = (
      if (axis = axis_self) then (beginstate, Nothing)
      else
        match beginstate with
        | (s, (i,j)) ->
            let x =
                   if (i = 0) then transition_matrix_0.(s).(axis)
              else if (j = 0) then transition_matrix_1.(s).(axis)
	      else                 transition_matrix_2.(s).(axis)
	    in
  	      match x with
	        | (q, Preserve,           o) -> ((q, (i,  j)),   o)
	        | (q, IncrementFirst,     o) -> ((q, (i+1,j)),   o)
	        | (q, DecrementFirst,     o) -> ((q, (i-1,j)),   o)
	        | (q, IncrementSecond,    o) -> ((q, (i,  j+1)), o)
	        | (q, DecrementSecond,    o) -> ((q, (i,  j-1)), o)
	        | (q, IncrementDecrement, o) -> ((q, (i+1,j-1)), o)
	        | (q, IncrementBoth,      o) -> ((q, (i+1,j+1)), o)
	        | (q, DecrementBoth,      o) -> ((q, (i-1,j-1)), o)
    ) 
    in let state_no_index_ij = (
      match sbdo with
      | SBDO_Tidy    -> state_t_lin
      | SBDO_DupTidy -> state_d_lin
      | SBDO_Sloppy  -> state_s_ogn
      | _            -> state_t_lin
    )
    and state_no_index_j = (
      match sbdo with
      | SBDO_Tidy    -> state_t_l
      | SBDO_DupTidy -> state_d_l
      | SBDO_Sloppy  -> state_s_o
      | _            -> state_t_l
    )
    in let final_state_after_ddo = (
      match sbdo with
      | SBDO_Tidy    -> tidy_final_state_after_ddo
      | SBDO_DupTidy -> duptidy_final_state_after_ddo
      | SBDO_Sloppy  -> sloppy_final_state_after_ddo
      | _            -> tidy_final_state_after_ddo
    )
    in let result = (
         match result with
         | ((state, (i, j)), o) ->
	     let i = if (state >= state_no_index_ij || i < 0) then 0 else i
	     and j = if (state >= state_no_index_j  || j < 0) then 0 else j
	     in ((state, (i,j)), o)
    ) in match result with
    | ((state, i), a) -> if (laststep)
                         then (final_state_after_ddo (state, i), needs_final_action sbdo a (state, i))
                         else ((state, i), a)

(* auxiliary definitions *)
let init_state sbdo: state =
  match sbdo with
  | SBDO_Tidy    -> tidy_init_state
  | SBDO_DupTidy -> duptidy_init_state
  | SBDO_Sloppy  -> sloppy_init_state
  | _            -> tidy_init_state

let sink_state sbdo: state = 
  match sbdo with
  | SBDO_Tidy    -> tidy_sink_state
  | SBDO_DupTidy -> duptidy_sink_state
  | SBDO_Sloppy  -> sloppy_sink_state
  | _            -> tidy_sink_state

let duptidy_undefined_state = (state_d_undefined, (0,0)) 

let undefined_state sbdo: state = 
  match sbdo with
  | SBDO_Tidy    -> tidy_sink_state
  | SBDO_DupTidy -> duptidy_undefined_state
  | SBDO_Sloppy  -> sloppy_sink_state
  | _            -> tidy_sink_state

let needs_dupelim a: bool =
  ((a = DistinctDocorder) || (a = Distinct))

let needs_sorting a: bool =
  ((a = DistinctDocorder) || (a = Docorder))
  
let print_action action =
  match action with
  | Nothing -> "Nothing"
  | Docorder -> "Docorder"
  | Distinct -> "Distinct"
  | DistinctDocorder -> "DistinctDocorder"

let print_state (st:state) =
    String.concat "" ["(";(string_of_int (fst st));" [i=";(string_of_int (fst (snd st)));", j=";(string_of_int (snd (snd st)));"])"; ]
