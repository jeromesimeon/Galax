(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: df_struct.mli,v 1.2 2007/02/01 22:08:54 simeon Exp $ *)

(* Module: Df_struct
   Description:
     This module provides data structures together with corresponding creation
   and manipulation functions for a generic class of data flow graphs.

   - Michael *)


exception Dferror of string


(* nodes that do not have successors *)
type dfsink_kind =
  | DFSerialize
  | DFControl

(* nodes that have exactly one successor *)
type dfpass_kind =
    
  (* signals wether order of a node relative to any other is relevant *)
  | DFOrdered of int
  | DFUnordered

  (* end point of multiple arcs *)
  | DFMerge
      
  | DFImmediate

type dfnode_kind =
  | DFSink of dfsink_kind
  | DFPass of dfpass_kind
  | DFFork
      

type dfnode_id

type ('a, 'b) dfnode

(* data flow sources, terminal *)
type ('a, 'b) dfgraph = ('a, 'b) dfnode list * ('a, 'b) dfnode


(*****************)
(* node creation *)
(*****************)

val mkdfnode_dfsink :
  dfsink_kind -> 'a -> ('a, 'b) dfnode

val mkdfnode_dfpass :
  dfpass_kind -> 'a -> ('a, 'b) dfnode

val mkdfnode_dffork :
  'a -> ('a, 'b) dfnode


(******************)
(* graph creation *)
(******************)

val mkdfgraph_empty :
  unit -> ('a, 'b) dfgraph

val mkdfgraph_singleton :
  ('a, 'b) dfnode -> ('a, 'b) dfgraph


(***************)
(* node access *)
(***************)

val get_dfnode_value :
  ('a, 'b) dfnode -> 'a

val get_dfnode_kind :
  ('a, 'b) dfnode -> dfnode_kind

val get_dfnode_id :
  ('a, 'b) dfnode -> dfnode_id


(* convinience functions *)

val is_dfsink :
  dfsink_kind -> ('a, 'b) dfnode -> bool

val is_dfpass :
  dfpass_kind -> ('a, 'b) dfnode -> bool

val is_dffork :
  ('a, 'b) dfnode -> bool


(********************)
(* node affiliation *)
(********************)

(* Node affiliation serves the purpose of grouping several logically related nodes,
   according to the specialized semantics of a particular graph instantiation. *)

val affiliate_unary :
  ('a, 'b) dfnode -> unit

val affiliate_binary :
   ('a, 'b) dfnode ->  ('a, 'b) dfnode -> unit

val affiliate_many :
   ('a, 'b) dfnode list -> unit

val get_affiliates :
   ('a, 'b) dfnode ->  ('a, 'b) dfnode list


(*********************)
(* graph composition *)
(*********************)

(* Termination of one ore more graphs by a specified node means
   1) introducing an arc from the terminal node of the graph(s) to the
      specified one
   2) setting the new node as the terminal node of the resulting graph *)

val terminate_unary :
   ('a, 'b) dfnode ->  ('a, 'b) dfgraph ->  ('a, 'b) dfgraph

val terminate_binary :
   ('a, 'b) dfnode ->  ('a, 'b) dfgraph ->  ('a, 'b) dfgraph ->  ('a, 'b) dfgraph

val terminate_many :
   ('a, 'b) dfnode ->  ('a, 'b) dfgraph list ->  ('a, 'b) dfgraph


(* The result graph merge(a, b) has all the sources of the input graphs a, b
   and a's terminal node as its terminal nodes. *)

val merge_dfgraphs :
   ('a, 'b) dfgraph ->  ('a, 'b) dfgraph ->  ('a, 'b) dfgraph


(*******************)
(* graph iteration *)
(*******************)

(* Applies the specified function exactly once to each node in the graph;
   'b is intended to be used as a functional context. *)

val iter_dfs :
  ( ('a, 'b) dfnode -> 'b -> 'b) -> 'b ->  ('a, 'b) dfgraph -> unit


(* Performs a dfs traversal of the graph starting at the specified node.
   For each node, first calculates a modified context c' using g that it
   then hands to the next recursive instance that will apply f on its children
   (using c'). Lastly, f is applied on the current node, the values resulting
   from its children and the original context c.

   Each node is visited exactly once; intermediate results are stored inside
   the very node they belong to. *)

val fold_left_dfs :
  ('a list ->  ('b, 'a) dfnode -> 'c -> 'a) -> (* f *)
     (('b, 'a) dfnode -> 'c -> 'c) ->          (* g *)
        'a -> 'c -> ('b, 'a) dfnode -> 'a


(*******************)
(* graph searching *)
(*******************)

val find_all :
  ( ('a, 'b) dfnode -> bool) ->  ('a, 'b) dfgraph ->  ('a, 'b) dfnode list


(**************)
(* dot output *)
(**************)

val string_of_dfnode_id :
  dfnode_id -> string

val print_dot_dfgraph :
  Format.formatter ->  ('a, 'b) dfgraph -> unit


(* Clusters the resulting graph according to node affiliation, i.e.,
   nodes a, b are in the same cluster iff they are affiliated. *)

val print_dot_clustered_dfgraph :
  Format.formatter ->  ('a, 'b) dfgraph -> unit


(* Uses the specified function to print node information that depends on the
   particular graph instantiation. The user must ensure that function produces
   valid dot format output. *)

val print_dot_dfgraph_custom :
  Format.formatter -> (Format.formatter -> dfnode_kind -> dfnode_id -> 'a -> unit) ->  ('a, 'b) dfgraph -> unit
