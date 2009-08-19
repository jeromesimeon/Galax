(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* Module: Monitor.
   Description:
     This module is used for monitoring the activity of Galax. 
*)

open Monitoring_context
open Processing_context

(* Note:

   - Assumptions: 

   0. Monitoring data is contained in the the processing context.

   1. Monitor output is reported on a per-function-call basis. 
      Only one Galax API function call is monitored at any one time. 

   2. A function call is either part of the "prolog" or is a "query/statement".

   3. Phases within a function call are mutually exclusive and not
      nested.  

      The one exception is document parsing/loading, which may be
      nested within evaluation (as a call to fn:doc) or may occur at
      the top-level (as a call to Galax.load_document).

      The evaluation phase contains all document parse/load times for
      documents parsed/loaded during evaluation.  The individual
      document parse/load times are reported for reference.  

   4. The DTD of the monitor output is below.
      A processing phase is reported if it has some non-zero statistic.
      A statistic is reported if it is not zero.

      <!ELEMENT monitor (summary, call* )>
      <!ENTITY % stats (allocated_memory?, elapsed_time?, node_count?)>
      <!ELEMENT summary %stats, prolog, statements)>
      <!ELEMENT prolog  %stats>
      <!ELEMENT statements %stats>
      <!ELEMENT call (parsing?, normalization?, rewriting?, factorization?, compilation?, evaluation?, serialization?, parse-load-document*>
      <!ATTLIST call name CDATA #REQUIRED
                kind (prolog|statements) #REQUIRED>
      <!ELEMENT parsing %stats>
      <!ELEMENT normalization %stats>
      <!ELEMENT rewriting %stats>
      <!ELEMENT factorization %stats>
      <!ELEMENT compilation %stats>
      <!ELEMENT evaluation (%stats, load-document* )>
      <!ELEMENT serialization %stats>
      <!ELEMENT toplevel-load-document %stats>
      <!ATTLIST toplevel-load-document name CDATA #REQUIRED>
      <!ELEMENT load-document %stats>
      <!ATTLIST load-document name CDATA #REQUIRED>
      <!ELEMENT allocated_memory #PCDATA> <!-- memory in KB formatted as %.3fK -->
      <!ELEMENT elapsed_time #PCDATA>     <!-- time formatted as %.3fs or %im%.3fs or %ih%im%.3fs -->
      <!ELEMENT node_count #PCDATA>       <!-- an integer -->

   5. monitor_of_last_call yields well-formed document of element call
      monitor_of_all_calls yields well-formed document of element monitor
*)

(*****************************)
(* Monitored call interface  *)
(*****************************)

val wrap_monitor       	 : processing_context -> phase -> ('a -> 'b) -> 'a -> 'b

val start_monitor_call 	 : processing_context -> call_kind -> string -> unit
val end_monitor_call   	 : processing_context -> unit

val start_monitor_external_call	 : processing_context -> string -> unit
val end_monitor_external_call  	 : processing_context -> unit

val monitor_of_last_call : processing_context -> Physical_value.item list
val monitor_of_all_calls : processing_context -> Physical_value.item list

val serialize_monitor    : processing_context -> unit

