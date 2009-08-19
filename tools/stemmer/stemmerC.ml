(* This is a wrapper around stemmer.c, used entirely for testing
   purposes.  If speed is a big issue, this implementation is most likely
   quite a bit faster.

   $Id: stemmerC.ml,v 1.2 2004/10/05 15:49:03 simeon Exp $
*)

(* Register exceptions so they can be raised externally *)
exception No_stem of int ;;

Callback.register_exception "stemmer_exn" (No_stem 0) ;;

external stem : string -> string = "CamlStem" ;;
external word_size : string -> int = "CamlWordSize" ;;

(* End of file *)
