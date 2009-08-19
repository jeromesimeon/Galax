(* This will test the output of the stemmer.c implementation against
   the stemmer.ml implementation.

   $Id: test.ml,v 1.2 2004/10/05 15:49:03 simeon Exp $
*)

open Stemmer
open StemmerC

let _ =
  let rec aux failed chan =
    try
      let word = String.lowercase (input_line chan) in
        if not (Stemmer.stem word = StemmerC.stem word) then
          begin
            print_string (word ^ " did not match ("^(Stemmer.stem word)^" != "^(StemmerC.stem word)^")\n");
            aux (succ failed) chan
          end
        else
            aux failed chan
    with _ ->
      begin
        print_int failed;
        print_string " tests failed\n";
      end
  in
  let chan = open_in "/usr/share/dict/words" in
    aux 0 chan
;;

    
