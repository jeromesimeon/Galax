(: defs :)
declare variable $latency := ();

(: P1 :)
fun pingEvent(Y,E) {
  let T = now();
  let X = myaddress();
  async Y { ping(X,E,T); }
}

(: P2 :)
fun ping(X,E,T) {
  let Y = myaddress();
  async X { pong(Y,E,T); }
}

(: P3 :)
fun pong(Y,E,T1) {
  (: update latency table :)
  let T = now();
  latency[Y] := T-T1; (: RACE :)
}

