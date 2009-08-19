module namespace B = "server";
import interface namespace S = "server" at "server.xqi";
import module namespace dxq = "http://www.galaxquery.org/dxq" at "../dxq.xq";
declare namespace foo = "Foo"; 
declare server A implements S at "a";
declare server Self implements S at "b";

declare variable $B:v := "v";

declare updating function B:local() { 
  dxq:my_address()
};

(:
declare function B:local() { 
  Self:c()
};
:)

declare function B:eval() {
  let $x := for server Self box { 1 }
  return $x 
};

declare function B:factorial($x) { 
  if ($x > 1) then $x * B:factorial($x - 1) 
  else $x 
};

declare updating function B:eval_box_at_A() {
  let $x := for server A box { B:c() }
  return from server A return { dxq:gui-report("At A!"); eval box $x }
};

(: ----------------------- :)
(: Test return values ONLY :)
(: ----------------------- :)
declare function B:c() { 
  "const@B"
};

declare function B:error() { 
  A:error() 
};

declare function B:s() { 
  A:s()
};

declare function B:i() { 
  A:i()
};

declare function B:d() { 
  A:d()
};

declare function B:ilist() { 
  A:ilist()
};

declare function B:mixed() { 
  A:mixed()
};

declare function B:attr() { 
      A:attr()
};

declare function B:elem() { 
      A:elem()
};

declare function B:primitive() { 
      A:primitive()
};

(: ----------------------------------------- :)
(: Test round-tripping of function arguments :)
(: ----------------------------------------- :)

(: Test these functions by passing "mixed" heterogenous values, 
   in various arguments:
   B:id((1, <a/>, "string"))
   B:id2((1, <a/>, "string"), (1, <a/>, "string"))
:)
declare function B:id($x) { 
      A:id($x) 
};
declare function B:id2($x,$y) { 
      A:id2($x,$y) 
};
declare function B:id3($x,$y,$z) { 
      A:id3($x,$y,$z) 
};
declare function B:id4($x,$y,$z,$w) { 
      A:id4($x,$y,$z,$w) 
};

(: ----------------------------------------- :)
(: Test path expressions over element values :)
(: ----------------------------------------- :)

(: 
   Test path expressions with no command-line arguments
   and with -dxq on -inline-functions on  
:)
(: B:value()/b :)
(: B:value()/b/c :)

declare function B:value() { 
      A:value() 
};

declare function B:path-b() { 
      A:value()/b 
};

declare function B:path-bc() { 
      A:value()/b/c 
};

(: <a><b><c>1</c></b><b><c>2</c></b><b><c>3</c></b></a> :)
declare function B:path-b1($x) { 
       A:path-b1($x)
};

(: ----------------------------------------- :)
(: Execute embedded in FLWOR expressions     :)
(: ----------------------------------------- :)

declare function B:flwr() { 
    for $i in (1, 2, 3),
        $j in ("a", "b")
    return   A:id($i)
};

declare function B:free_var($x) {
    let $z := A:id($x)
    return $z
};

declare function B:one_free_var(){
    let $x := 1
    for $y in ("foo", 2.0, 6)
    let $z := A:id2($x, $y) 
    return ($x, $y, $z)
};

declare function B:call_out() {
  at server A do { 
    let server C implement S at "b" return {
      C:c()
    }
  }
};

