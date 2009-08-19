module namespace A = "server";
import interface namespace S = "server" at "server.xqi";
import module namespace dxq = "http://www.galaxquery.org/dxq" at "../dxq.xq";
declare namespace foo = "Foo";
declare namespace baz = "Baz";
declare server Self implements S at "a";
declare server B implements S at "b";

declare variable $A:v := "v";
declare function A:main() { <a><b>hello world</b></a> };

(: ----------------------- :)
(: Constant functions      :)
(: Tests return values ONLY :)
(: ----------------------- :)
declare updating function A:local() { 
  dxq:my_address()
};

declare function A:error() {
  fn:error("Error in server A") 
};

declare function A:c() { 
  "const"
};

declare function A:s() { 
  "string"
};

declare function A:i() { 
  1
};

declare function A:d() { 
  2.0
};

declare function A:ilist() { 
  (1,2,3)
};

declare function A:mixed() { 
  (1,<a/>,"string")
};

declare function A:attr() { 
  attribute {"a"}{1}
};

declare function A:elem() { 
  <foo:bar><a/><baz:b/></foo:bar>
};

declare function A:primitive() { 
  xs:string("Hello"),
  xs:boolean("false"),
  xs:decimal("2.0"),
  xs:float("1267.43233E12"),
  xs:double("INF"),
  xs:dayTimeDuration("P10D"),	
  xs:dateTime('1991-12-31T23:00:00.0'),
  xs:time('23:00:00.0'),
  xs:date('1991-12-31'),
  xs:gYearMonth('2006-07'),
  xs:gYear('2006'),
  xs:gMonthDay('--06-22'),
  xs:gDay('---31'),
  xs:gMonth('--06'),
  xs:hexBinary('111110110111'),
  xs:base64Binary('111110110111'),
  xs:anyURI("http://slashdot.org")
 (: xs:QName("{http://slashdot.org,foo}")
   xs:NOTATION() :) 
  
  
};
(: ----------------------------------------- :)
(: Identity functions                        :)
(: Tests round-trip of function arguments    :)
(: ----------------------------------------- :)
declare function A:id($x) { $x };
declare function A:id2($x,$y) { $x,$y };
declare function A:id3($x,$y,$z) { ($x,$y,$z) };
declare function A:id4($x,$y,$z,$w) { ($x,$y,$z,$w) };


(: ------------------------------ :)
(: Path expressions               :)
(: ------------------------------ :)
declare function A:value() { 
  <a><b><c>1</c></b><b><c>2</c></b><b><c>3</c></b></a>
};

declare function A:path-b1($x) { 
  $x/b
};

declare function A:records() { 
  <rr><a id="1"/>
      <a id="2"/>
      <a id="3"/>
      <b id="1"><c id="1"/></b>
      <b id="2"><c id="2"/></b>
      <b id="3"><c id="3"/></b>
  </rr>
};

(: This generates the following error: 
<glx:error xmlns:glx="http://www.galaxquery.org">Algebra Parsing Error: Expecting alg:Expression did not find it found: alg:Env | expression parser | closure</glx:error> :)

(: declare function A:free_var($x){
  let $z := exec {"b"}{B:g($x)}
  return $z
};:)

(:This will be fixed once the INPUT value is shipped rather than i_1, it's element tuple.  Here's the error message we see:
Error: DXQ Server: Physical Type Error: Tuple field unknown:i_1 not found in physical type [] :)

(: declare function A:free_var(){
  for $i in (1, 2, 3)
  let $z :=  exec {"b"}{B:g($i)} 
  return $z
};

:)
