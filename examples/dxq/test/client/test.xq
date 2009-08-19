import module namespace T = "dxq:server" at "../server.xqi";
declare namespace foo = "Foo";
declare namespace baz = "Baz";

declare function local:id($x) { $x };

for $a in exec { "localhost:3000" }{ T:records()/a }
for $b in exec { "localhost:3000" }{ T:records()/b }
where $a/@id = $b/@id
return local:id($b/c)

(:
exec { "localhost:3001" } { T:c() } = "const" ;
exec { "localhost:3001" } { T:i() } = 1;
exec { "localhost:3001" } { T:d() } = 2.0;
deep-equal(exec { "localhost:3001" } { T:mixed() }, (1,<a/>,"string"));
deep-equal(exec { "localhost:3001" } { T:attr() }, attribute {"a"}{1});
deep-equal(exec { "localhost:3001" } { T:elem() }, <foo:bar><a/><baz:b/></foo:bar>);
deep-equal(exec { "localhost:3001" } { T:primitive() },
  (xs:string("Hello"),
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
  xs:anyURI("http://slashdot.org")));

(: This intentionally throws an error :)
exec { "localhost:3001" } { T:error() };
:)