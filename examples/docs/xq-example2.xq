import module namespace xq-example1 = "xq-example1" at "../docs/xq-example1.xq";
declare variable $v external;
declare variable $x external;
declare variable $y external;
declare variable $z := ($y * 2) ; 
let $v := xq-example1:test2(xs:int(20), "user test") return $v

