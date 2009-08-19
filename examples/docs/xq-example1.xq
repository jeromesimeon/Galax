module namespace xq-example1 = "xq-example1";

declare namespace foo = "http://www.foo.com";

declare function xq-example1:test0() as xs:string { 
  "test0"
};

declare function xq-example1:test1($d as document-node()) as document-node() { 
  $d
};

declare function xq-example1:test2($i as xs:integer, $j as xs:string) as xs:string { 
  $j
};

(: A simple XQuery function, which constructs an HTML value :)
(: For now, static typing is disable, so return type is just an element :)
declare function xq-example1:to_html($d as document-node(), $i as xs:integer, $s as xs:string, $f as xs:float) as element() {
  <html>
    <ul>
      <li>{ $f }</li> 
      <li>{ $s }</li>
      <li>{ $i }</li>
      { $d//li }
    </ul>
  </html>
};

