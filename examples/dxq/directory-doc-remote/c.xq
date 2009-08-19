module namespace C = "server";
import module namespace S = "dxq:server" at "s-int.xq";

declare function C:stats() {
  doc("query_response_size_c")
};

declare variable $dbaseb :=
  <dir>  
    <entry name="Alice" value="a"/>
    <entry name="Bob" value="b"/>
    <entry name="Claire" value="c"/>
    <entry name="Doug" value="d"/>
  </dir>; 

declare function C:stats() {
  doc("query_response_size_b")
};

declare function C:main() {
  <dir>
    { $dbaseb/entry }
    { exec {($dbaseb/entry[@name="Bob"]/@value)} { S:main() }/entry }
  </dir>
};
