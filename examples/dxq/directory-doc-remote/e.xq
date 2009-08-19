module namespace E = "server";
import module namespace S = "dxq:server" at "s-int.xq";

declare function E:stats() {
  doc("query_response_size_e")
};

declare variable $dbase :=
  <dir>
    <entry name="Alice" value="a"/>
  </dir>;

declare function local:main() {
  <dir>
    { $dbase/entry }
    { exec {$dbase/entry[@name="Alice"]/@value} { S:main() }/entry }
    { let $d :=
        exec {$dbase/entry[@name="Alice"]/@value} { S:main() }
      return
        exec {$d/entry[@name="Claire"]/@value} { S:main() }/entry 
    } 
  </dir>
};

declare function E:main() {
  local:main()
};
