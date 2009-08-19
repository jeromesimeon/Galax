declare function local:gd($dname) {
  doc(concat("dxq://",$dname))/dir
};
declare function local:qd($d,$n) {
  $d/entry[@name=$n]/@value 
};
declare function local:qn($dname,$n) {
  local:qd(local:gd($dname),$n)
};

declare variable $dbase :=
 <dir>  
 <entry name="Alice" value="a"/>
 </dir>;
declare variable $dfull :=
 <dir>
 { $dbase/entry }
 { local:gd(local:qd($dbase,"Alice"))/entry }
 { local:gd(local:qn(local:qd($dbase,"Alice"),"Bob"))/entry }
 </dir>;
declare function local:main() {
  $dfull
};

