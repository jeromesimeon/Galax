declare function local:gd($dname) {
  doc(concat("dxq://",$dname))/dir
};
declare function local:qd($d,$n) {
  $d/entry[@name=$n]/@value 
};
declare function local:qn($dname,$n) {
  local:qd(local:gd($dname),$n)
};

declare variable $dfull :=
 <dir>  
 <entry name="Claire" value="c"/>
 <entry name="Edgar" value="e"/>
 </dir>;
declare function local:main() {
  $dfull
};

