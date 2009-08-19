module namespace A = "server";
import module namespace S = "dxq:server" at "s-int.xq";

declare function A:stats(){
  doc("query_response_size_a")
};

declare variable $dfull :=
  <dir>
    <entry name="Alice" value="a"/>
    <entry name="Claire" value="c"/>
  </dir>;

declare variable $permissions_list  :=
  <perms>
  <entry name="Bob" resource="file /etc/hosts" op="modify"/>
  </perms>;

declare function A:stats(){
  doc("query_response_size_a")
};

declare function A:main() { 
  $dfull
};

(:
--------------------------------------------------------------------------
   Start Kristi's new code for supporting SD3 access control lists and
   permissions
--------------------------------------------------------------------------
:)

(:All "employee" groups get to read /etc/motd :)
declare function A:perms() {
     let $x := for $y in A:access()/entry[@groups="employee"]
              return
                <entry name="{$y/@name}" resource="file /etc/motd" op="read"/>
     return
     <perms>
       { $permissions_list/entry }
       { $x }
     </perms>
};

declare function A:perms_main() {
  exec {"a"} { A:perms() }
};

declare function A:access(){
 <group>
 <entry name="Alice" groups="wheel"/>
 <entry name="Alice" groups="employee"/>
 <entry name="Bob" groups="employee"/>
 </group>
};

declare function A:access_main(){
  exec {"a"} { A:access() }
};

declare function A:local_main(){
  A:access()
};

declare function A:add_permission($user, $res, $op) {
  let $pl :=
  <perms>
       {$permissions_list/entry}
       <entry name="{$user}" resource="{$res}" op="{$op}"/>
  </perms>
  return $pl
};
