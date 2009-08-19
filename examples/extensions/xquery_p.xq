(: XQueryP Version:

     --> Inserts are applied immediatly, in the order as
         they occur.
     --> Count returns 4 since the inserts have been applied when it
         is computed.
:)

declare variable $log := <log count="0"/>;

declare updating function local:log($x) {
  insert node element item { $x } into $log
};

{ declare $count := 0;
  avg(
    for $x in (1,2,3,4) return {local:log($x); set $count := $count + 1; $x}
  ),
  replace value of node $log/@count with $count }
;

$log

