(: Update Facility Version:

     --> Inserts are in reverse order as inserts keep being added to
         the pending update list.

     --> Count returns 0 since the updates are applied only after
         query evaluation terminates.
:)

declare variable $log := <log count="0"/>;

declare updating function local:log($x) {
  insert node element item { $x } into $log
};

avg(
  for $x in (1,2,3,4) return (local:log($x), $x)
),
replace value of node $log/@count with count($log/*);

$log

