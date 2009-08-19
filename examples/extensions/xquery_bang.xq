(: XQuery! Version:

     --> Inserts are applied when snap is closed, in reverse order as
         inserts keep being added to the pending update list within
         the snap.
     --> Count returns 4 since the updates are applied when the snap
         is closed.
:)

declare variable $log := <log count="0"/>;

declare updating function local:log($x) {
  insert node element item { $x } into $log
};

snap {
  avg(
    for $x in (1,2,3,4) return (local:log($x), $x)
  )
},
replace value of node $log/@count with count($log/*);

$log

