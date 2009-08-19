declare variable $doc := <doc/>;

$doc;

insert nodes (<x1/>,<x2/>) into $doc;

$doc;

let $x := (<x3/>,<x4/>) return
insert nodes <x>{count($x)}</x> into $doc;

$doc

