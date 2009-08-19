(:  test that document nodes in the replace content are expanded to their list of children nodes :)


declare variable $docA := document{<A><B>bb</B></A>};
declare variable $docB := document{<B><C>cc</C></B>};

replace node $docA/A/B with $docB
;
insert node <new/> into $docA/A
;

$docA



