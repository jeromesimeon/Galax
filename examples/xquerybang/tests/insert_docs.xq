(:  test that document nodes in the insert content are expanded to their list of children nodes :)


declare variable $docA := document{<A><B>bb</B></A>};
declare variable $docB := document{<B><C>cc</C></B>};

insert node <D/> into $docA/A
;
insert node $docB into $docA/A
;

$docA

