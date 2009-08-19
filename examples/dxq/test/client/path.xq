import module namespace A = "server" at "../pathserver.xq";

(: 
   Pushing navigation ("axis steps")

   The plan for the following expression:
:)
glx:remote-plan("localhost", 3000, A:main())/entry;
(: 
   Should be rewritten into the plan for this expression:
:)
glx:remote-plan("localhost", 3000, A:main()/entry);

(:
   Pushing selections.

   The plan for the following expression:
:)
glx:remote-plan("localhost", 3000, A:main())/entry[@name="Claire"]; 
(: 
   Should be rewritten into the plan for this expression:
:)
glx:remote-plan("localhost", 3000, A:main()/entry[@name="Claire"]); 

(: 
  The above two rewritings work because the expressions being pushed
  through the Execute operator are "closed", i.e., they do not refer
  to any free variables.   We have more work to do to push expressions
  that contain free variables. 
:)

(: 

  It's not clear whether XML constructors should be pushed through
  Execute() because they (may) increase the size of the result value.
  But from a semantics standpoint, they are easy to push.  This
  rewriting is *NOT* implemented yet. 

  Pushing constructors with literal names: 

:)
<foobar>{ 
glx:remote-plan("localhost", 3000, A:main())/entry
}</foobar>;

glx:remote-plan("localhost", 3000, <foobar>{A:main()/entry}</foobar>);

(: 
  And element constructors with dynamic names: 
:)
element { "foobar" } {
glx:remote-plan("localhost", 3000, A:main())/entry
};

glx:remote-plan("localhost", 3000, element { "foobar" } { A:main()/entry } ); 


(: 

  We have two heuristics for applying a rewriting:

    1. Reduces number of messages to a particular remote host and/or
    2. Reduces size of network data 

  Here are some other rewritings that we need to consider.
  I think we should be "application driven" w.r.t. rewritings.  Focus
  on the BIG inefficiencies first. 

  if ( Expr ) then execute { Expr } else 
  if (execute { Expr }) then  else 

  let $x := execute { Expr } 
  return Expr_$x  

  let $x := Expr
  return execute { Expr_$x }

  Sequence operator:
  h1 = h2, p1 = h2
  ( execute { h1, p1, Expr1 }, execute { h2, p2, Expr2 } )
  ====
  execute { h1, p1, (Expr1, Expr2) }

  element foo { execute { Expr } ) 
  ====
  execute { element foo { Expr } } 

  
:)
