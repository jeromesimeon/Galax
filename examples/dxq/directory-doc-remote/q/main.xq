import module namespace A = "server" at "../a.xq";

<a>{ 
  glx:remote-plan("localhost", 3000, A:main())
}</a>;

<b>{ 
  glx:remote-plan("localhost", 3001, A:main())
}</b>;

<c>{ 
  glx:remote-plan("localhost", 3002, A:main())
}</c>;

<d>{ 
  glx:remote-plan("localhost", 3003, A:main())
}</d>;

<e>{ 
  glx:remote-plan("localhost", 3004, A:main())
}</e>;

