(: don't forget to start the Jabber server
   and to add the path to the Galax libraries to
   your LD_LIBRARY_PATH :)

declare namespace jbrdelay = "jabber:x:delay";

glx:jabber-buddies ("nicola@localhost/foobar", "nicola", xs:int("1"), fn:false())
(: glx:jabber-buddies ("jerome@localhost/tests", "jerome",xs:int("1"), fn:false()) :)
