(: declare namespace C = "chord"; :)
(: import module namespace S = "dxq:chord" at "chord.xqi"; :)

declare function glx:mcreate () as xs:integer external;
declare function glx:delay ($delay as xs:integer) as empty-sequence() external;
declare function glx:lock ($mutex as xs:integer) as empty-sequence() external;
declare function glx:try_lock ($mutex as xs:integer) as xs:boolean external;
declare function glx:unlock ($mutex as xs:integer) as empty-sequence() external;

(: glx:my_address() returns DXQ server state and therefore cannot be
   pushed into a remote exec. 

   Same is true for all Mutex functions.

   Would labeling them as "updating", i.e., has side effects be good enough?
:)
declare function glx:my_address() external;

declare variable $m external; (: m = ceiling(log(N)) :)
declare variable $finger := 
  <fingers>
    <finger idx="{}" node="{}" start="{}">
    </finger>
  </fingers>; 
declare variable $fingers_mutex :=  glx:mcreate(); 

(: Return closest finger preceding id [Chord, pg 5]

   n.closest_preceding_finger(id) ==
     for i = m downto 1
       if (finger[i].node in (n,id))
         return finger[i].node;
     return n;
:)
declare function closest_preceding_finger($id) {
};

(: Ask node n to find id's predecessor [Chord, pg 5]
   
   n.find_predecessor(id) ==
     n' = n;
     while ( id not in (n', n'.successor] )
       n' = n'.closest_preceding_finger(id); 
     return n';
:)
declare function find_precedessor($id) {
};

(: Ask node n to find id's successor [Chord, pg 5]

   n.find_successor(id) ==
     n' = find_predecessor(id); 
     return n'.successor();
:)
declare function find_successor($id) {
};

(: Finger table :)
(: #define successor finger[1].node  [Chord, pg 6]  :)

(: Question: where is m defined?

   [Chord, pg 6]
   Node n joins the network; 
   n' is an arbitrary node in the network 

   n.join(n') == 

     if (n')
       init_finger_table(n');
       // Move keys in (predecessor, n] from successor
       update_others();
     
     else 
       // n is ht eonly node in the network
       for i = 1 to m
         finger[i].node = n;
       predecessor = n;   
:)
declare function join($nprime) {
};

(: [Chord, pg 6]
   Initialize finger table of local node;
   n' is an arbitrary node already in the network

   n.init_finger_table(n') ==
     finger[1].node = n'.find_successor(finger[1].start);
     predecessor = successor.predecessor;
     successor.predecessor = n;
     for i = 1 to m - 1 
       if (finger[i+1].start in [n, finger[i].node))
         finger[i+1].node = finger[i].node;
       else 
         finger[i+1].node = n'.find_successor(finger[i+1].start);
:)
declare function init_finger_table($nprime) {
};

(: [Chord, pg 6]
   Update all nodes whose finger tables should refer to n

   n.update_others() ==
     for i = 1 to m
       // Find last node p whose i-th finger might be n
       p = find_predecessor(n = 2^(i-1));
       p.update_finger_table(n,i);
:)
declare function update_others() {
};

(: [Chord, pg 6]
   If s is i-th finger of n, update n's finger table with s
   
   n.update_finger_table(s,i) ==
     if (s in [n, finger[i].node))
       finger[i].node = s;
       p = predecessor; // Get first node preceding n
       p.update_finger_table(s,i);
:)
declare function update_finger_table($s,$i) {
};     

(: [Chord, pg 7]
   Stabilization.  This definition of join replaces join above.

   n.join(n') ==
     predecessor = nil;
     successor = n'.find_successor(n);
:)
declare function join($nprime) {
};

(: [Chord, pg 7]
   Periodically verify n's immediate successor, and tell the successor
   about n.

   n.stabilize() ==
     x = successor.predecessor;
     if (x in (n,successor))  ?? Is this an open interval ??
       successor = x;
     successor.notify(n); 
:)
declare function stabilize() {
};

(: [Chord, pg 7]
   n' thinks it might be our predecessor

   n.notify(n') ==
     if (predecessor is nil or n' in (predecessor, n))
       predecessor = n;'
:)
declare function notify($nprime) {
};
 
(: [Chord, pg 7]
   Periodically refresh finger table entries.
  
   n.fix_fingers() ==
     i = random index > 1 into finger[];
     finger[i].node = find_successor(finger[i].start);
:)
declare function fix_fingers() {
};
