module namespace N = "narada";
import interface namespace S = "narada" at "narada.xqi";
import module namespace dxq = "http://www.galaxquery.org/dxq" at "../../dxq.xq";
declare variable $local:self := "n1";
(:
declare namespace N = "narada";
import interface namespace S = "narada" at "narada.xqi";
declare variable $local:self := "n1";
:)

(: declare server namespace Self implement S at $local:self;  :)

(:---------------------------------------------------------:)
(: Node state                                              :)
(:---------------------------------------------------------:)
(: Node state variable :)
declare variable $local:status := "alive";       (: departing or dead :)
declare variable $local:start := glx:gettime();  (: Start time :)

(: Node's sequence number :)
declare variable $local:sequence := 0;
declare variable $local:sequence_mutex :=  dxq:mcreate(); 

(: Maximum time before a neighbor is considered dead. :)
declare variable $local:T_m := 60.0;  

(: Neighbors :)
declare variable $local:neighbors := (); 
declare variable $local:neighbors_mutex :=  dxq:mcreate(); 

(: Members :)
declare variable $local:members := ();
declare variable $local:members_mutex :=  dxq:mcreate(); 

(: Routing info :)
declare variable $local:routes := ();
declare variable $local:routes_mutex :=  dxq:mcreate(); 

(:---------------------------------------------------------:)
(: Debugging                                               :)
(:---------------------------------------------------------:)
declare updating function N:neighborTimes() {
  <nTimes>{
    for $n in $local:neighbors/neighbor,
        $m in $local:members/member[@address = $n/@address]
    return glx:gettime() - $m/time
  }</nTimes>
};
declare updating function N:latencyTo($target) {
  let server T implement S at $target return 
  let $t := glx:gettime()
  return {
    from server T return 1;
    glx:gettime() - $t
  }
};
(:---------------------------------------------------------:)
(: Initialize global state                                 :)
(:---------------------------------------------------------:)
declare updating function N:init() { 
  set $local:status := "alive";

  let $fst_neighbors := fn:doc("latencies.xml")/config/neighbors/neighbor[@s=$local:self] return 

  if (fn:empty($fst_neighbors)) then 
    fn:error((), "First neighbors are not known")  
  else {
    let $nbrs := for $n in $fst_neighbors return <neighbor address="{$n/@t}"/>
    return {
      dxq:lock($local:neighbors_mutex);
        set $local:neighbors := <neighbors>{$nbrs}</neighbors>; 
      dxq:unlock($local:neighbors_mutex)
    };
  
    dxq:lock($local:members_mutex);
      set $local:members := 
      (: Self is included in members with liveness true. :)
       <members>
         <member address="{$local:self}">
           <sequence>{ $local:sequence }</sequence>
           <live>1</live>
         </member>
       </members>; 
    dxq:unlock($local:members_mutex);
    
    dxq:lock($local:routes_mutex);
    dxq:lock($local:neighbors_mutex);
      set $local:routes := 
      <routes> 
        <route address="{$local:self}" distance="0" latency="0"/>
        { for $n in $local:neighbors/neighbor
          let $lat := N:latencyTo($n/@address) 
          return 
            <route address="{$n/@address}" distance="1" latency="{$lat}">
              <hop from="{$n/@address}" />
            </route>
        }
      </routes>;
    dxq:unlock($local:neighbors_mutex);
    dxq:unlock($local:routes_mutex)
  };
  N:reportNeighborGraph()
};

(:
  refreshProbe() 
  Increment local sequence number and update local node's member info.

  In original Narada paper, this neighbor synchronization is done in
  one message, but unclear from Hellerstein paper how many messages
  are actually sent.
:)
declare updating function N:refreshProbe() {
(:   dxq:gui-report(fn:string-join(("N:refreshProbe@",$local:self),""));   :)
   dxq:lock($local:sequence_mutex);
     set $local:sequence := $local:sequence + 1;  
     replace value of node $local:members/member[1]/sequence with $local:sequence;
   dxq:unlock($local:sequence_mutex);

  dxq:lock($local:neighbors_mutex);
  dxq:lock($local:members_mutex);
   for $y in $local:neighbors/neighbor (: , $m in $local:members/member :)
   return {
(:   dxq:gui-report(fn:string-join(("self = ",$local:self," => ",$y/@address)," "));  :)
     let server T implement S at $y/@address 
     return at server T do 
       for $m in $local:members/member return
       T:refreshMsg($local:self, fn:data($m/@address), fn:data($m/sequence), fn:data($m/live))
   };
  dxq:unlock($local:members_mutex);
  dxq:unlock($local:neighbors_mutex)
};

(: If this is first time I've heard from neighbor, add to neighbor table :)
declare updating function N:insertNeighbor($a,$optlat) { 
  if (fn:empty($local:neighbors/neighbor[@address = $a])) then {
    dxq:lock($local:neighbors_mutex);

    (: Only add neighbor if its exceeds threshold of STUPID utility function :)
    let $memct := fn:count($local:members/member) return 
(:    if (fn:count($local:neighbors/neighbor) < ($memct*0.2)) then { :)
    if (true()) then { 
      insert node <neighbor address="{$a}"/>
      into $local:neighbors;
      (: Report status change :)
      N:reportNeighborGraph();

      let $lat := (if (fn:empty($optlat)) then N:latencyTo($a) else $optlat) return {
        dxq:lock($local:routes_mutex);
        let $route := $local:routes/route[@address=$a] return
        let $new_route :=
          <route address="{$a}" distance="1" latency="{$lat}">
            <hop from="{$a}"/>
          </route>
        return
          if (fn:empty($route)) then {
            insert node $new_route into $local:routes
          } else {
            replace node $route with $new_route
          };
        dxq:unlock($local:routes_mutex)
      }
    }
    else ();
    dxq:unlock($local:neighbors_mutex)
  }
  (: Already a neighbor :)
  else ()
};

declare updating function N:refreshMsg($y, $address, $seq, $live) {
(:  dxq:gui-report(fn:string-join(("N:refreshMsg@",$local:self," From $y =
 ",$y," addr=",$address," seq=",$seq), ""));  :)

(:  fn:trace((),"refreshMsg"); :)
  if ($local:status = "alive") then {
    let $t := glx:gettime() 
    return
    let $m := $local:members/member[@address = $address] return
    if ($local:self != $address) then 
      (: If new member is not in local table, insert it. :)
      if (fn:empty($m)) then {
        dxq:lock($local:members_mutex);
          insert node 
            <member address="{$address}">
              <sequence>{$seq}</sequence>
              <time>{$t}</time>
              <live>{$live}</live> 
            </member>
          into $local:members;
          N:reportMemberGraph();
        dxq:unlock($local:members_mutex)
      }
      (: Otherwise if member is already in table and member's sequence
         number is greater than mine, update table.  :)
      else {
        (: We actually need to lock on read as well... :)
        dxq:lock($local:members_mutex);
          if ($m/sequence cast as xs:integer < $seq cast as xs:integer) then {
              replace value of node $m/sequence with ($seq cast as xs:integer);
              replace value of node $m/time with $t;
              replace value of node $m/live with $live
          }
          else ();
        dxq:unlock($local:members_mutex)
      }
    else ();
    N:insertNeighbor($y, ())
  }
  else ()	
};

(: deadNeighbor($n)  Delete a neighbor and mark as dead in member table. :)
declare updating function N:deadNeighbor($n) {
  let $t := glx:gettime(), 
      $addr := $n/@address 
  return 
  { 
    dxq:lock($local:neighbors_mutex);  
      delete node $n;
      (: Report status change :)
      N:reportNeighborGraph();
    dxq:unlock($local:neighbors_mutex); 

    dxq:lock($local:members_mutex); 
      let $m := $local:members/member[@address = $addr] return {
        replace value of node $m/live with 0;
        replace value of node $m/sequence with $m/sequence + 1
      };
      (: Report status change :)
      N:reportMemberGraph();
    dxq:unlock($local:members_mutex); 

    dxq:lock($local:routes_mutex); 
      for $r in $local:routes/route[hop/@from=$addr or hop/@to=$addr]
      return { 
	dxq:gui-report(fn:string-join(($local:self,"deleting route to",$r/@address)," "));
        delete node $r
      };
      (: Report status change :)
      N:reportRoutingGraph();
    dxq:unlock($local:routes_mutex) 
  }
};

(: neighborProbe() 
   If a neighbor doesn't respond, mark as dead, and initiate partition
   detection and recovery. 
:)
declare updating function N:neighborProbe() {
(:  dxq:gui-report(fn:string-join(("NeighborProbe@",$local:self),"")); :)
(:  fn:trace((),"neighborProbe"); :)
  let $t := glx:gettime() return
  for $n in $local:neighbors/neighbor, 
      $m in $local:members/member[$n/@address = @address]
  let $diff := $t - $m/time
  where $diff > $local:T_m
  return {
    dxq:gui-report(fn:string-join(($local:self,"thinks",$n/@address,"is dead",fn:string($diff),"after",fn:string($t - $local:start))," "));
    N:deadNeighbor($n)  
    (: TODO: Partition detection and recovery :)
  }
};

(: routingProbe()
   Send my routing table to all of my neighbors 
:)
declare updating function N:routingProbe() { 
(:  dxq:gui-report(fn:string-join(("N:routingProbe@",$local:self),"")); :)
  for $n in $local:neighbors/neighbor
  return
    let server T implement S at $n/@address 
    return 
      at server T do T:routingTableMsg($local:self, $local:routes)
};

declare updating function N:routingTableMsg($neighbor, $neighbor_routes)
{
  dxq:lock($local:routes_mutex);
    let $lat := $local:routes/route[@address = $neighbor]/@latency return
    for $neighbor_route in $neighbor_routes/route 
    where fn:empty($neighbor_route/hop[(@to|@from)=$local:self])
    return 
      let $route := $local:routes/route[@address = $neighbor_route/@address]
      return 
        let $next_hop := $neighbor_route/hop[1]/@from return
        let $new_route :=  
           <route address="{$neighbor_route/@address}" 
                   distance="{$neighbor_route/@distance + 1}"
                   latency="{$lat + $neighbor_route/@latency}">{
              (<hop from="{$neighbor}" to="{$next_hop}"/>,$neighbor_route/hop)
            }</route>
        return
        if (fn:empty($route)) then {
          insert node $new_route into $local:routes;
          N:reportRoutingGraph()
        } else {
          if ($neighbor_route/@distance + 1 < $route/@distance) then { 
            replace node $route with $new_route;
            N:reportRoutingGraph()
          }
          else ()
        };
  dxq:unlock($local:routes_mutex)
};

(: latencyProbe()
   Randomly choose a member, other than self, and probe for latency.
:)
declare updating function N:latencyProbe() { 
(:  dxq:gui-report(fn:string-join(("N:latencyProbe@",$local:self),"")); :)
  let $memct := fn:count($local:members/member) - 1
  return 
    if ($memct > 0) then 
     (: First member is always self, so randomly select member after
        first. :)
      let $r := glx:random_int($memct) + 2, 
          (: Locking necessary on read? :)
          $y := $local:members/member[$r], 
          $t := glx:gettime()
      return 
        let server T implement S at $y/@address 
        return  
          at server T do T:pingMsg($local:self,$t)
    else ()
};

(: pingMsg($sender,$time) 
   Pong the event back to sender with sender's time. 
:)
declare updating function N:pingMsg($x,$t) { 
  if ($local:status = "alive") then 
    let server T implement S at $x 
    return 
      at server T do T:pongMsg($local:self,$t)
  else () 
};

(: pongMsg($member,$time)  Update latency received from $member. 
   Measure utility of adding that member as a neighbor. 
:)
declare updating function N:pongMsg($a,$t1) { 
  if ($local:status = "alive") then 
    let $t2 := glx:gettime(), 
        $latency := ($t2 - $t1), 
        $m := $local:members/member[@address = $a] 
    (: This works because members are never deleted :)
    return {
      dxq:lock($local:members_mutex);
        if ($m/latency) then {
          replace value of node $m/latency with $latency
        } else {
          insert node <latency>{$latency}</latency> into $m
        };
      dxq:unlock($local:members_mutex);
      for $route in 
        $local:routes/route[@address= $a][@latency cast as xs:integer > $latency cast as xs:integer] 
      return 
        (: Cheaper to talk to this member directly than to go through established route :)
        N:insertNeighbor($a, $latency)
    }
  else ()
};

(: neighborDepartMsg($a)  When neighbor departs, mark as dead. :)
declare updating function N:neighborDepartMsg($a) {
 if ($local:status = "alive") then 
   N:deadNeighbor($local:neighbors/neighbor[@address=$a])
 else ()
};

(:
  Protocol messages
:)
declare updating function N:refreshLoop($delay) {
  while (true()) return {
    if ($local:status = "alive") then {
(:  fn:trace($local:self, "N:refreshLoop");  :)
      N:refreshProbe()
    } else ();
    dxq:delay($delay)
  }
};
declare updating function N:latencyLoop($delay) {
  while (true()) return {
    if ($local:status = "alive") then  {
(:  fn:trace($local:self, "N:latencyLoop");  :)
      N:latencyProbe()
    } else (); 
    dxq:delay($delay)
  }
};
declare updating function N:neighborLoop($delay) {
  while (true()) return {
    if ($local:status = "alive") then {
(:  fn:trace($local:self, "N:neighborProbe");  :)
      N:neighborProbe()
    }
    else ();
    dxq:delay($delay)
  }
};
declare updating function N:routingLoop($delay) {
  while (true()) return {
    if ($local:status = "alive") then {
(:  fn:trace($local:self, "N:routingProbe");  :)
      N:routingProbe()
    }     
    else (); 
    dxq:delay($delay)
  }
};

(: N:go() sends:
   a refreshProbe message once every two seconds,
   a latencyProbe message once every three seconds, and
   a neighborProbe message once every four seconds, and 
   a routingProbe message once every five seconds.
:)
declare updating function N:go() {
  set $local:status := "alive"; 
(:  N:init();  :)
  (: Add a delay so that all the servers are initialized 
     before they start exchanging messages. :)
(:  dxq:delay(2);
  N:reportNeighborGraph(); 
:)
  let server Self implement S at $local:self return {
    at server Self do N:refreshLoop(3); 
    at server Self do N:routingLoop(5);
    at server Self do N:latencyLoop(7);
    at server Self do N:neighborLoop(9)
  }
};

(: N:depart()  Explicitly depart from network. :)
declare updating function N:depart() { 
  set $local:status := "departing";
  dxq:lock($local:neighbors_mutex);  
  for $n in $local:neighbors/neighbor return delete node $n;
  dxq:unlock($local:neighbors_mutex);  
  for $n in $local:neighbors/neighbor return
    let server T implement S at $n/@address 
    return 
      at server T do T:neighborDepartMsg($local:self);
  dxq:delay(3); (: Wait a while before dying :)
  set $local:status := "dead";
  (: A hack to tell GUI that node has died intentionally :)
  dxq:gui-report("DEAD!");
  N:reportNeighborGraph()
};

(: N:pause() :)
declare updating function N:pause() { 
  set $local:status := "dead"
};

declare updating function N:continue() { 
  set $local:status := "alive"
};

(: N:die()  Die without warning. :)
declare updating function N:die() { 
  set $local:status := "dead";
  dxq:lock($local:neighbors_mutex);  
    for $n in $local:neighbors/neighbor return delete node $n;
  dxq:unlock($local:neighbors_mutex);  
  (: A hack to tell GUI that node has died intentionally :)
  dxq:gui-report("DEAD!");
  N:reportNeighborGraph()
};

declare function N:graphEdge($s,$t,$l,$w) { 
  <edge s="{$s}" t="{$t}" l="{$l}" w="{$w}"/>
};

declare updating function N:reportNeighborGraph() {
  let $graph := 
    <graph name="Neighbors">{
      for $n in $local:neighbors/neighbor return 
      let $l := $local:members/member[@address = $n/@address]/latency return
        N:graphEdge($local:self,$n/@address,$l,2)
    }</graph>
  return 
  dxq:gui-status("l", $graph)
};

declare updating function N:reportMemberGraph() {
  let $graph := 
    <graph name="Members">{
    for $m in $local:members/member[live = "1"] return 
      if (fn:not($local:self = $m/@address)) then 
        N:graphEdge($local:self,$m/@address,"",7)
      else ()
    }</graph>
  return 
  dxq:gui-status("g", $graph)
};

(:
  Routing Graph: Illustrated using weighted edges.
:)
declare updating function N:reportRoutingGraph() {
  let $edges := 
    for $f in distinct-values($local:routes/route/hop/@from),
        $t in distinct-values($local:routes/route/hop/@to) 
    where fn:not($t = "")
    return 
    let $wt := fn:count($local:routes/route/hop[@from = $f][@to = $t])
    return if ($wt > 0) then N:graphEdge($f,$t,"",$wt*3) else ()
  return

  let $neighbor_edges := 
    for $n in $local:neighbors/neighbor return
      N:graphEdge($local:self,$n/@address,"", 
        fn:max($edges[@s = $n/@address]/@w))
  return

  let $graph := 
    <graph name="Routes">{ 
      ($neighbor_edges, $edges)
    }</graph>
  return 
  dxq:gui-status("g", $graph)
};

declare updating function N:multicast($from,$msg) { 
  dxq:gui-report(fn:string-join(("Multicast message received!", $msg), " "));
  for $n in $local:neighbors/neighbor return 
    if (fn:not($n/@address = $from)) then 
      let server T implement S at $n/@address 
      return at server T do T:multicast(($from,$local:self),$msg)
    else ()
};

declare updating function N:unicast($to,$msg) { 
  if ($to = $local:self) then 
    dxq:gui-report(fn:string-join(("Unicast message received!", $msg), " "))
  else 
    for $route in $local:routes/route[@address=$to]
    let $hop := $route/hop[1]
    return 
      let server T implement S at $hop/@from
      return at server T do T:unicast($to,$msg)
};

(:

      for $r in $local:routes/route 
      let $lbl := fn:string-join($r/hop/@from, ",")

   From Narada Paper:

   A. Group management 

      Each node issues a refreshEvent() message to its neighbors
      periodically, with monotonically increasing sequence number.  If
      node has not received an update from another member for some
      time T_m, assume member is dead or partioned.  Node then
      initiates actions to repair partition.  Updates sent to
      neighbors.  Distance vector routing algorithm run on top of
      mesh. 
  
      1) Member join: 
  
         Bootstrap list must include one active group member.

      2) Member leave and failure: 

         Explicit leaving of group reported to neighbors.  Leaving
         member continues to fwd packets to minimize transient packet
         loss.

         Node maintains Q of members from which node has not received
         sequence number updates for at least time T_m.  

         ??? How is Q initialized ???

   B. Mesh performance

      Nodes probe each other at random and new links may be added
      depending on perceived utility gain.  Conversely, nodes monitor
      utility of existing links & drop links perceived as not useful. 

   Partition the network every once in a while.  How do we see this?


  let $max_route := fn:max(for $r in $local:routes/route return fn:count($r/hop)),
  for $distance in 1 to $max_route - 1 return
  let $hops := 
    for $r in $local:routes/route return 
    for $i in 1 to ($max - 1) 
    let $fst := $r/hop[$i],
        $snd := $r/hop[$i+1]
    return
      if (fn:not(fn:empty($fst)) and fn:not(fn:empty($snd))) then
        <hop from="{$fst}" to="{$snd}"/>
      else ()
  return 
  for $src in fn:distinct-values($hops/@from) 
  return 
    let $lbl := fn:string-join($hops[@from = $src]/@to/string(), ",")
    return 
      N:graphEdge($src,$m/@address,"M")
    }</graph>
  return 
  dxq:gui-status($graph)


:)

