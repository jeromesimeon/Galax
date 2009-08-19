(: Utility module of built-in functions for DXQ :)
module namespace dxq = "http://www.galaxquery.org/dxq";

(: Server functions :)
declare updating function dxq:my_address() external;

(: DNS functions :)

declare function dxq:dns-lt($x as xs:string, $y as xs:string) as xs:boolean external; 
declare function dxq:dns-le($x as xs:string, $y as xs:string) as xs:boolean external; 

(: Web GUI :)
declare updating function dxq:delay ($delay as xs:integer) as empty-sequence() external;
declare updating function dxq:gui-report ($msg as xs:string) as empty-sequence() external;
declare updating function dxq:gui-status ($kind as xs:string, $graph as item()) as empty-sequence() external;

(: Mutexes :)
declare updating function dxq:mcreate () as xs:integer external;
declare updating function dxq:lock ($mutex as xs:integer) as empty-sequence() external;
declare updating function dxq:try_lock ($mutex as xs:integer) as xs:boolean external;
declare updating function dxq:unlock ($mutex as xs:integer) as empty-sequence() external;

(: Condition variables :)
declare updating function dxq:cvcreate() as xs:integer external;
declare updating function dxq:signal($cv as xs:integer) as empty-sequence() external;
declare updating function dxq:broadcast($cv as xs:integer) as empty-sequence() external;
declare updating function dxq:wait($cv as xs:integer, $mutex as xs:integer) as empty-sequence() external;

(: Bit functions :)
(: shift_right_logical x y shifts x to the right by y bits. This is
a logical shift: zeroes are inserted in the vacated bits regardless of
the sign of x. The result is unspecified if y < 0 or y >= 32. :)
declare function dxq:shift_right_logical($x as xs:integer, $y as xs:integer) as xs:integer external;
declare function dxq:shift_left_logical($x as xs:integer, $y as xs:integer) as xs:integer external;
declare function dxq:logical_xor ($x as xs:integer, $y as xs:integer) as xs:integer external; 
declare function dxq:logical_and ($x as xs:integer, $y as xs:integer) as xs:integer external; 
declare function dxq:logical_or ($x as xs:integer, $y as xs:integer) as xs:integer external; 

declare function dxq:nlz ($x as xs:integer) as xs:integer external; 
