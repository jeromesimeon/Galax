module namespace Mutex = "mutex";
declare function Mutex:mcreate () as xs:int external;
declare function Mutex:delay ($delay as xs:int) as empty-sequence() external;
declare function Mutex:lock ($mutex as xs:int) as empty-sequence() external;
declare function Mutex:try_lock ($mutex as xs:int) as xs:boolean external;
declare function Mutex:unlock ($mutex as xs:int) as empty-sequence() external;
