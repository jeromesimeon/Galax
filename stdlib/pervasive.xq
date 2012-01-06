module namespace glx = "http://www.galaxquery.org";

(: :::::::::::::::::
     F&O functions
   ::::::::::::::::: :)

(: F&O Section 2. Accessors :)

declare function fn:node-name($arg1 as node()?)    as xs:QName?   	 external;
declare function fn:nilled($arg1 as node()?)       as xs:boolean? 	 external;
declare function fn:string($arg1 as item()?)       as xs:string   	 external;
declare function fn:data($arg1 as item()*)         as xs:anyAtomicType* external;
declare function fn:base-uri($arg1 as node()?)     as xs:string?  	 external;
declare function fn:document-uri($arg1 as node()?) as xs:string?  	 external;

(: F&O Section 4. Trace function :)

declare updating function fn:trace($arg1 as item()*, $arg2 as xs:string) as item()* external;

(: F&O Section 5. Constructor functions :)

(:
   Galax supports constructor functions for xs:string, xs:boolean,
   xs:decimal, xs:float, xs:double, xs:integer, and xs:int.  Galax
   does not support any user-declared simple types.
:)

(: F&O Section 6. Functions and Operators on Numerics :)

declare function op:numeric-add($arg1 as numeric()?, $arg2 as numeric()?)      as numeric()? external;
declare function op:numeric-subtract($arg1 as numeric()?, $arg2 as numeric()?) as numeric()? external;
declare function op:numeric-multiply($arg1 as numeric()?, $arg2 as numeric()?) as numeric()? external;
declare function op:numeric-divide($arg1 as numeric()?, $arg2 as numeric()?)   as numeric()? external;
declare function op:numeric-mod($arg1 as numeric()?, $arg2 as numeric()?)      as numeric()? external;
declare function op:numeric-idivide($arg1 as numeric()?, $arg2 as numeric()?)  as numeric()? external;
declare function op:numeric-unary-plus($arg1 as numeric()?)                   as numeric()? external;
declare function op:numeric-unary-minus($arg1 as numeric()?)                  as numeric()? external;

(: xs:int :)
declare function op:int-add($arg1 as xs:int?, $arg2 as xs:int?)        as xs:int? 	  external;
declare function op:int-subtract($arg1 as xs:int?, $arg2 as xs:int?)   as xs:int? 	  external;
declare function op:int-multiply($arg1 as xs:int?, $arg2 as xs:int?)   as xs:int? 	  external;
declare function op:int-divide($arg1 as xs:int?, $arg2 as xs:int?)     as xs:decimal? 	  external;
declare function op:int-idivide($arg1 as xs:int?, $arg2 as xs:int?)    as xs:int? 	  external;
declare function op:int-mod($arg1 as xs:int?, $arg2 as xs:int?)        as xs:int? 	  external;

declare function op:int-unary-plus($arg1 as xs:int?)           	 as xs:int? 	  external;
declare function op:int-unary-minus($arg1 as xs:int?)          	 as xs:int? 	  external;

declare function op:int-equal($arg1 as xs:int, $arg2 as xs:int)        	 as xs:boolean external;
declare function op:int-nequal($arg1 as xs:int, $arg2 as xs:int)       	 as xs:boolean external;
declare function op:int-lt($arg1 as xs:int, $arg2 as xs:int)           	 as xs:boolean external;
declare function op:int-gt($arg1 as xs:int, $arg2 as xs:int)           	 as xs:boolean external;
declare function op:int-le($arg1 as xs:int, $arg2 as xs:int)           	 as xs:boolean external;
declare function op:int-ge($arg1 as xs:int, $arg2 as xs:int)           	 as xs:boolean external;

(: xs:integer :)
declare function op:integer-add($arg1 as xs:integer?, $arg2 as xs:integer?)      as xs:integer? external;
declare function op:integer-subtract($arg1 as xs:integer?, $arg2 as xs:integer?) as xs:integer? external;
declare function op:integer-multiply($arg1 as xs:integer?, $arg2 as xs:integer?) as xs:integer? external;
declare function op:integer-divide($arg1 as xs:integer?, $arg2 as xs:integer?)   as xs:decimal? external;
declare function op:integer-idivide($arg1 as xs:integer?, $arg2 as xs:integer?)  as xs:integer? external;
declare function op:integer-mod($arg1 as xs:integer?, $arg2 as xs:integer?)      as xs:integer? external;

declare function op:integer-unary-plus($arg1 as xs:integer?)  as xs:integer? external;
declare function op:integer-unary-minus($arg1 as xs:integer?) as xs:integer? external;

declare function op:integer-equal($arg1 as xs:integer, $arg2 as xs:integer)  	 as xs:boolean  external;
declare function op:integer-nequal($arg1 as xs:integer, $arg2 as xs:integer) 	 as xs:boolean  external;
declare function op:integer-lt($arg1 as xs:integer, $arg2 as xs:integer)     	 as xs:boolean  external;
declare function op:integer-gt($arg1 as xs:integer, $arg2 as xs:integer)     	 as xs:boolean  external;
declare function op:integer-le($arg1 as xs:integer, $arg2 as xs:integer)     	 as xs:boolean  external;
declare function op:integer-ge($arg1 as xs:integer, $arg2 as xs:integer)     	 as xs:boolean  external;

(: xs:decimal :)
declare function op:decimal-add($arg1 as xs:decimal?, $arg2 as xs:decimal?)      as xs:decimal? external;
declare function op:decimal-subtract($arg1 as xs:decimal?, $arg2 as xs:decimal?) as xs:decimal? external;
declare function op:decimal-multiply($arg1 as xs:decimal?, $arg2 as xs:decimal?) as xs:decimal? external;
declare function op:decimal-divide($arg1 as xs:decimal?, $arg2 as xs:decimal?)   as xs:decimal? external;
declare function op:decimal-idivide($arg1 as xs:decimal?, $arg2 as xs:decimal?)  as xs:integer? external;
declare function op:decimal-mod($arg1 as xs:decimal?, $arg2 as xs:decimal?)      as xs:decimal? external;

declare function op:decimal-unary-plus($arg1 as xs:decimal?)  as xs:decimal? external;
declare function op:decimal-unary-minus($arg1 as xs:decimal?) as xs:decimal? external;

declare function op:decimal-equal($arg1 as xs:decimal, $arg2 as xs:decimal)  	 as xs:boolean  external;
declare function op:decimal-nequal($arg1 as xs:decimal, $arg2 as xs:decimal) 	 as xs:boolean  external;
declare function op:decimal-lt($arg1 as xs:decimal, $arg2 as xs:decimal)    	 as xs:boolean  external;
declare function op:decimal-gt($arg1 as xs:decimal, $arg2 as xs:decimal)    	 as xs:boolean  external;
declare function op:decimal-le($arg1 as xs:decimal, $arg2 as xs:decimal)    	 as xs:boolean  external;
declare function op:decimal-ge($arg1 as xs:decimal, $arg2 as xs:decimal)    	 as xs:boolean  external;

(: xs:float :)
declare function op:float-add($arg1 as xs:float?, $arg2 as xs:float?)         	 as xs:float?   external;
declare function op:float-subtract($arg1 as xs:float?, $arg2 as xs:float?)    	 as xs:float?   external;
declare function op:float-multiply($arg1 as xs:float?, $arg2 as xs:float?)    	 as xs:float?   external;
declare function op:float-divide($arg1 as xs:float?, $arg2 as xs:float?)      	 as xs:float?   external;
declare function op:float-idivide($arg1 as xs:float?, $arg2 as xs:float?)    	 as xs:integer?   external;
declare function op:float-mod($arg1 as xs:float?, $arg2 as xs:float?)         	 as xs:float?   external;

declare function op:float-unary-plus($arg1 as xs:float?)  as xs:float?   external;
declare function op:float-unary-minus($arg1 as xs:float?) as xs:float?   external;

declare function op:float-equal($arg1 as xs:float, $arg2 as xs:float)       	 as xs:boolean  external;
declare function op:float-nequal($arg1 as xs:float, $arg2 as xs:float)      	 as xs:boolean  external;
declare function op:float-lt($arg1 as xs:float, $arg2 as xs:float)          	 as xs:boolean  external;
declare function op:float-gt($arg1 as xs:float, $arg2 as xs:float)          	 as xs:boolean  external;
declare function op:float-le($arg1 as xs:float, $arg2 as xs:float)          	 as xs:boolean  external;
declare function op:float-ge($arg1 as xs:float, $arg2 as xs:float)          	 as xs:boolean  external;

(: xs:double :)
declare function op:double-add($arg1 as xs:double?, $arg2 as xs:double?)      	 as xs:double?  external;
declare function op:double-subtract($arg1 as xs:double?, $arg2 as xs:double?) 	 as xs:double?  external;
declare function op:double-multiply($arg1 as xs:double?, $arg2 as xs:double?) 	 as xs:double?  external;
declare function op:double-divide($arg1 as xs:double?, $arg2 as xs:double?)   	 as xs:double?  external;
declare function op:double-idivide($arg1 as xs:double?, $arg2 as xs:double?)  	 as xs:integer?  external;
declare function op:double-mod($arg1 as xs:double?, $arg2 as xs:double?)      	 as xs:double?  external;

declare function op:double-unary-plus($arg1 as xs:double?)    		 as xs:double?  external;
declare function op:double-unary-minus($arg1 as xs:double?)   		 as xs:double?  external;
       
declare function op:double-equal($arg1 as xs:double, $arg2 as xs:double)    	 as xs:boolean  external;
declare function op:double-nequal($arg1 as xs:double, $arg2 as xs:double)   	 as xs:boolean  external;
declare function op:double-lt($arg1 as xs:double, $arg2 as xs:double)       	 as xs:boolean  external;
declare function op:double-gt($arg1 as xs:double, $arg2 as xs:double)       	 as xs:boolean  external;
declare function op:double-le($arg1 as xs:double, $arg2 as xs:double)       	 as xs:boolean  external;
declare function op:double-ge($arg1 as xs:double, $arg2 as xs:double)       	 as xs:boolean  external;

declare function fn:abs($arg1 as numeric()?)	      as numeric()?  external;
declare function fn:abs-double($arg1 as xs:double?)   as xs:double?  external;
declare function fn:abs-float($arg1 as xs:float?)     as xs:float?   external;
declare function fn:abs-decimal($arg1 as xs:decimal?) as xs:decimal? external;
declare function fn:abs-integer($arg1 as xs:integer?) as xs:integer? external;

declare function fn:floor($arg1 as numeric()?)		as numeric()?  external;
declare function fn:floor-double($arg1 as xs:double?)	as xs:double?  external;
declare function fn:floor-float($arg1 as xs:float?)	as xs:float?   external;
declare function fn:floor-decimal($arg1 as xs:decimal?)	as xs:decimal? external;
declare function fn:floor-integer($arg1 as xs:integer?)	as xs:integer? external;

declare function fn:ceiling($arg1 as numeric()?)          as numeric()?  external;
declare function fn:ceiling-double($arg1 as xs:double?)   as xs:double?  external;
declare function fn:ceiling-float($arg1 as xs:float?)     as xs:float?   external;
declare function fn:ceiling-decimal($arg1 as xs:decimal?) as xs:decimal? external;
declare function fn:ceiling-integer($arg1 as xs:integer?) as xs:integer? external;

declare function fn:round($arg1 as numeric()?)		as numeric()?  external;
declare function fn:round-double($arg1 as xs:double?)	as xs:double?  external;
declare function fn:round-float($arg1 as xs:float?)	as xs:float?   external;
declare function fn:round-decimal($arg1 as xs:decimal?)	as xs:decimal? external;
declare function fn:round-integer($arg1 as xs:integer?)	as xs:integer? external;

declare function fn:round-half-to-even($arg1 as numeric()?, $arg2 as xs:integer)		as numeric()?  external;
declare function fn:round-half-to-even-double($arg1 as xs:double?, $arg2 as xs:integer)	as xs:double?  external;
declare function fn:round-half-to-even-float($arg1 as xs:float?, $arg2 as xs:integer)	as xs:float?   external;
declare function fn:round-half-to-even-decimal($arg1 as xs:decimal?, $arg2 as xs:integer)	as xs:decimal? external;
declare function fn:round-half-to-even-integer($arg1 as xs:integer?, $arg2 as xs:integer)	as xs:integer? external;

(:
   F&O Section 7. Functions on Strings
:)
declare function fn:codepoints-to-string($arg1 as xs:integer*) as xs:string external;
declare function fn:string-to-codepoints($arg1 as xs:string?) as xs:integer* external;
declare function fn:codepoint-equal($arg1 as xs:string?, $arg2 as xs:string?) as xs:boolean? external;

declare function fn:compare($arg1 as xs:string?, $arg2 as xs:string?, $arg3 as xs:string) as xs:integer? external;

(: fn:concat is variadic :)
declare function fn:concat($arg1 as xs:anyAtomicType?, $arg2 as xs:anyAtomicType?) as xs:string external;
declare function fn:string-join($arg1 as xs:string*, $arg2 as xs:string?) as xs:string external;

(: Galax does not support any of the string functions that take the
   name of a collation sequence. :)
declare function fn:substring($arg1 as xs:string?, $arg2 as xs:double, $arg3 as xs:double) as xs:string  external;
declare function fn:string-length($arg1 as xs:string?)                       as xs:integer external;
declare function fn:normalize-space($arg1 as xs:string?)	             as xs:string  external;
declare function fn:normalize-unicode($arg1 as xs:string?, $arg2 as xs:string) as xs:string? external;
declare function fn:upper-case($arg1 as xs:string?)                          as xs:string  external;
declare function fn:lower-case($arg1 as xs:string?)                          as xs:string  external;
declare function fn:translate($arg1 as xs:string?, $arg2 as xs:string, $arg3 as xs:string) as xs:string  external;
declare function fn:encode-for-uri($uri-part as xs:string?) as xs:string external;
declare function fn:iri-to-uri($uri-part as xs:string?) as xs:string external;
declare function fn:escape-html-uri($uri as xs:string?) as xs:string external;

declare function fn:contains($arg1 as xs:string?, $arg2 as xs:string?, $arg3 as xs:string) as xs:boolean? external;
declare function fn:starts-with($arg1 as xs:string?, $arg2 as xs:string?, $arg3 as xs:string) as xs:boolean? external;
declare function fn:ends-with($arg1 as xs:string?, $arg2 as xs:string?, $arg3 as xs:string)   as xs:boolean? external;
declare function fn:substring-before($arg1 as xs:string?, $arg2 as xs:string?, $arg3 as xs:string) as xs:string?  external;
declare function fn:substring-after($arg1 as xs:string?, $arg2 as xs:string?, $arg3 as xs:string)  as xs:string?  external;

declare function fn:matches($arg1 as xs:string?, $arg2 as xs:string, $arg3 as xs:string) as xs:boolean external;
declare function fn:tokenize($arg1 as xs:string?, $arg2 as xs:string, $arg3 as xs:string?) as xs:string* external;
declare function fn:replace($arg1 as xs:string?, $arg2 as xs:string, $arg3 as xs:string, $arg4 as xs:string?) as xs:string external;

declare function op:string-equal($arg1 as xs:string, $arg2 as xs:string)     as xs:boolean external;
declare function op:string-nequal($arg1 as xs:string, $arg2 as xs:string)    as xs:boolean external;
declare function op:string-lt($arg1 as xs:string, $arg2 as xs:string)        as xs:boolean external;
declare function op:string-gt($arg1 as xs:string, $arg2 as xs:string)        as xs:boolean external;
declare function op:string-le($arg1 as xs:string, $arg2 as xs:string)        as xs:boolean external;
declare function op:string-ge($arg1 as xs:string, $arg2 as xs:string)        as xs:boolean external;

(: F&O Section 11. Functions and Operators for anyURI :)

(:
   fn:resolve-uri with one argument is normalized into fn:resolve-uri
   with two arguments
:)
declare function fn:resolve-uri($arg1 as xs:string?, $arg2 as xs:string)  as xs:string    external;
declare function op:anyURI-equal($arg1 as xs:anyURI, $arg2 as xs:anyURI)  as xs:boolean  external;
declare function op:anyURI-nequal($arg1 as xs:anyURI, $arg2 as xs:anyURI) as xs:boolean external;


(:
  F&O Section 9. Functions and Operators on Booleans
:)

declare function fn:true()  as xs:boolean external;
declare function fn:false() as xs:boolean external;

declare function fn:not($arg1 as xs:boolean) as xs:boolean external;
(: F&O writes signature as:
declare function fn:not($arg1 as item()*) as xs:boolean external;
:)

declare function op:boolean-equal($arg1 as xs:boolean, $arg2 as xs:boolean)  as xs:boolean external;
declare function op:boolean-nequal($arg1 as xs:boolean, $arg2 as xs:boolean) as xs:boolean external;
declare function op:boolean-lt($arg1 as xs:boolean, $arg2 as xs:boolean)     as xs:boolean external;
declare function op:boolean-gt($arg1 as xs:boolean, $arg2 as xs:boolean)     as xs:boolean external;
declare function op:boolean-le($arg1 as xs:boolean, $arg2 as xs:boolean)     as xs:boolean external;
declare function op:boolean-ge($arg1 as xs:boolean, $arg2 as xs:boolean)     as xs:boolean external;

(:
   F&O Section 10. Functions and Operators on Durations, Dates, and Times
:)

(: 10.3 Comparisons on Duration, Date, and time :)
declare function op:yearMonthDuration-equal($arg1 as xs:yearMonthDuration,$arg2 as xs:yearMonthDuration) as xs:boolean external;
declare function op:yearMonthDuration-nequal($arg1 as xs:yearMonthDuration,$arg2 as xs:yearMonthDuration) as xs:boolean external;
declare function op:yearMonthDuration-lt($arg1 as xs:yearMonthDuration,$arg2 as xs:yearMonthDuration) as xs:boolean external;
declare function op:yearMonthDuration-le($arg1 as xs:yearMonthDuration,$arg2 as xs:yearMonthDuration) as xs:boolean external;
declare function op:yearMonthDuration-gt($arg1 as xs:yearMonthDuration,$arg2 as xs:yearMonthDuration) as xs:boolean external;
declare function op:yearMonthDuration-ge($arg1 as xs:yearMonthDuration,$arg2 as xs:yearMonthDuration) as xs:boolean external;

declare function op:dayTimeDuration-equal($arg1 as xs:dayTimeDuration,$arg2 as xs:dayTimeDuration) as xs:boolean external;
declare function op:dayTimeDuration-nequal($arg1 as xs:dayTimeDuration,$arg2 as xs:dayTimeDuration) as xs:boolean external;
declare function op:dayTimeDuration-lt($arg1 as xs:dayTimeDuration,$arg2 as xs:dayTimeDuration) as xs:boolean external;
declare function op:dayTimeDuration-le($arg1 as xs:dayTimeDuration,$arg2 as xs:dayTimeDuration) as xs:boolean external;
declare function op:dayTimeDuration-gt($arg1 as xs:dayTimeDuration,$arg2 as xs:dayTimeDuration) as xs:boolean external;
declare function op:dayTimeDuration-ge($arg1 as xs:dayTimeDuration,$arg2 as xs:dayTimeDuration) as xs:boolean external;

declare function op:duration-equal($arg1 as xs:duration,$arg2 as xs:duration) as xs:boolean external;
declare function op:duration-nequal($arg1 as xs:duration,$arg2 as xs:duration) as xs:boolean external;

declare function op:dateTime-equal($arg1 as xs:dateTime,$arg2 as xs:dateTime) as xs:boolean external;
declare function op:dateTime-nequal($arg1 as xs:dateTime,$arg2 as xs:dateTime) as xs:boolean external;
declare function op:dateTime-lt($arg1 as xs:dateTime,$arg2 as xs:dateTime) as xs:boolean external;
declare function op:dateTime-le($arg1 as xs:dateTime,$arg2 as xs:dateTime) as xs:boolean external;
declare function op:dateTime-gt($arg1 as xs:dateTime,$arg2 as xs:dateTime) as xs:boolean external;
declare function op:dateTime-ge($arg1 as xs:dateTime,$arg2 as xs:dateTime) as xs:boolean external;

declare function op:date-equal($arg1 as xs:date,$arg2 as xs:date) as xs:boolean external;
declare function op:date-nequal($arg1 as xs:date,$arg2 as xs:date) as xs:boolean external;
declare function op:date-lt($arg1 as xs:date,$arg2 as xs:date) as xs:boolean external;
declare function op:date-le($arg1 as xs:date,$arg2 as xs:date) as xs:boolean external;
declare function op:date-gt($arg1 as xs:date,$arg2 as xs:date) as xs:boolean external;
declare function op:date-ge($arg1 as xs:date,$arg2 as xs:date) as xs:boolean external;

declare function op:gYearMonth-equal($arg1 as xs:gYearMonth,$arg2 as xs:gYearMonth) as xs:boolean external;
declare function op:gYearMonth-nequal($arg1 as xs:gYearMonth,$arg2 as xs:gYearMonth) as xs:boolean external;

declare function op:gYear-equal($arg1 as xs:gYear,$arg2 as xs:gYear) as xs:boolean external;
declare function op:gYear-nequal($arg1 as xs:gYear,$arg2 as xs:gYear) as xs:boolean external;

declare function op:gMonthDay-equal($arg1 as xs:gMonthDay,$arg2 as xs:gMonthDay) as xs:boolean external;
declare function op:gMonthDay-nequal($arg1 as xs:gMonthDay,$arg2 as xs:gMonthDay) as xs:boolean external;

declare function op:gDay-equal($arg1 as xs:gDay,$arg2 as xs:gDay) as xs:boolean external;
declare function op:gDay-nequal($arg1 as xs:gDay,$arg2 as xs:gDay) as xs:boolean external;

declare function op:gMonth-equal($arg1 as xs:gMonth,$arg2 as xs:gMonth) as xs:boolean external;
declare function op:gMonth-nequal($arg1 as xs:gMonth,$arg2 as xs:gMonth) as xs:boolean external;

declare function op:time-equal($arg1 as xs:time,$arg2 as xs:time) as xs:boolean external;
declare function op:time-nequal($arg1 as xs:time,$arg2 as xs:time) as xs:boolean external;
declare function op:time-lt($arg1 as xs:time,$arg2 as xs:time) as xs:boolean external;
declare function op:time-le($arg1 as xs:time,$arg2 as xs:time) as xs:boolean external;
declare function op:time-gt($arg1 as xs:time,$arg2 as xs:time) as xs:boolean external;
declare function op:time-ge($arg1 as xs:time,$arg2 as xs:time) as xs:boolean external;


(: 10.5 Component Extraction Functions on Duration, Date and Time Values :)
declare function fn:years-from-duration($arg1 as xs:duration?) as xs:integer? external;
declare function fn:months-from-duration($arg1 as xs:duration?) as xs:integer? external;
declare function fn:days-from-duration($arg1 as xs:duration?) as xs:integer? external;
declare function fn:hours-from-duration($arg1 as xs:duration?) as xs:integer? external;
declare function fn:minutes-from-duration($arg1 as xs:duration?) as xs:integer? external;
declare function fn:seconds-from-duration($arg1 as xs:duration?) as xs:decimal? external;
declare function fn:year-from-dateTime($arg1 as xs:dateTime?) as xs:integer? external;
declare function fn:month-from-dateTime($arg1 as xs:dateTime?) as xs:integer? external;
declare function fn:day-from-dateTime($arg1 as xs:dateTime?) as xs:integer? external;
declare function fn:hours-from-dateTime($arg1 as xs:dateTime?) as xs:integer? external;
declare function fn:minutes-from-dateTime($arg1 as xs:dateTime?) as xs:integer? external;
declare function fn:seconds-from-dateTime($arg1 as xs:dateTime?) as xs:decimal? external;
declare function fn:timezone-from-dateTime($arg1 as xs:dateTime?) as xs:dayTimeDuration? external;
declare function fn:year-from-date($arg1 as xs:date?) as xs:integer? external;
declare function fn:month-from-date($arg1 as xs:date?) as xs:integer? external;
declare function fn:day-from-date($arg1 as xs:date?) as xs:integer? external;
declare function fn:timezone-from-date($arg1 as xs:date?) as xs:dayTimeDuration? external;
declare function fn:hours-from-time($arg1 as xs:time?) as xs:integer? external;
declare function fn:minutes-from-time($arg1 as xs:time?) as xs:integer? external;
declare function fn:seconds-from-time($arg1 as xs:time?) as xs:decimal? external;
declare function fn:timezone-from-time($arg1 as xs:time?) as xs:dayTimeDuration? external;

(: 10.5 Arithmetic Functions on Durations :)

declare function op:add-yearMonthDurations($arg1 as xs:yearMonthDuration?, $arg2 as xs:yearMonthDuration?) as xs:yearMonthDuration? external;
declare function op:subtract-yearMonthDurations($arg1 as xs:yearMonthDuration?, $arg2 as xs:yearMonthDuration?) as xs:yearMonthDuration? external;
declare function op:multiply-yearMonthDuration($arg1 as xs:yearMonthDuration?, $arg2 as xs:double?) as xs:yearMonthDuration? external;
declare function op:divide-yearMonthDuration($arg1 as xs:yearMonthDuration?, $arg2 as xs:double?) as xs:yearMonthDuration? external;
declare function op:multiply-yearMonthDuration2($arg1 as xs:double?, $arg2 as xs:yearMonthDuration?) as xs:yearMonthDuration? external;
declare function op:divide-yearMonthDuration-by-yearMonthDuration($arg1 as xs:yearMonthDuration?, $arg2 as xs:yearMonthDuration?) as xs:decimal? external;

declare function op:add-dayTimeDurations($arg1 as xs:dayTimeDuration?,$arg2 as xs:dayTimeDuration?) as xs:dayTimeDuration? external;
declare function op:subtract-dayTimeDurations($arg1 as xs:dayTimeDuration?, $arg2 as xs:dayTimeDuration?) as xs:dayTimeDuration? external;
declare function op:multiply-dayTimeDuration($arg1 as xs:dayTimeDuration?, $arg2 as xs:double?) as xs:dayTimeDuration? external;
declare function op:multiply-dayTimeDuration2($arg1 as xs:double?, $arg2 as xs:dayTimeDuration?) as xs:dayTimeDuration? external;
declare function op:divide-dayTimeDuration($arg1 as xs:dayTimeDuration?, $arg2 as xs:double?) as xs:dayTimeDuration? external;
declare function op:divide-dayTimeDuration-by-dayTimeDuration($arg1 as xs:dayTimeDuration?, $arg2 as xs:dayTimeDuration?) as xs:decimal? external;

(: 10.6 Timezone Adjustment :)
declare function fn:adjust-time-to-timezone($arg1 as xs:time?, $arg2 as xs:dayTimeDuration?) as xs:time? external;
declare function fn:adjust-date-to-timezone($arg1 as xs:date?, $arg2 as xs:dayTimeDuration?) as xs:date? external;
declare function fn:adjust-dateTime-to-timezone($arg1 as xs:dateTime?, $arg2 as xs:dayTimeDuration?) as xs:dateTime? external;

declare function fn:adjust-time-to-timezone-unary($arg1 as xs:time?) as xs:time? external;
declare function fn:adjust-date-to-timezone-unary($arg1 as xs:date?) as xs:date? external;
declare function fn:adjust-dateTime-to-timezone-unary($arg1 as xs:dateTime?) as xs:dateTime? external;

(: 10.7 Arithmetic Functions on Durations, Dates, Times :)

declare function op:subtract-dateTimes($arg1 as xs:dateTime?,$arg2 as xs:dateTime?) as xs:dayTimeDuration? external;
declare function op:subtract-dates($arg1 as xs:date?,$arg2 as xs:date?) as xs:dayTimeDuration? external;
declare function op:subtract-times($arg1 as xs:time?,$arg2 as xs:time?) as xs:dayTimeDuration? external;

declare function op:add-yearMonthDuration-to-dateTime($arg1 as xs:dateTime?,$arg2 as xs:yearMonthDuration?) as xs:dateTime? external;
declare function op:add-yearMonthDuration-to-dateTime2($arg1 as xs:yearMonthDuration?,$arg2 as xs:dateTime?) as xs:dateTime? external;
declare function op:add-dayTimeDuration-to-dateTime($arg1 as xs:dateTime?,$arg2 as xs:dayTimeDuration?) as xs:dateTime? external;
declare function op:add-dayTimeDuration-to-dateTime2($arg1 as xs:dayTimeDuration?,$arg2 as xs:dateTime?) as xs:dateTime? external;
declare function op:subtract-yearMonthDuration-from-dateTime($arg1 as xs:dateTime?, $arg2 as xs:yearMonthDuration?) as xs:dateTime? external;
declare function op:subtract-dayTimeDuration-from-dateTime($arg1 as xs:dateTime?, $arg2 as xs:dayTimeDuration?) as xs:dateTime? external;
declare function op:add-yearMonthDuration-to-date($arg1 as xs:date?,$arg2 as xs:yearMonthDuration?) as xs:date? external;
declare function op:add-yearMonthDuration-to-date2($arg1 as xs:yearMonthDuration?,$arg2 as xs:date?) as xs:date? external;
declare function op:add-dayTimeDuration-to-date($arg1 as xs:date?,$arg2 as xs:dayTimeDuration?) as xs:date? external;
declare function op:add-dayTimeDuration-to-date2($arg1 as xs:dayTimeDuration?,$arg2 as xs:date?) as xs:date? external;
declare function op:subtract-yearMonthDuration-from-date($arg1 as xs:date?, $arg2 as xs:yearMonthDuration?) as xs:date? external;

declare function op:subtract-dayTimeDuration-from-date($arg1 as xs:date?, $arg2 as xs:dayTimeDuration?) as xs:date? external;
declare function op:add-dayTimeDuration-to-time($arg1 as xs:time?,$arg2 as xs:dayTimeDuration?) as xs:time? external;
declare function op:add-dayTimeDuration-to-time2($arg1 as xs:dayTimeDuration?,$arg2 as xs:time?) as xs:time? external;
declare function op:subtract-dayTimeDuration-from-time($arg1 as xs:time?, $arg2 as xs:dayTimeDuration?) as xs:time? external;

(:
   F&O Section 11. Functions on QNames
:)

declare function fn:resolve-QName($arg1 as xs:string?, $arg2 as element()) as xs:QName external;
declare function fn:QName($uri as xs:string?,$qname as xs:string) as xs:QName external;

declare function fn:local-name-from-QName($arg1 as xs:QName?)     as xs:NCName? external;
declare function fn:namespace-uri-from-QName($arg1 as xs:QName?)  as xs:anyURI? external;
declare function fn:prefix-from-QName($arg1 as xs:QName?) as xs:NCName? external;

declare function fn:namespace-uri-for-prefix($prefix as xs:string?, $element as element()) as xs:anyURI? external;
declare function fn:in-scope-prefixes($arg1 as element()) as xs:string* external;

declare function op:QName-equal($arg1 as xs:QName, $arg2 as xs:QName)     as xs:boolean external;
declare function op:QName-nequal($arg1 as xs:QName, $arg2 as xs:QName)    as xs:boolean external;

(:
   F&O Section 12. Functions and Operators on base64Binary and hexBinary
:)

declare function op:hexBinary-equal($arg1 as xs:hexBinary, $arg2 as xs:hexBinary)          as xs:boolean external;
declare function op:base64Binary-equal($arg1 as xs:base64Binary, $arg2 as xs:base64Binary) as xs:boolean external;
declare function op:hexBinary-nequal($arg1 as xs:hexBinary, $arg2 as xs:hexBinary)          as xs:boolean external;
declare function op:base64Binary-nequal($arg1 as xs:base64Binary, $arg2 as xs:base64Binary) as xs:boolean external;


(:
   F&O Section 13. Functions and Operators on NOTATION
:)
  
(:
declare function op:NOTATION-equal($arg1 as xs:NOTATION,$arg2 as xs:NOTATION) as xs:boolean external;
:)


(:
   F&O Section 14. Functions and Operators on Nodes
:)

declare function fn:name($arg1 as node()?)                   as xs:string external;
declare function fn:local-name($arg1 as node()?)             as xs:string external;
declare function fn:namespace-uri($arg1 as node()?)          as xs:string external;
declare function fn:number($arg1 as xs:anyAtomicType?)      as xs:double external;

(: fn:lang is only defined over context nodes, not arbitrary nodes: :)
declare function fn:lang($arg1 as xs:string?,$arg2 as node()) as xs:boolean external;

declare function op:is-same-node($arg1 as node()?, $arg2 as node()?)      as xs:boolean? external;
declare function op:node-before($arg1 as node()?, $arg2 as node()?)     as xs:boolean? external;
declare function op:node-after($arg1 as node()?, $arg2 as node()?)      as xs:boolean? external;
declare function fn:root($arg1 as node()?)                              as node()?     external;


(:
   F&O Section 15. Functions and Operators on Sequences
:)
(: 15.1 General Functions :)

(: The function signature for fn:boolean() in the dynamic semantics is:
   fn:boolean(item()*) 
   but in the static semantics it is:
   fn:boolean(empty|NodeType+|xs:boolean|xs:string|xs:untypedAtomic|fs:numeric) 
:)
declare function fn:boolean($arg1 as item()*) as xs:boolean  external;
declare function fn:index-of($arg1 as xs:anyAtomicType*, $arg2 as xs:anyAtomicType, $arg3 as xs:string) as xs:integer* external;
declare function fn:empty($arg1 as item()*)            	    		 as xs:boolean  external;
declare function fn:exists($arg1 as item()*)            	    	 as xs:boolean  external;
declare function fn:distinct-values($arg1 as xs:anyAtomicType*, $arg2 as xs:string)      	 as xs:anyAtomicType* 	   external;
declare function fn:insert-before($arg1 as item()*, $arg2 as xs:integer, $arg3 as item()*) 	 as item()*   	   external;
declare function fn:remove($arg1 as item()*, $arg2 as xs:integer) as item()* external;
declare function fn:reverse($arg1 as item()*) as item()* external;
declare function fn:subsequence($arg1 as item()*, $arg2 as xs:integer) as item()* external;
declare function fn:subsequence($arg1 as item()*, $arg2 as xs:integer, $arg3 as xs:integer) as item()*   	   external;

declare function fn:unordered($arg1 as item()*)                             as item()*        external;

(: 15.2 Functions that test cardinality of sequences :)
declare function fn:zero-or-one($arg1 as item()*) as item()? external;
declare function fn:exactly-one($arg1 as item()*) as item()  external;
declare function fn:one-or-more($arg1 as item()*) as item()+ external;

(: 15.3 Equals, Union, Intersection, Except :)
declare function fn:deep-equal($arg1 as item()*, $arg2 as item()*, $arg3 as xs:string) as xs:boolean external;
declare function op:union($arg1 as node()*, $arg2 as node()*) as node()* external;
declare function op:intersect($arg1 as node()*, $arg2 as node()*) as node()* external;
declare function op:except($arg1 as node()*, $arg2 as node()*) as node()* external;

(: 15.4 Aggregate Functions :)
declare function fn:count($arg1 as item()*) as xs:integer external;

declare function fn:avg($arg1 as xs:anyAtomicType*) as xs:anyAtomicType? external;
declare function fn:max($arg1 as xs:anyAtomicType*) as xs:anyAtomicType? external;
declare function fn:min($arg1 as xs:anyAtomicType*) as xs:anyAtomicType? external;
declare function fn:sum($arg1 as xs:anyAtomicType*, $arg2 as xs:anyAtomicType?)	as xs:anyAtomicType? external;
declare function fn:sum($arg1 as xs:anyAtomicType*) as xs:anyAtomicType external;

(: xs:dayTimeDuration :)
declare function fn:avg-dayTimeDuration($arg1 as xs:dayTimeDuration*)	 as xs:dayTimeDuration?     external;
declare function fn:max-dayTimeDuration($arg1 as xs:dayTimeDuration*)	 as xs:dayTimeDuration?     external;
declare function fn:min-dayTimeDuration($arg1 as xs:dayTimeDuration*)	 as xs:dayTimeDuration?     external;
declare function fn:sum-dayTimeDuration($arg1 as xs:dayTimeDuration*, $arg2 as xs:dayTimeDuration?) as xs:dayTimeDuration?     external;
declare function fn:sum-dayTimeDuration($arg1 as xs:dayTimeDuration*) as xs:dayTimeDuration external;

(: xs:yearMonthDuration :)
declare function fn:avg-yearMonthDuration($arg1 as xs:yearMonthDuration*) as xs:yearMonthDuration?     external;
declare function fn:max-yearMonthDuration($arg1 as xs:yearMonthDuration*) as xs:yearMonthDuration?     external;
declare function fn:min-yearMonthDuration($arg1 as xs:yearMonthDuration*) as xs:yearMonthDuration?     external;
declare function fn:sum-yearMonthDuration($arg1 as xs:yearMonthDuration*, $arg2 as xs:yearMonthDuration?) as xs:yearMonthDuration?     external;
declare function fn:sum-yearMonthDuration($arg1 as xs:yearMonthDuration*) as xs:yearMonthDuration     external;

(: xs:string :)
declare function fn:max-string($arg1 as xs:string*) as xs:string? external;
declare function fn:min-string($arg1 as xs:string*) as xs:string? external; 

(: xs:date :)
declare function fn:max-date($arg1 as xs:date*) as xs:date? external;
declare function fn:min-date($arg1 as xs:date*) as xs:date? external; 

(: xs:time :)

declare function fn:max-time($arg1 as xs:time*) as xs:time? external;
declare function fn:min-time($arg1 as xs:time*) as xs:time? external; 

(: xs:dateTime :)

declare function fn:max-dateTime($arg1 as xs:dateTime*) as xs:dateTime? external;
declare function fn:min-dateTime($arg1 as xs:dateTime*) as xs:dateTime? external; 

(: xs:integer :)
declare function fn:avg-integer($arg1 as xs:integer*) as xs:decimal? external;
declare function fn:max-integer($arg1 as xs:integer*) as xs:integer? external;
declare function fn:min-integer($arg1 as xs:integer*) as xs:integer? external;
declare function fn:sum-integer($arg1 as xs:integer*) as xs:integer external;
declare function fn:sum-integer($arg1 as xs:integer*, $arg2 as xs:integer?) as xs:integer? external;

(: xs:decimal :)
declare function fn:avg-decimal($arg1 as xs:decimal*) as xs:decimal? external;
declare function fn:max-decimal($arg1 as xs:decimal*) as xs:decimal? external;
declare function fn:min-decimal($arg1 as xs:decimal*) as xs:decimal? external;
declare function fn:sum-decimal($arg1 as xs:decimal*) as xs:decimal external;
declare function fn:sum-decimal($arg1 as xs:decimal*, $arg2 as xs:decimal?) as xs:decimal?     external;

(: xs:float :)
declare function fn:avg-float($arg1 as xs:float*) as xs:float? external;
declare function fn:max-float($arg1 as xs:float*) as xs:float? external;
declare function fn:min-float($arg1 as xs:float*) as xs:float? external;
declare function fn:sum-float($arg1 as xs:float*) as xs:float external;
declare function fn:sum-float($arg1 as xs:float*, $arg2 as xs:float?) as xs:float? external;

(: xs:double :)
declare function fn:avg-double($arg1 as xs:double*) as xs:double?  external;
declare function fn:max-double($arg1 as xs:double*) as xs:double?  external;
declare function fn:min-double($arg1 as xs:double*) as xs:double?  external;
declare function fn:sum-double($arg1 as xs:double*) as xs:double external;
declare function fn:sum-double($arg1 as xs:double*, $arg2 as xs:double?) as xs:double? external;

(: 15.5 Functions that generate sequences :)
declare function op:to($arg1 as xs:integer?, $arg2 as xs:integer?) as xs:integer+ external;

declare function fn:doc($arg1 as xs:string?) as document-node()? external;
declare function fn:doc-available($arg1 as xs:string?) as xs:boolean external;
declare function fn:collection($arg1 as xs:string?) as node()* external;
declare function fn:id($arg as xs:string*, $node as node()) as element()* external;
declare function fn:idref($arg as xs:string*, $node as node()) as node()* external; 

(: ::::::::::::::::::::::::
     Comparison functions
   :::::::::::::::::::::::: :)

(:
   Note: Those functions are done in a polymorphic fashion in
   Galax. This differs from the F&O document where comparison functions
   are specialized by type.
:)

(: comparators :)

declare function op:equal($arg1 as xs:anyAtomicType?, $arg2 as xs:anyAtomicType?)  as xs:boolean external;
declare function op:nequal($arg1 as xs:anyAtomicType?, $arg2 as xs:anyAtomicType?) as xs:boolean external;
declare function op:ge($arg1 as xs:anyAtomicType?, $arg2 as xs:anyAtomicType?)     as xs:boolean external;
declare function op:le($arg1 as xs:anyAtomicType?, $arg2 as xs:anyAtomicType?)     as xs:boolean external;
declare function op:gt($arg1 as xs:anyAtomicType?, $arg2 as xs:anyAtomicType?)     as xs:boolean external;
declare function op:lt($arg1 as xs:anyAtomicType?, $arg2 as xs:anyAtomicType?)     as xs:boolean external;

(: following functions handle cases when one argument to value
   comparison is empty :)
declare function op:equal-left-empty($arg1 as empty-sequence(), $arg2 as xs:anyAtomicType?)  as empty-sequence() external;
declare function op:nequal-left-empty($arg1 as empty-sequence(), $arg2 as xs:anyAtomicType?) as empty-sequence() external;
declare function op:ge-left-empty($arg1 as empty-sequence(), $arg2 as xs:anyAtomicType?)     as empty-sequence() external;
declare function op:le-left-empty($arg1 as empty-sequence(), $arg2 as xs:anyAtomicType?)     as empty-sequence() external;
declare function op:gt-left-empty($arg1 as empty-sequence(), $arg2 as xs:anyAtomicType?)     as empty-sequence() external;
declare function op:lt-left-empty($arg1 as empty-sequence(), $arg2 as xs:anyAtomicType?)     as empty-sequence() external;

declare function op:equal-right-empty($arg1 as xs:anyAtomicType?, $arg2 as empty-sequence())  as empty-sequence() external;
declare function op:nequal-right-empty($arg1 as xs:anyAtomicType?, $arg2 as empty-sequence()) as empty-sequence() external;
declare function op:ge-right-empty($arg1 as xs:anyAtomicType?, $arg2 as empty-sequence())     as empty-sequence() external;
declare function op:le-right-empty($arg1 as xs:anyAtomicType?, $arg2 as empty-sequence())     as empty-sequence() external;
declare function op:gt-right-empty($arg1 as xs:anyAtomicType?, $arg2 as empty-sequence())     as empty-sequence() external;
declare function op:lt-right-empty($arg1 as xs:anyAtomicType?, $arg2 as empty-sequence())     as empty-sequence() external;


(: F&O Section 16 : Context functions :)

declare function fn:static-base-uri() as xs:anyURI? external;
declare function fn:current-dateTime() as xs:dateTime external;
declare function fn:current-date() as xs:date external;
declare function fn:current-time() as xs:time external;
declare function fn:default-collation() as xs:string external;
declare function fn:implicit-timezone() as xs:dayTimeDuration? external;

declare function fn:dateTime($arg1 as xs:date?, $arg2 as xs:time?) as xs:dateTime external;

(: ::::::::::::::::::::::::::::::
     Formal Semantics functions
   :::::::::::::::::::::::::::::: :)

(: Used in semantics of path expressions :)
(: Sort by document order and duplicate removal :)

declare function fs:distinct-docorder-or-atomic-sequence($arg1 as item()*) as item()* external;
declare function fs:distinct-docorder($arg1 as node()*)       as node()* external;
(: distinct-docorder can be optimized to either docorder or distinct: :)
declare function fs:docorder($arg1 as node()*)                as node()* external;
declare function fs:distinct($arg1 as node()*)                as node()* external;
(: docorder and distinct can be optimized to node-sequence: :)
declare function fs:node-sequence($arg1 as node()*)           as node()* external;
declare function fs:node-sequence-or-atomic-sequence($arg1 as item()*) as item()* external;

(: Used in semantics of constructors :)

(: Converts a sequence of items into an untyped atomic value :)
declare function fs:item-sequence-to-untypedAtomic($arg1 as item()*) as xs:untypedAtomic external;
declare function fs:item-sequence-to-untypedAtomic-optional($arg1 as item()*) as xs:untypedAtomic? external;

(: These functions are only needed for typing constructors:)
declare function fs:item-sequence-to-node-sequence($arg1 as item()*) as node()* external; 

(: Used in semantics of arithmetic, value, and general comparisions.
   Converts untypedAtomic argument to a target type.  Returns all
   other arguments unchanged. :)
declare function fs:untyped-to-integer($arg1 as xs:anyAtomicType?) as xs:anyAtomicType? external;
declare function fs:untyped-to-double($arg1 as xs:anyAtomicType?) as xs:anyAtomicType?  external;
declare function fs:untyped-to-string($arg1 as xs:anyAtomicType?) as xs:anyAtomicType?  external;
declare function fs:untyped-to-any($arg1 as xs:anyAtomicType, $arg2 as xs:anyAtomicType) as xs:anyAtomicType external;

declare function fs:promote-to-numeric($arg1 as numeric()*, $arg2 as numeric()) as xs:anyAtomicType* external;
declare function fs:promote-to-anystring($arg1 as anystring()*) as xs:anyAtomicType* external;
declare function fs:unsafe-promote-to-numeric($arg1 as numeric()*, $arg2 as numeric()) as xs:anyAtomicType* external;
declare function fs:convert-simple-operand($arg1 as xs:anyAtomicType*, $arg2 as xs:anyAtomicType) as xs:anyAtomicType* external;

(: Used in semantics of XPath predicate expressions: :)
declare function fs:first-item($arg1 as item()*) as item()? external;
declare function fs:last-item($arg1 as item()*)  as item()? external;

(: :::::::::::::::::::
     Galax functions
   ::::::::::::::::::: :)

(: Note: The following functions are not part of the F&O spec. They
   are added in Galax for user's convenience. :)

(: Union, intersection and exception on values :)

declare function glx:union-values($arg1 as xs:anyAtomicType*, $arg2 as xs:anyAtomicType*)     as xs:anyAtomicType* external;
declare function glx:intersect-values($arg1 as xs:anyAtomicType*, $arg2 as xs:anyAtomicType*) as xs:anyAtomicType* external;
declare function glx:except-values($arg1 as xs:anyAtomicType*, $arg2 as xs:anyAtomicType*)    as xs:anyAtomicType* external;

(: Output functions :)

declare function glx:print-string($arg1 as xs:string) as empty-sequence() external;
declare function glx:print-item($arg1 as item())      as empty-sequence() external;
declare function glx:string-of-item($arg1 as item())  as xs:string external;
declare function glx:doc-of-string($arg1 as xs:string) as document-node() external;

declare function glx:print-string-err($arg1 as xs:string)  	as empty-sequence()   external;
declare function glx:print-item-err($arg1 as item())          	as empty-sequence()   external;

declare function glx:save-document($arg1 as xs:string,$arg2 as document-node()) as empty-sequence() external;

(: xml:lang functions :)

declare function glx:get-lang($arg1 as node())                     as xs:string? external;

(: all the traditional float operations :)

declare function glx:exponent($arg1 as xs:double, $arg2 as xs:double)   as xs:double external;
declare function glx:sqrt($arg1 as xs:double)      		as xs:double external;
declare function glx:exp($arg1 as xs:double)       		as xs:double external;
declare function glx:log($arg1 as xs:double)       		as xs:double external;
declare function glx:log10($arg1 as xs:double)     		as xs:double external;
declare function glx:cos($arg1 as xs:double)       		as xs:double external;
declare function glx:sin($arg1 as xs:double)       		as xs:double external;
declare function glx:tan($arg1 as xs:double)       		as xs:double external;
declare function glx:acos($arg1 as xs:double)      		as xs:double external;
declare function glx:asin($arg1 as xs:double)      		as xs:double external;
declare function glx:atan($arg1 as xs:double)      		as xs:double external;
declare function glx:atan2($arg1 as xs:double, $arg2 as xs:double) as xs:double external;
declare function glx:cosh($arg1 as xs:double)      		as xs:double external;
declare function glx:sinh($arg1 as xs:double)      		as xs:double external;
declare function glx:tanh($arg1 as xs:double)      		as xs:double external;

(: Previously was fn:string-pad :)
declare function glx:string-pad($arg1 as xs:string?, $arg2 as xs:integer) as xs:string? external;

(: Jabber :)
(: - jid      - Jabber ID: username@server/source        :)
(: - password - string of password of username on server :)
(: - timeout  - in seconds                               :)
(: - verbose mode flag - if true, print on stderr messages sent to/rcv'd by jabber server :)

declare function glx:jabber-buddies($arg1 as xs:string,$arg2 as xs:string,$arg3 as xs:int,$arg4 as xs:boolean) as document-node() external;

(: glx:file-exists
    If argument is empty, returns empty.  Otherwise, checks if local
    file exists and returns true, otherwise false.
:)
declare function glx:file-exists($arg1 as xs:string?) as xs:boolean? external;

(: glx:stem
    If first argument is empty, returns empty.  
    Otherwise, returns the stem of the given word.

    The second argument specifies a case sensitive/insensitive stemming.
    If the second argument is not specified then case insensitve is default.
    $arg2 = "i" => case insensitive stemming
    $arg2 = "s" => case sensitive stemming
:)
declare function glx:stem($arg1 as xs:string?, $arg2 as xs:string?) as xs:string? external;

declare function glx:deep-distinct($arg1 as item()*) as item()* external;

(: Added accessor function for pre-order -- Philippe :)
declare function glx:get-order($arg as node()) as xs:integer external;
declare function glx:get-docid($arg as node()) as xs:integer external;

(: Delay for the specified amount of time (in seconds; accepts
   non-integer sleep times) :)

declare function glx:sleep($arg as xs:float) external;

(: Similar to fn:doc, but re-reads the document each time it is called :)
declare function glx:getdoc($arg1 as xs:string?) as document-node()? external;

(: SOAP calls :)
declare function glx:soap-call($arg1 as xs:anyURI,$arg2 as xs:string,$arg3 as xs:string,$arg4 as item()*) 
  as document-node() external;

(: Generic function for HTTP requests :)
declare function glx:http-request( $method as xs:string, $url as xs:string, $content as element()? ) as xs:string? external; 
(: Implementation of GET and POST using the camlnet package :)
declare function glx:http-get-request( $url as xs:string ) as item()? external; 

(: Returns the current time, in seconds since Jan.1, 1970 :)
declare updating function glx:gettime() as xs:double external;

(: Perform major O'Caml collection, and return number of live words. :)
declare updating function glx:livewords() as xs:int external;

(: O'Caml Random module :)
declare updating function glx:random_int($bound as xs:integer) as xs:integer external; 
declare updating function glx:keyref($keyname as xs:string,$keyval as xs:anyAtomicType) external; 

