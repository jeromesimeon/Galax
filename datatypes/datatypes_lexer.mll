(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: datatypes_lexer.mll,v 1.16 2007/02/01 22:08:46 simeon Exp $ *)

(* Module Datatype_lexer
   Description:
   This module contains an auxiliary lexer used to parse XML Schema
   built-in types.
*)

{

open Decimal
open Lexing

open Error

}

(*****************************)
(* Lexer regular expressions *)
(*****************************)

(* Lexical building-blocks *)

let ws = [ ' ' '\t' '\r' '\n' ]
    
let digit = ['0'-'9']
    
let sign            = '-' | '+'
let exponent        = 'e' | 'E'
  
(* Whitespace *)
  
let whitespace = ws*
let whitespaceplus = ws+
    
(* Numerics *)
    
let integer_numeric = sign? digit+
let decimal_numeric = sign? ((digit+ ('.' digit*)?) | ('.' digit+))
let double_numeric  = decimal_numeric (exponent sign? digit+)?


(* Note:
     Some of these Char codes have been taken from the PXP ISO
     parser. I am not sure that they are doing the right thing and
     should be double checked for conformance.
   - Jerome
 *)
let base_char    = ['A'-'Z' 'a'-'z']
let ideographics = ['\192'-'\214' '\216'-'\246' '\248'-'\255']
let extender     = '\183'

(* XML names *)

let letter   = base_char | ideographics 
let nmstart  = letter | '_'
let nmchar   = letter | extender | digit | '.' | '-' | '_'
 
let ncname   = nmstart nmchar*


(*
   let valid_date (y,m,d) = true
   
   if date is not valid then
   raise Query(Validation("The value "^...^" is not a valid date."))
*)
    
(* Date and time *)
    
let _C = digit
let _Y = digit
let _M = digit 
let _D = digit
let _h = digit
let _m = digit
let _s = digit
let _UTC = 'Z'
let _rel = sign
let _minus = '-'
    
(****************)
(* Lexing rules *)
(****************)
    
(* NCNAME *)
    rule parse_ncname = parse
| whitespace ncname  whitespace eof
    { (Whitespace.remove_whitespace(lexeme lexbuf)) }

(* xs:integer *)
and parse_integer = parse
| whitespace integer_numeric whitespace eof
    { _integer_of_string (Whitespace.remove_whitespace(lexeme lexbuf)) }
    
(* xs:decimal *)
and parse_decimal = parse
| whitespace decimal_numeric whitespace eof
    { _decimal_of_string (Whitespace.remove_whitespace(lexeme lexbuf)) }
    
(* xs:float *)
and parse_float = parse
| whitespace (double_numeric | "NaN" | "INF" | "-INF") whitespace eof
    { float_of_string (Whitespace.remove_whitespace(lexeme lexbuf)) }
    
(* xs:double *)
and parse_double = parse
| whitespace (double_numeric | "NaN" | "INF" | "-INF") whitespace eof
    { float_of_string (Whitespace.remove_whitespace(lexeme lexbuf)) }
    
(* xs:boolean *)
and parse_boolean = parse
  | whitespace "true" whitespace eof
      { true }
  | whitespace "false" whitespace eof
      { false }
  | whitespace "1" whitespace eof
      { true }
  | whitespace "0" whitespace eof
      { false }
      
(* xs:date *)
      
(* From http://www.w3c.org/TR/xpath-functions/ *)
(* Extend to include optional time zone :::::::  Taken care of possibly -Doug *)

and parse_date = parse
  | whitespace ((_C _C _Y _Y as year) "-" (_M _M as month) "-" (_D _D as day))((_rel as rel)(_h _h as hour) ":" (_m _m as minute)) whitespace eof
      { let zone =
	match rel with
	| '+' -> 1
	| '-' -> -1
	| _ -> raise (Query (Protocol_Error ("Has to be either a + or a -")))
      in
      DateTime.mkdate(Some(int_of_string year), int_of_string month, int_of_string day, 
		      Some(DateTime.mkdayTimeDuration(0,(int_of_string hour) * zone, (int_of_string minute) * zone, Decimal._decimal_zero)) )
	}
      
  | whitespace ((_C _C _Y _Y as year) "-"(_M _M as month) "-" (_D _D as day)) (_UTC) whitespace eof
      { DateTime.mkdate (Some(int_of_string year), int_of_string month, int_of_string day, 
			 Some(DateTime.mkdayTimeDuration(0, 0, 0, Decimal._decimal_zero))) }
  | whitespace (_C _C _Y _Y as year) "-"(_M _M as month) "-" (_D _D as day) whitespace eof
      { DateTime.mkdate (Some(int_of_string year), int_of_string month, int_of_string day, None) }

and parse_gYearMonth = parse
  | whitespace (((_C _C _Y _Y as year)) "-" (_M _M as month)) ((_rel as rel)(_h _h as hour) ":" (_m _m as minute)) whitespace eof
      { let zone =
	match rel with
	| '+' -> 1
	| '-' -> -1
	| _ -> raise (Query (Protocol_Error ("Has to be either a + or a -")))
      in
      DateTime.mkgYearMonth((int_of_string year), int_of_string month, 
		      Some(DateTime.mkdayTimeDuration(0,(int_of_string hour) * zone, (int_of_string minute) * zone, Decimal._decimal_zero)) )
	}
      
  | whitespace (((_C _C _Y _Y as year)) "-"(_M _M as month) (_UTC)) whitespace eof
      { 
    DateTime.mkgYearMonth ((int_of_string year), int_of_string month, 
			 Some(DateTime.mkdayTimeDuration(0, 0, 0, Decimal._decimal_zero))) }
  | whitespace (_C _C _Y _Y as year) "-"(_M _M as month) whitespace eof
      { DateTime.mkgYearMonth (int_of_string year, int_of_string month, None) }

and parse_gYear = parse
  | whitespace ((_C _C _Y _Y as year))((_rel as rel)(_h _h as hour) ":" (_m _m as minute)) whitespace eof
      {let zone =
	match rel with
	| '+' -> 1
	| '-' -> (-1)
	| _ -> raise (Query (Protocol_Error ("Has to be either a + or a -")))
      in
      DateTime.mkgYear((int_of_string year), 
		      Some(DateTime.mkdayTimeDuration(0,(int_of_string hour) * zone, (int_of_string minute) * zone, Decimal._decimal_zero)) )
	}
      
  | whitespace ((_C _C _Y _Y as year) (_UTC)) whitespace eof
      { DateTime.mkgYear ((int_of_string year), 
			 Some(DateTime.mkdayTimeDuration(0, 0, 0, Decimal._decimal_zero))) }
  | whitespace ((_C _C _Y _Y as year)) whitespace eof
      { DateTime.mkgYear ((int_of_string year), None) }

and parse_gMonthDay = parse
  | whitespace ("--" (_M _M as month) "-" (_D _D as day))((_rel as rel)(_h _h as hour) ":" (_m _m as minute)) whitespace eof
      { let zone =
	match rel with
	| '+' -> 1
	| '-' -> -1
	| _ -> raise (Query (Protocol_Error ("Has to be either a + or a -")))
      in
      DateTime.mkgMonthDay(int_of_string month, int_of_string day, 
		      Some(DateTime.mkdayTimeDuration(0,(int_of_string hour) * zone, (int_of_string minute) * zone, Decimal._decimal_zero)) )
	}
  | whitespace ("--"(_M _M as month) "-" (_D _D as day)) (_UTC) whitespace eof
      { DateTime.mkgMonthDay (int_of_string month, int_of_string day, 
			 Some(DateTime.mkdayTimeDuration(0, 0, 0, Decimal._decimal_zero))) }
  | whitespace "--"(_M _M as month) "-" (_D _D as day) whitespace eof
      { DateTime.mkgMonthDay (int_of_string month, int_of_string day, None) }

and parse_gDay = parse
  | whitespace ("---" (_D _D as day))((_rel as rel)(_h _h as hour) ":" (_m _m as minute)) whitespace eof
      { let zone =
	match rel with
	| '+' -> 1
	| '-' -> -1
	| _ -> raise (Query (Protocol_Error ("Has to be either a + or a -")))
      in
      DateTime.mkgDay(int_of_string day, 
		      Some(DateTime.mkdayTimeDuration(0,(int_of_string hour) * zone, (int_of_string minute) * zone, Decimal._decimal_zero)) )
	}
      
  | whitespace ("---" (_D _D as day)) (_UTC) whitespace eof
      { DateTime.mkgDay (int_of_string day, 
			 Some(DateTime.mkdayTimeDuration(0, 0, 0, Decimal._decimal_zero))) }
  | whitespace "---" (_D _D as day) whitespace eof
      { DateTime.mkgDay (int_of_string day, None) }
      
and parse_gMonth = parse
  | whitespace ("--" (_M _M as month))((_rel as rel)(_h _h as hour) ":" (_m _m as minute)) whitespace eof
      { let zone =
	match rel with
	| '+' -> 1
	| '-' -> -1
	| _ -> raise (Query (Protocol_Error ("Has to be either a + or a -")))
      in
      DateTime.mkgMonth(int_of_string month, 
		      Some(DateTime.mkdayTimeDuration(0,(int_of_string hour) * zone, (int_of_string minute) * zone, Decimal._decimal_zero)) )
	}
  | whitespace ("--"(_M _M as month)) (_UTC) whitespace eof
      { DateTime.mkgMonth (int_of_string month, 
			 Some(DateTime.mkdayTimeDuration(0, 0, 0, Decimal._decimal_zero))) }
  | whitespace "--"(_M _M as month) whitespace eof
      { DateTime.mkgMonth (int_of_string month, None) }

(* xs:time *)
(* Extend to include optional time zone *)
      
(*
    F&O 17.1.5 Casting to date and time types

   When a value of any primitive type is cast as xs:date, the xs:date
   value TV is derived from ST and SV as follows:

    * If ST is xs:untypedAtomic or xs:string, see 17.1.1 Casting from
      xs:string and xs:untypedAtomic.
*)

and parse_time = parse
  | whitespace ((_h _h as hour) ":" (_m _m as minute) ":" (_s _s as second) ("." (_s+ as fracsecs))?)((_rel as rel)(_h _h as tzhour) ":" (_m _m as tzminute)) whitespace eof
      { let fsstr = 
        match fracsecs with
        | None -> ""
        | Some fs -> "." ^ fs 
      in 
      let zone = 
	match rel with 
	| '+' -> 1
        | '-' -> -1
	| _ -> raise (Query (Protocol_Error ("Has to be either a + or a -")))
      in 
      DateTime.mktime(int_of_string hour, int_of_string minute, Decimal._decimal_of_string (second ^ fsstr),
	              Some(DateTime.mkdayTimeDuration(0, (int_of_string tzhour) * zone, (int_of_string tzminute) * zone, 
						      Decimal._decimal_zero)))
      } 
  | whitespace ((_h _h as hour) ":" (_m _m as minute) ":" (_s _s as second) ("." (_s+ as fracsecs))?) (_UTC) whitespace eof
      { let fsstr =
	match fracsecs with
	| None -> ""
	| Some fs -> "." ^ fs
      in
      DateTime.mktime(int_of_string hour, int_of_string minute, Decimal._decimal_of_string(second ^ fsstr), 
		      Some(DateTime.mkdayTimeDuration(0,0,0, Decimal._decimal_zero))) }
  | whitespace ((_h _h as hour) ":" (_m _m as minute) ":" (_s _s as second) ("." (_s+ as fracsecs))?) whitespace eof
      { let fsstr =
	match fracsecs with
	| None -> ""
	| Some fs -> "." ^ fs
      in
      DateTime.mktime(int_of_string hour, int_of_string minute, Decimal._decimal_of_string(second ^ fsstr), None)  }
      
(* xs:dateTime *)
and parse_dateTime = parse
  | whitespace ((_C _C _Y _Y as year) "-"(_M _M as month) "-" (_D _D as day)) 'T' 
      ((_h _h as hour) ":" (_m _m as minute) ":" (_s _s as second) ("." (_s+ as fracsecs))?)
      ((_rel as rel)(_h _h as tzhour) ":" (_m _m as tzminute)) whitespace eof
      { 
    let zone = match rel with
    | '+' -> 1
    | '-' -> -1 
    | _ -> raise (Query (Protocol_Error ("Has to be either a + or a -"))) in
    let tmp_date = DateTime.mkdate(Some(int_of_string year), int_of_string month, int_of_string day, 
                                   Some(DateTime.mkdayTimeDuration(0, (int_of_string tzhour) * zone, (int_of_string tzminute) * zone,
								   Decimal._decimal_zero))) and
	fsstr =
      match fracsecs with
      | None -> ""
      | Some fs -> "." ^ fs
    in
    let (dtd,tmp_time) = DateTime.mktime_dtd(int_of_string hour, int_of_string minute, Decimal._decimal_of_string (second ^ fsstr),
				   Some(DateTime.mkdayTimeDuration(0, (int_of_string tzhour) * zone, (int_of_string tzminute) * zone, 
							      Decimal._decimal_zero))) in
    DateTime.mkdateTime(DateTime.add_dayTimeDuration_to_date tmp_date dtd, tmp_time, 
			Some(DateTime.mkdayTimeDuration(0, (int_of_string tzhour) * zone, (int_of_string tzminute) * zone, 
							      Decimal._decimal_zero))) 
   }
      
  | whitespace ((_C _C _Y _Y as year) "-"(_M _M as month) "-" (_D _D as day)) 'T' 
      ((_h _h as hour) ":" (_m _m as minute) ":" (_s _s as second) ("." (_s+ as fracsecs))?) (_UTC) whitespace (* Why can't eof be here? -Mary *)
   { 
   let tmp_date = DateTime.mkdate(Some(int_of_string year), int_of_string month, int_of_string day, 
                                   Some(DateTime.mkdayTimeDuration(0,0,0, Decimal._decimal_zero))) and
   fsstr =
   match fracsecs with
      | None -> ""
      | Some fs -> "." ^ fs
   in
     let (dtd,tmp_time) = DateTime.mktime_dtd(int_of_string hour, int_of_string minute, Decimal._decimal_of_string (second ^ fsstr), 
       Some(DateTime.mkdayTimeDuration(0,0,0, Decimal._decimal_zero))) in
     DateTime.mkdateTime(DateTime.add_dayTimeDuration_to_date tmp_date dtd, tmp_time, Some(DateTime.mkdayTimeDuration(0,0,0, Decimal._decimal_zero)))
   }
  | whitespace ((_C _C _Y _Y as year) "-" (_M  _M as month) "-" (_D _D as day)) 'T'
      ((_h _h as hour) ":" (_m _m as minute) ":" (_s _s as second) ("." (_s+ as fracsecs))?) whitespace eof
      {
    let tmp_date = DateTime.mkdate(Some(int_of_string year), int_of_string month, int_of_string day, None) and
	fsstr =
      match fracsecs with
      | None -> ""
      | Some fs -> "." ^ fs
    in
    let (dtd,tmp_time) = DateTime.mktime_dtd(int_of_string hour, int_of_string minute, Decimal._decimal_of_string (second ^ fsstr), None) in
    DateTime.mkdateTime(DateTime.add_dayTimeDuration_to_date tmp_date dtd, tmp_time, None)
}
      
(* xs:yearMonthDuration *)
(*
  [\-]?P[0-9]+(Y([0-9]+M)?|M)
*)
and parse_yearMonthDuration = parse
  | whitespace (_minus as neg)? 'P' (_Y+ as years) 'Y' (_M+ as months) 'M' whitespace eof
      { let mult =
	match neg with
	| None -> 1
	| Some _ -> (-1) in
      DateTime.mkyearMonthDuration (int_of_string(years) * mult, int_of_string(months) * mult) }
  | whitespace (_minus as neg)? 'P' (_Y+ as years) 'Y' whitespace eof
      { let mult = 
	match neg with 
	| None -> 1
	| Some _ -> (-1) in
      DateTime.mkyearMonthDuration (int_of_string(years) * mult, 0)}
  | whitespace (_minus as neg)? 'P' (_M+ as months) 'M' whitespace eof
      { let mult = 
	match neg with
	| None -> 1
	| Some _ -> (-1) in
      DateTime.mkyearMonthDuration (0, int_of_string(months) * mult)
      }	
      
(* xs:dayTimeDuration *)
and parse_dayTimeDuration = parse
  | whitespace (_minus as neg)? 'P' (_D+ as days) 'D' whitespace eof
      { let mult =
	match neg with
	| None -> 1
	| Some _ -> (-1) in
      DateTime.mkdayTimeDuration (int_of_string(days) * mult, 0, 0, Decimal._decimal_of_string("0.")) }
  | whitespace (_minus as neg)? 'P' ((_D+ as days) 'D') (('T' ((_h+ as hours) 'H')? 
				    ((_m+ as minutes) 'M')? ((_s+ as seconds) ('.' (_s+ as fracsecs))? 'S')? ) as time)? whitespace eof
      { 
      let _  =
	match time with 
      |	None -> ()
      |	Some _ -> 
	  if not(Gmisc.is_some hours || Gmisc.is_some minutes || Gmisc.is_some seconds) then
	    raise (Invalid_argument "Invalid xs:dayTimeDuration")
	  else ()
      in 
      let mult = 
	match neg with 
	| None -> 1
	| Some _ -> (-1)
      and fsstr =
	match fracsecs with
	| None -> ""
	| Some fs -> "." ^ fs in
      let tmphours = 
	match hours with
	| None -> 0
	| Some x -> int_of_string(x) * mult
      and tmpminutes = 
	match minutes with
	| None -> 0
	| Some x -> int_of_string(x) * mult
      and tmpseconds =
	match seconds with
	| None -> Decimal._decimal_of_string("0")
	| Some x -> Decimal._decimal_mult (Decimal._decimal_of_string(x ^ fsstr)) (Decimal._decimal_of_string(string_of_int mult)) 
      and tmpdays = (int_of_string days) * mult in
      DateTime.mkdayTimeDuration(tmpdays, tmphours, tmpminutes, tmpseconds) }
  | whitespace (_minus as neg)? "PT" ((_h+ as hours) 'H') ((_m+ as minutes) 'M')? ((_s+ as seconds) ('.' (_s+ as fracsecs))? 'S')? whitespace eof
      { let mult = 
	match neg with 
	| None -> 1
	| Some _ -> (-1)
      and fsstr =
	match fracsecs with
	| None -> ""
	| Some fs -> "." ^ fs in
      let tmphours =
	(int_of_string hours) * mult
      and tmpminutes = 
	match minutes with
	| None -> 0
	| Some x -> int_of_string(x) * mult
      and tmpseconds =
	match seconds with
	| None -> Decimal._decimal_of_string("0")
	| Some x -> Decimal._decimal_mult (Decimal._decimal_of_string(x ^ fsstr)) (Decimal._decimal_of_string(string_of_int mult)) in 
      DateTime.mkdayTimeDuration(0, tmphours, tmpminutes, tmpseconds) }
  | whitespace (_minus as neg)? "PT" ((_h+ as hours) 'H')? ((_m+ as minutes) 'M') ((_s+ as seconds) ('.' (_s+ as fracsecs))? 'S')? whitespace eof
      { let mult = 
	match neg with 
	| None -> 1
	| Some _ -> (-1)
      and fsstr =
	match fracsecs with
	| None -> ""
	| Some fs -> "." ^ fs in
      let tmpminutes =
	(int_of_string minutes) * mult
      and tmphours = 
	match hours with
	| None -> 0
	| Some x -> int_of_string(x) * mult
      and tmpseconds =
	match seconds with
	| None -> Decimal._decimal_of_string("0")
	| Some x -> Decimal._decimal_mult (Decimal._decimal_of_string(x ^ fsstr)) (Decimal._decimal_of_string(string_of_int mult)) in 
      DateTime.mkdayTimeDuration(0, tmphours, tmpminutes, tmpseconds) }
  | whitespace (_minus as neg)? "PT" ((_h+ as hours) 'H')? ((_m+ as minutes) 'M')? (_s+ as seconds) 
      ('.' (_s+ as fracsecs))? 'S' whitespace eof
      { let mult = 
	match neg with
	| None -> 1
	| Some _ -> (-1) 
      and fsstr = 
	match fracsecs with
	| None -> ""
	| Some fs -> "." ^ fs in
      let tmpminutes =
	match minutes with
	| None -> 0
	| Some x -> int_of_string(x) * mult
      and tmphours = 
	match hours with
	| None -> 0
	| Some x -> int_of_string(x) * mult
      and tmpseconds = Decimal._decimal_mult (Decimal._decimal_of_string(seconds ^ fsstr)) (Decimal._decimal_of_string(string_of_int mult)) in 
      DateTime.mkdayTimeDuration(0, tmphours, tmpminutes, tmpseconds) 
}

    
(* 
   '-'? 'P' (_D+ as days) 'D' ('T'([0-9]+(H([0-9]+(M([0-9]+(\.[0-9]* )?S
   |\.[0-9]+S)?|(\.[0-9]* )?S)|(\.[0-9]* )?S)?|M([0-9]+
   (\.[0-9]* )?S|\.[0-9]+S)?|(\.[0-9]* )?S)|\.[0-9]+S))?
   |T([0-9]+(H([0-9]+(M([0-9]+(\.[0-9]* )?S|\.[0-9]+S)?
   |(\.[0-9]* )?S)|(\.[0-9]* )?S)?|M([0-9]+(\.[0-9]* )?S|\.[0-9]+S)?
   |(\.[0-9]* )?S)|\.[0-9]+S))
*)
