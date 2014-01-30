(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: dateTime.ml,v 1.37 2007/02/01 22:08:46 simeon Exp $ *)

(* Module: DateTime
   Description:
     This modules implement internal support for the XML Schema
     primitive simple types related to date and time.
 *)

(** DateTime module to process dates/times/duration
   @author Doug Petkanics
*)

open Error
open Str
open Lexing
open Unix

open Gmisc 
open Decimal

(* Some constants *)

let ten      = Decimal._decimal_of_int 10
let minusone = Decimal._decimal_of_int (-1)
let sixty    = Decimal._decimal_of_int 60
let sixtyi   = Decimal._integer_of_int 60
let tfi      = Decimal._integer_of_int 24

(* types *)

(** Offset of the timezone from UTC *)    
type _TZrel = 
    Unknown    (* Timezone is unkown *)
  | Positive   (* Timezone has a positive offset from UTC *)
  | Negative   (* Timezone has a negative offset from UTC *)
  | UTC        (* Timezone is UTC *)
      
type _timezone =
    { tz_hours   : int;
      tz_minutes : int;
      tz_rel     : _TZrel }
      
type xs_time =
    { t_hours   : int;
      t_minutes : int;
      t_seconds : Decimal._decimal; 
      t_timezone : _timezone }
      
type xs_date =
    { d_year  	 : int;
      d_month 	 : int;
      d_day   	 : int;
      d_timezone : _timezone }
      
type xs_dateTime =
    { dt_date     : xs_date;
      dt_time     : xs_time;
      dt_timezone : _timezone }

type xs_gYearMonth = xs_date
type xs_gYear      = xs_date
type xs_gMonthDay  = xs_date
type xs_gDay       = xs_date
type xs_gMonth     = xs_date

(* Duration Datatypes *)

type xs_yearMonthDuration =
    { ymd_years  : int;
      ymd_months : int } 

type xs_dayTimeDuration =
    { dtd_days    : int;
      dtd_hours   : int;
      dtd_minutes : int;
      dtd_seconds : _decimal } 
      
type xs_duration =
    { dur_sign : bool;
      dur_ymd  : xs_yearMonthDuration;
      dur_dtd  : xs_dayTimeDuration }


(****************)

(* those are coming out of XML Schema Part 2. Appendix E
   - Jerome *)

let fQuotient (a, b) = (* the greatest integer less than or equal to a/b *)
  (Decimal._decimal_floor (Decimal._decimal_div a b))
(*
    * fQuotient(-1,3) = -1
    * fQuotient(0,3)...fQuotient(2,3) = 0
    * fQuotient(3,3) = 1
    * fQuotient(3.123,3) = 1
*)

let modulo (a, b) = (* a - fQuotient(a,b)*b *)
  Decimal._decimal_sub a (Decimal._decimal_mult (fQuotient(a,b)) b)

(*
          o modulo(-1,3) = 2
          o modulo(0,3)...modulo(2,3) = 0...2
          o modulo(3,3) = 0
          o modulo(3.123,3) = 0.123
*)

let fQuotientT(a, low, high) = (* fQuotient(a - low, high - low) *)
  fQuotient(Decimal._decimal_sub a low,Decimal._decimal_sub high low)

(*
          o fQuotient(0, 1, 13) = -1
          o fQuotient(1, 1, 13) ... fQuotient(12, 1, 13) = 0
          o fQuotient(13, 1, 13) = 1
          o fQuotient(13.123, 1, 13) = 1
*)

let moduloT(a, low, high) = (* modulo(a - low, high - low) + low *)
  Decimal._decimal_add
    (modulo (Decimal._decimal_sub a low,Decimal._decimal_sub high low))
    low

(*
          o modulo(0, 1, 13) = 12
          o modulo(1, 1, 13) ... modulo(12, 1, 13) = 1...12
          o modulo(13, 1, 13) = 1
          o modulo(13.123, 1, 13) = 1.123
*)

let maximumDayInMonthFor(yearValue, monthValue) =
  let yearValue = Decimal._decimal_of_int yearValue in
  let monthValue = Decimal._decimal_of_int monthValue in
  let m =
    Decimal._int_of_decimal
      (moduloT(monthValue,Decimal._decimal_of_int 1,Decimal._decimal_of_int 13))
  in
  let y =
    Decimal._int_of_decimal
      (Decimal._decimal_add
	 yearValue
	 (fQuotientT(monthValue,Decimal._decimal_of_int 1,Decimal._decimal_of_int 13)))
  in
  match m with
  | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
  | 4 | 6 | 9 | 11 -> 30
  | 2 ->
      if ((y mod 400 = 0) || (not(y mod 100 = 0) && (y mod 4 = 0)))
      then 29
      else 28
  | _ ->
      raise (Query(Malformed_DateTimeValue("Month is not between 1 and 12")))

(*
          o M := modulo(monthValue, 1, 13)
          o Y := yearValue + fQuotient(monthValue, 1, 13)
          o Return a value based on M and Y:

31 	M = January, March, May, July, August, October, or December
30 	M = April, June, September, or November
29 	M = February AND (modulo(Y, 400) = 0 OR (modulo(Y, 100) != 0) AND modulo(Y, 4) = 0)
28 	Otherwise
*)

(****************)

(* VALIDITY CHECKING *)

(** Check to see if the given year is a leapyear.  The first leap year
    was in 1582, then there is a leapyear in every year divisible by 4
    that is not divisible by 100 unless it is also divisible by 400
    @param year The year to check @return true if year is a leapyear,
    false otherwise *)

let leapyear year =
  if (* year > 1582 &&*) (year mod 400 = 0 || (year mod 4 = 0 && not (year mod 100 = 0)))
  then true
  else false

(** Check to see if the given dayTimeDuration is a valid timezone
   @param dtd The day time duration
   @return True if the absolute value of dtd is <= 14 hours *)
let valid_timezone dtd =
  match (abs(dtd.dtd_hours), abs(dtd.dtd_minutes)) with
  | (14, 0) -> ()
  | (hours, minutes) ->
      if hours > 13 || minutes > 59
      then raise (Query (Validation("Not a valid timezone")))
      else ()

(** Checks to see if the given day is valid for the given month, and
   ensures that all values are in range. In 1582 there was no 5th
   through 14 of October due to a decree by Pope Gregory XIII
   @param yearmonthday The (year * month * day) to check
   @return True if it is a valid date, false otherwise *)
let valid_date (yearopt, month, day) =
  let validyear =
    match yearopt with
    | None -> true
    | Some year ->
	(if year = 1582 && day >= 4 && day < 15 && month = 10
	then false
	else true)
  in
  let validmonth = (month >= 1 && month <= 12) in
  let validday =
    match (yearopt, month, day) with
    | (Some curyear, 2, 29) -> leapyear curyear
    | (_, 2, 30) -> false
    | (_, curmonth, 31) ->
	(curmonth = 1
       || curmonth = 3
       || curmonth = 5
       || curmonth = 7
       || curmonth = 8
       || curmonth = 10
       || curmonth = 12)
    | (_, curmonth, curday) -> (curday >= 1 && curday <= 31)
  in
  if (validyear && validmonth && validday)
  then ()
  else raise (Query (Validation("Not a valid date")))

(** Checks an hour minutes and second to determine if the time given is valid *)
let valid_time (h, m, s) =
  if (h < 0 || h > 23) ||
    (m < 0 || m > 59) ||
    (Decimal._decimal_lt s Decimal._decimal_zero) ||
    (Decimal._decimal_ge s sixty)
  then raise (Query (Validation("Not a valid time")))
  else ()

(** Checks a dateTime to see if the individual date and time are valid *)
let valid_dateTime d t =
  try
    valid_date (Some d.d_year, d.d_month, d.d_day);
    valid_time (t.t_hours, t.t_minutes, t.t_seconds)
  with
  | _ ->
      raise (Query (Validation("Not a valid dateTime")))

(** The 'create' functions below are used to map 
   the given values in the 'mk' functions which are
   available in the interface, to the internal representations 
   of the datatypes. *)

(** Create timezone constructs a timezone and figures
   out what the tz_rel is supposed to be based upon the 
   hours and minutes of the timezone offset, rather
   than the input value which potentially could be
   incorrect *)
let create_timezone (tzrel, hours, minutes) =
  { tz_hours = hours;
    tz_minutes = minutes;
    tz_rel =
    match tzrel with
    | Unknown -> Unknown
    | _ -> if hours > 0 || minutes > 0 then Positive else
      if hours < 0 || minutes < 0 then Negative
      else UTC }
    
(** Creates a time *)
let create_time (h, m, s, tzrel, hours, minutes) =
  { t_hours   = h;
    t_minutes = m;
    t_seconds = s; 
    t_timezone = create_timezone(tzrel, hours, minutes) }

let create_time_with_timezone (h, m, s, tz) =
  { t_hours   = h;
    t_minutes = m;
    t_seconds = s; 
    t_timezone = tz }

(** Creates a date *)
let create_date (y, m, d, tzrel, hours, minutes) =
  { d_year  = y; 
    d_month = m;
    d_day   = d;
    d_timezone = create_timezone(tzrel, hours, minutes) }

let create_date_with_timezone (y, m, d, tz) =
  { d_year  = y; 
    d_month = m;
    d_day   = d;
    d_timezone = tz }
    

(** Creates a dateTime *)
let create_dateTime (dt, tm, tz) =
  { dt_date = dt;
    dt_time = tm;
    dt_timezone = tz }

let create_dateTime_from_date dt =
  let tz = dt.d_timezone in
  let zero_time = create_time_with_timezone (0,0,Decimal._decimal_zero,tz) in
  create_dateTime (dt,zero_time,tz)

(** Creates a yearMonthDuration *)
let create_yearMonthDuration (years, months) =
  { ymd_years = years;
    ymd_months = months } 

(* Creates a dayTimeDuration *)
let create_dayTimeDuration (days, hours, minutes, seconds) =
  { dtd_days    = days;
    dtd_hours   = hours;
    dtd_minutes = minutes;
    dtd_seconds = seconds } 
    
(* return to string functions *)

(** Ensure that hours are represented as two digits 
   @param h hours
   @return string containing two digits *)
let string_of_hours h =
  if abs(h) > 9 then string_of_int h
  else let tmph = "00" ^ (string_of_int h) in
  let len = 
    try String.length tmph
    with Invalid_argument(x) -> raise (Query(Malformed_DateTimeValue("Can't get the length of string tmph"))) in
  let hourstring =
    try String.sub tmph (len - 2) 2
    with Invalid_argument(x) -> raise (Query(Malformed_DateTimeValue("can't get the substring of tmph from of length 2 from len - 2"))) in
  hourstring
   

(** Ensure that minutes, like hours, are represented as two digits 
   @param m Minutes
   @return string containing two digits *)
let string_of_minutes m =
  string_of_hours m

(** Ensure that if seconds are less than 10, then a leading 0 is appended on
   @param s Seconds
   @return Decimal number of seconds with at least two digits before
   the decimal point *)
let string_of_seconds s =
  let sstring = Decimal._string_of_decimal s in
  let tmpstring = 
    if Decimal._decimal_lt s ten
    then "0" ^ sstring
    else sstring in
  tmpstring

(** Display timezone correctly based upon the relative offset 
   @param tz The Timezone
   @return The string representation of the timezone *)
let string_of_timezone tz =
    match tz.tz_rel with
    | Unknown -> ""
    | UTC -> "Z"
    | Positive -> "+" ^ string_of_hours (tz.tz_hours) ^ ":" ^ string_of_minutes (tz.tz_minutes)
    | Negative -> "-" ^ string_of_hours (abs(tz.tz_hours)) ^ ":" ^ string_of_minutes (abs(tz.tz_minutes))

(** Display the time correctly. string_of_seconds can
   possibly come back with a trailing decimal point.
   If this is the case remove the decimal point. 
   @param tm Time
   @return The string representation of the time *)
let string_of_time tm = 
  let stringsecs = string_of_seconds (tm.t_seconds) in
   let tmpsecs = 
    try
      if String.get stringsecs ((String.length stringsecs) - 1) = '.' then 
	String.sub stringsecs 0 ((String.length stringsecs) - 1) else stringsecs 
  with Invalid_argument(x) -> raise (Query(Malformed_DateTimeValue("In string_of_time: Can't create tmpsecs"))) in 
  let stringtz = string_of_timezone tm.t_timezone in
  (string_of_hours (tm.t_hours)) ^ ":" ^ (string_of_minutes (tm.t_minutes)) ^ ":" ^
  tmpsecs ^ stringtz

(** The year must be a four digit number 
   @param y Year
   @return The string representation of a year *)
let string_of_year y =
  if (abs y) > 999
  then
    string_of_int y
  else
    let tmpy = "0000" ^ (string_of_int (abs y)) in
    let len =
      try String.length tmpy 
      with Invalid_argument(x) -> raise (Query(Malformed_DateTimeValue("In string_of_year: Can't take length of tmpy"))) and
	neg = if y < 0 then "-" else "" in
    try neg ^ (String.sub tmpy (len - 4) 4)
    with Invalid_argument(x) -> raise (Query(Malformed_DateTimeValue("In string_of_year: Can't take the substring of tmpy")))

(** The month must be a two digit number 
   @param m Month
   @return The string representation of the month *)
let string_of_month m =
  if abs(m) > 9 then string_of_int m
  else let tmpm = "00" ^ (string_of_int m) in
  let len = 
    try String.length tmpm 
    with Invalid_argument(x) -> raise (Query(Malformed_DateTimeValue("In string_of_month: Can't take the length of tmpm"))) in
  try String.sub tmpm (len - 2) 2
  with Invalid_argument(x) -> raise (Query(Malformed_DateTimeValue("In string_of_month: Can't take the substring of tmpm")))

(** Like month, the string must have two digits for a day
   @param d Day
   @return The string form of a day *)
let string_of_day d = string_of_month d
  
(** String form of a date
   @param date Date
   @return The string representation of a date YYYY-MM-DD ^ timezone *)

let string_of_date date =
  let stringtz = string_of_timezone (date.d_timezone)  in
  (string_of_year date.d_year) ^ "-" ^ (string_of_month date.d_month) ^ "-" ^ (string_of_day date.d_day) ^ stringtz

let string_of_gYearMonth date =
  let stringtz = string_of_timezone (date.d_timezone)  in
  (string_of_year date.d_year) ^ "-" ^ (string_of_month date.d_month) ^ stringtz

let string_of_gYear date =
  let stringtz = string_of_timezone (date.d_timezone)  in
  (string_of_year date.d_year) ^ stringtz

let string_of_gMonthDay date =
  let stringtz = string_of_timezone (date.d_timezone)  in
  "--" ^ (string_of_month date.d_month) ^ "-" ^ (string_of_day date.d_day) ^ stringtz

let string_of_gDay date =
  let stringtz = string_of_timezone (date.d_timezone)  in
  "---" ^ (string_of_day date.d_day) ^ stringtz

let string_of_gMonth date =
  let stringtz = string_of_timezone (date.d_timezone)  in
  "--" ^ (string_of_month date.d_month) ^ stringtz

(** String form of a dateTime. Make sure that the seconds don't have
   a trailing decimal point if they don't need to.
   @param dt dateTime
   @return String rep of a dateTime *)
let string_of_dateTime dt =
  let date = dt.dt_date and
      tm = dt.dt_time and
      stringtz = string_of_timezone (dt.dt_timezone) in
  let stringsecs = string_of_seconds (tm.t_seconds) in
  let tmpsecs =
    try
      if String.get stringsecs ((String.length stringsecs) - 1) = '.'
      then String.sub stringsecs 0 ((String.length stringsecs) - 1)
      else stringsecs
    with Invalid_argument _ ->
      raise (Query(Malformed_DateTimeValue("In string_of_dateTime: Can't compute tmpsecs")))
  in
  (string_of_year date.d_year) ^ "-" ^ (string_of_month date.d_month) ^ "-" ^ (string_of_day date.d_day) ^ "T" ^
  (string_of_hours (tm.t_hours)) ^ ":" ^ (string_of_minutes (tm.t_minutes)) ^ ":" ^
  tmpsecs ^ stringtz
  
(** String form of a yearMonthDuration. If either years or months = 0 then
   do not include the Y or the M in the string.
   @param ymd The yearMonthDuration
   @return The String form of ymd *)

let value_of_yearMonthDuration (x,y) =
  x * 12 + y

let string_of_year_month_pair (x,y) =
  match (x,y) with
  | 0,0 ->
      "P0M"
  | _ ->
      let neg = if value_of_yearMonthDuration (x,y) < 0 then "-" else "" in
      let years =
	if x = 0
	then ""
	else
	  (string_of_int (abs(x))) ^ "Y"
      and months =
	if y = 0
	then ""
	else
	    (string_of_int (abs(y))) ^ "M"
      in
      neg ^ "P" ^ years ^ months

let string_of_yearMonthDuration ymd =
  string_of_year_month_pair (ymd.ymd_years,ymd.ymd_months)

let canonical_of_yearMonthDuration ymd =
  let (x,y) = (ymd.ymd_years,ymd.ymd_months) in
  let v = value_of_yearMonthDuration (x,y) in
  let month = v mod 12 in
  let year = v / 12 in
  string_of_year_month_pair (year,month)

(** String form of a dayTimeDuration. If any of the components are 0
   then do not include the abbreviation in the string.
   @param dtd The dayTimeDuration
   @return the String form of the dtd. *)

let value_of_dayTimeDuration (w,x,y,z) =
  let dh = w * 24 + x in
  let dm = dh * 60 + y in
  Decimal._decimal_add
    (Decimal._decimal_mult
       (Decimal._decimal_of_int dm)
       (Decimal._decimal_of_int 60))
    z

let string_of_day_time_tuple (w,x,y,z) =
  if (w = 0) && (x = 0) && (y = 0) && (Decimal._decimal_eq z Decimal._decimal_zero)
  then
    "PT0S"
  else
    let neg =
      if (Decimal._decimal_lt
	    (value_of_dayTimeDuration (w,x,y,z))
	    Decimal._decimal_zero)
      then "-"
      else ""
    in
    let days =
      match w with
      | 0 -> ""
      | x -> (string_of_int (abs(x))) ^ "D" and
	hours =
      match x with
      | 0 -> ""
      | x -> (string_of_int (abs(x))) ^ "H" and
	minutes = 
      match y with
      | 0 -> ""
      | x -> (string_of_int (abs(x))) ^ "M" and
	seconds = 
      match Decimal._string_of_decimal z with
      | "00." -> ""
      | "00" -> ""
      | "0" -> ""
      | "0." -> ""
      | x -> if String.get x 0 = '-' then (String.sub x 1 (String.length x - 1)) ^ "S" else x ^ "S" in
    let time = if hours ^ minutes ^ seconds = "" then "" else "T" in
    neg ^ "P" ^ days ^ time ^ hours ^ minutes ^ seconds

let string_of_dayTimeDuration dtd =
  string_of_day_time_tuple
    (dtd.dtd_days,dtd.dtd_hours,dtd.dtd_minutes,dtd.dtd_seconds)

let canonical_dayTimeDuration dtd =
  let (w,x,y,z) = (dtd.dtd_days,dtd.dtd_hours,dtd.dtd_minutes,dtd.dtd_seconds) in
  let v = value_of_dayTimeDuration (w,x,y,z) in
  let m = Decimal._decimal_idiv v sixty in
  let sec = Decimal._decimal_sub v (Decimal._decimal_mult (Decimal._decimal_of_integer m) sixty) in
  let h = Decimal._integer_idiv m sixtyi in
  let min = Decimal._int_of_integer (Decimal._integer_sub m (Decimal._integer_mult h sixtyi)) in
  let d = Decimal._integer_idiv h tfi in
  let hou = Decimal._int_of_integer (Decimal._integer_sub h (Decimal._integer_mult d tfi)) in
  let day = Decimal._int_of_integer d in
  (day,hou,min,sec)

let canonical_of_dayTimeDuration dtd =
  string_of_day_time_tuple (canonical_dayTimeDuration dtd)

(* F&O 17.1.2 Casting to xs:string and xs:untypedAtomic

  If ST is xs:duration then let SYM be SV cast as xs:yearMonthDuration, 
  and let SDT be SV cast as xs:dayTimeDuration;
  Now, let the next intermediate value, TYM, be SYM cast as TT, and
  let TDT be SDT cast as TT.

  If TYM is "P0M", then TV is TDT. 

  Otherwise, TYM and TDT are merged according to the following rules:

   1. If TDT is "PT0S", then TV is TYM.

   2. Otherwise, TV is the concatenation of all the characters in TYM
      and all the characters except the first "P" and the optional
      negative sign in TDT.
*)
let string_of_duration dur =
  let sign = if dur.dur_sign then "" else "-" in
  let ymds =
    let y = canonical_of_yearMonthDuration dur.dur_ymd in
    snd (Gmisc.split_left_on_char y 'P')
  in
  let dtds =
    let y = canonical_of_dayTimeDuration dur.dur_dtd in
    snd (Gmisc.split_left_on_char y 'P')
  in
  let dur = 
    match ymds,dtds with
    | "0M","T0S" -> "PT0S"
    | _,"T0S" -> sign ^ "P" ^ ymds
	(* day-time-duration already has "T" delimiter in it *)
    | "0M",_ -> sign ^ "P" ^ dtds
(*    | "0M",_ -> sign ^ "PT" ^ dtds *)
    | _ ->
	(* day-time-duration already has "T" delimiter in it *)
	sign ^ "P" ^ ymds ^ dtds
(*	sign ^ "P" ^ ymds ^ "T" ^ dtds *)
  in
  dur
  

(** The following constructors provide easy interfaces to the
    programmer.  However, they all call the 'create' functions above
    which easily convert the parameters into the internal
    representations of the datatypes. *)

(** Makes a date. Makes sure the date and timezone will be valid and 
   figures out the relative timezone offset.
   @param dateTuple A year, month, day, and duration
   @return a new date *)
let mkdate (yearopt, month, day, dtd) =
  begin
    (valid_date (yearopt, month, day));
    let year = 
      match yearopt with 
      |	None -> 1
      |	Some year -> year
    in
    match dtd with
    | None -> create_date (year, month, day, Unknown, 0, 0)
    | Some dtd2 -> 
	begin
	  valid_timezone dtd2;
	  let rel = if dtd2.dtd_hours > 0 || dtd2.dtd_minutes > 0 then Positive
	  else if dtd2.dtd_hours < 0 || dtd2.dtd_minutes < 0 then Negative
	  else UTC in
	  create_date (year, month, day, rel, dtd2.dtd_hours, dtd2.dtd_minutes)
	end
  end

let mkgYearMonth (year,month,dtd) = mkdate (Some year,month,maximumDayInMonthFor(year, month),dtd)
let mkgYear      (year,dtd)       = mkdate (Some year,1,1,dtd)
let mkgMonthDay  (month,day,dtd)  = mkdate (None,month,day,dtd)
let mkgDay       (day,dtd)        = mkdate (None,1,day,dtd)
let mkgMonth     (month,dtd)      = mkdate (None,month,1,dtd)


(** Creats a new dayTimeDuration
   @param dtdTuple a day, hour minute, and second
   @return A new dayTimeDuration *)
let mkdayTimeDuration (days, hours, minutes, seconds) = 
  create_dayTimeDuration (days, hours, minutes, seconds)

let zero_dayTimeDuration = mkdayTimeDuration (0,0,0,Decimal._decimal_zero)


(** Makes a time. Makes sure the time and timezone will be valid and
   figures out the relative timezone offset.
   @param timeTuple An hour minute, second, and duration
   @return A new time *)
let fix_time (hour, minute, second, dtd) =
  if (hour = 24) &&
    (minute = 0) &&
    (Decimal._decimal_eq second Decimal._decimal_zero)
  then
    (mkdayTimeDuration(1,0,0,Decimal._decimal_zero),0,minute,second,dtd)
  else
    (zero_dayTimeDuration,hour,minute,second,dtd)

let mktime (hour, minute, second, dtd) =
  let (_, hour, minute, second, dtd) = fix_time (hour, minute, second, dtd) in
  begin
    valid_time (hour, minute, second);
    match dtd with
    | None -> create_time(hour, minute, second, Unknown, 0, 0)
    | Some dtd2 ->
	begin
	  valid_timezone dtd2;
	  let rel = if dtd2.dtd_hours > 0 || dtd2.dtd_minutes > 0 then Positive
	  else if dtd2.dtd_hours < 0 || dtd2.dtd_minutes < 0 then Negative
	  else UTC in
	  create_time (hour, minute, second, rel, dtd2.dtd_hours, dtd2.dtd_minutes)
	end
  end

let mktime_dtd (hour, minute, second, dtd) =
  let (dtd1, hour, minute, second,dtd) = fix_time (hour, minute, second, dtd) in
  begin
    valid_time (hour, minute, second);
    match dtd with
    | None -> (dtd1, create_time(hour, minute, second, Unknown, 0, 0))
    | Some dtd2 ->
	begin
	  valid_timezone dtd2;
	  let rel = if dtd2.dtd_hours > 0 || dtd2.dtd_minutes > 0 then Positive
	  else if dtd2.dtd_hours < 0 || dtd2.dtd_minutes < 0 then Negative
	  else UTC in
	  (dtd1, create_time (hour, minute, second, rel, dtd2.dtd_hours, dtd2.dtd_minutes))
	end
  end

(** Makes a time without raising an error if the time is not valid.
   This can be useful for doing manipulations of times in some of the 
   required functions.
   @param timeTuple An hour minute, second, and duration
   @return A new time which is possibly out of range *)
let mktime_noerr (hour, minute, second, dtd) =
  match dtd with
  | None -> create_time (hour, minute, second, Unknown, 0, 0)
  | Some dtd2 -> 
      begin
	valid_timezone dtd2;
	let rel = if dtd2.dtd_hours > 0 || dtd2.dtd_minutes > 0 then Positive
	else if dtd2.dtd_hours < 0 || dtd2.dtd_minutes < 0 then Negative
	else UTC in
	create_time (hour, minute, second, rel, dtd2.dtd_hours, dtd2.dtd_minutes)
      end

(** Makes a dateTime. Checks to ensure that it will be valid and raises an error otherwise
   @param dateTimeTuple A date, time and duration 
   @return a new dateTime *)
let mkdateTime (date, time, dtd) =
  begin
    valid_dateTime date time;
    match dtd with
    | None -> create_dateTime (date, time, (create_timezone(Unknown, 0, 0)))
    | Some dtd2 -> 
	begin
	  valid_timezone dtd2;
	  let rel = if dtd2.dtd_hours > 0 || dtd2.dtd_minutes > 0 then Positive
	  else if dtd2.dtd_hours < 0 || dtd2.dtd_minutes < 0 then Negative
	  else UTC in
	  create_dateTime (date,
			   time,
			   (create_timezone(rel, dtd2.dtd_hours, dtd2.dtd_minutes)))
	end
  end

(** Creates a new yearMonthDuration
   @param ymdTuple A year and a month
   @return A new yearMonthDuration *)
let mkyearMonthDuration (years, months) =
  create_yearMonthDuration(years, months)

let zero_yearMonthDuration = mkyearMonthDuration (0,0)

let mkduration sign (ymd,dtd) =
  { dur_sign = sign;
    dur_ymd  = ymd;
    dur_dtd  = dtd }

(** Normalization to UTC time 
   @param tm The time to be normalized
   @return The normalized time *)
let normalize_time tm =
  let offsethours =
    match tm.t_timezone.tz_rel with
    | Positive -> tm.t_timezone.tz_hours * (-1)
    | Negative -> tm.t_timezone.tz_hours * (-1)
    | _ -> 0
  in
  let offsetminutes =
    match tm.t_timezone.tz_rel with
    | Positive -> tm.t_timezone.tz_minutes * (-1)
    | Negative -> tm.t_timezone.tz_minutes * (-1)
    | _ -> 0
  in
  let newmins = 
    match tm.t_timezone.tz_rel with
    | Negative -> (tm.t_minutes + offsetminutes) mod 60
    | Positive ->
	if tm.t_minutes + offsetminutes >= 0
	then (tm.t_minutes + offsetminutes)
	else (60 + tm.t_minutes + offsetminutes)
    | _ -> tm.t_minutes
  in
  let newhours =
    if tm.t_minutes + offsetminutes > 59
    then tm.t_hours + offsethours + 1
    else if tm.t_minutes + offsetminutes < 0
    then tm.t_hours + offsethours - 1
    else (tm.t_hours + offsethours)
  in
  create_time (newhours, newmins, tm.t_seconds, UTC, 0, 0)

let normalize_time_overflow tm =
  let offsethours =
    match tm.t_timezone.tz_rel with
    | Positive -> tm.t_timezone.tz_hours * (-1)
    | Negative -> tm.t_timezone.tz_hours * (-1)
    | _ -> 0
  in
  let offsetminutes =
    match tm.t_timezone.tz_rel with
    | Positive -> tm.t_timezone.tz_minutes * (-1)
    | Negative -> tm.t_timezone.tz_minutes * (-1)
    | _ -> 0
  in
  let newmins = 
    match tm.t_timezone.tz_rel with
    | Negative -> (tm.t_minutes + offsetminutes) mod 60
    | Positive ->
	if tm.t_minutes + offsetminutes >= 0
	then (tm.t_minutes + offsetminutes)
	else (60 + tm.t_minutes + offsetminutes)
    | _ -> tm.t_minutes
  in
  let newhours =
    if tm.t_minutes + offsetminutes > 59
    then tm.t_hours + offsethours + 1
    else if tm.t_minutes + offsetminutes < 0
    then tm.t_hours + offsethours - 1
    else (tm.t_hours + offsethours)
  in
  create_time (newhours, newmins, tm.t_seconds, UTC, 0, 0)

(** Given a dateTime, and a positive or negative direction, increment the date to the next
   day. If the month is supposed to change then it will increment the month, and
   if the year is due to change it will increment the year.
   @param dt The dateTime
   @param offset Either Positive or Negative direction
   @return dateTuple The new year, month, and day *)
let increment_month dt offset = 
  let year = dt.dt_date.d_year in
  match offset with
  | Positive ->
      (match (dt.dt_date.d_month, dt.dt_date.d_day) with
      | (1, 31) -> (year, 2, 1)
      | (2, 29) -> (year,3, 1)
      | (2, 28) -> if leapyear year then (year,2, 29) else (year,3, 1)
      | (3, 31) -> (year, 4, 1)
      | (4, 30) -> (year, 5, 1)
      | (5, 31) -> (year, 6, 1)
      | (6, 30) -> (year, 7, 1)
      | (7, 31) -> (year, 8, 1)
      | (8, 31) -> (year, 9, 1)
      | (9, 30) -> (year, 10, 1)
      | (10, 31) -> (year, 11, 1)
      | (11, 30) -> (year, 12, 1)
      | (12, 31) -> if year = -1 then (1, 1, 1) else (year+1, 1, 1)
      | (m, d) -> (year,m, d+1))
  | Negative ->
      (match (dt.dt_date.d_month, dt.dt_date.d_day) with
      |	(1, 1) -> if year = 1 then (-1, 12, 31) else (year - 1, 12, 31)
      |	(2, 1) -> (year, 1, 31)
      |	(3, 1) -> (year, 2, if leapyear year then 29 else 28)
      |	(4, 1) -> (year, 3, 31)
      |	(5, 1) -> (year, 4, 30)
      |	(6, 1) -> (year, 5, 31)
      |	(7, 1) -> (year, 6, 30)
      |	(8, 1) -> (year, 7, 31)
      |	(9, 1) -> (year, 8, 31)
      |	(10, 1) -> (year, 9, 30)
      |	(11, 1) -> (year, 10, 31)
      |	(12, 1) -> (year, 11, 30)
      |	(m, d) -> (year, m, d - 1))
  | _ -> raise (Query (Protocol_Error("Must be either a Positive or Negative direction.")))

(** Given a date, and a positive or negative direction, increment the date to the next
   day. If the month is supposed to change then it will increment the month, and
   if the year is due to change it will increment the year.
   @param dt The date
   @param offset Either Positive or Negative direction
   @return dateTuple The new year, month, and day *)
let increment_month_given_date dt offset = 
  let year = dt.d_year in
  match offset with
  | Positive ->
      (match (dt.d_month, dt.d_day) with
      | (1, 31) -> (year, 2, 1)
      | (2, 29) -> (year,3, 1)
      | (2, 28) -> if leapyear year then (year,2, 29) else (year,3, 1)
      | (3, 31) -> (year, 4, 1)
      | (4, 30) -> (year, 5, 1)
      | (5, 31) -> (year, 6, 1)
      | (6, 30) -> (year, 7, 1)
      | (7, 31) -> (year, 8, 1)
      | (8, 31) -> (year, 9, 1)
      | (9, 30) -> (year, 10, 1)
      | (10, 31) -> (year, 11, 1)
      | (11, 30) -> (year, 12, 1)
      | (12, 31) -> if year = -1 then (1, 1, 1) else (year+1, 1, 1)
      | (m, d) -> (year,m, d+1))
  | Negative ->
      (match (dt.d_month, dt.d_day) with
      |	(1, 1) -> if year = 1 then (-1, 12, 31) else (year - 1, 12, 31)
      |	(2, 1) -> (year, 1, 31)
      |	(3, 1) -> (year, 2, if leapyear year then 29 else 28)
      |	(4, 1) -> (year, 3, 31)
      |	(5, 1) -> (year, 4, 30)
      |	(6, 1) -> (year, 5, 31)
      |	(7, 1) -> (year, 6, 30)
      |	(8, 1) -> (year, 7, 31)
      |	(9, 1) -> (year, 8, 31)
      |	(10, 1) -> (year, 9, 30)
      |	(11, 1) -> (year, 10, 31)
      |	(12, 1) -> (year, 11, 30)
      |	(m, d) -> (year, m, d - 1))
  | _ -> raise (Query (Protocol_Error("Must be either a Positive or Negative direction.")))

(** Given a dateTime, normalize it with respect to UTC
   The implementation below is a bit of a hack. First find the normalized time. 
   Then call the increment_month function with either a positive or negative
   direction, depending on whether the newtime called for the day to 
   increase, or decrease. If the newtime called for the day to stay the same
   then just keep the date the same.
   @param dt The dateTime
   @return The new dateTime normalized with respect to UTC *)
let normalize_dateTime dt =
  let tm = dt.dt_time and 
      tz = dt.dt_timezone in
  let newtime =
    normalize_time (create_time (tm.t_hours, tm.t_minutes, tm.t_seconds, tz.tz_rel, tz.tz_hours, tz.tz_minutes))
  in
  let ((newyear, newmonth, newday), newhours) =
    if newtime.t_hours > 23 then
      (increment_month dt Positive, newtime.t_hours mod 24)
    else if newtime.t_hours < 0
    then (increment_month dt Negative, 24 + newtime.t_hours) 
    else ((dt.dt_date.d_year, dt.dt_date.d_month, dt.dt_date.d_day), newtime.t_hours)
  in
  create_dateTime(create_date(newyear, newmonth, newday, UTC, 0, 0),
		  create_time(newhours, newtime.t_minutes, newtime.t_seconds, UTC, 0, 0), create_timezone(UTC, 0, 0))


(** Turn a dayTimeDuration into its equivalent number of seconds
   @param dtd The dayTimeDuration
   @return The number of seconds the dayTimeDuration is equal to *)
let normalize_dayTimeDuration dtd =
  value_of_dayTimeDuration (dtd.dtd_days,dtd.dtd_hours,dtd.dtd_minutes,dtd.dtd_seconds)

(** Turn a yearMonthDuration into its equivalent number of months
   @param ymd The yearMonthDuration
   @return The equivalent number of months *)
let normalize_yearMonthDuration ymd = 
  let newmonths = ymd.ymd_months + (12 * ymd.ymd_years) in 
  create_yearMonthDuration (0, newmonths)

(** Negate a dayTimeDuration
   @param dtd A DayTimeDuration
   @return negation of the dayTimeDuration *)
let negate_dayTimeDuration dtd =
  create_dayTimeDuration
    (dtd.dtd_days * -1,
     dtd.dtd_hours * -1,
     dtd.dtd_minutes * -1,
     Decimal._decimal_mult dtd.dtd_seconds minusone)

let negate_date d =
  create_date_with_timezone
    (d.d_year * -1,
     d.d_month,
     d.d_day,
     d.d_timezone)

let negate_gYear = negate_date
let negate_gYearMonth = negate_date

let negate_time t =
  create_time_with_timezone
    (t.t_hours * -1,
     t.t_minutes * -1,
     Decimal._decimal_unary_minus t.t_seconds,
     t.t_timezone)

let negate_dateTime dt =
  create_dateTime
    (negate_date dt.dt_date,
     (*negate_time*) dt.dt_time,
     dt.dt_timezone)

(** Negate a yearMonthDuration
   @param ymd A yearMonthDuration
   @return The negation of a yearMonthDuration *)
let negate_yearMonthDuration ymd =
  create_yearMonthDuration ( ymd.ymd_years * -1, ymd.ymd_months * -1)

(** Sometimes due to many of the manipulations that need to be done
   on dates, times, and durations, the datatypes contain values which are
   not valid, but could easily be represented as valid values. Example:
   a time 12:00:75 is invalid, but 12:01:15 is the same thing but valid.
   The following standardize functions are used to convert invalid values into
   equivalent valid ones *)

let value_of_time_plus_hours prevhours tm =
  let hours = Decimal._decimal_of_int (tm.t_hours + prevhours) in
  let minutes =
    Decimal._decimal_add
      (Decimal._decimal_of_int tm.t_minutes)
      (Decimal._decimal_mult
	 hours
	 sixty)
  in
  let seconds =
    Decimal._decimal_add
      (tm.t_seconds)
      (Decimal._decimal_mult
	 minutes
	 sixty)
  in
  seconds

let value_of_time tm =
  value_of_time_plus_hours 0 tm

(** Standardizes a time value. Make sure seconds and minutes are less
   than 60, and hours are less than 24
   @param tm The Time
   @return The valid time *)

let standardize_time_aux tmval =
  let seconds = Decimal._decimal_mod tmval sixty in
  let tmpminutes = Decimal._decimal_idiv tmval sixty in
  let minutes =
    Decimal._int_of_integer
      (Decimal._integer_mod tmpminutes sixtyi)
  in
  let hours =
    Decimal._int_of_integer
      (Decimal._integer_idiv tmpminutes sixtyi)
  in
  (hours,minutes,seconds)

let standardize_time tm =
  let tmval = value_of_time tm in
  let (hours,minutes,seconds) = standardize_time_aux tmval in
  create_time (hours,
	       minutes,
	       seconds,
	       tm.t_timezone.tz_rel,
	       tm.t_timezone.tz_hours,
	       tm.t_timezone.tz_minutes)

let standardize_time_trunc tm =
  let tmval = value_of_time tm in
  let tmval =
    let tt = Decimal._decimal_mod tmval (Decimal._decimal_of_int 86400) in
    if (Decimal._decimal_lt tt Decimal._decimal_zero)
    then
      (Decimal._decimal_add tt (Decimal._decimal_of_int 86400))
    else
      tt
  in
  let (hours,minutes,seconds) = standardize_time_aux tmval in
  create_time (hours,
	       minutes,
	       seconds,
	       tm.t_timezone.tz_rel,
	       tm.t_timezone.tz_hours,
	       tm.t_timezone.tz_minutes)

let standardize_time_with_days (days,tm) =
  let extrahours = days * 24 in
  let totaltime = value_of_time_plus_hours extrahours tm in
  let (hours,minutes,seconds) = standardize_time_aux totaltime in
  let fixeddays = hours / 24 in
  let fixedhours = hours mod 24 in
  (create_dayTimeDuration (fixeddays - days,0,0,Decimal._decimal_zero),
   create_time (fixedhours,
		minutes,
		seconds,
		tm.t_timezone.tz_rel,
		tm.t_timezone.tz_hours,
		tm.t_timezone.tz_minutes))

(** Standardizes a dayTimeDuration value. Make sure that all time
   values would be valid in a time, and that any carry over is in the
   days place.
   @param dtd The dayTimeDuration
   @return The valid "days heavy" dayTime duration *)
let standardize_dayTimeDuration dtd =
  create_dayTimeDuration (canonical_dayTimeDuration dtd)
  
(** Increment the day in a dateTime
   @param dt a dateTime
   @param daysToAdd The number of days to add on
   @param direction Positive or Negative
   @return The new dateTime *)
let rec increment_day dt daysToAdd direction =
    match daysToAdd with
    | 0 -> dt
    | x ->
	let dttemp =
	  create_dateTime (dt, 
			   (create_time (0, 0, Decimal._decimal_zero, Unknown, 0, 0)), 
			   (create_timezone (Unknown, 0, 0)))
	in 
	let (tmpyear, tmpmonth, tmpday) = increment_month dttemp direction in
	increment_day (create_date
			 (tmpyear,
			  tmpmonth,
			  tmpday,
			  dt.d_timezone.tz_rel,
			  dt.d_timezone.tz_hours,
			  dt.d_timezone.tz_minutes))
	  (if direction = Positive then x - 1 else x + 1) direction

(** Add a dayTimeDuration to a date
   @param dt The Date
   @param dtd The dayTimeDuration
   @return The new date after having the dayTimeDuration added on *)
let add_dayDuration_to_date dt dtd =
  let timeToAdd = (standardize_dayTimeDuration dtd) in
  if timeToAdd.dtd_days >= 0 then increment_day dt timeToAdd.dtd_days Positive
  else increment_day dt timeToAdd.dtd_days Negative

let standardize_dateTime dt =
  let date = dt.dt_date in
  let years = date.d_year in
  let months = date.d_month in
  let days = date.d_day in
  let (dtd,time) = standardize_time_with_days (days,dt.dt_time) in
  let final_date = create_date_with_timezone (years,months,days,date.d_timezone) in
  let corrected_final_date =
    add_dayDuration_to_date final_date dtd
  in
  create_dateTime (corrected_final_date,time,dt.dt_timezone)


(** Standardizes a yearMonthDuration such that months is less than 12
   @param ymd The yearMonthDuration
   @return The standardized value *)
let standardize_yearMonthDuration ymd = 
  create_yearMonthDuration(ymd.ymd_years + (ymd.ymd_months / 12), ymd.ymd_months mod 12)

(** Extraction functions on each type are mostly self explanatory.
   hours_from_time gets the hours from the time. In the case
   of durations it is necessary to standardize them first, so that
   the result of the extraction functions conforms with the result
   of the testcases in the functions and operators document. Even
   though in a duration of P24M it is appropriate to say that there
   are 24 months, the spec expects the output to be 0 months, as the
   standardized form is P2Y0M. *)

let years_from_duration ymd =
  ymd.ymd_years + (ymd.ymd_months / 12)

let months_from_duration ymd = ymd.ymd_months mod 12

let seconds_from_duration dtd =
  let dtdstand = standardize_dayTimeDuration dtd in
  dtdstand.dtd_seconds

let minutes_from_duration dtd =
  let dtdstand = standardize_dayTimeDuration dtd in
  dtdstand.dtd_minutes

let hours_from_duration dtd =
  let dtdstand = standardize_dayTimeDuration dtd in
  dtdstand.dtd_hours

let days_from_duration dtd =
  let dtdstand = standardize_dayTimeDuration dtd in
  dtdstand.dtd_days

let hours_from_time tm = tm.t_hours

let minutes_from_time tm = tm.t_minutes

let seconds_from_time tm = tm.t_seconds

(** A helper function which is necessary to create a dayTimeDuration
   from a timezone. The dayTimeDuration is the conventional form of a timezone
   but now how a timezone is represented internally in this library.
   @param timezone The timezone
   @return A dayTimeDuration form of the timezone *)
let make_dayTimeDuration_of_timezone timezone =
  match timezone.tz_rel with
  | Positive -> create_dayTimeDuration (0, timezone.tz_hours, timezone.tz_minutes, Decimal._decimal_zero)
  | Negative -> create_dayTimeDuration (0, timezone.tz_hours, timezone.tz_minutes, Decimal._decimal_zero)
  | _ -> create_dayTimeDuration(0, 0, 0, Decimal._decimal_zero)

let is_zero_tz timezone =
  (timezone.tz_hours = 0) &&
  (timezone.tz_minutes = 0)

let make_opt_dayTimeDuration_of_timezone timezone =
  match timezone.tz_rel with
  | Positive ->
      if (is_zero_tz timezone)
      then None
      else
	Some (create_dayTimeDuration (0, timezone.tz_hours, timezone.tz_minutes, Decimal._decimal_zero))
  | Negative ->
      if (is_zero_tz timezone)
      then None
      else
	Some (create_dayTimeDuration (0, timezone.tz_hours, timezone.tz_minutes, Decimal._decimal_zero))
  | UTC ->
      Some (create_dayTimeDuration(0, 0, 0, Decimal._decimal_zero))
  | Unknown -> None

let timezone_from_time tm = make_dayTimeDuration_of_timezone (tm.t_timezone)
let opt_timezone_from_time tm = make_opt_dayTimeDuration_of_timezone (tm.t_timezone)

let year_from_date date = date.d_year

let month_from_date date = date.d_month

let day_from_date date = date.d_day

let timezone_from_date date = make_dayTimeDuration_of_timezone (date.d_timezone)

let opt_timezone_from_date date = make_opt_dayTimeDuration_of_timezone (date.d_timezone)

let year_from_dateTime dt = dt.dt_date.d_year

let month_from_dateTime dt = dt.dt_date.d_month

let day_from_dateTime dt = dt.dt_date.d_day

let hours_from_dateTime dt = dt.dt_time.t_hours

let minutes_from_dateTime dt = dt.dt_time.t_minutes

let seconds_from_dateTime dt = dt.dt_time.t_seconds

let timezone_from_dateTime dt = make_dayTimeDuration_of_timezone (dt.dt_timezone)
let opt_timezone_from_dateTime dt = make_opt_dayTimeDuration_of_timezone (dt.dt_timezone)
    
let date_from_dateTime dt = dt.dt_date

let time_from_dateTime dt = dt.dt_time

(** Given a dayTimeDuration used to represent a timezone, find out if it is 
   a Positive or Negative offset from UTC
   @param dtd The dayTimeDuration
   @return Positive, Negative, or UTC depending on the offset *)
let findOffset dtd =
  if dtd.dtd_days > 0 || dtd.dtd_hours > 0 || dtd.dtd_minutes > 0 ||
    Decimal._decimal_gt dtd.dtd_seconds Decimal._decimal_zero then Positive
  else if dtd.dtd_days < 0 || dtd.dtd_hours < 0 || dtd.dtd_minutes < 0 ||
    Decimal._decimal_lt dtd.dtd_seconds Decimal._decimal_zero then Negative
  else UTC

(** Given a dayTimeDuration, convert it into the internal timezone representation
   @param dtd The dayTimeDuration
   @return The internal timezone representation *)
let make_timezone_from_dayTimeDuration dtd = create_timezone(findOffset dtd, dtd.dtd_hours, dtd.dtd_minutes) 

(** Compare two times. If a time does not have a timezone then the local_timezone is used.
   First normalize the two times, then compare the hours, minutes, seconds.
   @param local_timezone The local_timezone
   @param t1 Time1
   @param t2 Time2
   @return 1 if t1 is greater, -1 if it is less, and 0 if they're equal *)

let time_compare local_timezone t1 t2 =
  let tmp1 =
    match t1.t_timezone.tz_rel with
    | Unknown ->
	normalize_time (mktime (t1.t_hours,t1.t_minutes,t1.t_seconds,local_timezone))
    | _ -> normalize_time t1
  and tmp2 =
    match t2.t_timezone.tz_rel with
    | Unknown ->
	normalize_time (mktime (t2.t_hours,t2.t_minutes,t2.t_seconds,local_timezone))
    |	 _ -> normalize_time t2
  in
  if tmp1.t_hours > tmp2.t_hours then 1
  else if tmp2.t_hours > tmp1.t_hours then -1
  else if tmp1.t_minutes > tmp2.t_minutes then 1
  else if tmp2.t_minutes > tmp1.t_minutes then -1
  else if Decimal._decimal_gt tmp1.t_seconds tmp2.t_seconds then 1
  else if Decimal._decimal_gt tmp2.t_seconds tmp1.t_seconds then -1
  else 0
(** Compare two dateTimes. Use the local timezone if either of the dateTimes are given
   without a timezone. First normalize the dateTimes and then run the date_compare
   and time_compare on them.
   @param local_timezone The local timezone
   @param dt1 The first dateTime
   @param dt2 The second dateTime
   @return 1 if dt1 comes after dt2, -1 if it comes before, and 0 if they are the same *)
let dateTime_compare local_timezone dt1 dt2 =
  let dt1norm = match dt1.dt_timezone.tz_rel with
   | Unknown -> normalize_dateTime (mkdateTime (dt1.dt_date, dt1.dt_time, local_timezone))
   | _ -> normalize_dateTime dt1 and
      dt2norm = match dt2.dt_timezone.tz_rel with
   | Unknown -> normalize_dateTime (mkdateTime (dt2.dt_date, dt2.dt_time, local_timezone))
   | _ -> normalize_dateTime dt2 in
   let date1 = dt1norm.dt_date in
   let date2 = dt2norm.dt_date in
   if date1.d_year > date2.d_year then 1
   else if date2.d_year > date1.d_year then -1
   else if date1.d_month > date2.d_month then 1
   else if date2.d_month > date1.d_month then -1
   else if date1.d_day > date2.d_day then 1
   else if date2.d_day > date1.d_day then -1
   else 
     time_compare local_timezone (dt1norm.dt_time) (dt2norm.dt_time)


(** Compare two dates. The optional timezone is used in case one date is provided
   with a timezone and the other is not. First compare the year, month, and day, and
   then go to the timezone to determine which date is greater.
   @param opt_local_timezone The local timezone option
   @param d1 date1
   @param d2 date2
   @return 1 if d1 is greater, -1 if it is less, 0 if it is the same *)
let date_compare opt_local_timezone d1 d2 =
  let dt1 = create_dateTime_from_date d1 in
  let dt2 = create_dateTime_from_date d2 in
  dateTime_compare opt_local_timezone dt1 dt2

let gYearMonth_compare = date_compare
let gYear_compare      = date_compare
let gMonthDay_compare  = date_compare
let gDay_compare       = date_compare
let gMonth_compare     = date_compare

(** Comparators, arithmetic operations *)

(** Add dayTimeDuratoins by adding the individual components and standardizing 
   @param dtd1 A dayTimeDuration
   @param dtd2 A dayTimeDuration
   @return A new dayTimeDuration consisting of the sum of the two dtds passed *)
let add_dayTimeDurations dtd1 dtd2 =
  standardize_dayTimeDuration (create_dayTimeDuration (dtd1.dtd_days + dtd2.dtd_days, dtd1.dtd_hours + dtd2.dtd_hours, 
						       dtd1.dtd_minutes + dtd2.dtd_minutes, 
						       Decimal._decimal_add (dtd1.dtd_seconds) (dtd2.dtd_seconds)))

(** Subtract dtd2 from dtd1
   @param dtd1 A dayTimeDuration
   @param dtd2 A dayTimeDuration
   @return A new dayTimeDuraiton consisting of the difference between the two dtds *)
let subtract_dayTimeDurations dtd1 dtd2 =
  add_dayTimeDurations dtd1 (negate_dayTimeDuration dtd2)


(** Add a dayTimeDuration to a time. First add it and make a time with no error, then stardardize it
   @param tm The time
   @param dtd The dayTimeDuration
   @return The new time *)
let add_dayTimeDuration_to_time_aux tm dtd =
  let t =
    let odtd =
      match tm.t_timezone.tz_rel with
      | Unknown ->
	  None
      | _ ->
	  Some
	    (create_dayTimeDuration
	       (0,
		tm.t_timezone.tz_hours,
		tm.t_timezone.tz_minutes,
		Decimal._decimal_zero))
    in
    mktime_noerr ((tm.t_hours + dtd.dtd_hours), tm.t_minutes + dtd.dtd_minutes, 
		  (Decimal._decimal_add tm.t_seconds dtd.dtd_seconds), 
		  odtd)
  in
  standardize_time t

let add_dayTimeDuration_to_time tm dtd =
  let t = add_dayTimeDuration_to_time_aux tm dtd in
  standardize_time_trunc t

(** Add a dayTimeDuraiton to a dateTime. This is one of the trickiest
   functions to implement, and I'm still not convinced that it works
   100% correctly. A dayTimeDuration can contain any number of days,
   so it is likely to effect more than just the days portion of the
   date.  Also, if the time portion of the duration causes a spillover
   greater than 24 hours then the date portion of the dateTime also
   needs to be updated.

   @param dt The dateTime
   @param dtd The dayTimeDuration
   @return The new dateTime after adding the dayTimeDuration to the previous dateTime *)
let add_dayTimeDuration_to_dateTime dt dtd =
  let _ = standardize_dayTimeDuration dtd in
  let tmpdate = add_dayDuration_to_date dt.dt_date dtd in
  let tmptime = add_dayTimeDuration_to_time_aux dt.dt_time dtd in
  standardize_dateTime (create_dateTime (tmpdate,tmptime,dt.dt_timezone))

(** Add a dayTimeDuration to a date
   @param dt The Date
   @param dtd The dayTimeDuration
   @return The new date after having the dayTimeDuration added on *)
let add_dayTimeDuration_to_date dt dtd =
  let realdt = create_dateTime_from_date dt in
  let newdt = add_dayTimeDuration_to_dateTime realdt dtd in
  newdt.dt_date


(** Subtract two times. First normalize them, then do the subtraction, and create a dayTimeDuration.
   Normalize it, then stardardize it.
   @param tm1 time1
   @param tm2 time2
   @return The dayTimeDuration containing the difference between
   tm2 and tm1 *)
let subtract_times tm1 tm2 =
  let tm1norm = normalize_time tm1 and
      tm2norm = normalize_time tm2 in
  let hours = tm1norm.t_hours - tm2norm.t_hours and
      minutes = tm1norm.t_minutes - tm2norm.t_minutes and
      seconds = Decimal._decimal_sub tm1.t_seconds tm2.t_seconds in
  standardize_dayTimeDuration (create_dayTimeDuration (0, hours, minutes, seconds))



(** Find the local timezone using unix system calls. Find the local time of the 
   day, then find the utc time of the day, and create dateTime times. Then
   do the subtraction to get the local timezone offset. It is important to
   check that UTC and local are on the same day, or else there will
   be problems with the subtraction. Adjust accordingly.
   @return The local timezone *)
let local_timezone() = 
  let timeofday = Unix.gettimeofday() in
  let utc_time = Unix.gmtime timeofday in
  let local_time = Unix.localtime timeofday in
  let daylightsavings = if local_time.tm_isdst then (-1) else 0 in
  (* Check if the days differ : can never differ more than one! *)
  let datelocal = mkdate (Some(local_time.tm_year + 1900), local_time.tm_mon + 1, local_time.tm_mday, None) in
  let dateutc = mkdate (Some(utc_time.tm_year + 1900), utc_time.tm_mon + 1, utc_time.tm_mday, None) in
  let (timeutc, timelocal) = 
    match date_compare None dateutc datelocal with
    | 1 -> 
(* local 23 - utc (4+24) -5 *)
	(create_time (utc_time.tm_hour + 24, utc_time.tm_min, Decimal._decimal_zero, UTC, 0, 0), 
	 create_time (local_time.tm_hour + daylightsavings, local_time.tm_min, Decimal._decimal_zero, UTC, 0, 0))
    | -1 ->
	(create_time (utc_time.tm_hour, utc_time.tm_min, Decimal._decimal_zero, UTC, 0, 0), 
	 create_time (local_time.tm_hour + daylightsavings + 24, local_time.tm_min, Decimal._decimal_zero, UTC, 0, 0))
(* local (4+24) - utc 23 +5 *)
    | 0 ->   
	(create_time (utc_time.tm_hour, utc_time.tm_min, Decimal._decimal_zero, UTC, 0, 0), 
	 create_time (local_time.tm_hour + daylightsavings, local_time.tm_min, Decimal._decimal_zero, UTC, 0, 0))
(* local 4 - utc 23 *)
    | _ -> raise (Query (Protocol_Error("Compare can not return anything other than 0, 1, -1")))
  in subtract_times timelocal timeutc 

(** Find the number of days between date1 and date2. In this recursive implementation
   it adds one to the return value every time that it increments the day in date2 until
   the two days are equal.
   @param date1 The greater of the two dates
   @param date2 The lesser of the two dates
   @return The number of days between the two dates *)
let totaldays date1 date2 =
  let y1 = date1.d_year in
  let y2 = ref (date2.d_year) in
  let m1 = date1.d_month in
  let m2 = ref (date2.d_month) in
  let d1 = date1.d_day in
  let d2 = ref (date2.d_day) in
  let days = ref 0 in
  while not((y1 = !y2) && (m1 = !m2) && (d1 = !d2)) do
    let newdate = create_date (!y2, !m2, !d2, Unknown, 0, 0) in
    let (tmpyear, tmpmonth, tmpday) = increment_month_given_date newdate Positive in
    y2 := tmpyear;
    m2 := tmpmonth;
    d2 := tmpday;
    incr days
  done;
  !days

(** Subtract two dateTimes. First normalize them, then do the
    subtraction, and create a dayTimeDuration.  Normalize it, then
    stardardize it.
   @param tm1 dateTime1
   @param tm2 dateTime2
   @return The dayTimeDuration containing the difference between
   tm2 and tm1 *)
let subtract_dateTimes dtm1 dtm2 =
  let dtm1norm = normalize_dateTime dtm1 in
  let dtm2norm = normalize_dateTime dtm2 in
  let tm1norm = dtm1norm.dt_time in
  let tm2norm = dtm2norm.dt_time in
  let dt1 = dtm1norm.dt_date in
  let dt2 = dtm2norm.dt_date in
  let days =
    match (date_compare (Some (local_timezone())) dt1 dt2) with
    | 1 -> totaldays dt1 dt2
    | -1 -> -1 * (totaldays dt2 dt1)
    | 0 -> 0 
    | _ -> raise (Query (Protocol_Error("Compare can not return anything other than 0, 1, -1")))
  in
  let hours = tm1norm.t_hours - tm2norm.t_hours and
      minutes = tm1norm.t_minutes - tm2norm.t_minutes and
      seconds = Decimal._decimal_sub tm1norm.t_seconds tm2norm.t_seconds in
  standardize_dayTimeDuration (create_dayTimeDuration (days, hours, minutes, seconds))

(** Subtract two times. First normalize them, then do the subtraction, and create a dayTimeDuration.
   Normalize it, then stardardize it.
   @param d1 date1
   @param d2 date2
   @return The dayTimeDuration containing the difference between
   tm2 and tm1 *)
let subtract_dates d1 d2 =
  let dt1 = create_dateTime_from_date d1 in
  let dt2 = create_dateTime_from_date d2 in
  subtract_dateTimes dt1 dt2

(** Subtract a dayTimeDuration from a time
   @param tm a time
   @param dtd a dayTimeDuration
   @return The new time resulting from the subtraction *)
let subtract_dayTimeDuration_from_time tm dtd =
  add_dayTimeDuration_to_time tm (negate_dayTimeDuration dtd)


(** A UTC timezone
   @return A dayTimeDuration representing UTC *)
let default_UTC() = 
  create_dayTimeDuration(0,0,0, Decimal._decimal_zero)

(** Gets the current dateTime by making Unix system calls
   @return The current dateTime *)
let current_dateTime() = 
  let timeofday = Unix.gettimeofday() in 
  let local_time = Unix.localtime timeofday in 
  let tz = local_timezone() in 
  let tmpdate =
    mkdate (Some(local_time.tm_year + 1900),
	    local_time.tm_mon + 1,
	    local_time.tm_mday, Some tz)
  and tmptime =
    mktime (local_time.tm_hour,
	    local_time.tm_min, 
	    Decimal._decimal_of_int local_time.tm_sec,
	    Some tz)
  in
  mkdateTime(tmpdate, tmptime, Some tz)

(** Set the default date to January 1st, 1970 at midnight to correspond with Unix system calls 
   @return 1970-01-01T00:00:00Z *)
let default_dateTime() = 
  let dt = create_date(1970, 1, 1, UTC, 0, 0) and
      tm = create_time(0,0, Decimal._decimal_zero, UTC, 0, 0) in
  create_dateTime(dt, tm, create_timezone(UTC, 0, 0))
  
(** Find out if the yearMonthDuration is positive or negative
   @param ymd The yearMonthDuration
   @return Positive or Negative depending on the yearMonthDuration *)
let ymd_direction ymd =
  if ymd.ymd_years > 0 || ymd.ymd_months > 0 then Positive
  else Negative

(** Add the specified number of months to a date consisting of a year and a month.
   First find the direction of the months, then add or subtract as necessary, and adjust
   when the year needs to change.
   @param year The year
   @param month The month
   @param months The number of months left to increment
   @param direction Positive or Negative
   @return A tuple consisting of the new year and the new month *)
let rec increment_date_from_months year month months direction =
  match months with
  | 0 -> (year, month)
  | _ ->
      (match direction with
      |	Positive ->
	  (match (year, month) with
	    (* There is no year zero. if year is -1 then go to year 1 *)
	  | (-1, 12) -> increment_date_from_months 1 1 (months - 1) Positive
	  | (yr, 12) -> increment_date_from_months (yr + 1) 1 (months - 1) Positive
	  | (yr, mnth) -> increment_date_from_months yr (mnth + 1) (months - 1) Positive )
      |	Negative ->
	  (match (year, month) with
	    (* There is no year zero. if year is 1 then go to year -1 *)
	  | (1, 1) -> increment_date_from_months (-1) 12 (months + 1) Negative
	  | (yr, 1) -> increment_date_from_months (yr - 1) 12 (months + 1) Negative
	  | (yr, mnth) -> increment_date_from_months yr (mnth - 1) (months + 1) Negative)
      | _ -> raise (Query (Protocol_Error("Direction by default is either positive or negative"))))
  
(** Add a yearMonthDuration to a date
   @param dt The date
   @param ymd The duration
   @return The new date*) 
let add_yearMonthDuration_to_date dt ymd =
  let dir = ymd_direction ymd in
  let months = (normalize_yearMonthDuration ymd).ymd_months in
  let (newyear, newmonths) = increment_date_from_months (dt.d_year) (dt.d_month) months dir in
  let newdays =
    let max = maximumDayInMonthFor(newyear, newmonths) in
    let cd = dt.d_day in
    if (cd > max)
    then max
    else cd
  in
  create_date(newyear, newmonths, newdays, dt.d_timezone.tz_rel, dt.d_timezone.tz_hours, dt.d_timezone.tz_minutes)


(** Subtract a dayTimeDuration from a date
   @param dt The Date
   @param dtd The dayTimeDuration
   @return The new date after the subtraction *)
let subtract_dayTimeDuration_from_date dt dtd =
  add_dayTimeDuration_to_date dt (negate_dayTimeDuration dtd)

(** Subtract a yearMonthDuration from a date
   @param dt A date
   @param ymd A yearMonthDuration
   @return A date with the yearMonthDuration added on *)
let subtract_yearMonthDuration_from_date dt ymd =
  add_yearMonthDuration_to_date dt (negate_yearMonthDuration ymd)


(** Subtract two dates from one another yielding a
   dayTimeDuration. First find which date comes later and then run the
   two dates through totaldays.
   @param date1 The first date
   @param date2 The second date
   @return The difference in days between the two dates *)

(** Add a yearMonthDuration to a dateTime
   @param dt The dateTime
   @param ymd The yearMonthDuration
   @return The new dateTime resulting from adding the ymd to the dt *)
let add_yearMonthDuration_to_dateTime dt ymd =
  let newdate = add_yearMonthDuration_to_date dt.dt_date ymd in
  create_dateTime (newdate, dt.dt_time, create_timezone(dt.dt_timezone.tz_rel, dt.dt_timezone.tz_hours, dt.dt_timezone.tz_minutes))
let add_yearMonthDuration_to_dateTime2 ymd dt =
  add_yearMonthDuration_to_dateTime dt ymd

(** Subtract dateTimes yielding a dayTimeDuration using totaldays on
    the dates
   @param dt1 The first dateTime
   @param dt2 The second dateTime
   @return The dayTimeDuration between the two dateTimes *)
let subtract_dateTimes_yielding_dayTimeDuration dt1 dt2 =
  let days = match (date_compare (Some (local_timezone())) dt1.dt_date dt2.dt_date) with
  | 1 -> totaldays dt1.dt_date dt2.dt_date
  | -1 -> -1 * (totaldays dt2.dt_date dt1.dt_date)
  | 0 -> 0 
  | _ -> raise (Query (Protocol_Error("Compare can not return anything other than 0, 1, -1"))) and
      dt1norm = normalize_dateTime dt1 and
      dt2norm = normalize_dateTime dt2 in
  let hours = dt1norm.dt_time.t_hours - dt2norm.dt_time.t_hours and
      minutes = dt1norm.dt_time.t_minutes - dt2norm.dt_time.t_minutes and
      seconds = Decimal._decimal_sub dt1norm.dt_time.t_seconds dt2norm.dt_time.t_seconds in
  create_dayTimeDuration (days, hours, minutes, seconds)

(** Subtract yearMonthDuration from a dateTime
   @param dt The dateTime
   @param ymd The yearMonthDuration
   @return The new dateTime resulting from the subtraction *)
let subtract_yearMonthDuration_from_dateTime dt ymd =
  add_yearMonthDuration_to_dateTime dt (negate_yearMonthDuration ymd)

(** Subtract a dayTimeDuration from a dateTime
   @param dt The DateTime
   @param dtd the dayTimeDuration
   @return The new dateTime yielding from the subtraction *)
let subtract_dayTimeDuration_from_dateTime dt dtd =
  add_dayTimeDuration_to_dateTime dt (negate_dayTimeDuration dtd)

let get_compatible_timezone d t =
  let tz1 = d.d_timezone in
  let tz2 = t.t_timezone in
  match tz1.tz_rel,tz2.tz_rel with
  | Unknown,_ -> tz2
  | _,Unknown -> tz1
  | _ ->
      if (tz1.tz_rel = tz2.tz_rel) && (tz1.tz_hours = tz2.tz_hours) && (tz1.tz_minutes = tz2.tz_minutes)
      then
	tz1
      else
	raise (Query (Parameter_Mismatch "[err:FORG0008] date and time have different timezones in fn:dateTime"))

let dateTime_from_date_and_time d t =
  let tz = get_compatible_timezone d t in
  create_dateTime (d,t,tz)


(** Compare two yearMonthDurations to see which is larger
   @param ymd1 The first yearMonthDuration
   @param ymd2 The second yearMonthDuration
   @return 1 if the first is larger, -1 if it is smaller, 0 otherwise *)
let yearMonthDuration_compare ymd1 ymd2 =
  let ymd1norm = normalize_yearMonthDuration ymd1 and
      ymd2norm = normalize_yearMonthDuration ymd2 in
  if ymd1norm.ymd_months > ymd2norm.ymd_months then 1
  else if ymd2norm.ymd_months > ymd1norm.ymd_months then -1
  else 0

(** Add two yearMonthDurations
   @param ymd1 The first yearMonthDuration
   @param ymd2 The second yearMonthDuration
   @return The yearMonthDuration created by adding the two together *)
let add_yearMonthDurations ymd1 ymd2 =
  standardize_yearMonthDuration (create_yearMonthDuration (ymd1.ymd_years + ymd2.ymd_years, ymd1.ymd_months + ymd2.ymd_months))

(** Subtract two yearMonthDurations
   @param ymd1 The first yearMonthDuration
   @param ymd2 The second yearMonthDuration
   @return The yearMonthDuration obtained by subtracting ymd2 from ymd1 *)
let subtract_yearMonthDurations ymd1 ymd2 =
  add_yearMonthDurations ymd1 (negate_yearMonthDuration ymd2)

(** Multiply a yearMonthDuration by a float
   @param ymd The yearMonthDuration
   @param mult the floating point number
   @return A new yearMonthDuration with months rounded to the nearest month obtained by
   multiplying the number of months by the float. *)
let multiply_yearMonthDuration ymd mult =
  if Decimal.is_nan mult then raise (Query (Parameter_Mismatch ("NaN argument in xs:monthYearDuraction multiplication")));
  if (mult = infinity) || (mult = neg_infinity) then raise (Query (Parameter_Mismatch ("Overflow in xs:monthYearDuraction multiplication")));
  let ymdnorm = normalize_yearMonthDuration ymd in
  let newmonths = (float_of_int(ymdnorm.ymd_months)) *. mult in
  let tmpmonths = int_of_float (floor (newmonths +. 0.5)) in
(* let tmpmonths = if (ceil newmonths) -. newmonths > 0.5 then (int_of_float newmonths) else (int_of_float (ceil newmonths)) in *)
  mkyearMonthDuration ((tmpmonths / 12), (tmpmonths mod 12))
let multiply_yearMonthDuration2 mult ymd =
  multiply_yearMonthDuration ymd mult

(** Divide a yearMonthDuration by a float
   @param ymd The yearMonthDuration
   @param div The divisor
   @return A new yearMonthDuration with months rounded to the nearest month obtained by
   dividing the number of months by the float. *)
let divide_yearMonthDuration ymd div =
  multiply_yearMonthDuration ymd (1. /. div)

(** Divide two yearMonthDurations
   @param ymd1 The first yearMonthDuraiton
   @param ymd2 The second yearMonthDuration
   @return A decimal value attained from the division *)
let divide_yearMonthDuration_by_yearMonthDuration ymd1 ymd2 =
  let ymd1normmonths = Decimal._integer_of_int ((normalize_yearMonthDuration ymd1).ymd_months) and
      ymd2normmonths = Decimal._integer_of_int ((normalize_yearMonthDuration ymd2).ymd_months) in
  Decimal._integer_div ymd1normmonths ymd2normmonths

(** Compare two dayTimeDurations
   @param dtd1 The first dayTimeDuration
   @param dtd2 The second dayTimeDuration
   @return 1 if the first dtd is greater than the second, -1 if it is less, and 0 if they are the same *)
let dayTimeDuration_compare dtd1 dtd2 =
  let dtd1norm = normalize_dayTimeDuration dtd1 and
      dtd2norm = normalize_dayTimeDuration dtd2 in
  if Decimal._decimal_gt dtd1norm dtd2norm then 1
  else if Decimal._decimal_lt dtd1norm dtd2norm then -1
  else 0
 
(** Multiply a dayTimeDuration by a float.
   First normalize the dtd, then do the multiply.
   @param dtd The dayTimeDuration
   @param mult The floating point number
   @return the standardized dayTimeDuration resultant from multiplying the seconds by the float *)
let multiply_dayTimeDuration dtd mult =
  let dtdnorm = normalize_dayTimeDuration dtd in
  let newsecs = (Decimal._float_of_decimal dtdnorm) *. mult in
  standardize_dayTimeDuration (mkdayTimeDuration (0, 0, 0, Decimal._small_decimal_of_float newsecs))
let multiply_dayTimeDuration2 mult dtd =
  multiply_dayTimeDuration dtd mult

(** Divide a dayTimeDuration by a float
   @param dtd The dayTimeDuration
   @param div The float to divide by
   @return The standardize dayTimeDuraiton resultant from dividing the seconds by the float *)
let divide_dayTimeDuration dtd div =
  multiply_dayTimeDuration dtd (1. /. div)

(** Divide a dayTimeDuration by a dayTimeDuration
   @param dtd1 The first dayTimeDuration
   @param dtd2 The second dayTimeDuration
   @return The result of the division between the two dayTimeDurations *)
let divide_dayTimeDuration_by_dayTimeDuration dtd1 dtd2 =
  let dtd1secs = normalize_dayTimeDuration dtd1
  and dtd2secs = normalize_dayTimeDuration dtd2 in
  let newsecs = Decimal._decimal_div dtd1secs dtd2secs in
  newsecs

let duration_compare dur1 dur2 =
  if ((yearMonthDuration_compare dur1.dur_ymd dur2.dur_ymd) = 0)
      && ((dayTimeDuration_compare dur1.dur_dtd dur2.dur_dtd) = 0)
  then if (dur1.dur_sign = dur2.dur_sign)
  then 0
  else
    if ((yearMonthDuration_compare dur1.dur_ymd zero_yearMonthDuration) = 0)
	&& ((dayTimeDuration_compare dur1.dur_dtd zero_dayTimeDuration) = 0)
    then 0
    else 1
  else 1

(** Attach a given timezone to a given time, whether or not it already has a timezone.
   If the time does not have a timezone then just append the given timezone. If
   the time does have a timezone, then adjust it to the given one.

   @param time The time
   @param tz The timezone
   @return A new time with the new timezone attached and the time adjusted 
*)
let adjust_time_to_timezone time tz =
  match tz with
  | None ->
      if (time.t_timezone.tz_rel = Unknown)
      then
	time
      else
	mktime(time.t_hours, time.t_minutes, time.t_seconds, None)
  | Some tz ->
      if (time.t_timezone.tz_rel = Unknown)
      then
	mktime(time.t_hours, time.t_minutes, time.t_seconds, Some tz) 
      else
	let time_tz = timezone_from_time time in
	let diff = subtract_dayTimeDurations tz time_tz in
	let newtime = add_dayTimeDuration_to_time time diff in
	mktime(newtime.t_hours, newtime.t_minutes, newtime.t_seconds, Some tz) 

(** Attach a given timezone to a given dateTime, whether or not it already has a timezone.

   If the dateTime does not have a timezone then just append the given timezone. If
   the dateTime does have a timezone, then adjust it to the given one.

   @param dateTime The dateTime
   @param tz The timezone
   @return A new dateTime with the new timezone attached and the dateTime adjusted *)
let adjust_dateTime_to_timezone dateTime tz =
  match tz with
  | None ->
      if(dateTime.dt_timezone.tz_rel = Unknown)
      then
	dateTime
      else
	mkdateTime(dateTime.dt_date, dateTime.dt_time, None)
  | Some tz ->
      if(dateTime.dt_timezone.tz_rel = Unknown)
      then
	mkdateTime(dateTime.dt_date, dateTime.dt_time, Some tz)
      else
	let dateTime_tz = timezone_from_dateTime dateTime in
	let diff = subtract_dayTimeDurations tz dateTime_tz in
	let newdateTime = add_dayTimeDuration_to_dateTime dateTime diff in
	mkdateTime(newdateTime.dt_date, newdateTime.dt_time, Some tz)

(** Attach a given timezone to a given date, whether or not it already has a timezone.
   If the date does not have a timezone then just append the given timezone. If
   the date does have a timezone, then adjust it to the given one.

   @param date The date
   @param tz The timezone
   @return A new date with the new timezone attached and the date adjusted *)
let adjust_date_to_timezone date tz =
  let time0 =
    create_time_with_timezone (0, 0, Decimal._decimal_zero, date.d_timezone)
  in
  let dt = create_dateTime (date, time0, date.d_timezone) in
  let newdt = adjust_dateTime_to_timezone dt tz in
  create_date_with_timezone (newdt.dt_date.d_year, newdt.dt_date.d_month, newdt.dt_date.d_day, newdt.dt_timezone)

let yearMonthDuration_of_duration dur =
  if dur.dur_sign
  then dur.dur_ymd
  else negate_yearMonthDuration dur.dur_ymd
    
let dayTimeDuration_of_duration dur =
  if dur.dur_sign
  then dur.dur_dtd
  else negate_dayTimeDuration dur.dur_dtd

let duration_of_yearMonthDuration ymd =
  let (sign,ymd) =
    let (x,y) = (ymd.ymd_years,ymd.ymd_months) in
    if ((value_of_yearMonthDuration (x,y)) >= 0)
    then (true,ymd)
    else (false,negate_yearMonthDuration ymd)
  in
  mkduration sign (ymd,zero_dayTimeDuration)

let duration_of_dayTimeDuration dtd =
  let (sign,dtd) =
    if (Decimal._decimal_ge (value_of_dayTimeDuration (dtd.dtd_days,dtd.dtd_hours,dtd.dtd_minutes,dtd.dtd_seconds)) Decimal._decimal_zero)
    then (true,dtd)
    else (false,negate_dayTimeDuration dtd)
  in
  mkduration sign (zero_yearMonthDuration,dtd)
