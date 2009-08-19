(***********************************************************************)
(*                                                                     *)
(*                                 GALAX                               *)
(*                              XQuery Engine                          *)
(*                                                                     *)
(*  Copyright 2001-2007.                                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: dateTime.mli,v 1.24 2007/02/01 22:08:46 simeon Exp $ *)

(* Module DateTime
   Description:
   This modules implement internal support for the XML Schema
   primitive simple types related to date and time.
*)

(* types *)

(** Relative offset of the timezone to UTC *)
type _TZrel

(** Internal Representation of a timezone *)
type _timezone

(** xs:time datatype *)
type xs_time

(** xs:date datatypes *)
type xs_date

(** xs:dateTime datatype consisting of a date value and a time value *)
type xs_dateTime

(** xs:duration datatype consisting of a year,month,day,hour,minute, and second *)
type xs_duration

(** xs:gYearMonth datatype consisting of a year value and a month value *)
type xs_gYearMonth

(** xs:gYear datatype consisting of a year value *)
type xs_gYear

(** xs:gMonthDay datatype consisting of a month value and a day value *)
type xs_gMonthDay

(** xs:gDay datatype consisting of a day value *)
type xs_gDay

(** xs:gMonth datatype consisting of a month value *)
type xs_gMonth

(** xs:yearMonthDuration datatype *)
type xs_yearMonthDuration

(** xs:dayTimeDuration datatype *)
type xs_dayTimeDuration

(** string_of_ functions *)

(** Gives the string value of an xs_date
   @param date The date to be converted to a string 
   @return the string representation of a date *)
val string_of_date : xs_date -> string

val string_of_duration   : xs_duration   -> string
val string_of_gYearMonth : xs_gYearMonth -> string
val string_of_gYear      : xs_gYear      -> string
val string_of_gMonthDay  : xs_gMonthDay  -> string
val string_of_gDay       : xs_gDay       -> string
val string_of_gMonth     : xs_gMonth     -> string

(** Gives the string value of an xs_time 
   @return the string representation of a time *)
val string_of_time : xs_time -> string

(** Gives the string value of an xs_dateTime
   @return the string representation of a dateTime *)
val string_of_dateTime : xs_dateTime -> string

(** Gives the string representation of a yearMonthDuration 
   @return the string representation of a yearMonthDuration *)
val string_of_yearMonthDuration : xs_yearMonthDuration -> string
val canonical_of_yearMonthDuration : xs_yearMonthDuration -> string

(** Gives the string representation of a dayTimeDuration 
   @return the string representation of a dayTimeDuration *)
val string_of_dayTimeDuration : xs_dayTimeDuration -> string
val canonical_of_dayTimeDuration : xs_dayTimeDuration -> string
 
(** Constructors for the date/time datatypes *)

(** Takes a year, month, day, and timezone as a duration and returns an xs_date datatype.
    @return an xs_date *)
val mkdate : (int option * int * int * xs_dayTimeDuration option) -> xs_date

(** Takes an hour, minute, second and timezone as a duration and returns an xs_time datatype.
   @return an xs_time *)
val mktime : (int * int * Decimal._decimal * xs_dayTimeDuration option) -> xs_time
(** Takes an hour, minute, second and timezone as a duration and returns an xs_time datatype.
   @return an xs_time and dayTimeDuration, if time rolls over into a day *)
val mktime_dtd : (int * int * Decimal._decimal * xs_dayTimeDuration option) -> (xs_dayTimeDuration * xs_time)

(** Takes a date, a time, and a timezone as a duration and returns an
    xs_dateTime @return an xs_dateTime *)
val mkdateTime   : (xs_date * xs_time * xs_dayTimeDuration option) -> xs_dateTime
val mkgYearMonth : (int * int * xs_dayTimeDuration option) -> xs_gYearMonth
val mkgYear      : (int * xs_dayTimeDuration option) -> xs_gYear
val mkgMonthDay  : (int * int * xs_dayTimeDuration option) -> xs_gMonthDay
val mkgDay       : (int * xs_dayTimeDuration option) -> xs_gDay
val mkgMonth     : (int * xs_dayTimeDuration option) -> xs_gMonth

(** Takes a number of years and a number of months and returns a
    yearMonthDuration @return an xs_yearMonthDuration *)
val mkyearMonthDuration : (int * int) -> xs_yearMonthDuration
val zero_yearMonthDuration : xs_yearMonthDuration

(** Takes a number of days, hours, minutes, and seconds and returns a
    dayTimeDuration @return an xs_dayTimeDuration *)
val mkdayTimeDuration : (int * int * int * Decimal._decimal) -> xs_dayTimeDuration
val zero_dayTimeDuration : xs_dayTimeDuration

val mkduration :
    bool -> xs_yearMonthDuration * xs_dayTimeDuration -> xs_duration

(** Operations
    Comparisons and Arithmetic operations *)

(** Compares two dates. The dayTimeDuration option is the timezone
    used if one of the dates does not contain a timezone.  @return -1
    if the first is earlier than the second, 1 if it is later, and 0
    if it is the same *)
val date_compare       : xs_dayTimeDuration option -> xs_date -> xs_date -> int 
val duration_compare   : xs_duration -> xs_duration -> int 
val gYearMonth_compare : xs_dayTimeDuration option -> xs_gYearMonth -> xs_gYearMonth -> int 
val gYear_compare      : xs_dayTimeDuration option -> xs_gYear -> xs_gYear -> int 
val gMonthDay_compare  : xs_dayTimeDuration option -> xs_gMonthDay -> xs_gMonthDay -> int 
val gDay_compare       : xs_dayTimeDuration option -> xs_gDay -> xs_gDay -> int
val gMonth_compare     : xs_dayTimeDuration option -> xs_gMonth -> xs_gMonth -> int 

(** Add a yearMonthDuration to a date yielding the new date
   @return The given duration added to the date *)
val add_yearMonthDuration_to_date :  xs_date -> xs_yearMonthDuration -> xs_date

(** @return The date resulting from adding the given duration to the date given *)
val add_dayTimeDuration_to_date : xs_date -> xs_dayTimeDuration -> xs_date

(** @return The yearMonthDuration representing the difference
   in time between the first date and the second date *)
val subtract_dates : xs_date -> xs_date -> xs_dayTimeDuration

(** @return The date resulting from subtracting the given yearMonthDuration from the given date *)
val subtract_yearMonthDuration_from_date : xs_date -> xs_yearMonthDuration -> xs_date

(** @return The date resulting from subtracting the given dayTimeDuration from the given date *)
val subtract_dayTimeDuration_from_date : xs_date -> xs_dayTimeDuration -> xs_date

(** @return 1 if the first time is later than the second, -1 if it is earlier, and 0 if they are the same
   The dayTimeDuration option is the timezone used if one of the times has a timezone and one
   does not *)
val time_compare : xs_dayTimeDuration option -> xs_time -> xs_time -> int

(** @return The time resulting from adding the given duration to the given time *)
val add_dayTimeDuration_to_time : xs_time -> xs_dayTimeDuration -> xs_time

(** @return The duration of time between the first time and the second time *)
val subtract_dateTimes : xs_dateTime -> xs_dateTime -> xs_dayTimeDuration

(** @return The duration of time between the first time and the second time *)
val subtract_dates : xs_date -> xs_date -> xs_dayTimeDuration

(** @return The duration of time between the first time and the second time *)
val subtract_times : xs_time -> xs_time -> xs_dayTimeDuration

(** @return The time resulting from subtracting the dayTimeDuration from the given time *)
val subtract_dayTimeDuration_from_time : xs_time -> xs_dayTimeDuration -> xs_time

(** @return 1 if the first dateTime occurs after the second, -1 if it occurs before and 0 otherwise.
   The dayTimeDuration option is the timezone used if one of the dateTimes is given
   with a timezone and the other is not *)
val dateTime_compare : xs_dayTimeDuration option -> xs_dateTime -> xs_dateTime -> int

(** @return The dateTime resulting from adding the given duration to the given dateTime *)
val add_yearMonthDuration_to_dateTime : xs_dateTime -> xs_yearMonthDuration -> xs_dateTime
val add_yearMonthDuration_to_dateTime2 : xs_yearMonthDuration -> xs_dateTime -> xs_dateTime

(** @return The dateTime resulting from adding the given duration to the given dateTime *)
val add_dayTimeDuration_to_dateTime : xs_dateTime -> xs_dayTimeDuration -> xs_dateTime

(** @return The duration of time in days, hours, minutes... between the first dateTime and the second dateTime *)
val subtract_dateTimes : xs_dateTime -> xs_dateTime -> xs_dayTimeDuration

(** @return The dateTime resulting from subtracting the given duration from the given dateTime *)
val subtract_yearMonthDuration_from_dateTime : xs_dateTime -> xs_yearMonthDuration -> xs_dateTime

(** @return The dateTime resulting from subtracting the given duration from the given dateTime *)
val subtract_dayTimeDuration_from_dateTime : xs_dateTime -> xs_dayTimeDuration -> xs_dateTime

val dateTime_from_date_and_time : xs_date -> xs_time -> xs_dateTime

(** @return 1 if the first duration is longer than the second, -1 if it is less, 0 if they are the same *)
val yearMonthDuration_compare : xs_yearMonthDuration -> xs_yearMonthDuration -> int

(** @return The duration of time resultant from adding the first duration to the second *)
val add_yearMonthDurations : xs_yearMonthDuration -> xs_yearMonthDuration -> xs_yearMonthDuration

(** @return The duration of time resultant from subtracting the second duration from the first *)
val subtract_yearMonthDurations : xs_yearMonthDuration -> xs_yearMonthDuration -> xs_yearMonthDuration

(** @return The resultant duration from multiplying the given duration by the given floating point number *)
val multiply_yearMonthDuration : xs_yearMonthDuration -> float -> xs_yearMonthDuration
val multiply_yearMonthDuration2 : float -> xs_yearMonthDuration -> xs_yearMonthDuration

(** @return The resultant duration from dividing the given duration by the given floating point number *)
val divide_yearMonthDuration : xs_yearMonthDuration -> float -> xs_yearMonthDuration

(** @return The decimal representing the results of dividing one yearMonthDuration by a second
   yearMonthDuration *)
val divide_yearMonthDuration_by_yearMonthDuration : xs_yearMonthDuration -> xs_yearMonthDuration -> Decimal._decimal

(** @return 1 if the first duration is longer than the second, -1 if it is less, and 0 if they are the same *)
val dayTimeDuration_compare : xs_dayTimeDuration -> xs_dayTimeDuration -> int

(** @return The duration of time resultant from adding the first duration to the second *)
val add_dayTimeDurations : xs_dayTimeDuration -> xs_dayTimeDuration -> xs_dayTimeDuration

(** @return The duration of time resultant from subtracting the second duration from the first *)
val subtract_dayTimeDurations : xs_dayTimeDuration -> xs_dayTimeDuration -> xs_dayTimeDuration

(** @return The duration of time resultant from multiplying the given duration by the given floating point number *)
val multiply_dayTimeDuration : xs_dayTimeDuration -> float -> xs_dayTimeDuration
val multiply_dayTimeDuration2 : float -> xs_dayTimeDuration -> xs_dayTimeDuration

(** @return The duration of time resultant from dividing the given duration by the given floating point number *)
val divide_dayTimeDuration : xs_dayTimeDuration -> float -> xs_dayTimeDuration

(** @return The decimal resulting from the division of two dayTimeDurations *)
val divide_dayTimeDuration_by_dayTimeDuration : xs_dayTimeDuration -> xs_dayTimeDuration -> Decimal._decimal

(** Extraction functions on dates, times, and durations *)

(** @return The years in the duration *)
val years_from_duration : xs_yearMonthDuration -> int

(** @return The months in the duration *)
val months_from_duration : xs_yearMonthDuration -> int

(** @return The days in the duration *)
val days_from_duration : xs_dayTimeDuration -> int

(** @return The hours in the duration *)
val hours_from_duration : xs_dayTimeDuration -> int

(** @return The minutes in the duration *)
val minutes_from_duration : xs_dayTimeDuration -> int

(** @return The seconds in the duration *)
val seconds_from_duration : xs_dayTimeDuration -> Decimal._decimal

(** @return The hours in the time *)
val hours_from_time : xs_time -> int

(** @return The minutes in the time *)
val minutes_from_time : xs_time -> int

(** @return The seconds in the time *)
val seconds_from_time : xs_time -> Decimal._decimal

(** @return The timezone in the time *)
val timezone_from_time : xs_time -> xs_dayTimeDuration
val opt_timezone_from_time : xs_time -> xs_dayTimeDuration option

(** @return The year in the date *)
val year_from_date : xs_date -> int

(** @return The month in the date *)
val month_from_date : xs_date -> int

(** @return The day in the date *)
val day_from_date : xs_date -> int

(** @return The timezone in the date *)
val timezone_from_date : xs_date -> xs_dayTimeDuration
val opt_timezone_from_date : xs_date -> xs_dayTimeDuration option

(** @return The year in the dateTime *)
val year_from_dateTime : xs_dateTime -> int

(** @return The month in the dateTime *)
val month_from_dateTime : xs_dateTime -> int

(** @return The day in the dateTime *)
val day_from_dateTime : xs_dateTime -> int

(** @return The hours in the dateTime *)
val hours_from_dateTime : xs_dateTime -> int

(** @return The minutes in the dateTime *)
val minutes_from_dateTime : xs_dateTime -> int

(** @return The seconds in the dateTime *)
val seconds_from_dateTime : xs_dateTime -> Decimal._decimal

(** @return The timezone in the dateTime *)
val timezone_from_dateTime : xs_dateTime -> xs_dayTimeDuration
val opt_timezone_from_dateTime : xs_dateTime -> xs_dayTimeDuration option

(** @return The date in the dateTime *)
val date_from_dateTime : xs_dateTime -> xs_date

(** @return The time in the dateTime *)
val time_from_dateTime : xs_dateTime -> xs_time

(** Context functions *)

(** Gives the current dateTime based upon the Unix system time set by the user for the operating system
   @return The current dateTime *)
val current_dateTime : unit -> xs_dateTime

(** Gives the local timezone based upon the Unix system timezone set by the user.
   @return The local timezone *)
val local_timezone : unit -> xs_dayTimeDuration

(** Gives the dateTime corresponding the January 1st, 1970 at midnight. This is put in place
   to correspond to the Unix system calls which judge time based upon time elapsed since this
   default time
   @return The dateTime 1970-01-01T00:00:00 *)
val default_dateTime : unit -> xs_dateTime

(** Gives the dayTimeDuration corresponding to 0 days, 0 hours, 0 minutes, 0 seconds to represent
   the timezone offset at UTC
   @return The dayTimeDuration P0DT0H0M0S *)
val default_UTC : unit -> xs_dayTimeDuration

(** Adjusts the given time to the given timezone, or appends the timezone if there
   is no timezone attached to the given time
   @return The time adjusted to the given timezone *)
val adjust_time_to_timezone : xs_time -> xs_dayTimeDuration option -> xs_time

(** Adjusts the given date to the given timezone, or appends the timezone if there
   is no timezone attached to the given date
   @return The date adjusted to the given timezone *)
val adjust_date_to_timezone : xs_date -> xs_dayTimeDuration option -> xs_date

(** Adjusts the given dateTime to the given timezone, or appends the timezone if there
   is no timezone attached to the given dateTime
   @return The dateTime adjusted to the given timezone *)
val adjust_dateTime_to_timezone : xs_dateTime -> xs_dayTimeDuration option -> xs_dateTime

val yearMonthDuration_of_duration : xs_duration -> xs_yearMonthDuration
val dayTimeDuration_of_duration   : xs_duration -> xs_dayTimeDuration
val duration_of_yearMonthDuration : xs_yearMonthDuration -> xs_duration
val duration_of_dayTimeDuration   : xs_dayTimeDuration -> xs_duration

val negate_time : xs_time -> xs_time
val negate_date : xs_date -> xs_date
val negate_dateTime : xs_dateTime -> xs_dateTime
val negate_gYear : xs_gYear -> xs_gYear
val negate_gYearMonth : xs_gYearMonth -> xs_gYearMonth

