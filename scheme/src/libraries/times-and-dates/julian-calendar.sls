;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: julian calendar utilities
;;;Date: Thu Jul 29, 2010
;;;
;;;Abstract
;;;
;;;	The Julian  Date (JD, <http://en.wikipedia.org/wiki/Julian_day>)
;;;	is the  interval of time  in days and  fractions of a  day since
;;;	-4714-11-24T12:00:00Z  (November 24, -4714  at noon,  UTC scale,
;;;	time zone zero).
;;;
;;;	The Julian Day  Number (JDN) is the integral  part of the Julian
;;;	Date.  The day commencing  at the above-mentioned epoch is zero.
;;;	Negative  values can be  used for  preceding dates,  though they
;;;	predate all recorded history.
;;;
;;;	The Modified Julian Day Number (MJDN) represents a point in time
;;;	as number of days  since 1858-11-17T00:00:00Z (November 17, 1858
;;;	at midnight, UTC scale, time zone zero).
;;;
;;;	The MDJN is  4800001/2 = 2400000.5 days less  than the JDN; this
;;;	brings  the numbers  into a  more manageable  numeric  range and
;;;	makes the day numbers change at midnight UTC rather than noon.
;;;
;;;	Julian Date test  values can be computed with  the calculator at
;;;	(URL last verified Thu Jul 29, 2010):
;;;
;;;	   <http://www.imcce.fr/en/grandpublic/temps/jour_julien.php>
;;;        <http://www.imcce.fr/langues/en/grandpublic/temps/jour_julien.php>
;;;
;;;	the   number  computed  by   that  calculator   is  the   JD  at
;;;	year/month/day hour:minute:second.
;;;
;;;Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;

;;;SRFI-19: Time Data Types and Procedures.
;;;
;;;Modified by Derick Eddington to be included into the (srfi time) R6RS
;;;library.
;;;
;;;Copyright (C) I/NET, Inc. (2000, 2002, 2003). All Rights Reserved.
;;;
;;;This document and  translations of it may be  copied and furnished to
;;;others, and derivative works that  comment on or otherwise explain it
;;;or assist  in its implementation  may be prepared,  copied, published
;;;and  distributed, in  whole or  in part,  without restriction  of any
;;;kind, provided that the above copyright notice and this paragraph are
;;;included  on all  such copies  and derivative  works.   However, this
;;;document itself may  not be modified in any way,  such as by removing
;;;the  copyright  notice  or  references  to  the  Scheme  Request  For
;;;Implementation process  or editors, except as needed  for the purpose
;;;of  developing SRFIs  in  which case  the  procedures for  copyrights
;;;defined  in the  SRFI process  must be  followed, or  as  required to
;;;translate it into languages other than English.
;;;
;;;The limited permissions  granted above are perpetual and  will not be
;;;revoked by the authors or their successors or assigns.
;;;
;;;This document and the information  contained herein is provided on an
;;;"AS  IS" basis  and  THE AUTHOR  AND  THE SRFI  EDITORS DISCLAIM  ALL
;;;WARRANTIES,  EXPRESS OR  IMPLIED, INCLUDING  BUT NOT  LIMITED  TO ANY
;;;WARRANTY THAT THE USE OF THE INFORMATION HEREIN WILL NOT INFRINGE ANY
;;;RIGHTS OR ANY IMPLIED WARRANTIES  OF MERCHANTABILITY OR FITNESS FOR A
;;;PARTICULAR PURPOSE.


(library (times-and-dates julian-calendar)
  (export

    ;; constants
    $tai-epoch-in-jd

    ;; conversion
    time-point->julian-date			julian-date->time-point
    julian-day-encode-number			julian-day-decode-number
    julian-day->modified-julian-day		modified-julian-day->julian-day
    utc-seconds-and-nanoseconds->julian-day	julian-day->utc-seconds-and-nanoseconds
    tai-seconds-and-nanoseconds->julian-day	julian-day->tai-seconds-and-nanoseconds
    )
  (import (rnrs)
    (infix)
    (times-and-dates seconds-and-subseconds)
    (only (language-extensions) define-constant receive))


;;;; constants

;;Julian Day Number for the Epoch.
(define-constant $tai-epoch-in-jd 4881175/2)


;;;; encoding to and decoding from proleptic Gregorian calendar

(define (julian-day-encode-number year month day)
  ;;Return  an exact integer  representing the  Julian Day,  starting at
  ;;noon, for  the given  date; does the  computation for  the Gregorian
  ;;calendar.  Assumes the given date is correct.
  ;;
  ;;Notice that the returned value is not the "public" Julian Day number
  ;;defined by this library: the  returned value is missing the fraction
  ;;corresponding to hours, minutes, seconds and nanoseconds.
  ;;
  ;;From the Calendar FAQ, section 2.16.1.
  ;;
  (let* ((a (infix (14 - month) // 12))
	 (y (- (infix year + 4800 - a)
	       (if (negative? year) -1 0)
		;This  adjusts the year,  taking care  of the  fact that
		;between 1 Annus Dominis and 1 Before Christ there is no
		;year zero.  See the Calendar FAQ for details.
	       ))
	 (m (infix month + 12 * a - 3)))
    (infix day
	   + (153 * m + 2) // 5
	   + 365 * y
	   + y // 4
	   - y // 100
	   + y // 400
	   - 32045)))

(define (julian-day-decode-number jdn)
  ;;Return  4  values:  seconds,  day,  month,  year  in  the  Gregorian
  ;;calendar.
  ;;
  ;;*NOTE* Watch out for precedence of * and // !!!
  ;;
  ;;From the Calendar FAQ, section 2.16.1.
  ;;
  (let* ((days	(truncate jdn))
	 (a	(infix days + 32044))
	 (b	(infix (4 * a + 3) // 146097))
	 (c	(infix a - (146097 * b) // 4))
	 (d	(infix (4 * c + 3) // 1461))
	 (e	(infix c - (1461 * d) // 4))
	 (m	(infix ((5 * e) + 2) // 153))
	 (y	(infix (100 * b) + d - 4800 + (m // 10))))
    (values
     (if (>= 0 y) (- y 1) y)
		;Year.  This adjusts year,  taking care of the fact that
		;between 1 Annus Dominis and 1 Before Christ there is no
		;year zero.  See the Calendar FAQ for details.
     (infix m + 3 - 12 * (m // 10))			  ;month
     (infix e + 1 - (153 * m + 2) // 5)			  ;day
     (infix (jdn - days) * $number-of-seconds-in-one-day) ;seconds
     )))

;;; --------------------------------------------------------------------

(define (julian-day-encode-fraction hours minutes seconds)
  ;;Return an exact number representing  the fractional part of a Julian
  ;;Day Number  representing the given  hours, minutes and  seconds; the
  ;;returned value can  be added to an integer Julian  Day Number as the
  ;;one returned by JULIAN-DAY-ENCODE-NUMBER.
  ;;
  ;;The  function assumes that  the arguments  are exact  numbers: HOURS
  ;;being an integer  in the range [0, 23); MINUTES  being an integer in
  ;;the range [0, 59); seconds being a rational in the range [0, 60).
  ;;
  ;;The returned value is positive for times in the range from 12:00:00Z
  ;;(included) to 24:00:00Z (excluded); it  is negative for times in the
  ;;range 00:00:00Z (included) 11:59:60Z (excluded).
  ;;
  (let ((fraction (/ (+ (* hours	$number-of-seconds-in-one-hour)
			(* minutes	$number-of-seconds-in-one-minute)
			(* seconds	$number-of-seconds-in-one-hour))
		     $number-of-seconds-in-one-day)))
    (if (<= 1/2 fraction)
	(- fraction 1/2)
      (- 1/2 fraction))))

(define (julian-day-decode-fraction jdn)
  (let* ((fraction	(- jdn (truncate jdn)))
	 (seconds	(* fraction $number-of-seconds-in-one-day)))
    #f))


;;;; conversion to and from Modified Julian Day

(define (julian-day->modified-julian-day julian-day)
  (- julian-day 4800001/2))

(define (modified-julian-day->julian-day modified-julian-day)
  (+ modified-julian-day 4800001/2))


;;;; conversion to and from seconds and nanoseconds

(define (utc-seconds-and-nanoseconds->julian-day utc-seconds nanoseconds)
  ;;Return the  Julian Day Number  representing the given counts  of UTC
  ;;seconds and nanoseconds.
  ;;
  (infix $tai-epoch-in-jd +
	 (utc-seconds + nanoseconds / $number-of-nanoseconds-in-one-second)
	 / $number-of-seconds-in-one-day))


(define (julian-day->utc-seconds-and-nanoseconds jdn)
  ;;Return  two   values:  the   UTC  seconds  and   nanoseconds  counts
  ;;representing JDN.
  ;;
  (let ((nanoseconds (* (- jdn $tai-epoch-in-jd)
			$number-of-seconds-in-one-day
			$number-of-nanoseconds-in-one-second)))
    (values (div nanoseconds $number-of-nanoseconds-in-one-second)
	    (mod nanoseconds $number-of-nanoseconds-in-one-second))))

;;; --------------------------------------------------------------------

(define (tai-seconds-and-nanoseconds->julian-day tai-seconds nanoseconds)
  ;;Return the  Julian Day Number  representing the given counts  of TAI
  ;;seconds and nanoseconds.
  ;;
  (utc-seconds-and-nanoseconds->julian-day (tai->utc tai-seconds) nanoseconds))

(define (julian-day->tai-seconds-and-nanoseconds jdn)
  ;;Return  two   values:  the   TAI  seconds  and   nanoseconds  counts
  ;;representing JDN.
  ;;
  (receive (utc-seconds nanoseconds)
      (julian-day->utc-seconds-and-nanoseconds jdn)
    (values (utc->tai utc-seconds) nanoseconds)))


(define (time-point->julian-date year month day hours minutes seconds)
  ;;Convert the given date into a Julian Date value.
  ;;
  ;;This code was shamelessly copied from:
  ;;
  ;;	<http://www.imcce.fr/en/grandpublic/temps/jour_julien.php>
  ;;    <http://www.imcce.fr/langues/en/grandpublic/temps/jour_julien.php>
  ;;
  ;;in JavaScript:
  ;;
  ;; MM=(form.nmonth.value=="")? "0" : eval(form.nmonth.value);
  ;; DD=(form.nday.value=="")? "0": eval(form.nday.value);
  ;; YY=(form.nyear.value=="") ? "0" :eval(form.nyear.value);
  ;; HR=(form.nhour.value=="")? "0" :eval(form.nhour.value);
  ;; MN=(form.nminute.value=="") ? "0" :eval(form.nminute.value);
  ;; SS=(form.nsecondes.value=="") ? "0" : eval(form.nsecondes.value);
  ;; with (Math) {
  ;;   HR = HR + (MN / 60) + (SS / 3600);
  ;;   GGG = 1;
  ;;   if( YY < 1582 ) GGG = 0;
  ;;   if( YY <= 1582 && MM < 10 ) GGG = 0;
  ;;   if( YY <= 1582 && MM == 10 && DD < 5 ) GGG = 0;
  ;;   JD = -1 * floor(7 * (floor((MM + 9) / 12) + YY) / 4);
  ;;   S = 1;
  ;;   if ((MM - 9)<0) S=-1;
  ;;   A = abs(MM - 9);
  ;;   J1 = floor(YY + S * floor(A / 7));
  ;;   J1 = -1 * floor((floor(J1 / 100) + 1) * 3 / 4);
  ;;   JD = JD + floor(275 * MM / 9) + DD + (GGG * J1);
  ;;   JD = JD + 1721027 + 2 * GGG + 367 * YY - 0.5;
  ;;   JD = JD + (HR / 24);
  ;; }
  ;; form.result.value = JD;
  ;;
  (set! hours (infix hours + (minutes / 60) + (seconds / 3600)))
  (let* ((GGG (if (or (< year 1582)
		      (and (<= year 1582) (or (< month 10)
					      (and (= month 10) (< day 5)))))
		  0 1))
	 (JD	(- (infix floor (7/4 * (floor ((month + 9) / 12) + year)))))
	 (sign	(if (> month 9) -1 1))
	 (J1	(infix floor (year + sign * floor(abs (month - 9) / 7)))))
    (set! J1 (- (infix floor ((floor (J1 / 100) + 1) * 3/4))))
    (set! JD (infix JD + floor (275 * month / 9) + day + (GGG * J1)))
    (infix JD + 1721027 + 2 * GGG + 367 * year - 1/2 + (hours / 24))))

(define (julian-date->time-point JD)
  ;;Convert a Julian Date value to  a time point tuple; return 6 values:
  ;;year, month, day, hours, minutes, seconds.
  ;;
  ;;This code was shamelessly copied from:
  ;;
  ;;	<http://www.imcce.fr/en/grandpublic/temps/jour_julien.php>
  ;;    <http://www.imcce.fr/langues/en/grandpublic/temps/jour_julien.php>
  ;;
  ;;in JavaScript:
  ;;
  ;; JD = eval(form.result.value)
  ;; with (Math) {
  ;; 	Z = floor(JD+0.5);
  ;; 	F = JD+0.5 - Z;
  ;; 	if (Z < 2299161) {
  ;;    	A = Z
  ;; 		} else
  ;;    	{I = floor((Z - 1867216.25)/36524.25);
  ;;    	A = Z + 1 + I - floor(I/4);
  ;; 	 }
  ;; 	B = A + 1524;
  ;; 	C = floor((B - 122.1)/365.25);
  ;; 	D = floor(365.25 * C);
  ;; 	T = floor((B - D)/ 30.6001);
  ;; 	RJ = B - D - floor(30.6001 * T) + F;
  ;; 	JJ = floor(RJ);
  ;; 	RH = (RJ - floor(RJ)) * 24;
  ;; 	Heure = floor(RH);
  ;; 	Mn = floor((RH - Heure )*60);
  ;; 	Sec = ((RH - Heure )*60 - Mn )*60;
  ;; 	if (T < 14) {
  ;;    	MM = T - 1
  ;; 	} else {
  ;; 	  if ((T == 14) || (T == 15))  MM = T - 13
  ;; 	}
  ;; 	if (MM > 2) {
  ;;    	AA = C - 4716
  ;; 	} else {
  ;;    	if ((MM == 1) || (MM == 2)) AA = C - 4715
  ;; 	}
  ;; }
  ;;  form.nmonth.value =  MM;
  ;;  form.nday.value   =  JJ;
  ;;  form.nhour.value  =  Heure;
  ;;  form.nyear.value  =  AA;
  ;;  form.nminute.value=  Mn;
  ;;  form.nsecondes.value=Sec;
  ;;
  (let* ((Z		(infix floor (JD + 0.5)))
	 (F		(infix JD + 1/2 - Z))
	 (A		(if (< Z 2299161)
			    Z
			  (let ((I (infix floor ((Z - 1867216.25) / 36524.25))))
			    (infix Z + 1 + I - floor (I / 4)))))
	 (B		(infix A + 1524))
	 (C		(infix floor ((B - 122.1) / 365.25)))
	 (D		(infix floor(365.25 * C)))
	 (T		(infix floor((B - D)/ 30.6001)))
	 (RJ		(infix B - D - floor(30.6001 * T) + F))
	 (day		(floor RJ))
	 (RH		(infix (RJ - floor(RJ)) * 24))
	 (hour		(floor RH))
	 (minutes	(infix floor((RH - hour) * 60)))
	 (seconds	(infix ((RH - hour ) * 60 - minutes) * 60))
	 (month		(cond ((< T 14)
			       (- T 1))
			      ((or (= T 14) (= T 15))
			       (- T 13))
			      (else
			       0)))
	 (year		(cond ((> month 2)
			       (- C 4716))
			      ((or (= month 1) (= month 2))
			       (- C 4715))
			      (else
			       0))))
    (values (exact year) (exact month) (exact day) (exact hour) (exact minutes) seconds)))


;;;; done

)

;;; end of file
