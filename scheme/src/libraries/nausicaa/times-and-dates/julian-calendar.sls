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
    time-point->julian-date	julian-date->time-point
    julian-day-encode-number	julian-day-decode-number

    utc-seconds-and-nanoseconds->julian-day-number
    tai-seconds-and-nanoseconds->julian-day-number

    julian-day-number->utc-seconds-and-nanoseconds
    julian-day-number->tai-seconds-and-nanoseconds

    utc-seconds-and-nanoseconds->julian-date
    tai-seconds-and-nanoseconds->julian-date

    julian-date->utc-seconds-and-nanoseconds
    julian-date->tai-seconds-and-nanoseconds

    julian-day-number->modified-julian-day-number
    modified-julian-day-number->julian-day-number
    )
  (import (rnrs)
    (nausicaa language infix)
    (times-and-dates seconds-and-subseconds)
    (only (language-extensions) define-constant receive))


;;;; constants

;;Julian Day Number for the Epoch.
(define-constant $tai-epoch-in-jd 4881175/2)


;;;; julian dates, encoding to and decoding from proleptic Gregorian calendar

(define (time-point->julian-date year month day hours minutes seconds nanoseconds)
  ;;Convert the given date into a Julian Date value.
  ;;
  ;;This code was shamelessly copied from:
  ;;
  ;;	<http://www.imcce.fr/en/grandpublic/temps/jour_julien.php>
  ;;    <http://www.imcce.fr/langues/en/grandpublic/temps/jour_julien.php>
  ;;
  ;;in JavaScript.
  ;;
  (set! hours (infix hours
		     + (minutes     / 60)
		     + (seconds     / $number-of-seconds-in-one-hour)
		     + (nanoseconds / $number-of-nanoseconds-in-one-hour)))
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
  ;;Convert a Julian Date value to  a time point tuple; return 7 values:
  ;;year, month, day, hours, minutes, seconds, nanoseconds.
  ;;
  ;;This code was shamelessly copied from:
  ;;
  ;;	<http://www.imcce.fr/en/grandpublic/temps/jour_julien.php>
  ;;    <http://www.imcce.fr/langues/en/grandpublic/temps/jour_julien.php>
  ;;
  ;;in JavaScript.
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
    (values (exact year) (exact month) (exact day) (exact hour) (exact minutes)
	    (exact (floor seconds))
	    (exact (floor (* $number-of-nanoseconds-in-one-second (- seconds (floor seconds)))))
	    )))


;;;; julian day number, encoding to and decoding from proleptic Gregorian calendar

(define (julian-day-encode-number year month day)
  ;;Return an exact integer representing the Julian Day Number, starting
  ;;at noon, for the given  date; does the computation for the proleptic
  ;;Gregorian calendar.  Assumes the given date is correct.
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
  ;;Return  3  values:  year,  month,  day in  the  proleptic  Gregorian
  ;;calendar.  This function assumes that JDN is an exact integer.
  ;;
  ;;*NOTE* Watch out for precedence of * and // !!!
  ;;
  ;;From the Calendar FAQ, section 2.16.1.
  ;;
  (let* ((a	(infix jdn + 32044))
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
     (infix m + 3 - 12 * (m // 10))	;month
     (infix e + 1 - (153 * m + 2) // 5)	;day
     )))


;;;; conversion to and from Julian Day Number and Modified Julian Day Number

(define (julian-day-number->modified-julian-day-number jdn)
  (- jdn 4800001/2))

(define (modified-julian-day-number->julian-day-number mjdn)
  (+ mjdn 4800001/2))


;;;; conversion between seconds+nanoseconds and julian day number

(define (tai-seconds-and-nanoseconds->julian-day-number tai-seconds nanoseconds)
  ;;Return the  Julian Day Number  representing the given counts  of TAI
  ;;seconds and nanoseconds.
  ;;
  (infix $tai-epoch-in-jd +
	 (tai-seconds + nanoseconds / $number-of-nanoseconds-in-one-second)
	 / $number-of-seconds-in-one-day))


(define (julian-day-number->tai-seconds-and-nanoseconds jdn)
  ;;Return  two   values:  the   TAI  seconds  and   nanoseconds  counts
  ;;representing JDN.
  ;;
  ;;The strategy is: convert JDN to the total count of nanoseconds since
  ;;the Epoch  on the TAI scale,  then convert the  count of nanoseconds
  ;;into TAI-seconds and nanoseconds.
  ;;
  (let ((tai-nanosecs-since-epoch (* (- jdn $tai-epoch-in-jd)
				     $number-of-seconds-in-one-day
				     $number-of-nanoseconds-in-one-second)))
    (values (div tai-nanosecs-since-epoch $number-of-nanoseconds-in-one-second)
	    (mod tai-nanosecs-since-epoch $number-of-nanoseconds-in-one-second))))

;;; --------------------------------------------------------------------

(define (utc-seconds-and-nanoseconds->julian-day-number utc-seconds nanoseconds)
  ;;Return the  Julian Day Number  representing the given counts  of UTC
  ;;seconds and nanoseconds.
  ;;
  (tai-seconds-and-nanoseconds->julian-day-number (utc-seconds->tai-seconds utc-seconds) nanoseconds))

(define (julian-day-number->utc-seconds-and-nanoseconds jdn)
  ;;Return  two   values:  the   UTC  seconds  and   nanoseconds  counts
  ;;representing JDN.
  ;;
  (receive (tai-seconds nanoseconds)
      (julian-day-number->tai-seconds-and-nanoseconds jdn)
    (values (tai-seconds->utc-seconds tai-seconds) nanoseconds)))


;;;; conversion between seconds+nanoseconds and julian date

(define (utc-seconds-and-nanoseconds->julian-date utc-seconds nanoseconds)
  #f)

(define (tai-seconds-and-nanoseconds->julian-date tai-seconds nanoseconds)
  #f)

(define (julian-date->utc-seconds-and-nanoseconds jd)
  (values #f #f))

(define (julian-date->tai-seconds-and-nanoseconds jd)
  (values #f #f))


;;;; done

)

;;; end of file
