;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: low level functions dealing with dates
;;;Date: Sat Jul 17, 2010
;;;
;;;Abstract
;;;
;;;
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


#!r6rs
(library (nausicaa times-and-dates gregorian)
  (export

    ;; constants
    $number-of-seconds-in-half-day
    $number-of-days-per-month/non-leap-year
    $number-of-days-per-month/leap-year
    $number-of-days-the-first-day-of-each-month/non-leap-year
    $number-of-days-the-first-day-of-each-month/leap-year

    ;; conversions
    tai-seconds-and-nanoseconds->utc-date-fields

    ;; year functions
    gregorian-leap-year?	gregorian-year-number-of-days-since-beginning
    gregorian-natural-year	gregorian-year-western-easter-month-and-day
    gregorian-list-of-leap-years

    ;; week functions
    gregorian-index-of-day-in-week	gregorian-number-of-days-before-first-week)
  (import (rnrs)
    (only (nausicaa language extensions) define-constant receive)
    (nausicaa language infix)
    (nausicaa times-and-dates seconds-and-subseconds)
    (nausicaa times-and-dates julian-calendar))


;;;; constants

;;Number  of seconds  in a  half  day.  It  is used  in the  expressions
;;involving  the  Julian Day,  which  starts  at  noon (rather  than  at
;;midnight).
(define-constant $number-of-seconds-in-half-day 43200)

(define-constant $number-of-days-per-month/non-leap-year
  '#(0
     31		;Jan
     28		;Feb
     31		;Mar
     30		;Apr
     31		;May
     30		;Jun
     31		;Jul
     31		;Aug
     30		;Sep
     31		;Oct
     30		;Nov
     31))	;Dec

(define-constant $number-of-days-per-month/leap-year
  '#(0
     31		;Jan
     29		;Feb
     31		;Mar
     30		;Apr
     31		;May
     30		;Jun
     31		;Jul
     31		;Aug
     30		;Sep
     31		;Oct
     30		;Nov
     31))	;Dec

;;Number of days from  the beginning of the year, at the  first day of a
;;month; non-leap year.
(define-constant $number-of-days-the-first-day-of-each-month/non-leap-year
  '#(#f
     0		;Jan
     31		;Feb	  0 + 31
     59		;Mar	 31 + 28
     90		;Apr	 59 + 31
     120	;May	 90 + 30
     151	;Jun	120 + 31
     181	;Jul	151 + 30
     212	;Aug	181 + 31
     243	;Sep	212 + 31
     273	;Oct	243 + 30
     304	;Nov	273 + 31
     334))	;Dec	304 + 30

;;Number of days from  the beginning of the year, at the  first day of a
;;month; leap year.
(define-constant $number-of-days-the-first-day-of-each-month/leap-year
  '#(#f
     0		;Jan
     31		;Feb	  0 + 31
     60		;Mar	 31 + 29
     91		;Apr	 60 + 31
     121	;May	 91 + 30
     152	;Jun	121 + 31
     182	;Jul	152 + 30
     213	;Aug	182 + 31
     244	;Sep	213 + 31
     274	;Oct	244 + 30
     305	;Nov	274 + 31
     335))	;Dec	305 + 30


;;;; conversion between seconds and nanoseconds and date fields

(define (tai-seconds-and-nanoseconds->utc-date-fields tai-seconds nanoseconds tz-offset)
  ;;TZ-OFFSET must be the time zone offset in seconds.
  ;;
  (define (%tai-second-right-before-leap-second? tai-seconds)
    ;;Return  true  if  TAI-SECONDS  is  right  before  a  leap  second;
    ;;TAI-SECONDS must be a count of seconds since the Unix Epoch on the
    ;;TAI scale; TAI-SECONDS can  be zero, positive or negative.
    ;;
    (find (lambda (x)
	    (= tai-seconds (- (+ (car x) (cdr x)) 1)))
	  $leap-second-table))
  (define (%utc-seconds-and-nanoseconds->date utc-seconds nanoseconds aux-seconds)
    ;;Convert a seconds  count on the UTC scale  and a nanoseconds count
    ;;to a <date> object.  AUX-SECONDS must be a count of seconds to put
    ;;in the  seconds field of the <date>  object; if it is  false it is
    ;;ignored.
    ;;
    (receive (year month day secs)
	(julian-day-decode-number
	 (utc-seconds-and-nanoseconds->julian-day-number (+ utc-seconds tz-offset) nanoseconds))
      (let* ((hours    (div secs $number-of-seconds-in-one-hour))
	     (rem      (mod secs $number-of-seconds-in-one-hour))
	     (minutes  (div rem 60))
	     (seconds  (mod rem 60)))
	(values nanoseconds (or aux-seconds seconds) minutes hours
		day month year tz-offset))))

  (if (%tai-second-right-before-leap-second? tai-seconds)
;;;IF IT IS THE LEAP SECOND!!!!????
      ;;If  it's  *right* before  the  leap,  we  need to  pretend  to
      ;;subtract a second  and set the seconds explicitly  in the date
      ;;object.
      ;;
      ;;Notice that a leap second is always at 60 seconds in its minute,
      ;;so TAI-SECONDS is  right before a leap second if it  is at 59 in
      ;;its minute.
      ;;
      (%utc-seconds-and-nanoseconds->date (- (tai-seconds->utc-seconds tai-seconds) 1) nanoseconds 60)
    (%utc-seconds-and-nanoseconds->date (tai-seconds->utc-seconds tai-seconds) nanoseconds #f)))


;;;; year functions

(define (gregorian-leap-year? year)
  ;;Return true if YEAR is a leap year.  From the Calendar FAQ:
  ;;
  ;;  Every year divisible by 4 is a leap year.
  ;;  However, every year divisible by 100 is not a leap year.
  ;;  However, every year divisible by 400 is a leap year after all.
  ;;
  (or (zero? (mod year 400))
      (and (zero? (mod year 4))
	   (not (zero? (mod year 100))))))

(define (gregorian-list-of-leap-years inclusive-start inclusive-end)
  ;;Return a list of leap years in the selected range of years.
  ;;
  (let-values (((inclusive-start inclusive-end)
		(if (< inclusive-start inclusive-end)
		    (values inclusive-start inclusive-end)
		  (values inclusive-end inclusive-start))))
    (let loop ((year inclusive-start)
	       (ell  '()))
      (if (<= inclusive-end year)
	  (reverse ell)
	(loop (+ 1 year)
	      (if (gregorian-leap-year? year)
		  (cons year ell)
		ell))))))

(define (gregorian-year-number-of-days-since-beginning year month day)
  ;;Return the  number of days since  the beginning of the  year for the
  ;;specified date.  Assume that the given date is correct.
  ;;
  (+ day (vector-ref (if (gregorian-leap-year? year)
			 $number-of-days-the-first-day-of-each-month/leap-year
		       $number-of-days-the-first-day-of-each-month/non-leap-year)
		     month)))

(define (gregorian-natural-year n current-year)
  ;;Given a 'two digit' number, find the year within 50 years +/-.
  ;;
  ;;   (gregorian-natural-year 90 1983) => 1990
  ;;   (gregorian-natural-year 90 1913) => 1890
  ;;
  (let ((current-century (infix (current-year div0 100) * 100)))
    (cond ((>= n 100) n)
	  ((<  n 0)   n)
	  ((infix (current-century + n - current-year) <= 50)
	   (+ current-century n))
	  (else
	   (infix current-century - 100 + n)))))

(define (gregorian-year-western-easter-month-and-day year)
  ;;Return two  values being:  the 1-based index  of the month  in which
  ;;Easter falls,  the 1-based index of  the day in  which Easter falls.
  ;;The  computation is  for  western christianity  using the  proleptic
  ;;Gregorian calendar.
  ;;
  ;;From the Calendar FAQ, section 2.13.7.
  ;;
  (let* ((G	(infix year % 19))
	 (C	(infix year // 100))
	 (H	(infix (C - C // 4 - (8 * C + 13) // 25 + 19 * G + 15) % 30))
	 (I	(infix H - (H // 28) * (1 - (29 // (H + 1)) * ((21 - G) // 11))))
	 (J	(infix (year + year // 4 + I + 2 - C + C // 4) % 7))
	 (L	(infix I - J))

	 (easter-month	(infix 3 + (L + 40) // 44))
	 (easter-day	(infix L + 28 - 31 * (easter-month // 4))))
    (values easter-month easter-day)))


;;;; week functions

(define (gregorian-index-of-day-in-week year month day)
  ;;Return the index  of the day in its week,  zero based: Sun=0, Mon=1,
  ;;Tue=2, etc.  Assume the given date is correct.
  ;;
  ;;From the calendar FAQ section 2.6.
  ;;
  (let* ((a (infix (14 - month) // 12))
	 (y (infix year - a))
	 (m (infix month + (12 * a) - 2)))
    (infix (day + y + (y // 4) - (y // 100) + (y // 400) + ((31 * m) // 12)) % 7)))

(define (gregorian-number-of-days-before-first-week year day-of-week-starting-week)
  ;;Return the number of days before  the first day of the first week of
  ;;the YEAR.
  ;;
  ;;DAY-OF-WEEK-STARTING-WEEK  selects which  day starts  a week:  0 for
  ;;Sunday, 1 for Monday, 2 for Tuesday, etc.
  ;;
  (let ((index-of-day-in-week (gregorian-index-of-day-in-week year 1 1)))
    (infix (day-of-week-starting-week - index-of-day-in-week) % 7)))


;;;; done

)

;;; end of file
