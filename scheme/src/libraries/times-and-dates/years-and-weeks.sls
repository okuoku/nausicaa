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


(library (times-and-dates years-and-weeks)
  (export

    ;; constants
    $number-of-seconds-in-half-day
    $tai-epoch-in-jd
    $number-of-days-per-month/non-leap-year
    $number-of-days-per-month/leap-year
    $number-of-days-the-first-day-of-each-month/non-leap-year
    $number-of-days-the-first-day-of-each-month/leap-year

    ;; year functions
    gregorian-leap-year?	gregorian-year-number-of-days-since-beginning
    gregorian-natural-year	gregorian-year-western-easter-month-and-day

    ;; week functions
    gregorian-index-of-day-in-week	gregorian-number-of-days-before-first-week)
  (import (rnrs)
    (only (language-extensions) define-constant)
    (infix))


;;;; constants

;;Number  of seconds  in a  half  day.  It  is used  in the  expressions
;;involving  the  Julian Day,  which  starts  at  noon (rather  than  at
;;midnight).
(define-constant $number-of-seconds-in-half-day 43200)

;;Julian day number for the Epoch.
(define-constant $tai-epoch-in-jd 4881175/2)

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

(define (gregorian-year-number-of-days-since-beginning year month day)
  ;;Return the  number of days  from the beginning  of the year  for the
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
