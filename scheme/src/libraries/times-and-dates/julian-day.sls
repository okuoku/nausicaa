;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: julian day stuff
;;;Date: Thu Jul 29, 2010
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


(library (times-and-dates julian-day)
  (export

    ;; constants
    $tai-epoch-in-jd

    ;; conversion
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


;;;; done

)

;;; end of file
