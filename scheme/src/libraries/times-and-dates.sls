;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: times and dates handling
;;;Date: Mon May 17, 2010
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
;;;Modified by Marco Maggi upon inclusion in Nausicaa.
;;;
;;;TODO: For implementations which have threads, the thread timing stuff
;;;can probably be made to work.
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
(library (times-and-dates)
  (export

    ;; constants
    time-duration time-monotonic time-tai time-utc
    ;;time-process time-thread

    ;; current time and clock resolution
    current-date current-julian-day current-modified-julian-day
    current-time time-resolution

    ;; time object and accessor
    (rename (make-<time> make-time))
    time?
    time-type time-nanosecond time-second
    set-time-type! set-time-nanosecond! set-time-second!
    copy-time

    ;; time object comparison procedures
    time<=? time<? time=? time>=? time>?

    ;; time object arithmetic procedures
    time-difference add-duration subtract-duration

    ;; date object and accessors
    (rename (make-<date> make-date))
    <date>?
    <date>-nanosecond <date>-second <date>-minute <date>-hour
    <date>-day <date>-month <date>-year <date>-zone-offset
    date-year-day date-week-day date-week-number

    ;; converters
    date->julian-day
    date->modified-julian-day
    date->time-monotonic
    date->time-tai
    date->time-utc

    julian-day->date
    julian-day->time-monotonic
    julian-day->time-tai
    julian-day->time-utc

    modified-julian-day->date
    modified-julian-day->time-monotonic
    modified-julian-day->time-tai
    modified-julian-day->time-utc

    time-monotonic->date
    time-monotonic->julian-day
    time-monotonic->modified-julian-day
    time-monotonic->time-tai
    time-monotonic->time-utc

    time-tai->date
    time-tai->julian-day
    time-tai->modified-julian-day
    time-tai->time-monotonic
    time-tai->time-utc

    time-utc->date
    time-utc->julian-day
    time-utc->modified-julian-day
    time-utc->time-monotonic
    time-utc->time-tai

    ;; string conversion
    date->string string->date)
  (import (nausicaa)
    (infix)
    (rnrs mutable-strings)
    (formations)
    (times-and-dates compat))


;;;; porting

;;The following are required to the underlying Scheme implementation.
;;
;;HOST:CURRENT-TIME
;;  Must  return  a  single   value  holding  the  current  seconds  and
;;  nanoseconds since the Epoch.  These values can be retrieved with the
;;  platform's POSIX "gettimeofday()".
;;
;;HOST:TIME-SECOND
;;  Given the return value from HOST:CURRENT-TIME must return the number
;;  of seconds.
;;
;;HOST:TIME-NANOSECOND
;;  Given the return value from HOST:CURRENT-TIME must return the number
;;  of nanoseconds.

;;According  to the  SRFI document:  the  range for  nanoseconds is  [0;
;;9,999,999] inclusive; we know that:
;;
;;  1 nanosecond		= 10^{-9} seconds
;;  10^9 nanoseconds		= 1 second
;;  1,000,000,000 nanoseconds	= 1 second
;;
;;  1 microsecond		= 10^{-6} seconds
;;  10^3 microseconds		= 1 second
;;  1,000,000 microseconds	= 1 second
;;
;;  1 millisecond		= 10^{-3} seconds
;;  10^3 milliseconds		= 1 second
;;  1,000 milliseconds		= 1 second
;;
;;so the range  of nanoseconds can represent "small  times" from zero up
;;to "almost" 10 microseconds.
;;


;;;; helpers

(define-syntax %display
  ;;Like DISPLAY  but outputs the value  in the port bound  to "port" in
  ;;the lexical context of the macro use.
  ;;
  (lambda (stx)
    (syntax-case stx ()
      ((?k ?val)
       #`(display ?val #,(datum->syntax #'?k 'port))))))


;;;; global variables and constants

(define-enumeration enum-time-type
  (time-tai
   time-utc
   time-monotonic)
  time-type-set)

(define-constant time-tai	'time-tai)
(define-constant time-utc	'time-utc)
(define-constant time-monotonic	'time-monotonic)
(define-constant time-duration	'time-duration)
;;(define-constant time-thread	'time-thread)
;;(define-constant time-process	'time-process)

(define-constant $locale-number-separator ".")

(define-constant $locale-abbr-weekday-vector
  (vector "Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
(define-constant $locale-long-weekday-vector
  (vector "Sunday" "Monday" "Tuesday" "Wednesday"
	  "Thursday" "Friday" "Saturday"))

(define-constant $locale-abbr-month-vector
  ;;Note empty string in 0th place.
  (vector ""
	  "Jan" "Feb" "Mar" "Apr"
	  "May" "Jun" "Jul" "Aug"
	  "Sep" "Oct" "Nov" "Dec"))
(define-constant $locale-long-month-vector
  ;;Note empty string in 0th place.
  (vector ""
	  "January" "February" "March"
	  "April" "May" "June"
	  "July" "August" "September"
	  "October" "November" "December"))

(define-constant $locale-pm "PM")
(define-constant $locale-am "AM")

;; See date->string
(define-constant $locale-date-time-format "~a ~b ~d ~H:~M:~S~z ~Y")
(define-constant $locale-short-date-format "~m/~d/~y")
(define-constant $locale-time-format "~H:~M:~S")
(define-constant $iso-8601-date-time-format "~Y-~m-~dT~H:~M:~S~z")
;;-- Miscellaneous Constants.
;;-- only the $tai-epoch-in-jd might need changing if
;;   a different epoch is used.

;;Number of  nanoseconds in a  second.  Useful, for example,  to convert
;;seconds to nanoseconds.
(define-constant $number-of-nanoseconds-in-a-second (expt 10 9))

;;Number of seconds in a day.
(define-constant $number-of-seconds-in-a-day  86400)

;;Number of seconds in a half day.
(define-constant $number-of-seconds-in-half-day 43200)

;;Number of seconds in an hour.
(define-constant $number-of-seconds-in-an-hour  (* 60 60))

;;Number of seconds in a minute.
(define-constant $number-of-seconds-in-a-minute	60)

;;Julian day number for the Epoch.
(define-constant $tai-epoch-in-jd 4881175/2)

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

(define-constant $escape-char #\~)

(define-inline (%escape-char? ?char)
  (char=? $escape-char ?char))


;;;; errors

(define (%time-error who tag value)
  (define-constant tm:time-error-tags
    '((invalid-clock-type		. "invalid clock type")
      (unsupported-clock-type		. "unsupported clock type")
      (incompatible-time-types		. "incompatible time types")
      (not-duration			. "not duration")
      (dates-are-immutable		. "dates are immutable")
      (bad-date-format-string		. "bad date format string")
      (bad-date-template-string		. "bad date template string")
      (invalid-month-specification	. "invalid month specification")))
  (cond ((assq tag tm:time-error-tags)
	 => (lambda (p)
	      (if value
		  (error who (cdr p) value)
		(error who (cdr p)))))
	(else
	 (assertion-violation who "internal error: unsupported error tag" tag))))


;;;; leap seconds handling
;;
;;See:
;;
;;  ftp://maia.usno.navy.mil/ser7/tai-utc.dat
;;
;;and update as necessary.
;;
;;These procedures read the file in the above format and create the leap
;;second table.
;;
;; (set! $leap-second-table (tm:read-tai-utc-data "tai-utc.dat"))
;;
;; (define-syntax read-line
;;   (syntax-rules ()
;;     ((_)
;;      (get-line (current-input-port)))
;;     ((_ ?port)
;;      (get-line ?port))))
;;
;; (define (tm:read-tai-utc-data filename)
;;   (define (convert-jd jd)
;;     (* (- (inexact->exact jd) $tai-epoch-in-jd) $number-of-seconds-in-a-day))
;;   (define (convert-sec sec)
;;     (inexact->exact sec))
;;   (let ( (port (open-input-file filename))
;; 	 (table '()) )
;;     (let loop ((line (read-line port)))
;;       (if (not (eq? line (eof-object)))
;; 	  (receive (port getter)
;; 	      (open-string-output-port (string-append "(" line ")"))
;; 	    (let* ((data (read port))
;; 		   (year (car data))
;; 		   (jd   (cadddr (cdr data)))
;; 		   (secs (cadddr (cdddr data))))
;; 	      (if (>= year 1972)
;; 		  (set! table (cons (cons (convert-jd jd) (convert-sec secs)) table))
;; 		(loop (read-line port)))))))
;;     table))
;;
;; (define (read-leap-second-table filename)
;;   (set! $leap-second-table (tm:read-tai-utc-data filename))
;;   (values))

(define-constant $leap-second-table
  ;;Each entry is:
  ;;
  ;;    ( <UTC seconds since Epoch> . <number of seconds to add for TAI> )
  ;;
  ;;note they go higher to lower, and end in 1972.
  ;;
  '((1136073600 . 33)
    (915148800 . 32)
    (867715200 . 31)
    (820454400 . 30)
    (773020800 . 29)
    (741484800 . 28)
    (709948800 . 27)
    (662688000 . 26)
    (631152000 . 25)
    (567993600 . 24)
    (489024000 . 23)
    (425865600 . 22)
    (394329600 . 21)
    (362793600 . 20)
    (315532800 . 19)
    (283996800 . 18)
    (252460800 . 17)
    (220924800 . 16)
    (189302400 . 15)
    (157766400 . 14)
    (126230400 . 13)
    (94694400 . 12)
    (78796800 . 11)
    (63072000 . 10)))

(define (%leap-second-delta utc-seconds)
  ;;Given the UTC  seconds count since the Epoch,  compute the number of
  ;;leap seconds to correct it:
  ;;
  ;;  corrected-utc-seconds = utc-seconds + leap-seconds
  ;;
  ;;this correction yields the TAI seconds from the UTC seconds.
  ;;
  (if (infix (utc-seconds < ((1972 - 1970) * 365 * $number-of-seconds-in-a-day)))
      0
    (let loop ((table $leap-second-table))
      (if (>= utc-seconds (caar table))
	  (cdar table)
	(loop (cdr table))))))

(define (%leap-second-neg-delta tai-seconds)
  ;;From TAI seconds to UTC seconds.
  ;;
  (if (infix (tai-seconds < ((1972 - 1970) * 365 * $number-of-seconds-in-a-day)))
      0
    (let loop ((table $leap-second-table))
      (if (null? table)
	  0
	(let ((elm (car table)))
	  (if (<= (cdr elm) (- tai-seconds (car elm)))
	      (cdr elm)
	    (loop (cdr table))))))))


(define-class (<time> make-<time> time?)
  (nongenerative nausicaa:times-and-dates:<time>)
  (fields (mutable type		time-type	set-time-type!)
	  (mutable second	time-second	set-time-second!)
	  (mutable nanosecond	time-nanosecond	set-time-nanosecond!))
  (virtual-fields (immutable full-nanoseconds))
  (protocol (lambda (make-top)
	      (lambda (type secs nanosecs)
		(let ((in-range (< nanosecs $number-of-nanoseconds-in-a-second)))
		  ((make-top) type
		   (if in-range
		       secs
		     (infix secs + nanosecs // $number-of-nanoseconds-in-a-second))
		   (if in-range
		       nanosecs
		     (mod nanosecs $number-of-nanoseconds-in-a-second))))))))

(define (copy-time (time <time>))
  (make <time> time.type time.second time.nanosecond))

(define (<time>-full-nanoseconds (T <time>))
  ;;Convert a <time> object into  an exact integer representing the same
  ;;time in nanoseconds.
  ;;
  (infix ((T.second * $number-of-nanoseconds-in-a-second) + T.nanosecond)))


;;;; current time

;;; specific time getters
;;
;;Ikarus's (current-time) uses POSIX gettimeofday() I'm not sure why the
;;original was using time-nanoseconds as 10000 * the milliseconds.
;;

(define (%get-time-of-day)
  (let ((ct (host:current-time)))
    (values (host:time-second ct)
            (host:time-nanosecond ct))))

(define current-time
  (case-lambda
   (()
    (current-time (enum-time-type time-utc)))
   ((clock-type)
    (define (%current-time-utc)
      (receive (seconds nanoseconds)
	  (%get-time-of-day)
	(make <time> time-utc seconds nanoseconds)))
    (define (%current-time-tai)
      (receive (seconds nanoseconds)
	  (%get-time-of-day)
	(make <time> time-tai
	      (+ seconds (%leap-second-delta seconds))
	      nanoseconds)))
    (define (%current-time-monotonic)
      ;;We  assume  monotonic time  to  be the  same  as  TAI.  A  different
      ;;implementation of  CURRENT-TIME-MONTONIC will require  rewriting all
      ;;of the TIME-MONOTONIC converters, of course.
      ;;
      (receive (seconds nanoseconds)
	  (%get-time-of-day)
	(make <time> time-monotonic
	      (+ seconds (%leap-second-delta seconds))
	      nanoseconds)))
    ;; (define (%current-time-ms-time time-type proc)
    ;;   (let ((current-ms (proc)))
    ;;     (make <time> time-type XXX ZZZ)))
    ;; (define (tm:current-time-thread)
    ;;   (%current-time-ms-time time-process current-process-milliseconds))
    ;; (define (tm:current-time-process)
    ;;   (%current-time-ms-time time-process current-process-milliseconds))
    ;; (define (tm:current-time-gc)
    ;;   (%current-time-ms-time time-gc current-gc-milliseconds))
    (case clock-type
      ((time-tai)	(%current-time-tai))
      ((time-utc)	(%current-time-utc))
      ((time-monotonic) (%current-time-monotonic))
      ;;((time-thread)	(tm:current-time-thread))
      ;;((time-process)	(tm:current-time-process))
      ;;((time-gc)	(tm:current-time-gc))
      (else
       (%time-error 'current-time 'invalid-clock-type clock-type))))))

(define time-resolution
  (case-lambda
   (()
    (time-resolution (enum-time-type time-utc)))
   ((clock-type)
    (case clock-type
      ((time-tai time-utc time-monotonic)
       host:time-resolution)
      ;;((eq? clock-type time-thread) host:time-resolution)
      ;;((eq? clock-type time-process) host:time-resolution)
      ;;((eq? clock-type time-gc) host:time-resolution)
      (else
       (%time-error 'time-resolution 'invalid-clock-type clock-type))))))


;;;; time comparisons

(define (%time-compare-check (one <time>) (two <time>) who)
  (assert (is-a? one <time>))
  (assert (is-a? two <time>))
  (unless (eq? one.type two.type)
    (%time-error who 'incompatible-time-types #f)))

(define (time=? (one <time>) (two <time>))
  (%time-compare-check one two 'time=?)
  (and (= one.second     two.second)
       (= one.nanosecond two.nanosecond)))

(define (%time-compare who cmp-sec cmp-nano (one <time>) (two <time>))
  (%time-compare-check one two who)
  (or (cmp-sec one.second two.second)
      (and (= one.second two.second)
	   (cmp-nano one.nanosecond two.nanosecond))))

(define (time>? one two)
  (%time-compare 'time>? > > one two))

(define (time<? one two)
  (%time-compare 'time<? < < one two))

(define (time>=? one two)
  (%time-compare 'time>=? > >= one two))

(define (time<=? one two)
  (%time-compare 'time<=? < <= one two))


;;;; time arithmetics

(define (%nanoseconds->time time-type nanoseconds)
  ;;Convert  a full time  as exact  integer in  nanoseconds to  a <time>
  ;;object.
  ;;
  (make <time> time-type
	(div nanoseconds $number-of-nanoseconds-in-a-second)
	(mod nanoseconds $number-of-nanoseconds-in-a-second)))

(define (time-difference (T1 <time>) (T2 <time>))
  (assert (is-a? T1 <time>))
  (assert (is-a? T2 <time>))
  (unless (eq? T1.type T2.type)
    (%time-error 'time-difference 'incompatible-time-types (list T1 T2)))
  (if (time=? T1 T2)
      (make <time> 'time-duration 0 0)
    (let ((diff (- T1.full-nanoseconds T2.full-nanoseconds)))
      (make <time> 'time-duration
	    (div diff $number-of-nanoseconds-in-a-second)
	    (abs (mod diff $number-of-nanoseconds-in-a-second))))))

(define (add-duration (T <time>) (D <time>))
  (assert (is-a? T <time>))
  (assert (is-a? D <time>))
  (unless (eq? D.type 'time-duration)
    (%time-error 'add-duration 'not-duration D))
  (let ((sec-plus  (+ T.second     D.second))
	(nsec-plus (+ T.nanosecond D.nanosecond)))
    (receive (q r)
	(div-and-mod nsec-plus $number-of-nanoseconds-in-a-second)
      (if (negative? r)
	  (make <time> T.type
		(+ sec-plus q -1)
		(+ r $number-of-nanoseconds-in-a-second))
	(make <time> T.type (+ sec-plus q) r)))))

(define (subtract-duration (T <time>) (D <time>))
  (assert (is-a? T <time>))
  (assert (is-a? D <time>))
  (unless (eq? D.type 'time-duration)
    (%time-error 'subtract-duration 'not-duration D))
  (let ((sec-minus  (- T.second     D.second))
	(nsec-minus (- T.nanosecond D.nanosecond)))
    (receive (q r)
	(div-and-mod nsec-minus $number-of-nanoseconds-in-a-second)
      (if (negative? r)
	  (make <time> T.type
		(- sec-minus q 1)
		(+ r $number-of-nanoseconds-in-a-second))
	(make <time> T.type (- sec-minus q) r)))))


;;;; conversion between time types

(define (time-tai->time-utc (T <time>))
  (assert (is-a? T <time>))
  (unless (eq? T.type 'time-tai)
    (%time-error time-tai->time-utc 'incompatible-time-types T))
  (make <time> 'time-utc
	(- T.second (%leap-second-neg-delta T.second))
	T.nanosecond))

(define (time-tai->time-monotonic (T <time>))
  ;;This  depends  on  TIME-MONOTONIC  having  the  same  definition  as
  ;;TIME-TAI.
  ;;
  (assert (is-a? T <time>))
  (unless (eq? T.type 'time-tai)
    (%time-error 'time-tai->time-monotonic 'incompatible-time-types T))
  (make <time> 'time-monotonic T.second T.nanosecond))

;;; --------------------------------------------------------------------

(define (time-utc->time-tai (T <time>))
  (assert (is-a? T <time>))
  (unless (eq? T.type 'time-utc)
    (%time-error 'time-utc->time-tai 'incompatible-time-types T))
  (make <time> 'time-tai
	(+ T.second (%leap-second-delta T.second))
	T.nanosecond))

(define (time-utc->time-monotonic (T <time>))
  (assert (is-a? T <time>))
  (unless (eq? T.type 'time-utc)
    (%time-error 'time-utc->time-monotonic 'incompatible-time-types T))
  (make <time> 'time-monotonic
	(+ T.second (%leap-second-delta T.second))
	T.nanosecond))

;;; --------------------------------------------------------------------

(define (time-monotonic->time-utc (T <time>))
  (assert (is-a? T <time>))
  (unless (eq? T.type 'time-monotonic)
    (%time-error 'time-monotoinc->time-utc 'incompatible-time-types T))
  (make <time> 'time-utc
	(- T.second (%leap-second-neg-delta T.second))
	T.nanosecond))

(define (time-monotonic->time-tai (T <time>))
  ;;This  depends  on  TIME-MONOTONIC  having  the  same  definition  as
  ;;TIME-TAI.
  ;;
  (assert (is-a? T <time>))
  (unless (eq? T.type 'time-monotonic)
    (%time-error 'time-monotonic->time-tai 'incompatible-time-types T))
  (make <time> 'time-tai T.second T.nanosecond))


(define-class <date>
  (nongenerative nausicaa:times-and-dates:<date>)
  (fields (mutable nanosecond)
	  (mutable second)
	  (mutable minute)
	  (mutable hour)
	  (mutable day)
	  (mutable month)
	  (mutable year)
	  (mutable zone-offset))
  (virtual-fields (immutable year-day date-year-day))
  (protocol (lambda (make-top)
	      (lambda (nanosecond second minute hour day month year zone-offset)
		(when (or (< nanosecond 0) (<= $number-of-nanoseconds-in-a-second nanosecond))
		  (assertion-violation 'make-<date>
		    "nanoseconds count out of range, must be [0, 999999999]" nanosecond))

		(when (or (< second 0) (< 60 second))
		  (assertion-violation 'make-<date>
		    "seconds count out of range, must be [0, 60]" second))

		(when (or (< minute 0) (< 59 minute))
		  (assertion-violation 'make-<date>
		    "minutes count out of range, must be [0, 59]" minute))

		(when (or (< hour 0) (< 23 hour))
		  (assertion-violation 'make-<date>
		    "hours count out of range, must be [0, 23]" hour))

		;;*FIXME* Not all months have 31 days.
		(when (or (< day 0) (< 31 day))
		  (assertion-violation 'make-<date>
		    "days count out of range, must be [0, 31]" day))

		(when (or (< month 0) (< 12 month))
		  (assertion-violation 'make-<date>
		    "months count out of range, must be [0, 12]" month))

		(unless (and (integer? year) (exact? year))
		  (assertion-violation 'make-<date>
		    "expected exact integer as year value" year))

		((make-top)
		 nanosecond second minute hour
		 day month year zone-offset)))))


;;;; Julian day stuff

(define (%encode-julian-day-number day month year)
  ;;Return the julian day which starts at noon.
  ;;
  (let* ((a (infix (14 - month) // 12))
	 (y (- (infix year + 4800 - a)
	       (if (negative? year) -1 0)))
	 (m (infix month + 12 * a - 3)))
    (infix day
	   + (153 * m + 2) // 5
	   + 365 * y
	   + y // 4
	   - y // 100
	   + y // 400
	   - 32045)))

(define (%decode-julian-day-number jdn)
  ;;Return 4 values: seconds, date, month, year.
  ;;
  ;;*NOTE* Watch out for precedence of * and // !!!
  ;;
  (let* ((days	(truncate jdn))
	 (a	(infix days + 32044))
	 (b	(infix (4 * a + 3) // 146097))
	 (c	(infix a - (146097 * b) // 4))
	 (d	(infix (4 * c + 3) // 1461))
	 (e	(infix c - (1461 * d) // 4))
	 (m	(infix ((5 * e) + 2) // 153))
	 (y	(infix (100 * b) + d - 4800 + (m // 10))))
    (values	;seconds date month year
     (infix (jdn - days) * $number-of-seconds-in-a-day)
     (infix e + 1 - ((153 * m + 2) // 5))
     (infix m + 3 - 12 * (m // 10))
     (if (>= 0 y) (- y 1) y))))

(define (%time->julian-day-number seconds tz-offset)
  ;; special thing -- ignores nanos
  (infix $tai-epoch-in-jd +
	 ((seconds + tz-offset + $number-of-seconds-in-half-day) / $number-of-seconds-in-a-day)))


;;;; helpers for date manipulation

(define (%string-fractional-part r)
  ;;Given the number R convert  it to string and return everything after
  ;;the decimal dot.  Example:
  ;;
  ;;    (%fractional-part 1.2345) => ".2345"
  ;;
  (define (%string-index char str index len)
    (cond ((>= index len)
	   #f)
	  ((char=? char (string-ref str index))
	   index)
	  (else
	   (%string-index char str (+ index 1) len))))

  (if (integer? r)
      "0"
    ;;The following FORMAT call was originally:
    ;;
    ;;	((str (number->string (inexact r))))
    ;;
    ;;which relied on  the fact that NUMBER->STRING returns  a string in
    ;;fixed point format on many Scheme implementations.
    ;;
    ;;Unfortunately  implementations  like  Ikarus  return a  string  in
    ;;exponential format, which cannot be processed like we need here.
    ;;
    (let* ((str  (format "~f" (inexact r)))
	   (len  (string-length str)))
      (substring str (+ 1 (%string-index #\. str 0 len)) len))))

(define (%local-tz-offset)
  (host:time-gmt-offset (host:current-time)))

(define (%tai-before-leap-second? second)
  (find (lambda (x)
	  (= second (- (+ (car x) (cdr x)) 1)))
	$leap-second-table))

(define (%time->date (T <time>) tz-offset ttype)
  (assert (is-a? T <time>))
  (unless (eq? T.type ttype)
    (%time-error 'time->date 'incompatible-time-types T))
  (receive (secs date month year)
      (%decode-julian-day-number (%time->julian-day-number T.second tz-offset))
    (let* ((hours    (div secs $number-of-seconds-in-an-hour))
	   (rem      (mod secs $number-of-seconds-in-an-hour))
	   (minutes  (div rem 60))
	   (seconds  (mod rem 60)))
      (make <date>
	T.nanosecond seconds
	minutes hours
	date month year
	tz-offset))))


;;;; date conversion

(define time-tai->date
  (case-lambda
   ((time)
    (time-tai->date time (%local-tz-offset)))
   ((time tz-offset)
    (if (%tai-before-leap-second? (time-second time))
	;;If  it's  *right* before  the  leap,  we  need to  pretend  to
	;;subtract a second ...
	(let ((d (%time->date
		  (subtract-duration (time-tai->time-utc time)
				     (make <time> time-duration 1 0))
		  tz-offset time-utc)))
	  (<date>-second-set! d 60)
	  d)
      (%time->date (time-tai->time-utc time) tz-offset time-utc)))))

(define time-utc->date
  (case-lambda
   ((time)
    (time-utc->date time (%local-tz-offset)))
   ((time tz-offset)
    (%time->date time tz-offset time-utc))))

;;Again, time-monotonic is the same as time TAI
(define time-monotonic->date
  (case-lambda
   ((time)
    (time-monotonic->date time (%local-tz-offset)))
   ((time tz-offset)
    (%time->date time tz-offset time-monotonic))))

(define (date->time-utc (D <date>))
  (let ((jdays (- (%encode-julian-day-number D.day D.month D.year)
		  $tai-epoch-in-jd)))
    (make <time> 'time-utc
	  (infix ((jdays - 1/2) * 24 + D.hour) * $number-of-seconds-in-an-hour
		 + D.minute * 60
		 + D.second
		 - D.zone-offset)
	  D.nanosecond)))

(define-constant $one-second
  (make <time> 'time-duration 1 0))

(define (date->time-tai (D <date>))
  (let ((T (time-utc->time-tai (date->time-utc D))))
    (if (= 60 D.second)
	(subtract-duration T $one-second)
      T)))

(define (date->time-monotonic date)
  (time-utc->time-monotonic (date->time-utc date)))


;;;; date manipulation

(define (%leap-year? year)
  ;;Return true if YEAR is a leap year.
  ;;
  (or (= (mod year 400) 0)
      (and (= (mod year 4) 0)
	   (not (= (mod year 100) 0)))))

(define (leap-year? (D <date>))
  ;;Return true if D in in a leap year.
  ;;
  (%leap-year? D.year))

(define (%year-day day month year)
  ;;Return the  number of days  from the beginning  of the year  for the
  ;;specified date.  Assume that the given date is correct.
  ;;
  (+ (vector-ref (if (%leap-year? year)
		     $number-of-days-the-first-day-of-each-month/leap-year
		   $number-of-days-the-first-day-of-each-month/non-leap-year)
		 month)
     day))

(define (date-year-day (D <date>))
  (%year-day D.day D.month D.year))

(define (%week-day day month year)
  ;;Return the index  of the day in its week,  zero based: Sun=0, Mon=1,
  ;;Tue=2, etc.  Assume the given date is correct.
  ;;
  ;;From the calendar FAQ.
  ;;
  (let* ((a (infix (14 - month) // 12))
	 (y (infix year - a))
	 (m (infix month + (12 * a) - 2)))
    (infix (day + y + (y // 4) - (y // 100) + (y // 400) + ((31 * m) // 12)) % 7)))

(define (date-week-day (D <date>))
  ;;Return the index  of the day in its week,  zero based: Sun=0, Mon=1,
  ;;Tue=2, etc.
  ;;
  (%week-day D.day D.month D.year))

(define (%days-before-first-week (D <date>) day-of-week-starting-week)
  ;;Return the number of days before  the first day of the first week of
  ;;the year in D.
  ;;
  ;;DAY-OF-WEEK-STARTING-WEEK  selects which  day starts  a week:  0 for
  ;;Sunday, 1 for Monday, 2 for Tuesday, etc.
  ;;
  (let ((index-of-day-in-week (%week-day 1 1 D.year)))
    (infix (day-of-week-starting-week - index-of-day-in-week) % 7)))

(define (date-week-number (D <date>) day-of-week-starting-week)
  (let ((x (%days-before-first-week D day-of-week-starting-week)))
    (infix (D.year-day - x) // 7)))

(define current-date
  ;;Return a <date> object representin "now".
  ;;
  (case-lambda
   (()
    (current-date (%local-tz-offset)))
   ((tz-offset)
    (time-utc->date (current-time time-utc) tz-offset))))

(define (%natural-year n)
  ;;Given a 'two digit' number, find the year within 50 years +/-.
  ;;
  (let* ((current-year    (<date>-year (current-date)))
	 (current-century (infix (current-year // 100) * 100)))
    (cond ((>= n 100) n)
	  ((<  n 0)   n)
	  ((infix (current-century + n - current-year) <= 50)
	   (+ current-century n))
	  (else
	   (infix current-century - 100 + n)))))

(define (date->julian-day (D <date>))
  ;;Return the Julian day representing D.
  ;;
  (+ (%encode-julian-day-number D.day D.month D.year)
     (- 1/2)
     (/ (/ (+ (* D.hour $number-of-seconds-in-an-hour)
	      (* D.minute 60)
	      D.second
	      (div D.nanosecond $number-of-nanoseconds-in-a-second))
	   $number-of-seconds-in-a-day)
	(- D.zone-offset))))

(define (date->modified-julian-day D)
  ;;Return the Julian day representing D.
  ;;
  (- (date->julian-day D) 4800001/2))

(define (time-utc->julian-day (T <time>))
  ;;Return the Julian day representing T.
  ;;
  (unless (eq? T.type 'time-utc)
    (%time-error 'time-utc->julian-day 'incompatible-time-types T))
  (infix $tai-epoch-in-jd +
	 ((T.second + T.nanosecond / $number-of-nanoseconds-in-a-second) / $number-of-seconds-in-a-day)))

(define (time-utc->modified-julian-day T)
  ;;Return the Modified Julian day representing T.
  ;;
  (infix time-utc->julian-day (T) - 4800001/2))

(define (time-tai->julian-day (T <time>))
  ;;Return the Julian day representing T.
  ;;
  (unless (eq? T.type 'time-tai)
    (%time-error 'time-tai->julian-day 'incompatible-time-types T))
  (infix $tai-epoch-in-jd +
	 (T.second - %leap-second-delta (T.second) + T.nanosecond / $number-of-nanoseconds-in-a-second)
	 / $number-of-seconds-in-a-day))

(define (time-tai->modified-julian-day (T <time>))
  ;;Return the Modified Julian day representing T.
  ;;
  (infix time-tai->julian-day (T) - 4800001/2))

(define (time-monotonic->julian-day (T <time>))
  ;;Return  the  Julian  day  representing  T.   This  is  the  same  as
  ;;TIME-TAI->JULIAN-DAY.
  ;;
  (unless (eq? T.type 'time-monotonic)
    (%time-error 'time-monotonic->julian-day 'incompatible-time-types T))
  (infix $tai-epoch-in-jd +
	 (T.second - %leap-second-delta (T.second) + T.nanosecond / $number-of-nanoseconds-in-a-second)
	 / $number-of-seconds-in-a-day))

(define (time-monotonic->modified-julian-day (T <time>))
  ;;Return the Modified Julian day representing T.
  ;;
  (infix time-monotonic->julian-day (T) - 4800001/2))

(define (julian-day->time-utc jdn)
  ;;Return a <time> object in UTC format representing JDN.
  ;;
  (let ((nanosecs (infix $number-of-nanoseconds-in-a-second * $number-of-seconds-in-a-day
			 * (jdn - $tai-epoch-in-jd))))
    (make <time> time-utc
	  (floor (/ nanosecs $number-of-nanoseconds-in-a-second))
	  (mod nanosecs $number-of-nanoseconds-in-a-second))))

(define (julian-day->time-tai jdn)
  ;;Return a <time> object in TAI format representing JDN.
  ;;
  (time-utc->time-tai (julian-day->time-utc jdn)))

(define (julian-day->time-monotonic jdn)
  ;;Return a <time> object in Monotonic format representing JDN.
  ;;
  (time-utc->time-monotonic (julian-day->time-utc jdn)))

(define julian-day->date
  ;;Return a <date> object representing JDN in the selected time zone.
  ;;
  (case-lambda
   ((jdn tz-offset)
    (time-utc->date (julian-day->time-utc jdn) tz-offset))
   ((jdn)
    (julian-day->date (%local-tz-offset)))))

(define modified-julian-day->date
  ;;Return a <date> object representing JDN in the selected time zone.
  ;;
  (case-lambda
   ((jdn tz-offset)
    (julian-day->date (+ jdn 4800001/2) tz-offset))
   ((jdn)
    (modified-julian-day->date jdn (%local-tz-offset)))))

(define (modified-julian-day->time-utc jdn)
  ;;Return a <time> object in UTC format representing JDN.
  ;;
  (julian-day->time-utc (+ jdn 4800001/2)))

(define (modified-julian-day->time-tai jdn)
  ;;Return a <time> object in TAI format representing JDN.
  ;;
  (julian-day->time-tai (+ jdn 4800001/2)))

(define (modified-julian-day->time-monotonic jdn)
  ;;Return a <time> object in Monotonic format representing JDN.
  ;;
  (julian-day->time-monotonic (+ jdn 4800001/2)))

(define (current-julian-day)
  ;;Return a Julian Day value representing the current time.
  ;;
  (time-utc->julian-day (current-time time-utc)))

(define (current-modified-julian-day)
  ;;Return a Modified Julian Day value representing the current time.
  ;;
  (time-utc->modified-julian-day (current-time time-utc)))


(define date->string
  ;;Convert D to string according to the template string in FORMAT.
  ;;
  (case-lambda
   ((D)
    (date->string D "~c"))
   ((D format)

    (define (main (D <date>) (format <string>))
      (receive (port getter)
	  (open-string-output-port)
	(let next-char ((index 0))
	  (if (>= index format.length)
	      (values)
	    (let ((current-char	(string-ref format index))
		  (index		(+ 1 index)))

	      (define (%error-no-semantic-char index)
		(when (= index format.length)
		  (error 'date->string
		    "no semantic character after pad character in date format directive" format)))

	      (define (format-with-pad-char pad-char index)
		(let ((formatter (%get-formatter (string-ref format index))))
		  (if (not formatter)
		      (%time-error 'date->string 'bad-date-format format)
		    (begin
		      (formatter D pad-char port)
		      (next-char (+ 1 index))))))

	      (cond ((not (char=? current-char $escape-char))
		     (display current-char port)
		     (next-char index))
		    ((= index format.length)
		     (error 'date->string "date template string cannot end with escape character" format))
		    (else
		     (case (getf (format index))
		       ((#\-)
			(%error-no-semantic-char index)
			(format-with-pad-char #f (+ 1 index)))
		       ((#\_)
			(%error-no-semantic-char index)
			(format-with-pad-char #\space (+ 1 index)))
		       (else
			(format-with-pad-char #\0 index)))))
	      )))
	(getter)))

    (define (%get-formatter directive-char)
      (vector-ref $write-directives (char->integer directive-char)))

    (define (%last-n-digits i n)
      (abs (mod i (expt 10 n))))

    (define-inline (%locale-abbr-weekday n)
      (vector-ref $locale-abbr-weekday-vector n))

    (define-inline (%locale-long-weekday n)
      (vector-ref $locale-long-weekday-vector n))

    (define-inline (%locale-abbr-month n)
      (vector-ref $locale-abbr-month-vector n))

    (define-inline (%locale-long-month n)
      (vector-ref $locale-long-month-vector n))

    (define (%locale-am/pm hr)
      ;;Locale specific.
      (if (> hr 11) $locale-pm $locale-am))

    (define (%tz-printer offset port)
      (%display (cond ((zero? offset)	#\Z)
		      ((negative? offset)	#\-)
		      (else			#\+)))
      (unless (zero? offset)
	(receive (d m)
	    (div-and-mod offset $number-of-seconds-in-an-hour)
	  (let ((hours   (abs d))
		(minutes (abs (div m 60))))
	    (%display (%padding hours   #\0 2))
	    (%display (%padding minutes #\0 2))))))

    (define (%locale-print-time-zone date port)
      ;;*FIXME* It should print the time zone in symbolic form (for example:
      ;;Rome).  Take a look under the "/usr/share/zoneinfo" directory.
      ;;
      (values))

    (define-constant $write-directives
      ;;A table of output formatting directives.
      ;;
      ;;The  cars  are  the   format  directive  characters,  the  cdrs  are
      ;;procedures that take  the date, a padding character  (which might be
      ;;#f), and the output port.
      ;;
      (begin0-let ((table (make-vector 127 #f)))
	(for-each (lambda ((p <pair>))
		    (vector-set! table (char->integer p.car) p.cdr))
	  `((#\~ . ,(lambda (date pad-char port) (%display #\~)))

	    (#\a . ,(lambda (date pad-char port)
		      (%display (%locale-abbr-weekday (date-week-day date)))))

	    (#\A . ,(lambda (date pad-char port)
		      (%display (%locale-long-weekday (date-week-day date)))))

	    (#\b . ,(lambda ((D <date>) pad-char port)
		      (%display (%locale-abbr-month D.month))))

	    (#\B . ,(lambda ((D <date>) pad-char port)
		      (%display (%locale-long-month D.month))))

	    (#\c . ,(lambda (date pad-char port)
		      (%display (date->string date $locale-date-time-format))))

	    (#\d . ,(lambda ((D <date>) pad-char port)
		      (%display (%padding D.day #\0 2))))

	    (#\D . ,(lambda (date pad-char port)
		      (%display (date->string date "~m/~d/~y"))))

	    (#\e . ,(lambda ((D <date>) pad-char port)
		      (%display (%padding D.day #\space 2))))

	    (#\f . ,(lambda ((D <date>) pad-char port)
		      (%display (%padding (if (< $number-of-nanoseconds-in-a-second D.nanosecond)
					      (+ 1 D.second)
					    D.second)
					  pad-char 2))
		      (%display $locale-number-separator)
		      (%display (%string-fractional-part
				 (/ D.nanosecond $number-of-nanoseconds-in-a-second)))))

	    (#\h . ,(lambda (date pad-char port)
		      (%display (date->string date "~b"))))

	    (#\H . ,(lambda ((D <date>) pad-char port)
		      (%display (%padding D.hour pad-char 2))))

	    (#\I . ,(lambda ((D <date>) pad-char port)
		      (%display (%padding (if (> D.hour 12)
					      (- D.hour 12)
					    D.hour)
					  pad-char 2))))

	    (#\j . ,(lambda (date pad-char port)
		      (%display (%padding (date-year-day date) pad-char 3))))

	    (#\k . ,(lambda ((D <date>) pad-char port)
		      (%display (%padding D.hour #\space 2))))

	    (#\l . ,(lambda ((D <date>) pad-char port)
		      (%display (%padding (if (> D.hour 12)
					      (- D.hour 12)
					    D.hour)
					  #\space 2))))

	    (#\m . ,(lambda ((D <date>) pad-char port)
		      (%display (%padding D.month pad-char 2))))

	    (#\M . ,(lambda ((D <date>) pad-char port)
		      (%display (%padding D.minute pad-char 2))))

	    (#\n . ,(lambda (date pad-char port)
		      (newline port)))

	    (#\N . ,(lambda ((D <date>) pad-char port)
		      (%display (%padding D.nanosecond pad-char 9))))

	    (#\p . ,(lambda ((D <date>) pad-char port)
		      (%display (%locale-am/pm D.hour))))

	    (#\r . ,(lambda (date pad-char port)
		      (%display (date->string date "~I:~M:~S ~p"))))

	    (#\s . ,(lambda (date pad-char port)
		      (%display (time-second (date->time-utc date)))))

	    (#\S . ,(lambda ((D <date>) pad-char port)
		      (%display (%padding (if (> D.nanosecond $number-of-nanoseconds-in-a-second)
					      (+ 1 D.second)
					    D.second)
					  pad-char 2))))

	    (#\t . ,(lambda (date pad-char port)
		      (%display (integer->char 9))))

	    (#\T . ,(lambda (date pad-char port)
		      (%display (date->string date "~H:~M:~S"))))

	    (#\U . ,(lambda ((D <date>) pad-char port)
		      (%display (let ((week-number (date-week-number D 0)))
				  (%padding (if (> (%days-before-first-week D 0) 0)
						(+ week-number 1)
					      week-number)
					    #\0 2)))))

	    (#\V . ,(lambda (date pad-char port)
		      (%display (%padding (date-week-number date 1) #\0 2))))

	    (#\w . ,(lambda (date pad-char port)
		      (%display (date-week-day date))))

	    (#\x . ,(lambda (date pad-char port)
		      (%display (date->string date $locale-short-date-format))))

	    (#\X . ,(lambda (date pad-char port)
		      (%display (date->string date $locale-time-format))))

	    (#\W . ,(lambda (D pad-char port)
		      (let ((week-number (date-week-number D 1)))
			(%display (%padding (if (> (%days-before-first-week D 1) 0)
						(+ 1 week-number)
					      week-number)
					    #\0 2)))))

	    (#\y . ,(lambda ((D <date>) pad-char port)
		      (%display (%padding (%last-n-digits D.year 2) pad-char 2))))

	    (#\Y . ,(lambda ((D <date>) pad-char port)
		      (%display D.year)))

	    (#\z . ,(lambda ((D <date>) pad-char port)
		      (%tz-printer D.zone-offset port)))

	    (#\Z . ,(lambda (date pad-char port)
		      (%locale-print-time-zone date port)))

	    (#\1 . ,(lambda (date pad-char port)
		      (%display (date->string date "~Y-~m-~d"))))

	    (#\2 . ,(lambda (date pad-char port)
		      (%display (date->string date "~H:~M:~S~z"))))

	    (#\3 . ,(lambda (date pad-char port)
		      (%display (date->string date "~H:~M:~S"))))

	    (#\4 . ,(lambda (date pad-char port)
		      (%display (date->string date "~Y-~m-~dT~H:~M:~S~z"))))

	    (#\5 . ,(lambda (date pad-char port)
		      (%display (date->string date "~Y-~m-~dT~H:~M:~S"))))
	    ))))

    (define (%padding n pad-char requested-length)
      ;;Return  a   string  representation  of  number   N,  of  minimum
      ;;REQUESTED-LENGTH, padded  with character PAD-CHAR.   If PAD-CHAR
      ;;is #f,  no padding  is done, and  it's as if  NUMBER->STRING was
      ;;used.   If string is  longer than  REQUESTED-LENGTH, it's  as if
      ;;NUMBER->STRING was used.
      ;;
      (let* ((str     (number->string n))
	     (str.len (string-length str)))
	(if (or (< requested-length str.len) (not pad-char))
	    str
	  (let* ((new-str        (make-string requested-length pad-char))
		 (new-str-offset (- (string-length new-str) str.len)))
	    (do ((i 0 (+ i 1)))
		((>= i str.len)
		 new-str)
	      (string-set! new-str (+ new-str-offset i) (string-ref str i)))))))

    (main D format))))


(define (string->date input-string (template-string <string>))
  (define who 'string->date)

  (define (main)
    (let (((D <date>)	(make-from-fields <date> 0 0 0 0 #f #f #f (%local-tz-offset)))
	  (port		(open-string-input-port input-string)))
      (let loop ((index 0))
	(if (>= index template-string.length)
	    (if (and D.nanosecond D.second D.minute D.hour D.day D.month D.year D.zone-offset)
		D
	      (error who "bad date format string, incomplete date read" D template-string))
	  (let ((template-char (string-ref template-string index)))
	    (if (%escape-char? template-char)
		(let ((index (+ 1 index))) ;process directive
		  (when (> index template-string.length)
		    (error who
		      "end of date template string while looking for format directive char"
		      template-string))
		  (let* ((directive-char (string-ref template-string index))
			 ((info <format-directive>)
			  (vector-ref $read-directives (char->integer directive-char))))
		    (unless info
		      (error who
			"unknown escape sequence in date template string"
			template-string (string $escape-char directive-char)))
		    (%skip-until port info.skipper)
		    (info.store (info.reader port) D)
		    (loop (+ 1 index))))
	      (let ((in-char (read-char port)))	;process non-directive input char
		(cond ((eof-object? in-char)
		       (error who "date template string shorter than date input string" template-string))
		      ((not (char=? template-char in-char))
		       (error who
			 (string-append "mismatch between template char " (string template-char)
					" (offset " (number->string index) ") and input char"
					(string in-char))
			 template-char))
		      (else
		       (loop (+ 1 index)))))
	      ))))
      ))

  (define (%skip-until port skipper)
    ;;Consume chars from PORT until  SKIPPER returns true on a char; the
    ;;"true char" is not discarded.
    ;;
    (let ((ch (peek-char port)))
      (cond ((eof-object? ch)
	     (%time-error who 'bad-date-format-string template-string))
	    ((not (skipper ch))
	     (read-char port) ;discard char
	     (%skip-until port skipper)))))

  (define (%vector-find needle haystack comparator)
    (let loop ((index 0))
      (cond ((>= index (vector-length haystack))
	     #f)
	    ((comparator needle (vector-ref haystack index))
	     index)
	    (else
	     (loop (+ 1 index))))))

  (define (%locale-abbr-weekday->index string)
    (or (%vector-find string $locale-abbr-weekday-vector string=?)
	(error who "unrecognised abbreviated week day name" string)))

  (define (%locale-long-weekday->index string)
    (or (%vector-find string $locale-long-weekday-vector string=?)
	(error who "unrecognised full week day name" string)))

  (define (%locale-abbr-month->index string)
    (or (%vector-find string $locale-abbr-month-vector string=?)
	(error who "unrecognised abbreviated month name" string)))

  (define (%locale-long-month->index string)
    (or (%vector-find string $locale-long-month-vector string=?)
	(error who "unrecognised full month name" string)))

  (define (%char->int ch)
    (case ch
      ((#\0) 0)
      ((#\1) 1)
      ((#\2) 2)
      ((#\3) 3)
      ((#\4) 4)
      ((#\5) 5)
      ((#\6) 6)
      ((#\7) 7)
      ((#\8) 8)
      ((#\9) 9)
      (else
       (%time-error who 'bad-date-template-string
		    (list "Non-integer character" ch )))))

  (define (%make-max-width-integer-reader upto)
    (define (%integer-reader upto port)
      ;;Read an integer up to UPTO  characters long on port; if UPTO is #f
      ;;read any length.
      (let loop ((accum 0) (nchars 0))
	(let ((ch (peek-char port)))
	  (if (or (eof-object? ch)
		  (not (char-numeric? ch))
		  (and upto (>= nchars upto)))
	      accum
	    (loop (+ (* accum 10) (%char->int (read-char port))) (+ 1 nchars))))))
    (lambda (port)
      (%integer-reader upto port)))

  (define (%make-fractional-integer-reader upto)
    (define (%fractional-integer-reader upto port)
      ;;Read a fractional integer up to UPTO characters long on port; if
      ;;UPTO is #f read until EOF or non-numeric char.
      ;;
      ;;The  return value  is normalized  to UPTO  decimal  places.  For
      ;;example, if UPTO  is 9 and the string read  is "123", the return
      ;;value is 123000000.
      ;;
      (let loop ((accum 0) (nchars 0))
	(let ((ch (peek-char port)))
	  (if (or (eof-object? ch)
		  (not (char-numeric? ch))
		  (and upto (>= nchars upto)))
	      (* accum (expt 10 (- upto nchars)))
	    (loop (+ (* accum 10) (%char->int (read-char port))) (+ nchars 1))))))
    (lambda (port)
      (%fractional-integer-reader upto port)))

  (define (%make-fixed-width-integer-exact-reader n)
    (define (%integer-reader-exact n port)
      ;;Read  *exactly* N characters  and convert  to integer;  could be
      ;;padded.
      ;;
      (let ((padding-ok #t))
	(let loop ((accum 0) (nchars 0))
	  (let ((ch (peek-char port)))
	    (cond ((>= nchars n) accum)
		  ((eof-object? ch)
		   (%time-error who 'bad-date-template-string
				"Premature ending to integer read."))
		  ((char-numeric? ch)
		   (set! padding-ok #f)
		   (loop (+ (* accum 10) (%char->int (read-char port)))
			 (+ nchars 1)))
		  (padding-ok
		   (read-char port) ;consume padding
		   (loop accum (+ nchars 1)))
		  (else ;padding where it shouldn't be
		   (%time-error who 'bad-date-template-string
				"Non-numeric characters in integer read.")))))))
    (lambda (port)
      (%integer-reader-exact n port)))

  (define (%zone-reader port)
    ;;Read a time zone specification  from PORT and return the time zone
    ;;offset in seconds.  A time zone spec can be one of the following:
    ;;
    ;;	Z
    ;;	z
    ;;	+XYZT
    ;;	-XYZT
    ;;
    ;;where XY is the hours offset and ZT is the minutes offset.
    ;;
    (define (%error-unexpected-eof)
      (error who "unexpected end of input while reading time zone number"))
    (let ((ch (read-char port)))
      (cond ((eof-object? ch)
	     (%error-unexpected-eof))
	    ((or (char=? ch #\Z) (char=? ch #\z))
	     0)
	    (else
	     (let ((offset-in-seconds 0)
		   (positive?	(case ch
				  ((#\+) #t)
				  ((#\-) #f)
				  (else
				   (error who "missing offset sign while reading time zone number"))))
		   (%next-digit	(lambda ()
				  (let ((ch (read-char port)))
				    (if (eof-object? ch)
					(%error-unexpected-eof)
				      (%char->int ch))))))
	       ;;We have to enforce the order of %NEXT-DIGIT evaluations!!!
	       (set!  offset-in-seconds (* (%next-digit) 10 $number-of-seconds-in-an-hour))
	       (incr! offset-in-seconds (* (%next-digit)    $number-of-seconds-in-an-hour))
	       (incr! offset-in-seconds (* (%next-digit) 10 $number-of-seconds-in-a-minute))
	       (incr! offset-in-seconds (* (%next-digit)    $number-of-seconds-in-a-minute))
	       (if positive?
		   offset-in-seconds
		 (- offset-in-seconds)))))))

  (define (%make-locale-reader indexer-proc)
    ;;Return  a  closure  accepting  an  string  input  port  as  single
    ;;argument; the closure should be  applied to a port to read textual
    ;;date/time  elements and use  INDEXER-PROC to  convert them  to the
    ;;corresponding field for a <date> object.
    ;;
    ;;INDEXER-PROC is  meant to be  customisable for a  selected locale;
    ;;for example, when reading  full month names: given "January" under
    ;;the en_GB  locale must return  1, given "Gennaio" under  the it_IT
    ;;locale must return 1.
    ;;
    ;;If INDEXER-PROC does not recognise  the input string, it must take
    ;;care of raising an appropriate exception.
    ;;
    (define (%locale-reader port indexer-proc)
      ;;Read  and accumulate alphabetic  chars from  PORT; then  map the
      ;;resulting string with INDEXER-PROC and return the result.
      ;;
      (receive (string-port get-output-string)
	  (open-string-output-port)
	(indexer-proc (let read-char-string ((ch (peek-char port)))
			(cond ((eof-object? ch)
			       (get-output-string))
			      ((char-alphabetic? ch)
			       (write-char (read-char port) string-port)
			       (read-char-string (peek-char port)))
			      (else
			       (get-output-string)))))))
    (lambda (port)
      (%locale-reader port indexer-proc)))

  (define (%make-char-id-reader char)
    ;;Return  a procedure  which reads  CHAR from  a port  or  raises an
    ;;error.
    ;;
    (lambda (port)
      (let ((ch (read-char port)))
	(cond ((eof-object? ch)
	       (error 'date->string
		 (string-append "unexpected end of input string while reading" (string char))))
	      ((char=? char ch)
	       char)
	      (else
	       (error 'date->string
		 "expected " (string char) "character got " (string ch)))))))

  (define-class <format-directive>
    (fields (immutable char)
		;The character directive coming after the escape char.
	    (immutable skipper)
		;Character  predicate returning #t  when a  character is
		;good as first character  of this directive's value.  It
		;is used to skip characters from the input.
	    (immutable reader)
		;A  procedure which,  applied to  a textual  input port,
		;reads  the value  for this  directive (for  example the
		;full month name in  the selected locale).  If the input
		;from  the port does  not match  the desired  value: the
		;reader must raise an appropriate exception.
	    (immutable store))
		;An action procedure, that takes the value (from READER)
		;and some object (here,  always the date) and (probably)
		;side-effects it.   In some cases (e.g.,  ~A) the action
		;is to do nothing.
    (nongenerative nausicaa:times-and-dates:<format-directive>))

  (define-constant $read-directives
    (begin0-let ((table		(make-vector 127 #f))
		 (ireader4	(%make-max-width-integer-reader 4))
		 (ireader2	(%make-max-width-integer-reader 2))
		 (fireader9	(%make-fractional-integer-reader 9))
		 (ireaderf	(%make-max-width-integer-reader #f))
		 (eireader2	(%make-fixed-width-integer-exact-reader 2))
		 (eireader4	(%make-fixed-width-integer-exact-reader 4))
		 (locale-reader-abbr-weekday (%make-locale-reader %locale-abbr-weekday->index))
		 (locale-reader-long-weekday (%make-locale-reader %locale-long-weekday->index))
		 (locale-reader-abbr-month   (%make-locale-reader %locale-abbr-month->index))
		 (locale-reader-long-month   (%make-locale-reader %locale-long-month->index))
		 (char-fail (lambda (ch) #t))
		 (do-nothing (lambda (val object) (values))))
      (for-each (lambda ((o <format-directive>))
		  (vector-set! table (char->integer o.char) o))
	`(,(make <format-directive>
	     #\~ char-fail (%make-char-id-reader #\~) do-nothing)

	  ,(make <format-directive>
	     #\a char-alphabetic?	locale-reader-abbr-weekday do-nothing)

	  ,(make <format-directive>
	     #\A char-alphabetic?	locale-reader-long-weekday do-nothing)

	  ,(make <format-directive>
	     #\b char-alphabetic?	locale-reader-abbr-month
	     (lambda (val (D <date>)) (set! D.month val)))

	  ,(make <format-directive>
	     #\B char-alphabetic?	locale-reader-long-month
	     (lambda (val (D <date>)) (set! D.month val)))

	  ,(make <format-directive> #\d char-numeric? ireader2
		 (lambda (val (D <date>)) (set! D.day val)))

	  ,(make <format-directive> #\e char-fail eireader2
		 (lambda (val (D <date>)) (set! D.day val)))

	  ,(make <format-directive> #\h char-alphabetic? locale-reader-abbr-month
		 (lambda (val (D <date>)) (set! D.month val)))

	  ,(make <format-directive> #\H char-numeric? ireader2
		 (lambda (val (D <date>)) (set! D.hour val)))

	  ,(make <format-directive> #\k char-fail eireader2
		 (lambda (val (D <date>)) (set! D.hour val)))

	  ,(make <format-directive> #\m char-numeric? ireader2
		 (lambda (val (D <date>)) (set! D.month val)))

	  ,(make <format-directive> #\M char-numeric? ireader2
		 (lambda (val (D <date>)) (set! D.minute val)))

	  ,(make <format-directive> #\N char-numeric? fireader9
		 (lambda (val (D <date>)) (set! D.nanosecond val)))

	  ,(make <format-directive> #\S char-numeric? ireader2
		 (lambda (val (D <date>)) (set! D.second val)))

	  ,(make <format-directive> #\y char-fail eireader2
		 (lambda (val (D <date>)) (set! D.year  (%natural-year val))))

	  ,(make <format-directive> #\Y char-numeric? ireader4
		 (lambda (val (D <date>)) (set! D.year val)))

	  ,(make <format-directive> #\z
		 (lambda (c) (memv c '(#\Z #\z #\+ #\-)))
		 %zone-reader
		 (lambda (val (D <date>)) (set! D.zone-offset val)))
	  ))))

  (main))


;;;; done

)

;;; end of file
;;Local Variables:
;;coding: utf-8-unix
;;End:
