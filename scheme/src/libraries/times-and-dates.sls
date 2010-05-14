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


;;; helpers

(define-syntax read-line
  (syntax-rules ()
    ((_)
     (get-line (current-input-port)))
    ((_ ?port)
     (get-line ?port))))


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
  (case-lambda
   (()
    (current-date (%local-tz-offset)))
   ((tz-offset)
    (time-utc->date (current-time time-utc) tz-offset))))

(define (tm:natural-year n)
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
  (+ (%encode-julian-day-number D.day D.month D.year)
     (- 1/2)
     (/ (/ (+ (* D.hour $number-of-seconds-in-an-hour)
	      (* D.minute 60)
	      D.second
	      (div D.nanosecond $number-of-nanoseconds-in-a-second))
	   $number-of-seconds-in-a-day)
	(- D.zone-offset))))

(define (date->modified-julian-day date)
  (- (date->julian-day date) 4800001/2))

(define (time-utc->julian-day time)
  (if (not (eq? (time-type time) time-utc))
      (%time-error 'time-utc->julian-day 'incompatible-time-types  time)
    (+ (/ (+ (time-second time) (/ (time-nanosecond time) $number-of-nanoseconds-in-a-second))
	  $number-of-seconds-in-a-day)
       $tai-epoch-in-jd)))

(define (time-utc->modified-julian-day time)
  (- (time-utc->julian-day time)
     4800001/2))

(define (time-tai->julian-day time)
  (if (not (eq? (time-type time) time-tai))
      (%time-error 'time-tai->julian-day 'incompatible-time-types  time)
    (+ (/ (+ (- (time-second time)
		(%leap-second-delta (time-second time)))
	     (/ (time-nanosecond time) $number-of-nanoseconds-in-a-second))
	  $number-of-seconds-in-a-day)
       $tai-epoch-in-jd)))

(define (time-tai->modified-julian-day time)
  (- (time-tai->julian-day time)
     4800001/2))

;; this is the same as time-tai->julian-day
(define (time-monotonic->julian-day time)
  (if (not (eq? (time-type time) time-monotonic))
      (%time-error 'time-monotonic->julian-day 'incompatible-time-types  time)
    (+ (/ (+ (- (time-second time)
		(%leap-second-delta (time-second time)))
	     (/ (time-nanosecond time) $number-of-nanoseconds-in-a-second))
	  $number-of-seconds-in-a-day)
       $tai-epoch-in-jd)))

(define (time-monotonic->modified-julian-day time)
  (- (time-monotonic->julian-day time)
     4800001/2))

(define (julian-day->time-utc jdn)
  (let ((nanosecs (* $number-of-nanoseconds-in-a-second $number-of-seconds-in-a-day
		     (- jdn $tai-epoch-in-jd))))
    (make <time> time-utc
	  (floor (/ nanosecs $number-of-nanoseconds-in-a-second))
	  (mod nanosecs $number-of-nanoseconds-in-a-second))))

(define (julian-day->time-tai jdn)
  (time-utc->time-tai (julian-day->time-utc jdn)))

(define (julian-day->time-monotonic jdn)
  (time-utc->time-monotonic (julian-day->time-utc jdn)))

(define julian-day->date
  (case-lambda
   ((jdn offset)
    (time-utc->date (julian-day->time-utc jdn) offset))
   ((jdn)
    (julian-day->date (%local-tz-offset)))))

(define modified-julian-day->date
  (case-lambda
   ((jdn tz-offset)
    (julian-day->date (+ jdn 4800001/2) tz-offset))
   ((jdn)
    (modified-julian-day->date jdn (%local-tz-offset)))))

(define (modified-julian-day->time-utc jdn)
  (julian-day->time-utc (+ jdn 4800001/2)))

(define (modified-julian-day->time-tai jdn)
  (julian-day->time-tai (+ jdn 4800001/2)))

(define (modified-julian-day->time-monotonic jdn)
  (julian-day->time-monotonic (+ jdn 4800001/2)))

(define (current-julian-day)
  (time-utc->julian-day (current-time time-utc)))

(define (current-modified-julian-day)
  (time-utc->modified-julian-day (current-time time-utc)))


;;;; string conversion stuff

(define (tm:padding n pad-with length)
  ;;Return  a string  representation  of number  N,  of minimum  LENGTH,
  ;;padded with  character PAD-WITH.  If  PAD-WITH if #f, no  padding is
  ;;done, and it's  as if number->string was used.   If string is longer
  ;;than LENGTH, it's as if number->string was used.
  ;;
  (let* ((str     (number->string n))
	 (str-len (string-length str)))
    (if (or (> str-len length)
	    (not pad-with))
	str
      (let* ((new-str        (make-string length pad-with))
	     (new-str-offset (- (string-length new-str)
				str-len)))
	(do ((i 0 (+ i 1)))
	    ((>= i (string-length str)))
	  (string-set! new-str (+ new-str-offset i)
		       (string-ref str i)))
	new-str))))

(define (tm:last-n-digits i n)
  (abs (mod i (expt 10 n))))

(define (tm:locale-abbr-weekday n)
  (vector-ref $locale-abbr-weekday-vector n))

(define (tm:locale-long-weekday n)
  (vector-ref $locale-long-weekday-vector n))

(define (tm:locale-abbr-month n)
  (vector-ref $locale-abbr-month-vector n))

(define (tm:locale-long-month n)
  (vector-ref $locale-long-month-vector n))

(define (tm:vector-find needle haystack comparator)
  (let ((len (vector-length haystack)))
    (define (tm:vector-find-int index)
      (cond
       ((>= index len) #f)
       ((comparator needle (vector-ref haystack index)) index)
       (else (tm:vector-find-int (+ index 1)))))
    (tm:vector-find-int 0)))

(define (tm:locale-abbr-weekday->index string)
  (tm:vector-find string $locale-abbr-weekday-vector string=?))

(define (tm:locale-long-weekday->index string)
  (tm:vector-find string $locale-long-weekday-vector string=?))

(define (tm:locale-abbr-month->index string)
  (tm:vector-find string $locale-abbr-month-vector string=?))

(define (tm:locale-long-month->index string)
  (tm:vector-find string $locale-long-month-vector string=?))

;; do nothing.
;; Your implementation might want to do something...
;;
(define (tm:locale-print-time-zone date port)
  (values))

;; Again, locale specific.
(define (%locale-am/pm hr)
  (if (> hr 11) $locale-pm $locale-am))

(define (tm:tz-printer offset port)
  (cond ((zero? offset)		(display "Z" port))
	((negative? offset)	(display "-" port))
	(else			(display "+" port)))
  (if (not (= offset 0))
      (let ((hours   (abs (div offset $number-of-seconds-in-an-hour)))
	    (minutes (abs (div (mod offset $number-of-seconds-in-an-hour) 60))))
	(display (tm:padding hours   #\0 2) port)
	(display (tm:padding minutes #\0 2) port))))

;;A  table of  output formatting  directives.  The  cars are  the format
;;char, the cdrs are procedures  that take the date, a padding character
;;(which might be #f), and the output port.
(define tm:directives
  (list
   (cons #\~ (lambda (date pad-with port) (display #\~ port)))

   (cons #\a (lambda (date pad-with port)
	       (display (tm:locale-abbr-weekday (date-week-day date))
			port)))
   (cons #\A (lambda (date pad-with port)
	       (display (tm:locale-long-weekday (date-week-day date))
			port)))
   (cons #\b (lambda (date pad-with port)
	       (display (tm:locale-abbr-month (<date>-month date))
			port)))
   (cons #\B (lambda (date pad-with port)
	       (display (tm:locale-long-month (<date>-month date))
			port)))
   (cons #\c (lambda (date pad-with port)
	       (display (date->string date $locale-date-time-format) port)))
   (cons #\d (lambda (date pad-with port)
	       (display (tm:padding (<date>-day date)
				    #\0 2)
			port)))
   (cons #\D (lambda (date pad-with port)
	       (display (date->string date "~m/~d/~y") port)))
   (cons #\e (lambda (date pad-with port)
	       (display (tm:padding (<date>-day date)
				    #\space 2)
			port)))
   (cons #\f (lambda (date pad-with port)
	       (let ((secs	(<date>-second date))
		     (nanosecs	(<date>-nanosecond date)))
		 (display
		  (tm:padding (if (< $number-of-nanoseconds-in-a-second nanosecs)
				  (+ secs 1)
				secs)
			      pad-with 2)
		  port)
		 (display $locale-number-separator port)
		 (display (%string-fractional-part (/ nanosecs $number-of-nanoseconds-in-a-second)) port)
		 )))
   (cons #\h (lambda (date pad-with port)
	       (display (date->string date "~b") port)))
   (cons #\H (lambda (date pad-with port)
	       (display (tm:padding (<date>-hour date)
				    pad-with 2)
			port)))
   (cons #\I (lambda (date pad-with port)
	       (let ((hr (<date>-hour date)))
		 (display
		  (if (> hr 12)
		      (tm:padding (- hr 12) pad-with 2)
		    (tm:padding hr pad-with 2))
		  port))))
   (cons #\j (lambda (date pad-with port)
	       (display (tm:padding (date-year-day date)
				    pad-with 3)
			port)))
   (cons #\k (lambda (date pad-with port)
	       (display (tm:padding (<date>-hour date)
				    #\space 2)
			port)))
   (cons #\l (lambda (date pad-with port)
	       (let ((hr (if (> (<date>-hour date) 12)
			     (- (<date>-hour date) 12) (<date>-hour date))))
		 (display (tm:padding hr  #\space 2)
			  port))))
   (cons #\m (lambda (date pad-with port)
	       (display (tm:padding (<date>-month date)
				    pad-with 2)
			port)))
   (cons #\M (lambda (date pad-with port)
	       (display (tm:padding (<date>-minute date)
				    pad-with 2)
			port)))
   (cons #\n (lambda (date pad-with port)
	       (newline port)))
   (cons #\N (lambda (date pad-with port)
	       (display (tm:padding (<date>-nanosecond date)
				    pad-with 9)
			port)))
   (cons #\p (lambda (date pad-with port)
	       (display (%locale-am/pm (<date>-hour date)) port)))
   (cons #\r (lambda (date pad-with port)
	       (display (date->string date "~I:~M:~S ~p") port)))
   (cons #\s (lambda (date pad-with port)
	       (display (time-second (date->time-utc date)) port)))
   (cons #\S (lambda (date pad-with port)
	       (if (> (<date>-nanosecond date)
		      $number-of-nanoseconds-in-a-second)
		   (display (tm:padding (+ (<date>-second date) 1)
					pad-with 2)
			    port)
		 (display (tm:padding (<date>-second date)
				      pad-with 2)
			  port))))
   (cons #\t (lambda (date pad-with port)
	       (display (integer->char 9) port)))
   (cons #\T (lambda (date pad-with port)
	       (display (date->string date "~H:~M:~S") port)))
   (cons #\U (lambda (date pad-with port)
	       (display
		(if (> (%days-before-first-week date 0) 0)
		    (tm:padding (+ (date-week-number date 0) 1) #\0 2)
		  (tm:padding (date-week-number date 0) #\0 2))
		port)))
   (cons #\V (lambda (date pad-with port)
	       (display (tm:padding (date-week-number date 1)
				    #\0 2) port)))
   (cons #\w (lambda (date pad-with port)
	       (display (date-week-day date) port)))
   (cons #\x (lambda (date pad-with port)
	       (display (date->string date $locale-short-date-format) port)))
   (cons #\X (lambda (date pad-with port)
	       (display (date->string date $locale-time-format) port)))
   (cons #\W (lambda (date pad-with port)
	       (display
		(if (> (%days-before-first-week date 1) 0)
		    (tm:padding (+ (date-week-number date 1) 1) #\0 2)
		  (tm:padding (date-week-number date 1) #\0 2))
		port)))
   (cons #\y (lambda (date pad-with port)
	       (display (tm:padding (tm:last-n-digits
				     (<date>-year date) 2)
				    pad-with
				    2)
			port)))
   (cons #\Y (lambda (date pad-with port)
	       (display (<date>-year date) port)))
   (cons #\z (lambda (date pad-with port)
	       (tm:tz-printer (<date>-zone-offset date) port)))
   (cons #\Z (lambda (date pad-with port)
	       (tm:locale-print-time-zone date port)))
   (cons #\1 (lambda (date pad-with port)
	       (display (date->string date "~Y-~m-~d") port)))
   (cons #\2 (lambda (date pad-with port)
	       (display (date->string date "~H:~M:~S~z") port)))
   (cons #\3 (lambda (date pad-with port)
	       (display (date->string date "~H:~M:~S") port)))
   (cons #\4 (lambda (date pad-with port)
	       (display (date->string date "~Y-~m-~dT~H:~M:~S~z") port)))
   (cons #\5 (lambda (date pad-with port)
	       (display (date->string date "~Y-~m-~dT~H:~M:~S") port)))
   ))

(define (tm:get-formatter char)
  (let ( (associated (assoc char tm:directives)) )
    (if associated (cdr associated) #f)))

(define (tm:date-printer date index format-string str-len port)
  (if (>= index str-len)
      (values)
    (let ( (current-char (string-ref format-string index)) )
      (if (not (char=? current-char #\~))
	  (begin
	    (display current-char port)
	    (tm:date-printer date (+ index 1) format-string str-len port))

	(if (= (+ index 1) str-len) ; bad format string.
	    (%time-error 'tm:date-printer 'bad-date-format-string
			   format-string)
	  (let ( (pad-char? (string-ref format-string (+ index 1))) )
	    (cond
	     ((char=? pad-char? #\-)
	      (if (= (+ index 2) str-len) ; bad format string.
		  (%time-error 'tm:date-printer 'bad-date-format-string
				 format-string)
		(let ( (formatter (tm:get-formatter
				   (string-ref format-string
					       (+ index 2)))) )
		  (if (not formatter)
		      (%time-error 'tm:date-printer 'bad-date-format-string
				     format-string)
		    (begin
		      (formatter date #f port)
		      (tm:date-printer date (+ index 3)
				       format-string str-len port))))))

	     ((char=? pad-char? #\_)
	      (if (= (+ index 2) str-len) ; bad format string.
		  (%time-error 'tm:date-printer 'bad-date-format-string
				 format-string)
		(let ( (formatter (tm:get-formatter
				   (string-ref format-string
					       (+ index 2)))) )
		  (if (not formatter)
		      (%time-error 'tm:date-printer 'bad-date-format-string
				     format-string)
		    (begin
		      (formatter date #\space port)
		      (tm:date-printer date (+ index 3)
				       format-string str-len port))))))
	     (else
	      (let ( (formatter (tm:get-formatter
				 (string-ref format-string
					     (+ index 1)))) )
		(if (not formatter)
		    (%time-error 'tm:date-printer 'bad-date-format-string
				   format-string)
		  (begin
		    (formatter date #\0 port)
		    (tm:date-printer date (+ index 2)
				     format-string str-len port))))))))))))


(define date->string
  (case-lambda
   ((date)
    (date->string date "~c"))
   ((date format-string)
    (receive (port getter)
	(open-string-output-port)
      (tm:date-printer date 0 format-string (string-length format-string) port)
      (getter)))))

(define (tm:char->int ch)
  (cond
   ((char=? ch #\0) 0)
   ((char=? ch #\1) 1)
   ((char=? ch #\2) 2)
   ((char=? ch #\3) 3)
   ((char=? ch #\4) 4)
   ((char=? ch #\5) 5)
   ((char=? ch #\6) 6)
   ((char=? ch #\7) 7)
   ((char=? ch #\8) 8)
   ((char=? ch #\9) 9)
   (else (%time-error 'string->date 'bad-date-template-string
			(list "Non-integer character" ch )))))

;; read an integer upto n characters long on port; upto -> #f if any length
(define (tm:integer-reader upto port)
  (define (accum-int port accum nchars)
    (let ((ch (peek-char port)))
      (if (or (eof-object? ch)
	      (not (char-numeric? ch))
	      (and upto (>= nchars  upto )))
	  accum
	(accum-int port (+ (* accum 10) (tm:char->int (read-char
						       port))) (+
								nchars 1)))))
  (accum-int port 0 0))

(define (tm:make-integer-reader upto)
  (lambda (port)
    (tm:integer-reader upto port)))

;; read an fractional integer upto n characters long on port; upto -> #f if any length
;;
;; The return value is normalized to upto decimal places. For example, if upto is 9 and
;; the string read is "123", the return value is 123000000.
(define (tm:fractional-integer-reader upto port)
  (define (accum-int port accum nchars)
    (let ((ch (peek-char port)))
      (if (or (eof-object? ch)
	      (not (char-numeric? ch))
	      (and upto (>= nchars  upto )))
	  (* accum (expt 10 (- upto nchars)))
	(accum-int port (+ (* accum 10) (tm:char->int (read-char port))) (+ nchars 1)))))
  (accum-int port 0 0))

(define (tm:make-fractional-integer-reader upto)
  (lambda (port)
    (tm:fractional-integer-reader upto port)))


;; read *exactly* n characters and convert to integer; could be padded
(define (tm:integer-reader-exact n port)
  (let ( (padding-ok #t) )
    (define (accum-int port accum nchars)
      (let ((ch (peek-char port)))
	(cond
	 ((>= nchars n) accum)
	 ((eof-object? ch)
	  (%time-error 'string->date 'bad-date-template-string
			 "Premature ending to integer read."))
	 ((char-numeric? ch)
	  (set! padding-ok #f)
	  (accum-int port (+ (* accum 10) (tm:char->int (read-char
							 port)))
		     (+ nchars 1)))
	 (padding-ok
	  (read-char port) ; consume padding
	  (accum-int port accum (+ nchars 1)))
	 (else ; padding where it shouldn't be
	  (%time-error 'string->date 'bad-date-template-string
			 "Non-numeric characters in integer read.")))))
    (accum-int port 0 0)))


(define (tm:make-integer-exact-reader n)
  (lambda (port)
    (tm:integer-reader-exact n port)))

(define (tm:zone-reader port)
  (let ( (offset 0)
	 (positive? #f) )
    (let ( (ch (read-char port)) )
      (if (eof-object? ch)
	  (%time-error 'string->date 'bad-date-template-string
			 (list "Invalid time zone +/-" ch)))
      (if (or (char=? ch #\Z) (char=? ch #\z))
	  0
	(begin
	  (cond
	   ((char=? ch #\+) (set! positive? #t))
	   ((char=? ch #\-) (set! positive? #f))
	   (else
	    (%time-error 'string->date 'bad-date-template-string
			   (list "Invalid time zone +/-" ch))))
	  (let ((ch (read-char port)))
	    (if (eof-object? ch)
		(%time-error 'string->date 'bad-date-template-string
			       (list "Invalid time zone number" ch)))
	    (set! offset (* (tm:char->int ch)
			    10 60 60)))
	  (let ((ch (read-char port)))
	    (if (eof-object? ch)
		(%time-error 'string->date 'bad-date-template-string
			       (list "Invalid time zone number" ch)))
	    (set! offset (+ offset (* (tm:char->int ch)
				      60 60))))
	  (let ((ch (read-char port)))
	    (if (eof-object? ch)
		(%time-error 'string->date 'bad-date-template-string
			       (list "Invalid time zone number" ch)))
	    (set! offset (+ offset (* (tm:char->int ch)
				      10 60))))
	  (let ((ch (read-char port)))
	    (if (eof-object? ch)
		(%time-error 'string->date 'bad-date-template-string
			       (list "Invalid time zone number" ch)))
	    (set! offset (+ offset (* (tm:char->int ch)
				      60))))
	  (if positive? offset (- offset)))))))

;; looking at a char, read the char string, run thru indexer, return index
(define (tm:locale-reader port indexer)
  (receive (string-port get-output-string)
      (open-string-output-port)
    (define (read-char-string)
      (let ((ch (peek-char port)))
	(if (char-alphabetic? ch)
	    (begin (write-char (read-char port) string-port)
		   (read-char-string))
	  (get-output-string))))
    (let* ( (str (read-char-string))
	    (index (indexer str)) )
      (if index index (%time-error 'string->date
				     'bad-date-template-string
				     (list "Invalid string for " indexer))))))

(define (tm:make-locale-reader indexer)
  (lambda (port)
    (tm:locale-reader port indexer)))

(define (tm:make-char-id-reader char)
  (lambda (port)
    (if (char=? char (read-char port))
	char
      (%time-error 'string->date
		     'bad-date-template-string
		     "Invalid character match."))))

;; A List of formatted read directives.
;; Each entry is a list.
;; 1. the character directive;
;; a procedure, which takes a character as input & returns
;; 2. #t as soon as a character on the input port is acceptable
;; for input,
;; 3. a port reader procedure that knows how to read the current port
;; for a value. Its one parameter is the port.
;; 4. a action procedure, that takes the value (from 3.) and some
;; object (here, always the date) and (probably) side-effects it.
;; In some cases (e.g., ~A) the action is to do nothing

(define tm:read-directives
  (let ((ireader4 (tm:make-integer-reader 4))
	(ireader2 (tm:make-integer-reader 2))
	(fireader9 (tm:make-fractional-integer-reader 9))
	(ireaderf (tm:make-integer-reader #f))
	(eireader2 (tm:make-integer-exact-reader 2))
	(eireader4 (tm:make-integer-exact-reader 4))
	(locale-reader-abbr-weekday (tm:make-locale-reader
				     tm:locale-abbr-weekday->index))
	(locale-reader-long-weekday (tm:make-locale-reader
				     tm:locale-long-weekday->index))
	(locale-reader-abbr-month   (tm:make-locale-reader
				     tm:locale-abbr-month->index))
	(locale-reader-long-month   (tm:make-locale-reader
				     tm:locale-long-month->index))
	(char-fail (lambda (ch) #t))
	(do-nothing (lambda (val object) (values))))

    (list
     (list #\~ char-fail (tm:make-char-id-reader #\~) do-nothing)
     (list #\a char-alphabetic? locale-reader-abbr-weekday do-nothing)
     (list #\A char-alphabetic? locale-reader-long-weekday do-nothing)
     (list #\b char-alphabetic? locale-reader-abbr-month
	   (lambda (val object)
	     (<date>-month-set! object val)))
     (list #\B char-alphabetic? locale-reader-long-month
	   (lambda (val object)
	     (<date>-month-set! object val)))
     (list #\d char-numeric? ireader2 (lambda (val object)
					(<date>-day-set!
					 object val)))
     (list #\e char-fail eireader2 (lambda (val object)
				     (<date>-day-set! object val)))
     (list #\h char-alphabetic? locale-reader-abbr-month
	   (lambda (val object)
	     (<date>-month-set! object val)))
     (list #\H char-numeric? ireader2 (lambda (val object)
					(<date>-hour-set! object val)))
     (list #\k char-fail eireader2 (lambda (val object)
				     (<date>-hour-set! object val)))
     (list #\m char-numeric? ireader2 (lambda (val object)
					(<date>-month-set! object val)))
     (list #\M char-numeric? ireader2 (lambda (val object)
					(<date>-minute-set!
					 object val)))
     (list #\N char-numeric? fireader9 (lambda (val object)
					 (<date>-nanosecond-set! object val)))
     (list #\S char-numeric? ireader2 (lambda (val object)
					(<date>-second-set! object val)))
     (list #\y char-fail eireader2
	   (lambda (val object)
	     (<date>-year-set! object (tm:natural-year val))))
     (list #\Y char-numeric? ireader4 (lambda (val object)
					(<date>-year-set! object val)))
     (list #\z (lambda (c)
		 (or (char=? c #\Z)
		     (char=? c #\z)
		     (char=? c #\+)
		     (char=? c #\-)))
	   tm:zone-reader (lambda (val object)
			    (<date>-zone-offset-set! object val)))
     )))

(define (tm:string->date date index format-string str-len port template-string)
  (define (skip-until port skipper)
    (let ((ch (peek-char port)))
      (if (eof-object? ch)
	  (%time-error 'string->date 'bad-date-format-string template-string)
	(if (not (skipper ch))
	    (begin (read-char port) (skip-until port skipper))))))
  (if (>= index str-len)
      (begin
	(values))
    (let ( (current-char (string-ref format-string index)) )
      (if (not (char=? current-char #\~))
	  (let ((port-char (read-char port)))
	    (if (or (eof-object? port-char)
		    (not (char=? current-char port-char)))
		(%time-error 'string->date 'bad-date-format-string template-string))
	    (tm:string->date date (+ index 1) format-string str-len port template-string))
	;; otherwise, it's an escape, we hope
	(if (> (+ index 1) str-len)
	    (%time-error 'string->date 'bad-date-format-string template-string)
	  (let* ( (format-char (string-ref format-string (+ index 1)))
		  (format-info (assoc format-char tm:read-directives)) )
	    (if (not format-info)
		(%time-error 'string->date 'bad-date-format-string template-string)
	      (begin
		(let ((skipper (cadr format-info))
		      (reader  (caddr format-info))
		      (actor   (cadddr format-info)))
		  (skip-until port skipper)
		  (let ((val (reader port)))
		    (if (eof-object? val)
			(%time-error 'string->date 'bad-date-format-string template-string)
		      (actor val date)))
		  (tm:string->date date (+ index 2)
				   format-string str-len port
				   template-string))))))))))

(define (string->date input-string template-string)
  (define (tm:date-ok? date)
    (and (<date>-nanosecond date)
	 (<date>-second date)
	 (<date>-minute date)
	 (<date>-hour date)
	 (<date>-day date)
	 (<date>-month date)
	 (<date>-year date)
	 (<date>-zone-offset date)))
  (let ((newdate (make-from-fields <date> 0 0 0 0 #f #f #f (%local-tz-offset))))
    (tm:string->date newdate
		     0
		     template-string
		     (string-length template-string)
		     (open-string-input-port input-string)
		     template-string)
    (if (tm:date-ok? newdate)
	newdate
      (%time-error
       'string->date 'bad-date-format-string
       (list "Incomplete date read. " newdate template-string)))))


;;;; done

)

;;; end of file
