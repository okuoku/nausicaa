;;; -*- coding: utf-8 -*-
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

    ;; classes
    <date> <time> <duration> <seconds-and-nanoseconds>

    ;; current time and clock resolution
    current-date current-julian-day current-modified-julian-day
    current-time time-resolution

    ;; string conversion
    date->string string->date)
  (import (nausicaa)
    (infix syntax)
    (rnrs mutable-strings)
    (formations)
    (times-and-dates seconds-and-subseconds)
    (times-and-dates years-and-weeks)
    (times-and-dates compat))


;;;; helpers

(define-syntax %display
  ;;Like DISPLAY  but outputs the value  in the port bound  to "port" in
  ;;the lexical context of the macro use.
  ;;
  (lambda (stx)
    (syntax-case stx ()
      ((?k ?val)
       #`(display ?val #,(datum->syntax #'?k 'port))))))

(define-inline (%seconds-unit-count? count)
  (and (exact? count) (integer? count)))

(define (%seconds-and-nanoseconds-default-protocol make-upper)
  (lambda (secs nanosecs)
    (assert (%seconds-unit-count? secs))
    (assert (%seconds-unit-count? nanosecs))
    (receive (secs nanosecs)
	(sn-normalise secs nanosecs)
      ((make-upper) secs nanosecs))))

(define (%seconds-and-nanoseconds-maker-protocol make-upper)
  (lambda (secs millisecs microsecs nanosecs)
    (assert (%seconds-unit-count? secs))
    (assert (%seconds-unit-count? millisecs))
    (assert (%seconds-unit-count? microsecs))
    (assert (%seconds-unit-count? nanosecs))
    (receive (secs nanosecs)
	(smun->sn secs millisecs microsecs nanosecs)
      ((make-upper) secs nanosecs))))


;;;; global variables and constants

(define-constant $locale-number-separator ".")

(define-constant $locale-abbr-weekday-vector
  '#("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))

(define-constant $locale-long-weekday-vector
  '#("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"))

(define-constant $locale-abbr-month-vector
  ;;Note empty string in 0th place.
  '#(""
     "Jan" "Feb" "Mar" "Apr"
     "May" "Jun" "Jul" "Aug"
     "Sep" "Oct" "Nov" "Dec"))

(define-constant $locale-long-month-vector
  ;;Note empty string in 0th place.
  '#(""
     "January"	"February"	"March"
     "April"	"May"		"June"
     "July"	"August"	"September"
     "October"	"November"	"December"))

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
;; (define (tm:read-tai-utc-data filename)
;;   (define (convert-jd jd)
;;     (* (- (inexact->exact jd) $tai-epoch-in-jd) $number-of-seconds-in-a-day))
;;   (define (convert-sec sec)
;;     (inexact->exact sec))
;;   (let ( (port (open-input-file filename))
;; 	 (table '()) )
;;     (let loop ((line (get-line port)))
;;       (if (not (eq? line (eof-object)))
;; 	  (receive (port getter)
;; 	      (open-string-output-port (string-append "(" line ")"))
;; 	    (let* ((data (read port))
;; 		   (year (car data))
;; 		   (jd   (cadddr (cdr data)))
;; 		   (secs (cadddr (cdddr data))))
;; 	      (if (>= year 1972)
;; 		  (set! table (cons (cons (convert-jd jd) (convert-sec secs)) table))
;; 		(loop (get-line port)))))))
;;     table))
;;
;; (define (read-leap-second-table filename)
;;   (set! $leap-second-table (tm:read-tai-utc-data filename))
;;   (values))


(define-class <seconds-and-nanoseconds>
  (nongenerative nausicaa:times-and-dates:<seconds-and-nanoseconds>)
  (fields (immutable seconds)
	  (immutable nanoseconds))
  (virtual-fields (immutable full-seconds)
		  (immutable full-milliseconds)
		  (immutable full-microseconds)
		  (immutable full-nanoseconds))
  (methods deep-clone shallow-clone)
  (protocol %seconds-and-nanoseconds-default-protocol)
  (maker ()
	 (:seconds	0)
	 (:milliseconds	0)
	 (:microseconds	0)
	 (:nanoseconds	0))
  (maker-protocol %seconds-and-nanoseconds-maker-protocol))

(define (<seconds-and-nanoseconds>-deep-copy (S <seconds-and-nanoseconds>))
  (make <seconds-and-nanoseconds> S.seconds S.nanoseconds))

(define <seconds-and-nanoseconds>-shallow-copy
  <seconds-and-nanoseconds>-deep-copy)

(define (<seconds-and-nanoseconds>-full-seconds (S <seconds-and-nanoseconds>))
  (sn->seconds S.seconds S.nanoseconds))

(define (<seconds-and-nanoseconds>-full-milliseconds (S <seconds-and-nanoseconds>))
  (sn->milliseconds S.seconds S.nanoseconds))

(define (<seconds-and-nanoseconds>-full-microseconds (S <seconds-and-nanoseconds>))
  (sn->microseconds S.seconds S.nanoseconds))

(define (<seconds-and-nanoseconds>-full-nanoseconds (S <seconds-and-nanoseconds>))
  (sn->nanoseconds S.seconds S.nanoseconds))


(define-class <duration>
  (nongenerative nausicaa:times-and-dates:<duration>)
  (inherit <seconds-and-nanoseconds>)
  (protocol %seconds-and-nanoseconds-default-protocol)
  (maker ()
	 (:seconds	0)
	 (:milliseconds	0)
	 (:microseconds	0)
	 (:nanoseconds	0))
  (maker-protocol %seconds-and-nanoseconds-maker-protocol)
  (methods = < > <= >= + - * /))

;;; --------------------------------------------------------------------

(define (<duration>-= (A <duration>) (B <duration>))
  (and (= (abs A.seconds)     (abs B.seconds))
       (= (abs A.nanoseconds) (abs B.nanoseconds))))

(define (<duration>-< (A <duration>) (B <duration>))
  (let ((a.secs (abs A.seconds))
	(b.secs (abs B.seconds)))
    (or (< a.secs b.secs)
	(if (> a.secs b.secs)
	    #f
	  (< (abs A.nanoseconds) (abs B.nanoseconds))))))

(define (<duration>-<= (A <duration>) (B <duration>))
  (let ((a.secs (abs A.seconds))
	(b.secs (abs B.seconds)))
    (or (< a.secs b.secs)
	(if (> a.secs b.secs)
	    #f
	  (<= (abs A.nanoseconds) (abs B.nanoseconds))))))

(define (<duration>-> (A <duration>) (B <duration>))
  (let ((a.secs (abs A.seconds))
	(b.secs (abs B.seconds)))
    (or (> a.secs b.secs)
	(if (< a.secs b.secs)
	    #f
	  (> (abs A.nanoseconds) (abs B.nanoseconds))))))

(define (<duration>->= (A <duration>) (B <duration>))
  (let ((a.secs (abs A.seconds))
	(b.secs (abs B.seconds)))
    (or (> a.secs b.secs)
	(if (< a.secs b.secs)
	    #f
	  (>= (abs A.nanoseconds) (abs B.nanoseconds))))))

;;; --------------------------------------------------------------------

(define duration=
  (case-lambda
   (()  #t)
   ((o) #t)
   (((a <duration>) (b <duration>))
    (a.= b))
   (((a <duration>) (b <duration>) . durations)
    (and (a.= b)
	 (apply duration= b durations)))))

(define duration<
  (case-lambda
   (()  #t)
   ((o) #t)
   (((a <duration>) (b <duration>))
    (a.< b))
   (((a <duration>) (b <duration>) . durations)
    (and (a.< b)
	 (apply duration< b durations)))))

(define duration>
  (case-lambda
   (()  #t)
   ((o) #t)
   (((a <duration>) (b <duration>))
    (a.> b))
   (((a <duration>) (b <duration>) . durations)
    (and (a.> b)
	 (apply duration> b durations)))))

(define duration<=
  (case-lambda
   (()  #t)
   ((o) #t)
   (((a <duration>) (b <duration>))
    (a.<= b))
   (((a <duration>) (b <duration>) . durations)
    (and (a.<= b)
	 (apply duration<= b durations)))))

(define duration>=
  (case-lambda
   (()  #t)
   ((o) #t)
   (((a <duration>) (b <duration>))
    (a.>= b))
   (((a <duration>) (b <duration>) . durations)
    (and (a.>= b)
	 (apply duration>= b durations)))))

;;; --------------------------------------------------------------------

(define (<duration>-+ (A <duration>) (B <duration>))
  (assert (is-a? B <duration>))
  (receive (secs nanosecs)
      (sn-add A.seconds A.nanoseconds
		      B.seconds B.nanoseconds)
    (make <duration> secs nanosecs)))

(define (<duration>-- (A <duration>) (B <duration>))
  (assert (is-a? B <duration>))
  (receive (secs nanosecs)
      (sn-sub A.seconds A.nanoseconds
		      B.seconds B.nanoseconds)
    (make <duration> (abs secs) (abs nanosecs))))

(define (<duration>-* (S <duration>) lambda)
  (assert (real? lambda))
  (make <duration> (* lambda S.seconds) (* lambda S.nanoseconds)))

(define (<duration>-/ (S <duration>) lambda)
  (assert (real? lambda))
  (make <duration>
    (exact (/ S.seconds     lambda))
    (exact (/ S.nanoseconds lambda))))


(define-class <time>
  (nongenerative nausicaa:times-and-dates:<time>)
  (inherit <seconds-and-nanoseconds>)
  (methods date)
  (protocol %seconds-and-nanoseconds-default-protocol)

  (maker ()
	 (:seconds		0)
	 (:milliseconds		0)
	 (:microseconds		0)
	 (:nanoseconds		0)
	 (:julian-day		#f)
	 (:modified-julian-day	#f))
  (maker-protocol <time>-maker-protocol)

  (virtual-fields (immutable julian-day)
		  (immutable modified-julian-day))
  (methods = < > <= >= + -))

(define (<time>-maker-protocol make-upper)
  (define (%make-time-from-julian-day jdn)
    (let ((nanosecs (infix $number-of-nanoseconds-in-a-second * $number-of-seconds-in-a-day
			   * (jdn - $tai-epoch-in-jd))))
      ((make-upper)
       (utc->tai (floor (/ nanosecs $number-of-nanoseconds-in-a-second)))
       (mod nanosecs $number-of-nanoseconds-in-a-second))))
  (define (%make-time-from-modified-julian-day mjdn)
    (%make-time-from-julian-day make-upper (+ mjdn 4800001/2)))

  (lambda (secs millisecs microsecs nanosecs jdn mjdn)
    (assert (%seconds-unit-count? secs))
    (assert (%seconds-unit-count? millisecs))
    (assert (%seconds-unit-count? microsecs))
    (assert (%seconds-unit-count? nanosecs))
    (cond (jdn
	   (assert (exact? jdn))
	   (%make-time-from-julian-day make-upper jdn))
	  (mjdn
	   (assert (exact? mjdn))
	   (%make-time-from-modified-julian-day make-upper mjdn))
	  (else
	   (receive (secs nanosecs)
	       (smun->sn secs millisecs microsecs nanosecs)
	     ((make-upper) secs nanosecs))))))

;;; --------------------------------------------------------------------

(define (<time>-= (A <time>) (B <time>))
  (assert (is-a? B <time>))
  (and (= A.seconds     B.seconds)
       (= A.nanoseconds B.nanoseconds)))

(define (<time>-< (A <time>) (B <time>))
  (assert (is-a? B <time>))
  (sn< A.seconds A.nanoseconds
	       B.seconds B.nanoseconds))

(define (<time>-<= (A <time>) (B <time>))
  (assert (is-a? B <time>))
  (sn<= A.seconds A.nanoseconds
		B.seconds B.nanoseconds))

(define (<time>-> (A <time>) (B <time>))
  (assert (is-a? B <time>))
  (sn> A.seconds A.nanoseconds
	       B.seconds B.nanoseconds))

(define (<time>->= (A <time>) (B <time>))
  (assert (is-a? B <time>))
  (sn>= A.seconds A.nanoseconds
		B.seconds B.nanoseconds))

;;; --------------------------------------------------------------------

(define time=
  (case-lambda
   (()  #t)
   ((o) #t)
   (((a <time>) (b <time>))
    (a.= b))
   (((a <time>) (b <time>) . times)
    (and (a.= b)
	 (apply time= b times)))))

(define time<
  (case-lambda
   (()  #t)
   ((o) #t)
   (((a <time>) (b <time>))
    (a.< b))
   (((a <time>) (b <time>) . times)
    (and (a.< b)
	 (apply time< b times)))))

(define time>
  (case-lambda
   (()  #t)
   ((o) #t)
   (((a <time>) (b <time>))
    (a.> b))
   (((a <time>) (b <time>) . times)
    (and (a.> b)
	 (apply time> b times)))))

(define time<=
  (case-lambda
   (()  #t)
   ((o) #t)
   (((a <time>) (b <time>))
    (a.<= b))
   (((a <time>) (b <time>) . times)
    (and (a.<= b)
	 (apply time<= b times)))))

(define time>=
  (case-lambda
   (()  #t)
   ((o) #t)
   (((a <time>) (b <time>))
    (a.>= b))
   (((a <time>) (b <time>) . times)
    (and (a.>= b)
	 (apply time>= b times)))))

;;; --------------------------------------------------------------------

(define (<time>-+ (A <time>) (B <duration>))
  (assert (is-a? B <duration>))
  (receive (secs nanosecs)
      (sn-add A.seconds A.nanoseconds
		      B.seconds B.nanoseconds)
    (make <time> secs nanosecs)))

(define (<time>-- (A <time>) (B <duration>))
  (assert (is-a? B <duration>))
  (receive (secs nanosecs)
      (sn-sub A.seconds A.nanoseconds
		      B.seconds B.nanoseconds)
    (make <time> (abs secs) (abs nanosecs))))

;;; --------------------------------------------------------------------

(define <time>-date
  (case-lambda
   ((T)
    (<time>-date T (%local-tz-offset)))
   (((T <time>) tz-offset)
    (define (%tai-before-leap-second? second)
      (find (lambda (x)
	      (= second (- (+ (car x) (cdr x)) 1)))
	    $leap-second-table))
    (define (%seconds-and-nanoseconds->date seconds nanoseconds tz-offset aux-seconds)
      ;;Convert a seconds count on the UTC scale and a nanoseconds count
      ;;to a <date>  object.  TZ-OFFSET must be the  time zone offset in
      ;;seconds.  AUX-SECONDS must  be a count of seconds  to put in the
      ;;seconds  field  of the  <date>  object; if  it  is  false it  is
      ;;ignored.
      ;;
      (receive (secs date month year)
	  (%decode-julian-day-number (%time->julian-day-number seconds tz-offset))
	(let* ((hours    (div secs $number-of-seconds-in-an-hour))
	       (rem      (mod secs $number-of-seconds-in-an-hour))
	       (minutes  (div rem 60))
	       (seconds  (mod rem 60)))
	  (make <date>
	    nanoseconds (or aux-seconds seconds) minutes hours
	    date month year tz-offset))))

    (if (%tai-before-leap-second? T.seconds)
	;;If  it's  *right* before  the  leap,  we  need to  pretend  to
	;;subtract a second  and set the seconds explicitly  in the date
	;;object.
	(%seconds-and-nanoseconds->date (- (tai->utc T.seconds) 1) T.nanoseconds tz-offset 60)
      (%seconds-and-nanoseconds->date (tai->utc T.seconds) T.nanoseconds tz-offset #f))
    )))

;;; --------------------------------------------------------------------

(define <time>-julian-day
  (case-lambda
   ((T)
    (<time>-julian-day T (%local-tz-offset)))
   (((T <time>) tz-offset)
    (%time->julian-day-number T.seconds tz-offset))))

(define (%time->julian-day-number seconds tz-offset)
  ;; special thing -- ignores nanos
  (infix $tai-epoch-in-jd +
	 ((seconds + tz-offset + $number-of-seconds-in-half-day) / $number-of-seconds-in-a-day)))

;; (define (<time>-julian-day (T <time>))
;;   ;;Return the Julian day representing T.
;;   ;;
;;   (infix $tai-epoch-in-jd +
;; 	 (T.second - %leap-second-delta (T.second) + T.nanosecond / $number-of-nanoseconds-in-a-second)
;; 	 / $number-of-seconds-in-a-day))

(define (<time>-modified-julian-day (T <time>))
  ;;Return the Modified Julian day representing T.
  ;;
  (infix T.julian-day - 4800001/2))


;;;; current time

(define (%get-time-of-day)
  (let ((ct (host:current-time)))
    (values (host:time-second ct)
            (host:time-nanosecond ct))))

(define (current-time)
  (receive (seconds nanoseconds)
      (%get-time-of-day)
    (receive (seconds nanoseconds)
	(sn-normalise seconds nanoseconds)
      (make <time>
	(utc->tai seconds)
	nanoseconds))))

(define (current-julian-day)
  ;;Return a Julian Day value representing the current time.
  ;;
  (let (((T <time>) (current-time)))
    T.julian-day))

(define (current-modified-julian-day)
  ;;Return a Modified Julian Day value representing the current time.
  ;;
  (let (((T <time>) (current-time)))
    T.modified-julian-day))

(define-constant time-resolution
  host:time-resolution)

(define current-date
  ;;Return a <date> object representin "now".
  ;;
  (case-lambda
   (()
    (current-date (%local-tz-offset)))
   ((tz-offset)
    (let (((T <time>) (current-time)))
      (T.date tz-offset)))))

(define (%local-tz-offset)
  (host:time-gmt-offset (host:current-time)))


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
  (virtual-fields (immutable year-day date-year-day)
		  (immutable index-of-day-in-week)
		  (immutable number-of-days-since-year-beginning)
		  (immutable time))
  (methods leap-year?
	   easter-day
	   number-of-days-before-first-week
	   week-number
	   julian-day
	   modified-julian-day)
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

		(unless (and (integer? year) (exact? year))
		  (assertion-violation 'make-<date>
		    "expected exact integer as year value" year))

		(when (or (< month 0) (< 12 month))
		  (assertion-violation 'make-<date>
		    "months count out of range, must be [0, 12]" month))

		(let ((number-of-days (vector-ref month (if (%leap-year? year)
							    $number-of-days-per-month/leap-year
							  $number-of-days-per-month/non-leap-year))))
		  (when (or (< day 0) (< number-of-days day))
		    (assertion-violation 'make-<date>
		      (string-append "days count out of range, must be [0, "
				     (number->string number-of-days) "]")
		      day)))

		((make-top) nanosecond second minute hour day month year zone-offset))))
  (maker ()
	 (:nanosecond	0)
	 (:second	0)
	 (:minute	0)
	 (:hour		0)
	 (:day		0)
	 (:month	0)
	 (:year		0)
	 (:zone-offset	0))
  (maker-protocol <date>-maker-protocol))

(define (<date>-maker-protocol make-upper)
  (define %make-date-from-julian-day
    ;;Return a <date> object representing JDN in the selected time zone.
    ;;
    (case-lambda
     ((jdn)
      (%make-date-from-julian-day (%local-tz-offset)))
     ((jdn tz-offset)
      (let (((T <time>) (make* <time>
			  (:julian-day jdn))))
	(T.date tz-offset)))))
  (define %make-date-from-modified-julian-day
    ;;Return  a <date> object  representing modified  JDN in  the selected
    ;;time zone.
    ;;
    (case-lambda
     ((jdn)
      (%make-date-from-modified-julian-day jdn (%local-tz-offset)))
     ((jdn tz-offset)
      (%make-date-from-julian-day (+ jdn 4800001/2) tz-offset))))
  (lambda (nanosecond second minute hour day month year zone-offset jdn mjdn)
    (cond (jdn
	   (%make-date-from-julian-day jdn))
	  (mjdn
	   (%make-date-from-modified-julian-day mjdn))
	  (else
	   (make <date>
	     nanosecond second minute hour day month year zone-offset)))))

;;; --------------------------------------------------------------------

(define (<date>-time D)
  (define (date->seconds-and-nanoseconds (D <date>))
    ;;Convert a date  object into a count of UTC seconds  and a count of
    ;;nanoseconds; return the two counts.
    ;;
    (let ((jdays (- (%encode-julian-day-number D.day D.month D.year)
		    $tai-epoch-in-jd)))
      (values (utc->tai (infix ((jdays - 1/2) * 24 + D.hour) * $number-of-seconds-in-an-hour
			       + D.minute * 60
			       + D.second
			       - D.zone-offset))
	      D.nanosecond)))

  (receive (seconds nanoseconds)
      (date->seconds-and-nanoseconds D)
    (make <time>
      (if (= 60 seconds)
	  (- seconds 1)
	seconds)
      nanoseconds)))

;;; --------------------------------------------------------------------

(define (<date>-leap-year? (D <date>))
  ;;Return true if D in in a leap year.
  ;;
  (%leap-year? D.year))

(define (<date>-number-of-days-since-year-beginning (D <date>))
  (%number-of-days-since-year-beginning D.day D.month D.year))

(define (<date>-easter-day (D <date>))
  ;;Return a new <date> object  representing the Easter day for the year
  ;;in D.  The new object has sub-day time set to zero and the same zone
  ;;of D.
  ;;
  (receive (easter-month easter-day)
      (%easter-month-and-day D.year)
    (make <date>
      0 0 0 0 ;nanosecond second minute hour
      easter-day easter-month D.year D.zone-offset)))

;;; --------------------------------------------------------------------

(define (<date>-index-of-day-in-week (D <date>))
  ;;Return the index  of the day in its week,  zero based: Sun=0, Mon=1,
  ;;Tue=2, etc.
  ;;
  (%index-of-day-in-week D.year D.month D.day))

(define (<date>-number-of-days-before-first-week (D <date>) day-of-week-starting-week)
  (assert (exact?   day-of-week-starting-week))
  (assert (integer? day-of-week-starting-week))
  (%number-of-days-before-first-week D.year day-of-week-starting-week))

(define (<date>-week-number (D <date>) day-of-week-starting-week)
  (let ((x (%number-of-days-before-first-week D.year day-of-week-starting-week)))
    (infix (D.number-of-days-since-year-beginning - x) // 7)))

;;; --------------------------------------------------------------------

(define (<date>-julian-day (D <date>))
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

(define (<date>-modified-julian-day D)
  ;;Return the modified Julian day representing D.
  ;;
  (- (<date>-julian-day D) 4800001/2))


;;;; Julian day stuff

(define (%encode-julian-day-number day month year)
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
	       (if (negative?  year) -1 0)
		;This adjusts year, taking care of the fact that between
		;1 Annus  Dominis and 1  Before Christ there is  no year
		;zero.  See the Calendar FAQ for details.
	       ))
	 (m (infix month + 12 * a - 3)))
    (infix day
	   + (153 * m + 2) // 5
	   + 365 * y
	   + y // 4
	   - y // 100
	   + y // 400
	   - 32045)))

(define (%decode-julian-day-number jdn)
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
     (infix (jdn - days) * $number-of-seconds-in-a-day)	;seconds
     (infix e + 1 - (153 * m + 2) // 5)			;day
     (infix m + 3 - 12 * (m // 10))			;month
     (if (>= 0 y) (- y 1) y))))
		;Year.  This adjusts year,  taking care of the fact that
		;between 1 Annus Dominis and 1 Before Christ there is no
		;year zero.  See the Calendar FAQ for details.


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

	    (#\a . ,(lambda ((D <date>) pad-char port)
		      (%display (%locale-abbr-weekday D.index-of-day-in-week))))


	    (#\A . ,(lambda ((D <date>) pad-char port)
		      (%display (%locale-long-weekday D.index-of-day-in-week))))

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

	    (#\j . ,(lambda ((D <date>) pad-char port)
		      (%display (%padding D.number-of-days-since-year-beginning pad-char 3))))

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

	    (#\s . ,(lambda ((D <date>) pad-char port)
		      (%display (let (((T <time>) D.time))
				  (tai->utc T.seconds)))))

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
		      (%display (let ((week-number (D.week-number 0)))
				  (%padding (if (> (D.number-of-days-before-first-week 0) 0)
						(+ week-number 1)
					      week-number)
					    #\0 2)))))

	    (#\V . ,(lambda ((D <date>) pad-char port)
		      (%display (%padding (D.week-number 1) #\0 2))))

	    (#\w . ,(lambda ((D <date>) pad-char port)
		      (%display D.index-of-day-in-week)))

	    (#\x . ,(lambda (date pad-char port)
		      (%display (date->string date $locale-short-date-format))))

	    (#\X . ,(lambda (date pad-char port)
		      (%display (date->string date $locale-time-format))))

	    (#\W . ,(lambda ((D <date>) pad-char port)
		      (let ((week-number (D.week-number 1)))
			(%display (%padding (if (> (D.number-of-days-before-first-week 1) 0)
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
  ;;Convert INPUT-STRING  into a <date>  object according to  a template
  ;;string.
  ;;
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
		 (lambda (val (D <date>)) (set! D.year (%natural-year val D.year))))

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
