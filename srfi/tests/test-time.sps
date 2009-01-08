;;;
;;;Part of: Nausicaa/SRFI
;;;Contents: tests for time
;;;Date: Tue Jan  6, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008, 2009 Marco Maggi <marcomaggi@gna.org>
;;;
;;;Permission is hereby granted, free of charge, to any person obtaining
;;;a  copy of  this  software and  associated  documentation files  (the
;;;"Software"), to  deal in the Software  without restriction, including
;;;without limitation  the rights to use, copy,  modify, merge, publish,
;;;distribute, sublicense,  and/or sell copies  of the Software,  and to
;;;permit persons to whom the Software is furnished to do so, subject to
;;;the following conditions:
;;;
;;;The  above  copyright notice  and  this  permission  notice shall  be
;;;included in all copies or substantial portions of the Software.
;;;
;;;Except  as  contained  in  this  notice, the  name(s)  of  the  above
;;;copyright holders  shall not be  used in advertising or  otherwise to
;;;promote  the sale,  use or  other dealings  in this  Software without
;;;prior written authorization.
;;;
;;;THE  SOFTWARE IS  PROVIDED "AS  IS",  WITHOUT WARRANTY  OF ANY  KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT.  IN NO EVENT  SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY,  WHETHER IN AN
;;;ACTION OF  CONTRACT, TORT  OR OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION  WITH THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.


;;;; setup

;; this is for Nausicaa's implementation
(import (r6rs)
  (rnrs mutable-pairs (6))
  (check-lib)
  (format-lib)
  (srfi time))

;; this is for Larceny's implementation
;; (import (r6rs)
;;   (rnrs mutable-pairs (6))
;;   (check-lib)
;;   (format-lib)
;;   (srfi :39)
;;   (srfi :19))

;; this is for Ypsilon's implementation
;; (import (r6rs)
;;   (rnrs mutable-pairs (6))
;;   (check-lib)
;;   (format-lib)
;;   (srfi srfi-39)
;;   (srfi srfi-19))

(check-set-mode! 'report-failed)
(display "*** testing time\n")


;;;; debugging

(define debugging
  (make-parameter #f))

(define (debug thing . args)
  (when (debugging)
    (if (string? thing)
	  (apply format (current-error-port) thing args)
      (write thing (current-error-port)))
    (newline (current-error-port))))

(define (debug-print-condition message exc)
  (debug "~a\nwho: ~s\nmessage: ~s\nirritants: ~s"
	 message
	 (if (who-condition? exc)
	     (condition-who exc)
	   'no-who)
	 (if (message-condition? exc)
	     (condition-message exc)
	   #f)
	 (if (irritants-condition? exc)
	     (condition-irritants exc)
	   #f)))



;;;; date stuff

(check
    (date? (current-date))
  => #t)

(check
    (date? 123)
  => #f)

;;; --------------------------------------------------------------------

(check
    (let ((d (make-date 1 2 3 4 5 6 7 8)))
      (date-nanosecond d))
  => 1)

(check
    (let ((d (make-date 1 2 3 4 5 6 7 8)))
      (date-second d))
  => 2)

(check
    (let ((d (make-date 1 2 3 4 5 6 7 8)))
      (date-minute d))
  => 3)

(check
    (let ((d (make-date 1 2 3 4 5 6 7 8)))
      (date-hour d))
  => 4)

(check
    (let ((d (make-date 1 2 3 4 5 6 7 8)))
      (date-day d))
  => 5)

(check
    (let ((d (make-date 1 2 3 4 5 6 7 8)))
      (date-month d))
  => 6)

(check
    (let ((d (make-date 1 2 3 4 5 6 7 8)))
      (date-year d))
  => 7)

(check
    (let ((d (make-date 1 2 3 4 5 6 7 8)))
      (date-zone-offset d))
  => 8)

(check
    (let ((d (make-date 1 2 3 4
			5 1 1 1)))
      (date-year-day d))
  => 5)

(check
    (let ((d (make-date 1 2 3 4
			5 1 1 1)))
      (date-week-day d))
  => 5)

(check
    (let ((d (make-date 1 2 3 4
			5 2 1 1)))
      (date-week-number d 1))
  => 5)



;;;; time

(check
    (time? (current-time))
  => #t)

(check
    (time? (current-time time-utc))
  => #t)

(check
    ;;enforce the order of evaluation!!
    (let* ((a (current-time time-utc))
	   (b (current-time)))
      (time<=? a b))
  => #t)

(check
    (time? (current-time time-tai))
  => #t)

(check
    (time? (current-time time-monotonic))
  => #t)

;; (check
;;     (time? (current-time time-thread))
;;   => #t)

;; (check
;;     (time? (current-time time-process))
;;   => #t)

;;; --------------------------------------------------------------------

(check
    (integer? (time-resolution time-tai))
  => #t)

(check
    (integer? (time-resolution time-utc))
  => #t)

(check
    (integer? (time-resolution time-monotonic))
  => #t)

;; (check
;;     (integer? (time-resolution time-thread))
;;   => #t)

;; (check
;;     (integer? (time-resolution time-process))
;;   => #t)

;;; --------------------------------------------------------------------

(let ((t1 (make-time time-utc 0 1))
      (t2 (make-time time-utc 0 1))
      (t3 (make-time time-utc 0 2))
      (t11 (make-time time-utc 1001 1))
      (t12 (make-time time-utc 1001 1))
      (t13 (make-time time-utc 1001 2)))
  (check
      (time=? t1 t2)
    => #t)
  (check
      (time>? t3 t2)
    => #t)
  (check
      (time<? t2 t3)
    => #t)
  (check
      (time>=? t1 t2)
    => #t)
  (check
      (time>=? t3 t2)
    => #t)
  (check
      (time<=? t1 t2)
    => #t)
  (check
      (time<=? t2 t3)
    => #t)
  (check
      (time=? t11 t12)
    => #t)
  (check
      (time>? t13 t12)
    => #t)
  (check
      (time<? t12 t13)
    => #t)
  (check
      (time>=? t11 t12)
    => #t)
  (check
      (time>=? t13 t12)
    => #t)
  (check
      (time<=? t11 t12)
    => #t)
  (check
      (time<=? t12 t13)
    => #t))

;;This  was reported  to the  SRFI-19  post-discuss list,  but with  the
;;arguments to  MAKE-TIME given in the  wrong order.  This  test has the
;;arguments in the correct order and it works.
(check
    (time=? (make-time time-monotonic (expt 10 9) 0)
	    (make-time time-monotonic 0 1))
  => #t)

(check
    (time=? (make-time time-monotonic (* 5 (expt 10 9)) 0)
	    (make-time time-monotonic 0 5))
  => #t)

;;; --------------------------------------------------------------------

(let ((t1 (make-time time-utc 0 3000))
      (t2 (make-time time-utc 0 1000))
      (t3 (make-time time-duration 0 2000))
      (t4 (make-time time-duration 0 -2000)))
  (check
      (time=? t3 (time-difference t1 t2))
    => #t)
  (check
      (time=? t4 (time-difference t2 t1))
    => #t))

;;; --------------------------------------------------------------------

(define (test-one-utc-tai-edge utc tai-diff tai-last-diff)
  (let* (;; right on the edge they should be the same
	 (utc-basic (make-time time-utc 0 utc))
	 (tai-basic (make-time time-tai 0 (+ utc tai-diff)))
	 (utc->tai-basic (time-utc->time-tai utc-basic))
	 (tai->utc-basic (time-tai->time-utc tai-basic))
	 ;; a second before they should be the old diff
	 (utc-basic-1 (make-time time-utc 0 (- utc 1)))
	 (tai-basic-1 (make-time time-tai 0 (- (+ utc tai-last-diff) 1)))
	 (utc->tai-basic-1 (time-utc->time-tai utc-basic-1))
	 (tai->utc-basic-1 (time-tai->time-utc tai-basic-1))
	 ;; a second later they should be the new diff
	 (utc-basic+1 (make-time time-utc 0 (+ utc 1)))
	 (tai-basic+1 (make-time time-tai 0 (+ (+ utc tai-diff) 1)))
	 (utc->tai-basic+1 (time-utc->time-tai utc-basic+1))
	 (tai->utc-basic+1 (time-tai->time-utc tai-basic+1))
	 ;; ok, let's move the clock half a month or so plus half a second
	 (shy (* 15 24 60 60))
	 (hs (/ (expt 10 9) 2))
	 ;; a second later they should be the new diff
	 (utc-basic+2 (make-time time-utc hs (+ utc shy)))
	 (tai-basic+2 (make-time time-tai hs (+ (+ utc tai-diff) shy)))
	 (utc->tai-basic+2 (time-utc->time-tai utc-basic+2))
	 (tai->utc-basic+2 (time-tai->time-utc tai-basic+2))
	 )
    (and (time=? utc-basic tai->utc-basic)
	 (time=? tai-basic utc->tai-basic)
	 (time=? utc-basic-1 tai->utc-basic-1)
	 (time=? tai-basic-1 utc->tai-basic-1)
	 (time=? utc-basic+1 tai->utc-basic+1)
	 (time=? tai-basic+1 utc->tai-basic+1)
	 (time=? utc-basic+2 tai->utc-basic+2)
	 (time=? tai-basic+2 utc->tai-basic+2)
	 )))

(check
    (test-one-utc-tai-edge 915148800  32 31)
  => #t)
(check
    (test-one-utc-tai-edge 867715200  31 30)
  => #t)
(check
    (test-one-utc-tai-edge 820454400  30 29)
  => #t)
(check
    (test-one-utc-tai-edge 773020800  29 28)
  => #t)
(check
    (test-one-utc-tai-edge 741484800  28 27)
  => #t)
(check
    (test-one-utc-tai-edge 709948800  27 26)
  => #t)
(check
    (test-one-utc-tai-edge 662688000  26 25)
  => #t)
(check
    (test-one-utc-tai-edge 631152000  25 24)
  => #t)
(check
    (test-one-utc-tai-edge 567993600  24 23)
  => #t)
(check
    (test-one-utc-tai-edge 489024000  23 22)
  => #t)
(check
    (test-one-utc-tai-edge 425865600  22 21)
  => #t)
(check
    (test-one-utc-tai-edge 394329600  21 20)
  => #t)
(check
    (test-one-utc-tai-edge 362793600  20 19)
  => #t)
(check
    (test-one-utc-tai-edge 315532800  19 18)
  => #t)
(check
    (test-one-utc-tai-edge 283996800  18 17)
  => #t)
(check
    (test-one-utc-tai-edge 252460800  17 16)
  => #t)
(check
    (test-one-utc-tai-edge 220924800  16 15)
  => #t)
(check
    (test-one-utc-tai-edge 189302400  15 14)
  => #t)
(check
    (test-one-utc-tai-edge 157766400  14 13)
  => #t)
(check
    (test-one-utc-tai-edge 126230400  13 12)
  => #t)
(check
    (test-one-utc-tai-edge 94694400   12 11)
  => #t)
(check
    (test-one-utc-tai-edge 78796800   11 10)
  => #t)
(check
    (test-one-utc-tai-edge 63072000   10 0)
  => #t)
;;; at the epoch
(check
    (test-one-utc-tai-edge 0   0 0)
  => #t)
;;; close to it ...
(check
    (test-one-utc-tai-edge 10   0 0)
  => #t)
;;; about now ...
(check
    (test-one-utc-tai-edge 1045789645 32 32)
  => #t)

;;; --------------------------------------------------------------------

(define (tm:date= d1 d2)
  (and (= (date-year d1) (date-year d2))
       (= (date-month d1) (date-month d2))
       (= (date-day d1) (date-day d2))
       (= (date-hour d1) (date-hour d2))
       (= (date-second d1) (date-second d2))
       (= (date-nanosecond d1) (date-nanosecond d2))
       (= (date-zone-offset d1) (date-zone-offset d2))))

(check
    (tm:date= (time-tai->date (make-time time-tai 0 (+ 915148800 29)) 0)
	      (make-date 0 58 59 23 31 12 1998 0))
  => #t)
(check
    (tm:date= (time-tai->date (make-time time-tai 0 (+ 915148800 30)) 0)
	      (make-date 0 59 59 23 31 12 1998 0))
  => #t)
(check
    (tm:date= (time-tai->date (make-time time-tai 0 (+ 915148800 31)) 0)
	      (make-date 0 60 59 23 31 12 1998 0))
  => #t)
(check
    (tm:date= (time-tai->date (make-time time-tai 0 (+ 915148800 32)) 0)
	      (make-date 0 0 0 0 1 1 1999 0))
  => #t)

;;; --------------------------------------------------------------------

(check
    (time=? (make-time time-utc 0 (- 915148800 2))
	    (date->time-utc (make-date 0 58 59 23 31 12 1998 0)))
  => #t)
(check
    (time=? (make-time time-utc 0 (- 915148800 1))
	    (date->time-utc (make-date 0 59 59 23 31 12 1998 0)))
  => #t)
;;; yes, I think this is acutally right.
(check
    (time=? (make-time time-utc 0 (- 915148800 0))
	    (date->time-utc (make-date 0 60 59 23 31 12 1998 0)))
  => #t)
(check
    (time=? (make-time time-utc 0 (- 915148800 0))
	    (date->time-utc (make-date 0 0 0 0 1 1 1999 0)))
  => #t)
(check
    (time=? (make-time time-utc 0 (+ 915148800 1))
	    (date->time-utc (make-date 0 1 0 0 1 1 1999 0)))
  => #t)

;;; --------------------------------------------------------------------

(let ((ct-utc (make-time time-utc 6320000 1045944859))
      (ct-tai (make-time time-tai 6320000 1045944891))
      (cd (make-date 6320000 19 14 15 22 2 2003 -18000)))
  (and
   (check
       (time=? ct-utc (date->time-utc cd))
     => #t)
   (check
       (time=? ct-tai (date->time-tai cd))
     => #t)))


;;;; date to string, single digits

(define date-1
  (make-date
   1 ; nanosecond
   2 ; second
   3 ; minute
   4 ; hour
   5 ; day
   6 ; month
   1950 ; year, it has to be after 1752
   (* 60 60))) ; zone-offset, in seconds east of GMT


(check
    (date->string date-1 "~~")
  => "~")

(check
    (list (date->string date-1 "~a")
	  (date->string date-1 "~A"))
  => '("Mon" "Monday"))

(check
    (list (date->string date-1 "~b")
	  (date->string date-1 "~B"))
  => '("Jun" "June"))

(check
    (date->string date-1 "~c")
  => "Mon Jun 05 04:03:02+0100 1950")

(check
    (date->string date-1 "~d")
  => "05")

(check
    (date->string date-1 "~D")
  => "06/05/50")

(check
    (date->string date-1 "~e")
  => " 5")

(check
    (date->string date-1 "~f")
  => "02.000000001")

(check
    (date->string date-1 "~h")
  => "Jun")

(check
    (date->string date-1 "~H")
  => "04")

(check
    (date->string date-1 "~I")
  => "04")

(check
    (date->string date-1 "~j")
  => "156")

(check
    (date->string date-1 "~k")
  => " 4")

(check
    (date->string date-1 "~l")
  => " 4")

(check
    (date->string date-1 "~m")
  => "06")

(check
    (date->string date-1 "~M")
  => "03")

(check
    (date->string date-1 "~n")
  => "\n")

(check
    (date->string date-1 "~N")
  => "000000001")

(check
    (date->string date-1 "~p")
  => "AM")

(check
    (date->string date-1 "~r")
  => "04:03:02 AM")

(check
    (date->string date-1 "~s")
  => "-617749018")

(check
    (date->string date-1 "~S")
  => "02")

(check
    (date->string date-1 "~t")
  => "\t")

(check
    (date->string date-1 "~T")
  => "04:03:02")

(check
    (date->string date-1 "~U")
  => "23")

(check
    (date->string date-1 "~V")
  => "23")

(check
    (date->string date-1 "~w")
  => "1")

(check
    (date->string date-1 "~W")
  => "23")

;;; FIXME the following 2 are wrong according to the SRFI document
(check
    (date->string date-1 "~x")
  => "06/05/50")

(check
    (date->string date-1 "~X")
  => "04:03:02")

(check
    (date->string date-1 "~y")
  => "50")

(check
    (date->string date-1 "~Y")
  => "1950")

(check
    (date->string date-1 "~z")
  => "+0100")

;;; not implemented
(check
    (date->string date-1 "~Z")
  => "")

;; Check out ISO 8601 year-month-day format
(check
    (date->string date-1 "~1")
  => "1950-06-05")

;; Check out ISO 8601 hour-minute-second-timezone format
(check
    (date->string date-1 "~2")
  => "04:03:02+0100")

;; Check out ISO 8601 hour-minute-second format
(check
    (date->string date-1 "~3")
  => "04:03:02")

;; Check out ISO 8601 year-month-day-hour-minute-second-timezone format
(check
    (date->string date-1 "~4")
  => "1950-06-05T04:03:02+0100")

;; Check out ISO 8601 year-month-day-hour-minute-second format
(check
    (date->string date-1 "~5")
  => "1950-06-05T04:03:02")


;;;; date to string, 2 digits

(define date-2
  (make-date
   15		; nanosecond
   14		; second
   13		; minute
   12		; hour
   11		; day
   10		; month
   2000		; year
   0))		; zone-offset, in seconds east of GMT

(check
    (date->string date-2 "~~")
  => "~")

(check
    (list (date->string date-2 "~a")
	  (date->string date-2 "~A"))
  => '("Wed" "Wednesday"))

(check
    (list (date->string date-2 "~b")
	  (date->string date-2 "~B"))
  => '("Oct" "October"))

(check
    (date->string date-2 "~c")
  => "Wed Oct 11 12:13:14Z 2000")

(check
    (date->string date-2 "~d")
  => "11")

(check
    (date->string date-2 "~D")
  => "10/11/00")

(check
    (date->string date-2 "~e")
  => "11")

(check
    (date->string date-2 "~f")
  => "14.000000015")

(check
    (date->string date-2 "~h")
  => "Oct")

(check
    (date->string date-2 "~H")
  => "12")

(check
    (date->string date-2 "~I")
  => "12")

(check
    (date->string date-2 "~j")
  => "285")

(check
    (date->string date-2 "~k")
  => "12")

(check
    (date->string date-2 "~l")
  => "12")

(check
    (date->string date-2 "~m")
  => "10")

(check
    (date->string date-2 "~M")
  => "13")

(check
    (date->string date-2 "~n")
  => "\n")

(check
    (date->string date-2 "~N")
  => "000000015")

(check
    (date->string date-2 "~p")
  => "PM")

(check
    (date->string date-2 "~r")
  => "12:13:14 PM")

(check
    (date->string date-2 "~s")
  => "971266394")

(check
    (date->string date-2 "~S")
  => "14")

(check
    (date->string date-2 "~t")
  => "\t")

(check
    (date->string date-2 "~T")
  => "12:13:14")

(check
    (date->string date-2 "~U")
  => "41")

(check
    (date->string date-2 "~V")
  => "41")

(check
    (date->string date-2 "~w")
  => "3")

(check
    (date->string date-2 "~W")
  => "41")

;;; FIXME the following 2 are wrong according to the SRFI document
(check
    (date->string date-2 "~x")
  => "10/11/00")

(check
    (date->string date-2 "~X")
  => "12:13:14")

(check
    (date->string date-2 "~y")
  => "00")

(check
    (date->string date-2 "~Y")
  => "2000")

;; when the time zone offset is zero this is "Z"
(check
    (date->string date-2 "~z")
  => "Z")

;;; not implemented
(check
    (date->string date-2 "~Z")
  => "")

;; Check out ISO 8601 year-month-day format
(check
    (date->string date-2 "~1")
  => "2000-10-11")

;; Check out ISO 8601 hour-minute-second-timezone format
(check
    (date->string date-2 "~2")
  => "12:13:14Z")

;; Check out ISO 8601 hour-minute-second format
(check
    (date->string date-2 "~3")
  => "12:13:14")

;; Check out ISO 8601 year-month-day-hour-minute-second-timezone format
(check
    (date->string date-2 "~4")
  => "2000-10-11T12:13:14Z")

;; Check out ISO 8601 year-month-day-hour-minute-second format
(check
    (date->string date-2 "~5")
  => "2000-10-11T12:13:14")



;;;; other date to string conversions

;;This was reported as error in the SRFI-19 post-discuss list.  It seems
;;to work now.
(check
    (date->string
     (time-monotonic->date (make-time time-monotonic 1 0))
     "~f")
  => "00.000000001")


;;;; string->date

(parameterize ((debugging #t))

  (define date-3
    (make-date
     0		 ; nanosecond
     56		 ; second
     34		 ; minute
     12		 ; hour
     06		 ; day
     01		 ; month
     2009	 ; year
     (* 60 60))) ; zone-offset, in seconds east of GMT

  (check
      (let ((d (string->date "2009-01-06T12:34:56+0100"
			     "~Y-~m-~dT~H:~M:~S~z")))
	(time=? (date->time-utc date-3)
		(date->time-utc d)))
    => #t)

  (check
      (let ((d (string->date "2009 January 06T12:34:56+0100"
			     "~Y ~B ~dT~H:~M:~S~z")))
	(time=? (date->time-utc date-3)
		(date->time-utc d)))
    => #t)

  (check
      (let ((d (string->date "2009 Jan  6 12:34:56+0100"
			     "~Y ~b ~e ~k:~M:~S~z")))
	(time=? (date->time-utc date-3)
		(date->time-utc d)))
    => #t)

  (check
      (let ((d (string->date "09 Jan  6 12:34:56+0100"
			     "~y ~h ~e ~H:~M:~S~z")))
	(time=? (date->time-utc date-3)
		(date->time-utc d)))
    => #t)

  )

;;; TODO: figure out why ~f isn't working
;;; TODO: figure out why ~x and ~X aren't doing what the srfi-19 doc says they do
;; (let ((d (string->date "2009-01-06T12:34:56+0100")))
;;   (for-each
;;       (lambda (f)
;; 	(display (date->string d f))(newline)
;; 	(check
;; 	    (string? (date->string d f))
;; 	  => #t))
;;     '("~~" "~a" "~A" "~b" "~B" "~c" "~d" "~D" "~e" #;"~f" "~h" "~H"
;;       "~I" "~j" "~k" "~l" "~m" "~M" "~n" "~N" "~p" "~r" "~s"
;;       "~S" "~t" "~T" "~U" "~V" "~w" "~W" "~x" "~X" "~y" "~Y"
;;       "~z" "~Z" "~1" "~2" "~3" "~4" "~5")))




;;;; done

(check-report)

;;; end of file
