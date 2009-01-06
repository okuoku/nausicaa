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

(import (r6rs)
  (rnrs mutable-pairs (6))
  (check-lib)
  (srfi time))

(check-set-mode! 'report-failed)
(display "*** testing time\n")


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
    (time? (current-time 'time-utc))
  => #t)

(check
    (time<? (current-time 'time-utc)
	    (current-time))
  => #t)

(check
    (time? (current-time 'time-tai))
  => #t)

(check
    (time? (current-time 'time-monotonic))
  => #t)

;; (check
;;     (time? (current-time 'time-thread))
;;   => #t)

;; (check
;;     (time? (current-time 'time-process))
;;   => #t)

;;; --------------------------------------------------------------------

(check
    (integer? (time-resolution 'time-tai))
  => #t)

(check
    (integer? (time-resolution 'time-utc))
  => #t)

(check
    (integer? (time-resolution 'time-monotonic))
  => #t)

;; (check
;;     (integer? (time-resolution 'time-thread))
;;   => #t)

;; (check
;;     (integer? (time-resolution 'time-process))
;;   => #t)

;;; --------------------------------------------------------------------

(let ((t1 (make-time 'time-utc 0 1))
      (t2 (make-time 'time-utc 0 1))
      (t3 (make-time 'time-utc 0 2))
      (t11 (make-time 'time-utc 1001 1))
      (t12 (make-time 'time-utc 1001 1))
      (t13 (make-time 'time-utc 1001 2)))
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

;;; --------------------------------------------------------------------

(let ((t1 (make-time 'time-utc 0 3000))
      (t2 (make-time 'time-utc 0 1000))
      (t3 (make-time 'time-duration 0 2000))
      (t4 (make-time 'time-duration 0 -2000)))
  (check
      (time=? t3 (time-difference t1 t2))
    => #t)
  (check
      (time=? t4 (time-difference t2 t1))
    => #t))

;;; --------------------------------------------------------------------

(define (test-one-utc-tai-edge utc tai-diff tai-last-diff)
  (let* (;; right on the edge they should be the same
	 (utc-basic (make-time 'time-utc 0 utc))
	 (tai-basic (make-time 'time-tai 0 (+ utc tai-diff)))
	 (utc->tai-basic (time-utc->time-tai utc-basic))
	 (tai->utc-basic (time-tai->time-utc tai-basic))
	 ;; a second before they should be the old diff
	 (utc-basic-1 (make-time 'time-utc 0 (- utc 1)))
	 (tai-basic-1 (make-time 'time-tai 0 (- (+ utc tai-last-diff) 1)))
	 (utc->tai-basic-1 (time-utc->time-tai utc-basic-1))
	 (tai->utc-basic-1 (time-tai->time-utc tai-basic-1))
	 ;; a second later they should be the new diff
	 (utc-basic+1 (make-time 'time-utc 0 (+ utc 1)))
	 (tai-basic+1 (make-time 'time-tai 0 (+ (+ utc tai-diff) 1)))
	 (utc->tai-basic+1 (time-utc->time-tai utc-basic+1))
	 (tai->utc-basic+1 (time-tai->time-utc tai-basic+1))
	 ;; ok, let's move the clock half a month or so plus half a second
	 (shy (* 15 24 60 60))
	 (hs (/ (expt 10 9) 2))
	 ;; a second later they should be the new diff
	 (utc-basic+2 (make-time 'time-utc hs (+ utc shy)))
	 (tai-basic+2 (make-time 'time-tai hs (+ (+ utc tai-diff) shy)))
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


;;;; date to string

(define the-date
  (make-date
   1 ; nanosecond
   2 ; second
   3 ; minute
   4 ; hour
   5 ; day
   6 ; month
   7 ; year
   (* 60 61))) ; zone-offset, in seconds east of GMT

(check
    (date->string the-date "~a")
  => "Sun")

(check
    (date->string the-date "~A")
  => "Sunday")

(check
    (date->string the-date "~b")
  => "Jun")

(check
    (date->string the-date "~B")
  => "June")

(check
    (date->string the-date "~d")
  => "05")

(check
    (date->string the-date "~e")
  => " 5")

(check
    (date->string the-date "~h")
  => "Jun")

(check
    (date->string the-date "~H")
  => "04")

(check
    (date->string the-date "~k")
  => " 4")

(check
    (date->string the-date "~m")
  => "06")

(check
    (date->string the-date "~M")
  => "03")

(check
    (date->string the-date "~S")
  => "02")

(check
    (date->string the-date "~y")
  => "07")

(check
    (date->string the-date "~Y")
  => "7")

(check
    (date->string the-date "~z")
  => "+0101")

;;; --------------------------------------------------------------------

(define the-date
  (make-date
   10 ; nanosecond
   20 ; second
   30 ; minute
   23 ; hour
   10 ; day
    6 ; month
   70 ; year
   0)) ; zone-offset, in seconds east of GMT

(check
    (date->string the-date "~a")
  => "Sun")

(check
    (date->string the-date "~A")
  => "Sunday")

(check
    (date->string the-date "~b")
  => "Jun")

(check
    (date->string the-date "~B")
  => "June")

(check
    (date->string the-date "~d")
  => "10")

(check
    (date->string the-date "~e")
  => "10")

(check
    (date->string the-date "~h")
  => "Jun")

(check
    (date->string the-date "~H")
  => "23")

(check
    (date->string the-date "~k")
  => "23")

(check
    (date->string the-date "~m")
  => "06")

(check
    (date->string the-date "~M")
  => "30")

(check
    (date->string the-date "~S")
  => "20")

(check
    (date->string the-date "~y")
  => "70")

(check
    (date->string the-date "~Y")
  => "70")

(check
    (date->string the-date "~z")
  => "Z")

(check
    (date->string the-date "~Z")
  => "")


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

;; ;TODO
;; #;(define (string->date/all-formats)
;;   )

;; (date->string/all-formats)
;; #;(string->date/all-formats)



;;;; done

(check-report)

;;; end of file
