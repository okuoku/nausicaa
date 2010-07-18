;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: utilities to handle seconds and subseconds counts
;;;Date: Wed Jul  7, 2010
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


(library (times-and-dates seconds-and-subseconds)
  (export

    ;; constants
    $leap-second-table

    $number-of-nanoseconds-in-a-microsecond
    $number-of-nanoseconds-in-a-millisecond
    $number-of-nanoseconds-in-a-second
    $number-of-nanoseconds-in-a-minute
    $number-of-nanoseconds-in-an-hour
    $number-of-nanoseconds-in-a-day

    $number-of-microseconds-in-a-nanosecond
    $number-of-microseconds-in-a-millisecond
    $number-of-microseconds-in-a-second
    $number-of-microseconds-in-a-minute
    $number-of-microseconds-in-an-hour
    $number-of-microseconds-in-a-day

    $number-of-milliseconds-in-a-nanosecond
    $number-of-milliseconds-in-a-microsecond
    $number-of-milliseconds-in-a-second
    $number-of-milliseconds-in-a-minute
    $number-of-milliseconds-in-an-hour
    $number-of-milliseconds-in-a-day

    $number-of-seconds-in-a-nanosecond
    $number-of-seconds-in-a-microsecond
    $number-of-seconds-in-a-millisecond
    $number-of-seconds-in-a-minute
    $number-of-seconds-in-an-hour
    $number-of-seconds-in-a-day

    ;; utilities for seconds and subseconds
    sn-normalise	su-normalise		sm-normalise
    sn->seconds		su->seconds		sm->seconds
    sn->milliseconds	su->milliseconds	sm->milliseconds
    sn->microseconds	su->microseconds	sm->microseconds
    sn->nanoseconds	su->nanoseconds		sm->nanoseconds
    seconds->sn		seconds->su		seconds->sm
    milliseconds->sn	milliseconds->su	milliseconds->sm
    microseconds->sn	microseconds->su	microseconds->sm
    nanoseconds->sn	nanoseconds->su		nanoseconds->sm

    smun->sn		sn->smun
    sn-add		sn-sub
    sn<			sn>
    sn<=		sn>=

    utc->tai		tai->utc
    )
  (import (rnrs)
    (only (language-extensions)
	  define-inline define-constant receive)
    (infix syntax))


;;;; constants

(define-constant $number-of-nanoseconds-in-a-microsecond	#e1e3)
(define-constant $number-of-nanoseconds-in-a-millisecond	#e1e6)
(define-constant $number-of-nanoseconds-in-a-second		#e1e9)
(define-constant $number-of-nanoseconds-in-a-minute		(* #e1e9 60))
(define-constant $number-of-nanoseconds-in-an-hour		(* #e1e9 60 60))
(define-constant $number-of-nanoseconds-in-a-day		(* #e1e9 60 60 24))

(define-constant $number-of-microseconds-in-a-nanosecond	#e1e-3)
(define-constant $number-of-microseconds-in-a-millisecond	#e1e3)
(define-constant $number-of-microseconds-in-a-second		#e1e6)
(define-constant $number-of-microseconds-in-a-minute		(* #e1e6 60))
(define-constant $number-of-microseconds-in-an-hour		(* #e1e6 60 60))
(define-constant $number-of-microseconds-in-a-day		(* #e1e6 60 60 24))

(define-constant $number-of-milliseconds-in-a-nanosecond	#e1e-6)
(define-constant $number-of-milliseconds-in-a-microsecond	#e1e-3)
(define-constant $number-of-milliseconds-in-a-second		#e1e3)
(define-constant $number-of-milliseconds-in-a-minute		(* #e1e3 60))
(define-constant $number-of-milliseconds-in-an-hour		(* #e1e3 60 60))
(define-constant $number-of-milliseconds-in-a-day		(* #e1e3 60 60 24))

(define-constant $number-of-seconds-in-a-nanosecond		#e1e-9)
(define-constant $number-of-seconds-in-a-microsecond		#e1e-6)
(define-constant $number-of-seconds-in-a-millisecond		#e1e-3)
(define-constant $number-of-seconds-in-a-minute			60)
(define-constant $number-of-seconds-in-an-hour			(* 60 60))
(define-constant $number-of-seconds-in-a-day			(* 60 60 24))


;;;; helpers

(define-inline (non-negative? n)
  (or (positive? n) (zero? n)))

(define-inline (non-positive? n)
  (or (negative? n) (zero? n)))


(define (%normalise seconds subseconds subcount)
  (receive (d subseconds)
      (div0-and-mod0 subseconds subcount)
    (let ((seconds (+ d seconds)))
      (cond ((positive? seconds)
	     (if (non-negative? subseconds)
		 (values seconds subseconds)
	       (values (- seconds 1) (+ subseconds subcount))))
	    ((negative? seconds)
	     (if (non-negative? subseconds)
		 (values (+ 1 seconds) (- subseconds subcount))
	       (values seconds subseconds)))
	    (else
	     (values seconds subseconds))))))

(define-inline (sn-normalise seconds nanoseconds)
  (%normalise seconds nanoseconds $number-of-nanoseconds-in-a-second))

(define-inline (su-normalise seconds microseconds)
  (%normalise seconds microseconds $number-of-microseconds-in-a-second))

(define-inline (sm-normalise seconds milliseconds)
  (%normalise seconds milliseconds $number-of-milliseconds-in-a-second))


(define-inline (sn->seconds seconds nanoseconds)
  (+ seconds (/ nanoseconds $number-of-nanoseconds-in-a-second)))

(define-inline (su->seconds seconds microseconds)
  (+ seconds (/ microseconds $number-of-microseconds-in-a-second)))

(define-inline (sm->seconds seconds milliseconds)
  (+ seconds (/ milliseconds $number-of-milliseconds-in-a-second)))

;;; --------------------------------------------------------------------

(define-inline (sn->milliseconds seconds nanoseconds)
  (+ (* seconds      $number-of-milliseconds-in-a-second)
     (/ nanoseconds  $number-of-nanoseconds-in-a-millisecond)))

(define-inline (su->milliseconds seconds microseconds)
  (+ (* seconds      $number-of-milliseconds-in-a-second)
     (/ microseconds $number-of-microseconds-in-a-millisecond)))

(define-inline (sm->milliseconds seconds milliseconds)
  (+ (* seconds      $number-of-milliseconds-in-a-second)
     milliseconds))

;;; --------------------------------------------------------------------

(define-inline (sn->microseconds seconds nanoseconds)
  (+ (* seconds      $number-of-microseconds-in-a-second)
     (/ nanoseconds  $number-of-nanoseconds-in-a-microsecond)))

(define-inline (su->microseconds seconds microseconds)
  (+ (* seconds      $number-of-microseconds-in-a-second)
     microseconds))

(define-inline (sm->microseconds seconds milliseconds)
  (+ (* seconds      $number-of-microseconds-in-a-second)
     (* milliseconds $number-of-microseconds-in-a-millisecond)))

;;; --------------------------------------------------------------------

(define-inline (sn->nanoseconds seconds nanoseconds)
  (+ (* seconds      $number-of-nanoseconds-in-a-second)
     nanoseconds))

(define-inline (su->nanoseconds seconds microseconds)
  (+ (* seconds      $number-of-nanoseconds-in-a-second)
     (* microseconds $number-of-nanoseconds-in-a-microsecond)))

(define-inline (sm->nanoseconds seconds milliseconds)
  (+ (* seconds      $number-of-nanoseconds-in-a-second)
     (* milliseconds $number-of-nanoseconds-in-a-millisecond)))


(define (%subseconds->sx subseconds seconds-count subseconds-count)
  (receive (seconds subseconds)
      (receive (seconds subseconds)
	  (div0-and-mod0 subseconds seconds-count)
	(cond ((positive? seconds)
	       (if (non-negative? subseconds)
		   (values seconds subseconds)
		 (values (- seconds 1) (+ subseconds seconds-count))))
	      ((negative? seconds)
	       (if (non-negative? subseconds)
		   (values (+ 1 seconds) (- subseconds seconds-count))
		 (values seconds subseconds)))
	      (else ;;(assert (zero? seconds))
	       (values seconds subseconds))))
    (values seconds (* subseconds subseconds-count))))

;;; --------------------------------------------------------------------

(define-inline (seconds->sn seconds)
  (values seconds 0))

(define-inline (milliseconds->sn milliseconds)
  (%subseconds->sx milliseconds
		   $number-of-milliseconds-in-a-second
		   $number-of-nanoseconds-in-a-millisecond))

(define-inline (microseconds->sn microseconds)
  (%subseconds->sx microseconds
		   $number-of-microseconds-in-a-second
		   $number-of-nanoseconds-in-a-microsecond))

(define-inline (nanoseconds->sn nanoseconds)
  (%subseconds->sx nanoseconds
		   $number-of-nanoseconds-in-a-second
		   1))

;;; --------------------------------------------------------------------

(define-inline (seconds->su seconds)
  (values seconds 0))

(define-inline (milliseconds->su milliseconds)
  (%subseconds->sx milliseconds
		   $number-of-milliseconds-in-a-second
		   $number-of-microseconds-in-a-millisecond))

(define-inline (microseconds->su microseconds)
  (%subseconds->sx microseconds
		   $number-of-microseconds-in-a-second
		   1))

(define-inline (nanoseconds->su nanoseconds)
  (%subseconds->sx nanoseconds
		   $number-of-nanoseconds-in-a-second
		   $number-of-microseconds-in-a-nanosecond))

;;; --------------------------------------------------------------------

(define-inline (seconds->sm seconds)
  (values seconds 0))

(define-inline (milliseconds->sm milliseconds)
  (%subseconds->sx milliseconds
		   $number-of-milliseconds-in-a-second
		   1))

(define-inline (microseconds->sm microseconds)
  (%subseconds->sx microseconds
		   $number-of-microseconds-in-a-second
		   $number-of-milliseconds-in-a-microsecond))

(define-inline (nanoseconds->sm nanoseconds)
  (%subseconds->sx nanoseconds
		   $number-of-nanoseconds-in-a-second
		   $number-of-milliseconds-in-a-nanosecond))


(define (smun->sn seconds milliseconds microseconds nanoseconds)
  (receive (u-s u-n)
      (microseconds->sn microseconds)
    (receive (m-s m-n)
	(milliseconds->sn milliseconds)
      (sn-normalise (+     seconds m-s u-s)
		    (+ nanoseconds m-n u-n)))))

(define (sn->smun seconds nanoseconds)
  (receive (seconds nanoseconds)
      (sn-normalise seconds nanoseconds)
    (receive (microseconds nanoseconds)
	(%normalise 0 nanoseconds $number-of-nanoseconds-in-a-microsecond)
      (receive (milliseconds microseconds)
	  (%normalise 0 microseconds $number-of-microseconds-in-a-millisecond)
	(values seconds milliseconds microseconds nanoseconds)))))


(define (sn-add s1 n1 s2 n2)
  (sn-normalise (+ s1 s2) (+ n1 n2)))

(define (sn-sub s1 n1 s2 n2)
  (sn-normalise (- s1 s2) (- n1 n2)))


(define (sn< s1 n1 s2 n2)
  (receive (seconds nanoseconds)
      (sn-sub s1 n1 s2 n2)
    (cond ((negative? seconds)
	   #t)
	  ((positive? seconds)
	   #f)
	  (else
	   (negative? nanoseconds)))))

(define (sn> s1 n1 s2 n2)
  (receive (seconds nanoseconds)
      (sn-sub s1 n1 s2 n2)
    (cond ((negative? seconds)
	   #f)
	  ((positive? seconds)
	   #t)
	  (else
	   (positive? nanoseconds)))))

(define (sn<= s1 n1 s2 n2)
  (receive (seconds nanoseconds)
      (sn-sub s1 n1 s2 n2)
    (cond ((negative? seconds)
	   #t)
	  ((positive? seconds)
	   #f)
	  (else
	   (non-positive? nanoseconds)))))

(define (sn>= s1 n1 s2 n2)
  (receive (seconds nanoseconds)
      (sn-sub s1 n1 s2 n2)
    (cond ((negative? seconds)
	   #f)
	  ((positive? seconds)
	   #t)
	  (else
	   (non-negative? nanoseconds)))))


(define-constant $leap-second-table
  ;;Each entry is:
  ;;
  ;;    ( <UTC seconds since Unix Epoch> .
  ;;      <number of seconds to add to UTC to compute TAI> )
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

(define (%utc-to-tai-leap-second-delta utc-seconds)
  ;;Given the UTC seconds count since the Unix Epoch, compute the number
  ;;of leap seconds to correct it:
  ;;
  ;;  tai-seconds = utc-seconds + leap-seconds
  ;;
  ;;this correction yields the TAI seconds from the UTC seconds.
  ;;
  (if (< utc-seconds 63072000)
      0
    (let loop ((table $leap-second-table))
      (if (>= utc-seconds (caar table))
	  (cdar table)
	(loop (cdr table))))))

(define (%tai-to-utc-leap-second-delta tai-seconds)
  ;;Given the TAI seconds count since the Unix Epoch, compute the number
  ;;of leap seconds to correct it:
  ;;
  ;;  utc-seconds = tai-seconds + leap-seconds
  ;;
  ;;this correction yields the UTC seconds from the TAI seconds.
  ;;
  (if (< tai-seconds 63072000)
      0
    (let loop ((table $leap-second-table))
      (if (null? table)
	  0
	(let ((elm (car table)))
	  (if (<= (cdr elm) (- tai-seconds (car elm)))
	      (- (cdr elm))
	    (loop (cdr table))))))))

(define (utc->tai seconds)
  (+ seconds (%utc-to-tai-leap-second-delta seconds)))

(define (tai->utc seconds)
  (+ seconds (%tai-to-utc-leap-second-delta seconds)))


;;;; done

)

;;; end of file
