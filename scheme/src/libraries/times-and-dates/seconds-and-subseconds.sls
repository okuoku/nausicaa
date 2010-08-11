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


(library (times-and-dates seconds-and-subseconds)
  (export

    ;; constants
    $leap-second-table

    $number-of-nanoseconds-in-one-microsecond
    $number-of-nanoseconds-in-one-millisecond
    $number-of-nanoseconds-in-one-second
    $number-of-nanoseconds-in-one-minute
    $number-of-nanoseconds-in-one-hour
    $number-of-nanoseconds-in-one-day

    $number-of-microseconds-in-one-nanosecond
    $number-of-microseconds-in-one-millisecond
    $number-of-microseconds-in-one-second
    $number-of-microseconds-in-one-minute
    $number-of-microseconds-in-one-hour
    $number-of-microseconds-in-one-day

    $number-of-milliseconds-in-one-nanosecond
    $number-of-milliseconds-in-one-microsecond
    $number-of-milliseconds-in-one-second
    $number-of-milliseconds-in-one-minute
    $number-of-milliseconds-in-one-hour
    $number-of-milliseconds-in-one-day

    $number-of-seconds-in-one-nanosecond
    $number-of-seconds-in-one-microsecond
    $number-of-seconds-in-one-millisecond
    $number-of-seconds-in-one-minute
    $number-of-seconds-in-one-hour
    $number-of-seconds-in-one-day

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

(define-constant $number-of-nanoseconds-in-one-microsecond	#e1e3)
(define-constant $number-of-nanoseconds-in-one-millisecond	#e1e6)
(define-constant $number-of-nanoseconds-in-one-second		#e1e9)
(define-constant $number-of-nanoseconds-in-one-minute		(* #e1e9 60))
(define-constant $number-of-nanoseconds-in-one-hour		(* #e1e9 60 60))
(define-constant $number-of-nanoseconds-in-one-day		(* #e1e9 60 60 24))

(define-constant $number-of-microseconds-in-one-nanosecond	#e1e-3)
(define-constant $number-of-microseconds-in-one-millisecond	#e1e3)
(define-constant $number-of-microseconds-in-one-second		#e1e6)
(define-constant $number-of-microseconds-in-one-minute		(* #e1e6 60))
(define-constant $number-of-microseconds-in-one-hour		(* #e1e6 60 60))
(define-constant $number-of-microseconds-in-one-day		(* #e1e6 60 60 24))

(define-constant $number-of-milliseconds-in-one-nanosecond	#e1e-6)
(define-constant $number-of-milliseconds-in-one-microsecond	#e1e-3)
(define-constant $number-of-milliseconds-in-one-second		#e1e3)
(define-constant $number-of-milliseconds-in-one-minute		(* #e1e3 60))
(define-constant $number-of-milliseconds-in-one-hour		(* #e1e3 60 60))
(define-constant $number-of-milliseconds-in-one-day		(* #e1e3 60 60 24))

(define-constant $number-of-seconds-in-one-nanosecond		#e1e-9)
(define-constant $number-of-seconds-in-one-microsecond		#e1e-6)
(define-constant $number-of-seconds-in-one-millisecond		#e1e-3)
(define-constant $number-of-seconds-in-one-minute			60)
(define-constant $number-of-seconds-in-one-hour			(* 60 60))
(define-constant $number-of-seconds-in-one-day			(* 60 60 24))


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
  (%normalise seconds nanoseconds $number-of-nanoseconds-in-one-second))

(define-inline (su-normalise seconds microseconds)
  (%normalise seconds microseconds $number-of-microseconds-in-one-second))

(define-inline (sm-normalise seconds milliseconds)
  (%normalise seconds milliseconds $number-of-milliseconds-in-one-second))


(define-inline (sn->seconds seconds nanoseconds)
  (+ seconds (/ nanoseconds $number-of-nanoseconds-in-one-second)))

(define-inline (su->seconds seconds microseconds)
  (+ seconds (/ microseconds $number-of-microseconds-in-one-second)))

(define-inline (sm->seconds seconds milliseconds)
  (+ seconds (/ milliseconds $number-of-milliseconds-in-one-second)))

;;; --------------------------------------------------------------------

(define-inline (sn->milliseconds seconds nanoseconds)
  (+ (* seconds      $number-of-milliseconds-in-one-second)
     (/ nanoseconds  $number-of-nanoseconds-in-one-millisecond)))

(define-inline (su->milliseconds seconds microseconds)
  (+ (* seconds      $number-of-milliseconds-in-one-second)
     (/ microseconds $number-of-microseconds-in-one-millisecond)))

(define-inline (sm->milliseconds seconds milliseconds)
  (+ (* seconds      $number-of-milliseconds-in-one-second)
     milliseconds))

;;; --------------------------------------------------------------------

(define-inline (sn->microseconds seconds nanoseconds)
  (+ (* seconds      $number-of-microseconds-in-one-second)
     (/ nanoseconds  $number-of-nanoseconds-in-one-microsecond)))

(define-inline (su->microseconds seconds microseconds)
  (+ (* seconds      $number-of-microseconds-in-one-second)
     microseconds))

(define-inline (sm->microseconds seconds milliseconds)
  (+ (* seconds      $number-of-microseconds-in-one-second)
     (* milliseconds $number-of-microseconds-in-one-millisecond)))

;;; --------------------------------------------------------------------

(define-inline (sn->nanoseconds seconds nanoseconds)
  (+ (* seconds      $number-of-nanoseconds-in-one-second)
     nanoseconds))

(define-inline (su->nanoseconds seconds microseconds)
  (+ (* seconds      $number-of-nanoseconds-in-one-second)
     (* microseconds $number-of-nanoseconds-in-one-microsecond)))

(define-inline (sm->nanoseconds seconds milliseconds)
  (+ (* seconds      $number-of-nanoseconds-in-one-second)
     (* milliseconds $number-of-nanoseconds-in-one-millisecond)))


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
		   $number-of-milliseconds-in-one-second
		   $number-of-nanoseconds-in-one-millisecond))

(define-inline (microseconds->sn microseconds)
  (%subseconds->sx microseconds
		   $number-of-microseconds-in-one-second
		   $number-of-nanoseconds-in-one-microsecond))

(define-inline (nanoseconds->sn nanoseconds)
  (%subseconds->sx nanoseconds
		   $number-of-nanoseconds-in-one-second
		   1))

;;; --------------------------------------------------------------------

(define-inline (seconds->su seconds)
  (values seconds 0))

(define-inline (milliseconds->su milliseconds)
  (%subseconds->sx milliseconds
		   $number-of-milliseconds-in-one-second
		   $number-of-microseconds-in-one-millisecond))

(define-inline (microseconds->su microseconds)
  (%subseconds->sx microseconds
		   $number-of-microseconds-in-one-second
		   1))

(define-inline (nanoseconds->su nanoseconds)
  (%subseconds->sx nanoseconds
		   $number-of-nanoseconds-in-one-second
		   $number-of-microseconds-in-one-nanosecond))

;;; --------------------------------------------------------------------

(define-inline (seconds->sm seconds)
  (values seconds 0))

(define-inline (milliseconds->sm milliseconds)
  (%subseconds->sx milliseconds
		   $number-of-milliseconds-in-one-second
		   1))

(define-inline (microseconds->sm microseconds)
  (%subseconds->sx microseconds
		   $number-of-microseconds-in-one-second
		   $number-of-milliseconds-in-one-microsecond))

(define-inline (nanoseconds->sm nanoseconds)
  (%subseconds->sx nanoseconds
		   $number-of-nanoseconds-in-one-second
		   $number-of-milliseconds-in-one-nanosecond))


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
	(%normalise 0 nanoseconds $number-of-nanoseconds-in-one-microsecond)
      (receive (milliseconds microseconds)
	  (%normalise 0 microseconds $number-of-microseconds-in-one-millisecond)
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
  ;;note they go higher to lower and end in 1972.
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
