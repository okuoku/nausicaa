;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for seconds utilities
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


(import (nausicaa)
  (times-and-dates seconds-and-subseconds)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing times-and-dates seconds utilities\n")


(parametrise ((check-test-name	'normalisation))

  (define-syntax tol
    (syntax-rules ()
      ((_ ?form)
       (call-with-values (lambda () ?form) list))))

  (check
      (tol (sn-normalise 10 100))
    => '(10 100))

  (check
      (tol (sn-normalise 10 #e1e9))
    => '(11 0))

  (check
      (tol (sn-normalise 10 #e2e9))
    => '(12 0))

  (check
      (tol (sn-normalise +10 #e+1.001e9))
    => '(+11 #e+1e6))

  (check
      (tol (sn-normalise -10 #e-1.001e9))
    => '(-11 #e-1e6))

  (check
      (tol (sn-normalise 10 #e-2e9))
    => '(8 0))

;;; --------------------------------------------------------------------

  (check
      (tol (sn-normalise 1 1))
    => '(1 1))

  (check
      (tol (sn-normalise -1 -1))
    => '(-1 -1))

  (check
      (tol (sn-normalise 1 -1))
    => (list 0 (- $number-of-nanoseconds-in-a-second 1)))

  (check
      (tol (sn-normalise -1 1))
    => (list 0 (- 1 $number-of-nanoseconds-in-a-second)))

;;; --------------------------------------------------------------------

  (check
      (tol (su-normalise 1 1))
    => '(1 1))

  (check
      (tol (su-normalise -1 -1))
    => '(-1 -1))

  (check
      (tol (su-normalise 1 -1))
    => (list 0 (- $number-of-microseconds-in-a-second 1)))

  (check
      (tol (su-normalise -1 1))
    => (list 0 (- 1 $number-of-microseconds-in-a-second)))

;;; --------------------------------------------------------------------

  (check
      (tol (sm-normalise 1 1))
    => '(1 1))

  (check
      (tol (sm-normalise -1 -1))
    => '(-1 -1))

  (check
      (tol (sm-normalise 1 -1))
    => (list 0 (- $number-of-milliseconds-in-a-second 1)))

  (check
      (tol (sm-normalise -1 1))
    => (list 0 (- 1 $number-of-milliseconds-in-a-second)))

  #t)


(parametrise ((check-test-name	'single-conversion))

  (define-syntax tol
    (syntax-rules ()
      ((_ ?form)
       (call-with-values (lambda () ?form) list))))

;;;                                    123456789
  (check (sn->seconds 1 1)	=> #e1.000000001)
  (check (su->seconds 1 1)	=> #e1.000001)
  (check (sm->seconds 1 1)	=> #e1.001)

  (check (sn->milliseconds 1 1)	=> #e1000.000001)
  (check (su->milliseconds 1 1)	=> #e1000.001)
  (check (sm->milliseconds 1 1)	=> #e1001)

  (check (sn->microseconds 1 1)	=> #e1000000.001)
  (check (su->microseconds 1 1)	=> #e1000001)
  (check (sm->microseconds 1 1)	=> #e1001000)

  (check (sn->nanoseconds 1 1)	=> #e1000000001)
  (check (su->nanoseconds 1 1)	=> #e1000001000)
  (check (sm->nanoseconds 1 1)	=> #e1001000000)

;;; --------------------------------------------------------------------

  (check (tol (seconds->sn 123))	=> '(123 0))
  (check (tol (seconds->su 123))	=> '(123 0))
  (check (tol (seconds->sm 123))	=> '(123 0))

  (check (tol (milliseconds->sn 123))	=> '(0 #e123e6))
  (check (tol (milliseconds->su 123))	=> '(0 #e123e3))
  (check (tol (milliseconds->sm 123))	=> '(0 123))

  (check (tol (microseconds->sn 123))	=> '(0 #e123e3))
  (check (tol (microseconds->su 123))	=> '(0 123))
  (check (tol (microseconds->sm 123))	=> '(0 #e123e-3))

  (check (tol (nanoseconds->sn 123))	=> '(0 123))
  (check (tol (nanoseconds->su 123))	=> '(0 #e123e-3))
  (check (tol (nanoseconds->sm 123))	=> '(0 #e123e-6))

  (check (tol (seconds->sn -123))	=> '(-123 0))
  (check (tol (seconds->su -123))	=> '(-123 0))
  (check (tol (seconds->sm -123))	=> '(-123 0))

  (check (tol (milliseconds->sn -123))	=> '(0 #e-123e6))
  (check (tol (milliseconds->su -123))	=> '(0 #e-123e3))
  (check (tol (milliseconds->sm -123))	=> '(0 -123))

  (check (tol (microseconds->sn -123))	=> '(0 #e-123e3))
  (check (tol (microseconds->su -123))	=> '(0 -123))
  (check (tol (microseconds->sm -123))	=> '(0 #e-123e-3))

  (check (tol (nanoseconds->sn -123))	=> '(0 -123))
  (check (tol (nanoseconds->su -123))	=> '(0 #e-123e-3))
  (check (tol (nanoseconds->sm -123))	=> '(0 #e-123e-6))

;;;                             9876543210
  (check (tol (milliseconds->sn       9123))	=> '(9 #e123e6))
  (check (tol (milliseconds->su       9123))	=> '(9 #e123e3))
  (check (tol (milliseconds->sm       9123))	=> '(9 123))

;;;                             9876543210
  (check (tol (microseconds->sn    9000123))	=> '(9 #e123e3))
  (check (tol (microseconds->su    9000123))	=> '(9 123))
  (check (tol (microseconds->sm    9000123))	=> '(9 #e123e-3))

;;;                             9876543210
  (check (tol (nanoseconds->sn  9000000123))	=> '(9 123))
  (check (tol (nanoseconds->su  9000000123))	=> '(9 #e123e-3))
  (check (tol (nanoseconds->sm  9000000123))	=> '(9 #e123e-6))

;;;                             9876543210
  (check (tol (milliseconds->sn      -9123))	=> '(-9 #e-123e6))
  (check (tol (milliseconds->su      -9123))	=> '(-9 #e-123e3))
  (check (tol (milliseconds->sm      -9123))	=> '(-9 -123))

;;;                             9876543210
  (check (tol (microseconds->sn   -9000123))	=> '(-9 #e-123e3))
  (check (tol (microseconds->su   -9000123))	=> '(-9 -123))
  (check (tol (microseconds->sm   -9000123))	=> '(-9 #e-123e-3))

;;;                             9876543210
  (check (tol (nanoseconds->sn -9000000123))	=> '(-9 -123))
  (check (tol (nanoseconds->su -9000000123))	=> '(-9 #e-123e-3))
  (check (tol (nanoseconds->sm -9000000123))	=> '(-9 #e-123e-6))

  #t)


(parametrise ((check-test-name	'multi-conversion))

  (define-syntax tol
    (syntax-rules ()
      ((_ ?form)
       (call-with-values (lambda () ?form) list))))

  (check
      (tol (smun->sn 1 2 3 4))
    => '(1 2003004))

  (check
      (tol (sn->smun 1 2003004))
    => '(1 2 3 4))

  #t)


(parametrise ((check-test-name	'arithmetics))

  (define-syntax tol
    (syntax-rules ()
      ((_ ?form)
       (call-with-values (lambda () ?form) list))))

  (check
      (tol (sn-add 1 2 3 4))
    => '(4 6))

  (check
      (tol (sn-sub 1 2 3 4))
    => '(-2 -2))

  #t)


(parametrise ((check-test-name	'comparison))

  (check (sn< 1 2 3 4)		=> #t)
  (check (sn< 3 4 1 2)		=> #f)
  (check (sn< 1 2 1 4)		=> #t)
  (check (sn< 1 4 1 2)		=> #f)
  (check (sn< 1 2 1 2)		=> #f)
  (check (sn< -1 2 1 2)		=> #t)
  (check (sn< -1 -2 -1 2)	=> #t)
  (check (sn< -1 2 -1 -2)	=> #f)

  (check (sn<= 1 2 3 4)		=> #t)
  (check (sn<= 3 4 1 2)		=> #f)
  (check (sn<= 1 2 1 4)		=> #t)
  (check (sn<= 1 4 1 2)		=> #f)
  (check (sn<= 1 2 1 2)		=> #t)
  (check (sn<= -1 2 1 2)	=> #t)
  (check (sn<= -1 -2 -1 2)	=> #t)
  (check (sn<= -1 2 -1 -2)	=> #f)

  (check (sn> 1 2 3 4)		=> #f)
  (check (sn> 3 4 1 2)		=> #t)
  (check (sn> 1 2 1 4)		=> #f)
  (check (sn> 1 4 1 2)		=> #t)
  (check (sn> 1 2 1 2)		=> #f)
  (check (sn> -1 2 1 2)		=> #f)
  (check (sn> -1 -2 -1 2)	=> #f)
  (check (sn> -1 2 -1 -2)	=> #t)

  (check (sn>= 1 2 3 4)		=> #f)
  (check (sn>= 3 4 1 2)		=> #t)
  (check (sn>= 1 2 1 4)		=> #f)
  (check (sn>= 1 4 1 2)		=> #t)
  (check (sn>= 1 2 1 2)		=> #t)
  (check (sn>= -1 2 1 2)	=> #f)
  (check (sn>= -1 -2 -1 2)	=> #f)
  (check (sn>= -1 2 -1 -2)	=> #t)

  #t)


(parametrise ((check-test-name	'utc))

  (define-syntax doit
    (syntax-rules ()
      ((_ ?utc-seconds ?leap)
       (begin
	 (check
	     (utc->tai ?utc-seconds)
	   => (+ ?utc-seconds ?leap))
	 (check
	     (utc->tai (+ 1 ?utc-seconds))
	   => (+ 1 ?utc-seconds ?leap))
	 (check
	     (tai->utc (+ ?utc-seconds ?leap))
	   => ?utc-seconds)
	 (check
	     (tai->utc (+ 1 ?utc-seconds ?leap))
	   => (+ 1 ?utc-seconds))
	 ))))

  (doit 1136073600 33)
  (doit 915148800 32)
  (doit 867715200 31)
  (doit 820454400 30)
  (doit 773020800 29)
  (doit 741484800 28)
  (doit 709948800 27)
  (doit 662688000 26)
  (doit 631152000 25)
  (doit 567993600 24)
  (doit 489024000 23)
  (doit 425865600 22)
  (doit 394329600 21)
  (doit 362793600 20)
  (doit 315532800 19)
  (doit 283996800 18)
  (doit 252460800 17)
  (doit 220924800 16)
  (doit 189302400 15)
  (doit 157766400 14)
  (doit 126230400 13)
  (doit 94694400 12)
  (doit 78796800 11)
  (doit 63072000 10)

  #t)


;;;; done

(check-report)

;;; end of file
