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


(import (nausicaa)
  (times-and-dates seconds)
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


;;;; done

(check-report)

;;; end of file
