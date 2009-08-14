;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: test for R6RS compliance and extensions
;;;Date: Mon Dec  8, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008, 2009 Marco Maggi <marcomaggi@gna.org>
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


;;;; setup

(import (nausicaa)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing some R6RS compliance and Nausicaa extensions\n")


;;;; numbers

(check
    (integer? (equal-hash '#(1 2 3)))
  => #t)

;;; --------------------------------------------------------------------

(check
    (finite? 123)
  => #t)

(check
    (finite? +inf.0)
  => #f)

(check
    (finite? 123+456i)
  => #t)

(check
    (finite? +inf.0+456i)
  => #f)

(check
    (finite? 123+inf.0i)
  => #f)

;;; --------------------------------------------------------------------

(check
    (infinite? 123)
  => #f)

(check
    (infinite? +inf.0)
  => #t)

(check
    (infinite? 123+456i)
  => #f)

(check
    (infinite? +inf.0+456i)
  => #t)

(check
    (infinite? 123+inf.0i)
  => #t)

;;; --------------------------------------------------------------------

(check
    (nan? 123)
  => #f)

(check
    (nan? +inf.0)
  => #f)

(check
    (nan? 123+456i)
  => #f)

(check
    (nan? +inf.0+456i)
  => #f)

(check
    (nan? 123+inf.0i)
  => #f)

(check
    (nan? +nan.0)
  => #t)

(check
    (nan? 123+nan.0i)
  => #t)

(check
    (nan? +nan.0+456i)
  => #t)

;;; --------------------------------------------------------------------

(check
    (rational? 1.2)
  => #f)

(check
    (rational? 12/10)
  => #t)

(check
    (rational-valued? 1.2)
  => #t)

(check
    (rational-valued? 12/10)
  => #t)

;;; --------------------------------------------------------------------

(check
    (equal? 1 2)
  => #f)

(check
    (equal? 1 1)
  => #t)

(check (equal? +nan.0 +nan.0)	=> #t)
(check (equal? +nan.0 -nan.0)	=> #t)
(check (equal? -nan.0 +nan.0)	=> #t)
(check (equal? -nan.0 -nan.0)	=> #t)

(check (equal? +nan.0 (string->number "+nan.0"))	=> #t)
(check (equal? +nan.0 (string->number "-nan.0"))	=> #t)
(check (equal? -nan.0 (string->number "+nan.0"))	=> #t)
(check (equal? -nan.0 (string->number "-nan.0"))	=> #t)

(check (eqv? +nan.0 +nan.0)	=> #t)
(check (eqv? +nan.0 -nan.0)	=> #t)
(check (eqv? -nan.0 +nan.0)	=> #t)
(check (eqv? -nan.0 -nan.0)	=> #t)

(check (eqv? +nan.0 (string->number "+nan.0"))	=> #t)
(check (eqv? +nan.0 (string->number "-nan.0"))	=> #t)
(check (eqv? -nan.0 (string->number "+nan.0"))	=> #t)
(check (eqv? -nan.0 (string->number "-nan.0"))	=> #t)

;;; --------------------------------------------------------------------

(check (max 1 +nan.0) => 1.0)
(check (nan? (min 1 +nan.0)) => #t)
(check (* 0 +nan.0) => +nan.0)

;;; --------------------------------------------------------------------

(check
    (=)
  => #t)

(check
    (= 1)
  => #t)

(check
    (= 1 2)
  => #f)

(check
    (= 1 1)
  => #t)


;;;; parameters

(define alpha
  (make-parameter 123))

(define beta
  (make-parameter 0
    (lambda (num)
      (unless (number? num)
	(raise 'woppa))
      num)))

;;; --------------------------------------------------------------------

(check
    (alpha)
  => 123)

(check
    (parameterize ((alpha 456))
      (list (alpha)))
  => '(456))

(check
    (parameterize ((alpha 456))
      (list (alpha)
	    (parameterize ((alpha 789))
	      (alpha))))
  => '(456 789))

;;; --------------------------------------------------------------------

(check
    (beta)
  => 0)

(check
    (parameterise ((beta 456))
      (list (beta)))
  => '(456))

(check
    (parameterise ((beta 456))
      (list (beta)
	    (parameterise ((beta 789))
	      (beta))))
  => '(456 789))

(check
    (guard (exc (else exc))
      (parameterise ((beta 'b))
	(list (beta))))
  => 'woppa)

;;; --------------------------------------------------------------------

(define gamma
  (make-parameter 0
    (lambda (num)
      (unless (number? num)
	(raise 'woppa))
      (case num
	((0) 'zero)
	((1) 'one)
	((2) 'two)))))

(check
    (gamma)
  => 'zero)

(check
    (begin
      (gamma 1)
      (gamma))
  => 'one)

(check
    (begin
      (gamma 2)
      (gamma))
  => 'two)


;;;; done

(check-report)

;;; end of file
