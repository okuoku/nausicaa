;;;
;;;Part of: Nausicaa/SRFI
;;;Contents: tests for parameters
;;;Date: Thu Dec 25, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
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

(import (r6rs)
  (srfi parameters))

(define testnum 0)

(define-syntax check
  (syntax-rules (=>)
    ((_ ?form => ?expected-result)
     (let ((result ?form)
	   (expected-result ?expected-result))
       (if (equal? result expected-result)
	   (begin
	     (display "test number ")
	     (display testnum)
	     (display " success")
	     (newline))
	 (begin
	   (display "test number ")
	   (display testnum)
	   (display " FAILURE expected ")
	   (display expected-result)
	   (display " got ")
	   (display result)
	   (newline)
	   (display "\t")
	   (write (quote ?form))
	   (newline)))
       (set! testnum (+ 1 testnum))))))

(newline)
(display "*** testing parameters ...")
(newline)



;;;; code

(define alpha
  (make-parameter 123))

(define beta
  (make-parameter 0
    (lambda (num)
      (unless (number? num)
	(raise 'woppa))
      num)))

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

(check
    (beta)
  => 0)

(check
    (parameterize ((beta 456))
      (list (beta)))
  => '(456))

(check
    (parameterize ((beta 456))
      (list (beta)
	    (parameterize ((beta 789))
	      (beta))))
  => '(456 789))

(check
    (guard (exc (else exc))
      (parameterize ((beta 'b))
	(list (beta))))
  => 'woppa)



;;;; done

(display "*** testing parameters end")
(newline)
(newline)


;;; end of file
