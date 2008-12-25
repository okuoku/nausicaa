;;;
;;;Part of: Nausicaa/SRFI
;;;Contents: tests for error-reporting
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

(import (except (r6rs) error)
  (srfi error-reporting))

(define testnum 0)

(define-syntax check
  (syntax-rules (=>)
    ((_ ?form => ?expected-result)
     (let ((result ?form))
       (if (equal? result ?expected-result)
	   (begin
	     (display "test number ")
	     (display testnum)
	     (display " success")
	     (newline))
	 (begin
	   (display "test number ")
	   (display testnum)
	   (display " FAILURE")
	   (newline)))
       (set! testnum (+ 1 testnum))))))

;; Here we use  EVAL because a syntax violation  error cannot be catched
;; by GUARD, and so it causes the program termination.
(define-syntax check-syntax-violation
  (syntax-rules ()
    ((_ ?form)
     (check
	 (guard (exc (else
;; 		      (write exc)(newline)
;; 		      (write (syntax-violation? exc))(newline)
		      (syntax-violation? exc)))
	   (eval '?form (environment '(rnrs) '(srfi and-let-star))))
       => #t))))


(newline)
(display "*** testing error-reporting ...")
(newline)



;;;; code

(check
    (guard (exc (else #t))
      (error 'wo))
  => #t)


;;;; done

(display "*** testing error-reporting end")
(newline)
(newline)


;;; end of file
