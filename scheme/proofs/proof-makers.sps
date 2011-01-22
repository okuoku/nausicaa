;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: proof for makers
;;;Date: Sat May 22, 2010
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


#!r6rs
(import (rnrs)
  (nausicaa language makers))

(define check-count		0)
(define check-success-count	0)
(define check-failure-count	0)

(define-syntax check
  (syntax-rules (=>)
    ((_ ?expr => ?expected-result)
     (check ?expr (=> equal?) ?expected-result))
    ((_ ?expr (=> ?equal) ?expected-result)
     (let ((result	?expr)
	   (expected	?expected-result))
       (set! check-count (+ 1 check-count))
       (if (?equal result expected)
	   (set! check-success-count (+ 1 check-success-count))
	 (begin
	   (set! check-failure-count (+ 1 check-failure-count))
	   (display "test error, expected\n\n")
	   (write expected)
	   (newline)
	   (display "\ngot:\n\n")
	   (write result)
	   (newline)
	   (display "\ntest body:\n\n")
	   (write '(check ?expr (=> ?equal) ?expected-result))
	   (newline)))))
    ))

(define (check-report)
  (display (string-append "*** executed " (number->string check-count)
			  " tests, successful: " (number->string check-success-count)
			  ", failed: "(number->string check-failure-count) "\n")))


(define-maker doit
  list ((alpha	1)
	(beta	2)
	(gamma	3)))

(check
    (doit)
  => '(1 2 3))

(check
    (doit (alpha 10))
  => '(10 2 3))

(check
    (doit (beta 20))
  => '(1 20 3))

(check
    (doit (gamma 30))
  => '(1 2 30))

(check
    (doit (alpha	10)
	  (beta	20))
  => '(10 20 3))

(check
    (doit (alpha	10)
	  (gamma	30))
  => '(10 2 30))

(check
    (doit (gamma	30)
	  (beta	20))
  => '(1 20 30))

(check
    (doit (alpha	10)
	  (beta	20)
	  (gamma	30))
  => '(10 20 30))

(check
    (let ((b 7))
      (doit (beta	(+ 6 (* 2 b)))
	    (alpha	(+ 2 8))))
  => '(10 20 3))


(check-report)

;;; end of file
