;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: proofs for the classes library
;;;Date: Tue May 25, 2010
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


(import (rnrs)
  (classes))

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


;;;; simple

(let ()

  (define-class <alpha>
    (fields (mutable a)))

  (check
      (let ((o (make-<alpha> 123)))
	(<alpha>-a o))
    => 123)

  #f)

(let ()

  (define-class (<alpha> make-<alpha> <alpha>?)
    (fields (mutable a)))

  (check
      (let ((o (make-<alpha> 123)))
	(<alpha>-a o))
    => 123)

  #f)


;;;; inherit

(let ()		;inherit with INHERIT

  (define-class <alpha>
    (inherit <top>)
    (nongenerative alpha)
    (fields (mutable a)))

  (define-class <beta>
    (inherit <alpha>)
    (protocol (lambda (alpha-maker)
		(lambda (a b)
		  (let ((beta-maker (alpha-maker a)))
		    (beta-maker b)))))
    (sealed #t)
    (opaque #t)
    (nongenerative test:beta)
    (fields (immutable b)))

  (check
      (let ((o (make-<beta> 1 2)))
	(list (<alpha>-a o)
	      (<beta>-b o)
	      ))
    => '(1 2))

  #f)


;;;; done

(check-report)

;;; end of file
