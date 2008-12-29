;;;
;;;Part of: Nausicaa/SRFI
;;;Contents: tests for srfi lists
;;;Date: Mon Dec 29, 2008
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
  (check-lib)
  (list-lib))

(check-set-mode! 'report-failed)

(define numbers '(0 1 2 3 4 5 6 7 8 9))


;;;; constructors

(check
    (xcons 1 2)
  => '(2 . 1))

(check
    (make-list 4 'c)
  => '(c c c c))

(check
    (list-tabulate 4 (lambda (i)
		       (cons i 'a)))
  => '((0 . a)
       (1 . a)
       (2 . a)
       (3 . a)))

(check
    (list-copy numbers)
  => numbers)

(check
    (cons* 1 2 3 4 '(5 6 7 8))
  => '(1 2 3 4 5 6 7 8))

(check
    (iota 5 10)
  => '(10 11 12 13 14))

(check
    (iota 5)
  => '(0 1 2 3 4))

(check
    (iota 5 10 5)
  => '(10 15 20 25 30))

(check
    (iota -5 10 5)
  => '())

(check
    (let ((ell (circular-list 1 2)))
      (list (car ell)
	    (cadr ell)
	    (caddr ell)
	    (cadddr ell)))
  => '(1 2 1 2))



;;;; predicates

(check
    (proper-list? '())
  => #t)

(check
    (proper-list? '(1 2 3))
  => #t)

(check
    (proper-list? '(1 2 3 . 4))
  => #f)

(check
    (proper-list? (circular-list 1 2))
  => #f)

(check
    (proper-list? '(1 . 2))
  => #f)

;;; --------------------------------------------------------------------

(check
    (circular-list? '())
  => #f)

(check
    (circular-list? '(1 2 3))
  => #f)

(check
    (circular-list? '(1 2 3 . 4))
  => #f)

(check
    (circular-list? (circular-list 1 2))
  => #t)

(check
    (circular-list? '(1 . 2))
  => #f)

;;; --------------------------------------------------------------------

(check
    (dotted-list? '())
  => #f)

(check
    (dotted-list? '(1 2 3))
  => #f)

(check
    (dotted-list? '(1 2 3 . 4))
  => #t)

(check
    (dotted-list? (circular-list 1 2))
  => #f)

(check
    (dotted-list? '(1 . 2))
  => #t)

;;; --------------------------------------------------------------------

(check
    (pair? '(1 . 2))
  => #t)

(check
    (list= numbers numbers)
  => #t)

(check
    (list= = numbers '(0 1 2 3 4))
  => #f)


;;;; selectors

(check
    (take numbers 5)
  => '(0 1 2 3 4))

(check
    (drop '(0 1 2 3 4 5 6 7 8 9) 5)
  => '(5 6 7 8 9))

(check
    (count even? numbers)
  => 5)

(check
    (take-right numbers 5)
  => '(5 6 7 8 9))

(check
    (drop-right numbers 5)
  => '(0 1 2 3 4))

(check
    (call-with-values
	(lambda () (split-at numbers 5))
      list)
  => '((0 1 2 3 4)
       (5 6 7 8 9)))

(check
    (last numbers)
  => 9)


;;;; miscellaneous

(check
    (length+ '(1 2 3 4 5 6))
  => 6)

(check
    (length+ (circular-list 1 2 3 4 5 6))
  => #f)



;;;; folding

(check
    (fold + 0 numbers)
  => 45)

(check
    (reduce + 0 numbers)
  => 45)

(check
    (unfold (lambda (x) (< 5 x))
	    (lambda (x) (* x x))
	    (lambda (x) (+ x 1))
	    1)
  => '(1 4 9 16 25))

(check
    (unfold (lambda (x) (< 5 x))
	    (lambda (x) (* x x))
	    (lambda (x) (+ x 1))
	    1
	    (lambda (x) (- x)))
  => '(1 4 9 16 25 . -6))

(check
    (unfold (lambda (x) #t)
	    (lambda (x) (* x x))
	    (lambda (x) (+ x 1))
	    1
	    (lambda (x) (- x)))
  => -1)

(check
    (unfold null-list? car cdr numbers)
  => numbers)

(check
    (unfold not-pair? car cdr '(1 2 3 4 . 5) values)
  => '(1 2 3 4 . 5))

(check
    (unfold null-list? car cdr '(1 2 3) (lambda (x) '(4 5 6)))
  => '(1 2 3 4 5 6))

;;; --------------------------------------------------------------------

(check
    (unfold-right zero?
		  (lambda (x) (* x x))
		  (lambda (x) (- x 1))
		  5)
  => '(1 4 9 16 25))

(check
    (unfold-right null-list? car cdr '(1 2 3 4 5))
  => '(5 4 3 2 1))

(check
    (unfold-right null-list? car cdr '(3 2 1) '(4 5 6))
  => '(1 2 3 4 5 6))


;;;; filtering

(check
    (call-with-values
	(lambda ()
	  (partition even? numbers))
      list)
  => '((0 2 4 6 8)
       (1 3 5 7 9)))

(check
    (remove! even? (list-copy numbers))
  => '(1 3 5 7 9))


;;;REMOVE  is defined  by the  SRFI  but is  already in  R6RS.  The  two
;;;definitions are different.  Here we test the SRFI one:
(check
    (srfi:remove even? numbers)
  => '(1 3 5 7 9))
;;and here we test the R6RS one:
(check
    (remove 8 numbers)
  => '(0 1 2 3 4 5 6 7 9))



;;;; done

(check-report)

;;; end of file
