;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for (arrays)
;;;Date: Mon Jul  6, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
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
  (arrays)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing arrays\n")


(parameterise ((check-test-name 'position))

  (let ((pos  (array-position 1 2 3 4 5))
	(pos2 (array-position 8 7 6 5 4)))

    (check (array-position? pos) => #t)
    (check (array-position->string pos) => "#<array-position -- 1 2 3 4 5>")

    (check (assert-array-position pos 'this) => #t)
    (check (guard (exc (else (assertion-violation? exc)))
	     (assert-array-position 123 'this)) => #t)
    (check (guard (exc (else (assertion-violation? exc)))
	     (assert-array-position #t 'this)) => #t)

    (check (assert-array-position/or-false pos 'this) => #t)
    (check (guard (exc (else (assertion-violation? exc)))
	     (assert-array-position/or-false 123 'this)) => #t)
    (check (assert-array-position/or-false #f 'this) => #t)

    (check
	(call-with-string-output-port
	    (lambda (port)
	      (array-position-display pos port)))
      => "#<array-position -- 1 2 3 4 5>")

    (check
	(call-with-string-output-port
	    (lambda (port)
	      (array-position-write pos port)))
      => "#(1 2 3 4 5)")

    )

  )


(parameterise ((check-test-name 'shape))

  (let ((shape  (array-shape '#(1 2 3 4 5)
			     '#(6 7 8 9 10)))
	(shape2 (array-shape '#(5 4 3 2 1)
			     '#(10 9 8 7 6)))
	(pos    (array-position  2 3 4 5 6))
	(pos2   (array-position  2 3 4 20 6)))

    (check (array-shape? shape) => #t)
    (check (array-shape-number-of-dimensions shape) => 5)
    (check (array-shape-number-of-elements shape) => (+ (- 6 1)
							(- 7 2)
							(- 8 3)
							(- 9 4)
							(- 10 5)))
    (check (array-shape->string shape) => "#<array-shape -- 1 2 3 4 5 -- 6 7 8 9 10>")

    (check (array-shape=? shape shape) => #t)
    (check (array-shape=? shape shape2) => #f)

    (check (array-shape-contains? shape pos) => #t)
    (check (array-shape-contains? shape pos2) => #f)

    (check (assert-array-shape shape 'this) => #t)
    (check (guard (exc (else (assertion-violation? exc)))
	     (assert-array-shape 123 'this)) => #t)
    (check (guard (exc (else (assertion-violation? exc)))
	     (assert-array-shape #t 'this)) => #t)

    (check (assert-array-shape/or-false shape 'this) => #t)
    (check (guard (exc (else (assertion-violation? exc)))
	     (assert-array-shape/or-false 123 'this)) => #t)
    (check (assert-array-shape/or-false #f 'this) => #t)

    (check
	(call-with-string-output-port
	    (lambda (port)
	      (array-shape-display shape port)))
      => "#<array-shape -- 1 2 3 4 5 -- 6 7 8 9 10>")

    (check
	(call-with-string-output-port
	    (lambda (port)
	      (array-shape-write shape port)))
      => "(array-shape '#(1 2 3 4 5) '#(6 7 8 9 10))")

    (check-for-true (array-supershape? shape shape))
    (check-for-false (array-supershape? shape shape2))
    (check-for-false (array-supershape?/strict shape shape))
    (check-for-false (array-supershape?/strict shape shape2))

    (check-for-true (array-subshape? shape shape))
    (check-for-false (array-subshape? shape2 shape))
    (check-for-false (array-subshape?/strict shape shape))
    (check-for-false (array-subshape?/strict shape2 shape)))

  (let ((shape  (array-shape '#(0 0 0 0 0)
			     '#(5 6 7 8 9)))
	(shape2 (array-shape '#(0 0 0 0 0)
			     '#(4 5 6 7 8)))
	(shape3 (array-shape '#(0 0 0 0 0)
			     '#(5 6 2 8 9))))

    (check-for-true (array-supershape? shape shape2))
    (check-for-true (array-supershape? shape shape3))
    (check-for-true (array-supershape?/strict shape shape2))
    (check-for-true (array-supershape?/strict shape shape3))

    (check-for-true (array-subshape? shape2 shape))
    (check-for-true (array-subshape? shape3 shape))
    (check-for-true (array-subshape?/strict shape2 shape))
    (check-for-true (array-subshape?/strict shape3 shape)))
  )


(parameterise ((check-test-name 'array))

  (let ((array  (make-array (array-shape '#(1 2 3 4 5)
					 '#(6 7 8 9 10))))
	(array2	(make-array (array-shape '#(5 4 3 2 1)
					 '#(10 9 8 7 6))))
	(array3 (make-array (array-shape '#(0 0 0 0)
					 '#(3 4 5 6))))
	(array4 (make-array (array-shape '#(0 0 0) ;this is small, good for string representation
					 '#(2 3 4))))
	(array5 (array (array-shape '#(0 0 0) '#(2 3 4))
		        1  2  3  4  5  6  7  8  9 10
		       11 12 13 14 15 16 17 18 19 20
		       21 22 23 24))
	(pos    (array-position  2 3 4 5 6))
	(pos2   (array-position  2 3 4 20 6)))

    (check (array-shape? array) => #t)
    (check (array-shape-number-of-dimensions array) => 5)
    (check (array-shape-number-of-elements array) => (+ (- 6 1)
							(- 7 2)
							(- 8 3)
							(- 9 4)
							(- 10 5)))

    (check-for-true (array? array3))
    (check (array-shape-number-of-dimensions array3) => 4)
    (check (array-shape-number-of-elements array3) => (+ (- 3 0)
							 (- 4 0)
							 (- 5 0)
							 (- 6 0)))
    (check (array-shape->string array) => "#<array-shape -- 1 2 3 4 5 -- 6 7 8 9 10>")

    (check (array-shape=? array array) => #t)
    (check (array-shape=? array array2) => #f)

    (check (array-shape-contains? array pos) => #t)
    (check (array-shape-contains? array pos2) => #f)

    (check (assert-array-shape array 'this) => #t)
    (check (guard (exc (else (assertion-violation? exc)))
	     (assert-array-shape 123 'this)) => #t)
    (check (guard (exc (else (assertion-violation? exc)))
	     (assert-array-shape #t 'this)) => #t)

    (check (assert-array-shape/or-false array 'this) => #t)
    (check (guard (exc (else (assertion-violation? exc)))
	     (assert-array-shape/or-false 123 'this)) => #t)
    (check (assert-array-shape/or-false #f 'this) => #t)

    (check
	(call-with-string-output-port
	    (lambda (port)
	      (array-shape-display array port)))
      => "#<array-shape -- 1 2 3 4 5 -- 6 7 8 9 10>")

    (check
	(call-with-string-output-port
	    (lambda (port)
	      (array-shape-write array port)))
      => "(array-shape '#(1 2 3 4 5) '#(6 7 8 9 10))")

    (check
	(call-with-string-output-port
	    (lambda (port)
	      (array-display (lambda (element)
			       (call-with-string-output-port
				   (lambda (port)
				     (display element port))))
			     array4 port)))
      => "#<array #<array-shape -- 0 0 0 -- 2 3 4> #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f >")

    (check
	(call-with-string-output-port
	    (lambda (port)
	      (array-write (lambda (element)
			     (call-with-string-output-port
				 (lambda (port)
				   (display element port))))
			   array4 port)))
      => "(array (array-shape '#(0 0 0) '#(2 3 4)) #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f )")

    (check
	(call-with-string-output-port
	    (lambda (port)
	      (array-display (lambda (element)
			       (call-with-string-output-port
				   (lambda (port)
				     (display element port))))
			     array5 port)))
      => "#<array #<array-shape -- 0 0 0 -- 2 3 4> 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 >")

    (check (array-ref array5 (array-position  0 0 0)) => 1)
    (check (array-ref array5 (array-position  0 0 1)) => 2)
    (check (array-ref array5 (array-position  0 0 2)) => 3)
    (check (array-ref array5 (array-position  0 0 3)) => 4)
    (check (array-ref array5 (array-position  0 1 0)) => 5)
    (check (array-ref array5 (array-position  0 1 1)) => 6)
    (check (array-ref array5 (array-position  0 1 2)) => 7)
    (check (array-ref array5 (array-position  0 1 3)) => 8)
    (check (array-ref array5 (array-position  0 2 0)) => 9)
    (check (array-ref array5 (array-position  0 2 1)) => 10)
    (check (array-ref array5 (array-position  0 2 2)) => 11)
    (check (array-ref array5 (array-position  0 2 3)) => 12)
    (check (array-ref array5 (array-position  1 0 0)) => 13)
    (check (array-ref array5 (array-position  1 0 1)) => 14)
    (check (array-ref array5 (array-position  1 0 2)) => 15)
    (check (array-ref array5 (array-position  1 0 3)) => 16)
    (check (array-ref array5 (array-position  1 1 0)) => 17)
    (check (array-ref array5 (array-position  1 1 1)) => 18)
    (check (array-ref array5 (array-position  1 1 2)) => 19)
    (check (array-ref array5 (array-position  1 1 3)) => 20)
    (check (array-ref array5 (array-position  1 2 0)) => 21)
    (check (array-ref array5 (array-position  1 2 1)) => 22)
    (check (array-ref array5 (array-position  1 2 2)) => 23)
    (check (array-ref array5 (array-position  1 2 3)) => 24)

    (let ((view (array-view array5 (lambda (position)
				     (vector 1
					     (vector-ref position 0)
					     (vector-ref position 1))))))
      (check (array-ref view (array-position  0 0)) => 13)
      (check (array-ref view (array-position  0 1)) => 14)
      (check (array-ref view (array-position  0 2)) => 15)
      (check (array-ref view (array-position  0 3)) => 16)
      (check (array-ref view (array-position  1 0)) => 17)
      (check (array-ref view (array-position  1 1)) => 18)
      (check (array-ref view (array-position  1 2)) => 19)
      (check (array-ref view (array-position  1 3)) => 20)
      (check (array-ref view (array-position  2 0)) => 21)
      (check (array-ref view (array-position  2 1)) => 22)
      (check (array-ref view (array-position  2 2)) => 23)
      (check (array-ref view (array-position  2 3)) => 24))

    (check-for-true (array-supershape? array array))
    (check-for-false (array-supershape? array array2))
    (check-for-false (array-supershape?/strict array array))
    (check-for-false (array-supershape?/strict array array2))

    (check-for-true (array-subshape? array array))
    (check-for-false (array-subshape? array2 array))
    (check-for-false (array-subshape?/strict array array))
    (check-for-false (array-subshape?/strict array2 array)))

  (let ((array  (make-array (array-shape '#(0 0 0 0 0)
					 '#(5 6 7 8 9))))
	(array2 (make-array (array-shape '#(0 0 0 0 0)
					 '#(4 5 6 7 8))))
	(array3 (make-array (array-shape '#(0 0 0 0 0)
					 '#(5 6 2 8 9)))))

    (check-for-true (array-supershape? array array2))
    (check-for-true (array-supershape? array array3))
    (check-for-true (array-supershape?/strict array array2))
    (check-for-true (array-supershape?/strict array array3))

    (check-for-true (array-subshape? array2 array))
    (check-for-true (array-subshape? array3 array))
    (check-for-true (array-subshape?/strict array2 array))
    (check-for-true (array-subshape?/strict array3 array)))

  )


;;;; done

(check-report)

;;; end of file
