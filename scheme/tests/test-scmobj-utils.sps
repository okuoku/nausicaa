;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for helper functions
;;;Date: Thu Nov 20, 2008
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



(import (nausicaa)
  (checks)
  (scmobj)
  (scmobj utils))

(check-set-mode! 'report-failed)
(display "*** testing scmobj utils\n")


;;;; special slots accessors

(let ()
  (define-class <alpha> () :a :b :c)
  (define o (make <alpha>
	      ':a '(1 2 3)
	      ':b '(4 5 6)
	      ':c '(7 8 9)))

  (prepend-to-slot o ':a 10)
  (prepend-to-slot o ':b 20)
  (append-to-slot o ':c 40)

  (check
      (slot-ref o ':a)
    => '(10 1 2 3))

  (check
      (slot-ref o ':b)
    => '(20 4 5 6))

  (check
      (slot-ref o ':c)
    => '(7 8 9 40)))

(let ()
  (define-class <alpha> () :a :b :c)
  (define o (make <alpha>
	      ':a '(1 2 3)
	      ':b '(4 5 6)
	      ':c '(7 8 9)))

  (with-slots-set! o (:a :b) (10 20))
  (with-slots-set! o '(:c) '(30))

  (check
      (slot-ref o ':a)
    => 10)
  (check
      (slot-ref o ':b)
    => 20)
  (check
      (slot-ref o ':c)
    => 30)

  (check
      (with-slots-ref o '(:a :b))
    => '(10 20))
  (check
      (with-slots-ref o '(:a :c))
    => '(10 30))
  )


;;; with slots

(let ()
  (define-class <alpha> () :a :b :c)
  (define A (make <alpha>
	      ':a 1 ':b 2 ':c 3))
  (define B (make <alpha>
	      ':a 4 ':b 5 ':c 6))
  (define C (make <alpha>
	      ':a 7 ':b 8 ':c 9))

  (with-slots ()
    (check
	(slot-ref A ':a)
      => 1))

  (with-slots (((d e f) (:a :b :c) A))
    (check
	(list d e f)
      => '(1 2 3)))

  (with-slots (((d e f) (:a :b :c) A)
	       ((g h i) (:a :b :c) B))
    (check
	(list d e f g h i)
      => '(1 2 3 4 5 6)))

  (with-slots (((d e f) (:a :b :c) A)
	       ((g h i) (:a :b :c) B)
	       ((l m n) (:a :b :c) C))
    (check
	(list d e f g h i l m n)
      => '(1 2 3 4 5 6 7 8 9)))
  )


;;; done

(check-report)

;;; end of file
