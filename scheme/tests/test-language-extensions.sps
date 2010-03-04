;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for (language-extensions)
;;;Date: Wed Nov 19, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008-2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing simple language extensions\n")


;;;; tests for: BEGIN0.

(check
    (begin0
	(list 1 2)
      (list 3 4))
  => '(1 2))

(check
    (call-with-values
	(lambda ()
	  (begin0
	      (values 1 2)
	    (values 3 4)))
      (lambda (a b)
	(list a b)))
  => '(1 2))

;;; --------------------------------------------------------------------

(check
    (begin0-let ((a 123))
      (list 1 2 3))
  => 123)

(check
    (begin0-let ((a 123))
      (set! a 456))
  => 456)

(check
    (let-values (((d e f) (begin0-let (((a b c) (values 1 2 3)))
			    (list 'a 'b 'c))))
      (list d e f))
  => '(1 2 3))


;;;; tests for iterators

(check
    (with-result
     (dotimes (i 3)
       1	; shooting the breeze
       2	; shooting the breeze
       (add-result i)))
  => '(#f (0 1 2)))

(check
    (with-result
     (dotimes (i 3 (+ 2 4))
       1	; shooting the breeze
       2	; shooting the breeze
       (add-result i)))
  => '(6 (0 1 2)))

; ------------------------------------------------------------

(check
    (with-result
     (dolist (i '(1 2 3) (+ 2 4))
       1	; shooting the breeze
       2	; shooting the breeze
       (add-result i)))
  => '(6 (1 2 3)))

(check
    (with-result
     (dolist (i '(1 2 3))
       1	; shooting the breeze
       2	; shooting the breeze
       (add-result i)))
  => '(#f (1 2 3)))

; ------------------------------------------------------------

(check
    (with-result
     (loop-upon-list (item '(1 2 3 4))
	 (break-when #f)
       (+ 1 2)	; shooting the breeze
       (+ 3 4)	; shooting the breeze
       (add-result item)))
  => '(#f (1 2 3 4)))

(check
    (with-result
     (loop-upon-list (item '(1 2 3 4) (+ 2 4))
	 (break-when #f)
       (+ 1 2)	; shooting the breeze
       (+ 3 4)	; shooting the breeze
       (add-result item)))
  => '(6 (1 2 3 4)))

(check
    (with-result
     (loop-upon-list (item '(1 2 3 4))
	 (break-when (= item 3))
       (+ 1 2)	; shooting the breeze
       (+ 3 4)	; shooting the breeze
       (add-result item)))
  => '(#f (1 2)))


;;;; ensure

(check
    (with-result
     (let ((flag 0))
       (ensure (= flag 1)
	   (by
	    (add-result 1)
	    (add-result 2))
	 (else
	  (add-result 3)
	  999))))
  => '(999 (1 2 3)))

(check
    (with-result
     (let ((flag 0))
       (ensure (= flag 1)
	   (by
	    (add-result 1)
	    (add-result 2))
	 (else
	  (add-result 3)
	  999))))
  => '(999 (1 2 3)))

(check
    (with-result
     (let ((flag 0))
       (ensure (= flag 1)
	   (by
	    (add-result 1)
	    (add-result 2)
	    (set! flag 1)
	    999)
	 (else
	  (add-result 3)
	  (add-result 4)))))
  => '(999 (1 2)))

(check
    (with-result
     (let ((flag 0))
       (ensure (= flag 1)
	   (by
	    (add-result 1)
	    (add-result 2)
	    (set! flag 1)
	    123)
	 (else
	  (add-result 3)
	  (add-result 4)))))
  => '(123 (1 2)))

(check
    (with-result
     (let ((flag 0))
       (ensure (= flag 1)
	   (by
	    (add-result 1)
	    (add-result 2))
	 (else-by
	  (add-result 3)
	  (add-result 4))
	 (else
	  (add-result 5)
	  (add-result 6)
	  (set! flag 1)
	  999))))
  => '(999 (1 2 3 4 5 6)))

(check
    (with-result
     (let ((flag 0))
       (ensure (= flag 1)
	   (by
	    (add-result 1)
	    (add-result 2))
	 (else-by
	  (add-result 3)
	  (add-result 4)
	  (set! flag 1)
	  999)
	 (else
	  (add-result 5)
	  (add-result 6)))))
  => '(999 (1 2 3 4)))

(check
    (with-result
     (let ((flag 0))
       (ensure (= flag 1)
	   (by
	    (add-result 1)
	    (add-result 2)
	    (set! flag 1)
	    999)
	 (else-by
	  (add-result 3)
	  (add-result 4))
	 (else
	  (add-result 5)
	  (add-result 6)))))
  => '(999 (1 2)))

(check
    (with-result
     (let ((flag 0))
       (ensure (= flag 1)
	   (by
	    (add-result 1)
	    (add-result 2))
	 (else-by
	  (add-result 3)
	  (add-result 4))
	 (else-by
	  (add-result 5)
	  (add-result 6))
	 (else
	  (add-result 7)
	  (add-result 8)
	  (set! flag 1)
	  999))))
  => '(999 (1 2 3 4 5 6 7 8)))

(check
    (with-result
     (let ((flag 0))
       (ensure (= flag 1)
	   (by
	    (add-result 1)
	    (add-result 2))
	 (else-by
	  (add-result 3)
	  (add-result 4))
	 (else-by
	  (add-result 5)
	  (add-result 6)
	  (set! flag 1)
	  999)
	 (else
	  (add-result 7)
	  (add-result 8)))))
  => '(999 (1 2 3 4 5 6)))

(check
    (with-result
     (let ((flag 0))
       (ensure (= flag 1)
	   (by
	    (add-result 1)
	    (add-result 2))
	 (else-by
	  (add-result 3)
	  (add-result 4)
	  (set! flag 1)
	  999)
	 (else-by
	  (add-result 5)
	  (add-result 6))
	 (else
	  (add-result 7)
	  (add-result 8)))))
  => '(999 (1 2 3 4)))

(check
    (with-result
     (let ((flag 0))
       (ensure (= flag 1)
	   (by
	    (add-result 1)
	    (add-result 2)
	    (set! flag 1)
	    999)
	 (else-by
	  (add-result 3)
	  (add-result 4))
	 (else-by
	  (add-result 5)
	  (add-result 6))
	 (else
	  (add-result 7)
	  (add-result 8)))))
  => '(999 (1 2)))


;;;; stuff

(check
    ((recursion (loop n)
       (if (zero? n)
	   1
	 (* n (loop (- n 1)))))
     5)
  => 120)


(check
    (let ()
      (define-values (a b c)
	#t
	(values 1 2 3))
      (list a b c))
  => '(1 2 3))

(check
    (let ()
      (define-values (a)
	#t
	(values 1))
      a)
  => 1)

(check
    (let ()
      (define-values (a)
	#t
	1)
      a)
  => 1)



;;;; done

(check-report)

;;; end of file
