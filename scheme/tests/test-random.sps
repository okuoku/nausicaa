;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for random
;;;Date: Thu Jun 25, 2009
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
  (checks)
  (random)
  (lists)
  (strings)
  (char-sets)
  (vectors))

(check-set-mode! 'report-failed)
(display "*** testing random\n")


(parameterise ((check-test-name 'device))

  (check
      (let* ((len 10)
	     (bv  (random-device-bytevector len)))
	(and (bytevector? bv)
	     (= len (bytevector-length bv))))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((len 10)
	     (bv  (urandom-device-bytevector len)))
	(and (bytevector? bv)
	     (= len (bytevector-length bv))))
    => #t)

  (check
      (let* ((len 5000)
	     (bv  (urandom-device-bytevector len)))
	(and (bytevector? bv)
	     (= len (bytevector-length bv))))
    => #t)

  )


(parameterise ((check-test-name 'numbers))

  (check-for-true (integer? (random-integer 10)))
  (check-for-true (real? (random-real)))

  (check-for-true
   (vector? (unfold-random-numbers/vector (lambda () (random-integer 10)) 10)))

  (check-for-true
   (vector-every integer? (unfold-random-numbers/vector (lambda () (random-integer 10)) 10)))

  (check-for-true
   (vector-every positive? (unfold-random-numbers/vector (lambda () (random-integer 10)) 10)))

;;; --------------------------------------------------------------------

  (let* ((source  (make-random-source))
	 (integer (random-source-integers-maker source)))

    (check-for-true (integer? (integer 100)))
    (check-for-true (positive? (integer 100)))
    (check-for-true (let ((n (integer 100)))
		      (and (<= 0 n) (< n (expt 2 32)))))

;;; --------------------------------------------------------------------

    (check-for-true (list? (unfold-random-numbers (lambda () (integer 100)) 10)))
    (check-for-true (every integer? (unfold-random-numbers (lambda () (integer 100)) 10)))
    (check-for-true (every positive? (unfold-random-numbers (lambda () (integer 100)) 10)))

;;; --------------------------------------------------------------------

    (check-for-true (vector? (unfold-random-numbers/vector (lambda () (integer 100)) 10)))
    (check-for-true (vector-every integer? (unfold-random-numbers/vector (lambda () (integer 100)) 10)))
    (check-for-true (vector-every positive? (unfold-random-numbers/vector (lambda () (integer 100)) 10)))

;;; --------------------------------------------------------------------

    (check-for-true (string? (unfold-random-numbers/string (lambda () (integer 100)) 10)))

    ))


(parameterise ((check-test-name 'source))

  (check-for-true
   (let* ((source	(make-random-source))
	  (int-maker	(random-source-integers-maker source)))
     (integer? (int-maker 10))))

  (check-for-true
   (let* ((source	(make-random-source))
	  (int-maker	(random-source-integers-maker source))
	  (n		(int-maker 10)))
     (and (<= 0 n) (< n 10))))

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((source	(make-random-source))
	  (int-maker	(random-source-reals-maker source)))
     (real? (int-maker))))

  (check-for-true
   (let* ((source	(make-random-source))
	  (real-maker	(random-source-reals-maker source))
	  (n		(real-maker)))
     (and (< 0 n) (< n 1))))

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((source-a	(make-random-source))
	  (source-b	(make-random-source))
	  (integer	(random-source-integers-maker source-b)))
     (random-source-seed! source-a (lambda () (integer 100)))
     (let* ((real-maker	(random-source-reals-maker source-a))
	    (n		(real-maker)))
       (and (< 0 n) (< n 1)))))

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((source	(make-random-source))
	  (state	(random-source-state-ref source)))
     (random-source-state-set! source state)
     (let ((int-maker	(random-source-integers-maker source)))
       (integer? (int-maker 10)))))

  )


;;;; examples

(when #t

  (display "Example of random lists:\n")
  (do ((i 0 (+ 1 i)))
      ((= i 5))
    (write (unfold-random-numbers (lambda ()
				    (random-integer 10))
				  10))
    (newline))


  (display "Example of random vectors:\n")
  (do ((i 0 (+ 1 i)))
      ((= i 5))
    (write (unfold-random-numbers/vector (lambda ()
					   (random-integer 10))
					 10))
    (newline))

  (display "Example of random strings:\n")
  (do ((i 0 (+ 1 i)))
      ((= i 5))
    (write (unfold-random-numbers/string (lambda ()
					   (+ 65 (random-integer 21)))
					 10))
    (newline))

  (display "Example of random passwords of printable characters:\n")
  (do ((i 0 (+ 1 i)))
      ((= i 5))
    (display (unfold-random-numbers/string
	      (lambda ()
		(do ((ch (random-integer 127) (random-integer 127)))
		    ((char-set-contains? char-set:ascii/graphic (integer->char ch))
		     ch)))
	      10))
    (newline))
  )


;;;; done

(check-report)

;;; end of file
