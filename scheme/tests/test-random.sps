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


(parameterise ((check-test-name 'no-fuss))

  (check-for-true (integer? (random-integer 10)))
  (check-for-true (real? (random-real)))

  )


(parameterise ((check-test-name 'default-source))

  (let* ((make-integer	(random-source-integers-maker default-random-source)))

    (define (integer) (make-integer 100))

    (check-for-true (integer? (integer)))
    (check-for-true (positive? (integer)))
    (check-for-true (let ((n (integer)))
		      (and (<= 0 n) (< n 100)))))

;;; --------------------------------------------------------------------

  (let* ((make-real	(random-source-reals-maker default-random-source)))

    (check-for-true (real? (make-real)))
    (check-for-true (let ((n (make-real)))
		      (and (< 0 n) (< n 1)))))

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((source-b	((random-source-maker)))
	  (make-integer	(random-source-integers-maker source-b)))
     (define (integer) (make-integer 100))
     (random-source-seed! default-random-source integer)
     (let* ((make-real	(random-source-reals-maker default-random-source))
	    (n		(make-real)))
       (and (< 0 n) (< n 1)))))

;;; --------------------------------------------------------------------

  (let* ((bytevector-maker (random-source-bytevectors-maker default-random-source))
	 (obj (bytevector-maker 50)))
    ;;(write obj)(newline)
    (check-for-true (bytevector? obj)))

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((state	(random-source-state-ref default-random-source)))
     (random-source-state-set! default-random-source state)
     (let ((make-integer (random-source-integers-maker default-random-source)))
       (integer? (make-integer 10)))))

  )


(parameterise ((check-test-name 'default-source-parameter))

  (let* ((source	((random-source-maker)))
	 (make-integer	(random-source-integers-maker source)))

    (define (integer) (make-integer 100))

    (check-for-true (integer? (integer)))
    (check-for-true (positive? (integer)))
    (check-for-true (let ((n (integer)))
		      (and (<= 0 n) (< n 100)))))

;;; --------------------------------------------------------------------

  (let* ((source	((random-source-maker)))
	 (make-real	(random-source-reals-maker source)))

    (check-for-true (real? (make-real)))
    (check-for-true (let ((n (make-real)))
		      (and (< 0 n) (< n 1)))))

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((source-a	((random-source-maker)))
	  (source-b	((random-source-maker)))
	  (make-integer	(random-source-integers-maker source-b)))
     (define (integer) (make-integer 100))
     (random-source-seed! source-a integer)
     (let* ((make-real	(random-source-reals-maker source-a))
	    (n		(make-real)))
       (and (< 0 n) (< n 1)))))

;;; --------------------------------------------------------------------

  (let* ((source		((random-source-maker)))
	 (bytevector-maker	(random-source-bytevectors-maker source))
	 (obj (bytevector-maker 50)))
    ;;(write obj)(newline)
    (check-for-true (bytevector? obj)))

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((source	((random-source-maker)))
	  (state	(random-source-state-ref source)))
     (random-source-state-set! source state)
     (let ((make-integer (random-source-integers-maker source)))
       (integer? (make-integer 10)))))

  )


(parameterise ((check-test-name 'mrg32k3a))

  (let* ((source	(make-random-source/mrg32k3a))
	 (make-integer	(random-source-integers-maker source)))

    (define (integer) (make-integer 100))

    (check-for-true (integer? (integer)))
    (check-for-true (positive? (integer)))
    (check-for-true (let ((n (integer)))
		      (and (<= 0 n) (< n 100)))))

;;; --------------------------------------------------------------------

  (let* ((source	(make-random-source/mrg32k3a))
	 (make-real	(random-source-reals-maker source)))

    (check-for-true (real? (make-real)))
    (check-for-true (let ((n (make-real)))
		      (and (< 0 n) (< n 1)))))

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((source-a	(make-random-source/mrg32k3a))
	  (source-b	(make-random-source/mrg32k3a))
	  (make-integer	(random-source-integers-maker source-b)))
     (define (integer) (make-integer 100))
     (random-source-seed! source-a integer)
     (let* ((make-real	(random-source-reals-maker source-a))
	    (n		(make-real)))
       (and (< 0 n) (< n 1)))))

;;; --------------------------------------------------------------------

  (let* ((source		(make-random-source/mrg32k3a))
	 (bytevector-maker	(random-source-bytevectors-maker source))
	 (obj (bytevector-maker 50)))
    ;;(write obj)(newline)
    (check-for-true (bytevector? obj)))

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((source	(make-random-source/mrg32k3a))
	  (state	(random-source-state-ref source)))
     (random-source-state-set! source state)
     (let ((make-integer (random-source-integers-maker source)))
       (integer? (make-integer 10)))))

  )


(parameterise ((check-test-name 'device))

  (let* ((source	(make-random-source/device))
	 (make-integer	(random-source-integers-maker source)))

    (define (integer) (make-integer 100))

    (check-for-true (integer? (integer)))
    (check-for-true (positive? (integer)))
    (check-for-true (let ((n (integer)))
		      (and (<= 0 n) (< n 100)))))

;;; --------------------------------------------------------------------

  (let* ((source	(make-random-source/device))
	 (make-real	(random-source-reals-maker source)))

    (check-for-true (real? (make-real)))
    (check-for-true (let ((n (make-real)))
		      (and (< 0 n) (< n 1)))))

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((source-a	(make-random-source/device))
	  (source-b	(make-random-source/device))
	  (make-integer	(random-source-integers-maker source-b)))
     (define (integer) (make-integer 100))
     (random-source-seed! source-a (let ((count 0))
				     (lambda ()
				       (and (< count 100)
					    (begin
					      (set! count (+ 1 count))
					      (make-integer 5000))))))
     (let* ((make-real	(random-source-reals-maker source-a))
	    (n		(make-real)))
       (and (< 0 n) (< n 1)))))

;;; --------------------------------------------------------------------

  (let* ((source		(make-random-source/device))
	 (bytevector-maker	(random-source-bytevectors-maker source))
	 (obj (bytevector-maker 50)))
    ;;(write obj)(newline)
    (check-for-true (bytevector? obj)))

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((source	(make-random-source/device))
	  (state	(random-source-state-ref source)))
     (random-source-state-set! source state)
     (let ((make-integer (random-source-integers-maker source)))
       (integer? (make-integer 10)))))

  )


(parameterise ((check-test-name 'device-low-level))

;;;This will block if "/dev/random" has not enough random bytes.
;;   (check
;;       (parameterise ((random-device-cache-length 5))
;; 	(let* ((len 1)
;; 	       (bv  (random-device-bytevector len)))
;; 	  (and (bytevector? bv)
;; 	       (= len (bytevector-length bv)))))
;;     => #t)

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


(parameterise ((check-test-name 'utils))

  (define make-integer (lambda () (random-integer 10)))

  (let ((obj (unfold-random-numbers make-integer 10)))
    (check-for-true (list? obj))
    (check-for-true (every integer? obj))
    (check-for-true (every non-negative? obj)))

  (let ((obj (unfold-random-numbers/vector make-integer 10)))
    (check-for-true (vector? obj))
    (check-for-true (vector-every integer? obj))
    (check-for-true (vector-every non-negative? obj)))

  (let ((obj (unfold-random-numbers/string make-integer 10)))
    (check-for-true (string? obj)))

;;; --------------------------------------------------------------------

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
