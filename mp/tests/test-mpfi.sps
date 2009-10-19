;;;
;;;Part of: Nausicaa/MP
;;;Contents: tests for the MPFI numbers
;;;Date: Wed Dec 10, 2008
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
  (compensations)
  (checks)
  (format)
  (foreign memory)
  (foreign cstrings)
  (foreign math mp mpfr)
  (foreign math mp mpfi)
  (foreign math mp sizeof))

(check-set-mode! 'report-failed)
(display "*** testing mpfi\n")



;;;; helpers

(define (mpfr->string o)
  (with-compensations
    (letrec*
	((l (malloc-small/c))
	 (str (compensate
		  (mpfr_get_str pointer-null l 10 0 o GMP_RNDN)
		(with
		 (primitive-free str))))
	 (s (cstring->string str))
	 (x (let ((x (pointer-ref-c-signed-long l 0)))
	      (if (char=? #\- (string-ref s 0))
		  (+ 1 x)
		x)))
	 (i (substring s 0 x))
	 (f (substring s x (strlen str))))
      (format "~a.~a" i f))))

(define (mpfi->string-interval o)
  (format "[~a, ~a]"
    (mpfr->string (struct-mpfi-left-ref o))
    (mpfr->string (struct-mpfi-right-ref o))))

(define (mpfi->string o)
  (let ((fr (malloc sizeof-mpfr_t)))
    (mpfr_init fr)
    (mpfi_get_fr fr o)
    (begin0
	(mpfr->string o)
      (mpfr_clear fr))))



(parametrise ((check-test-name 'explicit-allocation))

  (check
      (let ((a (malloc sizeof-mpfi_t))
	    (b (malloc sizeof-mpfi_t))
	    (c (malloc sizeof-mpfi_t)))
	(mpfi_init a)
	(mpfi_init b)
	(mpfi_init c)

	(mpfi_set_d a 10.4)
	(mpfi_set_si b 5)
	(mpfi_add c a b)

	(mpfi_clear a)
	(mpfi_clear b)
	(primitive-free a)
	(primitive-free b)
	(begin0
	    (substring (mpfi->string c) 0 5)
	  (primitive-free c)))
    => "15.40")

  #t)


(parametrise ((check-test-name 'compensated-allocation))

  (define (mpfi/c)
    (letrec ((p (compensate
		    (malloc sizeof-mpfi_t)
		  (with
		   (mpfi_clear p)
		   (primitive-free p)))))
      (mpfi_init p)
      p))

  (check
      (with-compensations
	(let ((c (mpfi/c)))
	  (with-compensations
	    (let ((a (mpfi/c))
		  (b (mpfi/c)))
	      (mpfi_set_d a 10.4)
	      (mpfi_set_si b 5)
	      (mpfi_add c a b)))
	  (substring (mpfi->string c) 0 5)))
    => "15.40")

  #t)


(parametrise ((check-test-name 'factory-allocation))

  (define mpfi-factory
    (make-caching-object-factory mpfi_init mpfi_clear sizeof-mpfi_t 10))

  (define (mpfi)
    (letrec ((p (compensate
		    (mpfi-factory)
		  (with
		   (mpfi-factory p)))))
      p))

  (check
      (with-compensations
	(let ((c (mpfi)))
	  (with-compensations
	    (let ((a (mpfi))
		  (b (mpfi)))
	      (mpfi_set_d a 10.4)
	      (mpfi_set_si b 5)
	      (mpfi_add c a b)))
	  (substring (mpfi->string c) 0 5)))
    => "15.40")

  (mpfi-factory 'purge)
  #t)


;;;; done

(check-report)

;;; end of file
