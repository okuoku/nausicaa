;;;
;;;Part of: Nausicaa/MP
;;;Contents: tests for the MPZ numbers
;;;Date: Thu Nov 27, 2008
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
  (foreign memory)
  (foreign cstrings)
  (foreign math mp mpz)
  (foreign math mp sizeof))

(check-set-mode! 'report-failed)
(display "*** testing mpz\n")


;;;; helpers

(define (mpz->string o)
  (let ((str (mpz_get_str pointer-null 10 o)))
    (begin0
	(cstring->string str)
      (primitive-free str))))


(parametrise ((check-test-name 'explicit-allocation))

  (check
      (let ((a (malloc sizeof-mpz_t))
	    (b (malloc sizeof-mpz_t))
	    (c (malloc sizeof-mpz_t)))
	(mpz_init a)
	(mpz_init b)
	(mpz_init c)

	(mpz_set_si a 10)
	(mpz_set_si b 5)
	(mpz_add c a b)

	(mpz_clear a)
	(mpz_clear b)
	(primitive-free a)
	(primitive-free b)
	(begin0
	    (substring (mpz->string c) 0 2)
	  (primitive-free c)))
    => "15")

  #t)


(parametrise ((check-test-name 'compensated-allocation))

  (define (mpz/c)
    (letrec ((p (compensate
		    (malloc sizeof-mpz_t)
		  (with
		   (mpz_clear p)
		   (primitive-free p)))))
      (mpz_init p)
      p))

  (check
      (with-compensations
	(let ((c (mpz/c)))
	  (with-compensations
	    (let ((a (mpz/c))
		  (b (mpz/c)))
	      (mpz_set_si a 10)
	      (mpz_set_si b 5)
	      (mpz_add c a b)))
	  (substring (mpz->string c) 0 2)))
    => "15")

  #t)


(parametrise ((check-test-name 'factory-allocation))

  (define mpz-factory
    (make-caching-object-factory mpz_init mpz_clear sizeof-mpz_t 10))

  (define (mpz)
    (letrec ((p (compensate
		    (mpz-factory)
		  (with
		   (mpz-factory p)))))
      p))

  (check
      (with-compensations
	(let ((c (mpz)))
	  (with-compensations
	    (let ((a (mpz))
		  (b (mpz)))
	      (mpz_set_si a 10)
	      (mpz_set_si b 5)
	      (mpz_add c a b)))
	  (substring (mpz->string c) 0 2)))
    => "15")

  (mpz-factory 'purge)

  #t)


;;;; done

(check-report)

;;; end of file
