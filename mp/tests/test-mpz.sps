;;;
;;;Part of: Nausicaa/MP
;;;Contents: tests for the MPZ numbers
;;;Date: Thu Nov 27, 2008
;;;Time-stamp: <2008-12-16 10:04:41 marco>
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
  (uriel lang)
  (uriel ffi)
  (uriel printing)
  (uriel test)
  (mp mpz)
  (mp sizeof))

(check-set-mode! 'report-failed)



;;;; helpers

(define (compensated-mpz)
  (letrec ((p (compensate
		  (malloc sizeof-mpz_t)
		(with
		 (mpz_clear p)
		 (primitive-free p)))))
    (mpz_init p)
    p))

(define mpz-factory
  (make-caching-object-factory mpz_init mpz_clear
			       sizeof-mpz_t 10))

(define (mpz)
  (letrec ((p (compensate
		  (mpz-factory)
		(with
		 (mpz-factory p)))))
    p))

;; (define (mpz->string o)
;;   (let ((str (mpz_get_str pointer-null 10 o)))
;;     (begin0
;; 	(cstring->string str)
;;       (primitive-free str))))

(define (mpz->string o)
  (with-compensations
    (letrec
	((str (compensate
		  (mpz_get_str pointer-null 10 o)
		(with
		 (primitive-free str)))))
      (cstring->string str))))



;;;; basic tests, explicit allocation

(check
    (let ((result
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
	     c)
	   ))
      (begin0
	  (substring (mpz->string result) 0 2)
	(primitive-free result)))
  => "15")



;;;; basic tests, compensated allocation

(check
    (let ((result
	   (let ((c (malloc sizeof-mpz_t)))
	     (mpz_init c)
	     (with-compensations
	       (let ((a (compensated-mpz))
		     (b (compensated-mpz)))
		 (mpz_set_si a 10)
		 (mpz_set_si b 5)
		 (mpz_add c a b)
		 c)))
	   ))
      (begin0
	  (substring (mpz->string result) 0 2)
	(primitive-free result)))
  => "15")



;;;; basic tests, factory usage

(check
    (with-compensations
      (let ((result
	     (let ((c (mpz)))
	       (with-compensations
		 (let ((a (mpz))
		       (b (mpz)))
		   (mpz_set_si a 10)
		   (mpz_set_si b 5)

		   (mpz_add c a b)
		   c)))
	     ))
	(substring (mpz->string result) 0 2)))
  => "15")



;;;; done

(mpz-factory 'purge)

(check-report)

;;; end of file
