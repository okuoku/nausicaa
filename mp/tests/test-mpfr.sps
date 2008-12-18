;;;
;;;Part of: Nausicaa/MP
;;;Contents: tests for the MPFR numbers
;;;Date: Wed Dec 10, 2008
;;;Time-stamp: <2008-12-18 21:34:45 marco>
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
  (uriel memory)
  (uriel cstring)
  (uriel test)
  (srfi format)
  (mp mpfr)
  (mp sizeof))

(check-set-mode! 'report-failed)



;;;; helpers

(define (compensated-mpfr)
  (letrec ((p (compensate
		  (malloc sizeof-mpfr_t)
		(with
		 (mpfr_clear p)
		 (primitive-free p)))))
    (mpfr_init p)
    p))

(define mpfr-factory
  (make-caching-object-factory mpfr_init mpfr_clear
			       sizeof-mpfr_t 10))

(define (mpfr)
  (letrec ((p (compensate
		  (mpfr-factory)
		(with
		 (mpfr-factory p)))))
    p))

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



;;;; basic tests, explicit allocation

(check
    (let ((result
	   (let ((a (malloc sizeof-mpfr_t))
		 (b (malloc sizeof-mpfr_t))
		 (c (malloc sizeof-mpfr_t)))
	     (mpfr_init a)
	     (mpfr_init b)
	     (mpfr_init c)

	     (mpfr_set_d a 10.4 GMP_RNDN)
	     (mpfr_set_si b 5 GMP_RNDN)
	     (mpfr_add c a b GMP_RNDN)

	     (mpfr_clear a)
	     (mpfr_clear b)
	     (primitive-free a)
	     (primitive-free b)
	     c)
	   ))
      (begin0
	  (substring (mpfr->string result) 0 5)
	(primitive-free result)))
  => "15.40")



;;;; basic tests, compensated allocation

(check
    (let ((result
	   (let ((c (malloc sizeof-mpfr_t)))
	     (mpfr_init c)
	     (with-compensations
	       (let ((a (compensated-mpfr))
		     (b (compensated-mpfr)))
		 (mpfr_set_d a 10.4 GMP_RNDN)
		 (mpfr_set_si b 5 GMP_RNDN)
		 (mpfr_add c a b GMP_RNDN)
		 c)))
	   ))
      (begin0
	  (substring (mpfr->string result) 0 5)
	(primitive-free result)))
  => "15.40")



;;;; basic tests, factory usage

(check
    (with-compensations
      (let ((result
	     (let ((c (mpfr)))
	       (with-compensations
		 (let ((a (mpfr))
		       (b (mpfr)))
		   (mpfr_set_d a 10.4 GMP_RNDN)
		   (mpfr_set_si b 5 GMP_RNDN)
		   (mpfr_add c a b GMP_RNDN)
		   c)))
	     ))
	(substring (mpfr->string result) 0 5)))
  => "15.40")



;;;; done

(mpfr-factory 'purge)

(check-report)

;;; end of file
