;;;
;;;Part of: Nausicaa/MP
;;;Contents: tests for the MPFR numbers
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
  (foreign memory)
  (foreign cstrings)
  (foreign math mp mpfr)
  (foreign math mp sizeof))

(check-set-mode! 'report-failed)
(display "*** testing mpfr\n")


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
      (string-append i "." f))))


(parametrise ((check-test-name 'explicit-allocation))

  (check
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
	(begin0
	    (substring (mpfr->string c) 0 5)
	  (primitive-free c)))
    => "15.40")

  #t)


(parametrise ((check-test-name	'dynamic-wind))

  (define-syntax with-mpfr
    (syntax-rules ()
      ((_ () ?form0 ?form ...)
       (begin ?form0 ?form ...))
      ((_ (?id0 ?id ...) ?form0 ?form ...)
       (let ((?id0 #f))
	 (dynamic-wind
	     (lambda ()
	       (set! ?id0 (malloc sizeof-mpfr_t))
	       (mpfr_init ?id0))
	     (lambda ()
	       (with-mpfr (?id ...) ?form0 ?form ...))
	     (lambda ()
	       (mpfr_clear ?id0)
	       (primitive-free ?id0)))))))

  (check
      (with-mpfr (a b c)
	(mpfr_set_d  a 10.4 GMP_RNDN)
	(mpfr_set_si b  5   GMP_RNDN)
	(mpfr_add c  a  b   GMP_RNDN)
        (substring (mpfr->string c) 0 2))
    => "15")

  #t)


(parametrise ((check-test-name 'compensated-allocation))

  (define (mpfr/c)
    (letrec ((p (compensate
		    (malloc sizeof-mpfr_t)
		  (with
		   (mpfr_clear p)
		   (primitive-free p)))))
      (mpfr_init p)
      p))

  (check
      (with-compensations
	(let ((c (mpfr/c)))
	  (with-compensations
	    (let ((a (mpfr/c))
		  (b (mpfr/c)))
	      (mpfr_set_d a 10.4 GMP_RNDN)
	      (mpfr_set_si b 5 GMP_RNDN)
	      (mpfr_add c a b GMP_RNDN)))
	  (substring (mpfr->string c) 0 5)))
    => "15.40")

  #t)


(parametrise ((check-test-name 'factory-allocation))

  (define mpfr-factory
    (make-caching-object-factory mpfr_init mpfr_clear sizeof-mpfr_t 10))

  (define (mpfr)
    (letrec ((p (compensate
		    (mpfr-factory)
		  (with
		   (mpfr-factory p)))))
      p))

  (check
      (with-compensations
	(let ((c (mpfr)))
	  (with-compensations
	    (let ((a (mpfr))
		  (b (mpfr)))
	      (mpfr_set_d a 10.4 GMP_RNDN)
	      (mpfr_set_si b 5 GMP_RNDN)
	      (mpfr_add c a b GMP_RNDN)))
	  (substring (mpfr->string c) 0 5)))
    => "15.40")

  (mpfr-factory 'purge)
  #t)


;;;; done

(check-report)

;;; end of file
