;;;
;;;Part of: Nausicaa/MP
;;;Contents: tests for the MPFRCX numbers
;;;Date: Fri Nov  6, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (strings)
  (foreign memory)
  (foreign cstrings)
  (foreign math mp mpfr)
  (foreign math mp mpfrcx)
  (foreign math mp sizeof))

(check-set-mode! 'report-failed)
(display "*** testing MPFRCX\n")


;;;; helpers

(define (mpfr->string o digits)
  (with-compensations
    (letrec*
	((l (malloc-small/c))
	 (str (compensate
		  (mpfr_get_str pointer-null l 10 digits o GMP_RNDN)
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

(define (mpfrx->string poly)
  (let-values (((port getter) (open-string-output-port)))
    (let ((len (struct-mpfrx_t-size-ref poly)))
      (display "(" port)
      (display (struct-mpfrx_t-size-ref poly) port)
      (display " " port)
      (display (struct-mpfrx_t-deg-ref poly) port)
      (display ";" port)
      (do ((i 0 (+ 1 i)))
	  ((= i len)
	   (display ")" port)
	   (getter))
	(display " " port)
	(display (mpfr->string (struct-mpfrx_t-coeff-ptr-ref poly i) 5)
		 port)))))


(parametrise ((check-test-name 'explicit-allocation))

  (define (init-poly! poly coeffs)
    (let ((len (vector-length coeffs)))
      (mpfrx_init poly len 50)
      (struct-mpfrx_t-deg-set! poly (- len 1))
      (do ((i 0 (+ 1 i)))
	  ((= i len))
	(mpfr_set_d (struct-mpfrx_t-coeff-ptr-ref poly i)
		    (vector-ref coeffs i)
		    GMP_RNDN))))

  (check
      (let ((a (malloc sizeof-mpfrx_t))
	    (b (malloc sizeof-mpfrx_t))
	    (c (malloc sizeof-mpfrx_t)))

	(init-poly! a '#(1.2 3.4 5.6 7.8 9.0))
	(init-poly! b '#(-1. -3. -5. -7. -9.))
	(mpfrx_init c 5 50)
	(mpfrx_add c a b)
	(mpfrx_clear a)
	(mpfrx_clear b)
	(primitive-free a)
	(primitive-free b)

	(begin0
	    (mpfrx->string c)
	  (mpfrx_clear c)
	  (primitive-free c)))
    => "(5 3; .20000 .40000 .60000 .80000 .00000)")

  #t)


#;(parametrise ((check-test-name	'dynamic-wind))

  (define-syntax with-mpfrx
    (syntax-rules ()
      ((_ () ?form0 ?form ...)
       (begin ?form0 ?form ...))
      ((_ (?id0 ?id ...) ?form0 ?form ...)
       (let ((?id0 #f))
	 (dynamic-wind
	     (lambda ()
	       (set! ?id0 (malloc sizeof-mpfrx_t))
	       (mpfrx_init ?id0))
	     (lambda ()
	       (with-mpfrx (?id ...) ?form0 ?form ...))
	     (lambda ()
	       (mpfrx_clear ?id0)
	       (primitive-free ?id0)))))))

  (check
      (with-mpfrx (a b c)
	(mpfrx_set_d  a 10.4 GMP_RNDN)
	(mpfrx_set_si b  5   GMP_RNDN)
	(mpfrx_add c  a  b   GMP_RNDN)
        (substring (mpfrx->string c) 0 2))
    => "15")

  #t)


#;(parametrise ((check-test-name 'compensated-allocation))

  (define (mpfrx/c)
    (letrec ((p (compensate
		    (malloc sizeof-mpfrx_t)
		  (with
		   (mpfrx_clear p)
		   (primitive-free p)))))
      (mpfrx_init p)
      p))

  (check
      (with-compensations
	(let ((c (mpfrx/c)))
	  (with-compensations
	    (let ((a (mpfrx/c))
		  (b (mpfrx/c)))
	      (mpfrx_set_d a 10.4 GMP_RNDN)
	      (mpfrx_set_si b 5 GMP_RNDN)
	      (mpfrx_add c a b GMP_RNDN)))
	  (substring (mpfrx->string c) 0 5)))
    => "15.40")

  #t)


#;(parametrise ((check-test-name 'factory-allocation))

  (define mpfrx-factory
    (make-caching-object-factory mpfrx_init mpfrx_clear sizeof-mpfrx_t 10))

  (define (mpfrx)
    (letrec ((p (compensate
		    (mpfrx-factory)
		  (with
		   (mpfrx-factory p)))))
      p))

  (check
      (with-compensations
	(let ((c (mpfrx)))
	  (with-compensations
	    (let ((a (mpfrx))
		  (b (mpfrx)))
	      (mpfrx_set_d a 10.4 GMP_RNDN)
	      (mpfrx_set_si b 5 GMP_RNDN)
	      (mpfrx_add c a b GMP_RNDN)))
	  (substring (mpfrx->string c) 0 5)))
    => "15.40")

  (mpfrx-factory 'purge)
  #t)


;;;; done

(check-report)

;;; end of file
