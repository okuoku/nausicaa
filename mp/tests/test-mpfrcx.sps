;;;
;;;Part of: Nausicaa/MP
;;;Contents: tests for the MPFRCX numbers
;;;Date: Fri Nov  6, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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

(define (mpfrx->string poly digits)
  (let-values (((port getter) (open-string-output-port)))
    (let ((degree (struct-mpfrx_t-deg-ref poly)))
      (display #\( port)
      (display degree port)
      (display #\; port)
      (do ((i degree (- i 1)))
	  ((= i -1)
	   (display #\) port)
	   (getter))
	(display #\space port)
	(display (mpfr->string (struct-mpfrx_t-coeff-ptr-ref poly i) digits)
		 port)))))


(parametrise ((check-test-name 'explicit-allocation))

  (check
      (let ((a (malloc sizeof-mpfrx_t))
	    (b (malloc sizeof-mpfrx_t))
	    (c (malloc sizeof-mpfrx_t)))

	;;By  subtracting  we  annihilate  the  coefficient  of  highest
	;;degree.
	;;
	(mpfrx_init* a '#(1.2 3.4 5.6 7.8 9.0) mpfr_set_d)
	(mpfrx_init* b '#(-1. -3. -5. -7. -9.) mpfr_set_d)
	(mpfrx_init c 5 50)
	(mpfrx_add c a b)
	(mpfrx_clear a)
	(mpfrx_clear b)
	(primitive-free a)
	(primitive-free b)

	(begin0
	    (mpfrx->string c 5)
	  (mpfrx_clear c)
	  (primitive-free c)))
    => "(3; .80000 .60000 .40000 .20000)")

  #t)


(parametrise ((check-test-name	'dynamic-wind))

  (define-syntax with-mpfrx
    (syntax-rules ()

      ((_ () ?form0 ?form ...)
       (begin ?form0 ?form ...))

      ((_ ((?id ?degree) ?init ...) ?form0 ?form ...)
       (let ((?id #f))
	 (dynamic-wind
	     (lambda ()
	       (set! ?id (malloc sizeof-mpfrx_t))
	       (mpfrx_init ?id (+ 1 ?degree) (mpfrcx-precision)))
	     (lambda ()
	       (with-mpfrx (?init ...) ?form0 ?form ...))
	     (lambda ()
	       (mpfrx_clear ?id)
	       (primitive-free ?id)))))

      ((_ ((?id ?coeffs ?setter) ?init ...) ?form0 ?form ...)
       (let ((?id #f))
	 (dynamic-wind
	     (lambda ()
	       (set! ?id (malloc sizeof-mpfrx_t))
	       (mpfrx_init* ?id ?coeffs ?setter))
	     (lambda ()
	       (with-mpfrx (?init ...) ?form0 ?form ...))
	     (lambda ()
	       (mpfrx_clear ?id)
	       (primitive-free ?id)))))))

  (check
      (with-mpfrx ((a '#(1.2 3.4 5.6 7.8 9.0) mpfr_set_d)
		   (b '#(-1. -3. -5. -7. -9.) mpfr_set_d)
		   (c 4))
	(mpfrx_add c a b)
	(mpfrx->string c 5))
    => "(3; .80000 .60000 .40000 .20000)")

  #t)


(parametrise ((check-test-name 'compensated-allocation))

  (define (mpfrx/c degree)
    (letrec ((p (compensate
		    (malloc sizeof-mpfrx_t)
		  (with
		   (mpfrx_clear p)
		   (primitive-free p)))))
      (mpfrx_init p (+ 1 degree) (mpfrcx-precision))
      p))

  (define (mpfrx*/c coeffs setter)
    (letrec ((p (compensate
		    (malloc sizeof-mpfrx_t)
		  (with
		   (mpfrx_clear p)
		   (primitive-free p)))))
      (mpfrx_init* p coeffs setter)
      p))

  (check
      (with-compensations
	(let ((c (mpfrx/c 4)))
	  (with-compensations
	    (let ((a (mpfrx*/c '#(1.2 3.4 5.6 7.8 9.0) mpfr_set_d))
		  (b (mpfrx*/c '#(-1. -3. -5. -7. -9.) mpfr_set_d)))
	      (mpfrx_add c a b)))
	  (mpfrx->string c 5)))
    => "(3; .80000 .60000 .40000 .20000)")

  #t)


;;;; done

(check-report)

;;; end of file
