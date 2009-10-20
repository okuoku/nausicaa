;;;
;;;Part of: Nausicaa/MP
;;;Contents: tests for the MPC numbers
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
  (foreign math mp mpc)
  (foreign math mp sizeof))

(check-set-mode! 'report-failed)
(display "*** testing mpc\n")


;;;; helpers

(define (mpfr->string o)
  (with-compensations
    (letrec*
	((l (malloc-small/c))
	 (str (compensate
		  (mpfr_get_str pointer-null l 10 0 o MPC_RNDNN)
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

(define (mpc->string o)
  (let ((im (mpfr->string (struct-mpc-im-ref o))))
    (format "~a~a~ai"
      (substring (mpfr->string (struct-mpc-re-ref o)) 0 5)
      (if (char=? #\- (string-ref im 0))
	  ""
	#\+)
      (substring im 0 5))))


(parametrise ((check-test-name 'explicit-allocation))

  (check
      (let ((a (malloc sizeof-mpc_t))
	    (b (malloc sizeof-mpc_t))
	    (c (malloc sizeof-mpc_t)))
	(mpc_init a)
	(mpc_init b)
	(mpc_init c)

	(mpc_set_d_d   a 10.4 -3.2 MPC_RNDNN)
	(mpc_set_si_si b  5    8   MPC_RNDNN)
	(mpc_add c a b MPC_RNDNN)
	(mpc_clear a)
	(mpc_clear b)
	(primitive-free a)
	(primitive-free b)
	(begin0
	    (mpc->string c)
	  (primitive-free c)))
    => "15.40+4.799i")

  #t)


(parametrise ((check-test-name	'dynamic-wind))

  (define-syntax with-mpc
    (syntax-rules ()
      ((_ () ?form0 ?form ...)
       (begin ?form0 ?form ...))
      ((_ (?id0 ?id ...) ?form0 ?form ...)
       (let ((?id0 #f))
	 (dynamic-wind
	     (lambda ()
	       (set! ?id0 (malloc sizeof-mpc_t))
	       (mpc_init ?id0))
	     (lambda ()
	       (with-mpc (?id ...) ?form0 ?form ...))
	     (lambda ()
	       (mpc_clear ?id0)
	       (primitive-free ?id0)))))))

  (check
      (with-mpc (a b c)
	(mpc_set_d_d   a 10.4 -3.2 MPC_RNDNN)
	(mpc_set_si_si b  5    8   MPC_RNDNN)
	(mpc_add c a b MPC_RNDNN)
        (mpc->string c))
    => "15.40+4.799i")

  #t)


(parametrise ((check-test-name 'compensated-allocation))

  (define (mpc/c)
    (letrec ((p (compensate
		    (malloc sizeof-mpc_t)
		  (with
		   (mpc_clear p)
		   (primitive-free p)))))
      (mpc_init p)
      p))

  (check
      (with-compensations
	(let ((c (mpc/c)))
	  (with-compensations
	    (let ((a (mpc/c))
		  (b (mpc/c)))
	      (mpc_set_d_d   a 10.4 -3.2 MPC_RNDNN)
	      (mpc_set_ui_ui b  5    8   MPC_RNDNN)
	      (mpc_add c a b MPC_RNDNN)))
	  (mpc->string c)))
    => "15.40+4.799i")

  #t)


(parametrise ((check-test-name 'factory-allocation))

  (define mpc-factory
    (make-caching-object-factory mpc_init mpc_clear sizeof-mpc_t 10))

  (define (mpc)
    (letrec ((p (compensate
		    (mpc-factory)
		  (with
		   (mpc-factory p)))))
      p))

  (check
      (with-compensations
	(let ((c (mpc)))
	  (with-compensations
	    (let ((a (mpc))
		  (b (mpc)))
	      (mpc_set_d_d   a 10.4 -3.2 MPC_RNDNN)
	      (mpc_set_si_si b  5    8   MPC_RNDNN)
	      (mpc_add c a b MPC_RNDNN)))
	  (mpc->string c)))
    => "15.40+4.799i")

  (mpc-factory 'purge)
  #t)


;;;; done

(check-report)

;;; end of file
