;;;
;;;Part of: Nausicaa/MP
;;;Contents: tests for the MPC numbers
;;;Date: Wed Dec 10, 2008
;;;Time-stamp: <2008-12-11 21:27:19 marco>
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

(import (rnrs)
  (uriel lang)
  (uriel ffi)
  (uriel test)
  (srfi format)
  (srfi parameters)
  (mp mpfr)
  (mp mpc)
  (mp sizeof))
;;;  (except (ikarus foreign) malloc))

(check-set-mode! 'report-failed)



;;;; helpers

(define (compensated-mpc)
  (letrec ((p (compensate
		  (malloc sizeof-mpc_t)
		(with
		 (mpc_clear p)
		 (primitive-free p)))))
    (mpc_init p)
    p))

(define mpc-factory
  (make-caching-object-factory mpc_init mpc_clear
			       sizeof-mpc_t 10))

(define (mpc)
  (letrec ((p (compensate
		  (mpc-factory)
		(with
		 (mpc-factory p)))))
    p))

(define (mpfr->string o)
  (with-compensations
    (letrec*
	((l (compensate-malloc/small))
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
      (format "~a.~a" i f))))


(define (mpc->string o)
  (let ((im (mpfr->string (struct-mpc-im-ref o))))
    (format "~a~a~ai"
      (substring (mpfr->string (struct-mpc-re-ref o)) 0 5)
      (if (char=? #\- (string-ref im 0))
	  ""
	#\+)
      (substring im 0 5))))

;; (define (mpc->string o)
;;   (let ((im (mpfr->string (struct-mpc-im-ref o))))
;;     (format "~a~a~ai"
;;       (mpfr->string (struct-mpc-re-ref o))
;;       (if (char=? #\- (string-ref im 0))
;; 	  ""
;; 	#\+)
;;       im)))



;;;; basic tests, explicit allocation

;; (define lib (dlopen "libmpc.so"))
;; (define f (make-c-callout 'signed-int '(pointer unsigned-long unsigned-long signed-int)))
;; (define g (f (dlsym lib "mpc_set_si_si")))

(parameterize ((testname 'alloc))
  (check
      (let ((result
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
	       c)
	     ))
	(begin0
	    (mpc->string result)
	  (primitive-free result)))
    => "15.40+4.799i")
  )



;;;; basic tests, compensated allocation

(check
    (let ((result
	   (let ((c (malloc sizeof-mpc_t)))
	     (mpc_init c)
	     (with-compensations
	       (let ((a (compensated-mpc))
		     (b (compensated-mpc)))
		 (mpc_set_d_d   a 10.4 -3.2 MPC_RNDNN)
		 (mpc_set_ui_ui b  5    8   MPC_RNDNN)
		 (mpc_add c a b MPC_RNDNN)
		 c)))
	   ))
      (begin0
	  (mpc->string result)
	(primitive-free result)))
  => "15.40+4.799i")



;;;; basic tests, factory usage

(check
    (with-compensations
      (let ((result
	     (let ((c (mpc)))
	       (with-compensations
		 (let ((a (mpc))
		       (b (mpc)))
		   (mpc_set_d_d   a 10.4 -3.2 MPC_RNDNN)
		   (mpc_set_si_si b  5    8   MPC_RNDNN)
		   (mpc_add c a b MPC_RNDNN)
		   c)))
	     ))
	(mpc->string result)))
  => "15.40+4.799i")




;;;; done

(mpc-factory 'purge)

(check-report)

;;; end of file
