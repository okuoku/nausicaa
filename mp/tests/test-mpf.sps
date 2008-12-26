;;;
;;;Part of: Nausicaa/MP
;;;Contents: tests for the MPF numbers
;;;Date: Thu Nov 27, 2008
;;;Time-stamp: <2008-12-26 22:18:09 marco>
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
  (uriel foreign)
  (uriel test)
  (mp mpf)
  (mp sizeof))

(check-set-mode! 'report-failed)



;;;; helpers

(define (compensated-mpf)
  (letrec ((p (compensate
		  (malloc sizeof-mpf_t)
		(with
		 (mpf_clear p)
		 (primitive-free p)))))
    (mpf_init p)
    p))

(define mpf-factory
  (make-caching-object-factory mpf_init mpf_clear
			       sizeof-mpf_t 10))

(define (mpf)
  (letrec ((p (compensate
		  (mpf-factory)
		(with
		 (mpf-factory p)))))
    p))

(define (mpf->string o)
  (with-compensations
    (letrec*
	((l (malloc-small/c))
	 (str (compensate
		  (mpf_get_str pointer-null l 10 0 o)
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
	   (let ((a (malloc sizeof-mpf_t))
		 (b (malloc sizeof-mpf_t))
		 (c (malloc sizeof-mpf_t)))
	     (mpf_init a)
	     (mpf_init b)
	     (mpf_init c)

	     (mpf_set_d a 10.4)
	     (mpf_set_si b 5)
	     (mpf_add c a b)

	     (mpf_clear a)
	     (mpf_clear b)
	     (primitive-free a)
	     (primitive-free b)
	     c)
	   ))
      (begin0
	  (substring (mpf->string result) 0 5)
	(primitive-free result)))
  => "15.40")



;;;; basic tests, compensated allocation

(check
    (let ((result
	   (let ((c (malloc sizeof-mpf_t)))
	     (mpf_init c)
	     (with-compensations
	       (let ((a (compensated-mpf))
		     (b (compensated-mpf)))
		 (mpf_set_d a 10.4)
		 (mpf_set_si b 5)
		 (mpf_add c a b)
		 c)))
	   ))
      (begin0
	  (substring (mpf->string result) 0 5)
	(primitive-free result)))
  => "15.40")



;;;; basic tests, factory usage

(check
    (with-compensations
      (let ((result
	     (let ((c (mpf)))
	       (with-compensations
		 (let ((a (mpf))
		       (b (mpf)))
		   (mpf_set_d a 10.4)
		   (mpf_set_si b 5)

		   (mpf_add c a b)
		   c)))
	     ))
	(substring (mpf->string result) 0 5)))
  => "15.40")



;;;; done

(mpf-factory 'purge)

(check-report)

;;; end of file
