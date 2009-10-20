;;;
;;;Part of: Nausicaa/MP
;;;Contents: tests for the MPF numbers
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
  (foreign math mp mpf)
  (foreign math mp sizeof))

(check-set-mode! 'report-failed)
(display "*** testing mpf\n")


;;;; helpers

(define (mpf->string o)
  (with-compensations
    (letrec*
	((l	(malloc-small/c))
	 (str	(compensate
		    (mpf_get_str pointer-null l 10 0 o)
		  (with
		   (primitive-free str))))
	 (s	(cstring->string str))
	 (x	(let ((x (pointer-ref-c-signed-long l 0)))
		  (if (char=? #\- (string-ref s 0))
		      (+ 1 x)
		    x)))
	 (i	(substring s 0 x))
	 (f	(substring s x (strlen str))))
      (string-append i "." f))))


(parametrise ((check-test-name 'explicit-allocation))

  (check
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
	(begin0
	    (substring (mpf->string c) 0 5)
	  (primitive-free c)))
    => "15.40")

  #t)


(parametrise ((check-test-name 'compensated-allocation))

  (define (mpf/c)
    (letrec ((p (compensate
		    (malloc sizeof-mpf_t)
		  (with
		   (mpf_clear p)
		   (primitive-free p)))))
      (mpf_init p)
      p))

  (check
      (with-compensations
	(let ((c (mpf/c)))
	  (with-compensations
	    (let ((a (mpf/c))
		  (b (mpf/c)))
	      (mpf_set_d a 10.4)
	      (mpf_set_si b 5)
	      (mpf_add c a b)))
	  (substring (mpf->string c) 0 5)))
    => "15.40")

  #t)


(parametrise ((check-test-name 'factory-allocation))

  (define mpf-factory
    (make-caching-object-factory mpf_init mpf_clear sizeof-mpf_t 10))

  (define (mpf)
    (letrec ((p (compensate
		    (mpf-factory)
		  (with
		   (mpf-factory p)))))
      p))

  (check
      (with-compensations
	(let ((c (mpf)))
	  (with-compensations
	    (let ((a (mpf))
		  (b (mpf)))
	      (mpf_set_d a 10.4)
	      (mpf_set_si b 5)
	      (mpf_add c a b)))
	  (substring (mpf->string c) 0 5)))
    => "15.40")

  (check
      (with-compensations
	(let ((c (mpf)))
	  (with-compensations
	    (let ((a (mpf))
		  (b (mpf)))
	      (mpf_set_d a 10.4)
	      (mpf_set_si b -50)
	      (mpf_add c a b)))
	  (substring (mpf->string c) 0 8)))
    => "-39.5999")

  (mpf-factory 'purge)
  #t)


;;;; done

(check-report)

;;; end of file
