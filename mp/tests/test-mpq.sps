;;;
;;;Part of: Nausicaa/MP
;;;Contents: tests for the MPQ numbers
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
  (foreign math mp mpq)
  (foreign math mp sizeof))

(check-set-mode! 'report-failed)
(display "*** testing mpq\n")


;;;; helpers

(define (mpq->string o)
  (let ((str #f))
    (dynamic-wind
	(lambda ()
	  (set! str (mpq_get_str pointer-null 10 o)))
	(lambda ()
	  (cstring->string str))
	(lambda ()
	  (primitive-free str)))))


(parametrise ((check-test-name 'explicit-allocation))

  (check
      (let ((a (malloc sizeof-mpq_t))
	    (b (malloc sizeof-mpq_t))
	    (c (malloc sizeof-mpq_t)))
	(mpq_init a)
	(mpq_init b)
	(mpq_init c)

	(mpq_set_ui a 6 10)
	(mpq_canonicalize a)
	(mpq_set_si b 6 5)
	(mpq_add c a b)

	(mpq_clear a)
	(mpq_clear b)
	(primitive-free a)
	(primitive-free b)
	(begin0
	    (mpq->string c)
	  (primitive-free c)))
    => "9/5")

  #t)


(parametrise ((check-test-name 'compensated-allocation))

  (define (mpq/c)
    (letrec ((p (compensate
		    (malloc sizeof-mpq_t)
		  (with
		   (mpq_clear p)
		   (primitive-free p)))))
      (mpq_init p)
      p))

  (check
      (with-compensations
	(let ((c (mpq/c)))
	  (with-compensations
	    (let ((a (mpq/c))
		  (b (mpq/c)))
	      (mpq_set_si a 6 10)
	      (mpq_canonicalize a)
	      (mpq_set_si b 6 5)
	      (mpq_add c a b)))
	  (mpq->string c)))
    => "9/5")

  #t)


(parametrise ((check-test-name 'factory-allocation))

  (define mpq-factory
    (make-caching-object-factory mpq_init mpq_clear sizeof-mpq_t 10))

  (define (mpq)
    (letrec ((p (compensate
		    (mpq-factory)
		  (with
		   (mpq-factory p)))))
      p))

  (check
      (with-compensations
	(let ((c (mpq)))
	  (with-compensations
	    (let ((a (mpq))
		  (b (mpq)))
	      (mpq_set_si a 6 10)
	      (mpq_canonicalize a)
	      (mpq_set_si b 6 5)
	      (mpq_add c a b)))
	  (mpq->string c)))
    => "9/5")

  (mpq-factory 'purge)
  #t)


;;;; done

(check-report)

;;; end of file
