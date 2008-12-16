;;;
;;;Part of: Nausicaa/MP
;;;Contents: tests for the MPQ numbers
;;;Date: Thu Nov 27, 2008
;;;Time-stamp: <2008-12-16 10:04:36 marco>
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
  (srfi parameters)
  (mp mpq)
  (mp sizeof))

(check-set-mode! 'report-failed)



;;;; helpers

(define (compensated-mpq)
  (letrec ((p (compensate
		  (malloc sizeof-mpq_t)
		(with
		 (mpq_clear p)
		 (primitive-free p)))))
    (mpq_init p)
    p))

(define mpq-factory
  (make-caching-object-factory mpq_init mpq_clear
			       sizeof-mpq_t 10))

(define (mpq)
  (letrec ((p (compensate
		  (mpq-factory)
		(with
		 (mpq-factory p)))))
    p))

;; (define (mpq->string o)
;;   (let ((str (mpq_get_str pointer-null 10 o)))
;;     (begin0
;; 	(cstring->string str)
;;       (primitive-free str))))

(define (mpq->string o)
  (with-compensations
    (letrec
	((str (compensate
		  (mpq_get_str pointer-null 10 o)
		(with
		 (primitive-free str)))))
      (cstring->string str))))



;;;; basic tests, explicit allocation

(parameterize ((testname 'alloc))
  (check
      (let ((result
	     (let ((a (malloc sizeof-mpq_t))
		   (b (malloc sizeof-mpq_t))
		   (c (malloc sizeof-mpq_t)))
	       (mpq_init a)
	       (mpq_init b)
	       (mpq_init c)

	       (mpq_set_ui a 6 10)
	       ;;(mpq_set_str a (string->cstring "6/10") 10)
	       ;;(display (mpq->string a))(newline)
	       (mpq_canonicalize a)
	       ;;(display (mpq->string a))(newline)
 	       (mpq_set_si b 6 5)
	       ;;(mpq_set_str b (string->cstring "6/5") 10)
	       ;;(display (mpq->string b))(newline)
 	       (mpq_add c a b)

	       (mpq_clear a)
	       (mpq_clear b)
	       (primitive-free a)
	       (primitive-free b)
	       c)
	     ))
	(begin0
	    (mpq->string result)
	  (primitive-free result)))
    => "9/5"))



;;;; basic tests, compensated allocation

(parameterize ((testname 'comp))
  (check
      (let ((result
	     (let ((c (malloc sizeof-mpq_t)))
	       (mpq_init c)
	       (with-compensations
		 (let ((a (compensated-mpq))
		       (b (compensated-mpq)))
		   (mpq_set_si a 6 10)
		   (mpq_canonicalize a)
		   (mpq_set_si b 6 5)
		   (mpq_add c a b)
		   c)))
	     ))
	(begin0
	    (mpq->string result)
	  (primitive-free result)))
    => "9/5"))



;;;; basic tests, factory usage

(parameterize ((testname 'factory))
  (check
      (with-compensations
	(let ((result
	       (let ((c (mpq)))
		 (with-compensations
		   (let ((a (mpq))
			 (b (mpq)))
		     (mpq_set_si a 6 10)
		     (mpq_canonicalize a)
		     (mpq_set_si b 6 5)
		     (mpq_add c a b)
		     c)))
	       ))
	  (mpq->string result)))
    => "9/5"))



;;;; done

(mpq-factory 'purge)

(check-report)

;;; end of file
