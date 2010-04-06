;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for the libraries library
;;;Date: Tue Apr  6, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (libraries)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing libraries\n")


;;;; library cache

(define library-registry
  (make-parameter #f))

(define (library-cache:load-library spec)
  (hashtable-ref (library-registry) spec #f))

(define (library-cache:register spec sexp)
  (hashtable-set! (library-registry) spec sexp))


(parametrise ((check-test-name	'loading))

  (check
      (let ((lib (load-library '(lists))))
	(<library>? lib))
    => #t)

  (check
      (let ((lib (load-library '(nausicaa))))
	(<library>? lib))
    => #t)

  (check
      (let-fields (((lib <library>) (load-library '(lists))))
	lib.spec)
    => '(lists))

  (check
      (guard (E ((library-not-found-condition? E)
		 #t)
		(else #f))
	(load-library '(it is impossible for this library to exist right?)))
    => #t)

  #f)


(parametrise ((check-test-name		'matching-basic)
	      (load-library-function	library-cache:load-library)
	      (library-registry		(make-hashtable equal-hash equal?)))

  (library-cache:register '(proof one)
			  '(library (proof one)
			     (export a b c)
			     (import (rnrs))
			     (define a 1)
			     (define (b arg)
			       (vector arg))
			     (define-syntax c
			       (syntax-rules ()
				 ((_ ?ch)
				  (string ?ch))))))

  (check
      (let-fields (((lib <library>) (load-library '(proof one))))
	lib.raw-exports)
    => '(a b c))

  (check
      (let-fields (((lib <library>) (load-library '(proof one))))
	lib.raw-imports)
    => '((rnrs)))

  #f)


;;;; done

(check-report)

;;; end of file
