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
  (libraries low)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing libraries\n")


;;;; library cache

(define library-cache:registry
  (make-parameter #f))

(define (library-cache:load-library spec)
  (hashtable-ref (library-cache:registry) spec #f))

(define (library-cache:register spec sexp)
  (hashtable-set! (library-cache:registry) spec sexp))


(parametrise ((check-test-name	'low-import-specs))

  (check
      (%apply-import-spec/only '() '(a b c))
    => '())

  (check
      (%apply-import-spec/only '((a b)) '())
    => '())

  (check
      (%apply-import-spec/only '((a b)) '(b))
    => '((a b)))

  (check
      (%apply-import-spec/only '((a b)) '(b c))
    => '((a b)))

  (check
      (%apply-import-spec/only '((a ea) (b eb) (c ec))
			       '(eb ec))
    => '((b eb) (c ec)))

;;; --------------------------------------------------------------------

  (check
      (%apply-import-spec/except '() '(b))
    => '())

  (check
      (%apply-import-spec/except '((a b)) '())
    => '((a b)))

  (check
      (%apply-import-spec/except '((a b)) '(b))
    => '())

  (check
      (%apply-import-spec/except '((a b)) '(c))
    => '((a b)))

  (check
      (%apply-import-spec/except '((a ea) (b eb) (c ec))
				 '(eb ec))
    => '((a ea)))

;;; --------------------------------------------------------------------

  (check
      (%apply-import-spec/prefix '() 'ciao:)
    => '())

  (check
      (%apply-import-spec/prefix '((a b)) 'ciao:)
    => '((a ciao:b)))

  (check
      (%apply-import-spec/prefix '((a ea) (b eb) (c ec))
				 'ciao:)
    => '((a ciao:ea) (b ciao:eb) (c ciao:ec)))

;;; --------------------------------------------------------------------

  (check
      (%apply-import-spec/rename '() '())
    => '())

  (check
      (%apply-import-spec/rename '((a b)) '())
    => '((a b)))

  (check
      (%apply-import-spec/rename '() '((a b)))
    => '())

  (check
      (%apply-import-spec/rename '((a b)) '((b c)))
    => '((a c)))

  (check
      (%apply-import-spec/rename '((a ea) (b eb) (c ec))
				 '((eb ebb) (ec ecc)))
    => '((a ea) (b ebb) (c ecc)))

  #t)


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
	      (library-cache:registry	(make-hashtable equal-hash equal?)))

  (library-cache:register '(proof one)
			  '(library (proof one)
			     (export a b c
				     (rename (a alpha)
					     (b beta)))
			     (import (rnrs)
			       (lists))
			     (define a 1)
			     (define (b arg)
			       (vector arg))
			     (define-syntax c
			       (syntax-rules ()
				 ((_ ?ch)
				  (string ?ch))))))

;;; --------------------------------------------------------------------

  (let-fields (((lib <library>) (load-library '(proof one))))

    (check
	lib.raw-exports
      => '(a b c (rename (a alpha) (b beta))))

    (check
	lib.raw-imports
      => '((rnrs) (lists)))

    (check
	lib.exports
      => '((a a) (b b) (c c) (a alpha) (b beta)))


    (check
	lib.imported-libraries
      => '((rnrs) (lists)))

    #f)

  #t)


(parametrise ((check-test-name		'imported-libraries)
	      (load-library-function	library-cache:load-library)
	      (library-cache:registry	(make-hashtable equal-hash equal?)))

  (library-cache:register '(proof one)
			  '(library (proof one)
			     (export)
			     (import (rnrs) (lists))))

  (library-cache:register '(proof two)
			  '(library (proof two)
			     (export)
			     (import (rnrs)
			       (only (a))
			       (except (b))
			       (prefix (c) p)
			       (rename (d))
			       (library (e))
			       )))

  (library-cache:register '(proof three)
			  '(library (proof three)
			     (export)
			     (import (rnrs)
			       (only (a) alpha beta)
			       (except (b) delta)
			       (prefix (c) gamma:)
			       (rename (d)
				       (ciao hello))
			       )))

;;; --------------------------------------------------------------------

  (let-fields (((lib <library>) (load-library '(proof one))))
    (check lib.imported-libraries => '((rnrs) (lists)))
    #f)

  (let-fields (((lib <library>) (load-library '(proof two))))
    (check lib.imported-libraries => '((rnrs) (a) (b) (c) (d) (e)))
    #f)

  (let-fields (((lib <library>) (load-library '(proof three))))
    (check lib.imported-libraries => '((rnrs) (a) (b) (c) (d)))
    #f)

  (let-fields (((lib <library>) (load-library '(rnrs unicode))))
    (check lib.imported-libraries => '())
    #f)

;;; --------------------------------------------------------------------



  #t)


(parametrise ((check-test-name		'imported-bindings)
	      (load-library-function	library-cache:load-library)
	      (library-cache:registry	(make-hashtable equal-hash equal?)))

(hashtable-clear! (library-cache:registry))

  (library-cache:register '(proof one)
			  '(library (proof one)
			     (export)
			     (import (rnrs unicode))))

;;; --------------------------------------------------------------------

  (let-fields (((lib <library>) (load-library '(rnrs unicode))))
    (check lib.imported-bindings => '())
    #f)

  (let-fields (((lib <library>) (load-library '(proof one))))
    (check lib.imported-bindings => '())
    #f)

  #t)


;;;; done

(check-report)

;;; end of file
