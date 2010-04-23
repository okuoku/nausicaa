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
  (libraries names)
  (libraries references)
  (libraries import-specs)
  (libraries conditions)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing libraries\n")


;;;; library cache
;;
;;This cache  is for the custom LOAD-LIBRARY  function, selected through
;;the LOAD-LIBRARY-FUNCTION  parameter.  Remember that  LOAD-LIBRARY has
;;its   own    cache,   which   will   be    queried   before   invoking
;;(LOAD-LIBRARY-FUNCTION); so  we have to  keep unique the names  of the
;;libraries we define this file.
;;

(define test-cache:registry
  (make-parameter #f))

(define (test-cache:make-table)
  (make-hashtable equal-hash equal?))

(define (test-cache:load-library spec)
  (hashtable-ref (test-cache:registry) spec #f))

(define (test-cache:register spec sexp)
  (hashtable-set! (test-cache:registry) spec sexp))


(parametrise ((check-test-name	'loading))

  (check
      (let ((lib (load-library '(lists))))
	(is-a? lib <library>))
    => #t)

  (check
      (let ((lib (load-library '(nausicaa))))
	(is-a? lib <library>))
    => #t)

  (check
      (let* (((lib <library>)		(load-library '(lists)))
	     ((name <library-name>)	lib.name))
	name.identifiers)
    => '(lists))

  (check
      (guard (E ((library-not-found-condition? E)
		 #t)
		(else
		 (write E)(newline)
		 #f))
	(load-library '(it is impossible for this library to exist right?)))
    => #t)

  #f)


#;(parametrise ((check-test-name		'matching-basic)
	      (load-library-function	test-cache:load-library)
	      (test-cache:registry	(test-cache:make-table)))

  (test-cache:register '(test matching-basic one)
		       '(library (test matching-basic one)
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

  (let-fields (((lib <library>) (load-library '(test matching-basic one))))

    (check
	lib.requested-library-name
      => '(test matching-basic one))

    (check
	lib.library-name
      => '(test matching-basic one))

    (check
	lib.imported-libraries
      => '((rnrs) (lists)))

    #f)

  #t)


#;(parametrise ((check-test-name		'exported-bindings)
	      (load-library-function	test-cache:load-library)
	      (test-cache:registry	(test-cache:make-table)))

  (test-cache:register '(test exported-bindings one)
		       '(library (test exported-bindings one)
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

  (let-fields (((lib <library>) (load-library '(test matching-basic one))))

    (check
	lib.exports
      => '((a a) (b b) (c c) (a alpha) (b beta)))

    #f)
  #t)


#;(parametrise ((check-test-name		'imported-libraries)
	      (load-library-function	test-cache:load-library)
	      (test-cache:registry	(test-cache:make-table)))

  (test-cache:register '(test imported-libraries one)
		       '(library (test imported-libraries one)
			  (export)
			  (import (rnrs) (lists))))

  (test-cache:register '(test imported-libraries two)
		       '(library (test imported-libraries two)
			  (export)
			  (import (rnrs)
			    (only (a))
			    (except (b))
			    (prefix (c) p)
			    (rename (d))
			    (library (e))
			    )))

  (test-cache:register '(test imported-libraries three)
		       '(library (test imported-libraries three)
			  (export)
			  (import (rnrs)
			    (only (a) alpha beta)
			    (except (b) delta)
			    (prefix (c) gamma:)
			    (rename (d)
				    (ciao hello))
			    )))

;;; --------------------------------------------------------------------

  (let-fields (((lib <library>) (load-library '(test imported-libraries one))))
    (check lib.imported-libraries => '((rnrs) (lists)))
    #f)

  (let-fields (((lib <library>) (load-library '(test imported-libraries two))))
    (check lib.imported-libraries => '((rnrs) (a) (b) (c) (d) (e)))
    #f)

  (let-fields (((lib <library>) (load-library '(test imported-libraries three))))
    (check lib.imported-libraries => '((rnrs) (a) (b) (c) (d)))
    #f)

  (let-fields (((lib <library>) (load-library '(rnrs unicode))))
    (check lib.imported-libraries => '())
    #f)

;;; --------------------------------------------------------------------



  #t)


#;(parametrise ((check-test-name		'imported-bindings)
	      (load-library-function	test-cache:load-library)
	      (test-cache:registry	(test-cache:make-table)))

  (test-cache:register '(test imported-bindings one)
		       '(library (test imported-bindings one)
			  (export)
			  (import (rnrs sorting)
			    (rnrs control (6)))))

;;; --------------------------------------------------------------------

  (let-fields (((lib <library>) (load-library '(rnrs unicode))))
    (check (lib.imported-bindings) => '())
    #f)

  (let-fields (((lib <library>) (load-library '(test imported-bindings one))))
    (check (lib.imported-bindings) => '((list-sort list-sort)
					(vector-sort vector-sort)
					(vector-sort! vector-sort!)
					(when when)
					(unless unless)
					(do do)
					(case-lambda case-lambda)))
    #f)

  #t)


;;;; done

(check-report)

;;; end of file
