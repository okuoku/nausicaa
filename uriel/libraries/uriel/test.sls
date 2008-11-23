;;;
;;;Part of: Uriel libraries
;;;Contents: utilities for unit testing
;;;Date: Wed Nov 19, 2008
;;;Time-stamp: <2008-11-23 08:50:43 marco>
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

(library (uriel test)
  (export
    with-result add-result get-result
    catch-exception false-if-exception
    check-for-true
    (rename (check-it check)) check-report check-ec check-set-mode!
    testname parameterize)
  (import (rnrs)
    (srfi parameters)
    (srfi lightweight-testing)
    (only (string-lib) string-suffix? string-prefix?)
    (only (ikarus) getenv))


;;;; code

(define result
  (make-parameter #f))

(define-syntax with-result
  (syntax-rules ()
    ((_ ?form0 ?form ...)
     (parameterize ((result '()))
       (list (begin ?form0 ?form ...)
	     (reverse (result)))))))

(define (add-result value)
  (result (cons value (result))))

(define (get-result)
  (result))

(define-syntax catch-exception
  (syntax-rules ()
    ((_ ?form0 ?form ...)
     (guard (exc (else exc))
       ?form0 ?form ...))))

(define-syntax false-if-exception
  (syntax-rules ()
    ((_ ?form0 ?form ...)
     (guard (exc (else #f))
       ?form0 ?form ...))))

(define testname
  (make-parameter #f
    (lambda (value)
      (unless (or (not value) (string? value) (symbol? value))
	(assertion-violation 'testname
	  "expected #f or string as parameter value" value))
      (if (symbol? value)
	  (symbol->string value)
	value))))

(define selected-test (getenv "NAME"))

(define (check-activation)
  (or (= 0 (string-length selected-test))
      (if (testname)
	  (or (string-prefix? selected-test (testname))
	      (string-suffix? selected-test (testname)))
	#f)))

(define-syntax check-it
  (syntax-rules (=>)
    ((_ ?expr => ?expected-result)
     (check-it ?expr (=> equal?) ?expected-result))

    ((_ ?expr (=> ?equal) ?expected-result)
     (when (check-activation)
       (check ?expr (=> ?equal) ?expected-result)))

    ((_ ?name ?expr => ?expected-result)
     (check-it ?name ?expr (=> equal?) ?expected-result))

    ((_ ?name ?expr (=> ?equal) ?expected-result)
     (parameterize ((testname ?name))
       (when (check-activation)
	 (check ?expr (=> ?equal) ?expected-result))))
    ))

(define-syntax check-for-true
  (syntax-rules ()
    ((_ ?form)
     (check-it (and ?form #t) => #t))))


;;;; done


) ;; end of library form

;;; end of file
