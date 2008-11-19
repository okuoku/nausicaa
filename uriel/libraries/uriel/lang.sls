;;;
;;; Part of: Uriel libraries
;;; Contents: Scheme language extensions
;;; Date: Mon Nov  3, 2008
;;; 
;;; Abstract
;;; 
;;; 
;;; Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
;;; 
;;; This program is free  software: you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation, either version 3 of
;;; the License, or (at your option) any later version.
;;; 
;;; This  program is  distributed  in  the hope  that  it will  be
;;; useful,  but WITHOUT  ANY WARRANTY;  without even  the implied
;;; warranty  of  MERCHANTABILITY  or  FITNESS  FOR  A  PARTICULAR
;;; PURPOSE.  See the GNU General Public License for more details.
;;; 
;;; You  should have  received a  copy of  the GNU  General Public
;;; License    along   with   this    program.    If    not,   see
;;; <http://www.gnu.org/licenses/>.
;;; 


;;; --------------------------------------------------------------------
;;; Setup.
;;; --------------------------------------------------------------------

(library (uriel lang)
  (export
      begin0

    with-compensations let-compensations let-compensations*
    compensate run-compensations

    with-deferred-exception-handler defer-exceptions
    run-deferred-exception-handlers
    )
  (import (rnrs)
    (srfi parameters))

;;; --------------------------------------------------------------------


;;; --------------------------------------------------------------------
;;; Simple sintaxes.
;;; --------------------------------------------------------------------

;;;This  syntax  comes  from  the  R6RS original  document,  Appendix  A
;;;``Formal semantics''.
(define-syntax begin0
  (syntax-rules ()
    ((_ ?expr0 ?expr ...)
     (call-with-values
	 (lambda () ?expr0)
       (lambda x
	 ?expr ...
	 (apply values x))))))


;;; --------------------------------------------------------------------


;;; --------------------------------------------------------------------
;;; Deferred exceptions.
;;; --------------------------------------------------------------------

(define deferred-exceptions
  (make-parameter #f))

(define deferred-exceptions-handler
  (make-parameter #f))

(define (run-deferred-exception-handlers)
  (when (deferred-exceptions)
    (for-each
	(lambda (exc)
	  ((deferred-exceptions-handler) exc))
      (deferred-exceptions))))

(define-syntax defer-exceptions
  (syntax-rules ()
    ((_ ?form0 ?form ...)
     (guard (exc (else
		  (when (deferred-exceptions)
		    (deferred-exceptions
		      (cons exc (deferred-exceptions))))))
       ?form0 ?form ...))))

(define-syntax with-deferred-exception-handler
  (syntax-rules ()
    ((_ ?handler ?form0 ?form ...)
     (parameterize ((deferred-exceptions '())
		    (deferred-exceptions-handler ?handler))
       (dynamic-wind
	   (lambda () #f)
	   (lambda () ?form0 ?form ...)
	   (lambda ()
	     (run-deferred-exception-handlers)))))))

;;; --------------------------------------------------------------------


;;; --------------------------------------------------------------------
;;; Compensations.
;;; --------------------------------------------------------------------

(define compensations
  (make-parameter #f))

(define (run-compensations)
  (when (compensations)
    (for-each
	(lambda (closure)
	  (defer-exceptions
	    (closure)))
      (compensations)))
  (compensations '()))

(define-syntax with-compensations
  (syntax-rules ()
    ((_ ?form0 ?form ...)
     (parameterize ((compensations '()))
       (with-exception-handler 
	   (lambda (exc)
	     (run-compensations)
	     (raise exc))
	 (lambda () ?form0 ?form ...))))))

(define-syntax let-compensations*
  (syntax-rules ()
    ((_ ?bindings ?form0 ?form ...)
     (with-compensations
       (let* ?bindings
	 (begin0
	     (begin ?form0 ?form ...)
	   (run-compensations)))))))

(define-syntax let-compensations
  (syntax-rules ()
    ((_ ?bindings ?form0 ?form ...)
     (with-compensations
       (let ?bindings
	 (begin0
	     (begin ?form0 ?form ...)
	   (run-compensations)))))))

(define-syntax compensate
  (syntax-rules (begin with)
    ((_ (begin ?alloc0 ?alloc ...) (with ?release0 ?release ...))
     (begin0
	 (begin ?alloc0 ?alloc ...)
       (compensations (cons (lambda () ?release0 ?release ...)
			    (compensations)))))

    ((_ (begin ?alloc0 ?alloc ...) ?allocn ?form ...)
     (compensate (begin ?alloc0 ?alloc ... ?allocn) ?form ...))

    ((_ ?alloc ?form ...)
     (compensate (begin ?alloc) ?form ...))))

;; ------------------------------------------------------------


;;; --------------------------------------------------------------------
;;; Done.
;;; --------------------------------------------------------------------

) ;; end of library form

;;; end of file
