;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: sub-interpreters implementation
;;;Date: Fri Jun 18, 2010
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


(library (interps)
  (export

    &interp-error
    make-interp-error-condition
    interp-error-condition?
    condition-interp-error/interp

    <interp>			<interp>?
    <interp>-eval
    <interp>-variable-set!	<interp>-variable-ref

    ;; auxiliary syntaxes
    imports:)
  (import (nausicaa)
    (interps variables)
    (interps variable-events)
    (rnrs eval)
    (sentinel))


;;;; helpers

(define undefined-variable
  (make-sentinel))


(define-condition &interp-error
  (parent &error)
  (fields interp))

(define-syntax* (class-case stx)
  (syntax-case stx (else)
    ((_ ?thing ((?class) . ?body) ... (else . ?else-body))
     (identifier? #'?thing)
     #'(cond ((is-a? ?thing ?class)
	      (with-class ((?thing ?class))
		. ?body))
	     ...
	     (else . ?else-body)))
    ((_ ?thing ((?class) . ?body) ...)
     (identifier? #'?thing)
     #'(cond ((is-a? ?thing ?class)
	      (with-class ((?thing ?class))
		. ?body))
	     ...))
    (_
     (synner "invalid syntax"))
    ))


(define-constant $default-import-specs
  '((only (interps variables) define-variable)))

(define-auxiliary-syntaxes
  imports:)

(define-class <interp>
  (nongenerative nausicaa:interps:<interp>)

  (fields (immutable table-of-variables)
	  (immutable eval-environment))

  (protocol (lambda (make-top)
	      (lambda (list-of-import-specs)
		((make-top) (make-eq-hashtable)
		 (apply environment (append $default-import-specs list-of-import-specs))))))

  (maker ()
	 (imports: '((rnrs))))

  (methods eval variable-ref variable-set!))


(define (<interp>-eval (o <interp>) body)

  (define-class <results>
    (nongenerative interps:<results>)
    (fields (immutable values)))

  (define (end-of-eval vals)
    (make <results>
      vals))

  (define-inline (raise-undefined-variable variable-name)
    (raise (condition
	    (make-who-condition '<interp>-eval)
	    (make-message-condition "attempt to access undefined variable in interpreter")
	    (make-interp-error-condition o)
	    (make-irritants-condition (list variable-name)))))

  (define vars-body
    (receive (keys vals)
	(hashtable-entries o.table-of-variables)
      (let ((number-of-variables (vector-length keys)))
	(let loop ((i 0) (defs '()))
	  (if (= i number-of-variables)
	      (reverse defs)
	    (loop (+ 1 i) (cons `(define-global ,(vector-ref keys i) (quote ,(vector-ref vals i)))
				defs)))))))
  (define expression
    `(call/cc (lambda (eval-kont)
		(define-syntax define-global
		  (syntax-rules ()
		    ((_ . ?args)
		     (define-variable eval-kont . ?args))))
		(let ()
		  (define define-variable) ;shadows the binding
		  (define eval-kont)	   ;shadows the binding
		  (call-with-values
		      (lambda ()
			,@vars-body ,body)
		    (lambda vals
		      ((quote ,end-of-eval) vals)))))))

  (let ((R (with-exception-handler
	       (lambda (E)
		 (if (interp-error-condition? E)
		     E
		   (raise (condition E (make-interp-error-condition o)))))
	     (lambda ()
	       (eval expression o.eval-environment)))))
    (class-case R
		((<results>)
		 (apply values R.values))
		((<variable-mutation>)
		 (hashtable-set! o.table-of-variables R.name R.value)
		 (R.kont))
		((<variable-reference>)
		 (R.kont
		  (let ((value (hashtable-ref o.table-of-variables R.name undefined-variable)))
		    (if (eq? value undefined-variable)
			(raise-undefined-variable R.name)
		      value))))
		(else
		 (assertion-violation '<interp>-eval
		   "invalid return value from interp evaluation" R)))
    ))


(define (<interp>-variable-set! (o <interp>) variable-name variable-value)
  (hashtable-set! o.table-of-variables variable-name variable-value))

(define (<interp>-variable-ref  (o <interp>) variable-name default)
  (hashtable-ref o.table-of-variables variable-name default))


;;;; done

)

;;; end of file
