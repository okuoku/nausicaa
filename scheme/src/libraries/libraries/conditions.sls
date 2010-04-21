;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: condition object typedefs
;;;Date: Wed Apr 21, 2010
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


(library (libraries conditions)
  (export
    &libraries-error
    make-libraries-error-condition
    libraries-error-condition?

    &library-not-found
    make-library-not-found-condition
    library-not-found-condition?
    condition-library-not-found/reference
    raise-library-not-found

    )
  (import (nausicaa))


(define-syntax define-condition
  (syntax-rules ()
    ((_ ?name ?clause ...)
     (%define-condition/collect-clauses (quote (define-condition ?name ?clause ...))
					?name
					() ;parent
					() ;fields
					?clause ...))))

(define-syntax %define-condition/collect-clauses
  (syntax-rules (parent fields)

    ;; no more clauses
    ((_ ?input-form ?name (?parent) (?field ...))
     (%define-condition/output ?input-form ?name ?parent (?field ...)))

    ;; error if PARENT given twice
    ((_ ?input-form ?name (?par) (?fie ...) (parent ?parent) ?clause ...)
     (syntax-violation 'define-condition
       "parent clause given twice in condition type definition"
       ?input-form (quote (parent ?parent))))

    ;; collect PARENT clause
    ((_ ?input-form ?name () (?fie ...) (parent ?parent) ?clause ...)
     (%define-condition/collect-clauses ?input-form ?name (?parent) (?fie ...) ?clause ...))

    ;; error if FIELDS given twice
    ((_ ?input-form ?name (?par ...) (?fie0 ?fie ...) (fields ?field ...) ?clause ...)
     (syntax-violation 'define-condition
       "fields clause given twice in condition type definition"
       ?input-form (quote (fields ?field ...))))

    ;; collect fields clause
    ((_ ?input-form ?name (?par ...) () (fields ?field ...) ?clause ...)
     (%define-condition/collect-fields ?input-form ?name (?par ...) () (?field ...) ?clause ...))
    ))

(define-syntax %define-condition/collect-fields
  (syntax-rules ()

    ;; no more fields
    ((_ ?input-form ?name (?par ...) (?fie ...) () ?clause ...)
     (%define-condition/collect-clauses ?input-form ?name (?par ...) (?fie ...) ?clause ...))

    ;; collect field
    ((_ ?input-form ?name (?par ...) (?fie ...) (?field0 ?field ...) ?clause ...)
     (%define-condition/collect-fields  ?input-form ?name (?par ...)
					(?fie ... ?field0) (?field ...) ?clause ...))
    ))

(define-syntax %define-condition/output
  (lambda (stx)
    (define (%type-name->name type-name)
      (let ((string-name (symbol->string type-name)))
	(if (char=? #\& (string-ref string-name 0))
	    (substring string-name 1 (string-length string-name))
	  (assertion-violation 'define-condition
	    "condition type name must begin with \"&\" character" type-name))))
    (define (%name->constructor-name name)
      (string->symbol (string-append "make-" name "-condition")))
    (define (%name->predicate-name name)
      (string->symbol (string-append name "-condition?")))
    (define (%accessors name fields-stx)
      (map (lambda (field)
	     (string->symbol (string-append "condition-" name "/" (symbol->string field))))
	(syntax->datum fields-stx)))
    (syntax-case stx ()
      ((_ ?input-form ?name ?parent (?field ...))
       (let ((name (%type-name->name (syntax->datum #'?name))))
	 (with-syntax ((CONSTRUCTOR	(datum->syntax #'?name (%name->constructor-name name)))
		       (PREDICATE	(datum->syntax #'?name (%name->predicate-name name)))
		       ((ACCESSOR ...)	(datum->syntax #'?name (%accessors name #'(?field ...)))))
	   #'(define-condition-type ?name ?parent CONSTRUCTOR PREDICATE
	       (?field ACCESSOR) ...)))))))


(define-condition &libraries-error (parent &error))

(define-condition &library-not-found
  (parent &libraries-error)
  (fields reference))

(define-inline (raise-library-not-found ?who ?reference)
  (raise
   (condition (make-library-not-found-condition ?reference)
	      (make-non-continuable-violation)
	      (make-who-condition ?who)
	      (make-message-condition "library not found"))))


;;;; done

)

;;; end of file
