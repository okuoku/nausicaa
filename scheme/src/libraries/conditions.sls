;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: predefined condition types
;;;Date:Thu Sep  3, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!r6rs
(library (conditions)
  (export

    define-condition

    ;; mismatch
    &mismatch make-mismatch-condition mismatch-condition?

    ;; wrong num args
    &wrong-num-args make-wrong-num-args-condition wrong-num-args-condition?
    condition-wrong-num-args/procname
    condition-wrong-num-args/expected
    condition-wrong-num-args/given
    raise-wrong-num-args-error

    ;; unimplemented
    &unimplemented make-unimplemented-condition unimplemented-condition?
    raise-unimplemented-error)
  (import (rnrs)
    (unimplemented)
    (makers)
    (only (auxiliary-syntaxes) parent fields)
    (for (syntax-utilities) expand))


(define-syntax define-condition
  (syntax-rules ()
    ((_ ?name ?clause ...)
     (%define-condition/collect-clauses (quote (define-condition ?name ?clause ...))
					?name
					() ;parent
					() ;fields
					?clause ...))))

(define-syntax %define-condition/collect-clauses
  (lambda (stx)
    (define (synner message subform/stx)
      (syntax-violation 'define-condition message (syntax->datum stx) (syntax->datum subform/stx)))

    (syntax-case stx (parent fields)

      ;; no more clauses
      ((_ ?input-form ?name (?par ...) (?fie ...))
       #'(%define-condition/fix-parent ?input-form ?name (?par ...) (?fie ...)))

      ;; error if PARENT given twice
      ((_ ?input-form ?name (?par) (?fie ...) (parent ?parent) ?clause ...)
       (synner "PARENT clause given twice in condition type definition" #'(parent ?parent)))

      ;; collect PARENT clause
      ((_ ?input-form ?name () (?fie ...) (parent ?parent) ?clause ...)
       #'(%define-condition/collect-clauses ?input-form ?name (?parent) (?fie ...) ?clause ...))

      ;; error if FIELDS given twice
      ((_ ?input-form ?name (?par ...) (?fie0 ?fie ...) (fields ?field ...) ?clause ...)
       (synner "FIELDS clause given twice in condition type definition" #'(fields ?field ...)))

      ;; error if field names are not identifiers
      ((_ ?input-form ?name (?par ...) () (fields ?field ...) ?clause ...)
       (not (all-identifiers? #'(?field ...)))
       (synner "condition type field specification must be an identifier" #'(fields ?field ...)))

      ;; collect FIELDS clause
      ((_ ?input-form ?name (?par ...) () (fields ?field ...) ?clause ...)
       #'(%define-condition/collect-clauses ?input-form ?name (?par ...) (?field ...) ?clause ...))
      )))

(define-syntax %define-condition/fix-parent
  ;;If there is no parent, default to "&error"
  ;;
  (syntax-rules ()
    ((_ ?input-form ?name () (?fie ...))
     (%define-condition/output ?input-form ?name &error (?fie ...)))
    ((_ ?input-form ?name (?parent) (?fie ...))
     (%define-condition/output ?input-form ?name ?parent (?fie ...)))))

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
    (define (%raise-syntax-name name)
      (string->symbol (string-append "raise-" name "-error")))
    (syntax-case stx ()
      ((_ ?input-form ?name ?parent (?field ...))
       (let ((name (%type-name->name (syntax->datum #'?name))))
	 (with-syntax ((CONSTRUCTOR	(datum->syntax #'?name (%name->constructor-name name)))
		       (PREDICATE	(datum->syntax #'?name (%name->predicate-name name)))
		       ((ACCESSOR ...)	(datum->syntax #'?name (%accessors name #'(?field ...))))
		       (RAISE-STX-NAME	(datum->syntax #'?name (%raise-syntax-name name))))
	   #'(begin
	       (define-condition-type ?name ?parent CONSTRUCTOR PREDICATE
		 (?field ACCESSOR) ...)

	       (define-syntax RAISE-STX-NAME
		 (syntax-rules ()
		   ((_ ?who ?message ?field ...)
		    (raise
		     (condition (make-who-condition ?who)
				(make-message-condition ?message)
				(CONSTRUCTOR ?field ...)
				(make-non-continuable-violation))))))
	       )))))))


(define-condition &mismatch
  (parent &assertion))

(define-condition &wrong-num-args
  (parent &assertion)
  (fields procname expected given))


;;;; done

)

;;; end of file
