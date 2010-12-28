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
(library (nausicaa language conditions)
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
    procname expected given

    ;; unimplemented
    &unimplemented make-unimplemented-condition unimplemented-condition?
    raise-unimplemented-error)
  (import (rnrs)
    (nausicaa language sentinel)
    (nausicaa language unimplemented)
    (nausicaa language makers)
    (only (nausicaa language auxiliary-syntaxes) parent fields)
    (for (only (nausicaa language syntax-utilities) all-identifiers?) expand)
    (for (only (nausicaa language syntax-utilities) define-auxiliary-syntax) run))


(define-maker (define-condition name)
  %define-condition
  ((parent &error)
   (fields sentinel)))

(define-syntax %define-condition
  (lambda (stx)
    (define (main)
      (syntax-case stx (list sentinel)
	((_ ?name ?parent (list ?field ...))
	 (begin
	   (unless (identifier? #'?name)
	     (%synner "expected identifier as condition name" #'?name))
	   (unless (identifier? #'?parent)
	     (%synner "expected identifier as condition parent name" #'(parent ?parent)))
	   (unless (all-identifiers? #'(?field ...))
	     (%synner "expected identifiers as condition field names" #'(fields ?field ...)))

	   (let ((name-str (%name-stx->name-str #'?name)))
	     (with-syntax
		 ((CONSTRUCTOR		(%name-str->constructor-name-stx #'?name name-str))
		  (PREDICATE		(%name-str->predicate-name-stx   #'?name name-str))
		  ((ACCESSOR ...)	(%accessors-stx #'?name name-str #'(?field ...)))
		  (RAISE-STX-NAME	(%name-str->raise-syntax-name #'?name name-str)))
	       #'(begin
		   (define-condition-type ?name ?parent CONSTRUCTOR PREDICATE (?field ACCESSOR) ...)

		   (define-auxiliary-syntax ?field ...)

		   (define-maker (RAISE-STX-NAME who message)
		     raise-it
		     ((?field (syntax-violation 'RAISE-STX-NAME "missing field value" '?field)) ...))

		   (define-syntax raise-it
		     (syntax-rules ()
		       ((_ ?who ?message ?field ...)
			(raise
			 (condition (make-who-condition ?who)
				    (make-message-condition ?message)
				    (CONSTRUCTOR ?field ...)
				    (make-non-continuable-violation))))))
		   )))))
	((?k ?name ?parent sentinel)
	 #'(?k ?name ?parent (list)))
	((?k ?name ?parent ?field)
	 #'(?k ?name ?parent (list ?field)))
	))

    (define (%name-stx->name-str type-name-stx)
      ;;Given an identifier representing the condition object type name,
      ;;check if  its first character is  an ampersand: if  it is return
      ;;the name itself, as a string, with the ampersand stripped; if it
      ;;is not, raise a syntax violation.
      ;;
      (let ((string-name (symbol->string (syntax->datum type-name-stx))))
	(if (char=? #\& (string-ref string-name 0))
	    (substring string-name 1 (string-length string-name))
	  (%synner "condition type name must begin with \"&\" character" type-name-stx))))

    (define (%name-str->constructor-name-stx lexical-context name-str)
      ;;Given the condition type name as a string, return an identifier,
      ;;in  LEXICAL-CONTEXT,  representing  the  name of  the  condition
      ;;object's constructor.
      ;;
      (datum->syntax lexical-context (string->symbol (string-append "make-" name-str "-condition"))))

    (define (%name-str->predicate-name-stx lexical-context name-str)
      ;;Given the condition type name as a string, return an identifier,
      ;;in  LEXICAL-CONTEXT,  representing  the  name of  the  condition
      ;;object's predicate.
      ;;
      (datum->syntax lexical-context (string->symbol (string-append name-str "-condition?"))))

    (define (%accessors-stx lexical-context name-str fields-stx)
      ;;Given the  condition type name as  a string and  a syntax object
      ;;holding a list of field  names, return a list of identifiers, in
      ;;LEXICAL-CONTEXT,  representing   the  names  of   the  condition
      ;;object's fields accessors.
      ;;
      (map (lambda (field-symbol)
	     (datum->syntax lexical-context
			    (string->symbol (string-append "condition-" name-str "/"
							   (symbol->string field-symbol)))))
	(syntax->datum fields-stx)))

    (define (%name-str->raise-syntax-name lexical-context name-str)
      ;;Given the condition type name as a string, return an identifier,
      ;;in LEXICAL-CONTEXT, representing the  name of the syntax used to
      ;;raise a compound condition object holding the type.
      ;;
      (datum->syntax lexical-context (string->symbol (string-append "raise-" name-str "-error"))))

    (define (%synner message subform)
      (syntax-violation 'define-condition message (syntax->datum stx) (syntax->datum subform)))

    (main)))


(define-condition &mismatch
  (parent &assertion))

(define-condition &wrong-num-args
  (parent &assertion)
  (fields procname expected given))


;;;; done

)

;;; end of file
