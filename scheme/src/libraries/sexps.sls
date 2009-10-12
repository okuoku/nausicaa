;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: S-expressions manipulation
;;;Date: Fri Aug 28, 2009
;;;
;;;Abstract
;;;
;;;	The  original  code was  posted  on  comp.lang.scheme by  Pascal
;;;	Bourgignon on Aug 29,  2009 in the thread "generic S-expressions
;;;	library".
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
;;;Copyright (c) 2009 Pascal Bourguignon
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
(library (sexps)
  (export
    sexp-match sexp-match?
    sexp-var sexp-var-rest
    sexp-pred sexp-or sexp-or* sexp-and sexp-and*
    sexp-any sexp-any* sexp-one sexp-one*
    let-sexp-variables sexp-variable sexp-variable?
    sexp-variable-name sexp-variable-default
    make-sexp-transformer sexp-substitute-bindings

    ;; conditions
    sexp-mismatch-error
    &sexp-mismatch make-sexp-mismatch sexp-mismatch?
    sexp-mismatch-pattern sexp-mismatch-form)
  (import (rnrs)
    (sentinel)
    (conditions))

(define-syntax form-car
  (syntax-rules ()
    ((_ ?form)
     (if (null? ?form) '() (car ?form)))))

(define-syntax form-cdr
  (syntax-rules ()
    ((_ ?form)
     (if (null? ?form) '() (cdr ?form)))))


(define-record-type sexp-variable
  (fields (immutable name)
	  (immutable default)))

(define-syntax let-sexp-variables
  (syntax-rules ()
    ((_ ((?var ?default) ...) ?form0 ?form ...)
     (let ((?var (make-sexp-variable (quote ?var) ?default)) ...) ?form0 ?form ...))))

(define-condition-type &sexp-mismatch
  &mismatch
  make-sexp-mismatch
  sexp-mismatch?
  (pattern sexp-mismatch-pattern)
  (form    sexp-mismatch-form))

(define-syntax sexp-mismatch-error
  ;;Being a syntax it yields a better stack trace?
  ;;
  (syntax-rules ()
    ((_ ?who ?pattern ?form)
     (raise (condition (make-sexp-mismatch ?pattern ?form)
		       (make-who-condition ?who)
		       (make-message-condition "S-expressions mismatch"))))))


(define (sexp-match pattern form)
  (cond ((pair? pattern)
	 (let ((pattern-token (car pattern))
	       (form-token    (form-car form)))
	   (cond ((pair? pattern-token)
		  (if (pair? form-token)
		      (append (sexp-match pattern-token form-token)
			      (sexp-match (cdr pattern) (form-cdr form)))
		    (sexp-mismatch-error 'sexp-match pattern form)))

		 ((procedure? pattern-token)
		  (call-with-values
		      (lambda ()
			(pattern-token form))
		    (lambda (bindings form-rest)
		      (append bindings
			      (sexp-match (cdr pattern) form-rest)))))

		 (else
		  (if (equal? pattern-token form-token)
		      (sexp-match (cdr pattern) (form-cdr form))
		    (sexp-mismatch-error 'sexp-match pattern-token form-token))))))

	((and (null? pattern) (null? form))
	 '())

	(else
	 (sexp-mismatch-error 'sexp-match pattern form))))

(define (sexp-match? pattern form)
  (guard (C ((sexp-mismatch? C) #f))
    (sexp-match pattern form)
    #t))	;enforce a boolean as return value


(define (sexp-var sexp-variable)
  ;;Match the car of FORM, binding it to SEXP-VARIABLE.
  ;;
  (lambda (form)
    (if (null? form)
	(sexp-mismatch-error 'sexp-var (list 'sexp-var sexp-variable) form)
      (values `((,sexp-variable . ,(car form))) (cdr form)))))

(define (sexp-var-rest sexp-variable)
  ;;Match the all of FORM, binding it to SEXP-VARIABLE.
  ;;
  (lambda (form)
    (if (null? form)
	(sexp-mismatch-error 'sexp-var (list 'sexp-var sexp-variable) form)
      (values `((,sexp-variable . ,form)) '()))))

(define (sexp-pred pred)
  ;;Attempt  to  match the  car  of  FORM by  applying  PRED  to it  and
  ;;expecting true.
  ;;
  (lambda (form)
    (if (pred (form-car form))
	(values '() (form-cdr form))
      (sexp-mismatch-error 'sexp-pred (list 'sexp-pred pred) (form-car form)))))

(define (sexp-or . alternatives)
  ;;Attempt to  match the  car of FORM  with each sexp  in ALTERNATIVES,
  ;;halting at  the first positive match.   The match is  positive if at
  ;;least one of the alternatives does match.
  ;;
  (lambda (form)
    (let ((form-token (form-car form)))
      (let loop ((alts alternatives))
	(if (null? alts)
	    (sexp-mismatch-error 'sexp-or (cons 'sexp-or alternatives) form-token)
	  (guard (E ((sexp-mismatch? E)
		     (loop (cdr alts))))
	    (values (sexp-match (list (car alts)) (list form-token))
		    (form-cdr form))))))))

(define (sexp-and . alternatives)
  ;;Attempt to  match the  car of FORM  with each sexp  in ALTERNATIVES,
  ;;halting at  the first  mismatch.  The match  is positive if  all the
  ;;alternatives do match.
  ;;
  ;;If ALTERNATIVES  is null, the car  of FORM always matches  and it is
  ;;discarded.
  ;;
  (lambda (form)
    (let ((frm-token (form-car form)))
      (let loop ((bindings   '())
		 (alts       alternatives))
	(if (null? alts)
	    (values bindings (form-cdr form))
	  (loop (append bindings (sexp-match (list (car alts)) (list frm-token)))
		(cdr alts)))))))

(define (sexp-any pattern)
  ;;Attempt to match  every sexp in FORM with  the sexp PATTERN, halting
  ;;at the first mismatch.  It is fine if PATTERN does not match the car
  ;;of FORM.
  ;;
  (let ((pattern (list pattern)))
    (lambda (form)
      (let loop ((bindings '())
		 (form     form))
	(if (null? form)
	    (values bindings form)
	  (guard (E ((sexp-mismatch? E)
		     (values bindings form)))
	    (loop (append bindings (sexp-match pattern (list (form-car form))))
		  (form-cdr form))))))))

(define (sexp-one pattern)
  ;;Attempt to match  every sexp in FORM with  the sexp PATTERN, halting
  ;;at the first mismatch.  At least the car of FORM must match PATTERN.
  ;;
  (let ((pattern (list pattern)))
    (lambda (form)
      (let loop ((bindings (sexp-match pattern (list (form-car form))))
		 (form     (form-cdr form)))
	(if (null? form)
	    (values bindings form)
	  (guard (E ((sexp-mismatch? E)
		     (values bindings form)))
	    (loop (append bindings (sexp-match pattern (list (form-car form))))
		  (form-cdr form))))))))


(define-syntax sexp-or*
  (syntax-rules ()
    ((_ ?alternative ...)
     (sexp-or (quote ?alternative) ...))))

(define-syntax sexp-and*
  (syntax-rules ()
    ((_ ?alternative ...)
     (sexp-and (quote ?alternative) ...))))

(define-syntax sexp-any*
  (syntax-rules ()
    ((_ ?pattern)
     (sexp-any (quote ?pattern)))))

(define-syntax sexp-one*
  (syntax-rules ()
    ((_ ?pattern)
     (sexp-one (quote ?pattern)))))


(define (make-sexp-transformer pattern output)
  (lambda (form)
    (sexp-substitute-bindings output
			      (sexp-match pattern form))))

(define (sexp-substitute-bindings output bindings-alist)
  (let recur ((sexp output))
    (cond ((pair? sexp)
	   (cons (recur (car sexp)) (recur (cdr sexp))))
	  ((sexp-variable? sexp)
	   (let ((p (assq sexp bindings-alist)))
	     (cond (p
		    (cdr p))
		   ((not (sentinel? (sexp-variable-default sexp)))
		    (sexp-variable-default sexp))
		   (else
		    (assertion-violation #f
		      (string-append "unbound variable \""
				     (symbol->string (sexp-variable-name sexp))
				     "\" in output S-expression")
		      output)))))
	  (else sexp))))


;;;; done

)

;;; end of file
