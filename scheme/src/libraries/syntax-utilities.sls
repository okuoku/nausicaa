;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: helper functions for expand time processing
;;;Date: Wed May 26, 2010
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


(library (syntax-utilities)
  (export

    ;; wrapping
    unwrap-syntax-object		unwrap-options

    ;; inspection
    quoted-syntax-object?

    ;; identifiers hangling
    all-identifiers?			duplicated-identifiers?
    identifier-memq			symbol-identifier=?

    ;; common identifier names constructor
    identifier->string			string->identifier
    identifier-prefix			identifier-suffix
    syntax-maker-identifier		syntax-predicate-identifier
    syntax-accessor-identifier		syntax-mutator-identifier
    syntax-dot-notation-identifier

    ;; definition clauses handling
    validate-list-of-clauses		filter-clauses
    validate-definition-clauses)
  (import (rnrs))


(define-enumeration enum-unwrap-options
  (keep-general-quoted)
  unwrap-options)

(define unwrap-syntax-object
  (case-lambda
   ((stx)
    (unwrap-syntax-object stx (unwrap-options)))
   ((stx options)
    ;;Given   a  syntax  object   STX  decompose   it  and   return  the
    ;;corresponding S-expression  holding datums and  identifiers.  Take
    ;;care of returning a proper list  when the input is a syntax object
    ;;holding a proper list.
    ;;
    ;;This functions also  provides a workaround for bugs  in Ikarus and
    ;;Mosh,  which expand syntax  objects holding  a list  into IMproper
    ;;lists.
    ;;
    ;;Aaron Hsu contributed the  SYNTAX->LIST function through a post on
    ;;comp.lang.scheme: it was used as starting point for this function.
    ;;
    (syntax-case stx ()
      (()
       '())
      ((?car . ?cdr)
       (and (enum-set-member? 'keep-general-quoted options)
	    (identifier? #'?car)
	    (or (free-identifier=? #'?car #'quote)
		(free-identifier=? #'?car #'quasiquote)
		(free-identifier=? #'?car #'syntax)
		(free-identifier=? #'?car #'quasisyntax)))
       (syntax (?car . ?cdr)))
      ((?car . ?cdr)
       (cons (unwrap-syntax-object (syntax ?car))
	     (unwrap-syntax-object (syntax ?cdr))))
      (#(?item ...)
       (list->vector (unwrap-syntax-object (syntax (?item ...)))))
      (?atom
       (identifier? (syntax ?atom))
       (syntax ?atom))
      (?atom
       (syntax->datum (syntax ?atom)))))))


(define (quoted-syntax-object? stx)
  ;;Given a syntax object: return true if  it is a list whose car is one
  ;;among   QUOTE,  QUASIQUOTE,   SYNTAX,   QUASISYNTAX;  return   false
  ;;otherwise.
  ;;
  (syntax-case stx ()
    ((?car . ?cdr)
     (and (identifier? #'?car)
	  (or (free-identifier=? #'?car #'quote)
	      (free-identifier=? #'?car #'quasiquote)
	      (free-identifier=? #'?car #'syntax)
	      (free-identifier=? #'?car #'quasisyntax)))
     #t)
    (_ #f)))


(define (all-identifiers? stx)
  ;;Given  a syntax  object: return  true if  it is  null or  a  list of
  ;;identifiers; return false otherwise.
  ;;
  (syntax-case stx ()
    ((?car . ?cdr)
     (identifier? #'?car)
     (all-identifiers? #'?cdr))
    (()		#t)
    (_		#f)))

(define (duplicated-identifiers? ell/stx)
  ;;Recursive  function.  Search  the  list of  identifiers ELL/STX  for
  ;;duplicated  identifiers; at  the first  duplicate found,  return it;
  ;;return false if no duplications are found.
  ;;
  (if (null? ell/stx)
      #f
    (let loop ((x  (car ell/stx))
	       (ls (cdr ell/stx)))
      (if (null? ls)
	  (duplicated-identifiers? (cdr ell/stx))
	(if (bound-identifier=? x (car ls))
	    x
	  (loop x (cdr ls)))))))

(define (identifier-memq identifier list-of-syntax-objects)
  ;;Given  a   list  of   syntax  objects  search   for  one   which  is
  ;;FREE-IDENTIFIER=? to IDENTIFIER and return the sublist starting with
  ;;it; return false if IDENTIFIER is not present.
  ;;
  (assert (identifier? identifier))
  (cond ((null? list-of-syntax-objects)
	 #f)
	((let ((stx (car list-of-syntax-objects)))
	   (and (identifier? stx) (free-identifier=? identifier stx)))
	 list-of-syntax-objects)
	(else
	 (identifier-memq identifier (cdr list-of-syntax-objects)))))

(define (symbol-identifier=? id1 id2)
  (assert (identifier? id1))
  (assert (identifier? id2))
  (eq? (syntax->datum id1) (syntax->datum id2)))


(define (%general-string-append . args)
  (let-values (((port getter) (open-string-output-port)))
    (let loop ((args args))
      (if (null? args)
	  (getter)
	(let ((thing (car args)))
	  (display (if (identifier? (car args))
		       (identifier->string (car args))
		     (car args))
		   port)
	  (loop (cdr args)))))))

(define-syntax identifier->string
  (syntax-rules ()
    ((_ ?identifier)
     (symbol->string (syntax->datum ?identifier)))))

(define-syntax string->identifier
  (syntax-rules ()
    ((_ ?context-identifier ?string)
     (datum->syntax ?context-identifier (string->symbol ?string)))))

(define (identifier-prefix prefix identifier)
  (string->identifier identifier (%general-string-append prefix identifier)))

(define (identifier-suffix identifier suffix)
  (string->identifier identifier (%general-string-append identifier suffix)))

(define (syntax-maker-identifier type-identifier)
  (identifier-prefix "make-" type-identifier))

(define (syntax-predicate-identifier type-identifier)
  (identifier-suffix type-identifier "?"))

(define (syntax-accessor-identifier type-identifier field-identifier)
  (string->identifier type-identifier
		      (%general-string-append type-identifier "-" field-identifier)))

(define (syntax-mutator-identifier type-identifier field-identifier)
  (string->identifier type-identifier
		      (%general-string-append type-identifier "-" field-identifier "-set!")))

(define (syntax-dot-notation-identifier variable-identifier field-identifier)
  (string->identifier variable-identifier
		      (%general-string-append variable-identifier "." field-identifier)))


(define (validate-list-of-clauses clauses synner)
  ;;Scan the unwrapped  syntax object CLAUSES expecting a  list with the
  ;;format:
  ;;
  ;;    ((<keyword identifier> <thing> ...) ...)
  ;;
  ;;SYNNER must be a closure used to raise a syntax violation if a parse
  ;;error occurs; it must accept  two arguments: the message string, the
  ;;invalid subform.
  ;;
  (unless (or (null? clauses) (list? clauses))
    (synner "expected possibly empty list of clauses" clauses))
  (for-each
      (lambda (clause)
	(unless (list? clause)
	  (synner "expected list as clause" clause))
	(unless (identifier? (car clause))
	  (synner "expected identifier as first clause element" (car clause))))
    clauses))

(define (filter-clauses keyword-identifier clauses)
  ;;Given a list of clauses with the format:
  ;;
  ;;    ((<keyword identifier> <thing> ...) ...)
  ;;
  ;;look for  the ones having  KEYWORD-IDENTIFIER as car and  return the
  ;;selected  clauses in  a single  list; return  the empty  list  if no
  ;;matching clause is found.
  ;;
  (assert (identifier? keyword-identifier))
  (let next-clause ((clauses  clauses)
		    (selected '()))
    (if (null? clauses)
	(reverse selected) ;it is important to keep the order
	(next-clause (cdr clauses)
		     (if (free-identifier=? keyword-identifier (caar clauses))
			 (cons (car clauses) selected)
		       selected)))))


(define (validate-definition-clauses mandatory-keywords optional-keywords
				     at-most-once-keywords exclusive-keywords-sets
				     clauses synner)
  ;;Scan the unwrapped  syntax object CLAUSES expecting a  list with the
  ;;format:
  ;;
  ;;    ((<keyword identifier> <thing> ...) ...)
  ;;
  ;;then verify that the <keyword  identifier> syntax objects are in the
  ;;list of identifiers MANDATORY-KEYWORDS or in the list of identifiers
  ;;OPTIONAL-KEYWORDS; any order is allowed.
  ;;
  ;;Identifiers in  MANDATORY-KEYWORDS must appear at least  once in the
  ;;clauses;  identifiers in AT-MOST-ONCE-KEYWORDS  must appear  at most
  ;;once; identifiers  in OPTIONAL-KEYWORDS can appear  zero or multiple
  ;;times.
  ;;
  ;;EXCLUSIVE-KEYWORDS-SETS  is  a list  of  lists,  each sublist  holds
  ;;identifiers; the identifiers in each sublist are mutually exclusive:
  ;;at most one can appear in CLAUSES.
  ;;
  ;;SYNNER must  be the closure  used to raise  a syntax violation  if a
  ;;parse  error  occurs; it  must  accept  two  arguments: the  message
  ;;string, the invalid subform.
  ;;

  (define (%identifiers-join-for-message identifiers)
    ;;Given  a possibly  empty list  of  identifiers, join  them into  a
    ;;string with a  comma as separator; return the  string.  To be used
    ;;to build error messages involving the list of identifiers.
    ;;
    (let ((keys (map symbol->string (map syntax->datum identifiers))))
      (if (null? keys)
	  ""
	(call-with-values
	    (lambda ()
	      (open-string-output-port))
	  (lambda (port getter)
	    (display (syntax->datum (car keys)) port)
	    (let loop ((keys (cdr keys)))
	      (if (null? keys)
		  (getter)
		(begin
		  (display ", " port)
		  (display (syntax->datum (car keys)) port)
		  (loop (cdr keys))))))))))


  (validate-list-of-clauses clauses synner)

  ;;Check that the keyword of each clause is in MANDATORY-KEYWORDS or in
  ;;OPTIONAL-KEYWORDS.
  (for-each
      (lambda (clause)
	(let ((key (car clause)))
	  (unless (or (identifier-memq key mandatory-keywords)
		      (identifier-memq key optional-keywords))
	    (synner (string-append
		     "unrecognised clause keyword \""
		     (identifier->string (car clause))
		     "\", expected one among: "
		     (%identifiers-join-for-message (append mandatory-keywords optional-keywords)))
		    clause))))
    clauses)

  ;;Check the mandatory keywords.
  (for-each (lambda (mandatory-key)
	      (unless (find (lambda (clause)
			      (free-identifier=? mandatory-key (car clause)))
			    clauses)
		(synner (string-append "missing mandatory clause "
				       (symbol->string (syntax->datum mandatory-key)))
			mandatory-key)))
    mandatory-keywords)

  ;;Check the keywords which must appear at most once.
  (for-each
      (lambda (once-key)
	(let* ((err-clauses (filter (lambda (clause)
				      (free-identifier=? once-key (car clause)))
			      clauses))
	       (count (length err-clauses)))
	  (unless (or (zero? count) (= 1 count))
	    (synner (string-append "clause " (symbol->string (syntax->datum once-key))
				   " given multiple times")
		    err-clauses))))
    at-most-once-keywords)

  ;;Check mutually exclusive keywords.
  (for-each (lambda (mutually-exclusive-ids)
	      (let ((err (filter (lambda (clause) clause)
			   (map (lambda (e)
				  (exists (lambda (clause)
					    (and (free-identifier=? e (car clause))
						 clause))
					  clauses))
			     mutually-exclusive-ids))))
		(when (< 1 (length err))
		  (synner "mutually exclusive clauses" err))))
    exclusive-keywords-sets))


;;;; done

)

;;; end of file