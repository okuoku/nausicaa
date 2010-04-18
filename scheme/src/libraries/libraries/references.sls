;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: handling of library references
;;;Date: Fri Apr 16, 2010
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


(library (libraries references)
  (export

    <library-reference>		<library-reference>?
    make-<library-reference>
    <library-reference>-identifiers
    <library-reference>-version-reference

    library-reference-decompose
    library-reference?
    library-version-reference?
    library-reference->identifiers
    library-reference->version-reference

    conforming-sub-version-and-sub-version-reference?
    conforming-version-and-version-reference?
    conforming-library-name-and-library-reference?
    )
  (import (nausicaa)
    (matches)
    (libraries names))

  (define-syntax %normalise-to-boolean
    (syntax-rules ()
      ((_ ?expr)
       (if ?expr #t #f))))


(define-class <library-reference>
  (fields (immutable identifiers)
	  (immutable version-reference))

  (method (conforming? (ref <library-reference>) (name <library-name>))
    (and (for-all eq? name.identifiers ref.identifiers)
	 (conforming-version-and-version-reference? name.version ref.version-reference)))

  (protocol (lambda (make-<top>)
	      (lambda (sexp)
		(receive (identifiers version-reference)
		    (library-reference-decompose sexp)
		  (if identifiers
		      ((make-<top>) identifiers version-reference)
		    (assertion-violation 'make-<library-reference>
		      "invalid library reference symbolic expression" sexp))))))

  (nongenerative nausicaa:libraries:<library-reference>))


(define (library-reference-decompose obj)
  ;;Scan  OBJ validating  it as  a <library  reference> as  specified by
  ;;R6RS.   Return two  values:  the list  of  identifiers, the  version
  ;;reference.  The version can be null.  If OBJ is not a valid <library
  ;;reference>: return #f and #f.
  ;;
  (if (or (null? obj) (not (list? obj)))
      (values #f #f)
    (let next-identifier ((next (car obj))
			  (rest (cdr obj))
			  (ids  '()))
      (cond ((symbol? next) ;identifier
	     (if (null? rest)
		 (values obj '()) ; == (values (reverse (cons next ids)) '())
	       (next-identifier (car rest) (cdr rest) (cons next ids))))
	    ((and (list? next) (null? rest)) ;version spec
	     (if (library-version-reference? next)
		 (values (reverse ids) next)
	       (values #f #f)))
	    (else
	     (values #f #f))))))

(define (library-reference? sexp)
  (receive (identifiers version)
      (library-reference-decompose sexp)
    (if identifiers #t #f)))

(define (library-reference->identifiers sexp)
  (receive (identifiers version)
      (library-reference-decompose sexp)
    identifiers))

(define (library-reference->version-reference sexp)
  (receive (identifiers version)
      (library-reference-decompose sexp)
    version))

(define (library-version-reference? obj)
  (define (main)
    (match obj
      (('and ?sub-version0 ?sub-version ...)
       (for-all %match-sub-version (cons ?sub-version0 ?sub-version)))
      (('or  ?sub-version0 ?sub-version ...)
       (for-all %match-sub-version (cons ?sub-version0 ?sub-version)))
      (('not ?sub-version)
       (%match-sub-version ?sub-version))
      (?sub-version
       (for-all %match-sub-version ?sub-version))))
  (define (%match-sub-version sub-version)
    (match sub-version
      (('and ?sub-version0 ?sub-version ...)
       (for-all %match-sub-version (cons ?sub-version0 ?sub-version)))
      (('or  ?sub-version0 ?sub-version ...)
       (for-all %match-sub-version (cons ?sub-version0 ?sub-version)))
      (('not ?sub-version)
       (%match-sub-version ?sub-version))
      (('<= ?sub-version)
       (%match-sub-version ?sub-version))
      (('>= ?sub-version)
       (%match-sub-version ?sub-version))
      ((:and (:predicate integer?) (:predicate exact?))
       #t)
      (*
       #f)))
  (main))


(define (conforming-sub-version-and-sub-version-reference? sub-version sub-version-reference)
  (define who 'conforming-sub-version-and-sub-version-reference?)
  (define (%error-invalid-sub-version-reference)
    (assertion-violation who
      "invalid library sub-version reference" sub-version-reference))
  (unless (and (integer?  sub-version)
	       (exact?    sub-version)
	       (or (zero? sub-version)
		   (positive? sub-version)))
    (assertion-violation who
      "invalid library sub-version number" sub-version))
  (%normalise-to-boolean
   (cond ((list? sub-version-reference)
	  (when (zero? (length (cdr sub-version-reference)))
	    (%error-invalid-sub-version-reference))
	  (case (car sub-version-reference)
	    ((>=)
	     (>= sub-version (cadr sub-version-reference)))
	    ((<=)
	     (<= sub-version (cadr sub-version-reference)))
	    ((and)
	     (for-all (lambda (sub-version-reference)
			(conforming-sub-version-and-sub-version-reference? sub-version
									   sub-version-reference))
		      (cdr sub-version-reference)))
	    ((or)
	     (find (lambda (sub-version-reference)
		     (conforming-sub-version-and-sub-version-reference? sub-version
									sub-version-reference))
		   (cdr sub-version-reference)))

	    ((not)
	     (if (= 1 (length (cdr sub-version-reference)))
		 (not (conforming-sub-version-and-sub-version-reference? sub-version
									 (cadr sub-version-reference)))
	       (%error-invalid-sub-version-reference)))

	    (else
	     (%error-invalid-sub-version-reference))))
	 ((and (integer?  sub-version-reference)
	       (exact?    sub-version-reference)
	       (or (zero? sub-version-reference)
		   (positive? sub-version-reference)))
	  (= sub-version sub-version-reference))
	 (else
	  (%error-invalid-sub-version-reference)))))


(define (conforming-version-and-version-reference? version version-reference)
  (define who 'conforming-version-and-version-reference?)
  (define (%error-invalid-version-reference)
    (assertion-violation who
      "invalid library version reference" version-reference))
  (or (and (null? version) (null? version-reference))
      (null? version-reference)
      (%normalise-to-boolean
       (case (car version-reference)
	 ((and)
	  (for-all (lambda (reference)
		     (conforming-version-and-version-reference? version reference))
		   (cdr version-reference)))
	 ((or)
	  (find (lambda (reference)
		  (conforming-version-and-version-reference? version reference))
		(cdr version-reference)))
	 ((not)
	  (if (= 2 (length version-reference))
	      (not (conforming-version-and-version-reference? version (cadr version-reference)))
	    (%error-invalid-version-reference)))
	 (else
	  (let next-sub-version ((version		version)
				 (version-reference	version-reference))
	    (cond ((null? version-reference)
		   (for-all zero? version))
		  ((null? version)
		   (null? version-reference))
		  ((conforming-sub-version-and-sub-version-reference? (car version)
								      (car version-reference))
		   (next-sub-version (cdr version) (cdr version-reference)))
		  (else
		   #f))))))))


(define (conforming-library-name-and-library-reference? name reference)
  (let-values (((identifiers version)		(library-name-decompose name))
	       ((identifiers-ref version-ref)	(library-reference-decompose reference)))
    (and (for-all eq? identifiers identifiers-ref)
	 (conforming-version-and-version-reference? version version-ref))))


;;;; done

)

;;; end of file
