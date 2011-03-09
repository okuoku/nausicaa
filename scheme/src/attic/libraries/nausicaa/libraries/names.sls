;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: R6RS library names handling
;;;Date: Thu Apr 15, 2010
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


#!r6rs
(library (nausicaa libraries names)
  (export
    <library-name>		<library-name>?
    make-<library-name>
    <library-name>-identifiers
    <library-name>-version

    library-name?
    library-name-decompose
    library-name->identifiers
    library-name->version

    library-name-identifiers=?
    library-name=?
    library-name<?
    library-name<=?

    library-version=?
    library-version<?
    library-version<=?
    )
  (import (nausicaa))


(define-class <library-name>
  (fields (immutable identifiers)
	  (immutable version))
  (protocol (lambda (make-<top>)
	      (lambda (sexp)
		(receive (identifiers version)
		    (library-name-decompose sexp)
		  (if identifiers
		      ((make-<top>) identifiers version)
		    (assertion-violation 'make-<library-name>
		      "invalid library name symbolic expression" sexp))))))

  (method (identifiers= (a <library-name>) (b <library-name>))
    (for-all eq? a.identifiers b.identifiers))

  (method (= (a <library-name>) (b <library-name>))
    (and (for-all eq? a.identifiers b.identifiers)
	 (library-version=? a.version b.version)))

  (method (< (a <library-name>) (b <library-name>))
    (and (for-all eq? a.identifiers b.identifiers)
	 (library-version<? a.version b.version)))

  (method (<= (a <library-name>) (b <library-name>))
    (and (for-all eq? a.identifiers b.identifiers)
	 (library-version<=? a.version b.version)))

  (method (> (a <library-name>) (b <library-name>))
    (and (for-all eq? a.identifiers b.identifiers)
	 (library-version<? b.version a.version)))

  (method (>= (a <library-name>) (b <library-name>))
    (and (for-all eq? a.identifiers b.identifiers)
	 (library-version<=? b.version a.version)))

  (nongenerative nausicaa:libraries:<library-name>))


(define (library-name-decompose obj)
  ;;Scan OBJ  validating it  as a <library  name> as specified  by R6RS.
  ;;Return   two  values:   the   list  of   identifiers,  the   version
  ;;specification.   The version can  be null.   If OBJ  is not  a valid
  ;;<library name>: return #f and #f.
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
	     (if (or (null? next)
		     (and (for-all integer?      next)
			  (for-all exact?        next)
			  (for-all non-negative? next)))
		 (values (reverse ids) next)
	       (values #f #f)))
	    (else
	     (values #f #f))))))

(define (library-name? sexp)
  (receive (identifiers version)
      (library-name-decompose sexp)
    (if identifiers #t #f)))

(define (library-name->identifiers sexp)
  (receive (identifiers version)
      (library-name-decompose sexp)
    identifiers))

(define (library-name->version sexp)
  (receive (identifiers version)
      (library-name-decompose sexp)
    version))


(define (library-name-identifiers=? sexp1 sexp2)
  (for-all eq?
	   (library-name->identifiers sexp1)
	   (library-name->identifiers sexp2)))

(define (%library-name-comparison version-predicate sexp1 sexp2)
  (let-values (((ids1 vrs1) (library-name-decompose sexp1))
	       ((ids2 vrs2) (library-name-decompose sexp2)))
    (and (for-all eq? ids1 ids2)
	 (version-predicate vrs1 vrs2))))

(define (library-name=? sexp1 sexp2)
  (%library-name-comparison library-version=? sexp1 sexp2))

(define (library-name<? sexp1 sexp2)
  (%library-name-comparison library-version<? sexp1 sexp2))

(define (library-name<=? sexp1 sexp2)
  (%library-name-comparison library-version<=? sexp1 sexp2))


(define (library-version=? vrs1 vrs2)
  (let next ((vrs1 vrs1)
	     (vrs2 vrs2))
    (cond ((null? vrs1)
	   (or (null? vrs2) (for-all zero? vrs2)))
	  ((null? vrs2)
	   (for-all zero? vrs1)) ;it cannot be (null? vrs1) here
	  (else
	   (and (= (car vrs1) (car vrs2))
		(next (cdr vrs1) (cdr vrs2)))))))

(define (library-version<? vrs1 vrs2)
  (let next ((vrs1 vrs1)
	     (vrs2 vrs2))
    (cond ((null? vrs1)
	   (cond ((null? vrs2)
		  #f)
		 ((find positive? vrs2)
		  #t)
		 (else
		  #f)))
	  ((null? vrs2)
	   #f)
	  ((< (car vrs1) (car vrs2))
	   #t)
	  (else
	   (and (= (car vrs1) (car vrs2))
		(next (cdr vrs1) (cdr vrs2)))))))

(define (library-version<=? vrs1 vrs2)
  (let next ((vrs1 vrs1)
	     (vrs2 vrs2))
    (cond ((null? vrs1)
	   #t)
	  ((null? vrs2)
	   (for-all zero? vrs1))
	  (else
	   (and (<= (car vrs1) (car vrs2))
		(next (cdr vrs1) (cdr vrs2)))))))


;;;; done

)

;;; end of file
