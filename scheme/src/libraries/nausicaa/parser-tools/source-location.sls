;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: source location record
;;;Date: Tue Jul 21, 2009
;;;
;;;Abstract
;;;
;;;	The  SOURCE-LOCATION record  type describes  the position  in an
;;;	input stream  of a token produced  by a lexer and  consumed by a
;;;	parser.   It is meant  to be  used by  all the  parser libraries
;;;	distributed with Nausicaa.
;;;
;;;Copyright (c) 2009, 2010, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (c) 2005-2008 Dominique Boucher
;;;
;;;Original  code  by Dominique  Boucher.   Port  to  R6RS and  Nausicaa
;;;integration by Marco Maggi.
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
(library (nausicaa parser-tools source-location)
  (export

    <source-location>
    make-<source-location>	make-<source-location>/start
    <source-location>?		<source-location>?/start
    <source-location>?/or-false	<source-location>?/start/or-false

    <source-location>-line
    <source-location>-input
    <source-location>-column
    <source-location>-offset

    source-location-update
    source-location->string

    source-location=?

    source-location-point=?
    source-location-point>?  source-location-point<?
    source-location-point>=? source-location-point<=?

    source-location-tab-function
    source-location-tab-function/8chars
    source-location-tab-function/tab-table
    source-location-tab-table

    source-location-honor-return)
  (import (nausicaa)
    (nausicaa generics object-to-string))


(define-class <source-location>
  (fields (immutable input)
	  (immutable line)
	  (immutable column)
	  (immutable offset))
  (virtual-fields (immutable line-string)
		  (immutable column-string)
		  (immutable offset-string))
  (nongenerative nausicaa:parser-tools:<source-location>))

(define-inline (make-<source-location>/start input-spec)
  (make-<source-location> input-spec 1 1 0))

(define (<source-location>?/or-false obj)
  (or (not obj)
      (<source-location>? obj)))

(define (<source-location>?/start obj)
  (and (<source-location>? obj)
       (= 1 (<source-location>-line   obj))
       (= 1 (<source-location>-column obj))))

(define (<source-location>?/start/or-false obj)
  (or (not obj)
      (<source-location>?/start obj)))

(define (<source-location>-line-string (o <source-location>))
  (number->string o.line))

(define (<source-location>-column-string (o <source-location>))
  (number->string o.column))

(define (<source-location>-offset-string (o <source-location>))
  (number->string o.offset))


(define (source-location=? a b)
  (cond ((not a) #f)
	((not b) #f)
	(else
	 (with-class ((a <source-location>)
		      (b <source-location>))
	   (and (= a.line b.line)
		(= a.column b.column)
		(= a.offset b.offset))))))

(define (source-location-point=? a b)
  (cond ((not a) #f)
	((not b) #f)
	(else
	 (with-class ((a <source-location>)
		      (b <source-location>))
	   (and (= a.line b.line)
		(= a.column b.column))))))

(define (source-location-point>? a b)
  (cond ((not a) #f)
	((not b) #t)
	(else
	 (with-class ((a <source-location>)
		      (b <source-location>))
	   (or (> a.line b.line)
	       (and (= a.line b.line)
		    (> a.column b.column)))))))

(define (source-location-point<? a b)
  (source-location-point>? b a))

(define (source-location-point>=? a b)
  (cond ((not a) #f)
	((not b) #t)
	(else
	 (with-class ((a <source-location>)
		      (b <source-location>))
	   (or (> a.line b.line)
	       (and (= a.line b.line)
		    (>= a.column b.column)))))))

(define (source-location-point<=? a b)
  (source-location-point>=? b a))


(define (source-location-update (location <source-location>) char/token-length)
  (and location
       (cond ((integer? char/token-length)
	      (make-<source-location> location.input location.line
				      (+ char/token-length location.column)
				      (+ char/token-length location.offset)))

	     ((char? char/token-length)
	      (let ((new-offset (+ 1 location.offset)))
		(case char/token-length
		  ((#\newline)
		   (make-<source-location> location.input (+ location.line 1) 1 new-offset))
		  ((#\return)
		   (make-<source-location> location.input location.line
					   (if (source-location-honor-return)
					       1
					     (+ 1 location.column))
					   new-offset))
		  ((#\tab)
		   (make-<source-location> location.input location.line
					   ((source-location-tab-function) location.column)
					   new-offset))
		  (else
		   (make-<source-location> location.input location.line
					   (+ 1 location.column) new-offset)))))
	     (else
	      (assertion-violation 'source-location-update
		"expected character or lexical token length"
		char/token-length)))))

(define (source-location-tab-function/8chars column)
  (+ 8 (- column (mod column 8))))

(define (source-location-tab-function/tab-table column)
  (let ((table (source-location-tab-table)))
    (let loop ((table table))
      (cond ((null? table)
	     (source-location-tab-function/8chars column))
	    ((< column (car table))
	     (car table))
	    (else
	     (loop (cdr table)))))))

(define source-location-tab-function
  (make-parameter source-location-tab-function/8chars
    (lambda (obj)
      (assert (procedure? obj))
      obj)))

(define source-location-tab-table
  (make-parameter '()
    (lambda (obj)
      (assert (list? obj))
      obj)))

(define source-location-honor-return
  (make-parameter #f))


(define (source-location->string (location <source-location>))
  (if location
      (string-append (object->string location.input)
		     ":"
		     (number->string location.line)
		     ":"
		     (number->string location.column))
    "<??>"))

(define-method (object->string (o <source-location>))
  (source-location->string o))


;;;; done

)

;;; end of file
