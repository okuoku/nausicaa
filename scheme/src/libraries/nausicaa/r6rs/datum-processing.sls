;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: parameters and functions to process Scheme data
;;;Date: Fri Jan  7, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa r6rs datum-processing)
  (export
    <commented-datum>			<interlexeme-space>
    remove-interlexeme-space

    identifier-datum-maker		make-identifier-datum
    boolean-datum-maker			make-boolean-datum
    number-datum-maker			make-number-datum
    string-datum-maker			make-string-datum
    character-datum-maker		make-character-datum
    pair-datum-maker			make-pair-datum
    list-datum-maker			make-list-datum
    vector-datum-maker			make-vector-datum
    bytevector-datum-maker		make-bytevector-datum

    quoted-datum-maker			make-quoted-datum
    quasiquoted-datum-maker		make-quasiquoted-datum
    unquoted-datum-maker		make-unquoted-datum
    unquoted-splicing-datum-maker	make-unquoted-splicing-datum
    syntax-datum-maker			make-syntax-datum
    quasisyntax-datum-maker		make-quasisyntax-datum
    unsyntax-datum-maker		make-unsyntax-datum
    unsyntax-splicing-datum-maker	make-unsyntax-splicing-datum

    interlexeme-space-datum-maker	make-interlexeme-space-datum
    whitespace-datum-maker		make-whitespace-datum
    line-comment-datum-maker		make-line-comment-datum
    nested-comment-datum-maker		make-nested-comment-datum
    sharp-semicolon-datum-maker		make-sharp-semicolon-datum
    sharp-bang-datum-maker		make-sharp-bang-datum
    sharp-bang-r6rs-datum-maker		make-sharp-bang-r6rs-datum)
  (import (nausicaa)
    (nausicaa language sentinel)
    (nausicaa parser-tools lexical-token)
    (nausicaa parser-tools source-location)
    (prefix (nausicaa r6rs fixed-strings) string.))


(define-class <commented-datum>
  (nongenerative nausicaa:r6rs:<commented-datum>)
  (fields
   ;;false or <interlexeme-space> instance
   (immutable interlexeme-space)
   ;;any datum
   (immutable datum)))

(define-class <interlexeme-space>
  (nongenerative nausicaa:r6rs:<interlexeme-space>)
  (fields (immutable atmospheres)))

(define (remove-interlexeme-space sexp)
  (cond ((pair? sexp)
	 (cond ((is-a? (car sexp) <interlexeme-space>)
		(remove-interlexeme-space (cdr sexp)))
	       (else
		(cons (remove-interlexeme-space (car sexp))
		      (remove-interlexeme-space (cdr sexp))))))
	((vector? sexp)
	 (list->vector (remove-interlexeme-space (vector->list sexp))))
	;;This should  happen only when the  sentinel is the  tail of an
	;;improper list.
	((is-a? sexp <interlexeme-space>)
	 '())
	(else
	 sexp)))


;;;; datum makers

(define (make-identifier-datum yypushback yycustom the-identifier-string)
  (string->symbol the-identifier-string))

(define (make-boolean-datum yypushback yycustom the-boolean)
  the-boolean)

(define (make-number-datum yypushback yycustom the-number)
  the-number)

(define (make-string-datum yypushback yycustom the-string)
  the-string)

(define (make-character-datum yypushback yycustom the-character)
  the-character)

(define (make-pair-datum yypushback yycustom the-car the-cdr)
  (cons the-car the-cdr))

(define (make-list-datum yypushback yycustom the-list-of-items)
  the-list-of-items)

(define (make-vector-datum yypushback yycustom the-list-of-items)
  (list->vector the-list-of-items))

(define (make-bytevector-datum yypushback yycustom the-list-of-u8-integers)
  (let ((wrong (exists (lambda (n)
			 (if (and (integer? n)
				  (exact?   n)
				  (<= 0 n 255))
			     #f
			   n))
		 the-list-of-u8-integers)))
    (if wrong
	(raise
	 (condition (make-lexical-violation)
		    (make-who-condition 'make-bytevector-datum)
		    (make-message-condition "invalid value in bytevector")
		    (make-irritants-condition `(,wrong))))
      (u8-list->bytevector the-list-of-u8-integers))))

;;; --------------------------------------------------------------------

(define (make-quoted-datum yypushback yycustom datum)
  (list 'quote datum))

(define (make-quasiquoted-datum yypushback yycustom datum)
  (list 'quasiquote datum))

(define (make-unquoted-datum yypushback yycustom datum)
  (list 'unquote datum))

(define (make-unquoted-splicing-datum yypushback yycustom datum)
  (list 'unquote-splicing datum))

;;; --------------------------------------------------------------------

(define (make-syntax-datum yypushback yycustom datum)
  (list 'syntax datum))

(define (make-quasisyntax-datum yypushback yycustom datum)
  (list 'quasisyntax datum))

(define (make-unsyntax-datum yypushback yycustom datum)
  (list 'unsyntax datum))

(define (make-unsyntax-splicing-datum yypushback yycustom datum)
  (list 'unsyntax-splicing datum))

;;; --------------------------------------------------------------------

(define (make-interlexeme-space-datum yypushback yycustom the-list-of-atmospheres)
  (make* <interlexeme-space>
    the-list-of-atmospheres))

(define (make-whitespace-datum yypushback yycustom the-whitespace-string)
  the-whitespace-string)

(define (make-line-comment-datum yypushback yycustom the-comment-string)
  the-comment-string)

(define (make-nested-comment-datum yypushback yycustom the-comment-string)
  the-comment-string)

(define (make-sharp-semicolon-datum yypushback yycustom the-interlexeme-space the-datum)
  (make* <commented-datum>
    the-interlexeme-space
    the-datum))

(define (make-sharp-bang-datum yypushback yycustom the-sharp-bang-string)
  the-sharp-bang-string)

(define (make-sharp-bang-r6rs-datum yypushback yycustom the-sharp-bang-r6rs-string)
  the-sharp-bang-r6rs-string)


;;;; parameters

(define-syntax define-datum-processor-parameter
  (syntax-rules ()
    ((_ ?param ?default)
     (define ?param
       (make-parameter ?default
	 (lambda (obj)
	   (if (procedure? obj)
	       obj
	     (assertion-violation '?param
	       "expected procedure as parameter value" obj))))))))

(define-datum-processor-parameter identifier-datum-maker	make-identifier-datum)
(define-datum-processor-parameter boolean-datum-maker		make-boolean-datum)
(define-datum-processor-parameter number-datum-maker		make-number-datum)
(define-datum-processor-parameter string-datum-maker		make-string-datum)
(define-datum-processor-parameter character-datum-maker		make-character-datum)
(define-datum-processor-parameter pair-datum-maker		make-pair-datum)
(define-datum-processor-parameter list-datum-maker		make-list-datum)
(define-datum-processor-parameter vector-datum-maker		make-vector-datum)
(define-datum-processor-parameter bytevector-datum-maker	make-bytevector-datum)

(define-datum-processor-parameter quoted-datum-maker		make-quoted-datum)
(define-datum-processor-parameter quasiquoted-datum-maker	make-quasiquoted-datum)
(define-datum-processor-parameter unquoted-datum-maker		make-unquoted-datum)
(define-datum-processor-parameter unquoted-splicing-datum-maker	make-unquoted-splicing-datum)
(define-datum-processor-parameter syntax-datum-maker		make-syntax-datum)
(define-datum-processor-parameter quasisyntax-datum-maker	make-quasisyntax-datum)
(define-datum-processor-parameter unsyntax-datum-maker		make-unsyntax-datum)
(define-datum-processor-parameter unsyntax-splicing-datum-maker	make-unsyntax-splicing-datum)

(define-datum-processor-parameter interlexeme-space-datum-maker	make-interlexeme-space-datum)
(define-datum-processor-parameter whitespace-datum-maker	make-whitespace-datum)
(define-datum-processor-parameter line-comment-datum-maker	make-line-comment-datum)
(define-datum-processor-parameter nested-comment-datum-maker	make-nested-comment-datum)
(define-datum-processor-parameter sharp-semicolon-datum-maker	make-sharp-semicolon-datum)
(define-datum-processor-parameter sharp-bang-datum-maker	make-sharp-bang-datum)
(define-datum-processor-parameter sharp-bang-r6rs-datum-maker	make-sharp-bang-r6rs-datum)


;;;; done

)

;;; end of file
