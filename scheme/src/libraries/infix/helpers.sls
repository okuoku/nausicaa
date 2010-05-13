;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: helper definitions for infix->prefix library
;;;Date: Thu May 13, 2010
;;;
;;;Abstract
;;;
;;;	Aaron Hsu  contributed the SYNTAX->LIST function  through a post
;;;	on comp.lang.scheme.
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


(library (infix helpers)
  (export syntax->list
	  eoi-token ell-lparen-token ell-rparen-token
	  error-handler)
  (import (rnrs)
    (parser-tools lexical-token)
    (parser-tools source-location))


(define eoi-token
  (make-<lexical-token> '*eoi* #f (eof-object) 0))

(define ell-lparen-token
  (list (make-<lexical-token> 'LPAREN #f #\( 0)))

(define ell-rparen-token
  (list (make-<lexical-token> 'RPAREN #f #\) 0)))

(define (error-handler message token)
  (error #f
    (if (or (not (<lexical-token>? token))
	    (not (<lexical-token>-location token)))
	message
      (let* ((position	(<lexical-token>-location token))
	     (line	(<source-location>-line position))
	     (column	(<source-location>-column position)))
	(string-append
	 message
	 " line " (if line (number->string line) "unknown")
	 " column " (if column (number->string column) "unknown"))))
    token))

(define (syntax->list stx)
  ;;Given a syntax object STX holding  a list, decompose it and return a
  ;;list of syntax  objects.  Take care of returning  a proper list when
  ;;the input is a syntax object holding a proper list.
  ;;
  ;;This functions  provides a workaround  for bugs in Ikarus  and Mosh,
  ;;which expand syntax objects holding a list into IMproper lists.
  ;;
  (syntax-case stx ()
    (()		'())
    ((?car . ?cdr)	(cons (syntax->list #'?car) (syntax->list #'?cdr)))
    (?atom		(identifier? #'?atom)	#'?atom)
    (?atom		(syntax->datum #'?atom))))


;;;; done

)

;;; end of file
