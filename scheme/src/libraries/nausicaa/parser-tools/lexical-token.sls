;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: lexical token record
;;;Date: Tue Jul 21, 2009
;;;
;;;Abstract
;;;
;;;	The <LEXICAL-TOKEN>  record type describes tokens  produced by a
;;;	lexer and consumed  by a parser.  It is meant to  be used by all
;;;	the parser libraries distributed with Nausicaa.
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
(library (nausicaa parser-tools lexical-token)
  (export
    <lexical-token>

    ;; auxiliary syntaxes
    category:
    location:
    value:
    length:

    ;; traditional records API
    make-<lexical-token>
    make-<lexical-token>/end-of-input
    make-<lexical-token>/lexer-error

    <lexical-token>?

    <lexical-token>-value
    <lexical-token>-category
    <lexical-token>-location
    <lexical-token>-length

    <lexical-token>?/end-of-input
    <lexical-token>?/lexer-error
    <lexical-token>?/special)
  (import (rnrs)
    (nausicaa parser-tools source-location)
    (nausicaa language classes)
    (nausicaa language makers)
    (only (nausicaa language syntax-utilities) define-auxiliary-syntaxes))


(define-auxiliary-syntaxes
  category:
  location:
  value:
  length:)

(define-class <lexical-token>
  (nongenerative nausicaa:parser-tools:<lexical-token>)
  (maker ()
	 (category:	#f (mandatory))
	 (location:	#f)
	 (value:	#f)
	 (length:	0))
  (fields (immutable category)
	  (immutable (location <source-location>))
	  (immutable value)
	  (immutable length))
  (virtual-fields (immutable end-of-input?	<lexical-token>?/end-of-input)
		  (immutable lexer-error?	<lexical-token>?/lexer-error)
		  (immutable special?		<lexical-token>?/special)))

(define (make-<lexical-token>/end-of-input location)
  (make-<lexical-token> '*eoi* location (eof-object) 0))

(define (make-<lexical-token>/lexer-error location value length)
  (make-<lexical-token> '*lexer-error* location (eof-object) 0))

(define (<lexical-token>?/end-of-input obj)
  (and (<lexical-token>? obj)
       (eq? '*eoi* (<lexical-token>-category obj))
       #t))

(define (<lexical-token>?/lexer-error obj)
  (and (<lexical-token>? obj)
       (eq? '*lexer-error* (<lexical-token>-category obj))
       #t))

(define (<lexical-token>?/special obj)
  (and (<lexical-token>? obj)
       (memq (<lexical-token>-category obj) '(*eoi* *lexer-error*))
       #t))


;;;; done

)

;;; end of file
