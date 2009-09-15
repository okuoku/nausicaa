;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: lexical token record
;;;Date: Tue Jul 21, 2009
;;;
;;;Abstract
;;;
;;;	The  LEXICAL-TOKEN record  type describes  tokens produced  by a
;;;	lexer and consumed  by a parser.  It is meant to  be used by all
;;;	the parser libraries distributed with Nausicaa.
;;;
;;;Copyright (c) 2009 Marco Maggi
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
(library (parser-tools lexical-token)
  (export
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
  (import (rnrs))


(define-record-type <lexical-token>
  (fields (immutable category)
	  (immutable location)
	  (immutable value)
	  (immutable length))
  (nongenerative nausicaa:parser-tools:lexical-token))

(define (make-<lexical-token>/end-of-input location)
  (make-<lexical-token> '*eoi* location (eof-object) 0))

(define (make-<lexical-token>/lexer-error location value length)
  (make-<lexical-token> '*lexer-error* location (eof-object) 0))

(define (<lexical-token>?/end-of-input obj)
  (and (<lexical-token>? obj)
       (eq? '*eoi* (<lexical-token>-category obj))))

(define (<lexical-token>?/lexer-error obj)
  (and (<lexical-token>? obj)
       (eq? '*lexer-error* (<lexical-token>-category obj))))

(define (<lexical-token>?/special obj)
  (and (<lexical-token>? obj)
       (memq (<lexical-token>-category obj) '(*eoi* *lexer-error*))))


;;;; done

)

;;; end of file
