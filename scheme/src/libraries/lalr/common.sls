;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: LALR(1) parser generator
;;;Date: Tue Jul 21, 2009
;;;
;;;Abstract
;;;
;;;	This library  is a LALR(1)  parser generator written  in Scheme.
;;;	It implements an efficient algorithm for computing the lookahead
;;;	sets.  The  algorithm is the  same as used  in GNU Bison  and is
;;;	described in:
;;;
;;;	   F.  DeRemer  and  T.  Pennello.  ``Efficient  Computation  of
;;;	   LALR(1)  Look-Ahead Set''.   TOPLAS, vol.  4, no.  4, october
;;;	   1982.
;;;
;;;	As a consequence, it is not written in a fully functional style.
;;;	In fact,  much of  the code  is a direct  translation from  C to
;;;	Scheme of the Bison sources.
;;;
;;;	The library is  a port to @rnrs{6} Scheme of  Lalr-scm by .  The
;;;	original code is available at:
;;;
;;;			<http://code.google.com/p/lalr-scm/>
;;;
;;;Copyright (c) 2005-2008 Dominique Boucher
;;;Port to R6RS and Nausicaa integration by Marco Maggi.
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
(library (lalr common)
  (export
    make-source-location	source-location?
    source-location-line
    source-location-input
    source-location-column
    source-location-offset
    source-location-length

    make-lexical-token		lexical-token?
    lexical-token-value
    lexical-token-category
    lexical-token-source

    lexical-token?/end-of-input
    lexical-token?/lexer-error
    lexical-token?/special)
  (import (rnrs))



(define-record-type (lexical-token make-lexical-token lexical-token?)
  (fields (immutable category lexical-token-category)
	  (immutable source   lexical-token-source)
	  (immutable value    lexical-token-value)))

(define-record-type (source-location make-source-location source-location?)
  (fields (immutable input   source-location-input)
	  (immutable line    source-location-line)
	  (immutable column  source-location-column)
	  (immutable offset  source-location-offset)
	  (immutable length  source-location-length)))

(define (lexical-token?/end-of-input obj)
  (and (lexical-token? obj)
       (eq? '*eoi* (lexical-token-category obj))))

(define (lexical-token?/lexer-error obj)
  (and (lexical-token? obj)
       (eq? '*lexer-error* (lexical-token-category obj))))

(define (lexical-token?/special obj)
  (and (lexical-token? obj)
       (memq (lexical-token-category obj) '(*eoi* *lexer-error*))))


;;;; done

)

;;; end of file
