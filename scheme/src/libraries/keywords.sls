;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: keywords implementation
;;;Date: Sun Jul  5, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
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


(library (keywords)
  (export make-keyword %make-keyword
	  keyword?
	  keyword->symbol keyword->string
	  (rename (%make-keyword symbol->keyword))
	  string->keyword
	  with-keywords)
  (import (rnrs))

  (define keyword-protocol
    ;;Store new keywords in a table, so that keywords with the same name
    ;;will be EQ? to each other.
    (let ((table (make-eq-hashtable)))
      (lambda (constructor)
	(lambda (name)
	  (unless (symbol? name)
	    (assertion-violation 'make-keyword
	      "expected symbol as keyword name" name))
	  (or (hashtable-ref table name #f)
	      (let ((r (constructor name)))
		(hashtable-set! table name r)
		r))))))

  (define (keyword->string key)
    (if (keyword? key)
	(symbol->string (keyword->symbol key))
      (assertion-violation 'keyword->string
	"expected keyword as argument" key)))

  (define (string->keyword str)
    (if (string? str)
	(%make-keyword (string->symbol str))
      (assertion-violation 'string->keyword
	"expected string as argument" str)))

  (define-syntax with-keywords
    (syntax-rules ()
      ((_ (?key0 ?key ...) ?form0 ?form ...)
       (let ((?key0 (make-keyword ?key0))
	     (?key  (make-keyword ?key))
	     ...)
	 ?form0 ?form ...))))

  (define-syntax make-keyword
    (syntax-rules ()
      ((_ ?sym)
       (%make-keyword (quote ?sym)))))

  (define-record-type (keyword %make-keyword keyword?)
    (fields (immutable name keyword->symbol))
    (protocol keyword-protocol)
    (sealed #t)
    (opaque #t)
    (nongenerative keyword)))

;;; end of file
