;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: keywords implementation
;;;Date: Sun Jul  5, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (export define-keywords keyword %keyword
	  keyword? keyword=?
	  keyword->symbol keyword->string
	  (rename (%keyword symbol->keyword))
	  string->keyword
	  with-keywords let-keywords let-keywords*)
  (import (rnrs))

  (define-record-type (<keyword> %keyword keyword?)
    (fields (immutable name keyword->symbol))
    (sealed #t)
    (opaque #t)
    (nongenerative keyword)
    (protocol (let ((table (make-eq-hashtable)))
		(lambda (constructor)
		  ;;Store new keywords in a table, so that keywords with
		  ;;the same name will be EQ? to each other.
		  (lambda (name)
		    (unless (symbol? name)
		      (assertion-violation 'keyword
			"expected symbol as keyword name" name))
		    (or (hashtable-ref table name #f)
			(let ((r (constructor name)))
			  (hashtable-set! table name r)
			  r)))))))

  (define (keyword=? a b)
    (or (eq? a b)
	(eq? (keyword->symbol a) (keyword->symbol b))))

  (define (keyword->string key)
    (if (keyword? key)
	(symbol->string (keyword->symbol key))
      (assertion-violation 'keyword->string
	"expected keyword as argument" key)))

  (define (string->keyword str)
    (if (string? str)
	(%keyword (string->symbol str))
      (assertion-violation 'string->keyword
	"expected string as argument" str)))

  (define-syntax with-keywords
    (syntax-rules ()
      ((_ (?key0 ?key ...) ?form0 ?form ...)
       (let ((?key0 (keyword ?key0))
	     (?key  (keyword ?key))
	     ...)
	 ?form0 ?form ...))))

  (define-syntax keyword
    (syntax-rules ()
      ((_ ?sym)
       (%keyword (quote ?sym)))))

  (define-syntax define-keywords
    (syntax-rules ()
      ((_ ?sym0 ?sym ...)
       (begin
	 (define ?sym0 (keyword ?sym0))
	 (define ?sym  (keyword ?sym))
	 ...))))

  (define (%parse-keywords options allow-unknown alist)
    (let loop ((options options))
      (unless (null? options)
	(let ((op (car options)))
	  (if (not (keyword? op))
	      (assertion-violation #f "expected keyword option" op)
	    (let ((p (assp (lambda (key)
			     (keyword=? op key))
			   alist)))
	      (cond (p
		     (if (null? (cdr options))
			 (assertion-violation #f "keyword option requires value" op)
		       ((cdr p) (cadr options))))
		    (allow-unknown #f)
		    (else
		     (assertion-violation #f "unrecognised option" (keyword->symbol op))))
	      (loop (cddr options))))))))

  (define-syntax let-keywords
    (syntax-rules ()
      ((_ ?options ?allow-unknown ((?name ?key ?default) ...) ?form0 ?form ...)
       (with-keywords (?key ...)
	 (let ((?name ?default) ...)
	   (%parse-keywords ?options ?allow-unknown
			    `((,?key . ,(lambda (v) (set! ?name v))) ...))
	   ?form0 ?form ...)))))

  (define-syntax let-keywords*
    (syntax-rules ()
      ((_ ?options ?allow-unknown ((?name ?key ?default) ...) ?form0 ?form ...)
       (with-keywords (?key ...)
	 (let* ((?name ?default) ...)
	   (%parse-keywords ?options ?allow-unknown
			    `((,?key . ,(lambda (v) (set! ?name v))) ...))
	   ?form0 ?form ...)))))


;;;; done

)

;;; end of file
