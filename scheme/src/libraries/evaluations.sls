;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: handling multiple code evaluations
;;;Date: Thu Dec 16, 2010
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
(library (evaluations)
  (export
    <environment>

    ;; auxiliary syntaxes
    bindings:
    imports:
    internal-prefix:)
  (import (nausicaa)
    (makers)
    (sentinel)
    (prefix (type-utilities) type.)
    (rnrs eval))


;;;; helpers

(define-auxiliary-syntaxes
  bindings:
  imports:
  internal-prefix:)

(type.define-type-assertion result-identifiers
  (type.predicate %result-identifiers?)
  (type.type-description "symbol")
  (type.value-description "binding name"))

(define (%result-identifiers? ell)
  (or (null? ell)
      (and (list? ell)
	   (for-all symbol? ell))))


(define-class <environment>
  (nongenerative nausicaa:evaluations:<environment>)
  (fields (immutable imports)
	  (immutable bindings)
	  (immutable environ)
	  (immutable lambda)
	  (immutable let)
	  (immutable define)
	  (immutable call-with-values)
	  (immutable values)
	  (immutable list))
  (protocol (lambda (make-top)
	      (lambda (imports bindings internal-prefix)
		(let* ((internal-bindings '(lambda let define call-with-values values list))
		       (imports		  `((prefix (only (rnrs) ,@internal-bindings) ,internal-prefix)
					    ,@imports)))
		  (apply (make-top)
			 imports
			 bindings
			 (apply environment imports)
			 (map (lambda (sym)
				(string->symbol (string-append (symbol->string internal-prefix)
							       (symbol->string sym))))
			   internal-bindings))))))
  (methods eval)
  (maker ()
	 (imports:		'((rnrs)))
	 (bindings:		'())
	 (internal-prefix:	'internals.)))

(define <environment>-eval
  (case-lambda
   ((o expr)
    (<environment>-eval o expr '()))
   (((o <environment>) expr result-identifiers)
    (assert-result-identifiers '<evaluation>-eval result-identifiers)
    (let* ((defs (map (lambda ((b <pair>))
			`(,o.define ,b.car (quote ,b.cdr)))
		   o.bindings))
	   (expr `(,o.let ()
			  ,@defs
			  (,o.call-with-values
			   (,o.lambda () ,expr)
			   (,o.lambda results (,o.values results (,o.list ,@result-identifiers)))))))
      (pretty-print expr)
      (receive (results binding-values)
	  (eval expr o.environ)
	(apply values
	       (map cons
		 result-identifiers
		 binding-values)
	       results))))))


;;;; done

)

;;; end of file
