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
    (gensym)
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
	  (immutable elet))
  (protocol (lambda (make-top)
	      (lambda (imports bindings)
		(let* (($elet	(gensym))
		       (imports `((rename (evaluations wrapper) (elet ,$elet))
				  ,@imports)))
		  ((make-top) imports bindings (apply environment imports) $elet)))))
  (maker ()
	 (imports:	'((rnrs)))
	 (bindings:	'()))
  (methods eval eval-for-bindings augment))

(define (<environment>-eval (o <environment>) expr)
;;;(pretty-print `(,o.elet ,o.bindings ,expr))
  (eval `(,o.elet () ,o.bindings ,expr) o.environ))

(define (<environment>-eval-for-bindings (o <environment>) expr result-identifiers)
  (assert-result-identifiers '<evaluation>-eval result-identifiers)
;;;(pretty-print `(,o.elet ,o.bindings ,expr))
  (let ((result-datums (eval `(,o.elet ,result-identifiers ,o.bindings ,expr) o.environ)))
    (map cons result-identifiers result-datums)))

(define (<environment>-augment (o <environment>) new-bindings)
  (make-from-fields <environment>
    o.imports (append new-bindings o.bindings) o.environ o.elet))


;;;; done

)

;;; end of file
