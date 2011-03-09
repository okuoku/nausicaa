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
;;;Copyright (c) 2010, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa evaluations)
  (export
    <environment>

    ;; auxiliary syntaxes
    bindings:
    imports:)
  (import (nausicaa)
    (nausicaa language makers)
    (nausicaa language sentinel)
    (nausicaa language gensym)
    (prefix (nausicaa type-utilities) type.)
    (rnrs eval)
    (rnrs mutable-pairs))


;;;; helpers

(define-auxiliary-syntaxes
  bindings:
  imports:)

(type.define-type-assertion result-identifiers
  (type.predicate %result-identifiers?)
  (type.type-description "symbol")
  (type.value-description "binding name"))

(define (%result-identifiers? ell)
  (or (null? ell)
      (and (list? ell)
	   (for-all symbol? ell))))

(define-syntax last-pair/stx
  (syntax-rules ()
    ((_ ?x)
     (let loop ((x ?x))
       (let ((d (cdr x)))
	 (if (pair? d)
	     (loop d)
	   x))))))

(define-syntax tail-set!/stx
  (syntax-rules ()
    ((_ ?ell ?tail)
     (let* ((ell ?ell)
	    (lp  (last-pair/stx ell)))
       (set-cdr! lp ?tail)))))

(define (%duplicates-alist-alist? a b)
  (let loop (((a <list>) a))
    (cond (a.null?
	   #f)
	  ((assq a.caar b)
	   #t)
	  (else
	   (loop a.cdr)))))

(define (%duplicates-symbols-alist? a b)
  (let loop (((a <list>) a))
    (cond (a.null?
	   #f)
	  ((assq a.car b)
	   #t)
	  (else
	   (loop a.cdr)))))


(define-class <environment>
  (nongenerative nausicaa:evaluations:<environment>)
  (fields (immutable (imports <list>))
	  (immutable (bindings <list>))
	  (immutable environ)
	  (immutable elet))
  (protocol (lambda (make-top)
	      (lambda (imports bindings)
		(let* (($elet	(gensym))
		       (imports `((rename (nausicaa evaluations wrapper) (elet ,$elet))
				  ,@imports)))
		  ((make-top) imports bindings (apply environment imports) $elet)))))
  (maker ()
	 (imports:	'((rnrs)))
	 (bindings:	'()))
  (methods eval eval-for-bindings augment eval-to-augment!))

(define (<environment>-eval (o <environment>) expr)
;;;(pretty-print `(,o.elet ,o.bindings ,expr))
  (eval `(,o.elet () ,o.bindings ,expr) o.environ))

(define (<environment>-eval-for-bindings (o <environment>) expr result-identifiers)
  (assert-result-identifiers '<evaluation>-eval result-identifiers)
;;;(pretty-print `(,o.elet ,o.bindings ,expr))
  (let ((result-datums (eval `(,o.elet ,result-identifiers ,o.bindings ,expr) o.environ)))
    (map cons result-identifiers result-datums)))

(define (<environment>-eval-to-augment! (o <environment>) expr new-identifiers)
  (assert (not (%duplicates-symbols-alist? new-identifiers o.bindings)))
  (tail-set!/stx o.bindings (o.eval-for-bindings expr new-identifiers)))

(define (<environment>-augment (o <environment>) new-bindings)
  (assert (not (%duplicates-alist-alist? new-bindings o.bindings)))
  (make-from-fields <environment>
    o.imports (append o.bindings new-bindings) o.environ o.elet))


;;;; done

)

;;; end of file
