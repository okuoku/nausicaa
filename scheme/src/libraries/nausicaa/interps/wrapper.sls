;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: wrapper syntax for interps expressions
;;;Date: Fri Dec 17, 2010
;;;
;;;Abstract
;;;
;;;	This library  exports the ELET macro  to be used  as wrapper for
;;;	expressions for EVAL.  Basically, it is to be used as:
;;;
;;;	    (eval `(elet ((,var . ,init) ...) ,form ...)
;;;	       (environment '(rnrs)
;;;	                    '(evaluations wrapper)))
;;;
;;;     but  since we  do not  want a  name collision  between  ELET and
;;;     bindings  imported by  the environment,  we should  do something
;;;     like:
;;;
;;;	    (let (($elet (gensym)))
;;;	      (eval `(,$elet ((,var . ,init) ...) ,form ...)
;;;	         (environment '(rnrs)
;;;	                      `(rename (evaluations wrapper)
;;;                                    (elet ,$elet)))))
;;;
;;;	where, ideally, GENSYM should  return a non-interned symbol also
;;;	having a unique name.
;;;
;;;	Notice that we  expect the INIT expressions to  be datums, being
;;;	result  of previous evaluations;  so ELET  quotes them  to avoid
;;;	errors when evaluating.
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
(library (nausicaa interps wrapper)
  (export elet)
  (import (rnrs)
    (only (nausicaa interps variables) define-variable)
    (only (nausicaa language syntax-utilities) all-identifiers?))
  (define-syntax elet
    (lambda (stx)
      (syntax-case stx ()
	((?k ?end-of-eval #(?var ...) #(?datum ...) ?expr)
	 (all-identifiers? #'(?var ...))
	 (with-syntax ((DEFINE-GLOBAL (datum->syntax #'?k 'define-global)))
	   #'(call/cc (lambda (eval-kont)
			(define-syntax DEFINE-GLOBAL
			  (syntax-rules ()
			    ((_ . ?args)
			     (define-variable eval-kont . ?args))))
			(DEFINE-GLOBAL ?var (quote ?datum))
			...
			(let ()
			  (define define-variable) ;shadows the binding
			  (define eval-kont)	   ;shadows the binding
			  (call-with-values
			      (lambda () ?expr)
			    (lambda results
			      ((quote ?end-of-eval) results))))))))
	))))

;;; end of file
