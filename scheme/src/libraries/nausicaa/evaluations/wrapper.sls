;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: wrapper syntax for eval expressions
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
(library (nausicaa evaluations wrapper)
  (export elet)
  (import (rnrs)
    (for (prefix (only (nausicaa language syntax-utilities)
		       all-identifiers?)
		 sx.)
	 expand))
  (define-syntax elet
    (lambda (stx)
      (syntax-case stx ()

	;; no body, no result identifiers
	((_ () ((?var . ?datum) ...))
	 (sx.all-identifiers? #'(?var ...))
	 #'(let ((?var (quote ?datum)) ...)
	     (values)))

	;; with body, no result identifiers
	((_ () ((?var . ?datum) ...) . ?body)
	 (sx.all-identifiers? #'(?var ...))
	 #'(let ((?var (quote ?datum)) ...)
	     . ?body))

	;; no body, with result identifiers
	((_ (?id0 ?id ...) ((?var . ?datum) ...))
	 (sx.all-identifiers? #'(?var ... ?id0 ?id ...))
	 #'(let ((?var (quote ?datum)) ...)
	     (list ?id0 ?id ...)))

	;; with body, with result identifiers
	((_ (?id0 ?id ...) ((?var . ?datum) ...) ?form0 ?form ...)
	 (sx.all-identifiers? #'(?var ... ?id0 ?id ...))
	 #'(let ((?var (quote ?datum)) ...)
	     ?form0 ?form ...
	     (list ?id0 ?id ...)))

	))))

;;; end of file
