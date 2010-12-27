;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: submodules implementation
;;;Date: Thu Oct 14, 2010
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
(library (submodules)
  (export submodule export prefix)
  (import (rnrs)
    (only (language-extensions) define-values)
    (only (syntax-utilities) define-auxiliary-syntax)
    (for (only (syntax-utilities)
	       unwrap-syntax-object
	       identifier-suffix
	       all-identifiers?
	       identifier-general-append) expand))


(define-auxiliary-syntax export)
(define-auxiliary-syntax prefix)

(define-syntax submodule
  (lambda (stx)
    (define (build-exported-name prefix bind-stx)
      (datum->syntax bind-stx (string->symbol (identifier-general-append prefix bind-stx))))
    (syntax-case stx (export prefix)
      ((_ ?name (export ?bind0 ?bind ...) (prefix ?prefix) ?body0 ?body ...)
       (all-identifiers? #'(?name ?bind0 ?bind ...))
       (with-syntax (((BIND ...) (map (lambda (bind-stx)
					(build-exported-name (unwrap-syntax-object #'?prefix) bind-stx))
				   (unwrap-syntax-object #'(?bind0 ?bind ...)))))
	 #'(define-values (BIND ...)
	     (let ()
	       ?body0 ?body ...
	       (values ?bind0 ?bind ...)))))

      ((?submodule ?name (export ?bind0 ?bind ...) (prefix) ?body0 ?body ...)
       #'(?submodule ?name (export ?bind0 ?bind ...) (prefix "") ?body0 ?body ...))

      ((?submodule ?name (export ?bind0 ?bind ...) ?body0 ?body ...)
       #`(?submodule ?name
		     (export ?bind0 ?bind ...)
		     (prefix #,(identifier-suffix #'?name "."))
		     ?body0 ?body ...))
      )))


;;;; done

)

;;; end of file
