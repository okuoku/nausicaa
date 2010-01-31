;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: implementation of begin0
;;;Date: Wed Oct  7, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2010 Marco Maggi <marcomaggi@gna.org>
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


(library (begin0)
  (export  begin0 begin0-let begin0-let* begin0-letrec)
  (import (rnrs))

  (define-syntax begin0
    ;;This  syntax comes  from the  R6RS original  document,  Appendix A
    ;;``Formal semantics''.
    (syntax-rules ()
      ((_ ?expr0 ?expr ...)
       (call-with-values
	   (lambda () ?expr0)
	 (lambda args
	   ?expr ...
	   (apply values args))))))

  (define-syntax begin0-let
    (syntax-rules ()
      ((_ (((?var0 ...) ?expr0) ((?var ...) ?expr) ...) ?form0 ?form ...)
       (let-values (((?var0 ...) ?expr0)
		    ((?var  ...) ?expr)
		    ...)
	 ?form0 ?form ...
	 (values ?var0 ...)))
      ((_ ((?var0 ?expr0) (?var ?expr) ...) ?form0 ?form ...)
       (let ((?var0 ?expr0)
	     (?var  ?expr)
	     ...)
	 ?form0 ?form ...
	 ?var0))))

  (define-syntax begin0-let*
    (syntax-rules ()
      ((_ ((?var0 ?expr0) (?var ?expr) ...) ?form0 ?form ...)
       (let* ((?var0 ?expr0)
	      (?var  ?expr)
	      ...)
	 ?form0 ?form ...
	 ?var0))))

  (define-syntax begin0-letrec
    (syntax-rules ()
      ((_ ((?var0 ?expr0) (?var ?expr) ...) ?form0 ?form ...)
       (letrec ((?var0 ?expr0)
		(?var  ?expr)
		...)
	 ?form0 ?form ...
	 ?var0)))))

;;; end of file
