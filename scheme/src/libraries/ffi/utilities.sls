;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: utility macros for FFI
;;;Date: Mon Nov 30, 2009
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


(library (ffi utilities)
  (export
    define-shared-object
    make-c-function*		make-c-function/with-errno*
    make-c-callout*		make-c-callout/with-errno*
    make-c-callback*
    define-c-functions		define-c-functions/with-errno
    define-c-callouts		define-c-callouts/with-errno)
  (import (nausicaa)
    (ffi primitives)
    (for (ffi clang-data-types) expand run))


(define-syntax define-shared-object
  (syntax-rules ()
    ((_ ?name ?library-name)
     (define ?name
       (open-shared-object ?library-name)))))

(define-syntax define-c-functions
  (syntax-rules ()
    ((_ ?shared-object (?name (?retval ?funcname (?arg0 ?arg ...))) ...)
     (begin
       (define ?name
	 (make-c-function* ?shared-object ?retval ?funcname (?arg0 ?arg ...)))
       ...))))

(define-syntax define-c-functions/with-errno
  (syntax-rules ()
    ((_ ?shared-object (?name (?retval ?funcname (?arg0 ?arg ...))) ...)
     (begin
       (define ?name
	 (make-c-function/with-errno* ?shared-object ?retval ?funcname (?arg0 ?arg ...)))
       ...))))

(define-syntax define-c-callouts
  (syntax-rules ()
    ((_ (?name (?retval ?funcname (?arg0 ?arg ...))) ...)
     (begin
       (define ?name
	 (make-c-callout* ?retval ?funcname (?arg0 ?arg ...)))
       ...))))

(define-syntax define-c-callouts/with-errno
  (syntax-rules ()
    ((_ (?name (?retval ?funcname (?arg0 ?arg ...))) ...)
     (begin
       (define ?name
	 (make-c-callout/with-errno* ?retval ?funcname (?arg0 ?arg ...)))
       ...))))


(define-syntax make-c-function*
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?shared-object ?ret-type ?funcname (?arg-type0 ?arg-type ...))
       (with-syntax ((RET	(clang-quote-type-stx-if-external #'?ret-type))
		     ((ARG ...)	(map clang-quote-type-stx-if-external #'(?arg-type0 ?arg-type ...))))
	 #'(make-c-function ?shared-object RET '?funcname (list ARG ...))))
      ((_ ?shared-object ?ret-type ?funcname ((?arg-type0 ?arg-name0) (?arg-type ?arg-name) ...))
       (with-syntax ((RET	(clang-quote-type-stx-if-external #'?ret-type))
		     ((ARG ...)	(map clang-quote-type-stx-if-external #'(?arg-type0 ?arg-type ...))))
	 #'(make-c-function ?shared-object RET '?funcname (list ARG ...))))
      )))

(define-syntax make-c-function/with-errno*
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?shared-object ?ret-type ?funcname (?arg-type0 ?arg-type ...))
       (with-syntax ((RET	(clang-quote-type-stx-if-external #'?ret-type))
		     ((ARG ...)	(map clang-quote-type-stx-if-external #'(?arg-type0 ?arg-type ...))))
	 #'(make-c-function/with-errno ?shared-object RET '?funcname (list ARG ...))))
      ((_ ?shared-object ?ret-type ?funcname ((?arg-type0 ?arg-name0) (?arg-type ?arg-name) ...))
       (with-syntax ((RET	(clang-quote-type-stx-if-external #'?ret-type))
		     ((ARG ...)	(map clang-quote-type-stx-if-external #'(?arg-type0 ?arg-type ...))))
	 #'(make-c-function/with-errno ?shared-object RET '?funcname (list ARG ...))))
      )))

(define-syntax make-c-callout*
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?ret-type ?address (?arg-type0 ?arg-type ...))
       (with-syntax ((RET	(clang-quote-type-stx-if-external #'?ret-type))
		     ((ARG ...)	(map clang-quote-type-stx-if-external #'(?arg-type0 ?arg-type ...))))
	 #'(make-c-callout RET ?address (list ARG ...))))
      ((_ ?ret-type ?address ((?arg-type0 ?arg-name0) (?arg-type ?arg-name) ...))
       (with-syntax ((RET	(clang-quote-type-stx-if-external #'?ret-type))
		     ((ARG ...)	(map clang-quote-type-stx-if-external #'(?arg-type0 ?arg-type ...))))
	 #'(make-c-callout RET ?address (list ARG ...))))
      )))

(define-syntax make-c-callout/with-errno*
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?ret-type ?address (?arg-type0 ?arg-type ...))
       (with-syntax ((RET	(clang-quote-type-stx-if-external #'?ret-type))
		     ((ARG ...)	(map clang-quote-type-stx-if-external #'(?arg-type0 ?arg-type ...))))
	 #'(make-c-callout/with-errno RET ?address (list ARG ...))))
      ((_ ?ret-type ?address ((?arg-type0 ?arg-name0) (?arg-type ?arg-name) ...))
       (with-syntax ((RET	(clang-quote-type-stx-if-external #'?ret-type))
		     ((ARG ...)	(map clang-quote-type-stx-if-external #'(?arg-type0 ?arg-type ...))))
	 #'(make-c-callout/with-errno RET ?address (list ARG ...))))
      )))

(define-syntax make-c-callback*
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?ret-type ?scheme-function (?arg-type0 ?arg-type ...))
       (with-syntax ((RET	(clang-quote-type-stx-if-external #'?ret-type))
		     ((ARG ...)	(map clang-quote-type-stx-if-external #'(?arg-type0 ?arg-type ...))))
	 #'(make-c-callback RET ?scheme-function (list ARG ...))))
      ((_ ?ret-type ?scheme-function ((?arg-type0 ?arg-name0) (?arg-type ?arg-name) ...))
       (with-syntax ((RET	(clang-quote-type-stx-if-external #'?ret-type))
		     ((ARG ...)	(map clang-quote-type-stx-if-external #'(?arg-type0 ?arg-type ...))))
	 #'(make-c-callback RET ?scheme-function (list ARG ...))))
      )))


;;;; done

)

;;; end of file
