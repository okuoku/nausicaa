;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: implementation of <top> class bindings
;;;Date: Mon May 24, 2010
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


(library (classes top)
  (export <top> <top>-superclass <top>-superlabel <top>-bindings
	  current-virtual-methods-vector)
  (import (rnrs)
    (parameters))


(define-record-type <top>
  (protocol (lambda (make-record)
	      (lambda ()
		(make-record (current-virtual-methods-vector)))))
  (fields __virtual_methods_vector__)
  (nongenerative nausicaa:builtin:<top>))

(define <top>-virtual-methods-functions
  '#())

(define <top>-virtual-methods-names
  '#())

(define current-virtual-methods-vector
  (make-parameter <top>-virtual-methods
    (lambda (table)
      (assert (vector? table))
      table)))

(define-syntax <top>-superclass
  (lambda (stx)
    (syntax-case stx (class-record-type-descriptor
		      class-type-uid
		      class-uid-list
		      public-constructor-descriptor
		      superclass-constructor-descriptor
		      from-fields-constructor-descriptor
		      parent-rtd-list
		      make make-from-fields is-a?
		      with-class-bindings-of
		      virtual-methods-vector-length
		      virtual-methods-vector-names
		      virtual-methods-vector-functions)

      ((_ class-record-type-descriptor)
       #'(record-type-descriptor <top>))

      ((_ class-type-uid)
       #'(quote nausicaa:builtin:<top>))

      ((_ class-uid-list)
       #'(quote (nausicaa:builtin:<top>)))

      ((_ public-constructor-descriptor)
       #'(record-constructor-descriptor <top>))

      ((_ superclass-constructor-descriptor)
       #'(record-constructor-descriptor <top>))

      ((_ from-fields-constructor-descriptor)
       #'(record-constructor-descriptor <top>))

      ((_ parent-rtd-list)
       #'(list (record-type-descriptor <top>)))

      ((_ is-a? ?arg)
       #'(<top>? ?arg))

      ((_ with-class-bindings-of ?inherit-options ?variable-name ?body0 ?body ...)
       #'(begin ?body0 ?body ...))

      ((_ virtual-methods-vector-length)
       0)

      ((_ virtual-methods-vector-functions)
       #'<top>-virtual-methods-functions)

      ((_ virtual-methods-vector-names)
       #'<top>-virtual-methods-names)

      ((_ ?keyword . ?rest)
       (syntax-violation '<top>
	 "invalid class internal keyword"
	 (syntax->datum #'(<top> ?keyword . ?rest))
	 (syntax->datum #'?keyword))))))

(define-syntax <top>-superlabel
  (syntax-rules (is-a? with-class-bindings-of)

    ((_ is-a? ?arg)
     #t)

    ((_ with-class-bindings-of ?inherit-options ?variable-name . ?body)
     (begin . ?body))

    ((_ ?keyword . ?rest)
     (syntax-violation '<top>
       "invalid class internal keyword"
       (syntax->datum #'(<top> ?keyword . ?rest))
       (syntax->datum #'?keyword)))))

(define-syntax <top>-bindings
  (syntax-rules ()
    ((_ ?class-name ?identifier . ?body)
     (begin . ?body))))


;;;; done

)

;;; end of file
