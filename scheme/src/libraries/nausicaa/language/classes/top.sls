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
(library (nausicaa language classes top)
  (export <top> <top>-bindings)
  (import (rnrs)
    (for (prefix (nausicaa language classes properties) prop.) expand)
    (nausicaa language classes internal-auxiliary-syntaxes))


(define <top>-rtd
  (make-record-type-descriptor '<top>
			       #f ;parent-rtd
			       'nausicaa:builtin:<top>
			       #f     ;sealed
			       #f     ;opaque
			       '#())) ;fields

(define <top>-cd
  (make-record-constructor-descriptor <top>-rtd #f #f))

(define make-<top>
  (record-constructor <top>-cd))

(define <top>?
  (record-predicate <top>-rtd))

(define-syntax <top>
  (lambda (stx)
    (syntax-case stx (:class-record-type-descriptor
		      :class-type-uid
		      :class-uid-list
		      :from-fields-constructor-descriptor
		      :is-a?
		      :parent-rtd-list
		      :public-constructor-descriptor
		      :superclass-constructor-descriptor
		      :with-class-bindings-of)

      ((_ :class-record-type-descriptor)
       #'<top>-rtd)

      ((_ :class-type-uid)
       #'(quote nausicaa:builtin:<top>))

      ((_ :class-uid-list)
       #'(quote (nausicaa:builtin:<top>)))

      ((_ :public-constructor-descriptor)
       #'<top>-cd)

      ((_ :superclass-constructor-descriptor)
       #'<top>-cd)

      ((_ :from-fields-constructor-descriptor)
       #'<top>-cd)

      ((_ :parent-rtd-list)
       #'(list <top>-rtd))

      ((_ :is-a? ?arg)
       #'(<top>? ?arg))

      ((_ :with-class-bindings-of ?inherit-options ?variable-name ?instance . ?body)
       #'(begin . ?body))

      ((_ ?keyword . ?rest)
       (syntax-violation '<top>
	 "invalid class internal keyword"
	 (syntax->datum stx) (syntax->datum #'?keyword))))))

(define-syntax <top>-bindings
  (syntax-rules ()
    ((_ ?class-name ?variable-name ?instance . ?body)
     (begin . ?body))))

(define-syntax dummy
  (begin
    (prop.struct-properties-define
     #'<top> (prop.make-class '()  ;list of supers
			      '()  ;field specs
			      '()  ;virtual field specs
			      '()  ;method specs
			      '()  ;mixins
			      '()))
;;;(write (prop.struct-properties-ref #'<top>))(newline)
    values)) ;list of field tags


;;;; done

)

;;; end of file
