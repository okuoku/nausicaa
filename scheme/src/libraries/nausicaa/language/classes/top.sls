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
  (export <top> <top>-superclass <top>-superlabel <top>-bindings)
  (import (rnrs)
    (nausicaa language classes internal-auxiliary-syntaxes)
    (prefix (nausicaa language identifier-properties) ip.)
    (prefix (nausicaa language classes properties) prop.))


(define-record-type <top>
  (nongenerative nausicaa:builtin:<top>))

(define-syntax <top>-superclass
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
       #'(record-type-descriptor <top>))

      ((_ :class-type-uid)
       #'(quote nausicaa:builtin:<top>))

      ((_ :class-uid-list)
       #'(quote (nausicaa:builtin:<top>)))

      ((_ :public-constructor-descriptor)
       #'(record-constructor-descriptor <top>))

      ((_ :superclass-constructor-descriptor)
       #'(record-constructor-descriptor <top>))

      ((_ :from-fields-constructor-descriptor)
       #'(record-constructor-descriptor <top>))

      ((_ :parent-rtd-list)
       #'(list (record-type-descriptor <top>)))

      ((_ :is-a? ?arg)
       #'(<top>? ?arg))

      ((_ :with-class-bindings-of ?inherit-options ?variable-name ?body0 ?body ...)
       #'(begin ?body0 ?body ...))

      ((_ ?keyword . ?rest)
       (syntax-violation '<top>
	 "invalid class internal keyword"
	 (syntax->datum #'(<top> ?keyword . ?rest))
	 (syntax->datum #'?keyword))))))

(define-syntax <top>-superlabel
  (syntax-rules (:is-a? :with-class-bindings-of)

    ((_ :is-a? ?arg)
     #t)

    ((_ :with-class-bindings-of ?inherit-options ?variable-name . ?body)
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

(define-syntax define-properties
  (lambda (stx)
    (begin
      (ip.define #'<top> #':struct-properties
		 (prop.make-class '()	;list of supers
				  '()	;field specs
				  '()	;virtual field specs
				  '()	;method specs
				  '()	;mixins
				  '()))	;list of field tags
      #'(define dummy))))
(define-properties)


;;;; done

)

;;; end of file
