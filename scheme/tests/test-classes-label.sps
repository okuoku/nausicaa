;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for labels
;;;Date: Wed Dec 15, 2010
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
(import (nausicaa)
  (nausicaa checks)
  (nausicaa debugging)
  (records-lib)
  (rnrs eval))

(check-set-mode! 'report-failed)
(display "*** testing class labels\n")


(parametrise ((check-test-name	'virtual-fields))

  (let ()	;virtual fields

    (define-label <alpha>
      (virtual-fields a b c))

    (define (<alpha>-a o) 1)
    (define (<alpha>-b o) 2)
    (define (<alpha>-c o) 3)

    (check
	(let/with-class (((o <alpha>) #t))
	  (list o o.a o.b o.c))
      => '(#t 1 2 3))

    #f)

  (let ()	;virtual fields

    (define-label <alpha>
      (virtual-fields (mutable a)
		      (immutable b)
		      (immutable c)))

    (define (<alpha>-a o) 1)
    (define (<alpha>-b o) 2)
    (define (<alpha>-c o) 3)

    (check
	(let/with-class (((o <alpha>) #t))
	  (list o o.a o.b o.c))
      => '(#t 1 2 3))

    #f)

  #t)


(parametrise ((check-test-name	'methods))

  (let ()	;methods

    (define-label <alpha>
      (methods a b c))

    (define (<alpha>-a o) 1)
    (define (<alpha>-b o) 2)
    (define (<alpha>-c o) 3)

    (check
	(let/with-class (((o <alpha>) #t))
	  (list o (o.a) (o.b) (o.c)))
      => '(#t 1 2 3))

    #f)

  (let ()	;in-definition methods

    (define-label <alpha>
      (method (a o) 1)
      (method (b o) 2)
      (method (c o) 3))

    (check
	(let/with-class (((o <alpha>) #t))
	  (list o (o.a) (o.b) (o.c)))
      => '(#t 1 2 3))

    #f)

  (let ()	;syntax methods

    (define-label <alpha>
      (method-syntax a
	(syntax-rules ()
	  ((_ ?obj) 1)))
      (method-syntax b
	(syntax-rules ()
	  ((_ ?obj) 2)))
      (method-syntax c
	(syntax-rules ()
	  ((_ ?obj) 3))))

    (check
	(let/with-class (((o <alpha>) #t))
	  (list o (o.a) (o.b) (o.c)))
      => '(#t 1 2 3))

    #f)

  #t)


(parametrise ((check-test-name	'predicate))

  (let ()	;predicate

    (define-label <alpha>
      (predicate integer?))

    (check
	(is-a? 123 <alpha>)
      => #t)

    (check
	(is-a? "ciao" <alpha>)
      => #f)

    #f)

  #t)


(parametrise ((check-test-name	'setter-getter))

  (let ()	;setter

    (define-label <alpha>
      (setter <alpha>-setf))

    (define (<alpha>-setf o key val)
      (list o key val))

    (check
	(let/with-class (((o <alpha>) #t))
	  (setf (o 1) 2))
      => '(#t 1 2))

    #f)

  (let ()	;getter

    (define-label <alpha>
      (getter <alpha>-getf))

    (define (<alpha>-getf o key)
      (list o key))

    (check
	(let/with-class (((o <alpha>) #t))
	  (getf (o 1)))
      => '(#t 1))

    #f)

  #t)


(parametrise ((check-test-name	'bindings))

  (let ()	;bindings

    (define-label <alpha>
      (bindings <alpha>-bindings))

    (define-syntax <alpha>-bindings
      (lambda (stx)
	(syntax-case stx ()
	  ((_ ?class-name ?identifier . ?body)
	   (with-syntax ((A (datum->syntax #'?identifier 'a)))
	     #`(let ((A 123)) . ?body))))))

    (check
	(let/with-class (((o <alpha>) #t))
	  a)
      => 123)

    #f)

  #t)


(parametrise ((check-test-name	'inheritance))

  (let ()		;virtual fields

    (define-label <alpha>
      (virtual-fields a b z))

    (define (<alpha>-a o) 1)
    (define (<alpha>-b o) 2)
    (define (<alpha>-z o) 88)

    (define-label <beta>
      (inherit <alpha>)
      (virtual-fields c d z))

    (define (<beta>-c o) 3)
    (define (<beta>-d o) 4)
    (define (<beta>-z o) 99)

    (check
	(let/with-class (((o <alpha>) #t))
	  (list o o.a o.b o.z))
      => '(#t 1 2 88))

    (check
	(let/with-class (((o <beta>) #t))
	  (list o o.a o.b o.c o.d o.z))
      => '(#t 1 2 3 4 99))

    #f)

  (let ()		;methods

    (define-label <alpha>
      (methods a b z))

    (define (<alpha>-a o) 1)
    (define (<alpha>-b o) 2)
    (define (<alpha>-z o) 88)

    (define-label <beta>
      (inherit <alpha>)
      (methods c d z))

    (define (<beta>-c o) 3)
    (define (<beta>-d o) 4)
    (define (<beta>-z o) 99)

    (check
	(let/with-class (((o <alpha>) #t))
	  (list o (o.a) (o.b) (o.z)))
      => '(#t 1 2 88))

    (check
	(let/with-class (((o <beta>) #t))
	  (list o (o.a) (o.b) (o.c) (o.d) (o.z)))
      => '(#t 1 2 3 4 99))

    #f)

  (let ()	;inherit <top>

    (define-label <alpha>
      (inherit <top>)
      (virtual-fields a b c))

    (define (<alpha>-a o) 1)
    (define (<alpha>-b o) 2)
    (define (<alpha>-c o) 3)

    (check
	(let/with-class (((o <alpha>) #t))
	  (list o o.a o.b o.c))
      => '(#t 1 2 3))

    #f)


  (let ()		;parent is a class

    (define-class <alpha>
      (fields a b z))

    (define-label <beta>
      (inherit <alpha>
	(concrete-fields))
      (virtual-fields c d z))

    (define (<beta>-c o) 3)
    (define (<beta>-d o) 4)
    (define (<beta>-z o) 99)

    (check
	(let/with-class (((o <alpha>) (make <alpha>
					1 2 3)))
	  (list o.a o.b o.z))
      => '(1 2 3))

    (check
	(let*/with-class (((p <alpha>)	(make <alpha> 1 2 -3))
			  ((o <beta>)	p))
	  (list o.a o.b o.c o.d o.z))
      => '(1 2 3 4 99))

    #f)

  #t)


(parametrise ((check-test-name	'custom-maker))

  (let ()	;tuple example

    (define-auxiliary-syntaxes
      a: b: c:)

    (define-maker make-tuple
      list
      ((a: #f)
       (b: #f)
       (c: #f)))

    (define (tuple-predicate obj)
      (and (not (null? obj))
	   (list? obj)
	   (= 3 (length obj))))

    (define-label (<tuple> tuple?)
      (predicate tuple-predicate)
      (virtual-fields (immutable a car)
		      (immutable b cadr)
		      (immutable c caddr))
      (custom-maker make-tuple))

    (let (((o <tuple>) (make <tuple>
			 (a: 1)
			 (b: 2)
			 (c: 3))))

      (check
	  (is-a? o <tuple>)
	=> #t)

      (check
	  (tuple? o)
	=> #t)

      (check
	  (list o.a o.b o.c)
	=> '(1 2 3))

      #f)

    #f)

  #t)


;;;; done

(check-report)

;;; end of file
