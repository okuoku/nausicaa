;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for class typed fields
;;;Date: Sun Dec 19, 2010
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
  (checks)
  (debugging)
  (rnrs eval)
  (rnrs mutable-pairs))

(check-set-mode! 'report-failed)
(display "*** testing class typed fields\n")


(parametrise ((check-test-name	'class-definitions))

  (define-foreign-class <mutable-pair>
    (predicate pair?)
    (virtual-fields (mutable car car set-car!)
		    (mutable cdr cdr set-cdr!)))

  (let ()	;typed field

    (define-class <alpha>
      (fields (mutable (a <mutable-pair>))))

    (check
	(let (((o <alpha>) (make <alpha> '(1 . 2))))
	  o.a)
      => '(1 . 2))

    (check
	(let (((o <alpha>) (make <alpha> '(1 . 2))))
	  (list o.a.car o.a.cdr))
      => '(1 2))

    (check
	(let (((o <alpha>) (make <alpha> '(1 . 2))))
	  (set! o.a.car 11)
	  (set! o.a.cdr 22)
	  (list o.a.car o.a.cdr))
      => '(11 22))

    #f)

  (let ()	;typed field with typed field

    (define-class <alpha>
      (fields (mutable (a <mutable-pair>))))

    (define-class <beta>
      (fields (immutable (b <alpha>))))

    (check
	(let (((o <beta>) (make <beta> (make <alpha> '(1 . 2)))))
	  o.b.a)
      => '(1 . 2))

    (check
	(let (((o <beta>) (make <beta> (make <alpha> '(1 . 2)))))
	  (list o.b.a.car o.b.a.cdr))
      => '(1 2))

    (check
	(let (((o <beta>) (make <beta> (make <alpha> '(1 . 2)))))
	  (set! o.b.a.car 11)
	  (set! o.b.a.cdr 22)
	  (list o.b.a.car o.b.a.cdr))
      => '(11 22))

    #f)

  #t)


(parametrise ((check-test-name	'label-definitions))

  (define-label <mutable-pair>
    (custom-maker cons)
    (predicate pair?)
    (virtual-fields (mutable car car set-car!)
		    (mutable cdr cdr set-cdr!)))

  (define-class <box>
    (fields (mutable box)))

  (let ()	;typed field

    (define-label <alpha>
      (custom-maker make-<box>)
      (virtual-fields (mutable (a <mutable-pair>) <box>-box <box>-box!)))

    (check
    	(let (((o <alpha>) (make <alpha> '(1 . 2))))
    	  o.a)
      => '(1 . 2))

    (check
    	(let (((o <alpha>) (make <alpha> '(1 . 2))))
    	  (list o.a.car o.a.cdr))
      => '(1 2))

    (check
    	(let (((o <alpha>) (make <alpha> '(1 . 2))))
    	  (set! o.a.car 11)
    	  (set! o.a.cdr 22)
    	  (list o.a.car o.a.cdr))
      => '(11 22))

    #f)

  (let ()	;typed field with typed field

    (define-label <alpha>
      (custom-maker make-<box>)
      (virtual-fields (mutable (a <mutable-pair>) <box>-box <box>-box!)))

    (define-label <beta>
      (custom-maker make-<box>)
      (virtual-fields (immutable (b <alpha>) <box>-box)))

    (check
	(let (((o <beta>) (make <beta> (make <alpha> '(1 . 2)))))
	  o.b.a)
      => '(1 . 2))

    (check
	(let (((o <beta>) (make <beta> (make <alpha> '(1 . 2)))))
	  (list o.b.a.car o.b.a.cdr))
      => '(1 2))

    (check
	(let (((o <beta>) (make <beta> (make <alpha> '(1 . 2)))))
	  (set! o.b.a.car 11)
	  (set! o.b.a.cdr 22)
	  (list o.b.a.car o.b.a.cdr))
      => '(11 22))

    #f)

  #t)


(parametrise ((check-test-name	'recursive-types))

  #;(check	;detect recursive in definition
      (guard (E ((syntax-violation? E)
		 (debug-print-condition "direct recursive type:" E)
		 (syntax-violation-subform E))
		(else E))
	(eval '(define-class <alpha>
		 (fields (mutable (a <alpha>))))
	      (environment '(nausicaa))))
    => '<alpha>)

  (check	;detect recursive type by WITH-CLASS
      (guard (E ((syntax-violation? E)
		 (debug-print-condition "direct recursive type:" E)
		 (syntax-violation-subform E))
		(else E))
	(eval '(let ()
		 (define-class <alpha>
		   (fields (mutable (a <alpha>))))
		 (define o (make <alpha> 1))
		 (with-class ((o <alpha>))
		   #t))
	      (environment '(nausicaa))))
    => '<alpha>)


  #t)


;;;; done

(check-report)

;;; end of file
