;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for class tagged fields
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
  (nausicaa checks)
  (nausicaa debugging)
  (rnrs eval)
  (rnrs mutable-pairs))

(check-set-mode! 'report-failed)
(display "*** testing class tagged fields\n")


(parametrise ((check-test-name	'class-definitions))

  (define-label <mutable-pair>
    (predicate pair?)
    (virtual-fields (mutable car car set-car!)
		    (mutable cdr cdr set-cdr!)))

  (let ()	;tagged field

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

  (let ()	;tagged field with tagged field

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

  (let ()	;tagged field

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

  (let ()	;tagged field with tagged field

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


(parametrise ((check-test-name	'non-recursive-types))

  (let ()	;not a recursive type definition
#|
<alpha> <-----
  ^           |
  |inherit    |field type
  |           |
<beta> -------
|#
    (define-class <alpha>
      (fields (mutable a)))

    (define-class <beta>
      (inherit <alpha>)
      (fields (b <alpha>)))

    (let ((o (make <beta> 1 (make <alpha> 2))))
      (check
	  (with-class ((o <beta>))
	    (list o.a o.b.a))
	=> '(1 2)))
    #f)

  (let ()	;not a recursive type definition
#|
<alpha> <-------------
  ^                   |
  |inherit            | inherit
  |                   |
<beta> ----------> <gamma>
       field type
|#
    (define-class <alpha>
      (fields (mutable a)))

    (define-class <beta>
      (inherit <alpha>)
      (fields (b <gamma>)))

    (define-class <gamma>
      (inherit <alpha>))

    (let (((o <beta>)
	   (make <beta> 'beta-alpha-a (make <gamma> 'gamma-alpha-a))))
      (check
	  (list o.a o.b.a)
	=> '(beta-alpha-a gamma-alpha-a))
      #f)
    #f)

  (let ()	;not a recursive type definition
#|
<alpha> <-------------
  ^                   |
  |inherit            | field type
  |                   |
<beta> ----------> <gamma>
       field type
|#
    (define-class <alpha>
      (fields (mutable a)))

    (define-class <beta>
      (inherit <alpha>)
      (fields (b <gamma>)))

    (define-class <gamma>
      (fields (g <alpha>)))

    (let ((o (make <beta>
	       'beta-alpha-a
	       (make <gamma>
		 (make <alpha>
		   'alpha-a)))))
      (check
	  (with-class ((o <beta>))
	    (list o.a o.b.g.a))
	=> '(beta-alpha-a alpha-a)))
    #f)

  #t)


(parametrise ((check-test-name	'recursive-types)
	      (debugging	#t))


  (check  	;recursive type in class definition
      (guard (E ((syntax-violation? E)
;;;		 (debug-print-condition "direct recursive type:" E)
		 (syntax-violation-subform E))
		(else E))
#|
   ----
  |    |
  v    |field type
<bad1>-
|#
	(eval '(let ()
		 (define-class <bad1>
		   (fields (mutable (a <bad1>))))
		 #f)
	      (environment '(nausicaa))))
    => '<bad1>)

  (check	;recursive type in label definition
      (guard (E ((syntax-violation? E)
;;;		 (debug-print-condition "direct recursive type:" E)
		 (syntax-violation-subform E))
		(else E))
#|
   ----
  |    |
  v    |field type
<bad2>--
|#
	(eval '(let ()
		 (define-label <bad2>
		   (virtual-fields (immutable (a <bad2>) car)))
		 #f)
	      (environment '(nausicaa))))
    => '<bad2>)

;;; --------------------------------------------------------------------

#|
<alpha1> -----
  ^           |
  | inherit   | field type
  |           |
<beta1> <-----
|#
  (check	;type recursion in parent class definition
      (guard (E ((syntax-violation? E)
;;;		 (debug-print-condition "weird recursive type:" E)
		 (syntax-violation-subform E))
		(else E))
	(eval '(let ()
		 (define-class <alpha1>
		   (fields (a <beta1>)))
		 (define-class <beta1>
		   (inherit <alpha1>))
		 #f)
	      (environment '(nausicaa))))
    => '<beta1>)

#|
<alpha2> -----
  ^           |
  | inherit   | field type
  |           |
<beta2> <-----
|#
  (check	;type recursion in parent label definition
      (guard (E ((syntax-violation? E)
;;;		 (debug-print-condition "weird recursive type:" E)
		 (syntax-violation-subform E))
		(else E))
	(eval '(let ()
		 (define-label <alpha2>
		   (virtual-fields (a <beta2>)))
		 (define-label <beta2>
		   (inherit <alpha2>))
		 #f)
	      (environment '(nausicaa))))
    => '<beta2>)

;;; --------------------------------------------------------------------

#|
  Recursive type:

      -------- <alpha3>
     |            ^
     v            |
  <gamma3> --> <beta3>
|#
  (check	;recursive type in class definition
      (guard (E ((syntax-violation? E)
;;;		 (debug-print-condition "direct recursive type:" E)
		 (syntax-violation-subform E))
		(else E))
	(eval '(let ()
		 (define-class <alpha3>
		   (fields (a <gamma3>)))

		 (define-class <beta3>
		   (inherit <alpha3>)
		   (fields b))

		 (define-class <gamma3>
		   (fields (g <beta3>)))
		 #f)
	      (environment '(nausicaa))))
    => '<gamma3>)

  #t)


(parametrise ((check-test-name	'unbound))

;;;The following tests for circular tagging are the same as before; here
;;;we use the same identifiers  for the classes in different invocations
;;;of EVAL.
;;;
;;;If the  identifier properties for  <ALPHA> and <BETA> are  set before
;;;the  identifiers   <ALPHA>  and   <BETA>  are  bound   to  something,
;;;FREE-IDENTIFIER=? will  not be able to distinguish  between <BETA> in
;;;the  first  test  and <BETA>  in  the  second  test and  an  infinite
;;;recursion will happen (last tested Wed Dec 22, 2010).
;;;
;;;To  avoid this we  MUST bind  the class  and label  identifier BEFORE
;;;setting their identifier properties.

  (check	;type recursion in parent class definition
      (guard (E ((syntax-violation? E)
		 (syntax-violation-subform E))
		(else E))
	(eval '(let ()
		 (define-class <alpha>
		   (fields (a <beta>)))
		 (define-class <beta>
		   (inherit <alpha>))
		 #f)
	      (environment '(nausicaa))))
    => '<beta>)

  (check	;type recursion in parent label definition
      (guard (E ((syntax-violation? E)
		 (syntax-violation-subform E))
		(else E))
	(eval '(let ()
		 (define-label <alpha>
		   (virtual-fields (a <beta>)))
		 (define-label <beta>
		   (inherit <alpha>))
		 #f)
	      (environment '(nausicaa))))
    => '<beta>)

  #t)


;;;; done

(check-report)

;;; end of file
