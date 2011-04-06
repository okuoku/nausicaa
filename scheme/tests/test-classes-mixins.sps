;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for class mixins
;;;Date: Sat Jan  8, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (rnrs eval)
  (nausicaa checks))

(check-set-mode! 'report-failed)
(display "*** testing class mixins\n")


(parametrise ((check-test-name	'class))

  (let ()
    (define-mixin <stuff>
      (fields c)
      (method (doit (o <stuff>))
	(+ 1 o.c)))

    (define-class <base>
      (fields a))

    (define-class <one>
      (inherit <base>)
      (maker ()
	     (a:	#f)
	     (p:	#f)
	     (c:	#f))
      (fields p)
      (mixins <stuff>))

    (define-class <two>
      (inherit <base>)
      (maker ()
	     (a:	#f)
	     (q:	#f)
	     (c:	#f))
      (fields q)
      (mixins <stuff>))

    (check
	(let-make ((o <one>
		      (a: 10)
		      (p: 20)
		      (c: 30)))
	  (o.doit))
      => 31)

    (check
	(let-make ((o <two>
		      (a: 10)
		      (q: 20)
		      (c: 30)))
	  (o.doit))
      => 31)

    #f)

;;; --------------------------------------------------------------------

  (let ()	;mixin accessing the fields of the receiving class

    (define-mixin <stuff1>
      (fields c)
      (method (doit (o <stuff1>))
	(+ o.a o.c)))

    (define-class <alpha1>
      (fields a)
      (mixins <stuff1>))

    (check
	(let-make ((o <alpha1> 1 2))
	  (o.doit))
      => 3)

    #f)

  #t)


(parametrise ((check-test-name	'label))

  (define-mixin <stuff2>
    (virtual-fields (immutable c car))
    (method (doit (o <stuff2>))
      (+ 1 o.c)))

  (define-label <one2>
    (virtual-fields (immutable p cdr))
    (mixins <stuff2>))

  (define-label <two2>
    (virtual-fields (immutable q cdr))
    (mixins <stuff2>))

  (check
      (let (((o <one2>) '(10 . 20)))
        (o.doit))
    => 11)

  (check
      (let (((o <two2>) '(10 . 20)))
        (o.doit))
    => 11)

  #t)


(parametrise ((check-test-name	'multiple-mixins))

  (define-mixin <stuff3>
    (fields c)
    (method (doit (o <stuff3>))
      (+ 1 o.c)))

  (define-mixin <other-stuff3>
    (fields p))

  (define-class <base3>
    (fields a))

  (define-class <one3>
    (inherit <base3>)
    (maker ()
	   (a:	#f)
	   (p:	#f)
	   (c:	#f))
    (mixins <stuff3> <other-stuff3>))

  (check
      (let-make ((o <one3>
		    (a: 10)
		    (p: 20)
		    (c: 30)))
        (o.doit))
    => 31)

  (check
      (let-make ((o <one3>
		    (a: 10)
		    (p: 20)
		    (c: 30)))
        (list o.a o.p o.c))
    => '(10 20 30))

  #t)


(parametrise ((check-test-name	'composite-mixins))

  (define-mixin <stuff4>
    (fields c)
    (method (doit (o <stuff4>))
      (+ 1 o.c)))

  (define-mixin <other-stuff4>
    (fields p)
    (mixins <stuff4>))

  (define-class <base4>
    (fields a))

  (define-class <one4>
    (inherit <base4>)
    (maker ()
	   (a:	#f)
	   (p:	#f)
	   (c:	#f))
    (mixins <other-stuff4>))

  (check
      (let-make ((o <one4>
		    (a: 10)
		    (p: 20)
		    (c: 30)))
        (o.doit))
    => 31)

  (check
      (let-make ((o <one4>
		    (a: 10)
		    (p: 20)
		    (c: 30)))
        (list o.a o.p o.c))
    => '(10 20 30))

  #t)


(parametrise ((check-test-name	'errors)
	      (debugging	#t))

  (check 	;empty mixin definition
      (guard (E ((syntax-violation? E)
		 (debug-print-condition "good:" E)
		 (syntax-violation-subform E))
		(else
		 (debug-print-condition "wrong:" E)
		 E))
	(eval '(let ()
		 (define-mixin <alpha10>))
	      (environment '(nausicaa))))
    => '#f)

;;; --------------------------------------------------------------------

  (check	;duplicated field names in class and mixin
      (guard (E ((syntax-violation? E)
		 (debug-print-condition "good:" E)
		 (syntax-violation-subform E))
		(else
		 (debug-print-condition "wrong:" E)
		 E))
	(eval '(let ()
		 (define-mixin <alpha11>
		   (fields A))
		 (define-class <beta11>
		   (fields A)
		   (mixins <alpha11>)))
	      (environment '(nausicaa))))
    => 'A)

  (check	;duplicated field names in class and composite mixin
      (guard (E ((syntax-violation? E)
		 (debug-print-condition "good:" E)
		 (syntax-violation-subform E))
		(else
		 (debug-print-condition "wrong:" E)
		 E))
	(eval '(let ()
		 (define-mixin <alpha12>
		   (fields A))
		 (define-mixin <beta12>
		   (fields B)
		   (mixins <alpha12>))
		 (define-class <delta12>
		   (fields A)
		   (mixins <beta12>)))
	      (environment '(nausicaa))))
    => 'A)

  (check	;recursive mixin composition
      (guard (E ((syntax-violation? E)
		 (debug-print-condition "good:" E)
		 (syntax-violation-subform E))
		(else
		 (debug-print-condition "wrong:" E)
		 E))
	(eval '(let ()
		 (define-mixin <beta13>
		   (fields A)
		   (mixins <beta13>)))
	      (environment '(nausicaa))))
    => '<beta13>)

;;; --------------------------------------------------------------------

  (check	;same mixin multiple selection in class
      (guard (E ((syntax-violation? E)
		 (debug-print-condition "good:" E)
		 (syntax-violation-subform E))
		(else
		 (debug-print-condition "wrong:" E)
		 E))
	(eval '(let ()
		 (define-mixin <beta14>
		   (fields A))
		 (define-class <alpha14>
		   (mixins <beta14> <beta14>)))
	      (environment '(nausicaa))))
    => '<beta14>)

  (check	;same mixin multiple selection in label
      (guard (E ((syntax-violation? E)
		 (debug-print-condition "good:" E)
		 (syntax-violation-subform E))
		(else
		 (debug-print-condition "wrong:" E)
		 E))
	(eval '(let ()
		 (define-mixin <beta15>
		   (fields A))
		 (define-label <alpha15>
		   (mixins <beta15> <beta15>)))
	      (environment '(nausicaa))))
    => '<beta15>)

  #t)


;;;; done

(check-report)

;;; end of file
