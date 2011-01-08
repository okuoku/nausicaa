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

    (define-mixin <stuff>
      (fields c)
      (method (doit (o <stuff>))
	(+ o.a o.c)))

    (define-class <alpha>
      (fields a)
      (mixins <stuff>))

    (check
	(let-make ((o <alpha> 1 2))
	  (o.doit))
      => 3)

    #f)

  #t)


(parametrise ((check-test-name	'label))

  (define-mixin <stuff>
    (virtual-fields (immutable c car))
    (method (doit (o <stuff>))
      (+ 1 o.c)))

  (define-label <one>
    (virtual-fields (immutable p cdr))
    (mixins <stuff>))

  (define-label <two>
    (virtual-fields (immutable q cdr))
    (mixins <stuff>))

  (check
      (let (((o <one>) '(10 . 20)))
        (o.doit))
    => 11)

  (check
      (let (((o <two>) '(10 . 20)))
        (o.doit))
    => 11)

  #t)


(parametrise ((check-test-name	'multiple-mixins))

  (define-mixin <stuff>
    (fields c)
    (method (doit (o <stuff>))
      (+ 1 o.c)))

  (define-mixin <other-stuff>
    (fields p))

  (define-class <base>
    (fields a))

  (define-class <one>
    (inherit <base>)
    (maker ()
	   (a:	#f)
	   (p:	#f)
	   (c:	#f))
    (mixins <stuff> <other-stuff>))

  (check
      (let-make ((o <one>
		    (a: 10)
		    (p: 20)
		    (c: 30)))
        (o.doit))
    => 31)

  (check
      (let-make ((o <one>
		    (a: 10)
		    (p: 20)
		    (c: 30)))
        (list o.a o.p o.c))
    => '(10 20 30))

  #t)


(parametrise ((check-test-name	'composite-mixins))

  (define-mixin <stuff>
    (fields c)
    (method (doit (o <stuff>))
      (+ 1 o.c)))

  (define-mixin <other-stuff>
    (fields p)
    (mixins <stuff>))

  (define-class <base>
    (fields a))

  (define-class <one>
    (inherit <base>)
    (maker ()
	   (a:	#f)
	   (p:	#f)
	   (c:	#f))
    (mixins <other-stuff>))

  (check
      (let-make ((o <one>
		    (a: 10)
		    (p: 20)
		    (c: 30)))
        (o.doit))
    => 31)

  (check
      (let-make ((o <one>
		    (a: 10)
		    (p: 20)
		    (c: 30)))
        (list o.a o.p o.c))
    => '(10 20 30))

  #t)


(parametrise ((check-test-name	'errors)
	      (debugging	#f))

  (check	;empty mixin definition
      (guard (E ((syntax-violation? E)
		 (debug-print-condition "good:" E)
		 (syntax-violation-subform E))
		(else
		 (debug-print-condition "wrong:" E)
		 E))
	(eval '(let ()
		 (define-mixin <alpha>))
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
		 (define-mixin <alpha>
		   (fields A))
		 (define-class <beta>
		   (fields A)
		   (mixins <alpha>)))
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
		 (define-mixin <alpha>
		   (fields A))
		 (define-mixin <beta>
		   (fields B)
		   (mixins <alpha>))
		 (define-class <delta>
		   (fields A)
		   (mixins <beta>)))
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
		 (define-mixin unique.<beta>
		   (fields A)
		   (mixins unique.<beta>)))
	      (environment '(nausicaa))))
    => 'unique.<beta>)

;;; --------------------------------------------------------------------

  (check	;same mixin multiple selection in class
      (guard (E ((syntax-violation? E)
		 (debug-print-condition "good:" E)
		 (syntax-violation-subform E))
		(else
		 (debug-print-condition "wrong:" E)
		 E))
	(eval '(let ()
		 (define-mixin <beta>
		   (fields A))
		 (define-class <alpha>
		   (mixins <beta> <beta>)))
	      (environment '(nausicaa))))
    => '<beta>)

  (check	;same mixin multiple selection in label
      (guard (E ((syntax-violation? E)
		 (debug-print-condition "good:" E)
		 (syntax-violation-subform E))
		(else
		 (debug-print-condition "wrong:" E)
		 E))
	(eval '(let ()
		 (define-mixin <beta>
		   (fields A))
		 (define-label <alpha>
		   (mixins <beta> <beta>)))
	      (environment '(nausicaa))))
    => '<beta>)

  #t)


;;;; done

(check-report)

;;; end of file
