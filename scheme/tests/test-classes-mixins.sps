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
  (nausicaa checks))

(check-set-mode! 'report-failed)
(display "*** testing class mixins\n")


(parametrise ((check-test-name	'class))

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


;;;; done

(check-report)

;;; end of file
