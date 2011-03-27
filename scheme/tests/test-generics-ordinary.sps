;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: usage tests for ordinary generic functions
;;;Date: Mon Jul  5, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008-2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (nausicaa generics object-to-string)
  (rnrs eval))

(check-set-mode! 'report-failed)
(display "*** testing ordinary generic functions\n")


(parametrise ((check-test-name 'errors))

  (check
      (guard (E ((syntax-violation? E)
;;;(write (condition-message E))(newline)
		 (syntax-violation-subform E))
		(else E))
	(eval '(let ()
		 (define-generic a (b))
		 (define-method (a b) #f)
		 (define-method (a b c) #f)
                 #f)
	      (environment '(nausicaa))))
    => 'a)

  (check
      (guard (E ((syntax-violation? E)
;;;(write (condition-message E))(newline)
		 (syntax-violation-subform E))
		(else E))
	(eval '(let ()
		 (define-generic a (p))
		 (define-generic b (p q))
		 (define-generic c (p)
		   (merge a b))
                 #f)
	      (environment '(nausicaa))))
    => 'b)

  #t)


(parametrise ((check-test-name 'simple-inheritance))

  (let ()
    (define-class <one>
      (fields (mutable a)
	      (mutable b)
	      (mutable c)))
    (define-class <two>
      (inherit <one>)
      (fields (mutable d)
	      (mutable e)
	      (mutable f)))
    (define-class <three>
      (inherit <two>)
      (fields (mutable g)
	      (mutable h)
	      (mutable i)))

    (define-generic alpha (o))

    (define-method alpha ((o <one>))
      (<one>-a o))

    (define-method alpha ((o <two>))
      (<two>-d o))

    (define-method alpha ((o <three>))
      (<three>-g o))

    (let ((a (make-<one> 1 10 100))
    	  (b (make-<two> 0 0 0 2 20 200))
    	  (c (make-<three> 0 0 0 0 0 0 3 30 300)))
      (check (alpha a) => 1)
      (check (alpha b) => 2)
      (check (alpha c) => 3)
      #t)
    #t)

;;; --------------------------------------------------------------------

  (let ()
    ;;This tests overwriting an existing method function.

    (define-class <one>
      (fields (mutable a)
	      (mutable b)
	      (mutable c)))
    (define-class <two>
      (inherit <one>)
      (fields (mutable d)
	      (mutable e)
	      (mutable f)))

    (define-generic alpha (o))

    (define-method alpha ((o <one>))
      (<one>-a o))

    (define-method alpha ((o <one>))
      (<one>-b o))

    (let ((o (make-<two> 1 2 3 4 5 6)))
      (check (alpha o) => 2)
      (check (alpha o) => 2)) ;this exercises the cache
    #t)

;;; --------------------------------------------------------------------

  (let ()
    ;;Built in types.

    (define-generic alpha (o))

    (define-method alpha ((o <fixnum>))		'<fixnum>)
    (define-method alpha ((o <flonum>))		'<flonum>)
    (define-method alpha ((o <integer>))	'<integer>)
    (define-method alpha ((o <real>))		'<real>)
    (define-method alpha ((o <complex>))	'<complex>)
    (define-method alpha ((o <number>))		'<number>)

    (check (alpha 12)		=> '<fixnum>)
    (check (alpha (expt 12 12)) => '<integer>)
    (check (alpha 2/3)		=> '<real>)
    (check (alpha 1.2+3.4i)	=> '<complex>)

    #t)

  #t)


(parametrise ((check-test-name 'next-method))

  (define-class <one>
    (fields (mutable a)
	    (mutable b)
	    (mutable c)))
  (define-class <two>
    (inherit <one>)
    (fields (mutable d)
	    (mutable e)
	    (mutable f)))
  (define-class <three>
    (inherit <two>)
    (fields (mutable g)
	    (mutable h)
	    (mutable i)))

  (define-generic alpha (o))

  (define-method (alpha (o <one>))
    (<one>-a o))

  (define-method alpha ((o <two>))
    (cons (<two>-d o)
	  (call-next-method)))

  (define-method alpha ((o <three>))
    (cons (<three>-g o)
	  (call-next-method)))

  (let ((a (make-<one> 1 2 3))
	(b (make-<two> 2.1 2.2 2.3 2.4 2.5 2.6))
	(c (make-<three> 3.1 3.2 3.3 3.4 3.5 3.6 3.7 3.8 3.9)))

    (check (alpha a) => 1)
    (check (alpha b) => '(2.4 . 2.1))
    (check (alpha c) => '(3.7 3.4 . 3.1))

    #t)

  #t)


(parametrise ((check-test-name 'specificity))

  (define-class <a>
    (fields (mutable a)))
  (define-class <b>
    (fields (mutable b)))
  (define-class <c>
    (fields (mutable c)))
  (define-class <d>
    (fields (mutable d)))

  (define-class <1>
    (inherit <a>))
  (define-class <2>
    (inherit <b>))
  (define-class <3>
    (inherit <c>))
  (define-class <4>
    (inherit <d>))

  (define a (make-<a> 1))
  (define b (make-<b> 2))
  (define c (make-<c> 3))
  (define d (make-<d> 4))

  (define n1 (make-<1> 1))
  (define n2 (make-<2> 2))
  (define n3 (make-<3> 3))
  (define n4 (make-<4> 4))

;;; --------------------------------------------------------------------
;;; Two levels specificity.
  (let ()
    (define-generic alpha (p q r))
    (define-method (alpha (p <1>) (q <2>) (r <3>)) 1)
    (define-method (alpha (p <a>) (q <b>) (r <c>)) 2)
    (check (alpha n1 n2 n3) => 1)
    (check (alpha  a n2 n3) => 2)
    (check (alpha n1  b n3) => 2)
    (check (alpha n1 n2  c) => 2)
    (check (alpha  a  b  c) => 2)
    )

;;; --------------------------------------------------------------------
;;; Mixed levels specificity.
  (let ()
    (define-generic alpha (p q r))
    (define-method (alpha (p <1>) (q <2>) (r <3>)) 1)
    (define-method (alpha (p <1>) (q <b>) (r <3>)) 2)
    (define-method (alpha (p <a>) (q <b>) (r <c>)) 3)
    (check (alpha n1 n2 n3) => 1)
    (check (alpha  a n2 n3) => 3)
    (check (alpha n1  b n3) => 2)
    (check (alpha n1 n2  c) => 3)
    (check (alpha  a  b  c) => 3)
    )
  (let ()
    (define-generic alpha (p q r))
    (define-method (alpha (p <1>) (q <2>) (r <3>)) 1)
    (define-method (alpha (p <1>) (q <b>) (r <c>)) 2)
    (define-method (alpha (p <a>) (q <b>) (r <c>)) 3)
    (check (alpha n1 n2 n3) => 1)
    (check (alpha  a n2 n3) => 3)
    (check (alpha n1  b n3) => 2)
    (check (alpha n1 n2  c) => 2)
    (check (alpha  a  b  c) => 3)
    )

;;; --------------------------------------------------------------------
;;; Overwriting existing method.
  (let ()
    (define-generic alpha (p))
    (define-method (alpha (p <1>)) 123)
    (define-method (alpha (p <1>)) 456)
    (check (alpha n1) => 456))

  #t)


(parametrise ((check-test-name 'merge))

  (let ()	;merge without signature conflict
    (define-class <one>
      (fields (mutable a)
	      (mutable b)
	      (mutable c)))
    (define-class <two>
      (inherit <one>)
      (fields (mutable d)
	      (mutable e)
	      (mutable f)))
    (define-class <three>
      (inherit <two>)
      (fields (mutable g)
	      (mutable h)
	      (mutable i)))

    (define-generic alpha (o))
    (define-generic beta  (o))

    (define-method (alpha (o <one>))
      'alpha-one)

    (define-method (alpha (o <two>))
      'alpha-two)

    (define-method (beta (o <three>))
      'beta-three)

    (define-generic gamma (o)
      (merge alpha beta))

    (define a (make-<one> 1 10 100))
    (define b (make-<two> 0 0 0 2 20 200))
    (define c (make-<three> 0 0 0 0 0 0 3 30 300))

    (check (gamma a) => 'alpha-one)
    (check (gamma b) => 'alpha-two)
    (check (gamma c) => 'beta-three)

    #t)

;;; --------------------------------------------------------------------

  (let ()	;merge with signature conflict
    (define-class <one>
      (fields (mutable a)
	      (mutable b)
	      (mutable c)))
    (define-class <two>
      (inherit <one>)
      (fields (mutable d)
	      (mutable e)
	      (mutable f)))
    (define-class <three>
      (inherit <two>)
      (fields (mutable g)
	      (mutable h)
	      (mutable i)))

    (define-generic alpha (o))
    (define-generic beta  (o))

    (define-method (alpha (o <one>))
      'alpha-one)

    (define-method (alpha (o <two>))
      'alpha-two)

    (define-method (beta (o <three>))
      'beta-three)

    (define-method (beta (o <two>))	;this is discarded when merging
      'beta-two)

    (define-generic gamma (o)
      (merge alpha beta))

    (define a (make-<one> 1 10 100))
    (define b (make-<two> 0 0 0 2 20 200))
    (define c (make-<three> 0 0 0 0 0 0 3 30 300))

    (check (gamma a) => 'alpha-one)
    (check (gamma b) => 'alpha-two)
    (check (gamma c) => 'beta-three)

    #t)

  #t)


(parametrise ((check-test-name 'predefined))

  (define-class <alpha>
    (fields (immutable the-string)))

  (define-class <beta>
    (fields (immutable the-string)))

  (check
      (object->string 123)
    => "123")

  (let ((a (make-<alpha> "alpha"))
	(b (make-<beta>  "beta")))

    (define-method (object->string (o <alpha>))
      (<alpha>-the-string o))

    (define-method object->string ((o <beta>))
      (<beta>-the-string o))

    (check
	(object->string a)
      => "alpha")

    (check
	(object->string b)
      => "beta")

    #f)

  #t)


;;;; done

(check-report)

;;; end of file
