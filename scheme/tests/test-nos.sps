;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: usage tests for (nos)
;;;Date: Wed Aug 26, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008, 2009 Marco Maggi <marcomaggi@gna.org>
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


(import (nausicaa)
  (lists)
  (checks)
  (nos)
  (matches))

(check-set-mode! 'report-failed)
(display "*** testing NOS\n")


(parametrise ((check-test-name 'definition))

  (define-record-type <a>
    (fields (mutable	alpha)
	    (immutable	beta))
    (protocol (lambda (maker)
		(lambda args
		  (let ((alpha #f)
			(beta  #f))
		    (for-each (match-lambda
			       (('alpha x) (set! alpha x))
			       (('beta  x) (set! beta  x)))
		      args)
		    (maker alpha beta))))))

  (check
      (let ((a (make-<a> '(alpha 1) '(beta  2))))
	(list (<a>-alpha a)
	      (<a>-beta  a)))
    => '(1 2))

  #t)


(parameterise ((check-test-name 'generic-simple-inheritance))

  (let ()
    (define-record-type <one>
      (fields (mutable a)
	      (mutable b)
	      (mutable c)))
    (define-record-type <two>
      (parent <one>)
      (fields (mutable d)
	      (mutable e)
	      (mutable f)))
    (define-record-type <three>
      (parent <two>)
      (fields (mutable g)
	      (mutable h)
	      (mutable i)))

    (define-generic alpha)

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

    (define-record-type <one>
      (fields (mutable a)
	      (mutable b)
	      (mutable c)))
    (define-record-type <two>
      (parent <one>)
      (fields (mutable d)
	      (mutable e)
	      (mutable f)))

    (define-generic alpha)

    (define-method alpha ((o <one>))
      (<one>-a o))

    (define-method alpha ((o <one>))
      (<one>-b o))

    (let ((o (make-<two> 1 2 3 4 5 6)))
      (check (alpha o) => 2))
    #t)

;;; --------------------------------------------------------------------

  (let ()
    ;;Built in types.

    (define-generic alpha)

    (define-method alpha ((o <fixnum>))		'<fixnum>)
    (define-method alpha ((o <flonum>))		'<flonum>)
    (define-method alpha ((o <integer>))	'<integer>)
    (define-method alpha ((o <real>))		'<real>)
    (define-method alpha ((o <complex>))	'<complex>)
    (define-method alpha ((o <number>))		'<number>)

    ;;Here remember  that we are using  the methods above,  we are *not*
    ;;applying CLASS-OF.
    (check (alpha 12)		=> '<fixnum>)
    (check (alpha (expt 12 12)) => '<integer>)
    (check (alpha 2/3)		=> '<real>)
    (check (alpha 1.2+3.4i)	=> '<complex>)

    #t)

  #t)


(parameterise ((check-test-name 'generic-next-method))

  (define-record-type <one>
    (fields (mutable a)
	    (mutable b)
	    (mutable c)))
  (define-record-type <two>
    (parent <one>)
    (fields (mutable d)
	    (mutable e)
	    (mutable f)))
  (define-record-type <three>
    (parent <two>)
    (fields (mutable g)
	    (mutable h)
	    (mutable i)))

  (define-generic alpha)

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


(parameterise ((check-test-name 'generic-specificity))

  (define-record-type <a>
    (fields (mutable a)))
  (define-record-type <b>
    (fields (mutable b)))
  (define-record-type <c>
    (fields (mutable c)))
  (define-record-type <d>
    (fields (mutable d)))

  (define-record-type <1>
    (parent <a>))
  (define-record-type <2>
    (parent <b>))
  (define-record-type <3>
    (parent <c>))
  (define-record-type <4>
    (parent <d>))

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
    (define-generic alpha)
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
    (define-generic alpha)
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
    (define-generic alpha)
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
    (define-generic alpha)
    (define-method (alpha (p <1>)) 123)
    (define-method (alpha (p <1>)) 456)
    (check (alpha n1) => 456))
  (let ()
    (define-generic alpha)
    (define-method (alpha (p <1>) . rest) 123)
    (define-method (alpha (p <1>) . rest) 456)
    (check (alpha n1) => 456))
  (let ()
    (define-generic alpha)
    (define-method (alpha (p <1>)) 123)
    (define-method (alpha (p <1>) . rest) 456)
    (check (alpha n1) => 123)
    (check (alpha n1 10) => 456))
  (let ()
    (define-generic alpha)
    (define-method (alpha (p <1>) . rest) 456)
    (define-method (alpha (p <1>)) 123)
    (check (alpha n1) => 123)
    (check (alpha n1 10) => 456))

;;; --------------------------------------------------------------------
;;; Rest arguments.
  (let ()
    (define-generic alpha)
    (define-method alpha ((p <1>))		   1)
    (define-method alpha ((p <a>) . rest)	   2)
    (define-method alpha ((p <1>) . rest)	   3)
    (define-method alpha ((p <1>) (q <2>) . rest)  4)
    (define-method alpha ((p <a>) (q <b>) (r <c>)) 5)
    (check (alpha n1 n2 n3) => 5)
    (check (alpha  a n2 n3) => 5)
    (check (alpha n1  b n3) => 5)
    (check (alpha n1 n2  c) => 5)
    (check (alpha  a  b  c) => 5)
    (check (alpha n1 n2)    => 4)
    (check (alpha n1 n2  9) => 4)
    (check (alpha  a)       => 2)
    (check (alpha  a 123)   => 2)
    (check (alpha  a 123 4) => 2)
    (check (alpha n1)       => 1)
    (check (alpha n1 123)   => 3)
    (check (alpha n1 123 4) => 3)
    #f)
  (let ()
    (define-generic alpha)
    (define-method (alpha (p <1>))		   1)
    (define-method (alpha (p <a>) . rest)	   2)
    (define-method (alpha (p <1>) . rest)	   3)
    (define-method (alpha (p <1>) (q <2>) . rest)  4)
    (define-method (alpha (p <a>) (q <b>) (r <c>)) 5)
    (check (alpha n1 n2 n3) => 5)
    (check (alpha  a n2 n3) => 5)
    (check (alpha n1  b n3) => 5)
    (check (alpha n1 n2  c) => 5)
    (check (alpha  a  b  c) => 5)
    (check (alpha n1 n2)    => 4)
    (check (alpha n1 n2  9) => 4)
    (check (alpha  a)       => 2)
    (check (alpha  a 123)   => 2)
    (check (alpha  a 123 4) => 2)
    (check (alpha n1)       => 1)
    (check (alpha n1 123)   => 3)
    (check (alpha n1 123 4) => 3)
    #f)

  #t)


;;;; done

(check-report)

;;; end of file
