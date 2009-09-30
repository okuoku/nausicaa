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
  (records)
  (keywords)
  (matches)
  (for (nos-lib) expand))

(check-set-mode! 'report-failed)
(display "*** testing NOS\n")


(parametrise ((check-test-name 'definition-experiments))

  (let ()
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

;;; --------------------------------------------------------------------

  (let ()

    (define-record-type <a>
      (fields (mutable alpha)
	      (immutable beta)))

    (define-keyword :alpha)
    (define-keyword :beta)

    (define (make-<a>* . options)
      (let-keywords options #f ((alpha	:alpha	#f)
				(beta	:beta	#f))
	(make-<a> alpha beta)))

    (check
	(let ((a (make-<a>* :alpha 1 :beta 2)))
	  (list (<a>-alpha a)
		(<a>-beta  a)))
      => '(1 2))

    #t)

  #t)


(parametrise ((check-test-name	'macros))

  (let ((a (make <alpha>
	     1 2 3))
	(b (make <beta>
	     1 2 3
	     4 5 6)))

    (check
	(is-a? a <alpha>)
      => #t)

    (check
	(with-record-fields (((a b c) <alpha> a))
	  (list a b c))
      => '(1 2 3))

    (check
	(is-a? b <beta>)
      => #t)

    (check
	(with-record-fields (((a b c d e f) <beta> b))
	  (list a b c d e f))
      => '(1 2 3 4 5 6))

    #f)

;;; --------------------------------------------------------------------

  (check-for-true	(is-a? 123 <fixnum>))
  (check-for-false	(is-a? #\a <fixnum>))

  (check-for-true	(is-a? 1 <integer>))
  (check-for-false	(is-a? 1.2 <integer>))

  (check-for-true	(is-a? 1/2 <rational>))
  (check-for-false	(is-a? 1+2i <rational>))

  (check-for-true	(is-a? 1.0 <integer-valued>))
  (check-for-false	(is-a? 1.1 <integer-valued>))

  (check-for-true	(is-a? 1/2 <rational-valued>))
  (check-for-false	(is-a? #\a <rational-valued>))

  (check-for-true	(is-a? 1.1 <flonum>))
  (check-for-false	(is-a? #\a <flonum>))

  (check-for-true	(is-a? 1.1 <real>))
  (check-for-false	(is-a? #\a <real>))

  (check-for-true	(is-a? 1.1 <real-valued>))
  (check-for-false	(is-a? #\a <real-valued>))

  (check-for-true	(is-a? 1.1+2i <complex>))
  (check-for-false	(is-a? #\a <complex>))

  (check-for-true	(is-a? 1 <number>))
  (check-for-false	(is-a? #\a <number>))

  (check-for-true	(is-a? #\a <char>))
  (check-for-false	(is-a? 1 <char>))

  (check-for-true	(is-a? "ciao" <string>))
  (check-for-false	(is-a? 123 <string>))

  (check-for-true	(is-a? '#(1 2 3) <vector>))
  (check-for-false	(is-a? "ciao" <vector>))

  (check-for-true	(is-a? '#vu8(1 2 3) <bytevector>))
  (check-for-false	(is-a? "ciao" <bytevector>))

  (check-for-true	(is-a? (make-eq-hashtable) <hashtable>))
  (check-for-false	(is-a? "ciao" <hashtable>))

  (check-for-true	(is-a? (open-string-input-port "ciao") <input-port>))
  (check-for-false	(is-a? 123 <input-port>))

  (check-for-true	(let-values (((port getter) (open-string-output-port)))
			  (is-a? port <output-port>)))
  (check-for-false	(is-a? 123 <output-port>))

  ;; (check-for-true	(is-a? <binary-port>))
  (check-for-false	(is-a? 123 <binary-port>))

  (check-for-true	(is-a? (open-string-input-port "ciao") <textual-port>))
  (check-for-false	(is-a? 123 <textual-port>))

  (check-for-true	(is-a? (open-string-input-port "ciao") <port>))
  (check-for-false	(is-a? 123 <port>))

  (check-for-true	(is-a? (make-message-condition "ciao") <condition>))
  (check-for-false	(is-a? 123 <condition>))

  (check-for-true	(is-a? (make <alpha> 1 2 3) <record>))
  (check-for-false	(is-a? 123 <record>))

  (check-for-true	(is-a? '(1 . 2) <pair>))
  (check-for-false	(is-a? 1 <pair>))

  (check-for-true	(is-a? '(1 2) <list>))
  (check-for-false	(is-a? '(1 . 2) <list>))

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

    (declare-method alpha ((o <one>))
      (<one>-a o))

    (declare-method alpha ((o <two>))
      (<two>-d o))

    (declare-method alpha ((o <three>))
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

    (declare-method alpha ((o <one>))
      (<one>-a o))

    (declare-method alpha ((o <one>))
      (<one>-b o))

    (let ((o (make-<two> 1 2 3 4 5 6)))
      (check (alpha o) => 2)
      (check (alpha o) => 2)) ;this exercises the cache
    #t)

;;; --------------------------------------------------------------------

  (let ()
    ;;Built in types.

    (define-generic alpha)

    (declare-method alpha ((o <fixnum>))		'<fixnum>)
    (declare-method alpha ((o <flonum>))		'<flonum>)
    (declare-method alpha ((o <integer>))	'<integer>)
    (declare-method alpha ((o <real>))		'<real>)
    (declare-method alpha ((o <complex>))	'<complex>)
    (declare-method alpha ((o <number>))		'<number>)

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

  (declare-method (alpha (o <one>))
    (<one>-a o))

  (declare-method alpha ((o <two>))
    (cons (<two>-d o)
	  (call-next-method)))

  (declare-method alpha ((o <three>))
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
    (declare-method (alpha (p <1>) (q <2>) (r <3>)) 1)
    (declare-method (alpha (p <a>) (q <b>) (r <c>)) 2)
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
    (declare-method (alpha (p <1>) (q <2>) (r <3>)) 1)
    (declare-method (alpha (p <1>) (q <b>) (r <3>)) 2)
    (declare-method (alpha (p <a>) (q <b>) (r <c>)) 3)
    (check (alpha n1 n2 n3) => 1)
    (check (alpha  a n2 n3) => 3)
    (check (alpha n1  b n3) => 2)
    (check (alpha n1 n2  c) => 3)
    (check (alpha  a  b  c) => 3)
    )
  (let ()
    (define-generic alpha)
    (declare-method (alpha (p <1>) (q <2>) (r <3>)) 1)
    (declare-method (alpha (p <1>) (q <b>) (r <c>)) 2)
    (declare-method (alpha (p <a>) (q <b>) (r <c>)) 3)
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
    (declare-method (alpha (p <1>)) 123)
    (declare-method (alpha (p <1>)) 456)
    (check (alpha n1) => 456))
  (let ()
    (define-generic alpha)
    (declare-method (alpha (p <1>) . rest) 123)
    (declare-method (alpha (p <1>) . rest) 456)
    (check (alpha n1) => 456))
  (let ()
    (define-generic alpha)
    (declare-method (alpha (p <1>)) 123)
    (declare-method (alpha (p <1>) . rest) 456)
    (check (alpha n1) => 123)
    (check (alpha n1 10) => 456))
  (let ()
    (define-generic alpha)
    (declare-method (alpha (p <1>) . rest) 456)
    (declare-method (alpha (p <1>)) 123)
    (check (alpha n1) => 123)
    (check (alpha n1 10) => 456))

;;; --------------------------------------------------------------------
;;; Rest arguments.
  (let ()
    (define-generic alpha)
    (declare-method alpha ((p <1>))		   1)
    (declare-method alpha ((p <a>) . rest)	   2)
    (declare-method alpha ((p <1>) . rest)	   3)
    (declare-method alpha ((p <1>) (q <2>) . rest)  4)
    (declare-method alpha ((p <a>) (q <b>) (r <c>)) 5)
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
    (declare-method (alpha (p <1>))		   1)
    (declare-method (alpha (p <a>) . rest)	   2)
    (declare-method (alpha (p <1>) . rest)	   3)
    (declare-method (alpha (p <1>) (q <2>) . rest)  4)
    (declare-method (alpha (p <a>) (q <b>) (r <c>)) 5)
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


(parameterise ((check-test-name 'generic-merge))

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
    (define-generic beta)

    (declare-method (alpha (o <one>))
      'alpha-one)

    (declare-method (alpha (o <two>))
      'alpha-two)

    (declare-method (beta (o <three>))
      'beta-three)

    (let ()
      (define-generic/merge gamma alpha beta)

      (let ((a (make-<one> 1 10 100))
	    (b (make-<two> 0 0 0 2 20 200))
	    (c (make-<three> 0 0 0 0 0 0 3 30 300)))
	(check (gamma a) => 'alpha-one)
	(check (gamma b) => 'alpha-two)
	(check (gamma c) => 'beta-three)
	#t))
    #t)
  #t)


(parametrise ((check-test-name 'predefined))

  (define-record-type <alpha>
    (fields (immutable the-string)))

  (define-record-type <beta>
    (fields (immutable the-string)))

  (check
      (object->string 123)
    => "123")

  (let ((a (make-<alpha> "alpha"))
	(b (make-<beta>  "beta")))

    (declare-method (object->string (o <alpha>))
      (<alpha>-the-string o))

    (declare-method (object->string (o <beta>))
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
