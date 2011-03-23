;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for (generics)
;;;Date: Tue Nov 11, 2008
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
  (rnrs eval))

(check-set-mode! 'report-failed)
(display "*** testing starred generic functions\n")


(parameterise ((check-test-name 'errors))

  (check	;wrong num args in method definition
      (guard (E ((syntax-violation? E)
;;;(write (condition-message E))(newline)
		 (syntax-violation-form E))
		(else E))
	(eval '(let ()
		 (define-generic* a)
		 (define-method (a b) #f)
		 (define-method (a b c) #f)
                 #f)
	      (environment '(nausicaa))))
    => '((nausicaa:builtin:<top>) (nausicaa:builtin:<top>)))

  #t)


(parameterise ((check-test-name 'generic-simple-inheritance))

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

    (define-generic* alpha)

    (define-method alpha :primary ((o <one>))
      (<one>-a o))

    (define-method alpha :primary ((o <two>))
      (<two>-d o))

    (define-method alpha :primary ((o <three>))
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

    (define-generic* alpha)

    (define-method :primary alpha ((o <one>))
      (<one>-a o))

    (define-method :primary alpha ((o <one>))
      (<one>-b o))

    (let ((o (make-<two> 1 2 3 4 5 6)))
      (check (alpha o) => 2)
      (check (alpha o) => 2)) ;this exercises the cache
    #t)

;;; --------------------------------------------------------------------

  (let ()
    ;;Built in types.

    (define-generic* alpha)

    (define-method :primary alpha ((o <fixnum>))	'<fixnum>)
    (define-method :primary alpha ((o <flonum>))	'<flonum>)
    (define-method :primary alpha ((o <integer>))	'<integer>)
    (define-method :primary alpha ((o <real>))		'<real>)
    (define-method :primary alpha ((o <complex>))	'<complex>)
    (define-method :primary alpha ((o <number>))	'<number>)

    (check (alpha 12)		=> '<fixnum>)
    (check (alpha (expt 12 12)) => '<integer>)
    (check (alpha 2/3)		=> '<real>)
    (check (alpha 1.2+3.4i)	=> '<complex>)

    #t)

  #t)


(parameterise ((check-test-name 'generic-next-method))

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

  (define-generic* alpha)

  (define-method :primary (alpha (o <one>))
    (<one>-a o))

  (define-method :primary alpha ((o <two>))
    (cons (<two>-d o)
	  (call-next-method)))

  (define-method :primary alpha ((o <three>))
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
    (define-generic* alpha)
    (define-method :primary (alpha (p <1>) (q <2>) (r <3>)) 1)
    (define-method :primary (alpha (p <a>) (q <b>) (r <c>)) 2)
    (check (alpha n1 n2 n3) => 1)
    (check (alpha  a n2 n3) => 2)
    (check (alpha n1  b n3) => 2)
    (check (alpha n1 n2  c) => 2)
    (check (alpha  a  b  c) => 2)
    )

;;; --------------------------------------------------------------------
;;; Mixed levels specificity.
  (let ()
    (define-generic* alpha)
    (define-method :primary (alpha (p <1>) (q <2>) (r <3>)) 1)
    (define-method :primary (alpha (p <1>) (q <b>) (r <3>)) 2)
    (define-method :primary (alpha (p <a>) (q <b>) (r <c>)) 3)
    (check (alpha n1 n2 n3) => 1)
    (check (alpha  a n2 n3) => 3)
    (check (alpha n1  b n3) => 2)
    (check (alpha n1 n2  c) => 3)
    (check (alpha  a  b  c) => 3)
    )
  (let ()
    (define-generic* alpha)
    (define-method :primary (alpha (p <1>) (q <2>) (r <3>)) 1)
    (define-method :primary (alpha (p <1>) (q <b>) (r <c>)) 2)
    (define-method :primary (alpha (p <a>) (q <b>) (r <c>)) 3)
    (check (alpha n1 n2 n3) => 1)
    (check (alpha  a n2 n3) => 3)
    (check (alpha n1  b n3) => 2)
    (check (alpha n1 n2  c) => 2)
    (check (alpha  a  b  c) => 3)
    )

;;; --------------------------------------------------------------------
;;; Overwriting existing method.
  (let ()
    (define-generic* alpha)
    (define-method :primary (alpha (p <1>)) 123)
    (define-method :primary (alpha (p <1>)) 456)
    (check (alpha n1) => 456))

  #t)


(parameterise ((check-test-name 'generic-merge))

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

    (define-generic* alpha)
    (define-generic* beta)

    (define-method :primary (alpha (o <one>))
      'alpha-one)

    (define-method :primary (alpha (o <two>))
      'alpha-two)

    (define-method :primary (beta (o <three>))
      'beta-three)

    (let ()
      (define-generic* gamma
	(merge alpha beta))

      (let ((a (make-<one> 1 10 100))
	    (b (make-<two> 0 0 0 2 20 200))
	    (c (make-<three> 0 0 0 0 0 0 3 30 300)))
	(check (gamma a) => 'alpha-one)
	(check (gamma b) => 'alpha-two)
	(check (gamma c) => 'beta-three)
	#t))
    #t)
  #t)


(parameterise ((check-test-name 'protocol-errors))

  (define-class <a>
    (fields a))

  (define o (make <a> 1))

  (check	;:BEFORE method calls next method
      (let ()
	(define-generic* alpha)
	(define-method :primary alpha ((o <a>)) 1)
	(define-method :before  alpha ((o <a>)) (call-next-method))
	(guard (E ((assertion-violation? E)
;;;(write (condition-message E))(newline)
		   #t)
		  (else E))
	  (alpha o)))
    => #t)

  (check	;:AFTER method calls next method
      (let ()
	(define-generic* alpha)
	(define-method :primary alpha ((o <a>)) 1)
	(define-method :after   alpha ((o <a>)) (call-next-method))
	(guard (E ((assertion-violation? E)
;;;(write (condition-message E))(newline)
		   #t)
		  (else E))
	  (alpha o)))
    => #t)

  (check	;invoke next-method from :primary when none is available
      (let ()
	(define-generic* alpha)
	(define-method alpha ((o <a>)) (call-next-method))
	(guard (E ((assertion-violation? E)
;;;(write (condition-message E))(newline)
		   #t)
		  (else E))
	  (alpha o)))
    => #t)

  (check	;invoke next-method from :around when none is available
      (let ()
	(define-generic* alpha)
	(define-method :around alpha ((o <a>)) (call-next-method))
	(guard (E ((assertion-violation? E)
;;;(write (condition-message E))(newline)
		   #t)
		  (else E))
	  (alpha o)))
    => #t)

  (check	;invoke next-method from :primary when none is available
      (let ()
	(define-generic* alpha)
	(define-method :around  alpha ((o <a>)) (call-next-method))
	(define-method :primary alpha ((o <a>)) (call-next-method))
	(guard (E ((assertion-violation? E)
;;;(write (condition-message E))(newline)
		   #t)
		  (else E))
	  (alpha o)))
    => #t)

  #t)


(parameterise ((check-test-name 'protocol-around))

  (define-class <A>
    (fields a))

  (define-class <B>
    (inherit <A>)
    (fields b))

  (define A (make <A> 1))
  (define B (make <B> 1 2))

  (check	;call :AROUND method instead of :PRIMARY
      (with-result
       (define-generic* alpha)
       (define-method :around  alpha ((o <A>))
	 (add-result 'around-A))
       (define-method :primary alpha ((o <A>))
	 (add-result 'primary-A))
       (alpha A))
    => '(around-A (around-A)))

  (check	;consume all the :AROUND methods, avoid :PRIMARY
      (with-result
       (define-generic* alpha)
       (define-method alpha :around  ((o <B>))
	 (add-result 'around-B)
	 (call-next-method))
       (define-method alpha :around  ((o <A>))
	 (add-result 'around-A))
       (define-method alpha :primary ((o <B>))
	 (add-result 'primary-B))
       (alpha B))
    => '(around-A (around-B around-A)))

  (check	;call :PRIMARY method after the :AROUND methods have been consumed
      (with-result
       (define-generic* alpha)
       (define-method alpha :around  ((o <B>))
	 (add-result 'around-B)
	 (call-next-method))
       (define-method alpha :around  ((o <A>))
	 (add-result 'around-A)
	 (call-next-method))
       (define-method alpha :primary ((o <B>))
	 (add-result 'primary-B))
       (alpha B))
    => '(primary-B (around-B around-A primary-B)))

  (check	;consume all the :PRIMARY methods after the :AROUND methods have been consumed
      (with-result
       (define-generic* alpha)
       (define-method alpha :around  ((o <B>))
	 (add-result 'around-B)
	 (call-next-method))
       (define-method alpha :around  ((o <A>))
	 (add-result 'around-A)
	 (call-next-method))
       (define-method alpha :primary ((o <B>))
	 (add-result 'primary-B)
	 (call-next-method))
       (define-method alpha :primary ((o <A>))
	 (add-result 'primary-A))
       (alpha B))
    => '(primary-A (around-B around-A primary-B primary-A)))

  #t)


(parameterise ((check-test-name 'protocol-before))

  (define-class <A>
    (fields a))

  (define-class <B>
    (inherit <A>)
    (fields b))

  (define A (make <A> 1))
  (define B (make <B> 1 2))

  (check	;call :BEFORE and :PRIMARY
      (with-result
       (define-generic* alpha)
       (define-method :before  alpha ((o <A>))
	 (add-result 'before-A))
       (define-method :primary alpha ((o <A>))
	 (add-result 'primary-A))
       (alpha A))
    => '(primary-A (before-A primary-A)))

  (check	;call the :BEFORE methods, then :PRIMARY
      (with-result
       (define-generic* alpha)
       (define-method alpha :before  ((o <B>))
	 (add-result 'before-B))
       (define-method alpha :before  ((o <A>))
	 (add-result 'before-A))
       (define-method alpha :primary ((o <B>))
	 (add-result 'primary-B))
       (alpha B))
    => '(primary-B (before-B before-A primary-B)))

  (check	;consume all the :PRIMARY methods after the :BEFORE methods have been called
      (with-result
       (define-generic* alpha)
       (define-method alpha :before  ((o <B>))
	 (add-result 'before-B))
       (define-method alpha :before  ((o <A>))
	 (add-result 'before-A))
       (define-method alpha :primary ((o <B>))
	 (add-result 'primary-B)
	 (call-next-method))
       (define-method alpha :primary ((o <A>))
	 (add-result 'primary-A))
       (alpha B))
    => '(primary-A (before-B before-A primary-B primary-A)))

  #t)


(parameterise ((check-test-name 'protocol-after))

  (define-class <A>
    (fields a))

  (define-class <B>
    (inherit <A>)
    (fields b))

  (define A (make <A> 1))
  (define B (make <B> 1 2))

  (check	;call :AFTER and :PRIMARY
      (with-result
       (define-generic* alpha)
       (define-method :after  alpha ((o <A>))
	 (add-result 'after-A))
       (define-method :primary alpha ((o <A>))
	 (add-result 'primary-A))
       (alpha A))
    => '(primary-A (primary-A after-A)))

  (check	;call the :PRIMARY method, then the :AFTER ones
      (with-result
       (define-generic* alpha)
       (define-method alpha :after  ((o <B>))
	 (add-result 'after-B))
       (define-method alpha :after  ((o <A>))
	 (add-result 'after-A))
       (define-method alpha :primary ((o <B>))
	 (add-result 'primary-B))
       (alpha B))
    => '(primary-B (primary-B after-A after-B)))

  (check	;consume all the :PRIMARY methods the call the :AFTER methods
      (with-result
       (define-generic* alpha)
       (define-method alpha :after  ((o <B>))
	 (add-result 'after-B))
       (define-method alpha :after  ((o <A>))
	 (add-result 'after-A))
       (define-method alpha :primary ((o <B>))
	 (add-result 'primary-B)
	 (call-next-method))
       (define-method alpha :primary ((o <A>))
	 (add-result 'primary-A))
       (alpha B))
    => '(primary-A (primary-B primary-A after-A after-B)))

  #t)


(parameterise ((check-test-name 'protocol-full))

  (define-class <A>
    (fields a))

  (define-class <B>
    (inherit <A>)
    (fields b))

  (define A (make <A> 1))
  (define B (make <B> 1 2))

  (check
      (with-result
       (define-generic* alpha)
       (define-method :before  alpha ((o <A>))
	 (add-result 'before-A))
       (define-method :after  alpha ((o <A>))
	 (add-result 'after-A))
       (define-method :around  alpha ((o <A>))
	 (add-result 'around-A)
	 (call-next-method))
       (define-method :primary alpha ((o <A>))
	 (add-result 'primary-A))
       (alpha A))
    => '(primary-A (around-A before-A primary-A after-A)))

  (check
      (with-result
       (define-generic* alpha)
       (define-method :before  alpha ((o <A>))
	 (add-result 'before-A))
       (define-method :after  alpha ((o <A>))
	 (add-result 'after-A))
       (define-method :around  alpha ((o <A>))
	 (add-result 'around-A)
	 (call-next-method))
       (define-method :primary alpha ((o <A>))
	 (add-result 'primary-A))
       (define-method :before  alpha ((o <B>))
	 (add-result 'before-B))
       (define-method :after  alpha ((o <B>))
	 (add-result 'after-B))
       (define-method :around  alpha ((o <B>))
	 (add-result 'around-B)
	 (call-next-method))
       (define-method :primary alpha ((o <B>))
	 (add-result 'primary-B)
	 (call-next-method))
       (alpha B))
    => '(primary-A (around-B around-A before-B before-A primary-B primary-A after-A after-B)))

  #t)


;;;; done

(check-report)

;;; end of file
