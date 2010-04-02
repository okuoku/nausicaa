;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for (records basics)
;;;Date: Thu Apr  1, 2010
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


(import (nausicaa)
  (rnrs eval)
  (checks)
  (records-lib)
  (classes))

(check-set-mode! 'report-failed)
(display "*** testing classes basics\n")


(parametrise ((check-test-name	'definition))

  (let ()

    (define-class <alpha>
      (fields (mutable a)))

    (check
	(let ((o (make-<alpha> 123)))
	  (<alpha>-a o))
      => 123)

    #f)

;;; --------------------------------------------------------------------
;;; all the auxiliary syntaxes

  (let ()

    (define-class <alpha>
      (fields (mutable a)))

    (define-class <beta>
      (parent <alpha>)
      (protocol (lambda (alpha-maker)
      		  (lambda (a b)
      		    (let ((beta-maker (alpha-maker a)))
		      (beta-maker b)))))
      (sealed #t)
      (opaque #t)
      (nongenerative test:beta)
      (fields (immutable b)))

    (check
    	(let ((o (make-<beta> 1 2)))
    	  (list (<alpha>-a o)
    		(<beta>-b o)
		))
      => '(1 2))

    #f)

  (let ()

    (define-class <alpha>
      (fields (mutable a)))

    (define-class <beta>
      (parent-rtd (record-type-descriptor <alpha>)
		  (record-constructor-descriptor <alpha>))
      (fields (immutable b)))

    (check
    	(let ((o (make-<beta> 1 2)))
    	  (list (<alpha>-a o)
    		(<beta>-b o)
		))
      => '(1 2))

    #f)

;;; --------------------------------------------------------------------
;;; various field definitions

  (let ()

    (define-class <alpha>
      (fields (mutable a)
	      (immutable b)
	      c))

    (check
	(let ((o (make-<alpha> 1 2 3)))
	  (list (<alpha>-a o)
		(<alpha>-b o)
		(<alpha>-c o)))
      => '(1 2 3))

    (check
	(let ((o (make-<alpha> 1 2 3)))
	  (<alpha>-a-set! o 10)
	  (list (<alpha>-a o)
		(<alpha>-b o)
		(<alpha>-c o)))
      => '(10 2 3))

    #f)

  (check
      (eval '(letrec ()
  	       (define-class <alpha>
  		 (fields (mutable a)
  			 (immutable b)
  			 c))
  	       (define o (make-<alpha> 1 2 3))
  	       (with-fields ((<alpha> o))
  		 (set! o.a #t)
  		 o.a))
  	    (environment '(nausicaa) '(classes)))
    => #t)

  (check
      (guard (E ((syntax-violation? E) #t)
  		(else #f))
  	(eval '(letrec ()
  		 (define-class <alpha>
  		   (fields (mutable a)
  			   (immutable b)
  			   c))
  		 (define o (make-<alpha> 1 2 3))
  		 (with-fields ((<alpha> o))
  		   (set! o.b #f)))
  	      (environment '(nausicaa) '(classes))))
    => #t)

  (check
      (guard (E ((syntax-violation? E) #t)
  		(else #f))
  	(eval '(letrec ()
  		 (define-class <alpha>
  		   (fields (mutable a)
  			   (immutable b)
  			   c))
  		 (define o (make-<alpha> 1 2 3))
  		 (with-fields ((<alpha> o))
  		   (set! o.c #f)))
  	      (environment '(nausicaa) '(classes))))
    => #t)

;;; --------------------------------------------------------------------
;;; accessor and mutator names

  (let ()

    (define-class <alpha>
      (fields (mutable a access-a mutate-a)))

    (check
	(let ((o (make-<alpha> 123)))
	  (access-a o))
      => 123)

    (check
	(let ((o (make-<alpha> 123)))
	  (mutate-a o 456)
	  (access-a o))
      => 456)

    #f)

  (let ()

    (define-class <alpha>
      (fields (immutable a access-a)))

    (check
	(let ((o (make-<alpha> 123)))
	  (access-a o))
      => 123)

    #f)

  #t)


(parametrise ((check-test-name	'definitions-virtual-fields))

  (let ()	;immutable virtual fields

    (define-class <fraction>
      (fields (mutable number))
      (virtual-fields (immutable numerator)
		      (immutable denominator)))

    (define (<fraction>-numerator o)
      (numerator (<fraction>-number o)))

    (define (<fraction>-denominator o)
      (denominator (<fraction>-number o)))

    (check
	(let ((o (make-<fraction> 2/3)))
	  (<fraction>-numerator o))
      => 2)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (<fraction>-denominator o))
      => 3)

    #f)

;;; --------------------------------------------------------------------

  (let ()	;mutable virtual fields

    (define-class <fraction>
      (fields (mutable number))
      (virtual-fields (mutable numerator)
		      (mutable denominator)))

    (define (<fraction>-numerator o)
      (numerator (<fraction>-number o)))

    (define (<fraction>-numerator-set! o v)
      (let ((n (<fraction>-number o)))
	(<fraction>-number-set! o (/ v (denominator n)))))

    (define (<fraction>-denominator o)
      (denominator (<fraction>-number o)))

    (define (<fraction>-denominator-set! o v)
      (let ((n (<fraction>-number o)))
	(<fraction>-number-set! o (/ (numerator n) v))))

    (check
	(let ((o (make-<fraction> 2/3)))
	  (<fraction>-numerator o))
      => 2)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (<fraction>-denominator o))
      => 3)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (<fraction>-numerator-set! o 5)
	  (<fraction>-number o))
      => 5/3)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (<fraction>-denominator-set! o 5)
	  (<fraction>-number o))
      => 2/5)

    #f)

;;; --------------------------------------------------------------------

  (let ()	;explicitly named immutable virtual fields accessor

    (define-class <fraction>
      (fields (mutable number))
      (virtual-fields (immutable numerator the-numerator)
		      (immutable denominator the-denominator)))

    (define (the-numerator o)
      (numerator (<fraction>-number o)))

    (define (the-denominator o)
      (denominator (<fraction>-number o)))

    (check
	(let ((o (make-<fraction> 2/3)))
	  (the-numerator o))
      => 2)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (the-denominator o))
      => 3)

    #f)

;;; --------------------------------------------------------------------

  (let ()	;explicitly named virtual fields accessor and mutator

    (define-class <fraction>
      (fields (mutable number))
      (virtual-fields (mutable numerator the-numerator the-numerator-set!)
		      (mutable denominator the-denominator the-denominator-set!)))

    (define (the-numerator o)
      (numerator (<fraction>-number o)))

    (define (the-numerator-set! o v)
      (let ((n (<fraction>-number o)))
	(<fraction>-number-set! o (/ v (denominator n)))))

    (define (the-denominator o)
      (denominator (<fraction>-number o)))

    (define (the-denominator-set! o v)
      (let ((n (<fraction>-number o)))
	(<fraction>-number-set! o (/ (numerator n) v))))

    (check
	(let ((o (make-<fraction> 2/3)))
	  (the-numerator o))
      => 2)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (the-denominator o))
      => 3)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (the-numerator-set! o 5)
	  (<fraction>-number o))
      => 5/3)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (the-denominator-set! o 5)
	  (<fraction>-number o))
      => 2/5)

    #f)

;;; --------------------------------------------------------------------

  (let ()	;explicitly named virtual fields accessor and mutator
		;mixed mutable and immutable

    (define-class <fraction>
      (fields (mutable number))
      (virtual-fields (immutable numerator the-numerator)
		      (mutable denominator the-denominator the-denominator-set!)))

    (define (the-numerator o)
      (numerator (<fraction>-number o)))

    (define (the-denominator o)
      (denominator (<fraction>-number o)))

    (define (the-denominator-set! o v)
      (let ((n (<fraction>-number o)))
	(<fraction>-number-set! o (/ (numerator n) v))))

    (check
	(let ((o (make-<fraction> 2/3)))
	  (the-numerator o))
      => 2)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (the-denominator o))
      => 3)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (the-denominator-set! o 5)
	  (<fraction>-number o))
      => 2/5)

    #f)

  #t)


(parametrise ((check-test-name	'with-fields))

  (let ()	;automatic generation of FIELDS-ACCESSOR name

    (define-class <alpha>
      (fields (mutable a)))

    (define r (make-<alpha> 123))
    (define s (make-<alpha> #\a))
    (define t (make-<alpha> 1.0))

    (with-fields ((<alpha> r)
    		  (<alpha> s)
    		  (<alpha> t))
      (check
      	  (list r.a s.a t.a)
      	=> '(123 #\a 1.0))

      (set! r.a 456)
      (set! s.a #\b)
      (set! t.a 2.0)

      (check
	  (list r.a s.a t.a)
	=> '(456 #\b 2.0))

      #f))

;;; --------------------------------------------------------------------

  (let ()	;explicit FIELDS-ACCESSOR name

    (define-class <alpha>
      (fields (mutable a)
	      (mutable b)))

    (define r (make-<alpha> 1 2))
    (define s (make-<alpha> #\a #\b))
    (define t (make-<alpha> 1.0 2.0))
    (with-fields ((<alpha> r)
		  (<alpha> s)
		  (<alpha> t))
      (check
	  (list r.a s.a t.a
		r.b s.b t.b)
	=> '(1 #\a 1.0  2 #\b 2.0))

      (set! r.a 3)
      (set! s.a #\c)
      (set! t.a 3.0)

      (check
	  (list r.a s.a t.a
		r.b s.b t.b)
	=> '(3 #\c 3.0  2 #\b 2.0))

      (set! r.b 4)
      (set! s.b #\d)
      (set! t.b 4.0)

      (check
	  (list r.a s.a t.a
		r.b s.b t.b)
	=> '(3 #\c 3.0  4 #\d 4.0))

      #f))

  #t)


(parametrise ((check-test-name	'with-virtual-fields))

  (let ()	;immutable virtual fields

    (define-class <fraction>
      (fields (mutable number))
      (virtual-fields (immutable numerator)
		      (immutable denominator)))

    (define (<fraction>-numerator o)
      (numerator (<fraction>-number o)))

    (define (<fraction>-denominator o)
      (denominator (<fraction>-number o)))

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-fields ((<fraction> o))
	    o.numerator))
      => 2)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-fields ((<fraction> o))
	    o.denominator))
      => 3)

    #f)

  (let ()	;mutable virtual fields

    (define-class <fraction>
      (fields (mutable number))
      (virtual-fields (mutable numerator)
		      (mutable denominator)))

    (define (<fraction>-numerator o)
      (numerator (<fraction>-number o)))

    (define (<fraction>-numerator-set! o v)
      (let ((n (<fraction>-number o)))
	(<fraction>-number-set! o (/ v (denominator n)))))

    (define (<fraction>-denominator o)
      (denominator (<fraction>-number o)))

    (define (<fraction>-denominator-set! o v)
      (let ((n (<fraction>-number o)))
	(<fraction>-number-set! o (/ (numerator n) v))))

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-fields ((<fraction> o))
	    o.numerator))
      => 2)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-fields ((<fraction> o))
	    o.denominator))
      => 3)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-fields ((<fraction> o))
	    (set! o.numerator 5)
	    o.number))
      => 5/3)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-fields ((<fraction> o))
	    (set! o.denominator 5)
	    o.number))
      => 2/5)

    #f)

  #t)


(parametrise ((check-test-name 'parent-list))

;;;We cannot  rely on the  RTDs to be  equal when the definition  of the
;;;corresponding records comes from different library import paths, even
;;;when the records are nongenerative.  So we compare the UIDs.

  (let ()
    (define-class <alpha>)
    (define-class <beta>
      (parent <alpha>))
    (define-class <gamma>
      (parent <beta>))

    (check
	(map record-type-uid (record-parent-list (record-type-descriptor <alpha>)))
      => (map record-type-uid (list (record-type-descriptor <alpha>))))

    (check
	(map record-type-uid (record-parent-list (record-type-descriptor <beta>)))
      => (map record-type-uid (list (record-type-descriptor <beta>)
				    (record-type-descriptor <alpha>))))

    (check
	(map record-type-uid (record-parent-list (record-type-descriptor <gamma>)))
      => (map record-type-uid (list (record-type-descriptor <gamma>)
				    (record-type-descriptor <beta>)
				    (record-type-descriptor <alpha>))))
    #t)

;;; --------------------------------------------------------------------
;;; The following tests use the hierarchy from the (records-lib) library

  (let ((env (environment '(rnrs) '(records-lib))))

    (check
	(map record-type-uid (record-parent-list* <alpha>))
      => (eval '(map record-type-uid (list (record-type-descriptor <alpha>)))
	       env))

    (check
	(map record-type-uid (record-parent-list* <beta>))
      => (eval '(map record-type-uid (list (record-type-descriptor <beta>)
					   (record-type-descriptor <alpha>)))
	       env))

    (check
	(map record-type-uid (record-parent-list* <gamma>))
      => (eval '(map record-type-uid (list (record-type-descriptor <gamma>)
					   (record-type-descriptor <beta>)
					   (record-type-descriptor <alpha>)))
	       env))
    #f)

  #t)


(parametrise ((check-test-name 'function-makers))

  (let ()
    (define-class <alpha>
      (fields (mutable a)
	      (immutable b)
	      (mutable c)))

    (define-class <beta>
      (parent <alpha>)
      (fields (mutable d)
	      (immutable e)
	      (mutable f)))

    (define-class <gamma>
      (parent <beta>)
      (fields (mutable g)
	      (immutable h)
	      (mutable i)))

    (let* ((maker	(make-record-maker (record-type-descriptor <gamma>) 1))
	   (ga		(maker)))

      (check
	  (<gamma>? ga)
	=> #t)

      (check
	  (list (<alpha>-a ga) (<alpha>-b ga) (<alpha>-c ga)
		(<beta>-d  ga) (<beta>-e  ga) (<beta>-f  ga)
		(<gamma>-g ga) (<gamma>-h ga) (<gamma>-i ga))
	=> '(1 1 1  1 1 1  1 1 1))

      #t)

    (let* ((maker	(make-record-maker (record-type-descriptor <gamma>)))
	   (ga		(maker)))

      (check
	  (<gamma>? ga)
	=> #t)

      (check
	  (list (<alpha>-a ga)
		(<alpha>-b ga)
		(<alpha>-c ga)
		(<beta>-d ga)
		(<beta>-e ga)
		(<beta>-f ga)
		(<gamma>-g ga)
		(<gamma>-h ga)
		(<gamma>-i ga))
	=> '(#f #f #f  #f #f #f  #f #f #f))

      #t)
    #t)

;;; --------------------------------------------------------------------
;;; The following tests use the hierarchy from the (records-lib) library

  (check
      (let ((maker (make-record-maker* <alpha>)))
	(record? (maker)))
    => #t)

  (check
      (let* ((maker (make-record-maker* <alpha>))
	     (o     (maker)))
	(list ((record-accessor (record-type-descriptor <alpha>) 0) o)
	      ((record-accessor (record-type-descriptor <alpha>) 1) o)
	      ((record-accessor (record-type-descriptor <alpha>) 2) o)))
    => '(#f #f #f))

  (check
      (let ((maker (make-record-maker* <alpha> 1)))
	(record? (maker)))
    => #t)

  (check
      (let* ((maker (make-record-maker* <alpha> 1))
	     (o     (maker)))
	(list ((record-accessor (record-type-descriptor <alpha>) 0) o)
	      ((record-accessor (record-type-descriptor <alpha>) 1) o)
	      ((record-accessor (record-type-descriptor <alpha>) 2) o)))
    => '(1 1 1))

  (check
      (let* ((v     124)
	     (maker (make-record-maker* <alpha> v))
	     (o     (maker)))
	(list ((record-accessor (record-type-descriptor <alpha>) 0) o)
	      ((record-accessor (record-type-descriptor <alpha>) 1) o)
	      ((record-accessor (record-type-descriptor <alpha>) 2) o)))
    => '(124 124 124))

  (check
      (let* ((v     123)
	     (maker (make-record-maker* <alpha> (+ 1 v)))
	     (o     (maker)))
	(list ((record-accessor (record-type-descriptor <alpha>) 0) o)
	      ((record-accessor (record-type-descriptor <alpha>) 1) o)
	      ((record-accessor (record-type-descriptor <alpha>) 2) o)))
    => '(124 124 124))

  #t)


(parametrise ((check-test-name 'macro-makers))

;;; These tests make use of the record types exported by (records-lib).

  (let ((a (make <alpha>
	     1 2 3))
	(b (make <beta>
	     1 2 3
	     4 5 6)))

    (check
    	(list ((record-accessor (record-type-descriptor <alpha>) 0) a)
	      ((record-accessor (record-type-descriptor <alpha>) 1) a)
	      ((record-accessor (record-type-descriptor <alpha>) 2) a))
      => '(1 2 3))

    (check
    	(list ((record-accessor (record-type-descriptor <alpha>) 0) b)
	      ((record-accessor (record-type-descriptor <alpha>) 1) b)
	      ((record-accessor (record-type-descriptor <alpha>) 2) b)
	      ((record-accessor (record-type-descriptor <beta>) 0) b)
	      ((record-accessor (record-type-descriptor <beta>) 1) b)
	      ((record-accessor (record-type-descriptor <beta>) 2) b))
      => '(1 2 3 4 5 6))

    #f)

  #t)


(parametrise ((check-test-name 'predicates))

;;; These tests make use of the record types exported by (records-lib).

  (let ((a (make <alpha>
	     1 2 3))
	(b (make <beta>
	     1 2 3
	     4 5 6)))

    (check
    	(is-a? a <alpha>)
      => #t)

    (check
    	(is-a? b <beta>)
      => #t)

    #f)

;;; --------------------------------------------------------------------
;;; These tests make use of the record types exported by (records).

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

  ;;;(check-for-true	(is-a? <binary-port>))
  (check-for-false	(is-a? 123 <binary-port>))

  ;;(check-for-true	(is-a? (open-string-input-port "ciao") <textual-port>))
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


(parametrise ((check-test-name 'inspection))

  (let ()
    (define-class <alpha>
      (fields (mutable a)
	      (immutable b)
	      (mutable c)))

    (define-class <beta>
      (parent <alpha>)
      (fields (mutable d)
	      (immutable e)
	      (mutable f)))

    (define-class <gamma>
      (parent <beta>)
      (fields (mutable g)
	      (immutable h)
	      (mutable i)))

    (define make-a
      (make-record-maker (record-type-descriptor <alpha>)))

    (define make-g
      (make-record-maker (record-type-descriptor <gamma>)))

    (define a (make-a))
    (define g (make-g))

    (check
	(record-type-uid (record-type-of a))
      => (record-type-uid (record-type-descriptor <alpha>)))

    (check
	(record-type-uid (record-type-of g))
      => (record-type-uid (record-type-descriptor <gamma>)))

    #f)

;;; --------------------------------------------------------------------
;;; These tests make use of the record types exported by (records-lib).

  (let ((a (make <alpha>
	     1 2 3))
	(b (make <beta>
	     1 2 3
	     4 5 6)))

    (check
	(record-type-uid (record-type-of a))
      => (record-type-uid (record-type-descriptor <alpha>)))

    (check
	(record-type-uid (record-type-of b))
      => (record-type-uid (record-type-descriptor <beta>)))

    #f)


;;; --------------------------------------------------------------------
;;; These tests make use of the record types exported by (records).

  (check
      (record-type-uid (record-type-of 123))
    => (record-type-uid (record-type-descriptor <fixnum>)))

  (check
      (record-type-uid (record-type-of (expt 10 30)))
    => (record-type-uid (record-type-descriptor <integer>)))

  (check
      (record-type-uid (record-type-of 1/2))
    => (record-type-uid (record-type-descriptor <rational>)))

  (check
      (record-type-uid (record-type-of #i3+0i))
    => (record-type-uid (record-type-descriptor <integer-valued>)))

  (check
      (record-type-uid (record-type-of #i3.0+0i))
    => (record-type-uid (record-type-descriptor <integer-valued>)))

  (check
      (record-type-uid (record-type-of #i3/2+0i))
    => (record-type-uid (record-type-descriptor <rational-valued>)))

  (check
      (record-type-uid (record-type-of #i3/2+0.0i))
    => (record-type-uid (record-type-descriptor <rational-valued>)))

  (check
      (record-type-uid (record-type-of #\a))
    => (record-type-uid (record-type-descriptor <char>)))

  (check
      (record-type-uid (record-type-of "ciao"))
    => (record-type-uid (record-type-descriptor <string>)))

  (check
      (record-type-uid (record-type-of '#(1 2 3)))
    => (record-type-uid (record-type-descriptor <vector>)))

  (check
      (record-type-uid (record-type-of '#vu8(1 2 3)))
    => (record-type-uid (record-type-descriptor <bytevector>)))

  (check
      (record-type-uid (record-type-of (make-eq-hashtable)))
    => (record-type-uid (record-type-descriptor <hashtable>)))

  (check
      (record-type-uid (record-type-of (open-string-input-port "ciao")))
    => (record-type-uid (record-type-descriptor <input-port>)))

  ;;It never returns <textual-port>  because input and output attributes
  ;;are checked first.
  ;; (check
  ;;     (record-type-uid (record-type-of (open-string-input-port "ciao")))
  ;;   => (record-type-uid (record-type-descriptor <textual-port>)))

  ;;It  never returns  <port> because  input and  output  attributes are
  ;;checked first so it returns <input-port> or <output-port>.
  ;; (check
  ;;     (record-type-uid (record-type-of (open-string-input-port "ciao")))
  ;;   => (record-type-uid (record-type-descriptor <port>)))

  (check
      (record-type-uid (record-type-of (current-output-port)))
    => (record-type-uid (record-type-descriptor <output-port>)))

  (check
      (record-type-uid (record-type-of (make-message-condition "ciao")))
    => (record-type-uid (record-type-descriptor &message)))

  (check
      (record-type-uid (record-type-of '(1 . 2)))
    => (record-type-uid (record-type-descriptor <pair>)))

  #t)


;;;; done

(check-report)

;;; end of file