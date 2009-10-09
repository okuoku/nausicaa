;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for (records basics)
;;;Date: Tue Oct  6, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
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
  (for (records) expand run)
  (for (records-lib) expand run))

(check-set-mode! 'report-failed)
(display "*** testing records basics\n")


(parametrise ((check-test-name 'parent-list))

  (let ()
    (define-record-type <alpha>)
    (define-record-type <beta>
      (parent <alpha>))
    (define-record-type <gamma>
      (parent <beta>))

    (check
	(record-parent-list (record-type-descriptor <alpha>))
      => (list (record-type-descriptor <alpha>)))

    (check
	(record-parent-list (record-type-descriptor <beta>))
      => (list (record-type-descriptor <beta>)
	       (record-type-descriptor <alpha>)))

    (check
	(record-parent-list (record-type-descriptor <gamma>))
      => (list (record-type-descriptor <gamma>)
	       (record-type-descriptor <beta>)
	       (record-type-descriptor <alpha>)))
    #t)

;;; --------------------------------------------------------------------
;;; The following tests use the hierarchy from the (records-lib) library

  (let ((env (environment '(rnrs) '(records-lib))))

    (check
	(record-parent-list* <alpha>)
      => (eval '(list (record-type-descriptor <alpha>))
	       env))

    (check
	(record-parent-list* <beta>)
      => (eval '(list (record-type-descriptor <beta>)
		      (record-type-descriptor <alpha>))
	       env))

    (check
	(record-parent-list* <gamma>)
      => (eval '(list (record-type-descriptor <gamma>)
		      (record-type-descriptor <beta>)
		      (record-type-descriptor <alpha>))
	       env))
    #f)

  #t)


(parametrise ((check-test-name 'function-makers))

  (let ()
    (define-record-type <alpha>
      (fields (mutable a)
	      (immutable b)
	      (mutable c)))

    (define-record-type <beta>
      (parent <alpha>)
      (fields (mutable d)
	      (immutable e)
	      (mutable f)))

    (define-record-type <gamma>
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
    (define-record-type <alpha>
      (fields (mutable a)
	      (immutable b)
	      (mutable c)))

    (define-record-type <beta>
      (parent <alpha>)
      (fields (mutable d)
	      (immutable e)
	      (mutable f)))

    (define-record-type <gamma>
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
	(record-type-of a)
      => (record-type-descriptor <alpha>))

    (check
	(record-type-of g)
      => (record-type-descriptor <gamma>))

    #f)

;;; --------------------------------------------------------------------
;;; These tests make use of the record types exported by (records-lib).

  (let ((a (make <alpha>
	     1 2 3))
	(b (make <beta>
	     1 2 3
	     4 5 6)))

    (check
	(record-type-of a)
      => (record-type-descriptor <alpha>))

    (check
	(record-type-of b)
      => (record-type-descriptor <beta>))

    #f)


;;; --------------------------------------------------------------------
;;; These tests make use of the record types exported by (records).

  (check
      (record-type-of 123)
    => (record-type-descriptor <fixnum>))

  (check
      (record-type-of (expt 10 30))
    => (record-type-descriptor <integer>))

  (check
      (record-type-of 1/2)
    => (record-type-descriptor <rational>))

  (check
      (record-type-of #i3+0i)
    => (record-type-descriptor <integer-valued>))

  (check
      (record-type-of #i3.0+0i)
    => (record-type-descriptor <integer-valued>))

  (check
      (record-type-of #i3/2+0i)
    => (record-type-descriptor <rational-valued>))

  (check
      (record-type-of #i3/2+0.0i)
    => (record-type-descriptor <rational-valued>))

  (check
      (record-type-of #\a)
    => (record-type-descriptor <char>))

  (check
      (record-type-of "ciao")
    => (record-type-descriptor <string>))

  (check
      (record-type-of '#(1 2 3))
    => (record-type-descriptor <vector>))

  (check
      (record-type-of '#vu8(1 2 3))
    => (record-type-descriptor <bytevector>))

  (check
      (record-type-of (make-eq-hashtable))
    => (record-type-descriptor <hashtable>))

  (check
      (record-type-of (open-string-input-port "ciao"))
    => (record-type-descriptor <input-port>))

  ;;It never returns <textual-port>  because input and output attributes
  ;;are checked first.
  #;(check
      (record-type-of (open-string-input-port "ciao"))
    => (record-type-descriptor <textual-port>))

  ;;It  never returns  <port> because  input and  output  attributes are
  ;;checked first so it returns <input-port> or <output-port>.
  #;(check
      (record-type-of (open-string-input-port "ciao"))
    => (record-type-descriptor <port>))

  (check
      (record-type-of (current-output-port))
    => (record-type-descriptor <output-port>))

  (check
      (record-type-of (make-message-condition "ciao"))
    => (record-type-descriptor &message))

  (check
      (record-type-of '(1 . 2))
    => (record-type-descriptor <pair>))

  #t)


(parametrise ((check-test-name 'fields-accessor-mutator))

  (let ()
    (define-record-type <alpha>
      (fields (mutable a)
	      (immutable b)
	      (mutable c)))

    (define-record-type <beta>
      (parent <alpha>)
      (fields (mutable d)
	      (immutable e)
	      (mutable f)))

    (define-record-type <gamma>
      (parent <beta>)
      (fields (mutable g)
	      (immutable h)
	      (mutable i)))

    (let ((o (make-<gamma>
	      1 2 3
	      4 5 6
	      7 8 9)))

      (define <gamma>-a (record-field-accessor (record-type-descriptor <gamma>) 'a))
      (define <gamma>-b (record-field-accessor (record-type-descriptor <gamma>) 'b))
      (define <gamma>-c (record-field-accessor (record-type-descriptor <gamma>) 'c))
      (define <gamma>-d (record-field-accessor (record-type-descriptor <gamma>) 'd))
      (define <gamma>-e (record-field-accessor (record-type-descriptor <gamma>) 'e))
      (define <gamma>-f (record-field-accessor (record-type-descriptor <gamma>) 'f))
      (define <gamma>-g (record-field-accessor (record-type-descriptor <gamma>) 'g))
      (define <gamma>-h (record-field-accessor (record-type-descriptor <gamma>) 'h))
      (define <gamma>-i (record-field-accessor (record-type-descriptor <gamma>) 'i))

      (define <gamma>-a-set! (record-field-mutator (record-type-descriptor <gamma>) 'a))
      (define <gamma>-c-set! (record-field-mutator (record-type-descriptor <gamma>) 'c))
      (define <gamma>-d-set! (record-field-mutator (record-type-descriptor <gamma>) 'd))
      (define <gamma>-f-set! (record-field-mutator (record-type-descriptor <gamma>) 'f))
      (define <gamma>-g-set! (record-field-mutator (record-type-descriptor <gamma>) 'g))
      (define <gamma>-i-set! (record-field-mutator (record-type-descriptor <gamma>) 'i))

      (check
	  (list (<gamma>-a o)
		(<gamma>-b o)
		(<gamma>-c o)
		(<gamma>-d o)
		(<gamma>-e o)
		(<gamma>-f o)
		(<gamma>-g o)
		(<gamma>-h o)
		(<gamma>-i o))
	=> '(1 2 3 4 5 6 7 8 9))

      (<gamma>-a-set! o 10)
      (<gamma>-c-set! o 30)
      (<gamma>-d-set! o 40)
      (<gamma>-f-set! o 60)
      (<gamma>-g-set! o 70)
      (<gamma>-i-set! o 90)

      (check
	  (list (<gamma>-a o)
		(<gamma>-b o)
		(<gamma>-c o)
		(<gamma>-d o)
		(<gamma>-e o)
		(<gamma>-f o)
		(<gamma>-g o)
		(<gamma>-h o)
		(<gamma>-i o))
	=> '(10 2 30 40 5 60 70 8 90))

      (check
	  (record-field-mutator (record-type-descriptor <gamma>) 'b)
	=> #f)

      #f)

    #f)

;;; --------------------------------------------------------------------
;;; These tests use the hierarchy from the (records-lib) library.

  (let* ((make-gamma (record-constructor (record-constructor-descriptor <gamma>)))
	 (o (make-gamma 1 2 3
			4 5 6
			7 8 9)))

    (define <gamma>-a (record-field-accessor* <gamma> a))
    (define <gamma>-b (record-field-accessor* <gamma> b))
    (define <gamma>-c (record-field-accessor* <gamma> c))
    (define <gamma>-d (record-field-accessor* <gamma> d))
    (define <gamma>-e (record-field-accessor* <gamma> e))
    (define <gamma>-f (record-field-accessor* <gamma> f))
    (define <gamma>-g (record-field-accessor* <gamma> g))
    (define <gamma>-h (record-field-accessor* <gamma> h))
    (define <gamma>-i (record-field-accessor* <gamma> i))

    (define <gamma>-a-set! (record-field-mutator* <gamma> a))
    (define <gamma>-c-set! (record-field-mutator* <gamma> c))
    (define <gamma>-d-set! (record-field-mutator* <gamma> d))
    (define <gamma>-f-set! (record-field-mutator* <gamma> f))
    (define <gamma>-g-set! (record-field-mutator* <gamma> g))
    (define <gamma>-i-set! (record-field-mutator* <gamma> i))

    (check
	(list (<gamma>-a o)
	      (<gamma>-b o)
	      (<gamma>-c o)
	      (<gamma>-d o)
	      (<gamma>-e o)
	      (<gamma>-f o)
	      (<gamma>-g o)
	      (<gamma>-h o)
	      (<gamma>-i o))
      => '(1 2 3 4 5 6 7 8 9))

    (<gamma>-a-set! o 10)
    (<gamma>-c-set! o 30)
    (<gamma>-d-set! o 40)
    (<gamma>-f-set! o 60)
    (<gamma>-g-set! o 70)
    (<gamma>-i-set! o 90)

    (check
    	(list (<gamma>-a o)
    	      (<gamma>-b o)
    	      (<gamma>-c o)
    	      (<gamma>-d o)
    	      (<gamma>-e o)
    	      (<gamma>-f o)
    	      (<gamma>-g o)
    	      (<gamma>-h o)
    	      (<gamma>-i o))
      => '(10 2 30 40 5 60 70 8 90))

    (check
	(record-field-mutator* <gamma> b #t)
      => #f)

    (check
	(procedure? (record-field-mutator* <gamma> b))
      => #t)

    (check
	(guard (E (else (condition-message E)))
	  ((record-field-mutator* <gamma> b) o 1))
      => "attempt to mutate immutable field of record \"<alpha>\" in record hierarchy of \"<gamma>\"")

    #f)

  #t)


;;;; done

(check-report)

;;; end of file
