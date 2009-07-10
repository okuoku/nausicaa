;;;
;;;Part of: ScmObj
;;;Contents: tests for ScmObj
;;;Date: Tue Nov 11, 2008
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
  (scmobj)
  (sentinel))

(check-set-mode! 'report-failed)
(display "*** testing scmobj core\n")




(check
    (class-of <class>)
  => <class>)

(check
    (class-definition-name <class>)
  => '<class>)

(check
    (class-precedence-list <class>)
  => '())

(check
    (class-slots <class>)
  => '(:class-definition-name :class-precedence-list :slots :direct-slots))

(check
    (class-direct-slots <class>)
  => '(:class-definition-name :class-precedence-list :slots :direct-slots))

(check
    (class? <class>)
  => #t)

(check
    (instance? <class>)
  => #t)

(check
    (is-a? <class> <class>)
  => #t)

;;; --------------------------------------------------------------------

(check
    (class-of <builtin-class>)
  => <class>)

(check
    (class-definition-name <builtin-class>)
  => '<builtin-class>)

(check
    (class-precedence-list <builtin-class>)
  => '())

(check
    (class-slots <builtin-class>)
  => '())

(check
    (class-direct-slots <builtin-class>)
  => '())

(check
    (class? <builtin-class>)
  => #t)

(check
    (instance? <builtin-class>)
  => #t)

(check
    (is-a? <builtin-class> <class>)
  => #t)


;;;; class inspection: predefined entity classes

;; <circular-list>
;; <dotted-list>
;; <proper-list>
;; <list>
;; <pair>
;; <vector>
;; <bytevector>
;; <hashtable>
;; <record>
;; <condition>
;; <binary-port>
;; <textual-port>
;; <input-port>
;; <output-port>
;; <port>
;; <fixnum>
;; <flonum>
;; <integer>
;; <integer-valued>
;; <rational>
;; <rational-valued>
;; <real>
;; <real-valued>
;; <complex>
;; <number>


;;;; class inspection: custom classes with simple inheritance

(let* ((<one> (make-class () :a :b :c))
       (<two> (make-class (<one>) :d :e :f))
       (<three> (make-class (<two>) :g :h :i)))

  (check
      (class? <one>)
    => #t)
  (check
      (class? <two>)
    => #t)
  (check
      (class? <three>)
    => #t)

  (check
      (instance? <one>)
    => #t)
  (check
      (instance? <two>)
    => #t)
  (check
      (instance? <three>)
    => #t)

  (check
      (class-of <one>)
    => <class>)
  (check
      (class-of <two>)
    => <class>)
  (check
      (class-of <three>)
    => <class>)

  (check-for-true (sentinel? (class-definition-name <one>)))
  (check-for-true (sentinel? (class-definition-name <two>)))
  (check-for-true (sentinel? (class-definition-name <three>)))

  (check
      (class-precedence-list <one>)
    => '())
  (check
      (class-precedence-list <two>)
    => (list <one>))
  (check
      (class-precedence-list <three>)
    => (list <two> <one>))

  (check
      (class-slots <one>)
    => '(:a :b :c))
  (check
      (class-slots <two>)
    => '(:d :e :f :a :b :c))
  (check
      (class-slots <three>)
    => '(:g :h :i :d :e :f :a :b :c))

  (check
      (class-direct-slots <one>)
    => '(:a :b :c))
  (check
      (class-direct-slots <two>)
    => '(:d :e :f))
  (check
      (class-direct-slots <three>)
    => '(:g :h :i))

  (check
      (is-a? <one> <class>)
    => #t)
  (check
      (is-a? <two> <class>)
    => #t)
  (check
      (is-a? <three> <class>)
    => #t)
  )

;;; --------------------------------------------------------------------

(let ()
  (define-class <one> () :a :b :c)
  (define-class <two> (<one>) :d :e :f)
  (define-class <three> (<two>) :g :h :i)

  (check
      (class? <one>)
    => #t)
  (check
      (class? <two>)
    => #t)
  (check
      (class? <three>)
    => #t)

  (check
      (instance? <one>)
    => #t)
  (check
      (instance? <two>)
    => #t)
  (check
      (instance? <three>)
    => #t)

  (check
      (class-of <one>)
    => <class>)
  (check
      (class-of <two>)
    => <class>)
  (check
      (class-of <three>)
    => <class>)

  (check
      (class-definition-name <one>)
    => '<one>)
  (check
      (class-definition-name <two>)
    => '<two>)
  (check
      (class-definition-name <three>)
    => '<three>)

  (check
      (class-precedence-list <one>)
    => '())
  (check
      (class-precedence-list <two>)
    => (list <one>))
  (check
      (class-precedence-list <three>)
    => (list <two> <one>))

  (check
      (class-slots <one>)
    => '(:a :b :c))
  (check
      (class-slots <two>)
    => '(:d :e :f :a :b :c))
  (check
      (class-slots <three>)
    => '(:g :h :i :d :e :f :a :b :c))

  (check
      (is-a? <one> <class>)
    => #t)
  (check
      (is-a? <two> <class>)
    => #t)
  (check
      (is-a? <three> <class>)
    => #t)
  )


;;;; class inspection: custom classes with multiple inheritance

(let ()
  (define-class <a> () :a)
  (define-class <b> () :b)
  (define-class <c> () :c)
  (define-class <d> () :d)
  (define-class <e> () :e)

  (define-class <pp> (<a> <b>))
  (define-class <qq> (<c> <d>))
  (define-class <rr> (<pp> <e> <qq>))

  (check
      (every class? (list <pp> <qq> <rr>))
    => #t)
  (check
      (every instance? (list <pp> <qq> <rr>))
    => #t)

  (check
      (list
       (class-of <pp>)
       (class-of <qq>)
       (class-of <rr>))
    => (list <class> <class> <class>))

  (check
      (class-precedence-list <pp>)
    => (list <a> <b>))
  (check
      (class-precedence-list <qq>)
    => (list <c> <d>))
  (check
      (class-precedence-list <rr>)
    => (list <pp> <a> <b> <e> <qq> <c> <d>))

  (check
      (class-slots <pp>)
    => '(:a :b))
  (check
      (class-slots <qq>)
    => '(:c :d))
  (check
      (class-slots <rr>)
    => '(:a :b :e :c :d))

  (check
      (is-a? <pp> <class>)
    => #t)
  (check
      (is-a? <qq> <class>)
    => #t)
  (check
      (is-a? <rr> <class>)
    => #t)
  )

;;; --------------------------------------------------------------------

(let ()
  (define-class <a> () :a)
  (define-class <b> () :b)
  (define-class <c> () :c)
  (define-class <d> () :d)
  (define-class <e> () :e)

  (define-class <pp> (<a> <b>) :pp)
  (define-class <qq> (<c> <d>) :qq)
  (define-class <rr> (<pp> <e> <qq>) :rr)

  (check
      (every class? (list <pp> <qq> <rr>))
    => #t)
  (check
      (every instance? (list <pp> <qq> <rr>))
    => #t)

  (check
      (list
       (class-of <pp>)
       (class-of <qq>)
       (class-of <rr>))
    => (list <class> <class> <class>))

  (check
      (class-precedence-list <pp>)
    => (list <a> <b>))
  (check
      (class-precedence-list <qq>)
    => (list <c> <d>))
  (check
      (class-precedence-list <rr>)
    => (list <pp> <a> <b> <e> <qq> <c> <d>))

  (check
      (class-slots <pp>)
    => '(:pp :a :b))
  (check
      (class-slots <qq>)
    => '(:qq :c :d))
  (check
      (class-slots <rr>)
    => '(:rr :pp :a :b :e :qq :c :d))

  (check
      (is-a? <pp> <class>)
    => #t)
  (check
      (is-a? <qq> <class>)
    => #t)
  (check
      (is-a? <rr> <class>)
    => #t)

  )


;;; class instantiation: slot access

(let* ((<one>	(make-class () :a :b :c))
       (o	(make <one> :a 1 :b 2 :c 3)))
  (check
      (instance? o)
    => #t)
  (check
      (is-a? o <one>)
    => #t)

  (check
      (list (slot-ref o ':b)
	    (slot-ref o ':a)
	    (slot-ref o ':c))
    => '(2 1 3))
  )

;;; --------------------------------------------------------------------

(let ()
  (define-class <one> ()
    :a :b :c)
  (define o (make <one>
			   :a 1 :b 2 :c 3))
  (check
      (instance? o)
    => #t)
  (check
      (is-a? o <one>)
    => #t)

  (check
      (list (slot-ref o ':b)
	    (slot-ref o ':a)
	    (slot-ref o ':c))
    => '(2 1 3))
  )


;;; class instantiation: simple inheritance

(let* ((<one>	(make-class () :a :b :c))
       (<two>	(make-class (<one>) :d :e :f))
       (<three>	(make-class (<two>) :g :h :i))
       (o	(make <three>
			       :a 1 :b 2 :c 3
			       :d 4 :e 5 :f 6
			       :g 7 :h 8 :i 9)))

  (check
      (subclass? <one> <class>)
    => #f)
  (check
      (subclass? <two> <one>)
    => #t)
  (check
      (subclass? <three> <two>)
    => #t)
  (check
      (subclass? <three> <one>)
    => #t)
  (check
      (subclass? <one> <three>)
    => #f)

  (check
      (is-a? <one> <class>)
    => #t)
  (check
      (is-a? <two> <class>)
    => #t)
  (check
      (is-a? <three> <class>)
    => #t)

  (check
      (is-a? o <three>)
    => #t)
  (check
      (is-a? o <two>)
    => #t)

  (check
      (map (lambda (s)
	     (slot-ref o s))
	'(:b :d :i))
    => '(2 4 9))

  (check
      (map class-definition-name (class-precedence-list <three>))
    => `(,sentinel ,sentinel))

  )

;;; --------------------------------------------------------------------

(check
    (let ()
      (define-class <one> () :a :b :c)
      (define-class <two> (<one>) :d :e :f)
      (define-class <three> (<two>) :g :h :i)

      (define o (make <three>
		  :a 1 :b 2 :c 3
		  :d 4 :e 5 :f 6
		  :g 7 :h 8 :i 9))
      (list (slot-ref o ':b)
	    (slot-ref o ':d)
	    (slot-ref o ':i))
      )
  => '(2 4 9))

(check
    (let ()
      (define-class <one> () :a :b :c)
      (define-class <two> (<one>) :d :e :f)
      (define-class <three> (<two>) :g :h :i)

      (list (subclass? <one> <class>)
	    (subclass? <two> <one>)
	    (subclass? <three> <two>)
	    (subclass? <three> <one>)
	    (subclass? <one> <three>)))
  => '(#f #t #t #t #f))

(let ()
  (define-class <one> () :a :b :c)
  (define-class <two> (<one>) :d :e :f)
  (define-class <three> (<two>) :g :h :i)

  (check
      (map class-definition-name (class-precedence-list <three>))
    => '(<two> <one>)))


;;; generic functions

(let ()
  (define-class <one> () :a :b :c)
  (define-class <two> (<one>) :d :e :f)
  (define-class <three> (<two>) :g :h :i)

  (define-generic alpha o)

  (define-method alpha ((o <one>))
    (slot-ref o ':a))

  (define-method alpha ((o <two>))
    (slot-ref o ':d))

  (define-method alpha ((o <three>))
    (slot-ref o ':g))

  (let ((a (make <one> :a 1))
	(b (make <two> :d 2))
	(c (make <three> :g 3)))
    (check (alpha a) => 1)
    (check (alpha b) => 2)
    (check (alpha c) => 3))
  )

;;; --------------------------------------------------------------------

;;;This tests overwriting an existing method function.
(let ()
  (define-class <one> () :a :b :c)
  (define-class <two> (<one>) :d :e :f)

  (define-generic alpha o)

  (define-method alpha ((o <one>))
    (slot-ref o ':a))

  (define-method alpha ((o <one>))
    (slot-ref o ':b))

  (let ((o (make <two> :a 1 :b 2)))
    (check (alpha o) => 2))
  )


;;;; next method

(let ()
  (define-class <one> () :a :b :c)
  (define-class <two> (<one>) :d :e :f)
  (define-class <three> (<two>) :g :h :i)

  (define-generic alpha o)

  (define-method alpha ((o <one>))
    (slot-ref o ':a))

  (define-method alpha ((o <two>))
    (cons (slot-ref o ':d)
	  (call-next-method)))

  (define-method alpha ((o <three>))
    (cons (slot-ref o ':g)
	  (call-next-method)))

  (let ((a (make <one>
	     :a 1))
	(b (make <two>
	     :a 1 :d 2))
	(c (make <three>
	     :a 1 :d 2 :g 3)))
    (check (alpha a) => 1)
    (check (alpha b) => '(2 . 1))
    (check (alpha c) => '(3 . (2 . 1)))
    )
  #t)


;;;; multiple inheritance

(let ()
  (define-class <a> () :a)
  (define-class <b> () :b)
  (define-class <c> () :c)
  (define-class <d> () :d)
  (define-class <e> () :e)

  (define-class <pp> (<a> <b>))
  (define-class <qq> (<c> <d>))
  (define-class <rr> (<pp> <e> <qq>))

  (define i (make <pp>
	      :a 1 :b 2))

  (define j (make <qq>
	      :c 3 :d 4))

  (define k (make <rr>
	      :a 10 :b 20
	      :c 30 :d 40))

  (check
      (slot-ref i ':a)
    => 1)
  (check
      (slot-ref j ':d)
    => 4)
  (check
      (slot-ref k ':d)
    => 40)

  (check
      (map class-definition-name (class-precedence-list <rr>))
    => '(<pp> <a> <b> <e> <qq> <c> <d>))

  )


;;; generic functions: predefined entity classes

(let ()
  (define-generic alpha o)

  (define-method alpha ((o <fixnum>))	'<fixnum>)
  (define-method alpha ((o <flonum>))	'<flonum>)
  (define-method alpha ((o <integer>))	'<integer>)
  (define-method alpha ((o <real>))	'<real>)
  (define-method alpha ((o <complex>))	'<complex>)
  (define-method alpha ((o <number>))	'<number>)

  (check (class-definition-name (class-of 12)) => '<fixnum>)
  (check (class-definition-name (class-of 1.2)) => '<rational>)
  (check (class-definition-name (class-of (expt 12 12))) => '<integer>)
  (check (class-definition-name (class-of 1.2+3.4i)) => '<complex>)

  ;;Here remember  that we  are using the  methods above,  we are
  ;;*not* applying CLASS-OF.
  (check (alpha 12) => '<fixnum>)
  (check (alpha (expt 12 12)) => '<integer>)
  (check (alpha 2/3) => '<real>)
  (check (alpha 1.2+3.4i) => '<complex>)
  )


;;;; done

(check-report)

;;; end of file
