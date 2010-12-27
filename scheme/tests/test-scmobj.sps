;;;
;;;Part of: ScmObj
;;;Contents: tests for ScmObj
;;;Date: Tue Nov 11, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008, 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (rnrs)
  (nausicaa lists)
  (nausicaa checks)
  (nausicaa scmobj)
  (nausicaa scmobj utils))

(check-set-mode! 'report-failed)
(display "*** testing scmobj \n")


;;;; layout printing

(define-class <biologic>)
(define-class <fruit> (<biologic>))
(define-class <apple> (<fruit>) :variety :colour :quality)
(define-class <price> () :tag)
(define-class <priced-apple> (<apple> <price>))

(define o (make <apple>
            :variety 'renetta
            :colour  'green
            :quality 'high))

(define p (make <priced-apple>
            :variety 'renetta
            :colour  'green
            :quality 'high
            :tag 100))


;; (printf "\nthis is <class>\n")
;; (pretty-print <class>)

;; (printf "\nthis is <biologic>\n")
;; (pretty-print <biologic>)

;; (printf "\nthis is <fruit>\n")
;; (pretty-print <fruit>)

;; (printf "\nthis is <apple>\n")
;; (pretty-print <apple>)

;; (printf "\nthis is o\n")
;; (pretty-print o)

;; (printf "\nthis is <priced-apple>\n")
;; (pretty-print <priced-apple>)

;; (printf "\nthis is p\n")
;; (pretty-print p)


(parameterise ((check-test-name 'class-basic))

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

  )


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


(parameterise ((check-test-name 'class-simple-inheritance))

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

    (check
	(class-definition-name <one>)
      => ':uninitialised)

    (check
	(class-definition-name <two>)
      => ':uninitialised)

    (check
	(class-definition-name <three>)
      => ':uninitialised)

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

  )


(parameterise ((check-test-name 'class-multiple-inheritance))

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

  )


(parameterise ((check-test-name 'instance-slot-access))

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

  )


(parameterise ((check-test-name 'instance-inheritance-slot-access))

  (let* ((<one>		(make-class () :a :b :c))
	 (<two>		(make-class (<one>) :d :e :f))
	 (<three>	(make-class (<two>) :g :h :i))
	 (o		(make <three>
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
      => `(:uninitialised :uninitialised))

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

  )


(parameterise ((check-test-name 'generic-simple-inheritance))

  (let ()
    (define-class <one> () :a :b :c)
    (define-class <two> (<one>) :d :e :f)
    (define-class <three> (<two>) :g :h :i)

    (define-class <dummy> () :uno)

    (define-generic alpha)

    (declare-method alpha ((o <one>))
      (slot-ref o ':a))

    (declare-method alpha ((o <two>))
      (slot-ref o ':d))

    (declare-method alpha ((o <three>))
      (slot-ref o ':g))

    (let ((a (make <one> :a 1))
	  (b (make <two> :d 2))
	  (c (make <three> :g 3))
	  (d (make <dummy> :uno 1)))
      (check (alpha a) => 1)
      (check (alpha b) => 2)
      (check (alpha c) => 3)

      (check
	  (guard (E (else `((message   . ,(condition-message E))
			    (irritants . ,(condition-irritants E)))))
	    (alpha d))
	=> '((message   . "no method defined for these argument classes")
	     (irritants . ((<dummy>)))))

      (check
	  (guard (E (else `((message   . ,(condition-message E))
			    (irritants . ,(condition-irritants E)))))
	    (alpha d d))
	=> '((message   . "no method defined for these argument classes")
	     (irritants . ((<dummy> <dummy>)))))

      #t)

    #t)

;;; --------------------------------------------------------------------

  (let ()
    ;;This tests overwriting an existing method function.

    (define-class <one> () :a :b :c)
    (define-class <two> (<one>) :d :e :f)

    (define-generic alpha)

    (declare-method alpha ((o <one>))
      (slot-ref o ':a))

    (declare-method alpha ((o <one>))
      (slot-ref o ':b))

    (let ((o (make <two> :a 1 :b 2)))
      (check (alpha o) => 2))
    )

;;; --------------------------------------------------------------------

  (let ()
    ;;Built in classes.

    (define-generic alpha)

    (declare-method alpha ((o <fixnum>))		'<fixnum>)
    (declare-method alpha ((o <flonum>))		'<flonum>)
    (declare-method alpha ((o <integer>))	'<integer>)
    (declare-method alpha ((o <real>))		'<real>)
    (declare-method alpha ((o <complex>))	'<complex>)
    (declare-method alpha ((o <number>))		'<number>)

    (check (class-definition-name (class-of 12))		=> '<fixnum>)
    (check (class-definition-name (class-of 1.2))		=> '<rational>)
    (check (class-definition-name (class-of (expt 12 12)))	=> '<integer>)
    (check (class-definition-name (class-of 1.2+3.4i))		=> '<complex>)

    ;;Here remember  that we  are using the  methods above,  we are
    ;;*not* applying CLASS-OF.
    (check (alpha 12) => '<fixnum>)
    (check (alpha (expt 12 12)) => '<integer>)
    (check (alpha 2/3) => '<real>)
    (check (alpha 1.2+3.4i) => '<complex>)
    )

  )


(parameterise ((check-test-name 'generic-next-method))

  (let ()
    (define-class <one> () :a :b :c)
    (define-class <two> (<one>) :d :e :f)
    (define-class <three> (<two>) :g :h :i)

    (define-generic alpha)

    (declare-method alpha ((o <one>))
      (slot-ref o ':a))

    (declare-method alpha ((o <two>))
      (cons (slot-ref o ':d)
	    (call-next-method)))

    (declare-method alpha ((o <three>))
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

  )


(parameterise ((check-test-name 'generic-multiple-inheritance))

  (let ()
    (define-class <a> () :a)
    (define-class <b> () :b)
    (define-class <c> () :c)
    (define-class <d> () :d)
    (define-class <e> () :e)

    (define-class <pp> (<a> <b>))
    (define-class <qq> (<c> <d>))
    (define-class <rr> (<pp> <e> <qq>))

    (define pp (make <pp> :a 1 :b 2))
    (define qq (make <qq> :c 3 :d 4))
    (define rr (make <rr> :a 10 :b 20 :c 30 :d 40))

    (check (slot-ref pp ':a) => 1)
    (check (slot-ref qq ':d) => 4)
    (check (slot-ref rr ':d) => 40)

    (check
	(map class-definition-name (class-precedence-list <rr>))
      => '(<pp> <a> <b> <e> <qq> <c> <d>))

    (let ()

      (define-generic alpha)
      (define-generic beta)
      (define-generic delta)

      (declare-method alpha ((o <pp>)) '<pp>)
      (declare-method alpha ((o <e>))  '<e>)
      (check (alpha rr) => '<pp>)

      ;;Call the first in the list of multiple inheritance.
      (declare-method beta ((o <c>)) '<c>)
      (declare-method beta ((o <d>)) '<d>)
      (check (beta qq) => '<c>)

      ;;Call the second in the list of multiple inheritance.
      (declare-method delta ((o <b>)) '<b>)
      (check (delta pp) => '<b>)

      ))

  )


(parameterise ((check-test-name 'generic-application-protocol))

  ;;The following hierarchy has  single and multiple inheritance, but NO
  ;;diamond inheritance.
  (define-class <a> () :a)
  (define-class <b> () :b)
  (define-class <c> () :c)
  (define-class <d> () :d)
  (define-class <e> () :e)

  (define-class <pp> (<a> <b>))
  (define-class <qq> (<c> <d>))
  (define-class <rr> (<pp> <e> <qq>))

  (define pp (make <pp> :a 1 :b 2))
  (define qq (make <qq> :c 3 :d 4))
  (define rr (make <rr> :a 10 :b 20 :c 30 :d 40))

  ;;The   following   hierarchy  has   single,   multiple  and   diamond
  ;;inheritance.
  (define-class <t> () :t)
  (define-class <x> (<t>) :x)
  (define-class <y> (<x>) :y)
  (define-class <w> (<x>) :w)
  (define-class <z> (<y> <w>))

  (define z (make <z> :t 0 :x 1 :y 2 :w 3))

  ;;Yet another hierarchy with single, multiple and diamond inheritance.
  (define-class <0> () :0)
  (define-class <1> (<0>) :1)
  (define-class <2> (<1>) :2)
  (define-class <3> (<1>) :3)
  (define-class <4> (<3>) :4)
  (define-class <5> (<4> <2>))

  (define n (make <5> :0 0 :1 1 :2 2 :3 3 :4 4))

;;; --------------------------------------------------------------------
;;; Raise an  error if a ":before"  or ":after" method  invokes the next
;;; method.
  (check
      (let ()
	(define-generic alpha)
	(declare-method alpha ((o <a>)) 1)
	(declare-method alpha :before ((o <a>)) (call-next-method))
	(guard (exc (else (condition? exc)))
	  (alpha pp)))
    => #t)

  (check
      (let ()
	(define-generic alpha)
	(declare-method alpha ((o <a>)) 1)
	(declare-method alpha :after ((o <a>)) (call-next-method))
	(guard (exc (else (condition? exc)))
	  (alpha pp)))
    => #t)

;;; --------------------------------------------------------------------
;;; Raise an error if we invoke  a next method from a ":primary" method,
;;; when no next method is available.
  (check
      (let ()
	(define-generic alpha)
	(declare-method alpha ((o <a>)) (call-next-method))
	;;(alpha pp)
	(guard (exc (else (condition? exc)))
	  (alpha pp))
	)
    => #t)

;;; --------------------------------------------------------------------
;;; Raise  an error  if we  invoke a  method when  no  ":primary" method
;;; exists.
  (check
      (let ()
	(define-generic alpha)
	(declare-method alpha :around ((o <a>)) (call-next-method))
	;;(alpha pp)
	(guard (exc (else (condition? exc)))
	  (alpha pp))
	)
    => #t)
  (check
      (let ()
	(define-generic alpha)
	(declare-method alpha :primary ((o <a>)) (call-next-method))
	;;(alpha pp)
	(guard (exc (else (condition? exc)))
	  (alpha pp))
	)
    => #t)
  (check
      (let ()
	(define-generic alpha)
	(declare-method alpha :around  ((o <a>)) (call-next-method))
	(declare-method alpha :primary ((o <a>)) (call-next-method))
	;;(alpha pp)
	(guard (exc (else (condition? exc)))
	  (alpha pp))
	)
    => #t)

;;; --------------------------------------------------------------------
;;; Call ":around"  methods before ":primary"  methods.  Call ":primary"
;;; methods after all the ":around" methods have been consumed.
  (check
      (let ()
	(define-generic alpha)
	(declare-method alpha :around ((o <a>))  1)
	(declare-method alpha :primary ((o <a>)) 2)
	(alpha pp)
	)
    => 1)
  (check
      (let ()
	(define-generic alpha)
	(declare-method alpha :around ((o <pp>))  (cons 1 (call-next-method)))
	(declare-method alpha :around ((o <a>))   2)
	(declare-method alpha :primary ((o <pp>)) 3)
	(alpha pp)
	)
    => '(1 . 2))
  (check
      (let ()
	(define-generic alpha)
	(declare-method alpha :around ((o <pp>))  (cons 1 (call-next-method)))
	(declare-method alpha :around ((o <a>))   (cons 2 (call-next-method)))
	(declare-method alpha :primary ((o <pp>)) 3)
	(alpha pp)
	)
    => '(1 2 . 3))
  (check
      (let ()
	(define-generic alpha)
	(declare-method alpha :around ((o <pp>))  (cons 1 (call-next-method)))
	(declare-method alpha :around ((o <a>))   (cons 2 (call-next-method)))
	(declare-method alpha :primary ((o <pp>)) (cons 3 (call-next-method)))
	(declare-method alpha :primary ((o <a>))  4)
	(alpha pp)
	)
    => '(1 2 3 . 4))

;;; --------------------------------------------------------------------
;;; Apply all the ":around" methods  first, then all the ":before", then
;;; a ":primary" method, then all the ":after" methods.
  (check
      (let ()
	(define-generic alpha)
	(declare-method alpha :around ((o <rr>))		(cons 1 (call-next-method)))
	(declare-method alpha :around ((o <pp>))		(cons 2 (call-next-method)))
	(declare-method alpha :around ((o <a>))		(cons 3 (call-next-method)))
	(declare-method alpha :around ((o <b>))		(cons 4 (call-next-method)))
	(declare-method alpha :around ((o <e>))		(cons 5 (call-next-method)))
	(declare-method alpha :before ((o <rr>))		(add-result 1))
	(declare-method alpha :before ((o <pp>))		(add-result 2))
	(declare-method alpha :before ((o <a>))		(add-result 3))
	(declare-method alpha :before ((o <b>))		(add-result 4))
	(declare-method alpha :before ((o <e>))		(add-result 5))
	(declare-method alpha :after ((o <rr>))		(add-result 10))
	(declare-method alpha :after ((o <pp>))		(add-result 9))
	(declare-method alpha :after ((o <a>))		(add-result 8))
	(declare-method alpha :after ((o <b>))		(add-result 7))
	(declare-method alpha :after ((o <e>))		(add-result 6))
	(declare-method alpha :primary ((o <rr>))	(cons 6 (call-next-method)))
	(declare-method alpha :primary ((o <pp>))	(cons 7 (call-next-method)))
	(declare-method alpha :primary ((o <a>))		(cons 8 (call-next-method)))
	(declare-method alpha :primary ((o <b>))		(cons 9 (call-next-method)))
	(declare-method alpha :primary ((o <e>))		10)
	(with-result (alpha rr))
	)
    => '((1 2 3 4 5 6 7 8 9 . 10) (1 2 3 4 5 6 7 8 9 10)))

;;; --------------------------------------------------------------------
;;; Apply all the methods to a hierarchy having diamond inheritance.
  (check
      (let ()
	(define-generic alpha)
	(declare-method alpha :around ((o <z>))		(cons 1 (call-next-method)))
	(declare-method alpha :around ((o <y>))		(cons 2 (call-next-method)))
	(declare-method alpha :around ((o <w>))		(cons 3 (call-next-method)))
	(declare-method alpha :around ((o <x>))		(cons 4 (call-next-method)))
	(declare-method alpha :around ((o <t>))		(cons 5 (call-next-method)))
	(declare-method alpha :primary ((o <z>))		(cons 6 (call-next-method)))
	(declare-method alpha :primary ((o <y>))		(cons 7 (call-next-method)))
	(declare-method alpha :primary ((o <w>))		(cons 8 (call-next-method)))
	(declare-method alpha :primary ((o <x>))		(cons 9 (call-next-method)))
	(declare-method alpha :primary ((o <t>))		10)
	(declare-method alpha :before ((o <z>))		(add-result 1))
	(declare-method alpha :before ((o <y>))		(add-result 2))
	(declare-method alpha :before ((o <w>))		(add-result 3))
	(declare-method alpha :before ((o <x>))		(add-result 4))
	(declare-method alpha :before ((o <t>))		(add-result 5))
	(declare-method alpha :after ((o <z>))		(add-result 10))
	(declare-method alpha :after ((o <y>))		(add-result 9))
	(declare-method alpha :after ((o <w>))		(add-result 8))
	(declare-method alpha :after ((o <x>))		(add-result 7))
	(declare-method alpha :after ((o <t>))		(add-result 6))
	(with-result (alpha z))
	)
    => '((1 2 3 4 5 6 7 8 9 . 10) (1 2 3 4 5 6 7 8 9 10)))

;;; --------------------------------------------------------------------
;;; Apply all the methods to a hierarchy having diamond inheritance.
  (check
      (let ()
	(define-generic alpha)
	(declare-method alpha :around ((o <5>))		(cons 1 (call-next-method)))
	(declare-method alpha :around ((o <4>))		(cons 2 (call-next-method)))
	(declare-method alpha :around ((o <3>))		(cons 3 (call-next-method)))
	(declare-method alpha :around ((o <2>))		(cons 4 (call-next-method)))
	(declare-method alpha :around ((o <1>))		(cons 5 (call-next-method)))
	(declare-method alpha :around ((o <0>))		(cons 6 (call-next-method)))

	(declare-method alpha :primary ((o <5>))		(cons 7 (call-next-method)))
	(declare-method alpha :primary ((o <4>))		(cons 8 (call-next-method)))
	(declare-method alpha :primary ((o <3>))		(cons 9 (call-next-method)))
	(declare-method alpha :primary ((o <2>))		(cons 10 (call-next-method)))
	(declare-method alpha :primary ((o <1>))		(cons 11 (call-next-method)))
	(declare-method alpha :primary ((o <0>))		12)

	(declare-method alpha :before ((o <5>))		(add-result 1))
	(declare-method alpha :before ((o <4>))		(add-result 2))
	(declare-method alpha :before ((o <3>))		(add-result 3))
	(declare-method alpha :before ((o <2>))		(add-result 4))
	(declare-method alpha :before ((o <1>))		(add-result 5))
	(declare-method alpha :before ((o <0>))		(add-result 6))

  	(declare-method alpha :after ((o <5>))		(add-result 12))
	(declare-method alpha :after ((o <4>))		(add-result 11))
	(declare-method alpha :after ((o <3>))		(add-result 10))
	(declare-method alpha :after ((o <2>))		(add-result 9))
	(declare-method alpha :after ((o <1>))		(add-result 8))
	(declare-method alpha :after ((o <0>))		(add-result 7))

	(with-result (alpha n))
	)
    => '((1 2 3 4 5 6 7 8 9 10 11 . 12)
	 (1 2 3 4 5 6 7 8 9 10 11 12)))

  )


(parameterise ((check-test-name 'generic-specificity))

  (define-class <a> () :a)
  (define-class <b> () :b)
  (define-class <c> () :c)
  (define-class <d> () :d)

  (define-class <1> (<a>))
  (define-class <2> (<b>))
  (define-class <3> (<c>))
  (define-class <4> (<d>))

  (define a (make <a> :a 1))
  (define b (make <b> :b 2))
  (define c (make <c> :c 3))
  (define d (make <d> :d 4))

  (define n1 (make <1> :a 1))
  (define n2 (make <2> :b 2))
  (define n3 (make <3> :c 3))
  (define n4 (make <4> :d 4))

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

  )


(parameterise ((check-test-name 'slot-accessors))

  (let ()
    (define-class <alpha> () :a :b :c)
    (define o (make <alpha>
		:a '(1 2 3)
		:b '(4 5 6)
		:c '(7 8 9)))

    (prepend-to-slot o ':a 10)
    (prepend-to-slot o ':b 20)
    (append-to-slot o ':c 40)

    (check
	(slot-ref o ':a)
      => '(10 1 2 3))

    (check
	(slot-ref o ':b)
      => '(20 4 5 6))

    (check
	(slot-ref o ':c)
      => '(7 8 9 40)))

  (let ()
    (define-class <alpha> () :a :b :c)
    (define o (make <alpha>
		:a '(1 2 3)
		:b '(4 5 6)
		:c '(7 8 9)))

    (with-slots-set! o (:a :b) (10 20))
    (with-slots-set! o '(:c) '(30))

    (check
	(slot-ref o ':a)
      => 10)
    (check
	(slot-ref o ':b)
      => 20)
    (check
	(slot-ref o ':c)
      => 30)

    (check
	(with-slots-ref o '(:a :b))
      => '(10 20))
    (check
	(with-slots-ref o '(:a :c))
      => '(10 30))
    )

  )


(parameterise ((check-test-name 'with-slots))

  (define-class <alpha> () :a :b :c)
  (define A (make <alpha>
	      :a 1 :b 2 :c 3))
  (define B (make <alpha>
	      :a 4 :b 5 :c 6))
  (define C (make <alpha>
	      :a 7 :b 8 :c 9))

  (with-slots ()
    (check
	(slot-ref A ':a)
      => 1))

  (with-slots (((d e f) (:a :b :c) A))
    (check
	(list d e f)
      => '(1 2 3)))

  (with-slots (((d e f) (:a :b :c) A)
	       ((g h i) (:a :b :c) B))
    (check
	(list d e f g h i)
      => '(1 2 3 4 5 6)))

  (with-slots (((d e f) (:a :b :c) A)
	       ((g h i) (:a :b :c) B)
	       ((l m n) (:a :b :c) C))
    (check
	(list d e f g h i l m n)
      => '(1 2 3 4 5 6 7 8 9)))

  #t)


;;;; done

(check-report)

;;; end of file
