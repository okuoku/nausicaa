;;;
;;;Part of: ScmObj
;;;Contents: tests for ScmObj
;;;Date: Tue Nov 11, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
;;;
;;;This  program  is free  software:  you  can redistribute  it
;;;and/or modify it  under the terms of the  GNU General Public
;;;License as published by the Free Software Foundation, either
;;;version  3 of  the License,  or (at  your option)  any later
;;;version.
;;;
;;;This  program is  distributed in  the hope  that it  will be
;;;useful, but  WITHOUT ANY WARRANTY; without  even the implied
;;;warranty  of  MERCHANTABILITY or  FITNESS  FOR A  PARTICULAR
;;;PURPOSE.   See  the  GNU  General Public  License  for  more
;;;details.
;;;
;;;You should  have received a  copy of the GNU  General Public
;;;License   along   with    this   program.    If   not,   see
;;;<http://www.gnu.org/licenses/>.
;;;


;;;page
;;; ------------------------------------------------------------
;;; Setup.
;;; ------------------------------------------------------------

(import (rnrs)
	(only (ikarus) printf pretty-print)
	(srfi lightweight-testing)
	(scmobj))

(check-set-mode! 'report-failed)

;;; ------------------------------------------------------------

;;;page
;;; ------------------------------------------------------------
;;; Code.
;;; ------------------------------------------------------------

(check
    (let* ((<one>	(make-class () (:a :b :c)))
	   (o		(make <one>
			  ':a 1 ':b 2 ':c 3)))
      (list (slot-ref o ':b)
	    (slot-ref o ':a)
	    (slot-ref o ':c)))
  => '(2 1 3))

(check
    (let* ((<one>	(make-class () (:a :b :c)))
	   (o		(make <one>
			  ':a 1 ':b 2 ':c 3)))
      (slot-set! o ':c 123)
      (list (slot-ref o ':b)
	    (slot-ref o ':a)
	    (slot-ref o ':c)))
  => '(2 1 123))

;;; ------------------------------------------------------------

;;;page
;;; ------------------------------------------------------------
;;; Subclassing.
;;; ------------------------------------------------------------

(check
    (let* ((<one>	(make-class () (:a :b :c)))
	   (<two>	(make-class (<one>) (:d :e :f)))
	   (<three>	(make-class (<two>) (:g :h :i)))
	   (o		(make <three>
			  ':a 1 ':b 2 ':c 3
			  ':d 4 ':e 5 ':f 6
			  ':g 7 ':h 8 ':i 9)))
      (list (slot-ref o ':b)
	    (slot-ref o ':d)
	    (slot-ref o ':i)))
  => '(2 4 9))

(check
    (let* ((<one>	(make-class () (:a :b :c)))
	   (<two>	(make-class (<one>) (:d :e :f)))
	   (<three>	(make-class (<two>) (:g :h :i))))
      (list (subclass? <one> <class>)
	    (subclass? <two> <one>)
	    (subclass? <three> <two>)
	    (subclass? <three> <one>)
	    (subclass? <one> <three>)))
  => '(#t #t #t #t #f))

(let* ((<one>	(make-class () (:a :b :c)))
       (<two>	(make-class (<one>) (:d :e :f)))
       (<three>	(make-class (<two>) (:g :h :i))))
  (check
      (map class-definition-name (class-precedence-list <three>))
    => '(:uninitialized :uninitialized :uninitialized <class>)))

;;; ------------------------------------------------------------

(check
    (let ()
      (define-class <one> ()
	:a :b :c)
      (define-class <two> (<one>)
	:d :e :f)
      (define-class <three> (<two>)
	:g :h :i)

      (define o (make <three>
		  ':a 1 ':b 2 ':c 3
		  ':d 4 ':e 5 ':f 6
		  ':g 7 ':h 8 ':i 9))
      (list (slot-ref o ':b)
	    (slot-ref o ':d)
	    (slot-ref o ':i)))
  => '(2 4 9))

(check
    (let ()
      (define-class <one> ()
	:a :b :c)
      (define-class <two> (<one>)
	:d :e :f)
      (define-class <three> (<two>)
	:g :h :i)

      (list (subclass? <one> <class>)
	    (subclass? <two> <one>)
	    (subclass? <three> <two>)
	    (subclass? <three> <one>)
	    (subclass? <one> <three>)))
  => '(#t #t #t #t #f))

(let ()
  (define-class <one> ()
    :a :b :c)
  (define-class <two> (<one>)
    :d :e :f)
  (define-class <three> (<two>)
    :g :h :i)

  (check
      (map class-definition-name (class-precedence-list <three>))
    => '(<three> <two> <one> <class>)))

;;; ------------------------------------------------------------

;;;page
;;; ------------------------------------------------------------
;;; Inspection.
;;; ------------------------------------------------------------

(let ((<one>	(make-class () (:a :b :c))))
  (check
      (let ((o (make <one>
		 ':a 1 ':b 2 ':c 3)))
	(class-of o))
    => <one>))

;;; ------------------------------------------------------------

(check
    (list-of-slots <class>)
  => '(:class :class-definition-name :class-precedence-list :slots))

(check
    (let* ((<one>	(make-class () (:a :b :c)))
	   (<two>	(make-class (<one>) (:d :e :f)))
	   (<three>	(make-class (<two>) (:g :h :i))))

      (list (list-of-slots <one>)
	    (list-of-slots <two>)
	    (list-of-slots <three>)))
  => '((:class :a :b :c :class-definition-name :class-precedence-list :slots)
       (:class :d :e :f :a :b :c :class-definition-name :class-precedence-list :slots)
       (:class :g :h :i :d :e :f :a :b :c :class-definition-name :class-precedence-list :slots)))

;;; ------------------------------------------------------------

;;;page
;;; ------------------------------------------------------------
;;; Generic functions.
;;; ------------------------------------------------------------

(let ()
  (define-class <one> ()
    :a :b :c)
  (define-class <two> (<one>)
    :d :e :f)
  (define-class <three> (<two>)
    :g :h :i)

  (define-generic alpha o)

  (define-method alpha ((o <one>))
    (slot-ref o ':a))

  (define-method alpha ((o <two>))
    (slot-ref o ':d))

  (define-method alpha ((o <three>))
    (slot-ref o ':g))

  (let ((a (make <one> ':a 1))
	(b (make <two> ':d 2))
	(c (make <three> ':g 3)))
    (check (alpha a) => 1)
    (check (alpha b) => 2)
    (check (alpha c) => 3))
  )

;;; ------------------------------------------------------------

;;;This tests overwriting an existing method function.
(let ()
  (define-class <one> ()
    :a :b :c)
  (define-class <two> (<one>)
    :d :e :f)

  (define-generic alpha o)

  (define-method alpha ((o <one>))
    (slot-ref o ':a))

  (define-method alpha ((o <one>))
    (slot-ref o ':b))

  (let ((o (make <two> ':a 1 ':b 2)))
    (check (alpha o) => 2))
  )

;;; ------------------------------------------------------------

;;;page
;;; ------------------------------------------------------------
;;; Next method.
;;; ------------------------------------------------------------

(let ()
  (define-class <one> ()
    :a :b :c)
  (define-class <two> (<one>)
    :d :e :f)
  (define-class <three> (<two>)
    :g :h :i)

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
	     ':a 1))
	(b (make <two>
	     ':a 1 ':d 2))
	(c (make <three>
	     ':a 1 ':d 2 ':g 3)))
    (check (alpha a) => 1)
    (check (alpha b) => '(2 . 1))
    (check (alpha c) => '(3 . (2 . 1)))
    )
  #t)

;;; ------------------------------------------------------------

;;;page
;;; ------------------------------------------------------------
;;; Done.
;;; ------------------------------------------------------------

(check-report)


;;; end of file
