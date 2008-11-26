;;;
;;;Part of: Nausicaa/CLOS
;;;Contents: tests for clos
;;;Date: Wed Nov 26, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
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



;;;; setup

(import (rnrs)
  (clos core)
  (clos user)
  (clos introspection)
  (srfi parameters)
  (uriel test))

(check-set-mode! 'report-failed)


;;;; basics example

(parameterize ((testname 'basics))
  (define-class <one> ()
    a)

  (define-class <two> ()
    b)

  (define-generic doit)

  (define-method doit ((o <top>))
    (cons o 123))

  (define-method doit ((o <one>))
    (cons o 456))

  (define-method doit ((o <two>))
    (cons o 789))

  (let ((o 'ciao))
    (check
	(doit o)
      => (cons o 123)))

  (let ((o (make <one>)))
    (check
	(doit o)
      => (cons o 456)))

  (let ((o (make <two>)))
    (check
	(doit o)
      => (cons o 789)))

  )



;;;; class definition tests and class object inspection

(parameterize ((testname 'class-definition))
  (define-class <one> ()
    a b c)

  (define-class <two> ()
    d e f)

  (define-class <three> (<one> <two>)
    g h i)

;;; --------------------------------------------------------------------

  (check
      (class-of <one>)
    => <class>)

  (check
      (class-of <class>)
    => <class>)

;;   (check
;;       (class-of 123)
;;     => <top>)

;;; ------------------------------------------------------------

  (check (class-definition-name <one>)
    => '<one>)

  (check (class-definition-name (class-of <one>))
    => '<class>)

;;; ------------------------------------------------------------

  (check (map class-definition-name (class-direct-supers <one>))
    => '(<object>))

  (check (map class-definition-name (class-direct-supers <three>))
    => '(<one> <two>))

  (check (class-direct-supers <three>)
    => (list <one> <two>))

  (check (class-direct-supers <class>)
    => (list <object>))

;;; ------------------------------------------------------------

  (check
      (class-direct-slots <one>)
    => '((a) (b) (c)))

  (check
      (class-direct-slots <three>)
    => '((g) (h) (i)))

  ;; ------------------------------------------------------------

  (check
      (class-slots <one>)
    => '((a) (b) (c)))

  (check
      (class-slots <three>)
    => '((g) (h) (i)
	 (a) (b) (c)
	 (d) (e) (f)))

;;; ------------------------------------------------------------

  (check
      (class-precedence-list <one>)
    => (list <one> <object> <top>))

  (check
      (class-precedence-list <three>)
    => (list <three> <one> <two> <object> <top>))

  )



;;;; class instantiation tests and instance inspection

(parameterize ((testname 'class-instantiation))

  (define-class <one> ()
    a b c)

  (define-class <two> ()
    d e f)

  (define-class <three> (<one> <two>)
    g h i)

  (check
      (class-of (make <one> 'a 1 'b 2 'c 3))
    => <one>)

  (check
      (class-definition-name (class-of (make <one> 'a 1 'b 2 'c 3)))
    => '<one>)

  (define-method initialize ((o <one>) initargs)
    (initialize-direct-slots o (class-of o) initargs))

  (check
      (let ((o (make <one> 'a 1 'b 2 'c 3)))
	(slot-ref o 'b))
    => 2)

  )


;;;; generic function tests

(parameterize ((testname 'generics))

  (define-class <one> ()
    a b c)

  (define-class <two> ()
    d e f)

  (define-class <three> (<one> <two>)
    g h i)


  (define-generic my-slots)

  (define-method my-slots ((o <one>))
    (list (slot-ref o 'a)
	  (slot-ref o 'b)
	  (slot-ref o 'c)))

;;; --------------------------------------------------------------------

;;   (check
;;       (class-of <generic>)
;;     => '())

  )



;;;; default mop functions

(parameterize ((testname 'default-mop))

  (define-class <one> ()
    a b c)

  (define-class <two> ()
    d e f)

  (define-class <three> (<one> <two>)
    g h i)

  (check
      (compute-slots <one>)
    => '((a) (b) (c)))

  )


;;;; done

(check-report)

;;; end of file
