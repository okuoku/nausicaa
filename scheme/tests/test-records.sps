;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for (records)
;;;Date: Wed Sep  9, 2009
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
  (records)
  (for (records-lib) expand)
  (for (records-lib-2) expand)
  (nos)
  (checks)
  (rnrs eval))

(check-set-mode! 'report-failed)
(display "*** testing records\n")


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

  ;;These tests use the hierarchy from the (records-lib) library

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


(parametrise ((check-test-name 'makers))

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
	  (list (<alpha>-a ga)
		(<alpha>-b ga)
		(<alpha>-c ga)
		(<beta>-d ga)
		(<beta>-e ga)
		(<beta>-f ga)
		(<gamma>-g ga)
		(<gamma>-h ga)
		(<gamma>-i ga))
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

;;;The following tests use the hierarchy from the (records-lib) library.

  (check
      (let ((maker (make-record-maker* <alpha>)))
	(record? (maker)))
    => #t)

  (check
      (let ((maker (make-record-maker* <alpha> 1)))
	(record? (maker)))
    => #t)

  (check
      (let* ((maker (make-record-maker* <alpha> 1))
	     (o     (maker)))
	(with-record-fields (((a b c) <alpha> o))
	  (list a b c)))
    => '(1 1 1))

  (check
      (let* ((maker (make-record-maker* <alpha>))
	     (o     (maker)))
	(with-record-fields (((a b c) <alpha> o))
	  (list a b c)))
    => '(#f #f #f))

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

  (let ((o (make <gamma> 1 2 3
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

    (check 'this
	(record-field-mutator* <gamma> b)
      => #f)

    #f)

  #t)


(parametrise ((check-test-name 'fields-define))

  (let ((o (make <gamma> 1 2 3
			 4 5 6
			 7 8 9)))

    (define-record-accessors <gamma>)
    (define-record-mutators <gamma>)

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

    #f)

;;; --------------------------------------------------------------------

  (let ((o (make <gamma> 1 2 3
			 4 5 6
			 7 8 9)))

    (define-record-accessors/this <gamma>)
    (define-record-mutators/this <gamma>)

    (check
	(list (<gamma>-g o)
	      (<gamma>-h o)
	      (<gamma>-i o))
      => '(7 8 9))

    (<gamma>-g-set! o 70)
    (<gamma>-i-set! o 90)

    (check
	(list (<gamma>-g o)
	      (<gamma>-h o)
	      (<gamma>-i o))
      => '(70 8 90))

    #f)

;;; --------------------------------------------------------------------

  (let ((o (make <gamma> 1 2 3
			 4 5 6
			 7 8 9)))

    (define-record-accessors/parents <gamma>)
    (define-record-mutators/parents <gamma>)

    (check
	(list (<gamma>-a o)
	      (<gamma>-b o)
	      (<gamma>-c o)
	      (<gamma>-d o)
	      (<gamma>-e o)
	      (<gamma>-f o))
      => '(1 2 3 4 5 6))

    (<gamma>-a-set! o 10)
    (<gamma>-c-set! o 30)
    (<gamma>-d-set! o 40)
    (<gamma>-f-set! o 60)

    (check
	(list (<gamma>-a o)
	      (<gamma>-b o)
	      (<gamma>-c o)
	      (<gamma>-d o)
	      (<gamma>-e o)
	      (<gamma>-f o))
      => '(10 2 30 40 5 60))

    #f)

;;; --------------------------------------------------------------------

  (let ((o (make <gamma> 1 2 3
			 4 5 6
			 7 8 9)))

    (define-record-accessors&mutators <gamma>)

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

    (<gamma>-a o 10)
    (<gamma>-c o 30)
    (<gamma>-d o 40)
    (<gamma>-f o 60)
    (<gamma>-g o 70)
    (<gamma>-i o 90)

    ;;This raises an error because  B is immutable, so <GAMMA>-B is only
    ;;an accessor, not a mutator.
    ;;
    ;;(<gamma>-b o 30)

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

    #f)

;;; --------------------------------------------------------------------

  (let ((o (make <gamma> 1 2 3
			 4 5 6
			 7 8 9)))

    (define-record-accessors&mutators/this <gamma>)

    (check
	(list (<gamma>-g o)
	      (<gamma>-h o)
	      (<gamma>-i o))
      => '(7 8 9))

    (<gamma>-g o 70)
    (<gamma>-i o 90)

    ;;This raises an error because  B is immutable, so <GAMMA>-B is only
    ;;an accessor, not a mutator.
    ;;
    ;;(<gamma>-b o 30)

    (check
    	(list (<gamma>-g o)
    	      (<gamma>-h o)
    	      (<gamma>-i o))
      => '(70 8 90))

    #f)

;;; --------------------------------------------------------------------

  (let ((o (make <gamma> 1 2 3
			 4 5 6
			 7 8 9)))

    (define-record-accessors&mutators/parents <gamma>)

    (check
	(list (<gamma>-a o)
	      (<gamma>-b o)
	      (<gamma>-c o)
	      (<gamma>-d o)
	      (<gamma>-e o)
	      (<gamma>-f o))
      => '(1 2 3 4 5 6))

    (<gamma>-a o 10)
    (<gamma>-c o 30)
    (<gamma>-d o 40)
    (<gamma>-f o 60)

    ;;This raises an error because  B is immutable, so <GAMMA>-B is only
    ;;an accessor, not a mutator.
    ;;
    ;;(<gamma>-b o 30)

    (check
    	(list (<gamma>-a o)
    	      (<gamma>-b o)
    	      (<gamma>-c o)
    	      (<gamma>-d o)
    	      (<gamma>-e o)
    	      (<gamma>-f o))
      => '(10 2 30 40 5 60))

    #f)

  #t)


(parametrise ((check-test-name 'fields-with))

  (let ((o (make <gamma> 1 2 3
			 4 5 6
			 7 8 9)))

    (with-record-accessors <gamma>
	(a b c d e f g h i)

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

      (with-record-mutators <gamma>
	  (a c d f g i)

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

	#f)
      #f)
    #f)

  (let ((o (make <gamma> 1 2 3
			 4 5 6
			 7 8 9)))

    (with-record-accessors&mutators <gamma>
	(a b c d e f g h i)

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

      (<gamma>-a o 10)
      (<gamma>-c o 30)
      (<gamma>-d o 40)
      (<gamma>-f o 60)
      (<gamma>-g o 70)
      (<gamma>-i o 90)

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

      #f)

    (check
	(guard (E (else `((message   . ,(condition-message E))
			  (irritants . ,(condition-irritants E)))))
	  ;;This raises an error at  expand time, because DUMMY is not a
	  ;;field name.
	  (eval '(with-record-accessors <gamma>
		     (dummy)
		   #t)
		(environment '(nausicaa)
			     '(records)
			     '(for (records-lib) expand run))))
      => '((message   . "unknown field names in record type hierarchy of \"<gamma>\"")
	   (irritants . ((dummy)))))

    (check
	(guard (E (else `((message   . ,(condition-message E))
			  (irritants . ,(condition-irritants E)))))
	  ;;This raises an error at  expand time, because DUMMY is not a
	  ;;field name.
	  (eval '(with-record-mutators <gamma>
		     (dummy)
		   #t)
		(environment '(nausicaa)
			     '(records)
			     '(for (records-lib) expand run))))
      => '((message   . "unknown field names in record type hierarchy of \"<gamma>\"")
	   (irritants . ((dummy)))))

    (check
	(guard (E (else `((message   . ,(condition-message E))
			  (irritants . ,(condition-irritants E)))))
	  ;;This raises an error at  expand time, because DUMMY is not a
	  ;;field name.
	  (eval '(with-record-accessors&mutators <gamma>
		     (dummy)
		   #t)
		(environment '(nausicaa)
			     '(records)
			     '(for (records-lib) expand run))))
      => '((message   . "unknown field names in record type hierarchy of \"<gamma>\"")
	   (irritants . ((dummy)))))

    (check
	(guard (E (else `((message   . ,(condition-message E))
			  (irritants . ,(condition-irritants E)))))
	  ;;This  raises  an error  at  expand  time,  because B  is  an
	  ;;immutable field.
	  (eval '(with-record-mutators <gamma>
		     (b)
		   #t)
		(environment '(nausicaa)
			     '(records)
			     '(for (records-lib) expand run))))
      => '((message   . "attempt to create mutator for immutable record field of \"<gamma>\"")
	   (irritants . (b))))


    #f)

  #t)


(parametrise ((check-test-name 'proof-identifier))

  (let ((o (make <gamma> 1 2 3
			 4 5 6
			 7 8 9)))

    (define-record-accessors <gamma>)
    (define-record-mutators  <gamma>)

    (let-syntax ((a (identifier-syntax (<gamma>-a o)))
		 (b (identifier-syntax (<gamma>-b o)))
		 (c (identifier-syntax (<gamma>-c o)))
		 (d (identifier-syntax (<gamma>-d o)))
		 (e (identifier-syntax (<gamma>-e o)))
		 (f (identifier-syntax (<gamma>-f o)))
		 (g (identifier-syntax (<gamma>-g o)))
		 (h (identifier-syntax (<gamma>-h o)))
		 (i (identifier-syntax (<gamma>-i o))))
      (check
	  (list a b c d e f g h i)
	=> '(1 2 3 4 5 6 7 8 9))

      #f)

    (let-syntax ((a (identifier-syntax (_		(<gamma>-a o))
				       ((set! _ e)	(<gamma>-a-set! o e))))
		 (b (identifier-syntax (<gamma>-b o)))
		 (c (identifier-syntax (_		(<gamma>-c o))
				       ((set! _ e)	(<gamma>-c-set! o e))))
		 (d (identifier-syntax (_		(<gamma>-d o))
				       ((set! _ e)	(<gamma>-d-set! o e))))
		 (e (identifier-syntax (<gamma>-e o)))
		 (f (identifier-syntax (_		(<gamma>-f o))
				       ((set! _ e)	(<gamma>-f-set! o e))))
		 (g (identifier-syntax (_		(<gamma>-g o))
				       ((set! _ e)	(<gamma>-g-set! o e))))
		 (h (identifier-syntax (<gamma>-h o)))
		 (i (identifier-syntax (_		(<gamma>-i o))
				       ((set! _ e)	(<gamma>-i-set! o e)))))
      (check
	  (begin
	    (set! a 10)
	    (set! c 30)
	    (set! d 40)
	    (set! f 60)
	    (set! g 70)
	    (set! i 90)
	    (list a b c d e f g h i))
	=> '(10 2 30 40 5 60 70 8 90))

      #f)
    #f)

  #t)


(parametrise ((check-test-name 'fields-identifier))

  (let ((o (make <gamma> 1 2 3
			 4 5 6
			 7 8 9)))

    (check
	(with-record-fields ((a <gamma> o))
	  a)
      => 1)

    (check
	(with-record-fields (((a) <gamma> o))
	  a)
      => 1)

    (check
	(with-record-fields ((a <gamma> o)
			     (b <gamma> o)
			     (c <gamma> o)
			     (d <gamma> o)
			     (e <gamma> o)
			     (f <gamma> o)
			     (g <gamma> o)
			     (h <gamma> o)
			     (i <gamma> o))
	  (list a b c d e f g h i))
	=> '(1 2 3 4 5 6 7 8 9))

    (check
	(with-record-fields (((a b c d e f g h i) <gamma> o))
	  (list a b c d e f g h i))
	=> '(1 2 3 4 5 6 7 8 9))

    (check
	(with-record-fields (((a b c) <gamma> o)
			     (d <gamma> o)
			     (e <gamma> o)
			     ((f g) <gamma> o)
			     (h <gamma> o)
			     (i <gamma> o))
	  (list a b c d e f g h i))
	=> '(1 2 3 4 5 6 7 8 9))

    (check
	(with-record-fields (((a b c) <gamma> o)
			     ((d e) <gamma> o)
			     ((f g) <gamma> o)
			     ((h i) <gamma> o))
	  (set! a 10)
	  (set! c 30)
	  (set! d 40)
	  (set! f 60)
	  (set! g 70)
	  (set! i 90)
	  (list a b c d e f g h i))
      => '(10 2 30 40 5 60 70 8 90))

    ;;Raise an "unknown field" error.
    ;;
    (check
	(guard (E (else `((message   . ,(condition-message E))
			  (irritants . ,(condition-irritants E)))))
	  (eval '(let ((o (make <gamma> 1 2 3
					4 5 6
					7 8 9)))
		   (with-record-fields ((ciao <gamma> o))
		     123))
		(environment '(rnrs) '(nos) '(records)
			     '(for (records-lib) expand))))
      => '((message . "unknown field name in record type hierarchy of \"<gamma>\"")
	   (irritants . (ciao))))

    ;;Raise an "attempt to mutate immutable field" error.
    ;;
    (check
	(guard (E (else `((message   . ,(condition-message E))
			  (irritants . ,(condition-irritants E)))))
	  (eval '(let ((o (make <gamma> 1 2 3
					4 5 6
					7 8 9)))
		   (with-record-fields ((b <gamma> o))
		     (set! b 1)
		     b))
		(environment '(rnrs) '(nos) '(records)
			     '(for (records-lib) expand))))
      => '((message . "attempt to mutate immutable field for record \"<alpha>\"")
	   (irritants . (b))))

    #f)

;;; --------------------------------------------------------------------

  (let ((o (make <gamma> 1 2 3
			 4 5 6
			 7 8 9)))

    (check
	(with-record-fields ((((augh a)) <gamma> o))
	  augh)
      => 1)

    (check
	(with-record-fields ((((augh a)) <gamma> o)
			     (((bugh b)) <gamma> o)
			     (((cugh c)) <gamma> o)
			     (((dugh d)) <gamma> o)
			     (((eugh e)) <gamma> o)
			     (((fugh f)) <gamma> o)
			     (((gugh g)) <gamma> o)
			     (((hugh h)) <gamma> o)
			     (((iugh i)) <gamma> o))
	  (list augh bugh cugh dugh eugh fugh gugh hugh iugh))
      => '(1 2 3 4 5 6 7 8 9))

    (check
	(with-record-fields ((((ax a) (bx b) (cx c)
			       (dx d) (ex e) (fx f)
			       (gx g) (hx h) (ix i)) <gamma> o))
	  (list ax bx cx dx ex fx gx hx ix))
      => '(1 2 3 4 5 6 7 8 9))

    (check
	(with-record-fields ((((ax a) (bx b) (cx c)) <gamma> o)
			     (((dx d)) <gamma> o)
			     (((ex e)) <gamma> o)
			     (((fx f) (gx g)) <gamma> o)
			     (((hx h)) <gamma> o)
			     (((ix i)) <gamma> o))
	  (list ax bx cx dx ex fx gx hx ix))
      => '(1 2 3 4 5 6 7 8 9))

     (check
	 (let ((count 0))
	   (with-record-fields ((((ax a) (bx b) (cx c)) <gamma> (begin
								  (set! count (+ 1 count))
								  o))
				(((dx d) (ex e)) <gamma> o)
				(((fx f) (gx g)) <gamma> o)
				(((hx h) (ix i)) <gamma> o))
	     (set! ax 10)
	     (set! cx 30)
	     (set! dx 40)
	     (set! fx 60)
	     (set! gx 70)
	     (set! ix 90)
	     (list count ax bx cx dx ex fx gx hx ix)))
       => '(1 10 2 30 40 5 60 70 8 90))

    #f)

;;; --------------------------------------------------------------------

    (let ((p (make <alpha> 1 2 3))
	  (q (make <alpha> 4 5 6)))

      (check
	  (with-record-fields ((((a1 a) (b1 b) (c1 c)) <alpha> p)
			       (((a2 a) (b2 b) (c2 c)) <alpha> q))
	    (set! a1 10)
	    (set! a2 20)
	    (list a1 b1 c1 a2 b2 c2))
	=> '(10 2 3 20 5 6))

      #f)

    #t)


(parametrise ((check-test-name 'dotted-fields-identifier))

  (let ((o (make <gamma> 1 2 3
			 4 5 6
			 7 8 9)))

    (check
	(with-record-fields* ((a <gamma> o))
	  o.a)
      => 1)

    (check
    	(with-record-fields* (((a) <gamma> o))
    	  o.a)
      => 1)

    (check
	(with-record-fields* ((a <gamma> o)
			      (b <gamma> o)
			      (c <gamma> o)
			      (d <gamma> o)
			      (e <gamma> o)
			      (f <gamma> o)
			      (g <gamma> o)
			      (h <gamma> o)
			      (i <gamma> o))
	  (list o.a o.b o.c o.d o.e o.f o.g o.h o.i))
      => '(1 2 3 4 5 6 7 8 9))

    (check
    	(with-record-fields* (((a b c d e f g h i) <gamma> o))
    	  (list o.a o.b o.c o.d o.e o.f o.g o.h o.i))
      => '(1 2 3 4 5 6 7 8 9))

    (check
    	(with-record-fields* (((a b c) <gamma> o)
			      (d <gamma> o)
			      (e <gamma> o)
			      ((f g) <gamma> o)
			      (h <gamma> o)
			      (i <gamma> o))
    	  (list o.a o.b o.c o.d o.e o.f o.g o.h o.i))
      => '(1 2 3 4 5 6 7 8 9))

    (check
    	(with-record-fields* (((a b c) <gamma> o)
			      ((d e) <gamma> o)
			      ((f g) <gamma> o)
			      ((h i) <gamma> o))
    	  (set! o.a 10)
    	  (set! o.c 30)
    	  (set! o.d 40)
    	  (set! o.f 60)
    	  (set! o.g 70)
    	  (set! o.i 90)
    	  (list o.a o.b o.c o.d o.e o.f o.g o.h o.i))
      => '(10 2 30 40 5 60 70 8 90))

    ;;Raise an "unknown field" error.
    ;;
    (check
	(guard (E (else `((message   . ,(condition-message E))
			  (irritants . ,(condition-irritants E)))))
	  (eval '(let ((o (make <gamma> 1 2 3
					4 5 6
					7 8 9)))
		   (with-record-fields* ((ciao <gamma> o))
		     123))
		(environment '(rnrs) '(nos) '(records)
			     '(for (records-lib) expand run))))
      => '((message . "unknown field name in record type hierarchy of \"<gamma>\"")
	   (irritants . (ciao))))

    ;;Raise an "attempt to mutate immutable field" error.
    ;;
    (check
	(guard (E (else `((message   . ,(condition-message E))
			  (irritants . ,(condition-irritants E)))))
	  (eval '(let ((o (make <gamma> 1 2 3
					4 5 6
					7 8 9)))
		   (with-record-fields ((b <gamma> o))
		     (set! b 1)
		     b))
		(environment '(rnrs) '(nos) '(records)
			     '(for (records-lib) expand run))))
      => '((message . "attempt to mutate immutable field for record \"<alpha>\"")
	   (irritants . (b))))

    #f)

;;; --------------------------------------------------------------------

    (let ((p (make <alpha> 1 2 3))
	  (q (make <alpha> 4 5 6)))

      (check
	  (with-record-fields* (((a b c) <alpha> p)
			        ((a b c) <alpha> q))
	    (set! p.a 10)
	    (set! q.a 20)
	    (list p.a p.b p.c q.a q.b q.c))
	=> '(10 2 3 20 5 6))

      #f)

  #t)


(parametrise ((check-test-name 'virtual-fields))

  (let ((iota  91)
	(theta 92))

    (define (iota-ref o)
      iota)

    (define (iota-set! o v)
      (set! iota v))

    (define (theta-ref o)
      theta)

    (define (theta-set! o v)
      (set! theta v))

    (define-record-extension <gamma>
      (fields (iota iota-ref iota-set!)
	      (theta theta-ref theta-set!)))

    ;;The following definition is to verify that DEFINE-RECORD-EXTENSION
    ;;is a <definition> in a  <body>, so it allows other <definition> to
    ;;appear after it.
    (define dummy 123)

    (let ((o (make <gamma>
	       1 2 3
	       4 5 6
	       7 8 9)))

      (check
	  (with-virtual-fields ((iota <gamma> o))
	    iota)
	=> 91)

      (check
	  (with-virtual-fields (((iota theta) <gamma> o))
	    (list iota theta))
	=> '(91 92))

      (check
	  (with-virtual-fields (((iota theta) <gamma> o))
	    (set! iota 5)
	    (set! theta 6)
	    (list iota theta))
	=> '(5 6))

      #f)

    (set! iota  91)
    (set! theta 92)

    (let ((o (make <gamma>
	       1 2 3
	       4 5 6
	       7 8 9)))

      (check
	  (with-virtual-fields* ((iota <gamma> o))
	    o.iota)
	=> 91)

      (check
	  (with-virtual-fields* (((iota theta) <gamma> o))
	    (list o.iota o.theta))
	=> '(91 92))

      (check
	  (with-virtual-fields* (((iota theta) <gamma> o))
	    (set! o.iota 5)
	    (set! o.theta 6)
	    (list o.iota o.theta))
	=> '(5 6))

      #f)

    #f)

;;; --------------------------------------------------------------------
;;; The following tests make use of the definitions in (records-lib-2).


  (let ((o (make <beta>
	     1 2 3
	     4 5 6)))

    (check
	(with-virtual-fields ((def <beta> o))
	  def)
      => '(4 5 6))

    (check
	(with-virtual-fields (((def) <beta> o))
	  (set! def '(90 91))
	  def)
      => '(90 5 91))

    #f)

  (let ((o (make <beta>
	     1 2 3
	     4 5 6)))

    (check
	(with-virtual-fields* ((def <beta> o))
	  o.def)
      => '(4 5 6))

    (check
	(with-virtual-fields* (((def) <beta> o))
	  (set! o.def '(90 91))
	  o.def)
      => '(90 5 91))

    #f)

;;; --------------------------------------------------------------------

  (let ((S "Ciao"))

    (define-record-extension <string>
      (fields (length string-length #f)
	      (upcase string-upcase #f)
	      (dncase string-downcase #f)))

    (check
	(with-virtual-fields ((length <string> S))
	  length)
      => 4)

    (check
	(with-virtual-fields (((upcase dncase) <string> S))
	  (list upcase dncase))
      => '("CIAO" "ciao"))

    (check
	(with-virtual-fields ((((len length)) <string> S))
	  len)
      => 4)

    (check
	(with-virtual-fields* ((length <string> S))
	  S.length)
      => 4)

    (check
	(with-virtual-fields* (((upcase dncase) <string> S))
	  (list S.upcase S.dncase))
      => '("CIAO" "ciao"))

    #f)
  #t)


;;;; done

(check-report)

;;; end of file
