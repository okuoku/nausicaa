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
  (for (records-lib) expand run)
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

  (let ()
    (define-record-type <alpha>)
    (define-record-type <beta>
      (parent <alpha>))
    (define-record-type <gamma>
      (parent <beta>))

    (check
	(record-parent-list* <alpha>)
      => (list (record-type-descriptor <alpha>)))

    (check
	(record-parent-list* <beta>)
      => (list (record-type-descriptor <beta>)
	       (record-type-descriptor <alpha>)))

    (check
	(record-parent-list* <gamma>)
      => (list (record-type-descriptor <gamma>)
	       (record-type-descriptor <beta>)
	       (record-type-descriptor <alpha>)))

    #t)

  #t)


(parametrise ((check-test-name 'makers))

  (let ()
    (define-record-type <alpha>
      (fields (mutable a)
	      (mutable b)
	      (mutable c)))

    (define-record-type <beta>
      (parent <alpha>)
      (fields (mutable d)
	      (mutable e)
	      (mutable f)))

    (define-record-type <gamma>
      (parent <beta>)
      (fields (mutable g)
	      (mutable h)
	      (mutable i)))

    (let* ((maker	(make-record-maker* <gamma> 1))
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

    (let* ((maker	(make-record-maker* <gamma>))
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

  #t)


(parametrise ((check-test-name 'fields))

  (let ((o (make-<gamma> 1 2 3
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

  (let ((o (make-<gamma> 1 2 3
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

  (let ((o (make-<gamma> 1 2 3
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

  (let ((o (make-<gamma> 1 2 3
			 4 5 6
			 7 8 9)))

    (define-record-accessors/mutators <gamma>)

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

  (let ((o (make-<gamma> 1 2 3
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

  (let ((o (make-<gamma> 1 2 3
			 4 5 6
			 7 8 9)))

    (with-record-accessors/mutators <gamma>
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
      => '((message   . "unknown field names in record type hierarchy")
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
      => '((message   . "unknown field names in record type hierarchy")
	   (irritants . ((dummy)))))

    (check
	(guard (E (else `((message   . ,(condition-message E))
			  (irritants . ,(condition-irritants E)))))
	  ;;This raises an error at  expand time, because DUMMY is not a
	  ;;field name.
	  (eval '(with-record-accessors/mutators <gamma>
		     (dummy)
		   #t)
		(environment '(nausicaa)
			     '(records)
			     '(for (records-lib) expand run))))
      => '((message   . "unknown field names in record type hierarchy")
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
      => '((message   . "attempt to create mutator for immutable record field")
	   (irritants . (b))))


    #f)

  #t)


;;;; done

(check-report)

;;; end of file
