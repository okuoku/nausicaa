;;;
;;;Part of: Nausicaa/SRFI
;;;Contents: tests for list-lib helpers
;;;Date: Mon Dec 29, 2008
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

(import (r6rs)
  (check-lib))

(check-set-mode! 'report-failed)

(define-syntax make-queue
  (syntax-rules ()
    ((_ ?elm)
     (let* ((v		?elm)
	    (pair	(cons v '())))
       (cons pair pair)))))

(define-syntax enqueue!
  (syntax-rules ()
    ((_ ?q ?obj)
     (let ((q ?q)
	   (h (cons ?obj '())))
       (set-cdr! (cdr q) h)
       (set-cdr! q h)
       q))))



;;ELLS must  be a non-null list  of lists.  If  a list in ELLS  is null:
;;return null; else return the list of the cars.
(define (%cars ells)
  (let ((next	(car ells)))
    (if (null? next)
	'()
      (let loop ((cars	(make-queue (car next)))
		 (ells	(cdr ells)))
	(if (null? ells)
	    (car cars)
	  (let ((next (car ells)))
 	    (if (null? next)
 		'()
	      (loop (enqueue! cars (car next))
		    (cdr ells)))))))))

;;; --------------------------------------------------------------------

(check
    (%cars '(()))
  => '())

(check
    (%cars '((1)))
  => '(1))

(check
    (%cars '((1 2 3)
	     (10 20 30)))
  => '(1 10))

(check
    (%cars '((1 2 3)
	     (10 20 30)
	     (100 200 300)))
  => '(1 10 100))

(check
    (%cars '(()
	     (10 20 30)
	     (100 200 300)))
  => '())

(check
    (%cars '((1 2 3)
	     ()
	     (100 200 300)))
  => '())

(check
    (%cars '((1 2 3)
	     (10 20 30)
	     ()))
  => '())



;;; ELLS must be a list of lists.   If one of the lists in ELLS is null:
;;; return null; else return the list of cars of the lists in ELLS, with
;;; KNIL appended as last element.
(define (%cars+knil ells knil)
  (let ((next (car ells)))
    (if (null? next)
	'()
      (let loop ((cars	(make-queue (car next)))
		 (ells	(cdr ells)))
	(if (null? ells)
	    (car (enqueue! cars knil))
	  (let ((next (car ells)))
	    (if (null? next)
		'()
	      (loop (enqueue! cars (car next))
		    (cdr ells)))))))))

;;; --------------------------------------------------------------------

(check
    (%cars+knil '(()) 999)
  => '())

(check
    (%cars+knil '((1)) 999)
  => '(1 999))

(check
    (%cars+knil '((1 2 3)
		  (10 20 30))
		999)
  => '(1 10 999))

(check
    (%cars+knil '((1 2 3)
		  (10 20 30)
		  (100 200 300))
		999)
  => '(1 10 100 999))

(check
    (%cars+knil '(()
		  (10 20 30)
		  (100 200 300))
		999)
  => '())

(check
    (%cars+knil '((1 2 3)
		  ()
		  (100 200 300))
		999)
  => '())

(check
    (%cars+knil '((1 2 3)
		  (10 20 30)
		  ())
		999)
  => '())




;;ELLS must  be a non-null list  of lists.  If  a list in ELLS  is null:
;;return null; else return the list of the cdrs.
(define (%cdrs ells)
  (let ((next	(car ells)))
    (if (null? next)
	'()
      (let loop ((ells	(cdr ells))
		 (cdrs	(make-queue (cdr next))))
	(if (null? ells)
	    (car cdrs)
	  (let ((next	(car ells)))
	    (if (null? next)
		'()
	      (loop (cdr ells)
		    (enqueue! cdrs (cdr next))))))))))

;;; --------------------------------------------------------------------

(check
    (%cdrs '(()))
  => '())

(check
    (%cdrs '((1)))
  => '(()))

(check
    (%cdrs '((1)
	     (10)))
  => '(()
       ()))

(check
    (%cdrs '((1)
	     (10)
	     (100)))
  => '(()
       ()
       ()))

(check
    (%cdrs '((1 2 3)
	     (10 20 30)))
  => '((2 3)
       (20 30)))

(check
    (%cdrs '((1 2 3)
	     (10 20 30)
	     (100 200 300)))
  => '((2 3)
       (20 30)
       (200 300)))

(check
    (%cdrs '(()
	     (10 20 30)
	     (100 200 300)))
  => '())

(check
    (%cdrs '((1 2 3)
	     ()
	     (100 200 300)))
  => '())

(check
    (%cdrs '((1 2 3)
	     (10 20 30)
	     ()))
  => '())




;;ELLS must  be a non-null list  of lists.  If  a list in ELLS  is null:
;;return 2 null  values; else return 2 values: the list  of cars and the
;;list of cdrs.
(define (%cars/cdrs ells)
  (let ((next (car ells)))
    (if (null? next)
	(values '() '())
      (let loop ((cars	(make-queue (car next)))
		 (cdrs	(make-queue (cdr next)))
		 (ells	(cdr ells)))
	(if (null? ells)
	    (values (car cars)
		    (car cdrs))
	  (let ((next (car ells)))
	    (if (null? next)
		(values '() '())
	      (loop (enqueue! cars (car next))
		    (enqueue! cdrs (cdr next))
		    (cdr ells)))))))))

;;; --------------------------------------------------------------------

(check
    (call-with-values
	(lambda ()
	  (%cars/cdrs '(())))
      list)
  => '(()
       ()))

(check
    (call-with-values
	(lambda ()
	  (%cars/cdrs '((1))))
      list)
  => '((1)
       (())))

(check
    (call-with-values
	(lambda ()
	  (%cars/cdrs '((1)
			(10))))
      list)
  => '((1 10)
       (()
	())))

(check
    (call-with-values
	(lambda ()
	  (%cars/cdrs '((1)
			(10)
			(100))))
      list)
  => '((1 10 100)
       (()
	()
	())))

(check
    (call-with-values
	(lambda ()
	  (%cars/cdrs '((1 2 3)
			(10 20 30))))
      list)
  => '((1 10)
       ((2 3)
	(20 30))))

(check
    (call-with-values
	(lambda ()
	  (%cars/cdrs '((1 2 3)
			(10 20 30)
			(100 200 300))))
      list)
  => '((1 10 100)
       ((2 3)
	(20 30)
	(200 300))))

(check
    (call-with-values
	(lambda ()
	  (%cars/cdrs '(()
			(10 20 30)
			(100 200 300))))
      list)
  => '(()
       ()))

(check
    (call-with-values
	(lambda ()
	  (%cars/cdrs '((1 2 3)
			()
			(100 200 300))))
      list)
  => '(()
       ()))

(check
    (call-with-values
	(lambda ()
	  (%cars/cdrs '((1 2 3)
			(10 20 30)
			())))
      list)
  => '(()
       ()))




;;ELLS must  be a non-null list  of lists.  If  a list in ELLS  is null:
;;return 2 null values; else return 2 values: the list of cars with KNIL
;;appended, and the list of cdrs.
(define (%cars+knil/cdrs ells knil)
  (let ((next	(car ells)))
    (if (null? next)
	(values '() '())
      (let loop ((cars	(make-queue (car next)))
		 (cdrs	(make-queue (cdr next)))
		 (ells	(cdr ells)))
	(if (null? ells)
	    (values (car (enqueue! cars knil))
		    (car cdrs))
	  (let ((next	(car ells)))
	    (if (null? next)
		(values '() '())
	      (loop (enqueue! cars (car next))
		    (enqueue! cdrs (cdr next))
		    (cdr ells)))))))))

;;; --------------------------------------------------------------------

(check
    (call-with-values
	(lambda ()
	  (%cars+knil/cdrs '(()) 999))
      list)
  => '(()
       ()))

(check
    (call-with-values
	(lambda ()
	  (%cars+knil/cdrs '((1)) 999))
      list)
  => '((1 999)
       (())))

(check
    (call-with-values
	(lambda ()
	  (%cars+knil/cdrs '((1)
			     (10))
			   999))
      list)
  => '((1 10 999)
       (()
	())))

(check
    (call-with-values
	(lambda ()
	  (%cars+knil/cdrs '((1)
			     (10)
			     (100))
			   999))
      list)
  => '((1 10 100 999)
       (()
	()
	())))

(check
    (call-with-values
	(lambda ()
	  (%cars+knil/cdrs '((1 2 3)
			     (10 20 30))
			   999))
      list)
  => '((1 10 999)
       ((2 3)
	(20 30))))

(check
    (call-with-values
	(lambda ()
	  (%cars+knil/cdrs '((1 2 3)
			     (10 20 30)
			     (100 200 300))
			   999))
      list)
  => '((1 10 100 999)
       ((2 3)
	(20 30)
	(200 300))))

(check
    (call-with-values
	(lambda ()
	  (%cars+knil/cdrs '(()
			     (10 20 30)
			     (100 200 300))
			   999))
      list)
  => '(()
       ()))

(check
    (call-with-values
	(lambda ()
	  (%cars+knil/cdrs '((1 2 3)
			     ()
			     (100 200 300))
			   999))
      list)
  => '(()
       ()))

(check
    (call-with-values
	(lambda ()
	  (%cars+knil/cdrs '((1 2 3)
			     (10 20 30)
			     ())
			   999))
      list)
  => '(()
       ()))


;;;; done

(check-report)

;;; end of file
