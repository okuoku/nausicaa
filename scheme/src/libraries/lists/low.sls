;;;Derived from SRFI-1 list-processing library, reference implementation
;;;
;;;Copyright (c) 2008, 2009 Marco Maggi <marcomaggi@gna.org>
;;;Copyright (c) 1998, 1999 by Olin Shivers <shivers@ai.mit.edu>.
;;;Modified by Abdulaziz Ghuloum to port to Ikarus.
;;;Modified by Derick Eddington to port to R6RS.
;;;Modified by Marco Maggi for inclusion in Nausicaa.
;;;
;;;You may do as you please with  this code as long as you do not remove
;;;this copyright notice or hold me liable for its use.  Please send bug
;;;reports to <shivers@ai.mit.edu>.
;;;
;;;This is  a library of list-processing  and pair-processing functions.
;;;I wrote it after carefully  considering the functions provided by the
;;;libraries  found in  R4RS/R5RS Scheme,  MIT Scheme,  Gambit, RScheme,
;;;MzScheme,  slib, Common  Lisp,  Bigloo,  guile, T,  APL  and the  SML
;;;standard basis.  It is a pretty rich toolkit, providing a superset of
;;;the functionality found in any of the various Schemes I considered.
;;;
;;;This   implementation   is   intended   as   a   portable   reference
;;;implementation  for SRFI-1.   See the  porting notes  below  for more
;;;information.
;;;


#!r6rs
(library (lists low)
  (export

    ;; predicates
    %and/or-null?

    ;; queue
    %make-queue
    %enqueue!
    %queue-list-ref
    %queue-last-pair-ref
    %queue-last-pair-set!


    %cars		%cdrs		%cars/cdrs
    %cars*		%cdrs*		%cars/cdrs*
    %cars+cdrs*/no-test

    %cars+knil		%cars+knil/cdrs
    %cars+knil*		%cars+knil/cdrs*

    %knil+cars		%knil+cars/cdrs
    %knil+cars*		%knil+cars/cdrs*

    )
  (import (rnrs)
    (rnrs mutable-pairs))


;;;; helpers

(define error-message "expected lists of equal length")

(define-syntax car+cdr/stx
  (syntax-rules ()
    ((_ ?pair)
     (values (car ?pair) (cdr ?pair)))))

(define-syntax last-pair/stx
  (syntax-rules ()
    ((_ ?x)
     (let loop ((x ?x))
       (let ((d (cdr x)))
	 (if (pair? d)
	     (loop d)
	   x))))))

(define-syntax last-set!/stx
  (syntax-rules ()
    ((_ ?obj ?ell)
     (let* ((ell ?ell)
	    (lp  (last-pair/stx ell)))
       (set-cdr! lp (cons ?obj '()))
       ell))))

(define-syntax receive
  (syntax-rules ()
    ((_ formals expression b b* ...)
     (call-with-values
         (lambda () expression)
       (lambda formals b b* ...)))))


;;;; null predicates

(define (%and/or-null? . list-of-lists)
  (let loop ((and-nil #t)
	     (or-nil  #f)
	     (ells    list-of-lists))
    (if (null? ells)
	(values and-nil or-nil)
      (let ((nil (null? (car ells))))
	(loop (and nil and-nil)
	      (or  nil or-nil)
	      (cdr ells))))))


;;;; queues
;;
;; The following macros  handle queue values: Pairs whose  car is a list
;; and  whose  cdr is  the  last  pair in  the  list.   They allow  fast
;; insertion of elements at the end of the list.
;;
;;           -----------
;;   queue  | car | cdr |
;;           -----------
;;             |     |
;;         ----       ----------------
;;        |                           |
;;        v                           v
;;       ---    ---    ---    ---    ---
;; list | | |->| | |->| | |->| | |->| | |->()
;;       ---    ---    ---    ---    ---
;;       |      |      |      |      |
;;       o      o      o      o      o

(define %queue-list-ref		car)
(define %queue-last-pair-ref	cdr)
(define %queue-last-pair-set!	set-cdr!)

(define-syntax %make-queue
  (syntax-rules ()
    ((_ ?elm)
     (let* ((v		?elm)
	    (pair	(cons v '())))
       (cons pair pair)))))

(define-syntax %list->queue
  (syntax-rules ()
    ((_ ?ell)
     (let ((ell ?ell))
       (cons ell (last-pair/stx ell))))))

(define-syntax %enqueue!
  (syntax-rules ()
    ((_ ?queue ?obj)
     (let ((queue         ?queue)
	   (new-last-pair (cons ?obj '())))
       (set-cdr! (%queue-last-pair-ref queue) new-last-pair)
       (%queue-last-pair-set! queue new-last-pair)
       queue))))


;;;; cars and cdrs from list of lists, check for length

(define %cars
  (case-lambda
   ((list-of-lists)
    (%cars list-of-lists #f))
   ((list-of-lists who)
    (let-values (((and-nil or-nil) (apply %and/or-null? list-of-lists)))
      (cond (and-nil '())
	    (or-nil  (assertion-violation who
		       error-message list-of-lists))
	    (else
	     (map car list-of-lists)))))))

(define %cdrs
  (case-lambda
   ((list-of-lists)
    (%cdrs list-of-lists #f))
   ((list-of-lists who)
    (let-values (((and-nil or-nil) (apply %and/or-null? list-of-lists)))
      (cond (and-nil '())
	    (or-nil  (assertion-violation who
		       error-message list-of-lists))
	    (else
	     (map cdr list-of-lists)))))))

(define %cars/cdrs
  (case-lambda
   ((list-of-lists)
    (%cars/cdrs list-of-lists #f))
   ((list-of-lists who)
    (let-values (((and-nil or-nil) (apply %and/or-null? list-of-lists)))
      (cond (and-nil (values '() '()))
	    (or-nil  (assertion-violation who
		       error-message list-of-lists))
	    (else
	     (values (map car list-of-lists)
		     (map cdr list-of-lists))))))))


;;;; cars+knil and cdrs from list of lists, check for length

(define %cars+knil
  (case-lambda
   ((list-of-lists knil)
    (%cars+knil list-of-lists knil #f))
   ((list-of-lists knil who)
    (let-values (((and-nil or-nil) (apply %and/or-null? list-of-lists)))
      (cond (and-nil '())
	    (or-nil  (assertion-violation who
		       error-message list-of-lists))
	    (else
	     (last-set!/stx knil (map car list-of-lists))))))))

(define %cars+knil/cdrs
  (case-lambda
   ((list-of-lists knil)
    (%cars+knil/cdrs list-of-lists knil #f))
   ((list-of-lists knil who)
    (let-values (((and-nil or-nil) (apply %and/or-null? list-of-lists)))
      (cond (and-nil (values '() '()))
	    (or-nil  (assertion-violation who
		       error-message list-of-lists))
	    (else
	     (values (last-set!/stx knil (map car list-of-lists))
		     (map cdr list-of-lists))))))))

;;; --------------------------------------------------------------------

(define %knil+cars
  (case-lambda
   ((list-of-lists knil)
    (%knil+cars list-of-lists knil #f))
   ((list-of-lists knil who)
    (let-values (((and-nil or-nil) (apply %and/or-null? list-of-lists)))
      (cond (and-nil '())
	    (or-nil  (assertion-violation who
		       error-message list-of-lists))
	    (else
	     (cons knil (map car list-of-lists))))))))

(define %knil+cars/cdrs
  (case-lambda
   ((list-of-lists knil)
    (%knil+cars/cdrs list-of-lists knil #f))
   ((list-of-lists knil who)
    (let-values (((and-nil or-nil) (apply %and/or-null? list-of-lists)))
      (cond (and-nil (values '() '()))
	    (or-nil  (assertion-violation who
		       error-message list-of-lists))
	    (else
	     (values (cons knil (map car list-of-lists))
		     (map cdr list-of-lists))))))))


;;;; cars and cdrs from list of lists, no check for length

(define (%cars* list-of-lists)
  ;;LIST-OF-LISTS  must be  a  non-null list  of  lists.  If  a list  in
  ;;LIST-OF-LISTS  is null:  return null;  else return  the list  of the
  ;;cars.
  ;;
  (let ((next-list	(car list-of-lists)))
    (if (null?		next-list)
	'()
      (let loop ((cars		(%make-queue (car next-list)))
		 (list-of-lists	(cdr list-of-lists)))
	(if (null? list-of-lists)
	    (%queue-list-ref cars)
	  (let ((next-list (car list-of-lists)))
 	    (if (null? next-list)
 		'()
	      (loop (%enqueue! cars (car next-list))
		    (cdr list-of-lists)))))))))

(define (%cdrs* list-of-lists)
  ;;LIST-OF-LISTS  must be  a  non-null list  of  lists.  If  a list  in
  ;;LIST-OF-LISTS  is null:  return null;  else return  the list  of the
  ;;cdrs.
  ;;
  (let ((next-list (car list-of-lists)))
    (if (null? next-list)
	'()
      (let loop ((list-of-lists	(cdr list-of-lists))
		 (cdrs		(%make-queue (cdr next-list))))
	(if (null? list-of-lists)
	    (%queue-list-ref cdrs)
	  (let ((next-list (car list-of-lists)))
	    (if (null? next-list)
		'()
	      (loop (cdr list-of-lists)
		    (%enqueue! cdrs (cdr next-list))))))))))

(define (%cars/cdrs* list-of-lists)
  ;;LIST-OF-LISTS  must be  a  non-null list  of  lists.  If  a list  in
  ;;LIST-OF-LISTS is null:  return 2 null values; else  return 2 values:
  ;;the list of cars and the list of cdrs.
  ;;
  (let ((next-list (car list-of-lists)))
    (if (null? next-list)
	(values '() '())
      (let loop ((cars		(%make-queue (car next-list)))
		 (cdrs		(%make-queue (cdr next-list)))
		 (list-of-lists	(cdr list-of-lists)))
	(if (null? list-of-lists)
	    (values (%queue-list-ref cars)
		    (%queue-list-ref cdrs))
	  (let ((next-list (car list-of-lists)))
	    (if (null? next-list)
		(values '() '())
	      (loop (%enqueue! cars (car next-list))
		    (%enqueue! cdrs (cdr next-list))
		    (cdr list-of-lists)))))))))

(define (%cars+cdrs*/no-test lists)
  ;;Like %CARS/CDRS, but blow up if any list is empty.
  ;;
  (let recur ((lists lists))
    (if (pair? lists)
	(receive (list other-lists)
	    (car+cdr/stx lists)
	  (let-values (((a d)       (car+cdr/stx list))
		       ((cars cdrs) (recur other-lists)))
	    (values (cons a cars) (cons d cdrs))))
      (values '() '()))))


;;;; cars and cdrs and knil from list of lists, no check for length

(define (%cars+knil* list-of-lists knil)
  ;;LIST-OF-LISTS  must be  a list  of lists.   If one  of the  lists in
  ;;LIST-OF-LISTS is null: return null;  else return the list of cars of
  ;;the lists in LIST-OF-LISTS, with KNIL appended as last element.
  ;;
  (let ((next-list (car list-of-lists)))
    (if (null? next-list)
	'()
      (let loop ((cars	(%make-queue (car next-list)))
		 (list-of-lists	(cdr list-of-lists)))
	(if (null? list-of-lists)
	    (%queue-list-ref (%enqueue! cars knil))
	  (let ((next-list (car list-of-lists)))
	    (if (null? next-list)
		'()
	      (loop (%enqueue! cars (car next-list))
		    (cdr list-of-lists)))))))))

(define (%knil+cars* list-of-lists knil)
  ;;LIST-OF-LISTS  must be  a list  of lists.   If one  of the  lists in
  ;;LIST-OF-LISTS is null: return null;  else return the list of cars of
  ;;the lists in LIST-OF-LISTS, with KNIL prepended as first element.
  ;;
  (let ((next-list (car list-of-lists)))
    (if (null? next-list)
	'()
      (let loop ((cars	(%make-queue (car next-list)))
		 (list-of-lists	(cdr list-of-lists)))
	(if (null? list-of-lists)
	    (cons knil (%queue-list-ref cars))
	  (let ((next-list (car list-of-lists)))
	    (if (null? next-list)
		'()
	      (loop (%enqueue! cars (car next-list))
		    (cdr list-of-lists)))))))))

(define (%cars+knil/cdrs* list-of-lists knil)
  ;;LIST-OF-LISTS  must be  a  non-null list  of  lists.  If  a list  in
  ;;LIST-OF-LISTS is null:  return 2 null values; else  return 2 values:
  ;;the list of cars with KNIL appended, and the list of cdrs.
  ;;
  (let ((next-list (car list-of-lists)))
    (if (null? next-list)
	(values '() '())
      (let loop ((cars		(%make-queue (car next-list)))
		 (cdrs		(%make-queue (cdr next-list)))
		 (list-of-lists	(cdr list-of-lists)))
	(if (null? list-of-lists)
	    (values (%queue-list-ref (%enqueue! cars knil))
		    (%queue-list-ref cdrs))
	  (let ((next-list (car list-of-lists)))
	    (if (null? next-list)
		(values '() '())
	      (loop (%enqueue! cars (car next-list))
		    (%enqueue! cdrs (cdr next-list))
		    (cdr list-of-lists)))))))))

(define (%knil+cars/cdrs* list-of-lists knil)
  ;;LIST-OF-LISTS  must be  a  non-null list  of  lists.  If  a list  in
  ;;LIST-OF-LISTS is null:  return 2 null values; else  return 2 values:
  ;;the list of cars with KNIL prepended, and the list of cdrs.
  ;;
  (let ((next-list (car list-of-lists)))
    (if (null? next-list)
	(values '() '())
      (let loop ((cars	(%make-queue (car next-list)))
		 (cdrs	(%make-queue (cdr next-list)))
		 (list-of-lists	(cdr list-of-lists)))
	(if (null? list-of-lists)
	    (values (cons knil (%queue-list-ref cars))
		    (%queue-list-ref cdrs))
	  (let ((next-list (car list-of-lists)))
	    (if (null? next-list)
		(values '() '())
	      (loop (%enqueue! cars (car next-list))
		    (%enqueue! cdrs (cdr next-list))
		    (cdr list-of-lists)))))))))


;;;; done

)

;;; end of file
