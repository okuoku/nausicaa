;;;
;;;Part of: Nausicaa
;;;Contents: low level one dimensional values library
;;;Date: Wed Jun 10, 2009
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



(library (one-dimension)
  (export

    ;; range constructors
    %make-range %range-copy

    ;; range inspection
    %range? %range-length %range-contains?

    ;; range comparison
    %range=? %range<? %range<=?
    %range-start<? %range-start<=?
    %range-contiguous? %range-overlapping?
    %range-subset? %range-strict-subset?

    ;; range set operations
    %range-concatenate
    %range-union %range-intersection %range-difference
    %range-in-first-only

    ;; range list operations
    %range-for-each %range-fold %range-every %range-any
    %range->list

    ;; domain constructors
    %make-domain %domain-copy

    ;; domain inspection
    %domain? %domain-size %domain-empty? %domain-contains?

    ;; domain mutation
    %domain-add-item %domain-add-range

    ;; domain comparison
    %domain=? %domain<?

    ;; domain set operations
    %domain-intersection %domain-union %domain-difference
    %domain-complement %domain-subset? %domain-strict-subset?

    ;; domain list operations
    %domain-for-each %domain-every %domain-any
    %domain-fold %domain->list)
  (import (rnrs)
    (lists))


;;;; ranges

(define (%make-range start past item? item<?)
  (if (and (item? start)
	   (item? past)
	   (item<? start past))
      (cons start past)
    (assertion-violation '%make-range
      "invalid range limits" (list start past))))

(define (%range-copy range item-copy)
  (cons (item-copy (car range)) (item-copy (cdr range))))

(define (%range? obj item? item<?)
  (and (pair? obj)
       (let ((start (car obj))
	     (past  (cdr obj)))
	 (and (item? start)
	      (item? past)
	      (item<? start past)))))

(define (%range-contains? range obj item<? item<=?)
  (and (item<=? (car range) obj)
       (item<? obj (cdr range))))

(define (%range-length range item-)
  (item- (cdr range) (car range)))

(define (%range=? range-a range-b item=?)
  (or (eq? range-a range-b)
      (and (item=? (car range-a) (car range-b))
	   (item=? (cdr range-a) (cdr range-b)))))

(define (%range<? range-a range-b item<=?)
  (item<=? (cdr range-a) (car range-b)))

(define (%range<=? range-a range-b item<=?)
  (item<=? (cdr range-b) (car range-a)))

(define (%range-start<? range-a range-b item<?)
  (item<? (car range-a) (car range-b)))

(define (%range-start<=? range-a range-b item<=?)
  (item<=? (car range-a) (car range-b)))

(define (%range-contiguous? range-a range-b item=?)
  (or (item=? (cdr range-a) (car range-b))
      (item=? (cdr range-b) (car range-a))))

(define (%range-overlapping? range-a range-b item<? item<=?)
  (let ((start-a (car range-a)) (past-a (cdr range-a))
	(start-b (car range-b)) (past-b (cdr range-b)))
    (or (and (item<=? start-a start-b) (item<? start-b past-a))
	(and (item<=? start-b start-a) (item<? start-a past-b)))))

(define (%range-subset? range-a range-b item<=?)
  (and (item<=? (car range-a) (car range-b))
       (item<=? (cdr range-b) (cdr range-a))))

(define (%range-strict-subset? range-a range-b item<? item<=?)
  (or (and (item<=? (car range-a) (car range-b))
	   (item<?  (cdr range-b) (cdr range-a)))
      (and (item<?  (car range-a) (car range-b))
	   (item<=? (cdr range-b) (cdr range-a)))))

(define (%range-concatenate range-a range-b item<?)
  (let ((min (lambda (a b) (if (item<? a b) a b)))
	(max (lambda (a b) (if (item<? b a) a b))))
    (cons (min (car range-a) (car range-b))
	  (max (cdr range-a) (cdr range-b)))))

(define (%range-intersection range-a range-b item<? item<=?)
  (let ((start-a (car range-a)) (past-a (cdr range-a))
	(start-b (car range-b)) (past-b (cdr range-b))
	(min (lambda (a b) (if (item<? a b) a b)))
	(max (lambda (a b) (if (item<? b a) a b))))
    (if (or (item<=? start-a start-b past-a)
	    (item<=? start-b start-a past-b))
	(cons (max start-a start-b)
	      (min past-a past-b))
      #f)))

(define (%range-union range-a range-b item=? item<? item<=?)
  (let ((start-a (car range-a)) (past-a (cdr range-a))
	(start-b (car range-b)) (past-b (cdr range-b))
	(min (lambda (a b) (if (item<? a b) a b)))
	(max (lambda (a b) (if (item<? b a) a b))))
    (cond
     ((item=? past-a start-b)
      (values #f (cons start-a past-b)))
     ((item=? past-b start-a)
      (values #f (cons start-b past-a)))
     ((or (item<=? start-a start-b past-a)
	  (item<=? start-b start-a past-b))
      (values #f (cons (min start-a start-b)
		       (max past-a past-b))))
     ((item<? start-a start-b)
      (values range-a range-b))
     (else
      (values range-b range-a)))))

(define (%range-difference range-a range-b item=? item<?)
  (let-values (((range-a range-b) (if (%range-start<? range-a range-b item<?)
				      (values range-a range-b)
				    (values range-b range-a))))
    (let ((start-a (car range-a)) (past-a (cdr range-a))
	  (start-b (car range-b)) (past-b (cdr range-b)))
      (cond
       ((item=? past-a start-b)
	(values #f (cons start-a past-b)))

       ((item=? start-a start-b)
	(values #f (cond
		    ((item<? past-a past-b)
		     (cons past-a past-b))
		    ((item<? past-b past-a)
		     (cons past-b past-a))
		    (else
		     #f))))

       ((item=? past-a past-b)
	(values #f (cond
		    ((item<? start-a start-b)
		     (cons start-a start-b))
		    ;;START-A cannot  be > of START-B  because we sorted
		    ;;them before.
		    (else
		     #f))))

       ((item<? start-a start-b past-a)
	(if (item<? start-a past-b past-a)
	    (values (cons start-a start-b)
		    (cons past-b past-a))
	  (values (cons start-a start-b)
		  (cons past-a past-b))))

       ((item<? start-a start-b)
	(values range-a range-b))
       (else
	(values range-b range-a))))))

(define (%range-in-first-only range-a range-b item<? item<=?)
  (cond
   ((%range<? range-b range-a item<=?)
    (values #f #f))

   ((%range<? range-a range-b item<=?)
    (values range-a #f))

   (else
    ;;Here we know they are overlapping.
    (let ((start-a (car range-a)) (past-a (cdr range-a))
	  (start-b (car range-b)) (past-b (cdr range-b)))
      (values
       (and (item<=? start-a start-b)
	    (cons start-a start-b))
       (and (item<? past-b past-a)
	    (cons past-b past-a))
	)))))

(define (%range-for-each proc range item<=? item-next)
  (let ((start (car range))
	(past  (cdr range)))
    (do ((i start (item-next i)))
	((item<=? past i))
      (proc i))))

(define (%range-every proc range item<? item-next)
  (let ((past (cdr range)))
    (let loop ((i (car range)))
      (if (item<? i past)
	  (and (proc i)
	       (loop (item-next i)))
	#t))))

(define (%range-any proc range item<? item-next)
  (let ((past (cdr range)))
    (let loop ((i (car range)))
      (if (item<? i past)
	  (or (proc i)
	      (loop (item-next i)))
	#f))))

(define (%range-fold kons knil range item<? item-next)
  (let ((past (cdr range)))
    (let loop ((i (car range))
	       (knil knil))
      (if (item<? i past)
	  (loop (item-next i) (kons i knil))
	knil))))

(define (%range->list range item<? item-next)
  (%range-fold cons '() range item<? item-next))


;;;; domains

(define (%make-domain item? item=? item<? item<=? item-next . args)
  (let loop ((args args)
	     (domain '()))
    (if (null? args)
	domain
      (let ((thing (car args)))
	(cond
	 ((item? thing)
	  (loop (cdr args) (%domain-add-item domain thing item? item=? item<? item<=? item-next)))
	 ((and (pair? thing)
	       (let ((first (car thing))
		     (last  (cdr thing)))
		 (and (item? first)
		      (item? last)
		      (item<=? first last))))
	  (loop (cdr args)
		(%domain-add-range domain (cons (car thing)
						(item-next (cdr thing)))
				   item=? item<? item<=?)))
	 (else
	  (assertion-violation '%make-domain
	    "invalid element for domain" thing)))))))

(define (%domain-copy domain item-copy)
  (let loop ((x domain))
    (if (pair? x)
	(cons (loop (car x))
	      (loop (cdr x)))
      (item-copy x))))

(define (%domain-add-item domain obj item? item=? item<? item<=? item-next)
  (if (item? obj)
      (%domain-add-range domain (cons obj (item-next obj)) item=? item<? item<=?)
    (assertion-violation '%domain-add-item
      "expected character as new item" obj)))

(define (%domain-add-range domain new-range item=? item<? item<=?)
  (let loop ((domain domain)
	     (result '()))
    (if (%domain-empty? domain)
	(reverse (cons new-range result))
      (let ((range (car domain)))
;;;	  (write (list 'range range 'new-range new-range))(newline)
	(cond
	 ((%range=? range new-range item=?)
;;;	    (write (list 'equal range new-range))(newline)
	  (append-reverse (cons range result) (cdr domain)))
	 ((%range-overlapping? range new-range item<? item<=?)
;;;	    (write (list 'overlapping range new-range))(newline)
	  (let loop2 ((domain       (cdr domain))
		      (new-range (let-values (((head tail)
					       (%range-union range new-range
							     item=? item<? item<=?)))
				   tail)))
	    (if (null? domain)
		(reverse (cons new-range result))
	      (let ((range (car domain)))
		(cond ((%range-contiguous? range new-range item=?)
		       (loop2 (cdr domain) (%range-concatenate range new-range item<?)))
		      ((%range-overlapping? range new-range item<? item<=?)
		       (loop2 (cdr domain) (let-values (((head tail)
							 (%range-union range new-range
								       item=? item<? item<=?)))
					  tail)))
		      (else
		       (append-reverse (cons new-range (cons range result))
				       domain)))))))
	 ((%range-contiguous? range new-range item=?)
;;;	    (write (list 'contig range new-range))(newline)
	  (let loop2 ((domain    (cdr domain))
		      (new-range (%range-concatenate range new-range item<?)))
	    (if (null? domain)
		(reverse (cons new-range result))
	      (let ((range (car domain)))
		(cond ((%range-contiguous? range new-range item=?)
		       (loop2 (cdr domain) (%range-concatenate range new-range item<?)))
		      ((%range-overlapping? range new-range item<? item<=?)
		       (loop2 (cdr domain) (let-values (((head tail)
							 (%range-union range new-range
								       item=? item<? item<=?)))
					  tail)))
		      (else
		       (append-reverse (cons new-range (cons range result))
				       domain)))))))
	 ((%range<? new-range range item<=?)
;;;	    (write (list 'less range new-range))(newline)
	  (append-reverse (cons range (cons new-range result))
			  (cdr domain)))
	 (else
;;;	    (write (list 'other range new-range))(newline)
	  (loop (cdr domain) (cons range result))))))))

(define (%domain? domain item? item<? item<=? lower-bound upper-bound)
  (let loop ((domain domain))
    (if (null? domain)
	#t
      (let ((range (car domain)))
	(cond
	 ((not (%range? range item? item<?))
	  #f)
	 ((item<? (car range) lower-bound)
	  #f)
	 ((item<=? upper-bound (cdr range))
	  #f)
	 (else
	  (loop (cdr domain))))))))

(define (%domain-size domain item-)
  (fold (lambda (range size)
	  (+ size (%range-length range item-)))
	0 domain))

(define %domain-empty? null?)

(define (%domain-contains? domain obj item<? item<=?)
  (any (lambda (range)
	 (%range-contains? range obj item<? item<=?))
    domain))

(define (%domain=? domain-a domain-b item=?)
  (or (eq? domain-a domain-b)
      (cond
       ((null? domain-a) #f)
       ((null? domain-b) #f)
       (else
	(every (lambda (a b) (%range=? a b item=?))
	  domain-a domain-b)))))

(define (%domain<? domain-a domain-b item<=?)
  (cond ((null? domain-a) #f)
	((null? domain-b) #f)
	(else
	 (%range<? (last domain-a) (car domain-b) item<=?))))

(define (%domain-intersection domain-a domain-b item=? item<? item<=?)
  (let loop ((result	'())
	     (domain-a	domain-a)
	     (domain-b	domain-b))
    (if (or (%domain-empty? domain-a)
	    (%domain-empty? domain-b))
	(reverse result)
      (let ((range-a	(car domain-a))
	    (range-b	(car domain-b)))
;;;	  (write (list 'processing range-a range-b))(newline)
	(cond
	 ((%range=? range-a range-b item=?)
;;;	    (write (list 'equal range-a range-b))(newline)
	  (loop (cons range-a result)
		(cdr domain-a) (cdr domain-b)))
	 ((%range-overlapping? range-a range-b item<? item<=?)
;;;	    (write (list 'overlapping range-a range-b))(newline)
	  (loop (cons (%range-intersection range-a range-b item<? item<=?) result)
		(cdr domain-a) (cdr domain-b)))
	 ((%range<? range-a range-b item<=?)
;;;	    (write (list 'less-than range-a range-b))(newline)
	  (loop result (cdr domain-a) domain-b))
	 ((%range<? range-b range-a item<=?)
;;;	    (write (list 'greater-than range-a range-b))(newline)
	  (loop result domain-a (cdr domain-b)))
	 (else
	  (assertion-violation '%domain-intersection
	    "internal error processing ranges" (list range-a range-b))))))))

(define (%domain-union domain-a domain-b item=? item<? item<=?)
  (define (finish result domain)
    (if (null? result)
	domain
      (let loop ((result result)
		 (domain domain))
	(if (%domain-empty? domain)
	    (reverse result)
	  (let ((range (car domain))
		(top   (car result)))
	    (cond
	     ((or (%range-overlapping? top range item<? item<=?)
		  (%range-contiguous? top range item=?))
	      (let-values (((head tail) (%range-union top range item=? item<? item<=?)))
		(loop (let ((result (if head
					(cons head (cdr result))
				      (cdr result))))
			(if tail
			    (cons tail result)
			  result))
		      (cdr domain))))
	     (else
	      (loop (cons range result) (cdr domain)))))))))
  (let loop ((result '())
	     (domain-a domain-a)
	     (domain-b domain-b))
    (cond
     ((%domain-empty? domain-a)
      (finish result domain-b))
     ((%domain-empty? domain-b)
      (finish result domain-a))
     (else
      (let ((range-a (car domain-a))
	    (range-b (car domain-b)))
	(cond
	 ((and (not (null? result)) (%range-contiguous? (car result) range-a item=?))
	  (loop (cons (%range-concatenate (car result) range-a item<?) (cdr result))
		(cdr domain-a) domain-b))

	 ((and (not (null? result)) (%range-contiguous? (car result) range-b item=?))
	  (loop (cons (%range-concatenate (car result) range-b item<?) (cdr result))
		domain-a (cdr domain-b)))

	 ((and (not (null? result)) (%range-overlapping? (car result) range-a item<? item<=?))
	  (let-values (((head tail) (%range-union (car result) range-a item=? item<? item<=?)))
	    (loop (cons tail (cdr result)) (cdr domain-a) domain-b)))

	 ((and (not (null? result)) (%range-overlapping? (car result) range-b item<? item<=?))
	  (let-values (((head tail) (%range-union (car result) range-b item=? item<? item<=?)))
	    (loop (cons tail (cdr result)) domain-a (cdr domain-b))))

	 ((%range=? range-a range-b item=?)
	  (loop (cons range-a result) (cdr domain-a) (cdr domain-b)))

	 ((%range-contiguous? range-a range-b item=?)
	  (loop (cons (%range-concatenate range-a range-b item<?) result) (cdr domain-a) (cdr domain-b)))

	 ((%range-overlapping? range-a range-b item<? item<=?)
	  (let-values (((head tail) (%range-union range-a range-b item=? item<? item<=?)))
	    (loop (cons tail result) (cdr domain-a) (cdr domain-b))))

	 ((%range<? range-a range-b item<=?)
	  (loop (cons range-b (cons range-a result)) (cdr domain-a) (cdr domain-b)))

	 ((%range<? range-b range-a item<=?)
	  (loop (cons range-a (cons range-b result)) (cdr domain-a) (cdr domain-b)))

	 (else
	  (assertion-violation '%domain-union
	    "internal error processing ranges" (list range-a range-b)))))))))

(define (%domain-difference domain-a domain-b item=? item<? item<=?)
  (define (finish result domain)
    (if (null? result)
	domain
      (let loop ((result result)
		 (domain domain))
	(if (%domain-empty? domain)
	    (reverse result)
	  (let ((range (car domain))
		(top   (car result)))
	    (if (%range-overlapping? top range item<? item<=?)
		(let-values (((head tail) (%range-difference top range item=? item<?)))
		  (loop (let ((result (if head
					  (cons head (cdr result))
					(cdr result))))
			  (if tail
			      (cons tail result)
			    result))
			(cdr domain)))
	      (loop (cons range result) (cdr domain))))))))
  (define (cons-head-tail head tail result)
    (let ((result (if head (cons head result) result)))
      (if tail (cons tail result) result)))
  (let loop ((result '())
	     (domain-a domain-a)
	     (domain-b domain-b))
;;;    (write (list 'result result 'domain-a domain-a 'domain-b domain-b))(newline)
    (cond
     ((and (%domain-empty? domain-a) (%domain-empty? domain-b))
      (reverse result))
     ((%domain-empty? domain-a)
      (finish result domain-b))
     ((%domain-empty? domain-b)
      (finish result domain-a))
     (else
      (let ((range-a (car domain-a))
	    (range-b (car domain-b)))
	(cond
	 ((and (not (null? result)) (%range-contiguous? (car result) range-a item=?))
;;;	  (write (list 'result-contiguous-a (car result) range-a))(newline)
	  (loop (cons (%range-concatenate (car result) range-a item<?) (cdr result))
		(cdr domain-a) domain-b))

	 ((and (not (null? result)) (%range-contiguous? (car result) range-b item=?))
;;;	  (write (list 'result-contiguous-b (car result) range-b))(newline)
	  (loop (cons (%range-concatenate (car result) range-b item<?) (cdr result))
		domain-a (cdr domain-b)))

	 ((and (not (null? result)) (%range-overlapping? (car result) range-a item<? item<=?))
;;;	  (write (list 'res-overlapping-a (car result) range-a))(newline)
	  (let-values (((head tail) (%range-difference (car result) range-a item=? item<?)))
	    (loop (cons-head-tail head tail (cdr result)) (cdr domain-a) domain-b)))

	 ((and (not (null? result)) (%range-overlapping? (car result) range-b item<? item<=?))
;;;	  (write (list 'res-overlapping-b (car result) range-b))(newline)
	  (let-values (((head tail) (%range-difference (car result) range-b item=? item<?)))
	    (loop (cons-head-tail head tail (cdr result)) domain-a (cdr domain-b))))

	 ((%range=? range-a range-b item=?)
;;;	  (write (list 'equal range-a range-b))(newline)
	  (loop result (cdr domain-a) (cdr domain-b)))

	 ((%range-contiguous? range-a range-b item=?)
;;;	  (write (list 'contiguous range-a range-b))(newline)
	  (loop (cons (%range-concatenate range-a range-b item<?) result) (cdr domain-a) (cdr domain-b)))

	 ((%range-overlapping? range-a range-b item<? item<=?)
;;;	  (write (list 'overlapping range-a range-b))(newline)
	  (let-values (((head tail) (%range-difference range-a range-b item=? item<=?)))
	    (loop (cons-head-tail head tail result) (cdr domain-a) (cdr domain-b))))

	 ((%range<? range-a range-b item<=?)
;;;	  (write (list 'lesser range-a range-b))(newline)
	  (loop (cons range-b (cons range-a result)) (cdr domain-a) (cdr domain-b)))

	 ((%range<? range-b range-a item<=?)
;;;	  (write (list 'greater range-a range-b))(newline)
	  (loop (cons range-a (cons range-b result)) (cdr domain-a) (cdr domain-b)))

	 (else
	  (assertion-violation '%domain-union
	    "internal error processing ranges" (list range-a range-b)))))))))

(define (%domain-complement domain universe item=? item<? item<=?)
  (define (cons-head-tail head tail result)
    (let ((result (if head (cons head result) result)))
      (if tail (cons tail result) result)))
  (if (null? domain)
      universe
    (let loop ((result	'())
	       (universe	universe)
	       (domain	domain))
      (cond ((%domain-empty? universe)
	     result)

	    ((%domain-empty? domain)
	     (append-reverse universe result))

	    (else
	     (let ((range-a (car universe))
		   (range-b (car domain)))
	       (cond ((%range<? range-a range-b item<=?)
		      (loop (cons range-a result) (cdr universe) domain))

		     ((%range=? range-a range-b item=?)
		      (loop result (cdr universe) (cdr domain)))

		     ((%range-overlapping? range-a range-b item<? item<=?)
		      (let-values (((head tail)
				    (%range-in-first-only range-a range-b item<? item<=?)))
			(loop (cons-head-tail head tail result) (cdr universe) domain)))
		     )))))))

(define (%domain-subset? domain-a domain-b item<=?)
  (let loop ((domain-a domain-a)
	     (domain-b domain-b))
    (cond
     ((%domain-empty? domain-b) #t)
     ((%domain-empty? domain-a) #f)
     (else
      (let ((range-a (car domain-a))
	    (range-b (car domain-b)))
	(if (%range-subset? range-a range-b item<=?)
	    (loop domain-a (cdr domain-b))
	  (loop (cdr domain-a) domain-b)))))))

(define (%domain-strict-subset? domain-a domain-b item<? item<=?)
  (let loop ((subset? #f)
	     (domain-a domain-a)
	     (domain-b domain-b))
    (cond
     ((%domain-empty? domain-b) subset?)
     ((%domain-empty? domain-a) #f)
     (else
      (let ((range-a (car domain-a))
	    (range-b (car domain-b)))
	(cond ((%range<? range-a range-b item<=?)
	       (loop #t (cdr domain-a) domain-b))
	      ((%range-strict-subset? range-a range-b item<? item<=?)
	       (loop #t domain-a (cdr domain-b)))
	      ((%range-subset? range-a range-b item<=?)
	       (loop subset? domain-a (cdr domain-b)))
	      (else
	       #f
	       ;;(loop #t (cdr domain-a) domain-b)
	       )))))))

(define (%domain-for-each proc domain item<=? item-next)
  (for-each (lambda (range)
	      (%range-for-each proc range item<=? item-next))
    domain))

(define (%domain-every proc domain item<? item-next)
  (every (lambda (range)
	   (%range-every proc range item<? item-next))
    domain))

(define (%domain-any proc domain item<? item-next)
  (any (lambda (range)
	 (%range-any proc range item<? item-next))
    domain))

(define (%domain-fold kons knil domain item<? item-next)
  (let loop ((domain domain)
	     (knil knil))
    (if (null? domain)
	knil
      (loop (cdr domain) (%range-fold kons knil (car domain) item<? item-next)))))

(define (%domain->list domain item<? item-next)
  (reverse (apply append (map (lambda (range)
				(%range->list range item<? item-next))
			   domain))))


;;;; done

)

;;; end of file
