;;;
;;;Part of: Nausicaa
;;;Contents: low level one dimensional values library
;;;Date: Wed Jun 10, 2009
;;;
;;;Abstract
;;;
;;;	This is the double-closed ranges version of the library.
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



(library (one-dimension-cc)
  (export

    %make-type-descriptor

    ;; range constructors
    %make-range %range-copy

    ;; range inspection
    %range? %range-length %range-contains?

    ;; range comparison
    %range=? %range<? %range<=?
    %range-start<? %range-start<=?
    %range-last<?  %range-last<=?
    (rename (%range-last<?  %range-past<?)
	    (%range-last<=? %range-past<=?))
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


;;;; type descriptor

(define-record-type (type-descriptor %make-type-descriptor %type-descriptor?)
  (fields (immutable item?)
	  (immutable item=?)
	  (immutable item<?)
	  (immutable item<=?)
	  (immutable item-min)
	  (immutable item-max)
	  (immutable item-prev)
	  (immutable item-next)
	  (immutable item-minus)
	  (immutable item-copy)))


;;;; ranges

(define %make-range
  (case-lambda
   ((type start)
    (%make-range type start start))
   ((type start last)
    (let ((item?   (type-descriptor-item?   type))
	  (item<=? (type-descriptor-item<=? type)))
      (if (and (item? start)
	       (item? last)
	       (item<=? start last))
	  (cons start last)
	(assertion-violation '%make-range
	  "invalid range limits" (list start last)))))))

(define (%range-copy type range)
  (let ((item-copy (type-descriptor-item-copy type)))
    (cons (item-copy (car range)) (item-copy (cdr range)))))

(define (%range? type obj)
  (and (pair? obj)
       (let ((start   (car obj))
	     (last    (cdr obj))
	     (item?   (type-descriptor-item? type))
	     (item<=? (type-descriptor-item<=? type)))
	 (and (item? start)
	      (item? last)
	      (item<=? start last)))))

(define (%range-contains? type range obj)
  (let ((item<=? (type-descriptor-item<=? type)))
    (and (item<=? (car range) obj)
	 (item<=? obj (cdr range)))))

(define (%range-length type range)
  (let ((item-minus (type-descriptor-item-minus type)))
    (item-minus (cdr range) (car range))))

(define (%range=? type range-a range-b)
  (let ((item=? (type-descriptor-item=? type)))
    (or (eq? range-a range-b)
	(and (item=? (car range-a) (car range-b))
	     (item=? (cdr range-a) (cdr range-b))))))

(define (%range<? type range-a range-b)
  (let ((item<? (type-descriptor-item<? type)))
    (item<? (cdr range-a) (car range-b))))

(define (%range<=? type range-a range-b)
  (let ((item<=? (type-descriptor-item<=? type)))
    (item<=? (cdr range-a) (cdr range-b))))

(define (%range-start<? type range-a range-b)
  (let ((item<? (type-descriptor-item<? type)))
    (item<? (car range-a) (car range-b))))

(define (%range-start<=? type range-a range-b)
  (let ((item<=? (type-descriptor-item<=? type)))
    (item<=? (car range-a) (car range-b))))

(define (%range-last<? type range-a range-b)
  (let ((item<? (type-descriptor-item<? type)))
    (item<? (cdr range-a) (cdr range-b))))

(define (%range-last<=? type range-a range-b)
  (let ((item<=? (type-descriptor-item<=? type)))
    (item<=? (cdr range-a) (cdr range-b))))

(define (%range-contiguous? type range-a range-b)
  (let ((item-minus (type-descriptor-item-minus type)))
    (or (= 2 (item-minus (car range-b) (cdr range-a)))
	(= 2 (item-minus (car range-a) (cdr range-b))))))

(define (%range-overlapping? type range-a range-b)
  (let ((item<=? (type-descriptor-item<=? type))
	(start-a (car range-a)) (last-a (cdr range-a))
	(start-b (car range-b)) (last-b (cdr range-b)))
    (or (and (item<=? start-a start-b) (item<=? start-b last-a))
	(and (item<=? start-b start-a) (item<=? start-a last-b)))))

(define (%range-subset? type range-a range-b)
  (let ((item<=? (type-descriptor-item<=? type)))
    (and (item<=? (car range-a) (car range-b))
	 (item<=? (cdr range-b) (cdr range-a)))))

(define (%range-strict-subset? type range-a range-b)
  (let ((item<? (type-descriptor-item<? type))
	(item<=? (type-descriptor-item<=? type)))
    (or (and (item<=? (car range-a) (car range-b))
	     (item<?  (cdr range-b) (cdr range-a)))
	(and (item<?  (car range-a) (car range-b))
	     (item<=? (cdr range-b) (cdr range-a))))))

(define (%range-concatenate type range-a range-b)
  (let ((item<? (type-descriptor-item<? type))
	(min (type-descriptor-item-min type))
	(max (type-descriptor-item-max type)))
    (cons (min (car range-a) (car range-b))
	  (max (cdr range-a) (cdr range-b)))))

(define (%range-intersection type range-a range-b)
  (let ((start-a (car range-a)) (last-a (cdr range-a))
	(start-b (car range-b)) (last-b (cdr range-b))
	(item<=? (type-descriptor-item<=? type))
	(min (type-descriptor-item-min type))
	(max (type-descriptor-item-max type)))
    (and (or (item<=? start-a start-b last-a)
	     (item<=? start-b start-a last-b))
	 (cons (max start-a start-b)
	       (min last-a last-b)))))

(define (%range-union type range-a range-b)
  (let ((start-a (car range-a)) (last-a (cdr range-a))
	(start-b (car range-b)) (last-b (cdr range-b))
	(item=?  (type-descriptor-item=?   type))
	(item<?  (type-descriptor-item<?   type))
	(item<=? (type-descriptor-item<=?  type))
	(min     (type-descriptor-item-min type))
	(max     (type-descriptor-item-max type))
	(item-minus (type-descriptor-item-minus type)))
    (cond
     ((or (item=? last-a start-b)
	  (= 2 (item-minus start-b last-a)))
      (values #f (cons start-a last-b)))
     ((or (item=? last-b start-a)
	  (= 2 (item-minus start-a last-b)))
      (values #f (cons start-b last-a)))
     ((or (item<=? start-a start-b last-a)
	  (item<=? start-b start-a last-b))
      (values #f (cons (min start-a start-b)
		       (max last-a last-b))))
     ((item<? start-a start-b)
      (values range-a range-b))
     (else
      (values range-b range-a)))))

(define (%range-difference type range-a range-b)
  (let ((start-a (car range-a)) (last-a (cdr range-a))
	(start-b (car range-b)) (last-b (cdr range-b))
	(item=?  (type-descriptor-item=?  type))
	(item<?  (type-descriptor-item<?  type))
	(item<=? (type-descriptor-item<=? type))
	(item-minus (type-descriptor-item-minus type))
	(item-prev  (type-descriptor-item-prev type))
	(item-next  (type-descriptor-item-next type)))
    (cond
     ((item=? start-a start-b) ; same start
      (cond ((item=? last-a last-b)
	     (values #f #f))
	    ((item<? last-a last-b)
	     (values #f (let ((last-a/next (item-next last-a)))
			  (and (item<=? last-a/next last-b)
			       (cons last-a/next last-b)))))
	    ((item<? last-b last-a)
	     (values #f (let ((last-b/next (item-next last-b)))
			  (and (item<=? last-b/next last-a)
			       (cons last-b/next last-a)))))))
     ;;(answer-to-equal-limit last-a last-b "start"))

     ((item=? last-a last-b) ; same last
      (cond ((item=? start-a start-b)
	     (values #f #f))
	    ((item<? start-a start-b)
	     (values #f (let ((start-b/prev (item-prev start-b)))
			  (and (item<=? start-a start-b/prev)
			       (cons start-a start-b/prev)))))
	    ((item<? start-b start-a)
	     (values #f (let ((start-a/prev (item-prev start-a)))
			  (and (item<=? start-b start-a/prev)
			       (cons start-b start-a/prev)))))))
      ;;(answer-to-equal-limit start-a start-b "last"))

     ((= 2 (item-minus start-b last-a)) ; contiguous
      (values #f (cons start-a last-b)))

     ((= 2 (item-minus start-a last-b)) ; contiguous
      (values #f (cons start-b last-a)))

     ((item<? last-a start-b) ; disjoint
      (values range-a range-b))

     ((item<? last-b start-a) ; disjoint
      (values range-b range-a))

     ;;Here we know that START-A != START-B and LAST-A != LAST-B.

     ((item<? start-a start-b) ; overlapping, a < b
      (values (let ((start-b/prev (item-prev start-b)))
		(and (item<=? start-a start-b/prev)
		     (cons start-a start-b/prev)))
	      (if (item<=? last-a last-b)
		  (let ((last-a/next (item-next last-a)))
		    (and (item<=? last-a/next last-b)
			 (cons last-a/next last-b)))
		(let ((last-b/next (item-next last-b)))
		  (and (item<=? last-b/next last-a)
		       (cons last-b/next last-a))))))

     (else	; overlapping, a > b
      (assert (item<? start-b start-a))
      (values (let ((start-a/prev (item-prev start-a)))
		(and (item<=? start-b start-a/prev)
		     (cons start-b start-a/prev)))
	      (if (item<? last-a last-b)
		  (let ((last-a/next (item-next last-a)))
		    (and (item<=? last-a/next last-b)
			 (cons last-a/next last-b)))
		(let ((last-b/next (item-next last-b)))
		  (and (item<=? last-b/next last-a)
		       (cons last-b/next last-a)))))))))

(define (%range-in-first-only type range-a range-b)
  (cond
   ((%range<? type range-b range-a) ; disjoint
    (values #f range-a))

   ((%range<? type range-a range-b) ; disjoint
    (values #f range-a))

   (else
    ;;Here we know they are overlapping.
    (let ((start-a (car range-a)) (last-a (cdr range-a))
	  (start-b (car range-b)) (last-b (cdr range-b))
	  (item<?    (type-descriptor-item<? type))
	  (item-prev (type-descriptor-item-prev type))
	  (item-next (type-descriptor-item-next type)))
      (values
       (and (item<? start-a start-b)
	    (let ((start-b/prev (item-prev start-b)))
	      (and (item<? start-a start-b/prev)
		   (cons start-a start-b/prev))))
       (and (item<? last-b last-a)
	    (let ((last-b/next (item-next last-b)))
	      (and (item<? last-b/next last-a)
		   (cons last-b/next last-a)))))))))

(define (%range-for-each type proc range)
  (let ((last  (cdr range))
	(item=?    (type-descriptor-item=? type))
	(item<?    (type-descriptor-item<? type))
	(item-next (type-descriptor-item-next type)))
    ;;We do not  want to compute the item after LAST,  because it may be
    ;;out of bounds for the type.
    ;;
    ;;Notice that if ITEM-NEXT jumps over LAST the loop stops correctly.
    (let loop ((i (car range)))
      (cond ((item<? i last)
	     (proc i)
	     (loop (item-next i)))
	    ((item=? i last)
	     (proc i))))))

(define (%range-every type proc range)
  (let ((last (cdr range))
	(item=?    (type-descriptor-item=? type))
	(item<?    (type-descriptor-item<? type))
	(item-next (type-descriptor-item-next type)))
    ;;We do not  want to compute the item after LAST,  because it may be
    ;;out of bounds for the type.
    ;;
    ;;Notice that if ITEM-NEXT jumps over LAST the loop stops correctly.
    (let loop ((i (car range)))
      (cond ((item<? i last)
	     (and (proc i)
		  (loop (item-next i))))
	    ((item=? i last)
	     (proc i))
	    (else #t)))))

(define (%range-any type proc range)
  (let ((last (cdr range))
	(item=? (type-descriptor-item=? type))
	(item<? (type-descriptor-item<? type))
	(item-next (type-descriptor-item-next type)))
    ;;We do not  want to compute the item after LAST,  because it may be
    ;;out of bounds for the type.
    ;;
    ;;Notice that if ITEM-NEXT jumps over LAST the loop stops correctly.
    (let loop ((i (car range)))
      (cond ((item<? i last)
	     (or (proc i)
		 (loop (item-next i))))
	    ((item=? i last)
	     (proc i))
	    (else #f)))))

(define (%range-fold type kons knil range)
  (let ((last (cdr range))
	(item=?    (type-descriptor-item=? type))
	(item<?    (type-descriptor-item<? type))
	(item-next (type-descriptor-item-next type)))
    ;;We do not  want to compute the item after LAST,  because it may be
    ;;out of bounds for the type.
    ;;
    ;;Notice that if ITEM-NEXT jumps over LAST the loop stops correctly.
    (let loop ((i (car range))
	       (knil knil))
      (cond ((item<? i last)
	     (loop (item-next i) (kons i knil)))
	    ((item=? i last)
	     (kons i knil))
	    (else knil)))))

(define (%range->list type range)
  (%range-fold type cons '() range))


;;;; domains

(define (%make-domain type . args)
  (let ((item?     (type-descriptor-item? type))
	(item<=?   (type-descriptor-item<=? type)))
    (let loop ((args args)
	       (domain '()))
      (if (null? args)
	  domain
	(let ((thing (car args)))
	  (cond
	   ((item? thing)
	    (loop (cdr args) (%domain-add-item  type domain thing)))
	   ((%range? type thing)
	    (loop (cdr args) (%domain-add-range type domain thing)))
	   (else
	    (assertion-violation '%make-domain
	      "invalid element for domain" thing))))))))

(define (%domain-copy type domain)
  (let ((item-copy (type-descriptor-item-copy type)))
    (let loop ((x domain))
      (if (pair? x)
	  (cons (loop (car x))
		(loop (cdr x)))
	(item-copy x)))))

(define (%domain-add-item type domain obj)
  (let ((item? (type-descriptor-item? type))
	(item-next (type-descriptor-item-next type)))
    (if (item? obj)
	(%domain-add-range type domain (%make-range type obj))
      (assertion-violation '%domain-add-item
	"expected character as new item" obj))))

(define (%domain-add-range type domain new-range)
  (let loop ((domain domain)
	     (result '()))
    (if (%domain-empty? domain)
	(reverse (cons new-range result))
      (let ((range (car domain)))
;;;	  (write (list 'range range 'new-range new-range))(newline)
	(cond
	 ((%range=? type range new-range)
;;;	    (write (list 'equal range new-range))(newline)
	  (append-reverse (cons range result) (cdr domain)))
	 ((%range-overlapping? type range new-range)
;;;	    (write (list 'overlapping range new-range))(newline)
	  (let loop2 ((domain       (cdr domain))
		      (new-range (let-values (((head tail)
					       (%range-union type range new-range)))
				   tail)))
	    (if (null? domain)
		(reverse (cons new-range result))
	      (let ((range (car domain)))
		(cond ((%range-contiguous? type range new-range)
		       (loop2 (cdr domain) (%range-concatenate type range new-range)))
		      ((%range-overlapping? type range new-range)
		       (loop2 (cdr domain) (let-values (((head tail)
							 (%range-union type range new-range)))
					  tail)))
		      (else
		       (append-reverse (cons new-range (cons range result))
				       domain)))))))
	 ((%range-contiguous? type range new-range)
;;;	    (write (list 'contig range new-range))(newline)
	  (let loop2 ((domain    (cdr domain))
		      (new-range (%range-concatenate type range new-range)))
	    (if (null? domain)
		(reverse (cons new-range result))
	      (let ((range (car domain)))
		(cond ((%range-contiguous? type range new-range)
		       (loop2 (cdr domain) (%range-concatenate type range new-range)))
		      ((%range-overlapping? type range new-range)
		       (loop2 (cdr domain) (let-values (((head tail)
							 (%range-union type range new-range)))
					  tail)))
		      (else
		       (append-reverse (cons new-range (cons range result))
				       domain)))))))
	 ((%range<? type new-range range)
;;;	    (write (list 'less range new-range))(newline)
	  (append-reverse (cons range (cons new-range result))
			  (cdr domain)))
	 (else
;;;	    (write (list 'other range new-range))(newline)
	  (loop (cdr domain) (cons range result))))))))

(define (%domain? type domain)
  (let ((item<? (type-descriptor-item<? type))
	(item<=? (type-descriptor-item<? type)))
    (if (null? domain)
	#t
      (let ((range (car domain)))
	(if (not (%range? type range))
	    #f
	  (let loop ((range1	range)
		     (domain	(cdr domain)))
	    (if (null? domain)
		#t
	      (let ((range2 (car domain)))
		(cond
		 ((not (%range? type range2))
		  #f)
		 ((not (%range<? type range1 range2))
		  #f)
		 ((%range-contiguous? type range1 range2)
		  #f)
		 (else
		  (loop range2 (cdr domain))))))))))))

(define (%domain-size type domain)
  (fold (lambda (range size)
	  (+ size (%range-length type range)))
	0 domain))

(define %domain-empty? null?)

(define (%domain-contains? type domain obj)
  (any (lambda (range) (%range-contains? type range obj))
    domain))

(define (%domain=? type domain-a domain-b)
  ;;If the length is different, they are different.
  (or (eq? domain-a domain-b)
      (let loop ((domain-a domain-a)
		 (domain-b domain-b))
	(cond ((null? domain-a)
	       (null? domain-b))
	      ((null? domain-b)
	       (null? domain-a))
	      (else
	       (and (%range=? type (car domain-a) (car domain-b))
		    (loop (cdr domain-a) (cdr domain-b))))))))

(define (%domain<? type domain-a domain-b)
  (cond ((null? domain-a) #f)
	((null? domain-b) #f)
	(else
	 (%range<? type (last domain-a) (car domain-b)))))

;;; --------------------------------------------------------------------

(define (cons-head-tail head tail result)
  ;; This is an internal helper for set operations.
  (let ((result (if head (cons head result) result)))
    (if tail (cons tail result) result)))

(define (%domain-intersection type domain-a domain-b)
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
	 ((%range=? type range-a range-b)
;;;	    (write (list 'equal range-a range-b))(newline)
	  (loop (cons range-a result)
		(cdr domain-a) (cdr domain-b)))
	 ((%range-overlapping? type range-a range-b)
;;;	    (write (list 'overlapping range-a range-b))(newline)
	  (let ((result (cons (%range-intersection type range-a range-b) result)))
	    (if (%range-last<? type range-a range-b)
		(loop result (cdr domain-a) domain-b)
	      (loop result domain-a (cdr domain-b)))))
	 ((%range<? type range-a range-b)
;;;	    (write (list 'less-than range-a range-b))(newline)
	  (loop result (cdr domain-a) domain-b))
	 ((%range<? type range-b range-a)
;;;	    (write (list 'greater-than range-a range-b))(newline)
	  (loop result domain-a (cdr domain-b)))
	 (else
	  (assertion-violation '%domain-intersection
	    "internal error processing ranges" (list range-a range-b))))))))

(define (%domain-union type domain-a domain-b)
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
	     ((or (%range-overlapping? type top range)
		  (%range-contiguous?  type top range))
	      (let-values (((head tail) (%range-union type top range)))
		(loop (cons-head-tail head tail (cdr result)) (cdr domain))))
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
	 ((and (not (null? result)) (%range-contiguous? type (car result) range-a))
	  (loop (cons (%range-concatenate type (car result) range-a) (cdr result))
		(cdr domain-a) domain-b))

	 ((and (not (null? result)) (%range-contiguous? type (car result) range-b))
	  (loop (cons (%range-concatenate type (car result) range-b) (cdr result))
		domain-a (cdr domain-b)))

	 ((and (not (null? result)) (%range-overlapping? type (car result) range-a))
	  (let-values (((head tail) (%range-union type (car result) range-a)))
	    (loop (cons tail (cdr result)) (cdr domain-a) domain-b)))

	 ((and (not (null? result)) (%range-overlapping? type (car result) range-b))
	  (let-values (((head tail) (%range-union type (car result) range-b)))
	    (loop (cons tail (cdr result)) domain-a (cdr domain-b))))

	 ((%range=? type range-a range-b)
	  (loop (cons range-a result) (cdr domain-a) (cdr domain-b)))

	 ((%range-contiguous? type range-a range-b)
	  (loop (cons (%range-concatenate type range-a range-b) result) (cdr domain-a) (cdr domain-b)))

	 ((%range-overlapping? type range-a range-b)
	  (let-values (((head tail) (%range-union type range-a range-b)))
	    (loop (cons tail result) (cdr domain-a) (cdr domain-b))))

	 ((%range<? type range-a range-b)
	  (loop (cons range-b (cons range-a result)) (cdr domain-a) (cdr domain-b)))

	 ((%range<? type range-b range-a)
	  (loop (cons range-a (cons range-b result)) (cdr domain-a) (cdr domain-b)))

	 (else
	  (assertion-violation '%domain-union
	    "internal error processing ranges" (list range-a range-b)))))))))

(define (%domain-difference type domain-a domain-b)
  (define (finish result domain)
    (if (null? result)
	domain
      (let loop ((result result)
		 (domain domain))
	(if (%domain-empty? domain)
	    (reverse result)
	  (let ((range (car domain))
		(top   (car result)))
	    (cond ((%range-overlapping? type top range)
		   (let-values (((head tail) (%range-difference type top range)))
		     (loop (cons-head-tail head tail (cdr result))
			   (cdr domain))))
		  ((%range-contiguous? type top range)
		   (let-values (((head tail) (%range-union type top range)))
		     (loop (cons-head-tail head tail (cdr result))
			   (cdr domain))))
		  (else
		   (loop (cons range result) (cdr domain)))))))))
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
	 ((and (not (null? result)) (%range-contiguous? type (car result) range-a))
;;;	  (write (list 'result-contiguous-a (car result) range-a))(newline)
	  (loop (cons (%range-concatenate type (car result) range-a) (cdr result))
		(cdr domain-a) domain-b))

	 ((and (not (null? result)) (%range-contiguous? type (car result) range-b))
;;;	  (write (list 'result-contiguous-b (car result) range-b))(newline)
	  (loop (cons (%range-concatenate type (car result) range-b) (cdr result))
		domain-a (cdr domain-b)))

	 ((and (not (null? result)) (%range-overlapping? type (car result) range-a))
;;;	  (write (list 'res-overlapping-a (car result) range-a))(newline)
	  (let-values (((head tail) (%range-difference type (car result) range-a)))
	    (loop (cons-head-tail head tail (cdr result)) (cdr domain-a) domain-b)))

	 ((and (not (null? result)) (%range-overlapping? type (car result) range-b))
;;;	  (write (list 'res-overlapping-b (car result) range-b))(newline)
	  (let-values (((head tail) (%range-difference type (car result) range-b)))
	    (loop (cons-head-tail head tail (cdr result)) domain-a (cdr domain-b))))

	 ((%range=? type range-a range-b)
;;;	  (write (list 'equal range-a range-b))(newline)
	  (loop result (cdr domain-a) (cdr domain-b)))

	 ((%range-contiguous? type range-a range-b)
;;;	  (write (list 'contiguous range-a range-b))(newline)
	  (loop (cons (%range-concatenate type range-a range-b) result) (cdr domain-a) (cdr domain-b)))

	 ((%range-overlapping? type range-a range-b)
;;;	  (write (list 'overlapping range-a range-b))(newline)
	  (let-values (((head tail) (%range-difference type range-a range-b)))
;;;	    (write (list 'overlapping-ht head tail))(newline)
	    (loop (cons-head-tail head tail result) (cdr domain-a) (cdr domain-b))))

	 ((%range<? type range-a range-b)
;;;	  (write (list 'lesser range-a range-b))(newline)
	  (loop (cons range-b (cons range-a result)) (cdr domain-a) (cdr domain-b)))

	 ((%range<? type range-b range-a)
;;;	  (write (list 'greater range-a range-b))(newline)
	  (loop (cons range-a (cons range-b result)) (cdr domain-a) (cdr domain-b)))

	 (else
	  (assertion-violation '%domain-difference
	    "internal error processing ranges" (list range-a range-b)))))))))

(define (%domain-complement type domain universe)
  (if (null? domain)
      universe
    (let loop ((result		'())
	       (universe	universe)
	       (domain		domain))
      (cond ((%domain-empty? universe)
	     (reverse result))
	    ((%domain-empty? domain)
	     (reverse (append-reverse universe result)))
	    (else
	     (let ((range-a (car universe))
		   (range-b (car domain)))
	       (cond ((%range<? type range-b range-a)
		      (loop result universe (cdr domain)))

		     ((%range<? type range-a range-b)
		      (loop (cons range-a result) (cdr universe) domain))

		     ((%range=? type range-a range-b)
		      (loop result (cdr universe) (cdr domain)))

		     ((%range-overlapping? type range-a range-b)
;;;		      (write (list 'overlapping range-a range-b))(newline)
		      (let-values (((head tail)
				    (%range-in-first-only type range-a range-b)))
;;;			(write (list 'overlapping-ht head tail))(newline)
			(if (%range-last<? type range-b range-a)
			    (loop (if head (cons head result) result)
				  (cons tail (cdr universe)) (cdr domain))
			  (let ((result (cons-head-tail head tail result)))
;;;			    (write (list 'overlapping-result result))(newline)
			    (cond ((%range-last<? type range-a range-b)
;;;				   (write (list 'overlapping-discard range-a))(newline)
				   (loop result (cdr universe) domain))
				  (else
;;;				   (write (list 'overlapping-discard range-a range-b))(newline)
				   (loop result (cdr universe) (cdr domain))))))))
		     (else
		      ;;Just discard RANGE-A.
		      (assertion-violation '%domain-complement
			"internal error processing ranges" (list range-a range-b)))
		     )))))))

(define (%domain-subset? type domain-a domain-b)
  (let look-for-range-b-in-domain-a ((domain-a domain-a)
				     (domain-b domain-b))
    (cond
     ((%domain-empty? domain-b) #t)
     ((%domain-empty? domain-a) #f)
     (else
      (let ((range-a (car domain-a))
	    (range-b (car domain-b)))
	(if (%range-subset? type range-a range-b)
	    (look-for-range-b-in-domain-a domain-a (cdr domain-b))
	  (look-for-range-b-in-domain-a (cdr domain-a) domain-b)))))))

(define (%domain-strict-subset? type domain-a domain-b)
  (let look-for-range-b-in-domain-a ((subset? #f)
				     (domain-a domain-a)
				     (domain-b domain-b))
    (cond
     ((%domain-empty? domain-b) subset?)
     ((%domain-empty? domain-a) #f)
     (else
      (let ((range-a (car domain-a))
	    (range-b (car domain-b)))
	(cond ((%range<? type range-a range-b)
	       (look-for-range-b-in-domain-a #t (cdr domain-a) domain-b))
	      ((%range-strict-subset? type range-a range-b)
	       (look-for-range-b-in-domain-a #t domain-a (cdr domain-b)))
	      ((%range=? type range-a range-b)
	       (look-for-range-b-in-domain-a subset? (cdr domain-a) (cdr domain-b)))
	      ((%range-subset? type range-a range-b)
	       (look-for-range-b-in-domain-a subset? domain-a (cdr domain-b)))
	      (else #f)))))))

;;; --------------------------------------------------------------------

(define (%domain-for-each type proc domain)
  (for-each (lambda (range)
	      (%range-for-each type proc range))
    domain))

(define (%domain-every type proc domain)
  (every (lambda (range)
	   (%range-every type proc range))
    domain))

(define (%domain-any type proc domain)
  (any (lambda (range)
	 (%range-any type proc range))
    domain))

(define (%domain-fold type kons knil domain)
  (let loop ((domain domain)
	     (knil knil))
    (if (null? domain)
	knil
      (loop (cdr domain) (%range-fold type kons knil (car domain))))))

(define (%domain->list type domain)
  (reverse (apply append (map (lambda (range) (%range->list type range))
			   domain))))


;;;; done

)

;;; end of file
