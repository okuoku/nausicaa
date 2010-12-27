;;;
;;;Part of: Nausicaa
;;;Contents: char-sets library
;;;Date: Fri Jun 12, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa char-sets)
  (export

    ;; bounds
    char-set-lower-bound char-set-upper-bound
    char-set-inner-upper-bound char-set-inner-lower-bound

    ;; constructors
    (rename (%true-char-set char-set)) char-set-copy
    char-set-add char-set-add!

    ;; inspection
    char-set-size (rename (domain-ref char-set-domain-ref))
    char-set-write

    ;; predicates
    (rename (%true-char-set? char-set?))
    char-set-empty? char-set-contains?
    char-set=? char-set<?
    char-set-superset? char-set-superset?/strict
    char-set-subset? char-set-subset?/strict

    ;; set operations
    char-set-intersection char-set-union
    char-set-difference char-set-complement

    ;; string operations
    string->char-set

    ;; list operations
    char-set-for-each char-set-every
    char-set-any char-set-fold
    char-set->list

    ;; predefined
    char-set:empty       char-set:full

    char-set:ascii
    char-set:ascii/dec-digit	(rename (char-set:ascii/dec-digit char-set:ascii/digit))
    char-set:ascii/oct-digit	char-set:ascii/hex-digit
    char-set:ascii/lower-case	char-set:ascii/upper-case
    char-set:ascii/letter	char-set:ascii/letter+digit
    char-set:ascii/punctuation	char-set:ascii/symbol
    char-set:ascii/control	char-set:ascii/whitespace
    char-set:ascii/graphic	char-set:ascii/printable
    char-set:ascii/blank

    char-set:ascii/vowels		char-set:ascii/consonants
    char-set:ascii/vowels/lower-case	char-set:ascii/consonants/lower-case
    char-set:ascii/vowels/upper-case	char-set:ascii/consonants/upper-case
    )
  (import (rnrs)
    (nausicaa lists))


(define-record-type char-set
  (nongenerative nausicaa:char-sets:char-set)
  (fields (mutable domain domain-ref domain-set!)))

(define (%true-char-set . args)
  (make-char-set (apply make-domain args)))

(define (%true-char-set? cs)
  (and (char-set? cs)
       (domain? (domain-ref cs))))

(define (char-set-copy cs)
  (make-char-set (domain-copy (domain-ref cs))))

(define (char-set-add cs obj)
  (cond ((char? obj)
	 (make-char-set (domain-add-item (domain-ref cs) obj)))
	((range? obj)
	 (make-char-set (domain-add-range (domain-ref cs) obj)))
	(else
	 (assertion-violation 'char-set-add
	   "attempt to add an invalid object to a char-set" obj))))

(define (char-set-add! cs obj)
  (domain-set! cs (char-set-add cs obj)))

(define (char-set-size cs)
  (domain-size (domain-ref cs)))

(define (char-set-empty? cs)
  (domain-empty? (domain-ref cs)))

(define (char-set-contains? cs item)
  (domain-contains? (domain-ref cs) item))

(define (char-set=? cs-a cs-b)
  (domain=? (domain-ref cs-a) (domain-ref cs-b)))

(define (char-set<? cs-a cs-b)
  (domain<? (domain-ref cs-a) (domain-ref cs-b)))

(define (char-set-superset? cs . cs-args)
  (let loop ((domain      (domain-ref cs))
	     (domain-args (map domain-ref cs-args)))
    (or (null? domain-args)
	(let ((next-domain (car domain-args)))
	  (and (domain-superset? domain next-domain)
	       (loop next-domain (cdr domain-args)))))))

(define (char-set-subset? cs . cs-args)
  (apply char-set-superset? (reverse (cons cs cs-args))))

(define (char-set-superset?/strict cs . cs-args)
  (let loop ((domain      (domain-ref cs))
	     (domain-args (map domain-ref cs-args)))
    (or (null? domain-args)
	(let ((next-domain (car domain-args)))
	  (and (domain-superset?/strict domain next-domain)
	       (loop next-domain (cdr domain-args)))))))

(define (char-set-subset?/strict cs . cs-args)
  (apply char-set-superset?/strict (reverse (cons cs cs-args))))

(define (char-set-intersection cs . cs-args)
  (make-char-set (fold-left (lambda (domain-prev domain)
			      (domain-intersection domain domain-prev))
			    (domain-ref cs)
			    (map domain-ref cs-args))))

(define (char-set-union cs . cs-args)
  (make-char-set (fold-left (lambda (domain-prev domain)
			      (domain-union domain domain-prev))
			    (domain-ref cs)
			    (map domain-ref cs-args))))

(define (char-set-difference cs . cs-args)
  (make-char-set (fold-left (lambda (domain-prev domain)
			      (domain-difference domain domain-prev))
			    (domain-ref cs)
			    (map domain-ref cs-args))))

(define char-set-complement
  (case-lambda
   ((cs)
    (char-set-complement cs char-set:full))
   ((cs universe)
    (make-char-set (domain-complement (domain-ref cs) (domain-ref universe))))))

(define (char-set-for-each proc cs)
  (domain-for-each proc (domain-ref cs)))

(define (char-set-every proc cs)
  (domain-every proc (domain-ref cs)))

(define (char-set-any proc cs)
  (domain-any proc (domain-ref cs)))

(define (char-set-fold kons knil cs)
  (domain-fold kons knil (domain-ref cs)))

(define (char-set->list cs)
  (domain->list (domain-ref cs)))

(define (string->char-set str)
  (make-char-set (string->domain str)))

(define char-set-write
  (case-lambda
   ((cs)
    (char-set-write cs (current-output-port)))
   ((cs port)
    (display "(char-set " port)
    (for-each (lambda (range)
		(display (string-append
			  "'("
			  "#\\x" (number->string (char->integer (car range)) 16)
			  " . "
			  "#\\x" (number->string (char->integer (cdr range)) 16)
			  ") ") port))
      (domain-ref cs))
    (display #\) port))))


(define inclusive-lower-bound		0)
(define exclusive-inner-upper-bound	#xD800)
(define exclusive-inner-lower-bound	#xDFFF)
(define inclusive-upper-bound		#x10FFFF)

(define char-set-lower-bound		(integer->char inclusive-lower-bound))
(define char-set-inner-upper-bound	(integer->char (- exclusive-inner-upper-bound 1)))
(define char-set-inner-lower-bound	(integer->char (+ 1 exclusive-inner-lower-bound)))
(define char-set-upper-bound		(integer->char inclusive-upper-bound))

(define (number-in-range? x)
  (or (and (<= inclusive-lower-bound x)
	   (<  x exclusive-inner-upper-bound))
      (and (<  exclusive-inner-lower-bound x)
	   (<= x inclusive-upper-bound))))

(define (%char-minus a b)
  (+ 1 (- (char->integer a) (char->integer b))))

(define (%char-min a b)
  (if (char<? a b) a b))

(define (%char-max a b)
  (if (char<? a b) b a))

(define (%char-next ch range)
  (let* ((x  (+ 1 (char->integer ch))))
    (and (number-in-range? x)
	 (let ((ch (integer->char x)))
	   (if range
	       (and (<= x (char->integer (cdr range)))
		    ch)
	     ch)))))

(define (%char-prev ch range)
  (let* ((x  (- (char->integer ch) 1)))
    (and (number-in-range? x)
	 (let ((ch (integer->char x)))
	   (if range
	       (and (<= (char->integer (car range)) x)
		    ch)
	     ch)))))


;;;; domain wrappers for ranges

(define (make-range start last)
  (if (and (char? start)
	   (char? last)
	   (char<=? start last))
      (cons start last)
    (assertion-violation 'make-range
      "invalid range limits" (list start last))))

(define (range? obj)
  (and (pair? obj)
       (and (char? (car obj))
	    (char? (cdr obj))
	    (char<=? (car obj) (cdr obj)))))

(define (range-contains? range obj)
  (and (char<=? (car range) obj)
       (char<=? obj         (cdr range))))

(define (range-length range)
  (+ 1 (- (char->integer (cdr range)) (char->integer (car range)))))

(define (range=? range-a range-b)
  (or (eq? range-a range-b)
	(and (char=? (car range-a) (car range-b))
	     (char=? (cdr range-a) (cdr range-b)))))

(define (range<? range-a range-b)
  (char<? (cdr range-a) (car range-b)))

(define (range<=? range-a range-b)
  (char<=? (cdr range-a) (cdr range-b)))

(define (range-contiguous? range-a range-b)
  (or (= 2 (%char-minus (car range-b) (cdr range-a)))
      (= 2 (%char-minus (car range-a) (cdr range-b)))))

(define (range-superset? range-a range-b)
  (char<=? (car range-a) (car range-b) (cdr range-b) (cdr range-a)))

(define (range-superset?/strict range-a range-b)
  (or (and (char<=? (car range-a) (car range-b))
	   (char<?  (cdr range-b) (cdr range-a)))
      (and (char<?  (car range-a) (car range-b))
	   (char<=? (cdr range-b) (cdr range-a)))))

(define (range-start<? range-a range-b)
  (char<? (car range-a) (car range-b)))

(define (range-start<=? range-a range-b)
  (char<=? (car range-a) (car range-b)))

(define (range-last<? range-a range-b)
  (char<? (cdr range-a) (cdr range-b)))

(define (range-last<=? range-a range-b)
  (char<=? (cdr range-a) (cdr range-b)))

(define (range-overlapping? range-a range-b)
  (let ((start-a (car range-a)) (last-a (cdr range-a))
	(start-b (car range-b)) (last-b (cdr range-b)))
    (or (and (char<=? start-a start-b last-a))
	(and (char<=? start-b start-a last-b)))))

(define (range-concatenate range-a range-b)
  (cons (%char-min (car range-a) (car range-b))
	(%char-max (cdr range-a) (cdr range-b))))

(define (range-intersection range-a range-b)
  (let ((start-a (car range-a)) (last-a (cdr range-a))
	(start-b (car range-b)) (last-b (cdr range-b)))
    (and (or (char<=? start-a start-b last-a)
	     (char<=? start-b start-a last-b))
	 (cons (%char-max start-a start-b)
	       (%char-min last-a  last-b)))))

(define (range-union range-a range-b)
  ;;For this function  is it undocumented mandatory that:  If one of the
  ;;returned values if  #f, it must be the first  one.  This property is
  ;;used in the domain functions below.
  (let ((start-a (car range-a)) (last-a (cdr range-a))
	(start-b (car range-b)) (last-b (cdr range-b)))
    (cond
     ((= 2 (%char-minus start-b last-a))	(values #f (cons start-a last-b)))
		; contiguous, RANGE-A < RANGE-B
     ((= 2 (%char-minus start-a last-b))	(values #f (cons start-b last-a)))
		; contiguous, RANGE-B < RANGE-A
     ((char<? last-a start-b)			(values range-a range-b))
		; disjoint, RANGE-A < RANGE-B
     ((char<? last-b start-a)	(values range-b range-a))
		; disjoint, RANGE-B < RANGE-A
     ;;Here we know they are overlapping, that is we know that:
     ;;
     ;;  (and (char<=? start-b last-a)
     ;;       (char<=? start-a last-b)) => #t
     ;;
     (else
      (values #f (cons (%char-min start-a start-b)
		       (%char-max last-a  last-b)))))))

(define (range-difference range-a range-b)
  (let ((start-a (car range-a)) (last-a (cdr range-a))
	(start-b (car range-b)) (last-b (cdr range-b)))
    (cond
     ((= 2 (%char-minus start-b last-a))	(values #f (cons start-a last-b)))
		; contiguous, RANGE-A < RANGE-B
     ((= 2 (%char-minus start-a last-b))	(values #f (cons start-b last-a)))
		; contiguous, RANGE-B < RANGE-A
     ((char<? last-a start-b)		(values range-a range-b))
		; disjoint, RANGE-A < RANGE-B
     ((char<? last-b start-a)		(values range-b range-a))
		; disjoint, RANGE-B < RANGE-A
     ;;Here we know they are overlapping, that is we know that:
     ;;
     ;;  (and (char<=? start-b last-a)
     ;;       (char<=? start-a last-b)) => #t
     ;;
     ((char=? start-a start-b) ; same start
      (cond ((char=? last-a last-b)
	     (values #f #f))
	    ((char<? last-a last-b)
	     (values #f (let ((last-a/next (%char-next last-a range-b)))
			  (and (char<=? last-a/next last-b)
			       (cons last-a/next last-b)))))
	    ((char<? last-b last-a)
	     (values #f (let ((last-b/next (%char-next last-b range-a)))
			  (and (char<=? last-b/next last-a)
			       (cons last-b/next last-a)))))))

     ((char=? last-a last-b) ; same last
      (cond ((char=? start-a start-b)
	     (values #f #f))
	    ((char<? start-a start-b)
	     (values #f (let ((start-b/prev (%char-prev start-b range-a)))
			  (and (char<=? start-a start-b/prev)
			       (cons start-a start-b/prev)))))
	    ((char<? start-b start-a)
	     (values #f (let ((start-a/prev (%char-prev start-a range-b)))
			  (and (char<=? start-b start-a/prev)
			       (cons start-b start-a/prev)))))))
     ;;Here we know that START-A != START-B and LAST-A != LAST-B.
     ((char<? start-a start-b) ; overlapping, a < b
      (values (let ((start-b/prev (%char-prev start-b range-a)))
		(and (char<=? start-a start-b/prev)
		     (cons start-a start-b/prev)))
	      (if (char<=? last-a last-b)
		  (let ((last-a/next (%char-next last-a range-b)))
		    (and (char<=? last-a/next last-b)
			 (cons last-a/next last-b)))
		(let ((last-b/next (%char-next last-b range-a)))
		  (and (char<=? last-b/next last-a)
		       (cons last-b/next last-a))))))

     (else	; overlapping, a > b
      (assert (char<? start-b start-a))
      (values (let ((start-a/prev (%char-prev start-a range-b)))
		(and (char<=? start-b start-a/prev)
		     (cons start-b start-a/prev)))
	      (if (char<? last-a last-b)
		  (let ((last-a/next (%char-next last-a range-b)))
		    (and (char<=? last-a/next last-b)
			 (cons last-a/next last-b)))
		(let ((last-b/next (%char-next last-b range-a)))
		  (and (char<=? last-b/next last-a)
		       (cons last-b/next last-a)))))))))

(define (range-for-each proc range)
  (let loop ((i (car range)))
    (and i (proc i)
	 (loop (%char-next i range)))))

(define (range-every proc range)
  (let loop ((i (car range)))
    (if i
	(and (proc i)
	     (loop (%char-next i range)))
      #t)))

(define (range-any proc range)
  (let loop ((i (car range)))
    (and i
	 (or (proc i)
	     (loop (%char-next i range))))))

(define (range-fold kons knil range)
  (let loop ((i    (car range))
	     (knil knil))
    (if i
	(loop (%char-next i range) (kons i knil))
      knil)))

(define (range->list range)
  (range-fold cons '() range))


;;;; domain wrappers for characters

(define (make-domain . args)
  (let loop ((args args)
	     (domain '()))
    (if (null? args)
	domain
      (let ((thing (car args)))
	(cond
	 ((char? thing)
	  (loop (cdr args) (domain-add-item  domain thing)))
	 ((range? thing)
	  (loop (cdr args) (domain-add-range domain thing)))
	 (else
	  (assertion-violation 'make-domain
	    "invalid element for domain" thing)))))))

(define (domain-copy domain)
  (let loop ((x domain))
    (if (pair? x)
	(cons (loop (car x))
	      (loop (cdr x)))
      x)))

(define (domain-add-item domain obj)
  (if (char? obj)
      (domain-add-range domain (make-range obj obj))
    (assertion-violation 'domain-add-item
      "expected character as new item" obj)))

(define (domain-add-range domain new-range)
  (let loop ((domain domain)
	     (result '()))
    (if (domain-empty? domain)
	(reverse (cons new-range result))
      (let ((range (car domain)))
	(cond
	 ((range=? range new-range)
	  (append-reverse (cons range result) (cdr domain)))
	 ((range-overlapping? range new-range)
	  (let loop2 ((domain       (cdr domain))
		      (new-range (let-values (((head tail)
					       (range-union range new-range)))
				   tail)))
	    (if (null? domain)
		(reverse (cons new-range result))
	      (let ((range (car domain)))
		(cond ((range-contiguous? range new-range)
		       (loop2 (cdr domain) (range-concatenate range new-range)))
		      ((range-overlapping? range new-range)
		       (loop2 (cdr domain) (let-values (((head tail)
							 (range-union range new-range)))
					     tail)))
		      (else
		       (append-reverse (cons new-range (cons range result))
				       domain)))))))
	 ((range-contiguous? range new-range)
	  (let loop2 ((domain    (cdr domain))
		      (new-range (range-concatenate range new-range)))
	    (if (null? domain)
		(reverse (cons new-range result))
	      (let ((range (car domain)))
		(cond ((range-contiguous? range new-range)
		       (loop2 (cdr domain) (range-concatenate range new-range)))
		      ((range-overlapping? range new-range)
		       (loop2 (cdr domain) (let-values (((head tail)
							 (range-union range new-range)))
					     tail)))
		      (else
		       (append-reverse (cons new-range (cons range result))
				       domain)))))))
	 ((range<? new-range range)
	  (append-reverse (cons range (cons new-range result))
			  (cdr domain)))
	 (else
	  (loop (cdr domain) (cons range result))))))))

(define (domain? domain)
  (if (null? domain)
      #t
    (let ((range (car domain)))
      (if (not (range? range))
	  #f
	(let loop ((range1	range)
		   (domain	(cdr domain)))
	  (if (null? domain)
	      #t
	    (let ((range2 (car domain)))
	      (cond
	       ((not (range? range2))
		#f)
	       ((not (range<? range1 range2))
		#f)
	       ((range-contiguous? range1 range2)
		#f)
	       (else
		(loop range2 (cdr domain)))))))))))

(define (domain-size domain)
  (fold (lambda (range size)
	  (+ size (range-length range)))
	0 domain))

(define domain-empty? null?)

(define (domain-contains? domain obj)
  (any (lambda (range) (range-contains? range obj))
    domain))

(define (domain=? domain-a domain-b)
  ;;If the length is different, they are different.
  (or (eq? domain-a domain-b)
      (let loop ((domain-a domain-a)
		 (domain-b domain-b))
	(cond ((null? domain-a)
	       (null? domain-b))
	      ((null? domain-b)
	       (null? domain-a))
	      (else
	       (and (range=? (car domain-a) (car domain-b))
		    (loop (cdr domain-a) (cdr domain-b))))))))

(define (domain<? domain-a domain-b)
  (and (not (null? domain-a))
       (not (null? domain-b))
       (range<? (last domain-a) (car domain-b))))

;;; --------------------------------------------------------------------

(define (cons-head-tail head tail result)
  ;; This is an internal helper for set operations.
  (let ((result (if head (cons head result) result)))
    (if tail (cons tail result) result)))

(define (domain-superset? domain-a domain-b)
  (let look-for-range-b-in-domain-a ((domain-a domain-a)
				     (domain-b domain-b))
    (or (domain-empty? domain-b)
	(and (not (domain-empty? domain-a))
	     (let ((range-a (car domain-a))
		   (range-b (car domain-b)))
	       (if (range-superset? range-a range-b)
		   (look-for-range-b-in-domain-a domain-a (cdr domain-b))
		 (look-for-range-b-in-domain-a (cdr domain-a) domain-b)))))))

(define (domain-superset?/strict domain-a domain-b)
  (let look-for-range-b-in-domain-a ((superset? #f)
				     (domain-a domain-a)
				     (domain-b domain-b))
    (if (domain-empty? domain-b)
	superset?
      (and (not (domain-empty? domain-a))
	   (let ((range-a (car domain-a))
		 (range-b (car domain-b)))
	     (cond ((range<? range-a range-b)
		    (look-for-range-b-in-domain-a #t (cdr domain-a) domain-b))
		   ((range-superset?/strict range-a range-b)
		    (look-for-range-b-in-domain-a #t domain-a (cdr domain-b)))
		   ((range=? range-a range-b)
		    (look-for-range-b-in-domain-a superset? (cdr domain-a) (cdr domain-b)))
		   ((range-superset? range-a range-b)
		    (look-for-range-b-in-domain-a superset? domain-a (cdr domain-b)))
		   (else #f)))))))

(define (domain-intersection domain-a domain-b)
  (let loop ((result	'())
	     (domain-a	domain-a)
	     (domain-b	domain-b))
    (if (or (domain-empty? domain-a)
	    (domain-empty? domain-b))
	(reverse result)
      (let ((range-a	(car domain-a))
	    (range-b	(car domain-b)))
	(cond
	 ((range=? range-a range-b)
	  (loop (cons range-a result)
		(cdr domain-a) (cdr domain-b)))
	 ((range-overlapping? range-a range-b)
	  (let ((result (cons (range-intersection range-a range-b) result)))
	    (if (range-last<? range-a range-b)
		(loop result (cdr domain-a) domain-b)
	      (loop result domain-a (cdr domain-b)))))
	 ((range<? range-a range-b)
	  (loop result (cdr domain-a) domain-b))
	 ((range<? range-b range-a)
	  (loop result domain-a (cdr domain-b)))
	 (else
	  (assertion-violation 'domain-intersection
	    "internal error processing ranges" (list range-a range-b))))))))

(define (domain-union domain-a domain-b)
  (define (finish result domain)
    (if (null? result)
	domain
      (let loop ((result result)
		 (domain domain))
	(if (domain-empty? domain)
	    (reverse result)
	  (let ((range (car domain))
		(top   (car result)))
	    (cond
	     ((or (range-overlapping? top range)
		  (range-contiguous?  top range))
	      (let-values (((head tail) (range-union top range)))
		(loop (cons-head-tail head tail (cdr result)) (cdr domain))))
	     (else
	      (loop (cons range result) (cdr domain)))))))))
  (let loop ((result '())
	     (domain-a domain-a)
	     (domain-b domain-b))
    (cond
     ((domain-empty? domain-a)
      (finish result domain-b))
     ((domain-empty? domain-b)
      (finish result domain-a))
     (else
      (let ((range-a (car domain-a))
	    (range-b (car domain-b)))
	(cond
	 ((and (not (null? result)) (range-contiguous? (car result) range-a))
	  (loop (cons (range-concatenate (car result) range-a) (cdr result))
		(cdr domain-a) domain-b))

	 ((and (not (null? result)) (range-contiguous? (car result) range-b))
	  (loop (cons (range-concatenate (car result) range-b) (cdr result))
		domain-a (cdr domain-b)))

	 ((and (not (null? result)) (range=? (car result) range-a))
	  (loop result (cdr domain-a) domain-b))

	 ((and (not (null? result)) (range=? (car result) range-b))
	  (loop result domain-a (cdr domain-b)))

	 ((and (not (null? result)) (range-overlapping? (car result) range-a))
	  (let-values (((head tail) (range-union (car result) range-a)))
	    (loop (cons tail (cdr result)) (cdr domain-a) domain-b)))

	 ((and (not (null? result)) (range-overlapping? (car result) range-b))
	  (let-values (((head tail) (range-union (car result) range-b)))
	    (loop (cons tail (cdr result)) domain-a (cdr domain-b))))

	 ((range=? range-a range-b)
	  (loop (cons range-a result) (cdr domain-a) (cdr domain-b)))

	 ((range-contiguous? range-a range-b)
	  (loop (cons (range-concatenate range-a range-b) result) (cdr domain-a) (cdr domain-b)))

	 ((range-overlapping? range-a range-b)
	  (let-values (((head tail) (range-union range-a range-b)))
	    (loop (cons tail result) (cdr domain-a) (cdr domain-b))))

	 ((range<? range-a range-b)
	  (loop (cons range-a result) (cdr domain-a) domain-b))

	 ((range<? range-b range-a)
          (loop (cons range-b result) domain-a (cdr domain-b)))

	 (else
	  (assertion-violation 'domain-union
	    "internal error processing ranges" (list range-a range-b)))))))))

(define (domain-difference domain-a domain-b)
  (define (finish result domain)
    (if (null? result)
	domain
      (let loop ((result result)
		 (domain domain))
	(if (domain-empty? domain)
	    (reverse result)
	  (let ((range (car domain))
		(top   (car result)))
	    (cond ((range-overlapping? top range)
		   (let-values (((head tail) (range-difference top range)))
		     (loop (cons-head-tail head tail (cdr result))
			   (cdr domain))))
		  ((range-contiguous? top range)
		   (let-values (((head tail) (range-union top range)))
		     (loop (cons-head-tail head tail (cdr result))
			   (cdr domain))))
		  (else
		   (loop (cons range result) (cdr domain)))))))))
  (let loop ((result '())
	     (domain-a domain-a)
	     (domain-b domain-b))
    (cond
     ((and (domain-empty? domain-a) (domain-empty? domain-b))
      (reverse result))
     ((domain-empty? domain-a)
      (finish result domain-b))
     ((domain-empty? domain-b)
      (finish result domain-a))
     (else
      (let ((range-a (car domain-a))
	    (range-b (car domain-b)))
	(cond
	 ((and (not (null? result)) (range-contiguous? (car result) range-a))
	  (loop (cons (range-concatenate (car result) range-a) (cdr result))
		(cdr domain-a) domain-b))

	 ((and (not (null? result)) (range-contiguous? (car result) range-b))
	  (loop (cons (range-concatenate (car result) range-b) (cdr result))
		domain-a (cdr domain-b)))

	 ((and (not (null? result)) (range-overlapping? (car result) range-a))
	  (let-values (((head tail) (range-difference (car result) range-a)))
	    (loop (cons-head-tail head tail (cdr result)) (cdr domain-a) domain-b)))

	 ((and (not (null? result)) (range-overlapping? (car result) range-b))
	  (let-values (((head tail) (range-difference (car result) range-b)))
	    (loop (cons-head-tail head tail (cdr result)) domain-a (cdr domain-b))))

	 ((range=? range-a range-b)
	  (loop result (cdr domain-a) (cdr domain-b)))

	 ((range-contiguous? range-a range-b)
	  (loop (cons (range-concatenate range-a range-b) result) (cdr domain-a) (cdr domain-b)))

	 ((range-overlapping? range-a range-b)
	  (let-values (((head tail) (range-difference range-a range-b)))
	    (loop (cons-head-tail head tail result) (cdr domain-a) (cdr domain-b))))

	 ((range<? range-a range-b)
	  (loop (cons range-a result) (cdr domain-a) domain-b))

	 ((range<? range-b range-a)
	  (loop (cons range-b result) domain-a (cdr domain-b)))

	 (else
	  (assertion-violation 'domain-difference
	    "internal error processing ranges" (list range-a range-b)))))))))

(define (domain-complement domain universe)
  (if (null? domain)
      universe
    (let loop ((result		'())
	       (universe	universe)
	       (domain		domain))
      (cond ((domain-empty? universe)
	     (reverse result))
	    ((domain-empty? domain)
	     (reverse (append-reverse universe result)))
	    (else
	     (let ((range-a (car universe))
		   (range-b (car domain)))
	       (cond ((range<? range-b range-a)
		      (loop result universe (cdr domain)))

		     ((range<? range-a range-b)
		      (loop (cons range-a result) (cdr universe) domain))

		     ((range=? range-a range-b)
		      (loop result (cdr universe) (cdr domain)))

		     ((range-overlapping? range-a range-b)
		      (let-values (((head tail)
				    (range-in-first-only range-a range-b)))
			(if (range-last<? range-b range-a)
			    (loop (if head (cons head result) result)
				  (cons tail (cdr universe)) (cdr domain))
			  (let ((result (cons-head-tail head tail result)))
			    (cond ((range-last<? range-a range-b)
				   (loop result (cdr universe) domain))
				  (else
				   (loop result (cdr universe) (cdr domain))))))))
		     (else
		      ;;just discard RANGE-A
		      (assertion-violation 'domain-complement
			"internal error processing ranges" (list range-a range-b)))
		     )))))))

(define (range-in-first-only range-a range-b)
  (let ((start-a (car range-a)) (last-a (cdr range-a))
	(start-b (car range-b)) (last-b (cdr range-b)))
    (if (or (char<? last-b start-a)
	    (char<? last-a start-b)) ; disjoint (including contiguous)
	(values #f range-a)
      ;;Here we know they are overlapping.
      (values
       (and (char<? start-a start-b)
	    (let ((start-b/prev (%char-prev start-b range-a)))
	      (and (char<? start-a start-b/prev)
		   (cons start-a start-b/prev))))
       (and (char<? last-b last-a)
	    (let ((last-b/next (%char-next last-b range-a)))
	      (and (char<? last-b/next last-a)
		   (cons last-b/next last-a))))))))

(define (domain-for-each proc domain)
  (for-each (lambda (range)
	      (range-for-each proc range))
    domain))

(define (domain-every proc domain)
  (every (lambda (range)
	   (range-every proc range))
    domain))

(define (domain-any proc domain)
  (any (lambda (range)
	 (range-any proc range))
    domain))

(define (domain-fold kons knil domain)
  (let loop ((domain domain)
	     (knil knil))
    (if (null? domain)
	knil
      (loop (cdr domain) (range-fold kons knil (car domain))))))

(define (domain->list domain)
  (reverse (apply append (map range->list domain))))

(define (string->domain str)
  (apply make-domain (string->list str)))


;;;; basic predefined char sets

(define char-set:empty (make-char-set '()))

(define char-set:full
  (%true-char-set `(,char-set-lower-bound . ,char-set-inner-upper-bound)
		  `(,char-set-inner-lower-bound . ,char-set-upper-bound)))


;;;; ASCII predefined char sets

(define char-set:ascii
  ;;Notice  that ASCII  has numeric  codes in  the range  [0,  127]; the
  ;;numeric code 127 is included, and the number of codes is 128.
  (%true-char-set '(#\x0 . #\x127)))

(define char-set:ascii/dec-digit
  (%true-char-set '(#\0 . #\9)))

(define char-set:ascii/oct-digit
  (%true-char-set '(#\0 . #\7)))

(define char-set:ascii/hex-digit
  (%true-char-set '(#\0 . #\9) '(#\a . #\f) '(#\A . #\F)))

(define char-set:ascii/lower-case
  (%true-char-set '(#\a . #\z)))

(define char-set:ascii/upper-case
  (%true-char-set '(#\A . #\Z)))

(define char-set:ascii/letter
  (char-set-union char-set:ascii/lower-case char-set:ascii/upper-case))

(define char-set:ascii/letter+digit
  (char-set-union char-set:ascii/letter char-set:ascii/dec-digit))

(define char-set:ascii/punctuation
  ;;Yes I have verified that all of these have numeric code in the range
  ;;[0, 127] (Marco Maggi, Tue Jun 23, 2009).
  (%true-char-set #\! #\" #\# #\% #\& #\' #\( #\) #\* #\, #\- #\.
		  #\/ #\: #\; #\? #\@ #\[ #\\ #\] #\_ #\{ #\}))

(define char-set:ascii/symbol
  ;;Yes I have verified that all of these have numeric code in the range
  ;;[0, 127] (Marco Maggi, Tue Jun 23, 2009).
  (%true-char-set #\$ #\+ #\< #\= #\> #\^ #\` #\| #\~))

(define char-set:ascii/control
  ;;Notice that control characters are the ones whose numeric code is in
  ;;the range [0, 31] plus 127; the number of control characters is 33.
  (%true-char-set '(#\x0 . #\x31) (integer->char 127)))

(define char-set:ascii/whitespace
  (%true-char-set #\x0009		  ; HORIZONTAL TABULATION
		  #\x000A		  ; LINE FEED
		  #\x000B		  ; VERTICAL TABULATION
		  #\x000C		  ; FORM FEED
		  #\x000D		  ; CARRIAGE RETURN
		  #\x0020))		  ; SPACE

(define char-set:ascii/blank
  (%true-char-set #\tab #\space))

(define char-set:ascii/graphic
  (char-set-union char-set:ascii/letter+digit
		  char-set:ascii/punctuation
		  char-set:ascii/symbol))

(define char-set:ascii/printable
  (char-set-union char-set:ascii/whitespace
		  char-set:ascii/graphic)) ; NO-BREAK SPACE

(define char-set:ascii/vowels
  (%true-char-set #\a #\e #\i #\o #\u
		  #\A #\E #\I #\O #\U))

(define char-set:ascii/vowels/lower-case
  (%true-char-set #\a #\e #\i #\o #\u))

(define char-set:ascii/vowels/upper-case
  (%true-char-set #\A #\E #\I #\O #\U))

(define char-set:ascii/consonants
  (char-set-complement char-set:ascii/vowels char-set:ascii/letter))

(define char-set:ascii/consonants/lower-case
  (char-set-complement char-set:ascii/vowels/lower-case char-set:ascii/lower-case))

(define char-set:ascii/consonants/upper-case
  (char-set-complement char-set:ascii/vowels/upper-case char-set:ascii/upper-case))


;;;; done

)

;;; end of file
