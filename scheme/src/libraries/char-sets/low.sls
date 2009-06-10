;;;
;;;Part of: Nausicaa
;;;Contents: low level character sets library
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



(library (char-sets low)
  (export

    ;; range constructors
    %make-range

    ;; range inspection
    %range-length %range-empty? %range-contains?

    ;; range comparison
    %range=? %range<?
    %range-start<? %range-start<=?
    %range-contiguous? %range-overlapping?

    ;; range set operations
    %range-concatenate
    %range-union %range-intersection %range-difference

    ;; range list operations
    %range-for-each %range-fold
    %range-every %range-any
    )
  (import (rnrs)
    (lists))


;;; ranges

(define (%valid-range-bound num)
  ;;Return true if NUM is a valid range bound.
  (and (integer? num) (exact? num) (<= 0 num)))

(define (%valid-range? range)
  ;;Return true if RANGE is a valid range value.
  (and (pair? range)
       (%valid-range-bound (car range))
       (%valid-range-bound (car range))
       (not (%range-empty? range))))

(define (%make-range start past)
  ;;Compose the start index and the  past index into a range cons.  This
  ;;function  validates the  arguments to  make sure  that the  range is
  ;;valid.
  (if (and (%valid-range-bound start)
	   (%valid-range-bound past)
	   (< start past))
      (cons start past)
    (assertion-violation '%make-range
      "expected start value less than past value"
      (list start past))))

(define (%range-length range)
  ;;Assuming  that  RANGE  is  a  valid  range:  Return  the  number  of
  ;;characters in the range.
  (- (cdr range) (car range)))

(define (%range-empty? range)
  ;;Assuming that RANGE  is a pair of exact,  positive, integers: Return
  ;;true  if the  range is  empty.   Notice that  an empty  range is  an
  ;;INVALID range.
  (= (cdr range) (car range)))

(define (%range-contains? range char-num)
  ;;Assuming that  RANGE is  a valid range:  Return true if  the integer
  ;;CHAR-NUM is inside the range.
  (and (<= (car range) char-num)
       (< char-num (cdr range))))

(define (%range=? range-a range-b)
  ;;Assuming that RANGE-A  and RANGE-B are valid ranges:  Return true if
  ;;RANGE-A and RANGE-B represent the same range.
  (or (eq? range-a range-b)
      (and (= (car range-a) (car range-b))
	   (= (cdr range-a) (cdr range-b)))))

(define (%range<? range-a range-b)
  ;;Assuming that RANGE-A  and RANGE-B are valid ranges:  Return true if
  ;;RANGE-A has all members less than all members of RANGE-B.
  (< (cdr range-a) (car range-b)))

(define (%range-contiguous? range-a range-b)
  ;;Assuming that RANGE-A  and RANGE-B are valid ranges:  Return true if
  ;;RANGE-A and RANGE-B are contiguous.
  (= (cdr range-a) (car range-b)))

(define (%range-start<? range-a range-b)
  ;;Assuming that RANGE-A  and RANGE-B are valid ranges:  Return true if
  ;;the start of RANGE-A is less than the start of RANGE-B.
  (< (car range-a) (car range-b)))

(define (%range-start<=? range-a range-b)
  ;;Assuming that RANGE-A  and RANGE-B are valid ranges:  Return true if
  ;;the  start of  RANGE-A  is less  than,  or equal  to,  the start  of
  ;;RANGE-B.
  (<= (car range-a) (car range-b)))

(define (%range-overlapping? range-a range-b)
  ;;Assuming that RANGE-A  and RANGE-B are valid ranges:  return true if
  ;;the ranges have some characters in common.
  (let ((start-a (car range-a)) (past-a (cdr range-a))
	(start-b (car range-b)) (past-b (cdr range-b)))
    (or (< start-a start-b past-a)
	(< start-b start-a past-b))))

(define (%range-concatenate range-a range-b)
  ;;Assuming that RANGE-A and  RANGE-B are valid ranges: Concatenate the
  ;;ranges  and return  the resulting  range; it  does not  matter which
  ;;range has start  less than the other.  It makes  sense to apply this
  ;;function to ranges that satisfy %RANGE-CONTIGUOUS.
  (cons (min (car range-a) (car range-b))
	(max (cdr range-a) (cdr range-b))))

(define (%range-intersection range-a range-b)
  ;;Assuming  that RANGE-A  and  RANGE-B are  valid  ranges: Return  the
  ;;intersection of the ranges; it does not matter which range has start
  ;;less than the other.
  ;;
  ;;Intersection  is a  closed operation  on  the space  of ranges:  The
  ;;intersection  of two  ranges is  a range  (possibly empty).   If the
  ;;ranges  are not  overlapping  return false  to  represent the  empty
  ;;range.
  (let ((start-a (car range-a)) (past-a (cdr range-a))
	(start-b (car range-b)) (past-b (cdr range-b)))
    (if (or (<= start-a start-b past-a)
	    (<= start-b start-a past-b))
	(cons (max start-a start-b)
	      (min past-a past-b))
      #f)))

(define (%range-union range-a range-b)
  ;;Assuming that RANGE-A and RANGE-B are valid ranges: Return the union
  ;;range; it does not matter which range has start less than the other.
  ;;
  ;;Union is not a closed operation on the space of ranges: The union of
  ;;two ranges can be a range  (possibly empty) or a list of ranges.  If
  ;;the ranges  are not overlapping: The  return value is a  list of two
  ;;ranges.
  (let ((start-a (car range-a)) (past-a (cdr range-a))
	(start-b (car range-b)) (past-b (cdr range-b)))
    (cond
     ((= past-a start-b)
      (cons start-a past-b))
     ((= past-b start-a)
      (cons start-b past-a))
     ((or (<= start-a start-b past-a)
	  (<= start-b start-a past-b))
      (cons (min start-a start-b)
	    (max past-a past-b)))
     ((< start-a start-b)
      (list range-a range-b))
     (else
      (list range-b range-a)))))

(define (%range-difference range-a range-b)
  ;;Assuming  that RANGE-A  and  RANGE-B are  valid  ranges: Return  the
  ;;difference between  the ranges; it  does not matter which  range has
  ;;start less than the other.
  ;;
  ;;Difference is  not a  closed operation on  the space of  ranges: The
  ;;union of  two ranges can  be a range  (possibly empty) or a  list of
  ;;ranges.  If  the ranges are not  overlapping: The return  value is a
  ;;list  of two  ranges.   If the  ranges  are equal:  Return false  to
  ;;represent the empty string.
  (let-values (((range-a range-b) (if (%range-start<? range-a range-b)
				      (values range-a range-b)
				    (values range-b range-a))))
    (let ((start-a (car range-a)) (past-a (cdr range-a))
	  (start-b (car range-b)) (past-b (cdr range-b)))
      (cond
       ((= past-a start-b)
	(cons start-a past-b))

       ((= start-a start-b)
	(cond
	 ((< past-a past-b)
	  (cons past-a past-b))
	 ((> past-a past-b)
	  (cons past-b past-a))
	 (else
	  #f)))

       ((= past-a past-b)
	(cond
	 ((< start-a start-b)
	  (cons start-a start-b))
	 ;;START-A cannot be > of START-B because we sorted them before.
	 (else
	  #f)))

       ((< start-a start-b past-a)
	(if (< start-a past-b past-a)
	    (list (cons start-a start-b)
		  (cons past-b past-a))
	  (list (cons start-a start-b)
		(cons past-a past-b))))

       ((< start-a start-b)
	(list range-a range-b))
       (else
	(list range-b range-a))))))

(define (%range-for-each proc range)
  (do ((i (car range) (+ 1 i)))
      ((<= (cdr range) i))
    (proc (integer->char i))))

(define (%range-every proc range)
  (let ((last (cdr range)))
    (let loop ((i (car range)))
      (or (< i last)
	  (and (proc (integer->char i))
	       (loop (+ 1 i)))))))

(define (%range-any proc range)
  (let ((last (cdr range)))
    (let loop ((i (car range)))
      (if (< i last)
	  (or (proc (integer->char i))
	      (loop (+ 1 i)))
	#f))))

(define (%range-fold kons knil range)
  (let ((last (cdr range)))
    (let loop ((i (car range))
	       (knil knil))
      (if (< i last)
	  (loop (+ 1 i) (kons (integer->char i) knil))
	knil))))


;;; lists of ranges

(define (%set? ell)
  ;;Scan a list to  determine if it is a valid list  of ranges; an empty
  ;;list is a valid list of ranges.
  (every %valid-range? ell))

(define (%make-set))

(define (%set-size set)
  (fold (lambda (range size)
	  (+ size (%range-length range)))
	0 set))

(define (%set-empty?))

(define (%set-contains? ell char)
  ;;Assuming ELL is a valid set: Return  true if CHAR is a member of the
  ;;set.
  (let ((char-num (integer->char char)))
    (any (lambda (range)
	   (%range-contains? range char-num))
      ell)))

(define (%set=? set-a set-b)
  (every %range=? set-a set-b))

(define (%set<? set-a set-b)
  (%range<? (last set-a) (car set-b)))

(define (%set-intersection))

(define (%set-union))

(define (%set-difference))

(define (%set-for-each proc set)
  (for-each (lambda (range)
	      (%range-for-each proc range))
    set))

(define (%set-every proc set)
  (every (lambda (range)
	   (%range-every proc range))
    set))

(define (%set-any proc set)
  (any (lambda (range)
	 (%range-any proc range))
    set))

(define (%set-fold kons knil set)
  (let loop ((set set)
	     (knil knil))
    (if (null? set)
	knil
      (loop (cdr set) (%range-fold kons knil (car set))))))


;;;; done

)

;;; end of file
