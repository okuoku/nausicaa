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
    %range-length %range-empty? %range-member?

    ;; range comparison
    %range=? %range<?
    %range-start<? %range-start<=?
    %range-contiguous? %range-overlapping?

    ;; range operations
    %range-concatenate %range-union

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

(define (%range-member? range char-num)
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

(define (%range-union range-a range-b)
  ;;Assuming that RANGE-A and  RANGE-B are valid ranges: Concatenate the
  ;;ranges  and return  the resulting  range; it  does not  matter which
  ;;range  has  start  less than  the  other.   If  the ranges  are  not
  ;;overlapping: An assertion violation is raised.
  (let ((start-a (car range-a)) (past-a (cdr range-a))
	(start-b (car range-b)) (past-b (cdr range-b)))
    (if (or (<= start-a start-b past-a)
	    (<= start-b start-a past-b))
	(cons (min start-a start-b)
	      (max past-a past-b))
    (assertion-violation '%range-union
      "cannot apply union to non-overlapping ranges"
      (list range-a range-b)))))


;;; lists of ranges

(define (%set? ell)
  ;;Scan a list to  determine if it is a valid list  of ranges; an empty
  ;;list is a valid list of ranges.
  (every %valid-range? ell))

(define (%set-member? ell char)
  ;;Assuming ELL is a valid set: Return  true if CHAR is a member of the
  ;;set.
  (let ((char-num (integer->char char)))
    (any (lambda (range)
	   (%range-member? range char-num))
      ell)))


;;;; done

)

;;; end of file
