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
    %range=? %range<? %range>?
    %range-start<? %range-start<=?
    %range-contiguous? %range-overlapping?

    ;; range set operations
    %range-concatenate
    %range-union %range-intersection %range-difference

    ;; range list operations
    %range-for-each %range-fold %range-every %range-any
    %range->list

    ;; set constructors
    %make-set %set-copy

    ;; set inspection
    %set? %set-size %set-empty? %set-contains?

    ;; set mutation
    %set-add-char %set-add-range

    ;; set comparison
    %set=? %set<?

    ;; set set operations
    %set-intersection %set-union %set-difference %set-complement

    ;; set list operations
    %set-for-each %set-every %set-any %set-fold
    %set->list)
  (import (rnrs)
    (lists)
    (rnrs mutable-pairs))


;;;; ranges
;;
;;A "range" is a pair representing a half-open interval of numbers.  The
;;car of  the pair if the  included left-limit, the cdr  is the excluded
;;right-limit.
;;
;;Empty ranges (pairs with equal values) are invalid.
;;

(define (%valid-range-limit num)
  ;;Return true if NUM is a valid range limit.
  (and (integer? num) (exact? num) (<= 0 num)))

(define (%valid-range? range)
  ;;Return true if RANGE is a valid range value.
  (and (pair? range)
       (%valid-range-limit (car range))
       (%valid-range-limit (car range))
       (not (%range-empty? range))))

(define (%make-range start past)
  ;;Compose the start index and the  past index into a range cons.  This
  ;;function  validates the  arguments to  make sure  that the  range is
  ;;valid.
  (if (and (%valid-range-limit start)
	   (%valid-range-limit past)
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
  (<= (cdr range-a) (car range-b)))

(define (%range>? range-a range-b)
  ;;Assuming that RANGE-A  and RANGE-B are valid ranges:  Return true if
  ;;RANGE-B has all members less than all members of RANGE-A.
  (<= (cdr range-b) (car range-a)))

(define (%range-contiguous? range-a range-b)
  ;;Assuming that RANGE-A  and RANGE-B are valid ranges:  Return true if
  ;;RANGE-A and RANGE-B  are contiguous; it does not  matter which range
  ;;has start less than the other.
  (or (= (cdr range-a) (car range-b))
      (= (cdr range-b) (car range-a))))

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
    (or (and (<= start-a start-b) (< start-b past-a))
	(and (<= start-b start-a) (< start-a past-b)))))

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
  ;;Union is NOT a closed operation on the space of ranges: The union of
  ;;two ranges in general can be a set of two ranges.
  ;;
  ;;Assuming  that RANGE-A  and  RANGE-B are  valid  ranges: Return  two
  ;;values holding  the head and tail  of the union; it  does not matter
  ;;which range  has start less than the  other.  It can be  that one of
  ;;the returned values is #f.
  (let ((start-a (car range-a)) (past-a (cdr range-a))
	(start-b (car range-b)) (past-b (cdr range-b)))
    (cond
     ((= past-a start-b)
      (values #f (cons start-a past-b)))
     ((= past-b start-a)
      (values #f (cons start-b past-a)))
     ((or (<= start-a start-b past-a)
	  (<= start-b start-a past-b))
      (values #f (cons (min start-a start-b)
		       (max past-a past-b))))
     ((< start-a start-b)
      (list range-a range-b))
     (else
      (list range-b range-a)))))

(define (%range-difference range-a range-b)
  ;;Difference is  NOT a  closed operation on  the space of  ranges: The
  ;;difference between two  ranges in general is a set of two ranges.
  ;;
  ;;Assuming  that RANGE-A  and  RANGE-B are  valid  ranges: Return  two
  ;;values  holding the head  and tail  of the  difference; it  does not
  ;;matter which  range has start less  than the other.  It  can be that
  ;;one or both the returned values are #f.
  (let-values (((range-a range-b) (if (%range-start<? range-a range-b)
				      (values range-a range-b)
				    (values range-b range-a))))
    (let ((start-a (car range-a)) (past-a (cdr range-a))
	  (start-b (car range-b)) (past-b (cdr range-b)))
      (cond
       ((= past-a start-b)
	(values #f (cons start-a past-b)))

       ((= start-a start-b)
	(values #f (cond
		    ((< past-a past-b)
		     (cons past-a past-b))
		    ((> past-a past-b)
		     (cons past-b past-a))
		    (else
		     #f))))

       ((= past-a past-b)
	(values #f (cond
		    ((< start-a start-b)
		     (cons start-a start-b))
		    ;;START-A cannot  be > of START-B  because we sorted
		    ;;them before.
		    (else
		     #f))))

       ((< start-a start-b past-a)
	(if (< start-a past-b past-a)
	    (values (cons start-a start-b)
		    (cons past-b past-a))
	  (values (cons start-a start-b)
		  (cons past-a past-b))))

       ((< start-a start-b)
	(values range-a range-b))
       (else
	(values range-b range-a))))))

(define (%range-for-each proc range)
  ;;Assuming RANGE is a valid range: Apply PROC to each character in the
  ;;range.
  (do ((i (car range) (+ 1 i)))
      ((<= (cdr range) i))
    (proc (integer->char i))))

(define (%range-every proc range)
  ;;Assuming RANGE is a valid range: Apply PROC to each character in the
  ;;range  and return  true  if all  the  return values  are true.   The
  ;;application stops at the first false return value.
  (let ((past (cdr range)))
    (let loop ((i (car range)))
      (if (< i past)
	  (and (proc (integer->char i))
	       (loop (+ 1 i)))
	#t))))

(define (%range-any proc range)
  ;;Assuming RANGE is a valid range: Apply PROC to each character in the
  ;;range  and return  true  at least  one  return value  is true.   The
  ;;application stops at the first true return value.
  (let ((past (cdr range)))
    (let loop ((i (car range)))
      (if (< i past)
	  (or (proc (integer->char i))
	      (loop (+ 1 i)))
	#f))))

(define (%range-fold kons knil range)
  ;;Assuming RANGE  is a valid range:  Fold KONS over  the characters in
  ;;the range.
  (let ((past (cdr range)))
    (let loop ((i (car range))
	       (knil knil))
      (if (< i past)
	  (loop (+ 1 i) (kons (integer->char i) knil))
	knil))))

(define (%range->list range)
  ;;Assuming  RANGE is  a valid  range: Return  a list  holding  all the
  ;;characters in the range.
  (%range-fold cons '() range))


;;;; sets
;;
;;A "set"  is a  sorted lists  of ranges.  Empty  sets are  empty lists.
;;Ranges in a set do not overlap and are not contiguous.  Each range has
;;right limit strictly less than the left limit of its subsequent:
;;
;;   ((left1 . right1) (left2 . right2) (left3 . right3) ...)
;;
;;   left1 < right1 < left2 < right2 < left3 < right3 < ...
;;

(define (%set? set lower-bound upper-bound)
  ;;Scan a list to  determine if it is a valid list  of ranges; an empty
  ;;list is a valid list of ranges.
  (let loop ((set set))
    (if (null? set)
	#t
      (let ((range (car set)))
	(cond
	 ((not (%valid-range? range))
	  #f)
	 ((< (car range) lower-bound)
	  #f)
	 ((<= upper-bound (cdr range))
	  #f)
	 (else
	  (loop (cdr set))))))))

(define (%make-set . chars)
  ;;Build and return a new set.  CHARS  may be the empty list or a mixed
  ;;list of characters and pairs of characters.  Pairs of characters are
  ;;interpreted  as limits-inclusive  ranges of  characters (this  is in
  ;;contrast with the half-open representation of number ranges).
  (let loop ((chars chars)
	     (set '()))
    (if (null? chars)
	set
      (let ((thing (car chars)))
	(cond
	 ((char? thing)
	  (loop (cdr chars) (%set-add-char set thing)))
	 ((and (pair? thing)
	       (let ((first (car thing))
		     (last  (cdr thing)))
		 (and (char? first)
		      (char? last)
		      (<= (char->integer first) (char->integer last)))))
	  (loop (cdr chars)
		(%set-add-range set (cons (char->integer (car thing))
					  (+ 1 (char->integer (cdr thing)))))))
	 (else
	  (assertion-violation '%make-set
	    "invalid element for char set" thing)))))))

(define (%set-add-char set ch)
  ;;Assuming SET is a valid set and CH a character: Convert the caracter
  ;;to its  one-element range  representation, then add  it to  the set.
  ;;Return the resulting set.
  (let ((chnum (char->integer ch)))
    (%set-add-range set (cons chnum (+ 1 chnum)))))

(define (%set-add-range set new-range)
  ;;Assuming SET is  a valid set NEW-RANGE a valid  range: add the range
  ;;to the set, return the resulting set.
  (let loop ((set set)
	     (result '()))
    (if (%set-empty? set)
	(reverse (cons new-range result))
      (let ((range (car set)))
;;;	  (write (list 'range range 'new-range new-range))(newline)
	(cond
	 ((%range=? range new-range)
;;;	    (write (list 'equal range new-range))(newline)
	  (append-reverse (cons range result) (cdr set)))
	 ((%range-overlapping? range new-range)
;;;	    (write (list 'overlapping range new-range))(newline)
	  (let loop2 ((set       (cdr set))
		      (new-range (let-values (((head tail) (%range-union range new-range)))
				   tail)))
	    (if (null? set)
		(reverse (cons new-range result))
	      (let ((range (car set)))
		(cond ((%range-contiguous? range new-range)
		       (loop2 (cdr set) (%range-concatenate range new-range)))
		      ((%range-overlapping? range new-range)
		       (loop2 (cdr set) (let-values (((head tail) (%range-union range new-range)))
					  tail)))
		      (else
		       (append-reverse (cons new-range (cons range result))
				       set)))))))
	 ((%range-contiguous? range new-range)
;;;	    (write (list 'contig range new-range))(newline)
	  (let loop2 ((set		(cdr set))
		      (new-range	(%range-concatenate range new-range)))
	    (if (null? set)
		(reverse (cons new-range result))
	      (let ((range (car set)))
		(cond ((%range-contiguous? range new-range)
		       (loop2 (cdr set) (%range-concatenate range new-range)))
		      ((%range-overlapping? range new-range)
		       (loop2 (cdr set) (let-values (((head tail) (%range-union range new-range)))
					  tail)))
		      (else
		       (append-reverse (cons new-range (cons range result))
				       set)))))))
	 ((%range<? new-range range)
;;;	    (write (list 'less range new-range))(newline)
	  (append-reverse (cons range (cons new-range result))
			  (cdr set)))
	 (else
;;;	    (write (list 'other range new-range))(newline)
	  (loop (cdr set) (cons range result))))))))

(define %set-copy
  ;;Return  a  newly allocated  set  holding a  copy  of  the given  set
  ;;argument.
  tree-copy)

(define (%set-size set)
  ;;Assuming SET  is a valid set:  return the number of  elements in the
  ;;set.
  (fold (lambda (range size)
	  (+ size (%range-length range)))
	0 set))

(define %set-empty?
  ;;Return true if the set is empty.
  null?)

(define (%set-contains? set ch)
  ;;Assuming SET is a valid set and CH a character: Return true if CH is
  ;;a member of the set.
  (let ((chnum (char->integer ch)))
    (any (lambda (range)
	   (%range-contains? range chnum))
      set)))

(define (%set=? set-a set-b)
  ;;Assuming  SET-A  and SET-B  are  valid  sets:  Return true  if  they
  ;;represent the same set.
  (or (eq? set-a set-b)
      (cond
       ((null? set-a) #f)
       ((null? set-b) #f)
       (else
	(every %range=? set-a set-b)))))

(define (%set<? set-a set-b)
  ;;Assuming  SET-A and SET-B  are valid  sets: Return  true if  all the
  ;;elements of SET-A are strictly  less than all the elements of SET-B.
  ;;Empty sets cannot be ordered, so  if an argument is empty the return
  ;;value is false.
  (cond ((null? set-a) #f)
	((null? set-b) #f)
	(else
	 (%range<? (last set-a) (car set-b)))))

(define (%set-intersection set-a set-b)
  ;;Assuming SET-A  and SET-B are valid  sets: Return a  set holding all
  ;;the elements present in both of them.
  (let loop ((result	'())
	     (set-a	set-a)
	     (set-b	set-b))
    (if (or (%set-empty? set-a)
	    (%set-empty? set-b))
	(reverse result)
      (let ((range-a	(car set-a))
	    (range-b	(car set-b)))
;;;	  (write (list 'processing range-a range-b))(newline)
	(cond
	 ((%range=? range-a range-b)
;;;	    (write (list 'equal range-a range-b))(newline)
	  (loop (cons range-a result)
		(cdr set-a) (cdr set-b)))
	 ((%range-overlapping? range-a range-b)
;;;	    (write (list 'overlapping range-a range-b))(newline)
	  (loop (cons (%range-intersection range-a range-b) result)
		(cdr set-a) (cdr set-b)))
	 ((%range<? range-a range-b)
;;;	    (write (list 'less-than range-a range-b))(newline)
	  (loop result
		(cdr set-a) set-b))
	 ((%range>? range-a range-b)
;;;	    (write (list 'greater-than range-a range-b))(newline)
	  (loop result
		set-a   (cdr set-b)))
	 (else
	  (assertion-violation '%set-intersection
	    "internal error processing ranges" (list range-a range-b))))))))

(define (%set-union set-a set-b)
  ;;Assuming SET-A  and SET-B are valid  sets: Return a  set holding all
  ;;the elements present in SET-A and/or SET-B.
  (let loop ((result '())
	     (set-a set-a)
	     (set-b set-b))
    (cond
     ((%set-empty? set-a)
      (append-reverse result set-b))
     ((%set-empty? set-b)
      (append-reverse result set-a))
     (else
      (let ((range-a (car set-a))
	    (range-b (car set-b)))
	(cond
	 ((and (not (null? result))
	       (%range-contiguous? (car result) range-a))
	  (loop (cons (%range-concatenate (car result) range-a) (cdr result))
		(cdr set-a) (cons range-b set-b)))

	 ((and (not (null? result))
	       (%range-contiguous? (car result) range-b))
	  (loop (cons (%range-concatenate (car result) range-b) (cdr result))
		(cons range-a set-a) (cdr set-b)))

	 ((and (not (null? result))
	       (%range-overlapping? (car result) range-a))
	  (let-values (((head tail) (%range-union (car result) range-a)))
	    (loop (cons tail (cdr result))
		  (cdr set-a) (cons range-b set-b))))

	 ((and (not (null? result))
	       (%range-overlapping? (car result) range-b))
	  (let-values (((head tail) (%range-union (car result) range-b)))
	    (loop (cons tail (cdr result))
		  (cons range-a set-a) (cdr set-b))))

	 ((%range=? range-a range-b)
	  (loop (cons range-a result)
		(cdr set-a) (cdr set-b)))

	 ((%range-contiguous? range-a range-b)
	  (loop (cons (%range-concatenate range-a range-b) result)
		(cdr set-a) (cdr set-b)))

	 ((%range-overlapping? range-a range-b)
	  (let-values (((head tail) (%range-union range-a range-b)))
	    (loop (cons tail result)
		  (cdr set-a) (cdr set-b))))

	 ((%range<? range-a range-b)
	  (loop (cons range-b (cons range-a result))
		(cdr set-a) (cdr set-b)))

	 ((%range>? range-a range-b)
	  (loop (cons range-a (cons range-b result))
		(cdr set-a) (cdr set-b)))

	 (else
	  (assertion-violation '%set-union
	    "internal error processing ranges" (list range-a range-b)))))))))

(define (%set-difference set-a set-b)
  ;;Assuming SET-A  and SET-B are valid  sets: Return a  set holding all
  ;;the elements present only in SET-A or only in SET-B.
  (let loop ((result '())
	     (set-a set-a)
	     (set-b set-b))
    (cond
     ((%set-empty? set-a)
      (append-reverse result set-b))
     ((%set-empty? set-b)
      (append-reverse result set-a))
     (else
      (let ((range-a (car set-a))
	    (range-b (car set-b)))
	(cond
	 ((and (not (null? result))
	       (%range-contiguous? (car result) range-a))
	  (loop (cons (%range-concatenate (car result) range-a) (cdr result))
		(cdr set-a) (cons range-b set-b)))

	 ((and (not (null? result))
	       (%range-contiguous? (car result) range-b))
	  (loop (cons (%range-concatenate (car result) range-b) (cdr result))
		(cons range-a set-a) (cdr set-b)))

	 ((and (not (null? result))
	       (%range-overlapping? (car result) range-a))
	  (loop (cons (%range-union (car result) range-a) (cdr result))
		(cdr set-a) (cons range-b set-b)))

	 ((and (not (null? result))
	       (%range-overlapping? (car result) range-b))
	  (loop (cons (%range-union (car result) range-b) (cdr result))
		(cons range-a set-a) (cdr set-b)))

	 ((%range=? range-a range-b)
	  (loop result
		(cdr set-a) (cdr set-b)))

	 ((%range-contiguous? range-a range-b)
	  (loop (cons (%range-concatenate range-a range-b) result)
		(cdr set-a) (cdr set-b)))

	 ((%range-overlapping? range-a range-b)
	  (let-values (((head tail) (%range-difference range-a range-b)))
	    (loop (let ((result (if head
				    (cons head result)
				  result)))
		    (if tail
			(cons tail result)
		      result))
		  (cdr set-a) (cdr set-b))))

	 ((%range<? range-a range-b)
	  (loop (cons range-b (cons range-a result))
		(cdr set-a) (cdr set-b)))

	 ((%range>? range-a range-b)
	  (loop (cons range-a (cons range-b result))
		(cdr set-a) (cdr set-b)))

	 (else
	  (assertion-violation '%set-union
	    "internal error processing ranges" (list range-a range-b)))))))))

(define (%set-complement set)
  ;;Assuming SET is  a valid set: Return a set  holding all the elements
  ;;in the underlying space that are not in SET.
  #t)

(define (%set-for-each proc set)
  ;;Assuming PROC is a unary function and SET a valid set: apply PROC to
  ;;each element of SET.
  (for-each (lambda (range)
	      (%range-for-each proc range))
    set))

(define (%set-every proc set)
  ;;Assuming PROC is a unary function and SET a valid set: apply PROC to
  ;;each element  of SET and  return true if  all the return  values are
  ;;true.  The application stops at the first #f return value.
  (every (lambda (range)
	   (%range-every proc range))
    set))

(define (%set-any proc set)
  ;;Assuming PROC is a unary function and SET a valid set: apply PROC to
  ;;each  element of SET  and return  true if  at least  one application
  ;;returns  true.   The  application  stops at  the  first  application
  ;;returning true.
  (any (lambda (range)
	 (%range-any proc range))
    set))

(define (%set-fold kons knil set)
  ;;Assuming KONS  is a unary  function and SET  a valid set:  fold KONS
  ;;over the elements of the set.
  (let loop ((set set)
	     (knil knil))
    (if (null? set)
	knil
      (loop (cdr set) (%range-fold kons knil (car set))))))

(define (%set->list set)
  ;;Assuming SET is a valid set:  Return a list holding all the elements
  ;;in the set.
  (reverse (apply append (map %range->list set))))


;;;; done

)

;;; end of file
