;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: test for char-sets low level library
;;;Date: Wed Jun 10, 2009
;;;
;;;Abstract
;;;
;;;	Notice  that the  maximum integer  accepted by  INTEGER->CHAR is
;;;	1114111.
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


;;;; setup

(import (nausicaa)
  (checks)
  (char-sets low))

(check-set-mode! 'report-failed)
(display "*** testing char-sets low\n")

(define upper-bound (+ 1 1114111))


(parameterise ((check-test-name	'integer)
	       (debugging #t))

  (check
      (char? (integer->char 1114111))
    => #t)

  (check
      (guard (exc ((condition? exc) #t))
	(char? (integer->char 1114112)))
    => #t)

  )


(parameterise ((check-test-name	'range-constructors))

  (check
      (%make-range 10 20)
    => '(10 . 20))

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(%make-range 10 10))
    => #t)

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(%make-range -10 20))
    => #t)

  )


(parameterise ((check-test-name	'range-inspection))

  (check
      (%range-length (%make-range 10 20))
    => 10)

  (check
      (%range-length (%make-range 10 11))
    => 1)

;;; --------------------------------------------------------------------

  (check
      (%range-empty? '(10 . 20))
    => #f)

  (check
      (%range-empty? '(10 . 10))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (%range-contains? (%make-range 10 20) 10)
    => #t)

  (check
      (%range-contains? (%make-range 10 20) 20)
    => #f)

  (check
      (%range-contains? (%make-range 10 20) 1)
    => #f)

  (check
      (%range-contains? (%make-range 10 20) 30)
    => #f)

  (check
      (%range-contains? (%make-range 10 20) 15)
    => #t)

  )


(parameterise ((check-test-name	'range-comparison))

  (check
      (%range=? (%make-range 10 20)
		(%make-range 10 20))
    => #t)

  (check
      (%range=? (%make-range 9 20)
		(%make-range 10 20))
    => #f)

  (check
      (%range=? (%make-range 10 20)
		(%make-range 10 21))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (%range<? (%make-range 10 20)
		(%make-range 10 20))
    => #f)

  (check
      (%range<? (%make-range 10 20)
		(%make-range 30 40))
    => #t)

  (check
      (%range<? (%make-range 30 40)
		(%make-range 10 20))
    => #f)

  (check
      (%range<? (%make-range 10 20)
		(%make-range 15 25))
    => #f)

  (check
      (%range<? (%make-range 15 25)
		(%make-range 10 20))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (%range-start<? (%make-range 10 20)
		      (%make-range 15 25))
    => #t)

  (check
      (%range-start<? (%make-range 15 20)
		      (%make-range 15 25))
    => #f)

  (check
      (%range-start<? (%make-range 18 20)
		      (%make-range 15 25))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (%range-start<=? (%make-range 10 20)
		       (%make-range 15 25))
    => #t)

  (check
      (%range-start<=? (%make-range 15 20)
		       (%make-range 15 25))
    => #t)

  (check
      (%range-start<=? (%make-range 18 20)
		       (%make-range 15 25))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (%range-contiguous? (%make-range 10 20)
			  (%make-range 20 25))
    => #t)

  (check
      (%range-contiguous? (%make-range 10 30)
			  (%make-range 20 40))
    => #f)

  (check
      (%range-contiguous? (%make-range 20 40)
			  (%make-range 10 30))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (%range-overlapping? (%make-range 10 20)
			   (%make-range 20 25))
    => #f)

  (check
      (%range-overlapping? (%make-range 10 30)
			   (%make-range 20 40))
    => #t)

  (check
      (%range-overlapping? (%make-range 20 40)
			   (%make-range 10 30))
    => #t)

  )


(parameterise ((check-test-name	'range-set-operation))

  (check
      (%range-concatenate (%make-range 10 20)
			  (%make-range 10 20))
    (=> %range=?)
    (%make-range 10 20))

  (check
      (%range-concatenate (%make-range 10 20)
			  (%make-range 30 40))
    (=> %range=?)
    (%make-range 10 40))

;;; --------------------------------------------------------------------

  (check
      ;; equal
      (let-values (((head tail) (%range-union (%make-range 10 20)
					      (%make-range 10 20))))
	(list head tail))
    => (list #f (%make-range 10 20)))

  (check
      ;; overlapping
      (let-values (((head tail) (%range-union (%make-range 10 30)
					      (%make-range 20 40))))
	(list head tail))
    => (list #f (%make-range 10 40)))

  (check
      ;; overlapping
      (let-values (((head tail) (%range-union (%make-range 20 40)
					      (%make-range 10 30))))
	(list head tail))
    => (list #f (%make-range 10 40)))

  (check
      ;; contiguous
      (let-values (((head tail) (%range-union (%make-range 10 20)
					      (%make-range 20 40))))
	(list head tail))
    => (list #f (%make-range 10 40)))

  (check
      ;; contiguous
      (let-values (((head tail) (%range-union (%make-range 20 40)
					      (%make-range 10 20))))
	(list head tail))
    => (list #f (%make-range 10 40)))

  (check
      ;; dijoint
      (let-values (((head tail) (%range-union (%make-range 10 20)
					      (%make-range 30 40))))
	(list head tail))
    => '((10 . 20) (30 . 40)))

  (check
      ;; dijoint
      (let-values (((head tail) (%range-union (%make-range 30 40)
					      (%make-range 10 20))))
	(list head tail))
    => '((10 . 20) (30 . 40)))

;;; --------------------------------------------------------------------

  (check
      ;; equal ranges
      (let-values (((head tail) (%range-difference (%make-range 10 20)
						   (%make-range 10 20))))
	(list head tail))
    => '(#f #f))

  (check
      ;; overlapping ranges
      (let-values (((head tail) (%range-difference (%make-range 10 30)
						   (%make-range 20 40))))
	(list head tail))
    => '((10 . 20) (30 . 40)))

  (check
      ;; overlapping ranges
      (let-values (((head tail) (%range-difference (%make-range 20 40)
						   (%make-range 10 30))))
	(list head tail))
    => '((10 . 20) (30 . 40)))

  (check
      ;; non-overlapping ranges
      (let-values (((head tail) (%range-difference (%make-range 10 20)
						   (%make-range 30 40))))
	(list head tail))
    => '((10 . 20) (30 . 40)))

  (check
      ;; non-overlapping ranges
      (let-values (((head tail) (%range-difference (%make-range 30 40)
						   (%make-range 10 20))))
	(list head tail))
    => '((10 . 20) (30 . 40)))

  (check
      ;; contiguous ranges
      (let-values (((head tail) (%range-difference (%make-range 10 20)
						   (%make-range 20 40))))
	(list head tail))
    => (list #f (%make-range 10 40)))

  (check
      ;; contiguous ranges
      (let-values (((head tail) (%range-difference (%make-range 20 40)
						   (%make-range 10 20))))
	(list head tail))
    => (list #f (%make-range 10 40)))

  )


(parameterise ((check-test-name	'range-list-operation))

  (check
      (with-result
       (%range-for-each (lambda (ch)
			  (add-result (char->integer ch)))
			(%make-range 10 15))
       'here)
    => '(here (10 11 12 13 14)))

;;; --------------------------------------------------------------------

  (check
      (%range-every (lambda (ch)
		      (let ((n (char->integer ch)))
			(and (<= 10 n) (< n 15))))
		    (%make-range 10 15))
    => #t)

  (check
      (%range-every (lambda (ch)
		      (let ((n (char->integer ch)))
			(and (<= 10 n) (< n 13))))
		    (%make-range 10 15))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (%range-any (lambda (ch)
		    (let ((n (char->integer ch)))
		      (= 13 n)))
		  (%make-range 10 15))
    => #t)

  (check
      (%range-any (lambda (ch)
		      (let ((n (char->integer ch)))
			(= 100 n)))
		    (%make-range 10 15))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (%range-fold (lambda (ch knil)
		     (cons (char->integer ch) knil))
		   '()
		   (%make-range 10 15))
    => '(14 13 12 11 10))

;;; --------------------------------------------------------------------

  (check
      (%range->list (%make-range 65 70))
    => (reverse '(#\A #\B #\C #\D #\E)))

  (check
      (%range->list (%make-range 65 66))
    => '(#\A))

  )


(parameterise ((check-test-name	'set-predicate))

  (check
      (%set? '() 0 upper-bound)
    => #t)

  (check
      (%set? '((10 . 12)) 0 upper-bound)
    => #t)

  (check
      (%set? '((10 . 20)
	       (30 . 40))
	     0 upper-bound)
    => #t)

  (check
      (%set? '((10 . 20)
	       (20 . 40))
	     0 upper-bound)
    => #t)

  (check
      (%set? '((10 . 20)
	       (30 . 40)
	       (50 . 60))
	     0 upper-bound)
    => #t)

  (check
      (%set? `((10 . 20)
	       (30 . 40)
	       (50 . ,upper-bound))
	     0 upper-bound)
    => #f)

  )


(parameterise ((check-test-name	'set-constructor))

  ;;Tests   for   %MAKE-SET   tests  automatically   %SET-ADD-CHAR   and
  ;;%SET-ADD-RANGE.

  (check
      (%make-set)
    => '())

  (check
      (%make-set #\A)
    => '((65 . 66)))

  (check (%make-set #\A #\A) => '((65 . 66)))
  (check (%make-set #\A #\A #\A) => '((65 . 66)))
  (check (%make-set #\A #\B #\A) => '((65 . 67)))
  (check (%make-set #\A #\A #\B #\A) => '((65 . 67)))

  (check (%make-set #\A #\B) => '((65 . 67)))
  (check (%make-set #\B #\A) => '((65 . 67)))

  (check (%make-set #\A #\B #\C) => '((65 . 68)))
  (check (%make-set #\B #\A #\C) => '((65 . 68)))
  (check (%make-set #\C #\A #\B) => '((65 . 68)))
  (check (%make-set #\B #\C #\A) => '((65 . 68)))

  (check (%make-set #\A #\C) => '((65 . 66) (67 . 68)))
  (check (%make-set #\C #\A) => '((65 . 66) (67 . 68)))

;;; --------------------------------------------------------------------

  (check (%make-set '(#\A . #\B)) => '((65 . 67)))

  ;; equal
  (check (%make-set '(#\B . #\C) '(#\B . #\C)) => '((66 . 68)))

  ;; overlapping
  (check (%make-set '(#\A . #\B) '(#\B . #\C)) => '((65 . 68)))
  (check (%make-set '(#\B . #\C) '(#\A . #\B)) => '((65 . 68)))

  ;; contiguous
  (check (%make-set '(#\A . #\B) '(#\C . #\D)) => '((65 . 69)))
  (check (%make-set '(#\C . #\D) '(#\A . #\B)) => '((65 . 69)))

  ;; included
  (check (%make-set '(#\A . #\D) '(#\B . #\C)) => '((65 . 69)))
  (check (%make-set '(#\B . #\C) '(#\A . #\D)) => '((65 . 69)))

  ;; distanced
  (check (%make-set '(#\A . #\B) '(#\D . #\E)) => '((65 . 67) (68 . 70)))
  (check (%make-set '(#\D . #\E) '(#\A . #\B)) => '((65 . 67) (68 . 70)))

  (check (%make-set #\A #\D '(#\B . #\C)) => '((65 . 69)))
  (check (%make-set #\A '(#\B . #\C) #\D) => '((65 . 69)))
  (check (%make-set '(#\B . #\C) #\A #\D) => '((65 . 69)))
  (check (%make-set '(#\B . #\C) #\D #\A) => '((65 . 69)))

  )


(parameterise ((check-test-name	'set-inspection))

  (check
      (%set-empty? (%make-set))
    => #t)

  (check
      (%set-empty? (%make-set #\A))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (%set-size (%make-set))
    => 0)

  (check
      (%set-size (%make-set #\A))
    => 1)

  (check
      (%set-size (%make-set #\A #\C))
    => 2)

  (check
      (%set-size (%make-set '(#\A . #\D) '(#\F . #\H)))
    => (+ 4 3))

;;; --------------------------------------------------------------------

  (check
      (%set-contains? (%make-set '(#\A . #\F)) #\C)
    => #t)

  (check
      (%set-contains? (%make-set '(#\A . #\F)) #\M)
    => #f)

  )


(parameterise ((check-test-name	'set-comparison))

  (check (%set=? (%make-set) (%make-set #\A)) => #f)
  (check (%set=? (%make-set #\A) (%make-set)) => #f)

  (check
      (%set=? (%make-set #\A)
	      (%make-set #\A))
    => #t)

  (check
      (%set=? (%make-set #\A)
	      (%make-set #\B))
    => #f)

  (check
      (%set=? (%make-set '(#\A . #\G))
	      (%make-set '(#\A . #\G)))
    => #t)

  (check (%set=? (%make-set '(#\A . #\G)) (%make-set '(#\D . #\G))) => #f)
  (check (%set=? (%make-set '(#\D . #\G)) (%make-set '(#\A . #\G))) => #f)

  (check (%set=? (%make-set '(#\A . #\D)) (%make-set '(#\F . #\M))) => #f)
  (check (%set=? (%make-set '(#\F . #\M)) (%make-set '(#\A . #\D))) => #f)

;;; --------------------------------------------------------------------

  (check (%set<? (%make-set)     (%make-set #\B)) => #f)
  (check (%set<? (%make-set #\A) (%make-set))     => #f)

  (check (%set<? (%make-set #\A) (%make-set #\B)) => #t)
  (check (%set<? (%make-set #\A) (%make-set #\A)) => #f)
  (check (%set<? (%make-set #\B) (%make-set #\A)) => #f)

  )


(parameterise ((check-test-name	'set-set-operations))

  (check
      ;; empty
      (%set-intersection (%make-set)
			 (%make-set '(#\A . #\H)))
    (=> %set=?) (%make-set))

  (check
      ;; empty
      (%set-intersection (%make-set '(#\A . #\H))
			 (%make-set))
    (=> %set=?) (%make-set))

  (check
      ;; equal
      (%set-intersection (%make-set '(#\A . #\H))
			 (%make-set '(#\A . #\H)))
    (=> %set=?) (%make-set '(#\A . #\H)))

  (check
      ;; disjoint
      (%set-intersection (%make-set '(#\A . #\C))
			 (%make-set '(#\E . #\H)))
    (=> %set=?) (%make-set))

  (check
      ;; disjoint
      (%set-intersection (%make-set '(#\E . #\H))
			 (%make-set '(#\A . #\C)))
    (=> %set=?) (%make-set))

  (check
      ;; contiguous
      (%set-intersection (%make-set '(#\A . #\D))
			 (%make-set '(#\E . #\H)))
    (=> %set=?) (%make-set))

  (check
      ;; contiguous
      (%set-intersection (%make-set '(#\E . #\H))
			 (%make-set '(#\A . #\D)))
    (=> %set=?) (%make-set))

  (check
      ;; inclusion
      (%set-intersection (%make-set '(#\C . #\F))
			 (%make-set '(#\A . #\H)))
    (=> %set=?) (%make-set '(#\C . #\F)))

  (check
      ;; inclusion
      (%set-intersection (%make-set '(#\A . #\H))
			 (%make-set '(#\C . #\F)))
    (=> %set=?) (%make-set '(#\C . #\F)))

  (check
      (%set-intersection (%make-set '(#\A . #\D) '(#\H . #\M) '(#\O . #\P))
			 (%make-set '(#\C . #\F) '(#\I . #\L) '(#\N . #\Q)))
    (=> %set=?) (%make-set '(#\C . #\D)
			   '(#\I . #\L)
			   '(#\O . #\P)))

;;; --------------------------------------------------------------------

  (check
      ;; empty
      (%set-union (%make-set)
		  (%make-set '(#\A . #\H)))
    (=> %set=?) (%make-set '(#\A . #\H)))

  (check
      ;; empty
      (%set-union (%make-set '(#\A . #\H))
		  (%make-set))
    (=> %set=?) (%make-set '(#\A . #\H)))

  (check
      ;; equal
      (%set-union (%make-set '(#\A . #\H))
		  (%make-set '(#\A . #\H)))
    (=> %set=?) (%make-set '(#\A . #\H)))

  (check
      ;; disjoint
      (%set-union (%make-set '(#\A . #\C))
		  (%make-set '(#\E . #\H)))
    (=> %set=?) (%make-set '(#\A . #\C) '(#\E . #\H)))

  (check
      ;; disjoint
      (%set-union (%make-set '(#\E . #\H))
		  (%make-set '(#\A . #\C)))
    (=> %set=?) (%make-set '(#\A . #\C) '(#\E . #\H)))

  (check
      ;; contiguous
      (%set-union (%make-set '(#\A . #\D))
		  (%make-set '(#\E . #\H)))
    (=> %set=?) (%make-set '(#\A . #\H)))

  (check
      ;; contiguous
      (%set-union (%make-set '(#\E . #\H))
		  (%make-set '(#\A . #\D)))
    (=> %set=?) (%make-set '(#\A . #\H)))

  (check
      ;; inclusion
      (%set-union (%make-set '(#\C . #\F))
		  (%make-set '(#\A . #\H)))
    (=> %set=?) (%make-set '(#\A . #\H)))

  (check
      ;; inclusion
      (%set-union (%make-set '(#\A . #\H))
		  (%make-set '(#\C . #\F)))
    (=> %set=?) (%make-set '(#\A . #\H)))

  (check
      (%set-union (%make-set '(#\A . #\D) '(#\H . #\M) '(#\O . #\P))
		  (%make-set '(#\C . #\F) '(#\I . #\L) '(#\N . #\Q)))
    (=> %set=?) (%make-set '(#\A . #\F)
			   '(#\H . #\Q)))

;;; --------------------------------------------------------------------

  (check
      ;; empty
      (%set-difference (%make-set)
		       (%make-set '(#\A . #\H)))
    (=> %set=?) (%make-set '(#\A . #\H)))

  (check
      ;; empty
      (%set-difference (%make-set '(#\A . #\H))
		       (%make-set))
    (=> %set=?) (%make-set '(#\A . #\H)))

  (check
      ;; equal
      (%set-difference (%make-set '(#\A . #\H))
		       (%make-set '(#\A . #\H)))
    (=> %set=?) (%make-set))

  (check
      ;; disjoint
      (%set-difference (%make-set '(#\A . #\C))
		       (%make-set '(#\E . #\H)))
    (=> %set=?) (%make-set '(#\A . #\C) '(#\E . #\H)))

  (check
      ;; disjoint
      (%set-difference (%make-set '(#\E . #\H))
		       (%make-set '(#\A . #\C)))
    (=> %set=?) (%make-set '(#\A . #\C) '(#\E . #\H)))

  (check
      ;; contiguous
      (%set-difference (%make-set '(#\A . #\D))
		       (%make-set '(#\E . #\H)))
    (=> %set=?) (%make-set '(#\A . #\H)))

  (check
      ;; contiguous
      (%set-difference (%make-set '(#\E . #\H))
		       (%make-set '(#\A . #\D)))
    (=> %set=?) (%make-set '(#\A . #\H)))

  (check
      ;; inclusion
      (%set-difference (%make-set '(#\C . #\F))
		       (%make-set '(#\A . #\H)))
    (=> %set=?) (%make-set '(#\A . #\B)
			   '(#\G . #\H)))

  (check
      ;; inclusion
      (%set-difference (%make-set '(#\A . #\H))
		       (%make-set '(#\C . #\F)))
    (=> %set=?) (%make-set '(#\A . #\B)
			   '(#\G . #\H)))

  (check 'this
    (let ((r (%set-difference (%make-set '(#\A . #\D) '(#\H . #\M) '(#\O . #\P))
			      (%make-set '(#\C . #\F) '(#\I . #\L) '(#\N . #\Q)))))
;;;      (write r)(newline)
      r)
    (=> %set=?) (%make-set #\A #\B #\E #\F #\H #\M #\N #\Q))

  )


(parameterise ((check-test-name	'set-list-operations))

  (check
      (with-result
       (%set-for-each (lambda (ch)
			(add-result ch))
		      (%make-set #\A #\B #\C))
       #t)
    => '(#t (#\A #\B #\C)))

;;; --------------------------------------------------------------------

  (check
      (%set-every (lambda (ch)
		    (<= 65 (char->integer ch)))
		  (%make-set #\A #\B #\C))
    => #t)

  (check
      (%set-every (lambda (ch)
		    (<= 67 (char->integer ch)))
		  (%make-set #\A #\B #\C))
    => #f)


;;; --------------------------------------------------------------------

  (check
      (%set-any (lambda (ch)
		  (= 66 (char->integer ch)))
		(%make-set #\A #\B #\C))
    => #t)

  (check
      (%set-any (lambda (ch)
		  (= 100 (char->integer ch)))
		(%make-set #\A #\B #\C))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (%set-fold (lambda (ch knil)
		     (cons (char->integer ch) knil))
		 '()
		 (%make-set #\A #\B #\C))
    => '(67 66 65))

  (check
      (%set-fold (lambda (ch knil)
		     (cons (char->integer ch) knil))
		 '()
		 (%make-set))
    => '())

;;; --------------------------------------------------------------------

  (check
      (%set->list (%make-set))
    => '())

  (check
      (%set->list (%make-set #\A))
    => '(#\A))

  (check
      (%set->list (%make-set #\A #\B #\C #\D))
    => '(#\A #\B #\C #\D))

  )


;;;; done

(check-report)

;;; end of file
