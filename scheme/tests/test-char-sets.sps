;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: test for char-set library
;;;Date: Thu Jan 22, 2009
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


;;;; setup

(import (nausicaa)
  (checks)
  (char-sets)
  (char-sets blocks)
  (lists))

(check-set-mode! 'report-failed)
(display "*** testing char-sets\n")

(define (domain=? actual-result expected-result)
  (equal? (char-set-domain-ref actual-result) expected-result))

(define (char-prev ch)
  (integer->char (- (char->integer ch) 1)))

(define (char-next ch)
  (integer->char (+ 1 (char->integer ch))))


(parameterise ((check-test-name	'constructor))

  (check
      (char-set)
    (=> domain=?) '())

  (check
      (char-set #\A)
    (=> domain=?) '((#\A . #\A)))

  (check (char-set #\A #\A) (=> domain=?) '((#\A . #\A)))
  (check (char-set #\A #\A #\A) (=> domain=?) '((#\A . #\A)))
  (check (char-set #\A #\B #\A) (=> domain=?) '((#\A . #\B)))
  (check (char-set #\A #\A #\B #\A) (=> domain=?) '((#\A . #\B)))

  (check (char-set #\A #\B) (=> domain=?) '((#\A . #\B)))
  (check (char-set #\B #\A) (=> domain=?) '((#\A . #\B)))

  (check (char-set #\A #\B #\C) (=> domain=?) '((#\A . #\C)))
  (check (char-set #\B #\A #\C) (=> domain=?) '((#\A . #\C)))
  (check (char-set #\C #\A #\B) (=> domain=?) '((#\A . #\C)))
  (check (char-set #\B #\C #\A) (=> domain=?) '((#\A . #\C)))

  (check (char-set #\A #\C) (=> domain=?) '((#\A . #\A) (#\C . #\C)))
  (check (char-set #\C #\A) (=> domain=?) '((#\A . #\A) (#\C . #\C)))

;;; --------------------------------------------------------------------

  (check (char-set '(#\A . #\B)) (=> domain=?) '((#\A . #\B)))

  ;; equal
  (check (char-set '(#\B . #\C) '(#\B . #\C)) (=> domain=?) '((#\B . #\C)))

  ;; overlapping
  (check (char-set '(#\A . #\B) '(#\B . #\C)) (=> domain=?) '((#\A . #\C)))
  (check (char-set '(#\B . #\C) '(#\A . #\B)) (=> domain=?) '((#\A . #\C)))

  ;; contiguous
  (check (char-set '(#\A . #\B) '(#\C . #\D)) (=> domain=?) '((#\A . #\D)))
  (check (char-set '(#\C . #\D) '(#\A . #\B)) (=> domain=?) '((#\A . #\D)))

  ;; included
  (check (char-set '(#\A . #\D) '(#\B . #\C)) (=> domain=?) '((#\A . #\D)))
  (check (char-set '(#\B . #\C) '(#\A . #\D)) (=> domain=?) '((#\A . #\D)))

  ;; distanced
  (check (char-set '(#\A . #\B) '(#\D . #\E)) (=> domain=?) '((#\A . #\B) (#\D . #\E)))
  (check (char-set '(#\D . #\E) '(#\A . #\B)) (=> domain=?) '((#\A . #\B) (#\D . #\E)))

  (check (char-set #\A #\D '(#\B . #\C)) (=> domain=?) '((#\A . #\D)))
  (check (char-set #\A '(#\B . #\C) #\D) (=> domain=?) '((#\A . #\D)))
  (check (char-set '(#\B . #\C) #\A #\D) (=> domain=?) '((#\A . #\D)))
  (check (char-set '(#\B . #\C) #\D #\A) (=> domain=?) '((#\A . #\D)))

  )


(parameterise ((check-test-name	'predicate))

  (check
      (char-set? (char-set))
    => #t)

  (check
      (char-set? (char-set '(#\A . #\C)))
    => #t)

  (check
      (char-set? 123)
    => #f)

  )


(parameterise ((check-test-name	'inspection))

  (check
      (char-set-empty? (char-set))
    => #t)

  (check
      (char-set-empty? (char-set #\A))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (char-set-size (char-set))
    => 0)

  (check
      (char-set-size (char-set #\A))
    => 1)

  (check
      (char-set-size (char-set #\A #\C))
    => 2)

  (check
      (char-set-size (char-set '(#\A . #\D) '(#\F . #\H)))
    => (+ 4 3))

;;; --------------------------------------------------------------------

  (check
      (char-set-contains? (char-set '(#\A . #\F)) #\C)
    => #t)

  (check
      (char-set-contains? (char-set '(#\A . #\F)) #\M)
    => #f)

;;; --------------------------------------------------------------------

  (check
      ;; equal
      (char-set-subset? (char-set '(#\A . #\F))
			(char-set '(#\A . #\F)))
    => #t)

  (check
      ;; included
      (char-set-subset? (char-set '(#\A . #\M))
			(char-set '(#\B . #\F)))
    => #t)

  (check
      ;;                              included     included
      (char-set-subset? (char-set '(#\A . #\M) '(#\P . #\Z))
			(char-set '(#\B . #\F) '(#\S . #\X)))
    => #t)

  (check
      ;;                              included     overlapping
      (char-set-subset? (char-set '(#\A . #\M) '(#\P . #\X))
			(char-set '(#\B . #\F) '(#\S . #\Z)))
    => #f)

  (check
      ;;                              overlapping
      (char-set-subset? (char-set '(#\A . #\D) '(#\P . #\X))
			(char-set '(#\B . #\F) '(#\S . #\Z)))
    => #f)

  (check
      ;;                                           included     included
      (char-set-subset? (char-set '(#\0 . #\6) '(#\A . #\D) '(#\P . #\Z))
			(char-set              '(#\B . #\F) '(#\S . #\X)))
    => #f)

  (check
      ;;                                           included        included
      (char-set-subset? (char-set '(#\0 . #\6) '(#\A . #\G) #\M '(#\P . #\Z))
			(char-set              '(#\B . #\F)     '(#\S . #\X)))
    => #t)

  (check
      (char-set-subset? (char-set '(#\0 . #\6) '(#\A . #\D)     '(#\P . #\Z))
			(char-set              '(#\B . #\F) #\M '(#\S . #\X)))
    => #f)

  )


(parameterise ((check-test-name	'comparison))

  (check (char-set=? (char-set) (char-set #\A)) => #f)
  (check (char-set=? (char-set #\A) (char-set)) => #f)

  (check
      (char-set=? (char-set #\A)
		  (char-set #\A))
    => #t)

  (check
      (char-set=? (char-set #\A)
		  (char-set #\B))
    => #f)

  (check
      (char-set=? (char-set '(#\A . #\G))
		  (char-set '(#\A . #\G)))
    => #t)

  (check (char-set=? (char-set '(#\A . #\G)) (char-set '(#\D . #\G))) => #f)
  (check (char-set=? (char-set '(#\D . #\G)) (char-set '(#\A . #\G))) => #f)

  (check (char-set=? (char-set '(#\A . #\D)) (char-set '(#\F . #\M))) => #f)
  (check (char-set=? (char-set '(#\F . #\M)) (char-set '(#\A . #\D))) => #f)

;;; --------------------------------------------------------------------

  (check (char-set<? (char-set)     (char-set #\B)) => #f)
  (check (char-set<? (char-set #\A) (char-set))     => #f)

  (check (char-set<? (char-set #\A) (char-set #\B)) => #t)
  (check (char-set<? (char-set #\A) (char-set #\A)) => #f)
  (check (char-set<? (char-set #\B) (char-set #\A)) => #f)

  )


(parameterise ((check-test-name	'intersection))

  (check
      ;; empty
      (char-set-intersection (char-set)
			     (char-set '(#\A . #\H)))
    (=> char-set=?) (char-set))

  (check
      ;; empty
      (char-set-intersection (char-set '(#\A . #\H))
			     (char-set))
    (=> char-set=?) (char-set))

  (check
      ;; equal
      (char-set-intersection (char-set '(#\A . #\H))
			     (char-set '(#\A . #\H)))
    (=> char-set=?) (char-set '(#\A . #\H)))

  (check
      ;; disjoint
      (char-set-intersection (char-set '(#\A . #\C))
			     (char-set '(#\E . #\H)))
    (=> char-set=?) (char-set))

  (check
      ;; disjoint
      (char-set-intersection (char-set '(#\E . #\H))
			     (char-set '(#\A . #\C)))
    (=> char-set=?) (char-set))

  (check
      ;; contiguous
      (char-set-intersection (char-set '(#\A . #\D))
			     (char-set '(#\E . #\H)))
    (=> char-set=?) (char-set))

  (check
      ;; contiguous
      (char-set-intersection (char-set '(#\E . #\H))
			     (char-set '(#\A . #\D)))
    (=> char-set=?) (char-set))

  (check
      ;; inclusion
      (char-set-intersection (char-set '(#\C . #\F))
			     (char-set '(#\A . #\H)))
    (=> char-set=?) (char-set '(#\C . #\F)))

  (check
      ;; inclusion
      (char-set-intersection (char-set '(#\A . #\H))
			     (char-set '(#\C . #\F)))
    (=> char-set=?) (char-set '(#\C . #\F)))

  (check
      (char-set-intersection (char-set '(#\A . #\D) '(#\H . #\M) '(#\O . #\P))
			     (char-set '(#\C . #\F) '(#\I . #\L) '(#\N . #\Q)))
    (=> char-set=?) (char-set '(#\C . #\D)
			      '(#\I . #\L)
			      '(#\O . #\P)))

  (check
      ;; disjoint tail
      (char-set-intersection (char-set '(#\A . #\D) #\F)
			     (char-set '(#\C . #\E) #\F #\M #\P #\R))
    (=> char-set=?) (char-set '(#\C . #\D) #\F))

  (check
      ;; disjoint tail
      (char-set-intersection (char-set '(#\A . #\D) #\F #\M #\P #\R)
			     (char-set '(#\C . #\E) #\F))
    (=> char-set=?) (char-set '(#\C . #\D) #\F))

  (check
      ;; contiguous tail
      (char-set-intersection (char-set '(#\A . #\D) #\F #\G #\H #\I)
			     (char-set '(#\C . #\E) #\F))
    (=> char-set=?) (char-set '(#\C . #\D) #\F))

  (check
      ;; contiguous tail
      (char-set-intersection (char-set '(#\A . #\D) #\F)
			     (char-set '(#\C . #\E) #\F #\G #\H #\I))
    (=> char-set=?) (char-set '(#\C . #\D) #\F))

  (check
      ;; overlapping tail
      (char-set-intersection (char-set '(#\A . #\D) #\F)
			     (char-set '(#\C . #\E) #\F '(#\H . #\N) '(#\L . #\P)))
    (=> char-set=?) (char-set '(#\C . #\D) #\F))

  (check
      ;; overlapping tail
      (char-set-intersection (char-set '(#\A . #\D) #\F '(#\H . #\N) '(#\L . #\P))
			     (char-set '(#\C . #\E) #\F))
    (=> char-set=?) (char-set '(#\C . #\D) #\F))

  )


(parameterise ((check-test-name	'union))

  (check
      ;; empty
      (char-set-union (char-set)
		      (char-set '(#\A . #\H)))
    (=> char-set=?) (char-set '(#\A . #\H)))

  (check
      ;; empty
      (char-set-union (char-set '(#\A . #\H))
		      (char-set))
    (=> char-set=?) (char-set '(#\A . #\H)))

  (check
      ;; equal
      (char-set-union (char-set '(#\A . #\H))
		      (char-set '(#\A . #\H)))
    (=> char-set=?) (char-set '(#\A . #\H)))

  (check
      ;; disjoint
      (char-set-union (char-set '(#\A . #\C))
		      (char-set '(#\E . #\H)))
    (=> char-set=?) (char-set '(#\A . #\C) '(#\E . #\H)))

  (check
      ;; disjoint
      (char-set-union (char-set '(#\E . #\H))
		      (char-set '(#\A . #\C)))
    (=> char-set=?) (char-set '(#\A . #\C) '(#\E . #\H)))

  (check
      ;; contiguous
      (char-set-union (char-set '(#\A . #\D))
		      (char-set '(#\E . #\H)))
    (=> char-set=?) (char-set '(#\A . #\H)))

  (check
      ;; contiguous
      (char-set-union (char-set '(#\E . #\H))
		      (char-set '(#\A . #\D)))
    (=> char-set=?) (char-set '(#\A . #\H)))

  (check
      ;; inclusion
      (char-set-union (char-set '(#\C . #\F))
		      (char-set '(#\A . #\H)))
    (=> char-set=?) (char-set '(#\A . #\H)))

  (check
      ;; inclusion
      (char-set-union (char-set '(#\A . #\H))
		      (char-set '(#\C . #\F)))
    (=> char-set=?) (char-set '(#\A . #\H)))

  (check
      (char-set-union (char-set '(#\A . #\D) '(#\H . #\M) '(#\O . #\P))
		      (char-set '(#\C . #\F) '(#\I . #\L) '(#\N . #\Q)))
    (=> char-set=?) (char-set '(#\A . #\F)
			      '(#\H . #\Q)))

  (check
      ;; disjoint tail
      (char-set-union (char-set '(#\A . #\D) #\F)
		      (char-set '(#\C . #\E) #\F #\M #\P #\R))
    (=> char-set=?) (char-set '(#\A . #\E) #\F #\M #\P #\R))

  (check
      ;; disjoint tail
      (char-set-union (char-set '(#\A . #\D) #\F #\M #\P #\R)
		      (char-set '(#\C . #\E) #\F))
    (=> char-set=?) (char-set '(#\A . #\E) #\F #\M #\P #\R))

  (check
      ;; contiguous tail
      (char-set-union (char-set '(#\A . #\D) #\F #\G #\H #\I)
		      (char-set '(#\C . #\E) #\F))
    (=> char-set=?) (char-set '(#\A . #\E) #\F #\G #\H #\I))

  (check
      ;; contiguous tail
      (char-set-union (char-set '(#\A . #\D) #\F)
		      (char-set '(#\C . #\E) #\F #\G #\H #\I))
    (=> char-set=?) (char-set '(#\A . #\E) #\F #\G #\H #\I))

  (check
      ;; overlapping tail
      (char-set-union (char-set '(#\A . #\D) #\F)
		      (char-set '(#\C . #\E) #\F '(#\H . #\N) '(#\L . #\P)))
    (=> char-set=?) (char-set '(#\A . #\E) #\F '(#\H . #\N) '(#\L . #\P)))

  (check
      ;; overlapping tail
      (char-set-union (char-set '(#\A . #\D) #\F '(#\H . #\N) '(#\L . #\P))
		      (char-set '(#\C . #\E) #\F))
    (=> char-set=?) (char-set '(#\A . #\E) #\F '(#\H . #\N) '(#\L . #\P)))

  )


(parameterise ((check-test-name	'difference))

  (check
      ;; empty
      (char-set-difference (char-set)
			   (char-set '(#\A . #\H)))
    (=> char-set=?) (char-set '(#\A . #\H)))

  (check
      ;; empty
      (char-set-difference (char-set '(#\A . #\H))
			   (char-set))
    (=> char-set=?) (char-set '(#\A . #\H)))

  (check
      ;; equal
      (char-set-difference (char-set '(#\A . #\H))
			   (char-set '(#\A . #\H)))
    (=> char-set=?) (char-set))

  (check
      ;; disjoint
      (char-set-difference (char-set '(#\A . #\C))
			   (char-set '(#\E . #\H)))
    (=> char-set=?) (char-set '(#\A . #\C) '(#\E . #\H)))

  (check
      ;; disjoint
      (char-set-difference (char-set '(#\E . #\H))
			   (char-set '(#\A . #\C)))
    (=> char-set=?) (char-set '(#\A . #\C) '(#\E . #\H)))

  (check
      ;; contiguous
      (char-set-difference (char-set '(#\A . #\D))
			   (char-set '(#\E . #\H)))
    (=> char-set=?) (char-set '(#\A . #\H)))

  (check
      ;; contiguous
      (char-set-difference (char-set '(#\E . #\H))
			   (char-set '(#\A . #\D)))
    (=> char-set=?) (char-set '(#\A . #\H)))

  (check
      ;; inclusion
      (char-set-difference (char-set '(#\C . #\F))
			   (char-set '(#\A . #\H)))
    (=> char-set=?) (char-set '(#\A . #\B)
			      '(#\G . #\H)))

  (check
      ;; inclusion
      (char-set-difference (char-set '(#\A . #\H))
			   (char-set '(#\C . #\F)))
    (=> char-set=?) (char-set '(#\A . #\B)
			      '(#\G . #\H)))

  (check 'this
      (char-set-difference (char-set '(#\A . #\D) '(#\H . #\M) '(#\O . #\P))
			   (char-set '(#\C . #\F) '(#\I . #\L) '(#\N . #\Q)))
    (=> char-set=?) (char-set #\A #\B #\E #\F #\H #\M #\N #\Q))

  (check
      ;; disjoint tail
      (char-set-difference (char-set '(#\A . #\D) #\F)
			   (char-set '(#\C . #\E) #\F #\M #\P #\R))
    (=> char-set=?) (char-set '(#\A . #\B) #\E #\M #\P #\R))

  (check
      ;; disjoint tail
      (char-set-difference (char-set '(#\A . #\D) #\F #\M #\P #\R)
			   (char-set '(#\C . #\E) #\F))
    (=> char-set=?) (char-set '(#\A . #\B) #\E #\M #\P #\R))

  (check
      ;; contiguous tail
      (char-set-difference (char-set '(#\A . #\D) #\F #\G #\H #\I)
			   (char-set '(#\C . #\E) #\F))
    (=> char-set=?) (char-set '(#\A . #\B) #\E #\G #\H #\I))

  (check
      ;; contiguous tail
      (char-set-difference (char-set '(#\A . #\D) #\F)
			   (char-set '(#\C . #\E) #\F #\G #\H #\I))
    (=> char-set=?) (char-set '(#\A . #\B) #\E #\G #\H #\I))

  (check
      ;; overlapping tail
      (char-set-difference (char-set '(#\A . #\D) #\F)
			   (char-set '(#\C . #\E) #\F '(#\H . #\N) '(#\L . #\P)))
    (=> char-set=?) (char-set '(#\A . #\B) #\E '(#\H . #\N) '(#\L . #\P)))

  (check
      ;; overlapping tail
      (char-set-difference (char-set '(#\A . #\D) #\F '(#\H . #\N) '(#\L . #\P))
			   (char-set '(#\C . #\E) #\F))
    (=> char-set=?) (char-set '(#\A . #\B) #\E '(#\H . #\N) '(#\L . #\P)))

  )


(parameterise ((check-test-name	'complement))

  (check
      (char-set-complement (char-set))
    (=> char-set=?)
    char-set:full)

  (check
      (char-set-complement (char-set #\A))
    (=> char-set=?)
    (let* ((ch		#\A)
	   (chup	(char-next ch))
	   (chdn	(char-prev ch)))
      (char-set (cons char-set-lower-bound chdn)
		(cons chup char-set-inner-upper-bound)
		(cons char-set-inner-lower-bound char-set-upper-bound))))

  (check
      (char-set-complement (char-set '(#\A . #\D) '(#\M . #\Z)))
    (=> char-set=?)
    (char-set (cons char-set-lower-bound (char-prev #\A))
	      (cons (char-next #\D) (char-prev #\M))
	      (cons (char-next #\Z) char-set-inner-upper-bound)
	      (cons char-set-inner-lower-bound char-set-upper-bound)))

  (check
      (char-set-complement (char-set '(#\A . #\D) '(#\M . #\Z) '(#\4 . #\9)))
    (=> char-set=?)
    (char-set (cons char-set-lower-bound (char-prev #\4))
	      (cons (char-next #\9) (char-prev #\A))
	      (cons (char-next #\D) (char-prev #\M))
	      (cons (char-next #\Z) char-set-inner-upper-bound)
	      (cons char-set-inner-lower-bound char-set-upper-bound)))

  )


(parameterise ((check-test-name	'list-operations))

  (check
      (cadr (with-result
	     (char-set-for-each (lambda (ch)
				  (add-result ch))
				(char-set #\A #\B #\C))
	     #t))
    => '(#\A #\B #\C))

;;; --------------------------------------------------------------------

  (check
      (char-set-every (lambda (ch)
			(<= 65 (char->integer ch)))
		      (char-set #\A #\B #\C))
    => #t)

  (check
      (char-set-every (lambda (ch)
			(<= 67 (char->integer ch)))
		      (char-set #\A #\B #\C))
    => #f)


;;; --------------------------------------------------------------------

  (check
      (char-set-any (lambda (ch)
		      (= 66 (char->integer ch)))
		    (char-set #\A #\B #\C))
    => #t)

  (check
      (char-set-any (lambda (ch)
		      (= 100 (char->integer ch)))
		    (char-set #\A #\B #\C))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (char-set-fold (lambda (ch knil)
		       (cons (char->integer ch) knil))
		     '()
		     (char-set #\A #\B #\C))
    => '(67 66 65))

  (check
      (char-set-fold (lambda (ch knil)
		       (cons (char->integer ch) knil))
		     '()
		     (char-set))
    => '())

;;; --------------------------------------------------------------------

  (check
      (char-set->list (char-set))
    => '())

  (check
      (char-set->list (char-set #\A))
    => '(#\A))

  (check
      (char-set->list (char-set #\A #\B #\C #\D))
    => '(#\A #\B #\C #\D))

  )


(parameterise ((check-test-name	'string-operations))

  (check
      (string->char-set "")
    (=> char-set=?)
    (char-set))

  (check
      (string->char-set "ABCD")
    (=> char-set=?)
    (char-set #\A #\B #\C #\D))

  (check
      (string->char-set "ABCDBBCC")
    (=> char-set=?)
    (char-set #\A #\B #\C #\D))

  )


;;;; done

(check-report)

;;; end of file
