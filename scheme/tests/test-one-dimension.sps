;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: test for one-dimension library
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
  (one-dimension))

(check-set-mode! 'report-failed)
(display "*** testing one-dimension low\n")

(define inclusive-lower-bound (integer->char 0))
(define exclusive-upper-bound (integer->char 1114111))

(define (char-next item)
  (integer->char (+ 1 (char->integer item))))

(define (char- past start)
  (- (char->integer past)
     (char->integer start)))

(define (char-copy ch)
  ch)


;;;; char range wrappers

(define (make-range a b)
  (%make-range a b char? char<?))

(define (range-copy a)
  (%range-copy a char-copy))

(define (range? a)
  (%range? a char? char<?))

(define (range-contains? range obj)
  (%range-contains? range obj char<? char<=?))

(define (range-length range)
  (%range-length range char-))

(define (range=? range-a range-b)
  (%range=? range-a range-b char=?))

(define (range<? range-a range-b)
  (%range<? range-a range-b char<=?))

(define (range<=? range-a range-b)
  (%range<=? range-a range-b char<=?))

(define (range-contiguous? range-a range-b)
  (%range-contiguous? range-a range-b char=?))

(define (range-subset? range-a range-b)
  (%range-subset? range-a range-b char<=?))

(define (range-strict-subset? range-a range-b)
  (%range-strict-subset? range-a range-b char<? char<=?))

(define (range-start<? range-a range-b)
  (%range-start<? range-a range-b char<?))

(define (range-start<=? range-a range-b)
  (%range-start<=? range-a range-b char<=?))

(define (range-overlapping? range-a range-b)
  (%range-overlapping? range-a range-b char<? char<=?))

(define (range-concatenate range-a range-b)
  (%range-concatenate range-a range-b char<?))

(define (range-intersection range-a range-b)
  (%range-intersection range-a range-b char<? char<=?))

(define (range-union range-a range-b)
  (%range-union range-a range-b char=? char<? char<=?))

(define (range-difference range-a range-b)
  (%range-difference range-a range-b char=? char<?))

(define (range-for-each proc range)
  (%range-for-each proc range char<=? char-next))

(define (range-every proc range)
  (%range-every proc range char<? char-next))

(define (range-any proc range)
  (%range-any proc range char<? char-next))

(define (range-fold kons knil range)
  (%range-fold kons knil range char<? char-next))

(define (range->list range)
  (%range->list range char<? char-next))


;;;; char domain wrappers

(define (make-domain . args)
  (apply %make-domain char? char=? char<? char<=? char-next args))

(define (domain-copy domain)
  (%domain-copy domain char-copy))

(define (domain-add-item domain obj)
  (%domain-add-item domain obj char=? char<? char<=? char-next))

(define (domain-add-range domain new-range)
  (%domain-add-range domain new-range char=? char<? char<=?))

(define (domain? domain)
  (%domain? domain char? char<? char<=? inclusive-lower-bound exclusive-upper-bound))

(define (domain-size domain)
  (%domain-size domain char-))

(define domain-empty? %domain-empty?)

(define (domain-contains? domain obj)
  (%domain-contains? domain obj char<? char<=?))

(define (domain=? domain-a domain-b)
  (%domain=? domain-a domain-b char=?))

(define (domain<? domain-a domain-b)
  (%domain<? domain-a domain-b char<=?))

(define (domain-subset? domain-a domain-b)
  (%domain-subset? domain-a domain-b char<=?))

(define (domain-strict-subset? domain-a domain-b)
  (%domain-strict-subset? domain-a domain-b char<? char<=?))

(define (domain-intersection domain-a domain-b)
  (%domain-intersection domain-a domain-b char=? char<? char<=?))

(define (domain-union domain-a domain-b)
  (%domain-union domain-a domain-b char=? char<? char<=?))

(define (domain-difference domain-a domain-b)
  (%domain-difference domain-a domain-b char=? char<? char<=?))

(define (domain-complement domain)
  (%domain-complement domain char=? char<? char<=?
		      inclusive-lower-bound exclusive-upper-bound))

(define (domain-for-each proc domain)
  (%domain-for-each proc domain char<=? char-next))

(define (domain-every proc domain)
  (%domain-every proc domain char<? char-next))

(define (domain-any proc domain)
  (%domain-any proc domain char<? char-next))

(define (domain-fold kons knil domain)
  (%domain-fold kons knil domain char<? char-next))

(define (domain->list domain)
  (%domain->list domain char<? char-next))

(define (string->domain str)
  (apply make-domain (string->list str)))


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
      (make-range #\A #\D)
    => '(#\A . #\D))

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(make-range #\A #\A))
    => #t)

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(make-range 123 #\B))
    => #t)

  )


(parameterise ((check-test-name	'range-inspection))

  (check
      (range-length (make-range #\A #\D))
    => 3)

  (check
      (range-length (make-range #\A #\B))
    => 1)

;;; --------------------------------------------------------------------

  (check
      (range-contains? (make-range #\A #\M) #\A)
    => #t)

  (check
      (range-contains? (make-range #\A #\M) #\M)
    => #f)

  (check
      (range-contains? (make-range #\A #\M) #\2)
    => #f)

  (check
      (range-contains? (make-range #\A #\M) #\P)
    => #f)

  (check
      (range-contains? (make-range #\A #\M) #\G)
    => #t)

;;; --------------------------------------------------------------------

  (check
      ;; equal
      (range-subset? (make-range #\A #\M)
		     (make-range #\A #\M))
    => #t)

  (check
      ;; subset
      (range-subset? (make-range #\A #\M)
		     (make-range #\C #\G))
    => #t)

  (check
      ;; overlapping
      (range-subset? (make-range #\A #\M)
		     (make-range #\G #\P))
    => #f)

  (check
      ;; overlapping
      (range-subset? (make-range #\G #\P)
		     (make-range #\A #\M))
    => #f)

  (check
      ;; disjoint
      (range-subset? (make-range #\A #\M)
		     (make-range #\P #\Z))
    => #f)

  (check
      ;; disjoint
      (range-subset? (make-range #\P #\Z)
		     (make-range #\A #\M))
    => #f)

  (check
      (range-subset? (make-range #\C #\G)
		     (make-range #\A #\M))
    => #f)

;;; --------------------------------------------------------------------

  (check
      ;; equal
      (range-strict-subset? (make-range #\A #\M)
			    (make-range #\A #\M))
    => #f)

  (check
      ;; same start
      (range-strict-subset? (make-range #\A #\M)
			    (make-range #\A #\G))
    => #t)

  (check
      ;; same past
      (range-strict-subset? (make-range #\A #\M)
			    (make-range #\G #\M))
    => #t)

  (check
      ;; subset
      (range-strict-subset? (make-range #\A #\M)
			    (make-range #\C #\G))
    => #t)

  (check
      ;; overlapping
      (range-strict-subset? (make-range #\A #\M)
			    (make-range #\G #\P))
    => #f)

  (check
      ;; overlapping
      (range-strict-subset? (make-range #\G #\P)
			    (make-range #\A #\M))
    => #f)

  (check
      ;; disjoint
      (range-strict-subset? (make-range #\A #\M)
			    (make-range #\P #\Z))
    => #f)

  (check
      ;; disjoint
      (range-strict-subset? (make-range #\P #\Z)
			    (make-range #\A #\M))
    => #f)

  (check
      (range-strict-subset? (make-range #\C #\G)
			    (make-range #\A #\M))
    => #f)

  )


(parameterise ((check-test-name	'range-comparison))

  (check
      (range=? (make-range #\A #\M)
	       (make-range #\A #\M))
    => #t)

  (check
      (range=? (make-range #\9 #\M)
	       (make-range #\A #\M))
    => #f)

  (check
      (range=? (make-range #\A #\M)
	       (make-range #\A #\Q))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (range<? (make-range #\A #\M)
	       (make-range #\A #\M))
    => #f)

  (check
      (range<? (make-range #\A #\M)
	       (make-range #\P #\Z))
    => #t)

  (check
      (range<? (make-range #\P #\Z)
	       (make-range #\A #\M))
    => #f)

  (check
      (range<? (make-range #\A #\M)
	       (make-range #\G #\P))
    => #f)

  (check
      (range<? (make-range #\G #\P)
	       (make-range #\A #\M))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (range-start<? (make-range #\A #\M)
		     (make-range #\G #\P))
    => #t)

  (check
      (range-start<? (make-range #\G #\M)
		     (make-range #\G #\P))
    => #f)

  (check
      (range-start<? (make-range #\G #\M)
		     (make-range #\A #\P))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (range-start<=? (make-range #\A #\M)
		      (make-range #\G #\P))
    => #t)

  (check
      (range-start<=? (make-range #\G #\M)
		      (make-range #\G #\P))
    => #t)

  (check
      (range-start<=? (make-range #\G #\M)
		      (make-range #\A #\P))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (range-contiguous? (make-range #\A #\M)
			 (make-range #\M #\P))
    => #t)

  (check
      (range-contiguous? (make-range #\A #\P)
			 (make-range #\M #\Z))
    => #f)

  (check
      (range-contiguous? (make-range #\M #\Z)
			 (make-range #\A #\P))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (range-overlapping? (make-range #\A #\M)
			  (make-range #\M #\P))
    => #f)

  (check
      (range-overlapping? (make-range #\A #\P)
			  (make-range #\M #\Z))
    => #t)

  (check
      (range-overlapping? (make-range #\M #\Z)
			  (make-range #\A #\P))
    => #t)

  )


(parameterise ((check-test-name	'range-set-operation))

  (check
      (range-concatenate (make-range #\A #\M)
			 (make-range #\A #\M))
    (=> range=?)
    (make-range #\A #\M))

  (check
      (range-concatenate (make-range #\A #\M)
			 (make-range #\P #\Z))
    (=> range=?)
    (make-range #\A #\Z))

;;; --------------------------------------------------------------------

  (check
      ;; equal
      (let-values (((head tail) (range-union (make-range #\A #\M)
					     (make-range #\A #\M))))
	(list head tail))
    => (list #f (make-range #\A #\M)))

  (check
      ;; overlapping
      (let-values (((head tail) (range-union (make-range #\A #\P)
					     (make-range #\M #\Z))))
	(list head tail))
    => (list #f (make-range #\A #\Z)))

  (check
      ;; overlapping
      (let-values (((head tail) (range-union (make-range #\M #\Z)
					     (make-range #\A #\P))))
	(list head tail))
    => (list #f (make-range #\A #\Z)))

  (check
      ;; contiguous
      (let-values (((head tail) (range-union (make-range #\A #\M)
					     (make-range #\M #\Z))))
	(list head tail))
    => (list #f (make-range #\A #\Z)))

  (check
      ;; contiguous
      (let-values (((head tail) (range-union (make-range #\M #\Z)
					     (make-range #\A #\M))))
	(list head tail))
    => (list #f (make-range #\A #\Z)))

  (check
      ;; dijoint
      (let-values (((head tail) (range-union (make-range #\A #\M)
					     (make-range #\P #\Z))))
	(list head tail))
    => '((#\A . #\M) (#\P . #\Z)))

  (check
      ;; dijoint
      (let-values (((head tail) (range-union (make-range #\P #\Z)
					     (make-range #\A #\M))))
	(list head tail))
    => '((#\A . #\M) (#\P . #\Z)))

;;; --------------------------------------------------------------------

  (check
      ;; equal ranges
      (let-values (((head tail) (range-difference (make-range #\A #\M)
						  (make-range #\A #\M))))
	(list head tail))
    => '(#f #f))

  (check
      ;; overlapping ranges
      (let-values (((head tail) (range-difference (make-range #\A #\P)
						  (make-range #\M #\Z))))
	(list head tail))
    => '((#\A . #\M) (#\P . #\Z)))

  (check
      ;; overlapping ranges
      (let-values (((head tail) (range-difference (make-range #\M #\Z)
						  (make-range #\A #\P))))
	(list head tail))
    => '((#\A . #\M) (#\P . #\Z)))

  (check
      ;; non-overlapping ranges
      (let-values (((head tail) (range-difference (make-range #\A #\M)
						  (make-range #\P #\Z))))
	(list head tail))
    => '((#\A . #\M) (#\P . #\Z)))

  (check
      ;; non-overlapping ranges
      (let-values (((head tail) (range-difference (make-range #\P #\Z)
						  (make-range #\A #\M))))
	(list head tail))
    => '((#\A . #\M) (#\P . #\Z)))

  (check
      ;; contiguous ranges
      (let-values (((head tail) (range-difference (make-range #\A #\M)
						  (make-range #\M #\Z))))
	(list head tail))
    => (list #f (make-range #\A #\Z)))

  (check
      ;; contiguous ranges
      (let-values (((head tail) (range-difference (make-range #\M #\Z)
						  (make-range #\A #\M))))
	(list head tail))
    => (list #f (make-range #\A #\Z)))

  )


(parameterise ((check-test-name	'range-list-operation))

  (check
      (with-result
       (range-for-each add-result (make-range #\A #\F))
       'here)
    => '(here (#\A #\B #\C #\D #\E)))

;;; --------------------------------------------------------------------

  (check
      (range-every (lambda (ch)
		     (and (char<=? #\A ch) (char<? ch #\F)))
		   (make-range #\A #\F))
    => #t)

  (check
      (range-every (lambda (ch)
		     (and (char<=? #\A ch) (char<? ch #\D)))
		   (make-range #\A #\F))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (range-any (lambda (ch) (char=? #\D ch))
		 (make-range #\A #\F))
    => #t)

  (check
      (range-any (lambda (ch) (char=? #\4 ch))
		 (make-range #\A #\F))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (range-fold (lambda (ch knil)
		    (cons ch knil))
		  '()
		  (make-range #\A #\F))
    => '(#\E #\D #\C #\B #\A))

;;; --------------------------------------------------------------------

  (check
      (range->list (make-range #\A #\F))
    => (reverse '(#\A #\B #\C #\D #\E)))

  (check
      (range->list (make-range #\A #\B))
    => '(#\A))

  )


(parameterise ((check-test-name	'domain-predicate))

  (check
      (domain? '())
    => #t)

  (check
      (domain? '((#\A . #\C)))
    => #t)

  (check
      (domain? '((#\A . #\M)
		 (#\P . #\R)))
    => #t)

  (check
      (domain? '((#\A . #\M)
		 (#\M . #\R)))
    => #t)

  (check
      (domain? '((#\A . #\M)
		 (#\P . #\R)
		 (#\X . #\Z)))
    => #t)

  (check
      (domain? `((#\A . #\M)
		 (#\P . #\R)
		 (#\X . ,exclusive-upper-bound)))
    => #f)

  )


(parameterise ((check-test-name	'domain-constructor))

  ;;Tests   for   MAKE-DOMAIN   tests  automatically   DOMAIN-ADD-CHAR   and
  ;;DOMAIN-ADD-RANGE.

  (check
      (make-domain)
    => '())

  (check
      (make-domain #\A)
    => '((#\A . #\B)))

  (check (make-domain #\A #\A) => '((#\A . #\B)))
  (check (make-domain #\A #\A #\A) => '((#\A . #\B)))
  (check (make-domain #\A #\B #\A) => '((#\A . #\C)))
  (check (make-domain #\A #\A #\B #\A) => '((#\A . #\C)))

  (check (make-domain #\A #\B) => '((#\A . #\C)))
  (check (make-domain #\B #\A) => '((#\A . #\C)))

  (check (make-domain #\A #\B #\C) => '((#\A . #\D)))
  (check (make-domain #\B #\A #\C) => '((#\A . #\D)))
  (check (make-domain #\C #\A #\B) => '((#\A . #\D)))
  (check (make-domain #\B #\C #\A) => '((#\A . #\D)))

  (check (make-domain #\A #\C) => '((#\A . #\B) (#\C . #\D)))
  (check (make-domain #\C #\A) => '((#\A . #\B) (#\C . #\D)))

;;; --------------------------------------------------------------------

  (check (make-domain '(#\A . #\B)) => '((#\A . #\C)))

  ;; equal
  (check (make-domain '(#\B . #\C) '(#\B . #\C)) => '((#\B . #\D)))

  ;; overlapping
  (check (make-domain '(#\A . #\B) '(#\B . #\C)) => '((#\A . #\D)))
  (check (make-domain '(#\B . #\C) '(#\A . #\B)) => '((#\A . #\D)))

  ;; contiguous
  (check (make-domain '(#\A . #\B) '(#\C . #\D)) => '((#\A . #\E)))
  (check (make-domain '(#\C . #\D) '(#\A . #\B)) => '((#\A . #\E)))

  ;; subset
  (check (make-domain '(#\A . #\D) '(#\B . #\C)) => '((#\A . #\E)))
  (check (make-domain '(#\B . #\C) '(#\A . #\D)) => '((#\A . #\E)))

  ;; distanced
  (check (make-domain '(#\A . #\B) '(#\D . #\E)) => '((#\A . #\C) (#\D . #\F)))
  (check (make-domain '(#\D . #\E) '(#\A . #\B)) => '((#\A . #\C) (#\D . #\F)))

  (check (make-domain #\A #\D '(#\B . #\C)) => '((#\A . #\E)))
  (check (make-domain #\A '(#\B . #\C) #\D) => '((#\A . #\E)))
  (check (make-domain '(#\B . #\C) #\A #\D) => '((#\A . #\E)))
  (check (make-domain '(#\B . #\C) #\D #\A) => '((#\A . #\E)))

  )


(parameterise ((check-test-name	'domain-inspection))

  (check
      (domain-empty? (make-domain))
    => #t)

  (check
      (domain-empty? (make-domain #\A))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (domain-size (make-domain))
    => 0)

  (check
      (domain-size (make-domain #\A))
    => 1)

  (check
      (domain-size (make-domain #\A #\C))
    => 2)

  (check
      (domain-size (make-domain '(#\A . #\D) '(#\F . #\H)))
    => (+ 4 3))

;;; --------------------------------------------------------------------

  (check
      (domain-contains? (make-domain '(#\A . #\F)) #\C)
    => #t)

  (check
      (domain-contains? (make-domain '(#\A . #\F)) #\M)
    => #f)

;;; --------------------------------------------------------------------

  (check
      ;; equal
      (domain-subset? (make-domain '(#\A . #\F))
		      (make-domain '(#\A . #\F)))
    => #t)

  (check
      ;; subset
      (domain-subset? (make-domain '(#\A . #\M))
		      (make-domain '(#\B . #\F)))
    => #t)

  (check
      ;;                              subset     subset
      (domain-subset? (make-domain '(#\A . #\M) '(#\P . #\Z))
		      (make-domain '(#\B . #\F) '(#\S . #\X)))
    => #t)

  (check
      ;;                              subset     overlapping
      (domain-subset? (make-domain '(#\A . #\M) '(#\P . #\X))
		      (make-domain '(#\B . #\F) '(#\S . #\Z)))
    => #f)

  (check
      ;;                              overlapping
      (domain-subset? (make-domain '(#\A . #\D) '(#\P . #\X))
		      (make-domain '(#\B . #\F) '(#\S . #\Z)))
    => #f)

  (check
      ;;                                           subset     subset
      (domain-subset? (make-domain '(#\0 . #\6) '(#\A . #\D) '(#\P . #\Z))
		      (make-domain              '(#\B . #\F) '(#\S . #\X)))
    => #f)

  (check
      ;;                                           subset        subset
      (domain-subset? (make-domain '(#\0 . #\6) '(#\A . #\G) #\M '(#\P . #\Z))
		      (make-domain              '(#\B . #\F)     '(#\S . #\X)))
    => #t)

  (check
      (domain-subset? (make-domain '(#\0 . #\6) '(#\A . #\D)     '(#\P . #\Z))
		      (make-domain              '(#\B . #\F) #\M '(#\S . #\X)))
    => #f)

;;; --------------------------------------------------------------------

  (check
      ;; equal
      (domain-strict-subset? (make-domain '(#\A . #\F))
			     (make-domain '(#\A . #\F)))
    => #f)

  (check
      ;; same start
      (domain-strict-subset? (make-domain '(#\A . #\M))
			     (make-domain '(#\A . #\F)))
    => #t)

  (check
      ;; same past
      (domain-strict-subset? (make-domain '(#\A . #\M))
			     (make-domain '(#\F . #\M)))
    => #t)

  (check
      ;; subset
      (domain-strict-subset? (make-domain '(#\A . #\M))
			     (make-domain '(#\B . #\F)))
    => #t)

  (check
      ;;                                        equal
      (domain-strict-subset? (make-domain #\0 '(#\A . #\M))
			     (make-domain     '(#\A . #\M)))
    => #t)

  (check
      ;;                                    equal
      (domain-strict-subset? (make-domain '(#\A . #\M) #\0)
			     (make-domain '(#\A . #\M)))
    => #t)

  (check
      (domain-strict-subset? (make-domain #\0 '(#\B . #\M))
			     (make-domain     '(#\A . #\M)))
    => #f)

  (check
      (domain-strict-subset? (make-domain '(#\A . #\F) #\0)
			     (make-domain '(#\A . #\M)))
    => #f)

  (check
      ;;                                    subset        subset
      (domain-strict-subset? (make-domain '(#\A . #\M) '(#\P . #\Z))
			     (make-domain '(#\B . #\F) '(#\S . #\X)))
    => #t)

  (check
      ;;                                    subset     overlapping
      (domain-strict-subset? (make-domain '(#\A . #\M) '(#\P . #\X))
			     (make-domain '(#\B . #\F) '(#\S . #\Z)))
    => #f)

  (check
      ;;                                   overlapping
      (domain-strict-subset? (make-domain '(#\A . #\D) '(#\P . #\X))
			     (make-domain '(#\B . #\F) '(#\S . #\Z)))
    => #f)

  (check
      ;;                                                 subset       subset
      (domain-strict-subset? (make-domain '(#\0 . #\6) '(#\A . #\D) '(#\P . #\Z))
			     (make-domain              '(#\B . #\F) '(#\S . #\X)))
    => #f)

  (check
      ;;                                                 subset           subset
      (domain-strict-subset? (make-domain '(#\0 . #\6) '(#\A . #\G) #\M '(#\P . #\Z))
			     (make-domain              '(#\B . #\F)     '(#\S . #\X)))
    => #t)

  (check
      ;;                                                 equal            equal
      (domain-strict-subset? (make-domain '(#\0 . #\6) '(#\A . #\G) #\M '(#\P . #\Z))
			     (make-domain              '(#\A . #\G)     '(#\P . #\Z)))
    => #t)

  (check
      (domain-strict-subset? (make-domain '(#\0 . #\6) '(#\A . #\D)     '(#\P . #\Z))
			     (make-domain              '(#\B . #\F) #\M '(#\S . #\X)))
    => #f)

  )


(parameterise ((check-test-name	'domain-comparison))

  (check (domain=? (make-domain) (make-domain #\A)) => #f)
  (check (domain=? (make-domain #\A) (make-domain)) => #f)

  (check
      (domain=? (make-domain #\A)
		(make-domain #\A))
    => #t)

  (check
      (domain=? (make-domain #\A)
		(make-domain #\B))
    => #f)

  (check
      (domain=? (make-domain '(#\A . #\G))
		(make-domain '(#\A . #\G)))
    => #t)

  (check (domain=? (make-domain '(#\A . #\G)) (make-domain '(#\D . #\G))) => #f)
  (check (domain=? (make-domain '(#\D . #\G)) (make-domain '(#\A . #\G))) => #f)

  (check (domain=? (make-domain '(#\A . #\D)) (make-domain '(#\F . #\M))) => #f)
  (check (domain=? (make-domain '(#\F . #\M)) (make-domain '(#\A . #\D))) => #f)

;;; --------------------------------------------------------------------

  (check (domain<? (make-domain)     (make-domain #\B)) => #f)
  (check (domain<? (make-domain #\A) (make-domain))     => #f)

  (check 'this (domain<? (make-domain #\A) (make-domain #\B)) => #t)
  (check (domain<? (make-domain #\A) (make-domain #\A)) => #f)
  (check (domain<? (make-domain #\B) (make-domain #\A)) => #f)

  )


(parameterise ((check-test-name	'domain-set-operations))

  (check
      ;; empty
      (domain-intersection (make-domain)
			   (make-domain '(#\A . #\H)))
    (=> domain=?) (make-domain))

  (check
      ;; empty
      (domain-intersection (make-domain '(#\A . #\H))
			   (make-domain))
    (=> domain=?) (make-domain))

  (check
      ;; equal
      (domain-intersection (make-domain '(#\A . #\H))
			   (make-domain '(#\A . #\H)))
    (=> domain=?) (make-domain '(#\A . #\H)))

  (check
      ;; disjoint
      (domain-intersection (make-domain '(#\A . #\C))
			   (make-domain '(#\E . #\H)))
    (=> domain=?) (make-domain))

  (check
      ;; disjoint
      (domain-intersection (make-domain '(#\E . #\H))
			   (make-domain '(#\A . #\C)))
    (=> domain=?) (make-domain))

  (check
      ;; contiguous
      (domain-intersection (make-domain '(#\A . #\D))
			   (make-domain '(#\E . #\H)))
    (=> domain=?) (make-domain))

  (check
      ;; contiguous
      (domain-intersection (make-domain '(#\E . #\H))
			   (make-domain '(#\A . #\D)))
    (=> domain=?) (make-domain))

  (check
      ;; inclusion
      (domain-intersection (make-domain '(#\C . #\F))
			   (make-domain '(#\A . #\H)))
    (=> domain=?) (make-domain '(#\C . #\F)))

  (check
      ;; inclusion
      (domain-intersection (make-domain '(#\A . #\H))
			   (make-domain '(#\C . #\F)))
    (=> domain=?) (make-domain '(#\C . #\F)))

  (check
      (domain-intersection (make-domain '(#\A . #\D) '(#\H . #\M) '(#\O . #\P))
			   (make-domain '(#\C . #\F) '(#\I . #\L) '(#\N . #\Q)))
    (=> domain=?) (make-domain '(#\C . #\D)
			       '(#\I . #\L)
			       '(#\O . #\P)))

  (check
      ;; disjoint tail
      (domain-intersection (make-domain '(#\A . #\D) #\F)
			   (make-domain '(#\C . #\E) #\F #\M #\P #\R))
    (=> domain=?) (make-domain '(#\C . #\D) #\F))

  (check
      ;; disjoint tail
      (domain-intersection (make-domain '(#\A . #\D) #\F #\M #\P #\R)
			   (make-domain '(#\C . #\E) #\F))
    (=> domain=?) (make-domain '(#\C . #\D) #\F))

  (check
      ;; contiguous tail
      (domain-intersection (make-domain '(#\A . #\D) #\F #\G #\H #\I)
			   (make-domain '(#\C . #\E) #\F))
    (=> domain=?) (make-domain '(#\C . #\D) #\F))

  (check
      ;; contiguous tail
      (domain-intersection (make-domain '(#\A . #\D) #\F)
			   (make-domain '(#\C . #\E) #\F #\G #\H #\I))
    (=> domain=?) (make-domain '(#\C . #\D) #\F))

  (check
      ;; overlapping tail
      (domain-intersection (make-domain '(#\A . #\D) #\F)
			   (make-domain '(#\C . #\E) #\F '(#\H . #\N) '(#\L . #\P)))
    (=> domain=?) (make-domain '(#\C . #\D) #\F))

  (check
      ;; overlapping tail
      (domain-intersection (make-domain '(#\A . #\D) #\F '(#\H . #\N) '(#\L . #\P))
			   (make-domain '(#\C . #\E) #\F))
    (=> domain=?) (make-domain '(#\C . #\D) #\F))

;;; --------------------------------------------------------------------

  (check
      ;; empty
      (domain-union (make-domain)
		    (make-domain '(#\A . #\H)))
    (=> domain=?) (make-domain '(#\A . #\H)))

  (check
      ;; empty
      (domain-union (make-domain '(#\A . #\H))
		    (make-domain))
    (=> domain=?) (make-domain '(#\A . #\H)))

  (check
      ;; equal
      (domain-union (make-domain '(#\A . #\H))
		    (make-domain '(#\A . #\H)))
    (=> domain=?) (make-domain '(#\A . #\H)))

  (check
      ;; disjoint
      (domain-union (make-domain '(#\A . #\C))
		    (make-domain '(#\E . #\H)))
    (=> domain=?) (make-domain '(#\A . #\C) '(#\E . #\H)))

  (check
      ;; disjoint
      (domain-union (make-domain '(#\E . #\H))
		    (make-domain '(#\A . #\C)))
    (=> domain=?) (make-domain '(#\A . #\C) '(#\E . #\H)))

  (check
      ;; contiguous
      (domain-union (make-domain '(#\A . #\D))
		    (make-domain '(#\E . #\H)))
    (=> domain=?) (make-domain '(#\A . #\H)))

  (check
      ;; contiguous
      (domain-union (make-domain '(#\E . #\H))
		    (make-domain '(#\A . #\D)))
    (=> domain=?) (make-domain '(#\A . #\H)))

  (check
      ;; inclusion
      (domain-union (make-domain '(#\C . #\F))
		    (make-domain '(#\A . #\H)))
    (=> domain=?) (make-domain '(#\A . #\H)))

  (check
      ;; inclusion
      (domain-union (make-domain '(#\A . #\H))
		    (make-domain '(#\C . #\F)))
    (=> domain=?) (make-domain '(#\A . #\H)))

  (check
      (domain-union (make-domain '(#\A . #\D) '(#\H . #\M) '(#\O . #\P))
		    (make-domain '(#\C . #\F) '(#\I . #\L) '(#\N . #\Q)))
    (=> domain=?) (make-domain '(#\A . #\F)
			       '(#\H . #\Q)))

  (check
      ;; disjoint tail
      (domain-union (make-domain '(#\A . #\D) #\F)
		    (make-domain '(#\C . #\E) #\F #\M #\P #\R))
    (=> domain=?) (make-domain '(#\A . #\E) #\F #\M #\P #\R))

  (check
      ;; disjoint tail
      (domain-union (make-domain '(#\A . #\D) #\F #\M #\P #\R)
		    (make-domain '(#\C . #\E) #\F))
    (=> domain=?) (make-domain '(#\A . #\E) #\F #\M #\P #\R))

  (check
      ;; contiguous tail
      (domain-union (make-domain '(#\A . #\D) #\F #\G #\H #\I)
		    (make-domain '(#\C . #\E) #\F))
    (=> domain=?) (make-domain '(#\A . #\E) #\F #\G #\H #\I))

  (check
      ;; contiguous tail
      (domain-union (make-domain '(#\A . #\D) #\F)
		    (make-domain '(#\C . #\E) #\F #\G #\H #\I))
    (=> domain=?) (make-domain '(#\A . #\E) #\F #\G #\H #\I))

  (check
      ;; overlapping tail
      (domain-union (make-domain '(#\A . #\D) #\F)
		    (make-domain '(#\C . #\E) #\F '(#\H . #\N) '(#\L . #\P)))
    (=> domain=?) (make-domain '(#\A . #\E) #\F '(#\H . #\N) '(#\L . #\P)))

  (check
      ;; overlapping tail
      (domain-union (make-domain '(#\A . #\D) #\F '(#\H . #\N) '(#\L . #\P))
		    (make-domain '(#\C . #\E) #\F))
    (=> domain=?) (make-domain '(#\A . #\E) #\F '(#\H . #\N) '(#\L . #\P)))

;;; --------------------------------------------------------------------

  (check
      ;; empty
      (domain-difference (make-domain)
			 (make-domain '(#\A . #\H)))
    (=> domain=?) (make-domain '(#\A . #\H)))

  (check
      ;; empty
      (domain-difference (make-domain '(#\A . #\H))
			 (make-domain))
    (=> domain=?) (make-domain '(#\A . #\H)))

  (check
      ;; equal
      (domain-difference (make-domain '(#\A . #\H))
			 (make-domain '(#\A . #\H)))
    (=> domain=?) (make-domain))

  (check
      ;; disjoint
      (domain-difference (make-domain '(#\A . #\C))
			 (make-domain '(#\E . #\H)))
    (=> domain=?) (make-domain '(#\A . #\C) '(#\E . #\H)))

  (check
      ;; disjoint
      (domain-difference (make-domain '(#\E . #\H))
			 (make-domain '(#\A . #\C)))
    (=> domain=?) (make-domain '(#\A . #\C) '(#\E . #\H)))

  (check
      ;; contiguous
      (domain-difference (make-domain '(#\A . #\D))
			 (make-domain '(#\E . #\H)))
    (=> domain=?) (make-domain '(#\A . #\H)))

  (check
      ;; contiguous
      (domain-difference (make-domain '(#\E . #\H))
			 (make-domain '(#\A . #\D)))
    (=> domain=?) (make-domain '(#\A . #\H)))

  (check
      ;; inclusion
      (domain-difference (make-domain '(#\C . #\F))
			 (make-domain '(#\A . #\H)))
    (=> domain=?) (make-domain '(#\A . #\B)
			       '(#\G . #\H)))

  (check
      ;; inclusion
      (domain-difference (make-domain '(#\A . #\H))
			 (make-domain '(#\C . #\F)))
    (=> domain=?) (make-domain '(#\A . #\B)
			       '(#\G . #\H)))

  (check
      (domain-difference (make-domain '(#\A . #\D) '(#\H . #\M) '(#\O . #\P))
			 (make-domain '(#\C . #\F) '(#\I . #\L) '(#\N . #\Q)))
    (=> domain=?) (make-domain #\A #\B #\E #\F #\H #\M #\N #\Q))

  (check
      ;; disjoint tail
      (domain-difference (make-domain '(#\A . #\D) #\F)
			 (make-domain '(#\C . #\E) #\F #\M #\P #\R))
    (=> domain=?) (make-domain '(#\A . #\B) #\E #\M #\P #\R))

  (check
      ;; disjoint tail
      (domain-difference (make-domain '(#\A . #\D) #\F #\M #\P #\R)
			 (make-domain '(#\C . #\E) #\F))
    (=> domain=?) (make-domain '(#\A . #\B) #\E #\M #\P #\R))

  (check
      ;; contiguous tail
      (domain-difference (make-domain '(#\A . #\D) #\F #\G #\H #\I)
			 (make-domain '(#\C . #\E) #\F))
    (=> domain=?) (make-domain '(#\A . #\B) #\E #\G #\H #\I))

  (check
      ;; contiguous tail
      (domain-difference (make-domain '(#\A . #\D) #\F)
			 (make-domain '(#\C . #\E) #\F #\G #\H #\I))
    (=> domain=?) (make-domain '(#\A . #\B) #\E #\G #\H #\I))

  (check
      ;; overlapping tail
      (domain-difference (make-domain '(#\A . #\D) #\F)
			 (make-domain '(#\C . #\E) #\F '(#\H . #\N) '(#\L . #\P)))
    (=> domain=?) (make-domain '(#\A . #\B) #\E '(#\H . #\N) '(#\L . #\P)))

  (check
      ;; overlapping tail
      (domain-difference (make-domain '(#\A . #\D) #\F '(#\H . #\N) '(#\L . #\P))
			 (make-domain '(#\C . #\E) #\F))
    (=> domain=?) (make-domain '(#\A . #\B) #\E '(#\H . #\N) '(#\L . #\P)))

;;; --------------------------------------------------------------------

  (check
      (domain-complement (make-domain))
    (=> domain=?)
    (cons inclusive-lower-bound exclusive-upper-bound))

  (check
      (domain-complement (make-domain #\A))
    (=> domain=?)
    (let ((ch #\A))
      (list (cons inclusive-lower-bound ch)
	    (cons (char-next ch) exclusive-upper-bound))))

  (check
      (domain-complement (make-domain '(#\A . #\D) '(#\M . #\Z)))
    (=> domain=?)
    (list (cons inclusive-lower-bound #\A)
	  (cons (char-next #\D) #\M)
	  (cons (char-next #\Z) exclusive-upper-bound)))

  (check
      (domain-complement (make-domain '(#\A . #\D) '(#\M . #\Z) '(#\4 . #\9)))
    (=> domain=?)
    (list (cons inclusive-lower-bound #\4)
	  (cons (char-next #\9) #\A)
	  (cons (char-next #\D) #\M)
	  (cons (char-next #\Z) exclusive-upper-bound)))

  )


(parameterise ((check-test-name	'domain-list-operations))

  (check
      (with-result
       (domain-for-each (lambda (ch)
			  (add-result ch))
			(make-domain #\A #\B #\C))
       #t)
    => '(#t (#\A #\B #\C)))

;;; --------------------------------------------------------------------

  (check
      (domain-every (lambda (ch)
		      (<= 65 (char->integer ch)))
		    (make-domain #\A #\B #\C))
    => #t)

  (check
      (domain-every (lambda (ch)
		      (<= 67 (char->integer ch)))
		    (make-domain #\A #\B #\C))
    => #f)


;;; --------------------------------------------------------------------

  (check
      (domain-any (lambda (ch)
		    (= 66 (char->integer ch)))
		  (make-domain #\A #\B #\C))
    => #t)

  (check
      (domain-any (lambda (ch)
		    (= 100 (char->integer ch)))
		  (make-domain #\A #\B #\C))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (domain-fold (lambda (ch knil)
		     (cons (char->integer ch) knil))
		   '()
		   (make-domain #\A #\B #\C))
    => '(67 66 65))

  (check
      (domain-fold (lambda (ch knil)
		     (cons (char->integer ch) knil))
		   '()
		   (make-domain))
    => '())

;;; --------------------------------------------------------------------

  (check
      (domain->list (make-domain))
    => '())

  (check
      (domain->list (make-domain #\A))
    => '(#\A))

  (check
      (domain->list (make-domain #\A #\B #\C #\D))
    => '(#\A #\B #\C #\D))

  )


(parameterise ((check-test-name	'domain-string-operations))

  (check
      (string->domain "")
    (=> domain=?)
    (make-domain))

  (check
      (string->domain "ABCD")
    (=> domain=?)
    (make-domain #\A #\B #\C #\D))

  (check
      (string->domain "ABCDBBCC")
    (=> domain=?)
    (make-domain #\A #\B #\C #\D))

  )


;;;; done

(check-report)

;;; end of file
