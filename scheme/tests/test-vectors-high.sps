;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for the vectors library
;;;Date: Tue Jun 16, 2009
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
  (vectors)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing vectors\n")


(parameterise ((check-test-name 'views))

  (check
      (subvector* '#(0 1 2 3))
    => '#(0 1 2 3))

  (check
      (subvector* ('#(0 1 2 3)))
    => '#(0 1 2 3))

  (check
      (subvector* ('#(0 1 2 3) 2))
    => '#(2 3))

  (check
      (subvector* ('#(0 1 2 3) 0 4))
    => '#(0 1 2 3))

  (check
      (subvector* ('#(0 1 2 3) 0 0))
    => '#())

  (check
      (subvector* ('#(0 1 2 3) 1 1))
    => '#())

  (check
      (subvector* ('#(0 1 2 3) 0 1))
    => '#(0))

;;; --------------------------------------------------------------------

  (check
      (subvector* (view '#(0 1 2 3)))
    => '#(0 1 2 3))

  (check
      (subvector* (view '#(0 1 2 3) (start 2)))
    => '#(2 3))

  (check
      (subvector* (view '#(0 1 2 3) (start 0) (past 4)))
    => '#(0 1 2 3))

  (check
      (subvector* (view '#(0 1 2 3) (start 0) (past 0)))
    => '#())

  (check
      (subvector* (view '#(0 1 2 3) (start 1) (past 1)))
    => '#())

  (check
      (subvector* (view '#(0 1 2 3) (start 0) (past 1)))
    => '#(0))

  (check
      (subvector* (view '#(0 1 2 3) (past 2)))
    => '#(0 1))

  )


(parameterise ((check-test-name 'predicates))

  (check
      (vector-null? '#(0 1 2 3))
    => #f)

  (check
      (vector-null? '#())
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((vec '#(#\a #\b #\c)))
	(vector-every char-alphabetic? vec))
    => #t)

  (check
      (let* ((vec '#(#\a #\b #\c #\2)))
	(vector-every char-alphabetic? vec))
    => #f)

  (check
      (let* ((vec '#(#\a #\b #\2 #\d)))
	(vector-every char-alphabetic? vec))
    => #f)

  (check
      (let* ((vec '#()))
	(vector-every char-alphabetic? vec))
    => #f)

  (check
      (let* ((vec '#(1 2 3 4)))
	(vector-every (lambda (x) x) vec))
    => 4)

;;; --------------------------------------------------------------------

  (check
      (let* ((vec '#(#\1 #\2 #\a #\4)))
	(vector-any char-alphabetic? vec))
    => #t)

  (check
      (let* ((vec '#(#\1 #\2 #\3 #\a)))
	(vector-any char-alphabetic? vec))
    => #t)

  (check
      (let* ((vec '#(#\1 #\2 #\3 #\4)))
	(vector-any char-alphabetic? vec))
    => #f)

  (check
      (let* ((vec '#()))
	(vector-any char-alphabetic? vec))
    => #f)

  (check
      (let* ((vec '#(1 2 3 4)))
	(vector-any (lambda (x) x) vec))
    => 1)

  )


(parameterise ((check-test-name 'comparison))

  (check-for-true
   (let* ((vec '#(#\a #\b #\c #\d)))
     (vector= char=? vec vec)))

  (check-for-true
   (vector= char=? ('#(#\1 #\2 #\a #\b #\c #\d) 2) '#(#\a #\b #\c #\d)))

  (check-for-false
   (vector= char=? '#(#\a #\b #\c) '#(#\a #\b #\c #\d)))

  (check-for-false
   (vector= char=? '#(#\a #\b #\c #\d) '#(#\a #\b #\c)))

  (check-for-false
   (vector= char=? '#(#\A #\B #\c #\d) '#(#\a #\b #\c #\d)))

  (check-for-false
   (vector= char=? '#(#\a #\b #\c #\d) '#(#\a #\2 #\c #\d)))

;;; --------------------------------------------------------------------

  (check-for-false
   (vector<> char=? '#(#\a #\b #\c #\d) '#(#\a #\b #\c #\d)))

  (check-for-true
   (vector<> char=? '#(#\a #\b #\c) '#(#\a #\b #\c #\d)))

  (check-for-true
   (vector<> char=? '#(#\a #\b #\c #\d) '#(#\a #\b #\c)))

  (check-for-true
   (vector<> char=? '#(#\A #\B #\c #\d) '#(#\a #\b #\c #\d)))

  (check-for-true
   (vector<> char=? '#(#\a #\b #\c #\d) '#(#\a #\2 #\c #\d)))

  )


(parameterise ((check-test-name 'mapping))

  (check
      (vector-map* (lambda (i x) (char-upcase x)) '#(#\a #\b #\c #\d))
    => '#(#\A #\B #\C #\D))

  (check
      (vector-map* (lambda (i x) (char-upcase x)) '#())
    => '#())

;;; --------------------------------------------------------------------

  (check
      (let ((vec (vector-copy '#(#\a #\b #\c #\d))))
	(vector-map*! (lambda (i x) (char-upcase x)) vec)
	vec)
    => '#(#\A #\B #\C #\D))

  (check
      (let* ((vec '#()))
	(vector-map*! (lambda (i x) (char-upcase x)) vec)
	vec)
    => '#())

;;; --------------------------------------------------------------------

  (check
      (let* ((vec '#(#\a #\b #\c #\d))
	     (result '#()))
	(vector-for-each*
	 (lambda (i ch)
	   (set! result
		 (vector-append result
				(vector (char->integer (char-upcase ch))))))
	 vec)
	result)
    => '#(65 66 67 68))

  (check
      (let* ((vec '#())
	     (result '#()))
	(vector-for-each*
	 (lambda (i ch)
	   (set! result
		 (vector-append result
				(vector (char->integer (char-upcase ch))))))
	 vec)
	result)
    => '#())

  )


(parameterise ((check-test-name 'folding))

  (check
      (vector-fold (lambda (i nil x) (cons x nil)) '() '#(#\a #\b #\c #\d))
    => '(#\d #\c #\b #\a))

  (check
      (vector-fold (lambda (i nil x) (cons x nil)) '() '#())
    => '())

  (check
      (vector-fold (lambda (i count c)
		     (if (char-upper-case? c)
			 (+ count 1)
		       count))
		   0
		   '#(#\A #\B #\C #\d #\e #\f #\G #\H #\i))
    => 5)

;;; --------------------------------------------------------------------

  (check
      (vector-fold-right (lambda (i nil x) (cons x nil)) '() '#(#\a #\b #\c #\d))
    => '(#\a #\b #\c #\d))

  (check
      (vector-fold-right (lambda (i nil x) (cons x nil)) '() '#())
    => '())

;;; --------------------------------------------------------------------

  (check
      (vector-unfold null? car cdr '(#\a #\b #\c #\d))
    => '#(#\a #\b #\c #\d))

  (check
      (vector-unfold null? car cdr '())
    => '#())

;;; --------------------------------------------------------------------

  (check
      (vector-unfold-right null? car cdr '(#\a #\b #\c #\d))
    => '#(#\d #\c #\b #\a))

  (check
      (vector-unfold-right null? car cdr '())
    => '#())

;;; --------------------------------------------------------------------

  (check
      (vector-fold* cons '() '#(#\a #\b #\c #\d))
    => '(#\d #\c #\b #\a))

  (check
      (vector-fold* cons '() ('#(#\a #\b #\c #\d) 1 3))
    => '(#\c #\b))

;;; --------------------------------------------------------------------

  (check
      (vector-fold-right* cons '() '#(#\a #\b #\c #\d))
    => '(#\a #\b #\c #\d))

  (check
      (vector-fold-right* cons '() ('#(#\a #\b #\c #\d) 1 3))
    => '(#\b #\c))

;;; --------------------------------------------------------------------

  (check
      (vector-tabulate (lambda (idx) (integer->char (+ 65 idx))) 4)
    => '#(#\A #\B #\C #\D))

  (check
      (vector-tabulate integer->char 0)
    => '#())

  )


(parameterise ((check-test-name 'selecting))

  (check
      (vector-take '#(#\a #\b #\c #\d) 2)
    => '#(#\a #\b))

  (check
      (vector-take '#() 0)
    => '#())

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(vector-take '#(#\a #\b #\c #\d) 5))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (vector-take-right '#(#\a #\b #\c #\d) 2)
    => '#(#\c #\d))

  (check
      (vector-take-right '#() 0)
    => '#())

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(vector-take-right '#(#\a #\b #\c #\d) 5))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (vector-drop '#(#\a #\b #\c #\d) 2)
    => '#(#\c #\d))

  (check
      (vector-drop '#() 0)
    => '#())

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(vector-drop '#(#\a #\b #\c #\d) 5))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (vector-drop-right '#(#\a #\b #\c #\d) 2)
    => '#(#\a #\b))

  (check
      (vector-drop-right '#() 0)
    => '#())

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(vector-drop-right '#(#\a #\b #\c #\d) 5))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (vector-trim '#(#\A #\A #\A #\b #\c #\d) char-upper-case?)
    => '#(#\b #\c #\d))

  (check
      (vector-trim '#(#\b #\c #\d) char-upper-case?)
    => '#(#\b #\c #\d))

  (check
      (vector-trim '#() char-upper-case?)
    => '#())

;;; --------------------------------------------------------------------

  (check
      (vector-trim-right '#(#\b #\c #\d #\A #\A #\A) char-upper-case?)
    => '#(#\b #\c #\d))

  (check
      (vector-trim-right '#(#\b #\c #\d) char-upper-case?)
    => '#(#\b #\c #\d))

  (check
      (vector-trim-right '#() char-upper-case?)
    => '#())

;;; --------------------------------------------------------------------

  (check
      (vector-trim-both '#(#\A #\A #\A #\b #\c #\d #\A #\A #\A) char-upper-case?)
    => '#(#\b #\c #\d))

  (check
      (vector-trim-both '#(#\b #\c #\d) char-upper-case?)
    => '#(#\b #\c #\d))

  (check
      (vector-trim-both '#() char-upper-case?)
    => '#())

;;; --------------------------------------------------------------------

  (check
      (vector-pad '#(#\a #\b #\c) 3 #\0)
    => '#(#\a #\b #\c))

  (check
      (vector-pad '#(#\a #\b #\c) 5 #\0)
    => '#(#\0 #\0 #\a #\b #\c))

  (check
      (vector-pad '#(#\a #\b #\c) 5)
    => '#(#\space #\space #\a #\b #\c))

  (check
      (vector-pad '#(#\a #\b #\c) 2 #\0)
    => '#(#\b #\c))

  (check
      (vector-pad '#(#\a #\b #\c) 0 #\0)
    => '#())

;;; --------------------------------------------------------------------

  (check
      (vector-pad-right '#(#\a #\b #\c) 3 #\0)
    => '#(#\a #\b #\c))

  (check
      (vector-pad-right '#(#\a #\b #\c) 5 #\0)
    => '#(#\a #\b #\c #\0 #\0))

  (check
      (vector-pad-right '#(#\a #\b #\c) 2 #\0)
    => '#(#\a #\b))

  (check
      (vector-pad-right '#(#\a #\b #\c) 0 #\0)
    => '#())

;;; --------------------------------------------------------------------

  (check
      (let* ((vec (vector-copy '#(#\1 #\2))))
	(vector-copy*! vec (view '#(#\a #\b #\c #\d) (past 2)))
	vec)
    => '#(#\a #\b))

  (check
      (let ((vec '#()))
	(vector-copy*! vec ('#(#\a #\b #\c #\d) 0 0))
	vec)
    => '#())

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(let* ((vec (vector-copy '#(#\1 #\2))))
	  (vector-copy*! (vec 3) (view '#(#\a #\b #\c #\d) (past 2)))
	  vec))
    => #t)

  )


(parameterise ((check-test-name 'prefix))

  (check
      (vector-prefix-length '#(#\a #\b #\c #\d #\e #\f #\g)
			    '#(#\a #\b #\c #\d #\1 #\2 #\3)
			    char=?)
    => 4)

  (check
      (vector-prefix-length '#(#\a #\B #\c #\d #\e #\f #\g)
			    '#(#\a #\b #\c #\d #\1 #\2 #\3)
			    char=?)
    => 1)

  (check
      (vector-prefix-length '#(#\e #\f #\g) '#(#\1 #\2 #\3) char=?)
    => 0)

  (check
      (vector-prefix-length '#(#\a) '#(#\a) char=?)
    => 1)

  (check
      (vector-prefix-length '#(1) '#(2) =)
    => 0)

  (check
      (vector-prefix-length '#() '#(#\a #\b #\c #\d #\1 #\2 #\3) char=?)
    => 0)

  (check
      (vector-prefix-length '#(#\a #\b #\c #\d #\e #\f #\g) '#() char=?)
    => 0)

;;; --------------------------------------------------------------------

  (check
      (vector-suffix-length '#(#\e #\f #\g #\a #\b #\c #\d)
			    '#(#\1 #\2 #\3 #\a #\b #\c #\d)
			    char=?)
    => 4)

  (check
      (vector-suffix-length '#(#\e #\f #\g #\a #\b #\c #\d)
			    '#(#\1 #\2 #\3 #\a #\b #\C #\d)
			    char=?)
    => 1)

  (check
      (vector-suffix-length '#(#\e #\f #\g) '#(#\1 #\2 #\3) char=?)
    => 0)

  (check
      (vector-suffix-length '#(1) '#(1) =)
    => 1)

  (check
      (vector-suffix-length '#(1) '#(2) =)
    => 0)

  (check
      (vector-suffix-length '#() '#(#\a #\b #\c #\d #\1 #\2 #\3) char=?)
    => 0)

  (check
      (vector-suffix-length '#(#\a #\b #\c #\d #\e #\f #\g) '#() char=?)
    => 0)

;;; --------------------------------------------------------------------

  (check
      (vector-prefix? '#(#\a #\b #\c #\d) '#(#\a #\b #\c #\d #\1 #\2 #\3) char=?)
    => #t)

  (check
      (vector-prefix? '#(#\a #\b #\c #\d) '#(#\a #\B #\c #\d #\1 #\2 #\3) char=?)
    => #f)

  (check
      (vector-prefix? '#(#\e #\f #\g) '#(#\1 #\2 #\3) char=?)
    => #f)

  (check
      (vector-prefix? '#() '#(#\1 #\2 #\3) char=?)
    => #t)

  (check
      (vector-prefix? '#(#\e #\f #\g) '#() char=?)
    => #f)

  (check
      (vector-prefix? '#() '#() char=?)
    => #t)

;;; --------------------------------------------------------------------

  (check
      (vector-suffix? '#(#\a #\b #\c #\d) '#(#\1 #\2 #\3 #\a #\b #\c #\d) char=?)
    => #t)

  (check
      (vector-suffix? '#(#\a #\b #\c #\d) '#(#\1 #\2 #\3 #\a #\B #\c #\d) char=?)
    => #f)

  (check
      (vector-suffix? '#(#\e #\f #\g) '#(#\1 #\2 #\3) char=?)
    => #f)

  (check
      (vector-suffix? '#() '#(#\1 #\2 #\3) char=?)
    => #t)

  (check
      (vector-suffix? '#(#\e #\f #\g) '#() char=?)
    => #f)

  (check
      (vector-suffix? '#() '#() char=?)
    => #t)

  )


(parameterise ((check-test-name 'searching))

  (check
      (vector-index '#(#\a #\B #\c #\d) char-upper-case?)
    => 1)

  (check
      (vector-index ('#(#\a #\B #\c #\d) 1) char-upper-case?)
    => 1)

  (check
      (vector-index '#(#\a #\b #\c #\d) char-upper-case?)
    => #f)

  (check
      (vector-index '#() char-upper-case?)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (vector-index-right '#(#\a #\B #\c #\d) char-upper-case?)
    => 1)

  (check
      (vector-index-right ('#(#\a #\B #\c #\d) 1) char-upper-case?)
    => 1)

  (check
      (vector-index-right '#(#\a #\b #\c #\d) char-upper-case?)
    => #f)

  (check
      (vector-index-right '#() char-upper-case?)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (vector-skip '#(#\B #\a #\c #\d) char-upper-case?)
    => 1)

  (check
      (vector-skip ('#(#\B #\a #\c #\d) 1) char-upper-case?)
    => 1)

  (check
      (vector-skip '#(#\A #\B #\C #\D) char-upper-case?)
    => #f)

  (check
      (vector-skip '#() char-upper-case?)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (vector-skip-right '#(#\a #\c #\d #\B) char-upper-case?)
    => 2)

  (check
      (vector-skip-right ('#(#\a #\c #\d #\B) 1) char-upper-case?)
    => 2)

  (check
      (vector-skip-right '#(#\A #\B #\C #\D) char-upper-case?)
    => #f)

  (check
      (vector-skip-right '#() char-upper-case?)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (vector-count '#(#\a #\B #\c #\A #\d) char-upper-case?)
    => 2)

  (check
      (vector-count ('#(#\a #\B #\c #\d) 1) char-upper-case?)
    => 1)

  (check
      (vector-count '#(#\a #\b #\c #\d) char-upper-case?)
    => 0)

  (check
      (vector-count '#() char-upper-case?)
    => 0)

;;; --------------------------------------------------------------------

  (check
      (vector-contains '#(#\c #\i #\a #\o #\space #\h #\e #\l #\l #\o #\space #\s #\a #\l #\u #\t)
		       '#(#\h #\e #\l #\l #\o)
		       char=?)
    => 5)

  (check
      (vector-contains '#(#\c #\i #\a #\o #\space #\h #\e #\l #\l #\o #\space #\s #\a #\l #\u #\t)
		       '#(#\h #\o #\l #\a)
		       char=?)
    => #f)

  (check
      (vector-contains '#(#\c #\i #\a #\o #\space #\h #\e #\l #\l #\o #\space #\s #\a #\l #\u #\t)
		       '#()
		       char=?)
    => 0)

  (check
      (vector-contains '#() '#(#\h #\e #\l #\l #\o) char=?)
    => #f)

  )


(parameterise ((check-test-name 'filtering))

  (check
      (vector-delete '#(#\a #\B #\c #\B #\d) char-upper-case?)
    => '#(#\a #\c #\d))

  (check
      (vector-delete '#(#\a #\b #\c #\b #\d) char-upper-case?)
    => '#(#\a #\b #\c #\b #\d))

  (check
      (vector-delete '#() char-upper-case?)
    => '#())

;;; --------------------------------------------------------------------

  (check
      (vector-filter '#(#\a #\B #\c #\B #\d) char-upper-case?)
    => '#(#\B #\B))

  (check
      (vector-filter '#(#\a #\b #\c #\b #\d) char-upper-case?)
    => '#())

  (check
      (vector-filter '#() char-upper-case?)
    => '#())

  )


(parameterise ((check-test-name 'lists))

  (check
      (vector->list* '#(#\a #\b #\c #\d))
    => '(#\a #\b #\c #\d))

  (check
      (vector->list* ('#(#\a #\b #\c #\d) 1 3))
    => '(#\b #\c))

  (check
      (vector->list* '#())
    => '())

;;; --------------------------------------------------------------------

  (check
      (reverse-list->vector '(#\a #\b #\c #\d))
    => '#(#\d #\c #\b #\a))

  (check
      (reverse-list->vector '())
    => '#())

  )


(parameterise ((check-test-name 'xsubvector))

  (check
      (xsubvector '#(#\c #\i #\a #\o #\space) 0 5)
    => '#(#\c #\i #\a #\o #\space))

  (check
      (xsubvector '#(#\c #\i #\a #\o #\space) 0 9)
    => '#(#\c #\i #\a #\o #\space #\c #\i #\a #\o))

  (check
      (xsubvector '#(#\c #\i #\a #\o #\space) -5 5)
    => '#(#\c #\i #\a #\o #\space #\c #\i #\a #\o #\space))

  (check
      (xsubvector '#(#\c #\i #\a #\o #\space) 2 4)
    => '#(#\a #\o))

  (check
      (xsubvector '#(#\c #\i #\a #\o #\space) -3 7)
    => '#(#\a #\o #\space #\c #\i #\a #\o #\space #\c #\i))

  (check (xsubvector '#(#\a #\b #\c #\d #\e #\f) 1 7)
    => '#(#\b #\c #\d #\e #\f #\a))
  (check (xsubvector '#(#\a #\b #\c #\d #\e #\f) 2 8)
    => '#(#\c #\d #\e #\f #\a #\b))
  (check (xsubvector '#(#\a #\b #\c #\d #\e #\f) 3 9)
    => '#(#\d #\e #\f #\a #\b #\c))
  (check (xsubvector '#(#\a #\b #\c #\d #\e #\f) 4 10)
    => '#(#\e #\f #\a #\b #\c #\d))
  (check (xsubvector '#(#\a #\b #\c #\d #\e #\f) 5 11)
    => '#(#\f #\a #\b #\c #\d #\e))

  (check (xsubvector '#(#\a #\b #\c #\d #\e #\f) -1 5)
    => '#(#\f #\a #\b #\c #\d #\e))
  (check (xsubvector '#(#\a #\b #\c #\d #\e #\f) -2 4)
    => '#(#\e #\f #\a #\b #\c #\d ))
  (check (xsubvector '#(#\a #\b #\c #\d #\e #\f) -3 3)
    => '#(#\d #\e #\f #\a #\b #\c))
  (check (xsubvector '#(#\a #\b #\c #\d #\e #\f) -4 2)
    => '#(#\c #\d #\e #\f #\a #\b))
  (check (xsubvector '#(#\a #\b #\c #\d #\e #\f) -5 1)
    => '#(#\b #\c #\d #\e #\f #\a))

  (check
      (xsubvector '#(#\c #\i #\a #\o #\space) 3 3)
    => '#())

  (check
      (guard (exc ((assertion-violation? exc)
		   #t))
	(xsubvector '#() 0 5))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let ((result (vector-copy '#(#\0 #\1 #\2 #\3 #\4))))
	(vector-xcopy! result '#(#\c #\i #\a #\o #\space) 0 5)
	result)
    => '#(#\c #\i #\a #\o #\space))

  (check
      (let ((result (vector-copy '#(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8))))
	(vector-xcopy! result '#(#\c #\i #\a #\o #\space) 0 9)
	result)
    => '#(#\c #\i #\a #\o #\space #\c #\i #\a #\o))

  (check
      (let ((result (vector-copy '#(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))))
	(vector-xcopy! result '#(#\c #\i #\a #\o #\space) -5 5)
	result)
    => '#(#\c #\i #\a #\o #\space #\c #\i #\a #\o #\space))

  (check
      (let ((result (vector-copy '#(#\0 #\1))))
	(vector-xcopy! result '#(#\c #\i #\a #\o #\space) 2 4)
	result)
    => '#(#\a #\o))

  (check
      (let ((result (vector-copy '#(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))))
	(vector-xcopy! result '#(#\c #\i #\a #\o #\space) -3 7)
	result)
    => '#(#\a #\o #\space #\c #\i #\a #\o #\space #\c #\i))

  (check
      (guard (exc ((assertion-violation? exc) #t))
	  (vector-xcopy! '#() '#() 0 5))
    => #t)

  )


(parameterise ((check-test-name 'filling))

  (check
      (let* ((vec (vector-copy '#(#\a #\b #\c #\d))))
	(vector-fill*! vec #\b)
	vec)
    => '#(#\b #\b #\b #\b))

  (check
      (let* ((vec (vector-copy '#(#\a #\c #\c #\d))))
	(vector-fill*! (vec 1 3) #\b)
	vec)
    => '#(#\a #\b #\b #\d))

  (check
      (let* ((vec (vector-copy '#())))
	(vector-fill*! (vec 0 0) #\b)
	vec)
    => '#())

  )


(parameterise ((check-test-name 'reverse))

  (check
      (vector-reverse '#(#\a #\b #\c #\d))
    => '#(#\d #\c #\b #\a))

  (check
      (vector-reverse '#())
    => '#())

;;; --------------------------------------------------------------------

  (check
      (let* ((vec (vector-copy '#(#\a #\b #\c #\d))))
	(vector-reverse! vec)
	vec)
    => '#(#\d #\c #\b #\a))

  (check
      (let* ((vec (vector-copy '#())))
	(vector-reverse! vec)
	vec)
    => '#())

  )


(parameterise ((check-test-name 'concatenate))

  (check
      (vector-concatenate '(#(#\c #\i #\a #\o) #(#\space)
			    #(#\h #\e #\l #\l #\o) #(#\space)
			    #(#\s #\a #\l #\u #\t)))
    => '#(#\c #\i #\a #\o #\space #\h #\e #\l #\l #\o #\space #\s #\a #\l #\u #\t))

  (check
      (vector-concatenate '())
    => '#())

;;; --------------------------------------------------------------------

  (check
      (vector-concatenate-reverse '(#(#\c #\i #\a #\o) #(#\space)
				    #(#\h #\e #\l #\l #\o) #(#\space)
				    #(#\s #\a #\l #\u #\t))
				  '#(#\space #\h #\o #\l #\a) 3)
    => '#(#\s #\a #\l #\u #\t #\space #\h #\e #\l #\l #\o #\space #\c #\i #\a #\o #\space #\h #\o))

  (check
      (vector-concatenate-reverse '(#(#\c #\i #\a #\o) #(#\space)
				    #(#\h #\e #\l #\l #\o) #(#\space)
				    #(#\s #\a #\l #\u #\t))
				  '#(#\space #\h #\o #\l #\a))
    => '#(#\s #\a #\l #\u #\t #\space #\h #\e #\l #\l #\o #\space
	  #\c #\i #\a #\o #\space #\h #\o #\l #\a))

  (check
      (vector-concatenate-reverse '(#(#\c #\i #\a #\o) #(#\space)
				    #(#\h #\e #\l #\l #\o) #(#\space)
				    #(#\s #\a #\l #\u #\t)))
    => '#(#\s #\a #\l #\u #\t #\space #\h #\e #\l #\l #\o #\space #\c #\i #\a #\o))

  (check
      (vector-concatenate-reverse '())
    => '#())

  )


(parameterise ((check-test-name 'replace))

  (check
      (vector-replace '#(#\a #\b #\c #\d) '#(#\1 #\2 #\3 #\4))
    => '#(#\1 #\2 #\3 #\4))

  (check
      (vector-replace ('#(#\a #\b #\c #\d) 2 2) '#(#\1 #\2 #\3 #\4))
    => '#(#\a #\b #\1 #\2 #\3 #\4 #\c #\d))

  (check
      (vector-replace ('#(#\a #\b #\c #\d) 2 2) '#())
    => '#(#\a #\b #\c #\d))

  (check
      (vector-replace ('#(#\a #\b #\c #\d) 1 3) '#(#\1 #\2 #\3 #\4))
    => '#(#\a #\1 #\2 #\3 #\4 #\d))

  (check
      (vector-replace ('#(#\a #\b #\c #\d) 0 3) '#(#\1 #\2 #\3 #\4))
    => '#(#\1 #\2 #\3 #\4 #\d))

  (check
      (vector-replace ('#(#\a #\b #\c #\d) 1 4) '#(#\1 #\2 #\3 #\4))
    => '#(#\a #\1 #\2 #\3 #\4))

  )


;;;; done

(check-report)

;;; end of file
