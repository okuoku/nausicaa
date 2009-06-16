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
  (strings)
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
    => '#(1 2))

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
      (vector-empty? '#(0 1 2 3))
    => #f)

  (check
      (vector-empty? '#())
    => #t)

;;; --------------------------------------------------------------------

  (check
      (guard (exc ((assertion-violation? exc)
		   (condition-who exc)))
	(vector-every 123 '#(0 1 2)))
    => '%vector-every)

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
      (let* ((vec '#(#\a #\b #\2 #d)))
	(vector-every char-alphabetic? vec))
    => #f)

  (check
      (let* ((vec '#()))
	(vector-every char-alphabetic? vec))
    => #f)

  (check
      (let* ((vec '#(1 2 3 4)))
	(vector-every (lambda (x) x) vec))
    => #\4)

;;; --------------------------------------------------------------------

  (check
      (guard (exc ((assertion-violation? exc)
		   (condition-who exc)))
	(vector-any 123 '#(0 1 2)))
    => '%vector-any)

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
    => #\1)

  )


(parameterise ((check-test-name 'comparison))

  (check
      (vector-compare "abcdefg" "abcd123" values values values)
    => 4)

  (check
      (vector-compare "abcdef" "abcd123" values values values)
    => 4)

  (check
      (vector-compare "efg" "123" values values values)
    => 0)

  (check
      (vector-compare '#() "abcd" values values values)
    => 0)

  (check
      (vector-compare "abcd" '#() values values values)
    => 0)

  (check
      (vector-compare "abcdA" "abcdA"
		      (lambda (idx) 'less)
		      (lambda (idx) 'equal)
		      (lambda (idx) 'greater))
    => 'equal)

  (check
      (vector-compare "abcdA" "abcdB"
		      (lambda (idx) 'less)
		      (lambda (idx) 'equal)
		      (lambda (idx) 'greater))
    => 'less)

  (check
      (vector-compare "abcdB" "abcdA"
		      (lambda (idx) 'less)
		      (lambda (idx) 'equal)
		      (lambda (idx) 'greater))
    => 'greater)

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((str "abcd"))
     (vector= str str)))

  (check-for-true
   (vector= ("12abcd" 2) "abcd"))

  (check-for-false
   (vector= "abc" "abcd"))

  (check-for-false
   (vector= "abcd" "abc"))

  (check-for-false
   (vector= "ABcd" "abcd"))

  (check-for-false
   (vector= "abcd" "a2cd"))

;;; --------------------------------------------------------------------

  (check-for-false
   (vector<> "abcd" "abcd"))

  (check-for-true
   (vector<> "abc" "abcd"))

  (check-for-true
   (vector<> "abcd" "abc"))

  (check-for-true
   (vector<> "ABcd" "abcd"))

  (check-for-true
   (vector<> "abcd" "a2cd"))

  )


(parameterise ((check-test-name 'mapping))

  (check
      (vector-map char-upcase "aaaa")
    => "AAAA")

  (check
      (vector-map char-upcase '#())
    => '#())

;;; --------------------------------------------------------------------

  (check
      (let ((str (vector-copy "aaaa")))
	(vector-map! char-upcase str)
	str)
    => "AAAA")

  (check
      (let* ((str '#()))
	(vector-map! char-upcase str)
	str)
    => '#())

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aaaa")
	     (result '#()))
	(vector-for-each*
	 (lambda (ch)
	   (set! result
		 (vector-append result
				(number->vector (char->integer (char-upcase ch))))))
	 str)
	result)
    => "65656565")

  (check
      (let* ((str '#())
	     (result '#()))
	(vector-for-each*
	 (lambda (ch)
	   (set! result
		 (vector-append result
				(number->vector (char->integer (char-upcase ch))))))
	 str)
	result)
    => '#())

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aaaa")
	     (result '()))
	(vector-for-each-index
	 (lambda (idx)
	   (set! result (cons idx result)))
	 str)
	result)
    => '(3 2 1 0))

  (check
      (let* ((str '#())
	     (result '()))
	(vector-for-each-index
	 (lambda (idx)
	   (set! result (cons idx result)))
	 str)
	result)
    => '())

  )


(parameterise ((check-test-name 'case))

  (check
      (vector-upcase* "abcd")
    => "ABCD")

  (check
      (vector-upcase* "123abcd")
    => "123ABCD")

  (check
      (vector-upcase* "---abcd")
    => "---ABCD")

  (check
      (vector-upcase* "abcd efgh")
    => "ABCD EFGH")

;;; --------------------------------------------------------------------

  (check
      (let* ((str (vector-copy "abcd")))
	(vector-upcase*! str)
	str)
    => "ABCD")

  (check
      (let* ((str (vector-copy "123abcd")))
	(vector-upcase*! str)
	str)
    => "123ABCD")

  (check
      (let* ((str (vector-copy "---abcd")))
	(vector-upcase*! str)
	str)
    => "---ABCD")

  (check
      (let* ((str (vector-copy "abcd efgh")))
	(vector-upcase*! str)
	str)
    => "ABCD EFGH")

;;; --------------------------------------------------------------------

  (check
      (vector-downcase* "ABCD")
    => "abcd")

  (check
      (vector-downcase* "123AbcD")
    => "123abcd")

  (check
      (vector-downcase* "---aBCd")
    => "---abcd")

  (check
      (vector-downcase* "abcd EFGH")
    => "abcd efgh")

;;; --------------------------------------------------------------------

  (check
      (let* ((str (vector-copy "aBCd")))
	(vector-downcase*! str)
	str)
    => "abcd")

  (check
      (let* ((str (vector-copy "123ABcd")))
	(vector-downcase*! str)
	str)
    => "123abcd")

  (check
      (let* ((str (vector-copy "---aBCD")))
	(vector-downcase*! str)
	str)
    => "---abcd")

  (check
      (let* ((str (vector-copy "abCD Efgh")))
	(vector-downcase*! str)
	str)
    => "abcd efgh")

;;; --------------------------------------------------------------------

  (check
      (vector-titlecase* "abcd")
    => "Abcd")

  (check
      (vector-titlecase* "123abcd")
    => "123Abcd")

  (check
      (vector-titlecase* "---abcd")
    => "---Abcd")

  (check
      (vector-titlecase* "abcd efgh")
    => "Abcd Efgh")

  (check
      (vector-titlecase* ("greasy fried chicken" 2))
    => "Easy Fried Chicken")

;;; --------------------------------------------------------------------

  (check
      (let* ((str (vector-copy "abcd")))
	(vector-titlecase*! str)
	str)
    => "Abcd")

  (check
      (let* ((str (vector-copy "123abcd")))
	(vector-titlecase*! str)
	str)
    => "123Abcd")

  (check
      (let* ((str (vector-copy "---abcd")))
	(vector-titlecase*! str)
	str)
    => "---Abcd")

  (check
      (let* ((str (vector-copy "abcd efgh")))
	(vector-titlecase*! str)
	str)
    => "Abcd Efgh")

  (check
      (let ((str (vector-copy "greasy fried chicken")))
	(vector-titlecase*! (str 2))
	str)
    => "grEasy Fried Chicken")

  )


(parameterise ((check-test-name 'folding))

  (check
      (vector-fold cons '() "abcd")
    => '(#\d #\c #\b #\a))

  (check
      (vector-fold cons '() '#())
    => '())

  (check
      (vector-fold (lambda (c count)
		     (if (char-upper-case? c)
			 (+ count 1)
		       count))
		   0
		   "ABCdefGHi")
    => 5)

  (check
      (let* ((str "abc\\de\\f\\ghi")
	     (ans-len (vector-fold
		       (lambda (c sum)
			 (+ sum (if (char=? c #\\) 2 1)))
		       0 str))
	     (ans (make-vector ans-len)))
	(vector-fold
	 (lambda (c i)
	   (let ((i (if (char=? c #\\)
			(begin
			  (vector-set! ans i #\\)
			  (+ i 1))
		      i)))
	     (vector-set! ans i c)
	     (+ i 1)))
	 0 str)
	ans)
    => "abc\\\\de\\\\f\\\\ghi")

;;; --------------------------------------------------------------------

  (check
      (vector-fold-right cons '() "abcd")
    => '(#\a #\b #\c #\d))

  (check
      (vector-fold-right cons '() '#())
    => '())

;;; --------------------------------------------------------------------

  (check
      (vector-unfold null? car cdr '(#\a #\b #\c #\d))
    => "abcd")

  (check
      (vector-unfold null? car cdr '())
    => '#())

;;; --------------------------------------------------------------------

  (check
      (vector-unfold-right null? car cdr '(#\a #\b #\c #\d))
    => "dcba")

  (check
      (vector-unfold-right null? car cdr '())
    => '#())

;;; --------------------------------------------------------------------

  (check
      (vector-tabulate (lambda (idx) (integer->char (+ 65 idx))) 4)
    => "ABCD")

  (check
      (vector-tabulate integer->char 0)
    => '#())

  )


(parameterise ((check-test-name 'selecting))

  (check
      (vector-take "abcd" 2)
    => "ab")

  (check
      (vector-take '#() 0)
    => '#())

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(vector-take "abcd" 5))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (vector-take-right "abcd" 2)
    => "cd")

  (check
      (vector-take-right '#() 0)
    => '#())

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(vector-take-right "abcd" 5))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (vector-drop "abcd" 2)
    => "cd")

  (check
      (vector-drop '#() 0)
    => '#())

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(vector-drop "abcd" 5))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (vector-drop-right "abcd" 2)
    => "ab")

  (check
      (vector-drop-right '#() 0)
    => '#())

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(vector-drop-right "abcd" 5))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (vector-trim "aaabcd" #\a)
    => "bcd")

  (check
      (vector-trim "bcd" #\a)
    => "bcd")

  (check
      (vector-trim '#() #\a)
    => '#())

  (check
      (vector-trim "aaabcd" (char-set #\a #\b))
    => "cd")

  (check
      (vector-trim "bcd" (char-set #\a #\b))
    => "cd")

  (check
      (vector-trim '#() (char-set #\a #\b))
    => '#())

  (check
      (vector-trim "AAAbcd" char-upper-case?)
    => "bcd")

  (check
      (vector-trim "bcd" char-upper-case?)
    => "bcd")

  (check
      (vector-trim '#() char-upper-case?)
    => '#())

;;; --------------------------------------------------------------------

  (check
      (vector-trim-right "bcdaaa" #\a)
    => "bcd")

  (check
      (vector-trim-right "bcd" #\a)
    => "bcd")

  (check
      (vector-trim-right '#() #\a)
    => '#())

  (check
      (vector-trim-right "cdbaaa" (char-set #\a #\b))
    => "cd")

  (check
      (vector-trim-right "cdb" (char-set #\a #\b))
    => "cd")

  (check
      (vector-trim-right '#() (char-set #\a #\b))
    => '#())

  (check
      (vector-trim-right "bcdAAA" char-upper-case?)
    => "bcd")

  (check
      (vector-trim-right "bcd" char-upper-case?)
    => "bcd")

  (check
      (vector-trim-right '#() char-upper-case?)
    => '#())

;;; --------------------------------------------------------------------

  (check
      (vector-trim-both "aaabcdaaa" #\a)
    => "bcd")

  (check
      (vector-trim-both "bcd" #\a)
    => "bcd")

  (check
      (vector-trim-both '#() #\a)
    => '#())

  (check
      (vector-trim-both "aaabcdaa" (char-set #\a #\b))
    => "cd")

  (check
      (vector-trim-both "bcdb" (char-set #\a #\b))
    => "cd")

  (check
      (vector-trim-both '#() (char-set #\a #\b))
    => '#())

  (check
      (vector-trim-both "AAAbcdAAA" char-upper-case?)
    => "bcd")

  (check
      (vector-trim-both "bcd" char-upper-case?)
    => "bcd")

  (check
      (vector-trim-both '#() char-upper-case?)
    => '#())

;;; --------------------------------------------------------------------

  (check
      (vector-pad "abc" 3 #\0)
    => "abc")

  (check
      (vector-pad "abc" 5 #\0)
    => "00abc")

  (check
      (vector-pad "abc" 5)
    => "  abc")

  (check
      (vector-pad "abc" 2 #\0)
    => "bc")

  (check
      (vector-pad "abc" 0 #\0)
    => '#())

;;; --------------------------------------------------------------------

  (check
      (vector-pad-right "abc" 3 #\0)
    => "abc")

  (check
      (vector-pad-right "abc" 5 #\0)
    => "abc00")

  (check
      (vector-pad-right "abc" 2 #\0)
    => "ab")

  (check
      (vector-pad-right "abc" 0 #\0)
    => '#())

;;; --------------------------------------------------------------------

  (check
      (let* ((str (vector-copy "12")))
	(vector-copy*! str (view "abcd" (past 2)))
	str)
    => "ab")

  (check
      (let ((str '#()))
	(vector-copy*! str ("abcd" 0 0))
	str)
    => '#())

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(let* ((str (vector-copy "12")))
	  (vector-copy*! (str 3) (view "abcd" (past 2)))
	  str))
    => #t)

  )


(parameterise ((check-test-name 'prefix))

  (check
      (vector-prefix-length "abcdefg" "abcd123")
    => 4)

  (check
      (vector-prefix-length "aBcdefg" "abcd123")
    => 1)

  (check
      (vector-prefix-length "efg" "123")
    => 0)

  (check
      (vector-prefix-length "a" "a")
    => 1)

  (check
      (vector-prefix-length "1" "2")
    => 0)

  (check
      (vector-prefix-length '#() "abcd123")
    => 0)

  (check
      (vector-prefix-length "abcdefg" '#())
    => 0)

;;; --------------------------------------------------------------------

  (check
      (vector-suffix-length "efgabcd" "123abcd")
    => 4)

  (check
      (vector-suffix-length "efgabcd" "123abCd")
    => 1)

  (check
      (vector-suffix-length "efg" "123")
    => 0)

  (check
      (vector-suffix-length "a" "a")
    => 1)

  (check
      (vector-suffix-length "1" "2")
    => 0)

  (check
      (vector-suffix-length '#() "abcd123")
    => 0)

  (check
      (vector-suffix-length "abcdefg" '#())
    => 0)

;;; --------------------------------------------------------------------

  (check
      (vector-prefix-length-ci "aBcdefg" "aBcd123")
    => 4)

  (check
      (vector-prefix-length-ci "aBcdefg" "abcd123")
    => 4)

  (check
      (vector-prefix-length-ci "efg" "123")
    => 0)

  (check
      (vector-prefix-length-ci "a" "a")
    => 1)

  (check
      (vector-prefix-length-ci "1" "2")
    => 0)

  (check
      (vector-prefix-length-ci '#() "abcd123")
    => 0)

  (check
      (vector-prefix-length-ci "abcdefg" '#())
    => 0)

;;; --------------------------------------------------------------------

  (check
      (vector-suffix-length-ci "efgabCd" "123abCd")
    => 4)

  (check
      (vector-suffix-length-ci "efgabCd" "123abcd")
    => 4)

  (check
      (vector-suffix-length-ci "efg" "123")
    => 0)

  (check
      (vector-suffix-length-ci "a" "a")
    => 1)

  (check
      (vector-suffix-length-ci "1" "2")
    => 0)

  (check
      (vector-suffix-length-ci '#() "abcd123")
    => 0)

  (check
      (vector-suffix-length-ci "abcdefg" '#())
    => 0)

;;; --------------------------------------------------------------------

  (check
      (vector-prefix? "abcd" "abcd123")
    => #t)

  (check
      (vector-prefix? "abcd" "aBcd123")
    => #f)

  (check
      (vector-prefix? "efg" "123")
    => #f)

  (check
      (vector-prefix? '#() "123")
    => #t)

  (check
      (vector-prefix? "efg" '#())
    => #f)

  (check
      (vector-prefix? '#() '#())
    => #t)

;;; --------------------------------------------------------------------

  (check
      (vector-prefix-ci? "aBcd" "aBcd123")
    => #t)

  (check
      (vector-prefix-ci? "abcd" "aBcd123")
    => #t)

  (check
      (vector-prefix-ci? "efg" "123")
    => #f)

  (check
      (vector-prefix-ci? '#() "123")
    => #t)

  (check
      (vector-prefix-ci? "efg" '#())
    => #f)

  (check
      (vector-prefix-ci? '#() '#())
    => #t)

;;; --------------------------------------------------------------------

  (check
      (vector-suffix? "abcd" "123abcd")
    => #t)

  (check
      (vector-suffix? "abcd" "123aBcd")
    => #f)

  (check
      (vector-suffix? "efg" "123")
    => #f)

  (check
      (vector-suffix? '#() "123")
    => #t)

  (check
      (vector-suffix? "efg" '#())
    => #f)

  (check
      (vector-suffix? '#() '#())
    => #t)

;;; --------------------------------------------------------------------

  (check
      (vector-suffix-ci? "aBcd" "123aBcd")
    => #t)

  (check
      (vector-suffix-ci? "abcd" "123aBcd")
    => #t)

  (check
      (vector-suffix-ci? "efg" "123")
    => #f)

  (check
      (vector-suffix-ci? '#() "123")
    => #t)

  (check
      (vector-suffix-ci? "efg" '#())
    => #f)

  (check
      (vector-suffix-ci? '#() '#())
    => #t)

  )


(parameterise ((check-test-name 'searching))

  (check
      (vector-index "abcd" #\b)
    => 1)

  (check
      (vector-index ("abcd" 1) #\b)
    => 1)

  (check
      (vector-index "abcd" #\1)
    => #f)

  (check
      (vector-index '#() #\1)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (vector-index "abcd" (char-set #\b #\B))
    => 1)

  (check
      (vector-index ("abcd" 1) (char-set #\b #\B))
    => 1)

  (check
      (vector-index "abcd" (char-set #\0 #\1))
    => #f)

  (check
      (vector-index '#() (char-set #\0 #\1))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (vector-index "aBcd" char-upper-case?)
    => 1)

  (check
      (vector-index ("aBcd" 1) char-upper-case?)
    => 1)

  (check
      (vector-index "abcd" char-upper-case?)
    => #f)

  (check
      (vector-index '#() char-upper-case?)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (vector-index-right "abcd" #\b)
    => 1)

  (check
      (vector-index-right ("abcd" 1) #\b)
    => 1)

  (check
      (vector-index-right "abcd" #\1)
    => #f)

  (check
      (vector-index-right '#() #\1)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (vector-index-right "abcd" (char-set #\b #\B))
    => 1)

  (check
      (vector-index-right ("abcd" 1) (char-set #\b #\B))
    => 1)

  (check
      (vector-index-right "abcd" (char-set #\0 #\1))
    => #f)

  (check
      (vector-index-right '#() (char-set #\0 #\1))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (vector-index-right "aBcd" char-upper-case?)
    => 1)

  (check
      (vector-index-right ("aBcd" 1) char-upper-case?)
    => 1)

  (check
      (vector-index-right "abcd" char-upper-case?)
    => #f)

  (check
      (vector-index-right '#() char-upper-case?)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (vector-skip "bacd" #\b)
    => 1)

  (check
      (vector-skip ("bacd" 1) #\b)
    => 1)

  (check
      (vector-skip "1111" #\1)
    => #f)

  (check
      (vector-skip '#() #\1)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (vector-skip "bacd" (char-set #\b #\B))
    => 1)

  (check
      (vector-skip ("bacd" 1) (char-set #\b #\B))
    => 1)

  (check
      (vector-skip "1010" (char-set #\0 #\1))
    => #f)

  (check
      (vector-skip '#() (char-set #\0 #\1))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (vector-skip "Bacd" char-upper-case?)
    => 1)

  (check
      (vector-skip ("Bacd" 1) char-upper-case?)
    => 1)

  (check
      (vector-skip "ABCD" char-upper-case?)
    => #f)

  (check
      (vector-skip '#() char-upper-case?)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (vector-skip-right "acdb" #\b)
    => 2)

  (check
      (vector-skip-right ("acdb" 1) #\b)
    => 2)

  (check
      (vector-skip-right "1111" #\1)
    => #f)

  (check
      (vector-skip-right '#() #\1)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (vector-skip-right "acdb" (char-set #\b #\B))
    => 2)

  (check
      (vector-skip-right ("acdb" 1) (char-set #\b #\B))
    => 2)

  (check
      (vector-skip-right "0101" (char-set #\0 #\1))
    => #f)

  (check
      (vector-skip-right '#() (char-set #\0 #\1))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (vector-skip-right "acdB" char-upper-case?)
    => 2)

  (check
      (vector-skip-right ("acdB" 1) char-upper-case?)
    => 2)

  (check
      (vector-skip-right "ABCD" char-upper-case?)
    => #f)

  (check
      (vector-skip-right '#() char-upper-case?)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (vector-count "abcbd" #\b)
    => 2)

  (check
      (vector-count ("abcd" 1) #\b)
    => 1)

  (check
      (vector-count "abcd" #\1)
    => 0)

  (check
      (vector-count '#() #\1)
    => 0)

;;; --------------------------------------------------------------------

  (check
      (vector-count "abcBd" (char-set #\b #\B))
    => 2)

  (check
      (vector-count ("abcd" 1) (char-set #\b #\B))
    => 1)

  (check
      (vector-count "abcd" (char-set #\0 #\1))
    => 0)

  (check
      (vector-count '#() (char-set #\0 #\1))
    => 0)

;;; --------------------------------------------------------------------

  (check
      (vector-count "aBcAd" char-upper-case?)
    => 2)

  (check
      (vector-count ("aBcd" 1) char-upper-case?)
    => 1)

  (check
      (vector-count "abcd" char-upper-case?)
    => 0)

  (check
      (vector-count '#() char-upper-case?)
    => 0)

;;; --------------------------------------------------------------------

  (check
      (vector-contains "ciao hello salut" "hello")
    => 5)

  (check
      (vector-contains "ciao hello salut" "hola")
    => #f)

  (check
      (vector-contains "ciao hello salut" '#())
    => 0)

  (check
      (vector-contains '#() "hello")
    => #f)

;;; --------------------------------------------------------------------

  (check
      (vector-contains-ci "ciAO HELLO saLUT" "hello")
    => 5)

  (check
      (vector-contains-ci "ciao hello salut" "HOLA")
    => #f)

  (check
      (vector-contains-ci "ciao hello salut" '#())
    => 0)

  (check
      (vector-contains-ci '#() "hello")
    => #f)

  )


(parameterise ((check-test-name 'filtering))

  (check
      (vector-delete "abcbd" #\b)
    => "acd")

  (check
      (vector-delete "abcbd" #\0)
    => "abcbd")

  (check
      (vector-delete '#() #\b)
    => '#())

  (check
      (vector-delete "bbb" #\b)
    => '#())

;;; --------------------------------------------------------------------

  (check
      (vector-delete "abcbd" (char-set #\b #\B))
    => "acd")

  (check
      (vector-delete "abcbd" (char-set #\0 #\1))
    => "abcbd")

  (check
      (vector-delete '#() (char-set #\b #\B))
    => '#())

  (check
      (vector-delete "BbB" (char-set #\b #\B))
    => '#())

;;; --------------------------------------------------------------------

  (check
      (vector-delete "aBcBd" char-upper-case?)
    => "acd")

  (check
      (vector-delete "abcbd" char-upper-case?)
    => "abcbd")

  (check
      (vector-delete '#() char-upper-case?)
    => '#())

;;; --------------------------------------------------------------------

  (check
      (vector-filter "abcbd" #\b)
    => "bb")

  (check
      (vector-filter "abcbd" #\0)
    => '#())

  (check
      (vector-filter '#() #\b)
    => '#())

;;; --------------------------------------------------------------------

  (check
      (vector-filter "abcbd" (char-set #\b #\B))
    => "bb")

  (check
      (vector-filter "abcbd" (char-set #\0 #\1))
    => '#())

  (check
      (vector-filter '#() (char-set #\b #\B))
    => '#())

;;; --------------------------------------------------------------------

  (check
      (vector-filter "aBcBd" char-upper-case?)
    => "BB")

  (check
      (vector-filter "abcbd" char-upper-case?)
    => '#())

  (check
      (vector-filter '#() char-upper-case?)
    => '#())

  )


(parameterise ((check-test-name 'lists))

  (check
      (vector->list* "abcd")
    => '(#\a #\b #\c #\d))

  (check
      (vector->list* ("abcd" 1 3))
    => '(#\b #\c))

  (check
      (vector->list* '#())
    => '())

;;; --------------------------------------------------------------------

  (check
      (reverse-list->vector '(#\a #\b #\c #\d))
    => "dcba")

  (check
      (reverse-list->vector '())
    => '#())

  )

;;; --------------------------------------------------------------------

(parameterise ((check-test-name 'tokenize))

  (check
      (vector-tokenize "ciao hello salut"
		       (char-set #\a #\c #\e #\i #\h #\l #\o #\s #\t #\u))
    => '("ciao" "hello" "salut"))

  (check
      (vector-tokenize '#() (char-set #\a #\c #\e #\i #\h #\l #\o #\s #\t #\u))
    => '())

  (check
      (vector-tokenize "ciao hello salut" (char-set))
    => '())

  )

;;; --------------------------------------------------------------------

(parameterise ((check-test-name 'join))

  (check
      (vector-join '("c" "i" "a" "o") "," 'infix)
    => "c,i,a,o")

  (check
      (vector-join '("c" "i" "a" "o") "," 'strict-infix)
    => "c,i,a,o")

  (check
      (vector-join '("c" "i" "a" "o") "," 'suffix)
    => "c,i,a,o,")

  (check
      (vector-join '("c" "i" "a" "o") "," 'prefix)
    => ",c,i,a,o")

;;; --------------------------------------------------------------------

  (check
      (vector-join '() "," 'infix)
    => '#())

  (check
      (guard (exc ((assertion-violation? exc)
		   #t))
	(vector-join '() "," 'strict-infix))
    => #t)

  (check
      (vector-join '() "," 'suffix)
    => '#())

  (check
      (vector-join '() "," 'prefix)
    => '#())

;;; --------------------------------------------------------------------

  (check
      (vector-join '("c") "," 'infix)
    => "c")

  (check
      (vector-join '("c") "," 'strict-infix)
    => "c")

  (check
      (vector-join '("c") "," 'suffix)
    => "c,")

  (check
      (vector-join '("c") "," 'prefix)
    => ",c")

;;; --------------------------------------------------------------------

  (check
      (vector-join '('#()) "," 'infix)
    => '#())

  (check
      (vector-join '('#()) "," 'strict-infix)
    => '#())

  (check
      (vector-join '('#()) "," 'suffix)
    => ",")

  (check
      (vector-join '('#()) "," 'prefix)
    => ",")

;;; --------------------------------------------------------------------

  (check
      (vector-join '("c" "i" "a" "o") '#() 'infix)
    => "ciao")

  (check
      (vector-join '("c" "i" "a" "o") '#() 'strict-infix)
    => "ciao")

  (check
      (vector-join '("c" "i" "a" "o") '#() 'suffix)
    => "ciao")

  (check
      (vector-join '("c" "i" "a" "o") '#() 'prefix)
    => "ciao")

;;; --------------------------------------------------------------------

  (check
      (vector-join '("c" "i" "a" "o") ",;;" 'infix)
    => "c,;;i,;;a,;;o")

  (check
      (vector-join '("c" "i" "a" "o") ",;;" 'strict-infix)
    => "c,;;i,;;a,;;o")

  (check
      (vector-join '("c" "i" "a" "o") ",;;" 'suffix)
    => "c,;;i,;;a,;;o,;;")

  (check
      (vector-join '("c" "i" "a" "o") ",;;" 'prefix)
    => ",;;c,;;i,;;a,;;o")

  )


(parameterise ((check-test-name 'xsubvector))

  (check
      (xsubvector "ciao " 0 5)
    => "ciao ")

  (check
      (xsubvector "ciao " 0 9)
    => "ciao ciao")

  (check
      (xsubvector "ciao " -5 5)
    => "ciao ciao ")

  (check
      (xsubvector "ciao " 2 4)
    => "ao")

  (check
      (xsubvector "ciao " -3 7)
    => "ao ciao ci")

  (check (xsubvector "abcdef" 1 7) => "bcdefa")
  (check (xsubvector "abcdef" 2 8) => "cdefab")
  (check (xsubvector "abcdef" 3 9) => "defabc")
  (check (xsubvector "abcdef" 4 10) => "efabcd")
  (check (xsubvector "abcdef" 5 11) => "fabcde")

  (check (xsubvector "abcdef" -1 5) => "fabcde")
  (check (xsubvector "abcdef" -2 4) => "efabcd")
  (check (xsubvector "abcdef" -3 3) => "defabc")
  (check (xsubvector "abcdef" -4 2) => "cdefab")
  (check (xsubvector "abcdef" -5 1) => "bcdefa")

  (check
      (xsubvector "ciao " 3 3)
    => '#())

  (check
      (guard (exc ((assertion-violation? exc)
		   #t))
	(xsubvector '#() 0 5))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let ((result (vector-copy "01234")))
	(vector-xcopy! result "ciao " 0 5)
	result)
    => "ciao ")

  (check
      (let ((result (vector-copy "012345678")))
	(vector-xcopy! result "ciao " 0 9)
	result)
    => "ciao ciao")

  (check
      (let ((result (vector-copy "0123456789")))
	(vector-xcopy! result "ciao " -5 5)
	result)
    => "ciao ciao ")

  (check
      (let ((result (vector-copy "01")))
	(vector-xcopy! result "ciao " 2 4)
	result)
    => "ao")

  (check
      (let ((result (vector-copy "0123456789")))
	(vector-xcopy! result "ciao " -3 7)
	result)
    => "ao ciao ci")

  (check
      (guard (exc ((assertion-violation? exc) #t))
	  (vector-xcopy! '#() '#() 0 5))
    => #t)

  )


(parameterise ((check-test-name 'filling))

  (check
      (let* ((str (vector-copy "abcd")))
	(vector-fill*! str #\b)
	str)
    => "bbbb")

  (check
      (let* ((str (vector-copy "accd")))
	(vector-fill*! (str 1 3) #\b)
	str)
    => "abbd")

  (check
      (let* ((str (vector-copy '#())))
	(vector-fill*! (str 0 0) #\b)
	str)
    => '#())

  )


(parameterise ((check-test-name 'reverse))

  (check
      (vector-reverse "abcd")
    => "dcba")

  (check
      (vector-reverse '#())
    => '#())

;;; --------------------------------------------------------------------

  (check
      (let* ((str (vector-copy "abcd")))
	(vector-reverse! str)
	str)
    => "dcba")

  (check
      (let* ((str (vector-copy '#())))
	(vector-reverse! str)
	str)
    => '#())

  )


(parameterise ((check-test-name 'concatenate))

  (check
      (vector-concatenate '("ciao" " " "hello" " " "salut"))
    => "ciao hello salut")

  (check
      (vector-concatenate '())
    => '#())

;;; --------------------------------------------------------------------

  (check
      (vector-concatenate-reverse '("ciao" " " "hello" " " "salut") " hola" 3)
    => "salut hello ciao ho")

  (check
      (vector-concatenate-reverse '("ciao" " " "hello" " " "salut") " hola")
    => "salut hello ciao hola")

  (check
      (vector-concatenate-reverse '("ciao" " " "hello" " " "salut"))
    => "salut hello ciao")

  (check
      (vector-concatenate-reverse '())
    => '#())

  )


(parameterise ((check-test-name 'replace))

  (check
      (vector-replace "abcd" "1234")
    => "1234")

  (check
      (vector-replace ("abcd" 2 2) "1234")
    => "ab1234cd")

  (check
      (vector-replace ("abcd" 2 2) '#())
    => "abcd")

  (check
      (vector-replace ("abcd" 1 3) "1234")
    => "a1234d")

  (check
      (vector-replace ("abcd" 0 3) "1234")
    => "1234d")

  (check
      (vector-replace ("abcd" 1 4) "1234")
    => "a1234")

  )


;;;; done

(check-report)

;;; end of file
