;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for the strings library
;;;Date: Fri Jun  5, 2009
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
  (char-sets)
  (checks)
  (rnrs mutable-strings))

(check-set-mode! 'report-failed)
(display "*** testing strings\n")


(parameterise ((check-test-name 'views))

  (check
      (substring* "ciao")
    => "ciao")

  (check
      (substring* ("ciao"))
    => "ciao")

  (check
      (substring* ("ciao" 2))
    => "ao")

  (check
      (substring* ("ciao" 0 4))
    => "ciao")

  (check
      (substring* ("ciao" 0 0))
    => "")

  (check
      (substring* ("ciao" 1 1))
    => "")

  (check
      (substring* ("ciao" 0 1))
    => "c")

;;; --------------------------------------------------------------------

  (check
      (substring* (view "ciao"))
    => "ciao")

  (check
      (substring* (view "ciao" (start 2)))
    => "ao")

  (check
      (substring* (view "ciao" (start 0) (past 4)))
    => "ciao")

  (check
      (substring* (view "ciao" (start 0) (past 0)))
    => "")

  (check
      (substring* (view "ciao" (start 1) (past 1)))
    => "")

  (check
      (substring* (view "ciao" (start 0) (past 1)))
    => "c")

  (check
      (substring* (view "ciao" (past 2)))
    => "ci")

  )


(parameterise ((check-test-name 'predicates))

  (check
      (string-null? "ciao")
    => #f)

  (check
      (string-null? "")
    => #t)

;;; --------------------------------------------------------------------

  (check
      (guard (exc ((assertion-violation? exc)
		   (condition-who exc)))
	(string-every 123 "abc"))
    => '%string-every)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aaaa"))
	(string-every #\a str))
    => #t)

  (check
      (let* ((str "aaaab"))
	(string-every #\a str))
    => #f)

  (check
      (let* ((str "aabaa"))
	(string-every #\a str))
    => #f)

  (check
      (let* ((str ""))
	(string-every #\a str))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aaaa"))
	(string-every (char-set #\a) str))
    => #t)

  (check
      (let* ((str "aaaab"))
	(string-every (char-set #\a) str))
    => #f)

  (check
      (let* ((str "aabaa"))
	(string-every (char-set #\a) str))
    => #f)

  (check
      (let* ((str ""))
	(string-every (char-set #\a) str))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aaaa"))
	(string-every char-alphabetic? str))
    => #t)

  (check
      (let* ((str "aaaa2"))
	(string-every char-alphabetic? str))
    => #f)

  (check
      (let* ((str "aa2aa"))
	(string-every char-alphabetic? str))
    => #f)

  (check
      (let* ((str ""))
	(string-every char-alphabetic? str))
    => #f)

  (check
      (let* ((str "1234"))
	(string-every (lambda (x) x) str))
    => #\4)

;;; --------------------------------------------------------------------

  (check
      (guard (exc ((assertion-violation? exc)
		   (condition-who exc)))
	(string-any 123 "abc"))
    => '%string-any)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "ddadd"))
	(string-any #\a str))
    => #t)

  (check
      (let* ((str "dddda"))
	(string-any #\a str))
    => #t)

  (check
      (let* ((str "ddd"))
	(string-any #\a str))
    => #f)

  (check
      (let* ((str ""))
	(string-any #\a str))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "dddaddd"))
	(string-any (char-set #\a) str))
    => #t)

  (check
      (let* ((str "ddda"))
	(string-any (char-set #\a) str))
    => #t)

  (check
      (let* ((str "dddd"))
	(string-any (char-set #\a) str))
    => #f)

  (check
      (let* ((str ""))
	(string-any (char-set #\a) str))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "11a11"))
	(string-any char-alphabetic? str))
    => #t)

  (check
      (let* ((str "11111a"))
	(string-any char-alphabetic? str))
    => #t)

  (check
      (let* ((str "1111"))
	(string-any char-alphabetic? str))
    => #f)

  (check
      (let* ((str ""))
	(string-any char-alphabetic? str))
    => #f)

  (check
      (let* ((str "1234"))
	(string-any (lambda (x) x) str))
    => #\1)

  )


(parameterise ((check-test-name 'comparison-case-sensitive))

  (check
      (string-compare "abcdefg" "abcd123" values values values)
    => 4)

  (check
      (string-compare "abcdef" "abcd123" values values values)
    => 4)

  (check
      (string-compare "efg" "123" values values values)
    => 0)

  (check
      (string-compare "" "abcd" values values values)
    => 0)

  (check
      (string-compare "abcd" "" values values values)
    => 0)

  (check
      (string-compare "abcdA" "abcdA"
		      (lambda (idx) 'less)
		      (lambda (idx) 'equal)
		      (lambda (idx) 'greater))
    => 'equal)

  (check
      (string-compare "abcdA" "abcdB"
		      (lambda (idx) 'less)
		      (lambda (idx) 'equal)
		      (lambda (idx) 'greater))
    => 'less)

  (check
      (string-compare "abcdB" "abcdA"
		      (lambda (idx) 'less)
		      (lambda (idx) 'equal)
		      (lambda (idx) 'greater))
    => 'greater)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "abcd"))
	(string= str str))
    => #t)

  (check
      (string= ("12abcd" 2) "abcd")
    => 6)

  (check
      (string= "abc" "abcd")
    => #f)

  (check
      (string= "abcd" "abc")
    => #f)

  (check
      (string= "ABcd" "abcd")
    => #f)

  (check
      (string= "abcd" "a2cd")
    => #f)

;;; --------------------------------------------------------------------

  (check
      (string<> "abcd" "abcd")
    => #f)

  (check
      (string<> "abc" "abcd")
    => #t)

  (check
      (string<> "abcd" "abc")
    => #t)

  (check
      (string<> "ABcd" "abcd")
    => 0)

  (check
      (string<> "abcd" "a2cd")
    => 1)

;;; --------------------------------------------------------------------

  (check
      (string< "abcd" "abcd")
    => #f)

  (check
      (string< "abc" "abcd")
    => 3)

  (check
      (string< "abcd" "abc")
    => #f)

  (check
      (string< "ABcd" "abcd")
    => 0)

  (check
      (string< "abcd" "a2cd")
    => #f)

;;; --------------------------------------------------------------------

  (cond-expand
   (ikarus (check
	       (string<= "abcd" "abcd")
	     => 4))
   (else (check
	     (string<= "abcd" "abcd")
	   => #t)))

  (check
      (string<= "abc" "abcd")
    => 3)

  (check
      (string<= "abcd" "abc")
    => #f)

  (check
      (string<= "ABcd" "abcd")
    => 0)

  (check
      (string<= "abcd" "a2cd")
    => #f)

;;; --------------------------------------------------------------------

  (check
      (string> "abcd" "abcd")
    => #f)

  (check
      (string> "abcd" "abc")
    => 3)

  (check
      (string> "abc" "abcd")
    => #f)

  (check
      (string> "abcd" "ABcd")
    => 0)

  (check
      (string> "a2cd" "abcd")
    => #f)

;;; --------------------------------------------------------------------

  (cond-expand
   (ikarus (check
	       (string>= "abcd" "abcd")
	     => 4))
   (else (check
	     (string>= "abcd" "abcd")
	   => #t)))

  (check
      (string>= "abcd" "abc")
    => 3)

  (check
      (string>= "abc" "abcd")
    => #f)

  (check
      (string>= "abcd" "ABcd")
    => 0)

  (check
      (string>= "a2cd" "abcd")
    => #f)

  )


(parameterise ((check-test-name 'comparison-case-insensitive))

  (check
      (string-compare-ci "aBcdefg" "abcd123" values values values)
    => 4)

  (check
      (string-compare-ci "efg" "123" values values values)
    => 0)

  (check
      (string-compare-ci "" "abcd" values values values)
    => 0)

  (check
      (string-compare-ci "abcd" "" values values values)
    => 0)

  (check
      (string-compare-ci "abcdA" "abcda"
			 (lambda (idx) 'less) (lambda (idx) 'equal) (lambda (idx) 'greater))
    => 'equal)

  (check
      (string-compare-ci "abcdA" "abcdb"
			 (lambda (idx) 'less) (lambda (idx) 'equal) (lambda (idx) 'greater))
    => 'less)

  (check
      (string-compare-ci "abcdb" "abcdA"
			 (lambda (idx) 'less) (lambda (idx) 'equal) (lambda (idx) 'greater))
    => 'greater)

;;; --------------------------------------------------------------------

  (check
      (string-ci= "abcd" "abcd")
    => #t)

  (check
      (string-ci= ("12abcd" 2) "abcd")
    => 6)

  (check
      (string-ci= "abc" "abcd")
    => #f)

  (check
      (string-ci= "abcd" "abc")
    => #f)

  (check
      (string-ci= "ABcd" "abcd")
    => 4)

  (check
      (string-ci= "abcd" "a2cd")
    => #f)

;;; --------------------------------------------------------------------

  (check
      (string-ci<> "abcd" "abcd")
    => #f)

  (check
      (string-ci<> "abc" "abcd")
    => #t)

  (check
      (string-ci<> "abcd" "abc")
    => #t)

  (check
      (string-ci<> "ABcd" "abcd")
    => #f)

  (check
      (string-ci<> "abcd" "a2cd")
    => 1)

;;; --------------------------------------------------------------------

  (check
      (string-ci< "abcd" "abcd")
    => #f)

  (check
      (string-ci< "abc" "abcd")
    => 3)

  (check
      (string-ci< "abcd" "abc")
    => #f)

  (check
      (string-ci< "ABcd" "abcd")
    => #f)

  (check
      (string-ci< "abcd" "a2cd")
    => #f)

;;; --------------------------------------------------------------------

  (cond-expand
   (ikarus (check
	       (string-ci<= "abcd" "abcd")
	     => 4))
   (else (check
	     (string-ci<= "abcd" "abcd")
	   => #t)))

  (check
      (string-ci<= "abc" "abcd")
    => 3)

  (check
      (string-ci<= "abcd" "abc")
    => #f)

  (check
      (string-ci<= "ABcd" "abcd")
    => 4)

  (check
      (string-ci<= "abcd" "a2cd")
    => #f)

;;; --------------------------------------------------------------------

  (check
      (string-ci> "abcd" "abcd")
    => #f)

  (check
      (string-ci> "abcd" "abc")
    => 3)

  (check
      (string-ci> "abc" "abcd")
    => #f)

  (check
      (string-ci> "abcd" "ABcd")
    => #f)

  (check
      (string-ci> "a2cd" "abcd")
    => #f)

;;; --------------------------------------------------------------------

  (cond-expand
   (ikarus (check
	       (string-ci>= "abcd" "abcd")
	     => 4))
   (else (check
	     (string-ci>= "abcd" "abcd")
	   => #t)))

  (check
      (string-ci>= "abcd" "abc")
    => 3)

  (check
      (string-ci>= "abc" "abcd")
    => #f)

  (check
      (string-ci>= "abcd" "ABcd")
    => 4)

  (check
      (string-ci>= "a2cd" "abcd")
    => #f)

  )


(parameterise ((check-test-name 'mapping))

  (check
      (string-map char-upcase "aaaa")
    => "AAAA")

  (check
      (string-map char-upcase "")
    => "")

;;; --------------------------------------------------------------------

  (check
      (let ((str (string-copy "aaaa")))
	(string-map! char-upcase str)
	str)
    => "AAAA")

  (check
      (let* ((str ""))
	(string-map! char-upcase str)
	str)
    => "")

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aaaa")
	     (result ""))
	(string-for-each*
	 (lambda (ch)
	   (set! result
		 (string-append result
				(number->string (char->integer (char-upcase ch))))))
	 str)
	result)
    => "65656565")

  (check
      (let* ((str "")
	     (result ""))
	(string-for-each*
	 (lambda (ch)
	   (set! result
		 (string-append result
				(number->string (char->integer (char-upcase ch))))))
	 str)
	result)
    => "")

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aaaa")
	     (result '()))
	(string-for-each-index
	 (lambda (idx)
	   (set! result (cons idx result)))
	 str)
	result)
    => '(3 2 1 0))

  (check
      (let* ((str "")
	     (result '()))
	(string-for-each-index
	 (lambda (idx)
	   (set! result (cons idx result)))
	 str)
	result)
    => '())

  )


(parameterise ((check-test-name 'case))

  (check
      (string-upcase* "abcd")
    => "ABCD")

  (check
      (string-upcase* "123abcd")
    => "123ABCD")

  (check
      (string-upcase* "---abcd")
    => "---ABCD")

  (check
      (string-upcase* "abcd efgh")
    => "ABCD EFGH")

;;; --------------------------------------------------------------------

  (check
      (let* ((str (string-copy "abcd")))
	(string-upcase*! str)
	str)
    => "ABCD")

  (check
      (let* ((str (string-copy "123abcd")))
	(string-upcase*! str)
	str)
    => "123ABCD")

  (check
      (let* ((str (string-copy "---abcd")))
	(string-upcase*! str)
	str)
    => "---ABCD")

  (check
      (let* ((str (string-copy "abcd efgh")))
	(string-upcase*! str)
	str)
    => "ABCD EFGH")

;;; --------------------------------------------------------------------

  (check
      (string-downcase* "ABCD")
    => "abcd")

  (check
      (string-downcase* "123AbcD")
    => "123abcd")

  (check
      (string-downcase* "---aBCd")
    => "---abcd")

  (check
      (string-downcase* "abcd EFGH")
    => "abcd efgh")

;;; --------------------------------------------------------------------

  (check
      (let* ((str (string-copy "aBCd")))
	(string-downcase*! str)
	str)
    => "abcd")

  (check
      (let* ((str (string-copy "123ABcd")))
	(string-downcase*! str)
	str)
    => "123abcd")

  (check
      (let* ((str (string-copy "---aBCD")))
	(string-downcase*! str)
	str)
    => "---abcd")

  (check
      (let* ((str (string-copy "abCD Efgh")))
	(string-downcase*! str)
	str)
    => "abcd efgh")

;;; --------------------------------------------------------------------

  (check
      (string-titlecase* "abcd")
    => "Abcd")

  (check
      (string-titlecase* "123abcd")
    => "123Abcd")

  (check
      (string-titlecase* "---abcd")
    => "---Abcd")

  (check
      (string-titlecase* "abcd efgh")
    => "Abcd Efgh")

  (check
      (string-titlecase* ("greasy fried chicken" 2))
    => "Easy Fried Chicken")

;;; --------------------------------------------------------------------

  (check
      (let* ((str (string-copy "abcd")))
	(string-titlecase*! str)
	str)
    => "Abcd")

  (check
      (let* ((str (string-copy "123abcd")))
	(string-titlecase*! str)
	str)
    => "123Abcd")

  (check
      (let* ((str (string-copy "---abcd")))
	(string-titlecase*! str)
	str)
    => "---Abcd")

  (check
      (let* ((str (string-copy "abcd efgh")))
	(string-titlecase*! str)
	str)
    => "Abcd Efgh")

  (check
      (let ((str (string-copy "greasy fried chicken")))
	(string-titlecase*! (str 2))
	str)
    => "grEasy Fried Chicken")

  )


(parameterise ((check-test-name 'folding))

  (check
      (string-fold cons '() "abcd")
    => '(#\d #\c #\b #\a))

  (check
      (string-fold cons '() "")
    => '())

  (check
      (string-fold (lambda (c count)
		     (if (char-upper-case? c)
			 (+ count 1)
		       count))
		   0
		   "ABCdefGHi")
    => 5)

  (check
      (let* ((str "abc\\de\\f\\ghi")
	     (ans-len (string-fold
		       (lambda (c sum)
			 (+ sum (if (char=? c #\\) 2 1)))
		       0 str))
	     (ans (make-string ans-len)))
	(string-fold
	 (lambda (c i)
	   (let ((i (if (char=? c #\\)
			(begin
			  (string-set! ans i #\\)
			  (+ i 1))
		      i)))
	     (string-set! ans i c)
	     (+ i 1)))
	 0 str)
	ans)
    => "abc\\\\de\\\\f\\\\ghi")

;;; --------------------------------------------------------------------

  (check
      (string-fold-right cons '() "abcd")
    => '(#\a #\b #\c #\d))

  (check
      (string-fold-right cons '() "")
    => '())

;;; --------------------------------------------------------------------

  (check
      (string-unfold null? car cdr '(#\a #\b #\c #\d))
    => "abcd")

  (check
      (string-unfold null? car cdr '())
    => "")

;;; --------------------------------------------------------------------

  (check
      (string-unfold-right null? car cdr '(#\a #\b #\c #\d))
    => "dcba")

  (check
      (string-unfold-right null? car cdr '())
    => "")

;;; --------------------------------------------------------------------

  (check
      (string-tabulate (lambda (idx) (integer->char (+ 65 idx))) 4)
    => "ABCD")

  (check
      (string-tabulate integer->char 0)
    => "")

  )


(parameterise ((check-test-name 'selecting))

  (check
      (string-take "abcd" 2)
    => "ab")

  (check
      (string-take "" 0)
    => "")

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(string-take "abcd" 5))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (string-take-right "abcd" 2)
    => "cd")

  (check
      (string-take-right "" 0)
    => "")

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(string-take-right "abcd" 5))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (string-drop "abcd" 2)
    => "cd")

  (check
      (string-drop "" 0)
    => "")

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(string-drop "abcd" 5))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (string-drop-right "abcd" 2)
    => "ab")

  (check
      (string-drop-right "" 0)
    => "")

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(string-drop-right "abcd" 5))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (string-trim "aaabcd" #\a)
    => "bcd")

  (check
      (string-trim "bcd" #\a)
    => "bcd")

  (check
      (string-trim "" #\a)
    => "")

  (check
      (string-trim "aaabcd" (char-set #\a #\b))
    => "cd")

  (check
      (string-trim "bcd" (char-set #\a #\b))
    => "cd")

  (check
      (string-trim "" (char-set #\a #\b))
    => "")

  (check
      (string-trim "AAAbcd" char-upper-case?)
    => "bcd")

  (check
      (string-trim "bcd" char-upper-case?)
    => "bcd")

  (check
      (string-trim "" char-upper-case?)
    => "")

;;; --------------------------------------------------------------------

  (check
      (string-trim-right "bcdaaa" #\a)
    => "bcd")

  (check
      (string-trim-right "bcd" #\a)
    => "bcd")

  (check
      (string-trim-right "" #\a)
    => "")

  (check
      (string-trim-right "cdbaaa" (char-set #\a #\b))
    => "cd")

  (check
      (string-trim-right "cdb" (char-set #\a #\b))
    => "cd")

  (check
      (string-trim-right "" (char-set #\a #\b))
    => "")

  (check
      (string-trim-right "bcdAAA" char-upper-case?)
    => "bcd")

  (check
      (string-trim-right "bcd" char-upper-case?)
    => "bcd")

  (check
      (string-trim-right "" char-upper-case?)
    => "")

;;; --------------------------------------------------------------------

  (check
      (string-trim-both "aaabcdaaa" #\a)
    => "bcd")

  (check
      (string-trim-both "bcd" #\a)
    => "bcd")

  (check
      (string-trim-both "" #\a)
    => "")

  (check
      (string-trim-both "aaabcdaa" (char-set #\a #\b))
    => "cd")

  (check
      (string-trim-both "bcdb" (char-set #\a #\b))
    => "cd")

  (check
      (string-trim-both "" (char-set #\a #\b))
    => "")

  (check
      (string-trim-both "AAAbcdAAA" char-upper-case?)
    => "bcd")

  (check
      (string-trim-both "bcd" char-upper-case?)
    => "bcd")

  (check
      (string-trim-both "" char-upper-case?)
    => "")

;;; --------------------------------------------------------------------

  (check
      (string-pad "abc" 3 #\0)
    => "abc")

  (check
      (string-pad "abc" 5 #\0)
    => "00abc")

  (check
      (string-pad "abc" 5)
    => "  abc")

  (check
      (string-pad "abc" 2 #\0)
    => "bc")

  (check
      (string-pad "abc" 0 #\0)
    => "")

;;; --------------------------------------------------------------------

  (check
      (string-pad-right "abc" 3 #\0)
    => "abc")

  (check
      (string-pad-right "abc" 5 #\0)
    => "abc00")

  (check
      (string-pad-right "abc" 2 #\0)
    => "ab")

  (check
      (string-pad-right "abc" 0 #\0)
    => "")

;;; --------------------------------------------------------------------

  (check
      (let* ((str (string-copy "12")))
	(string-copy*! str (view "abcd" (past 2)))
	str)
    => "ab")

  (check
      (let ((str ""))
	(string-copy*! str ("abcd" 0 0))
	str)
    => "")

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(let* ((str (string-copy "12")))
	  (string-copy*! (str 3) (view "abcd" (past 2)))
	  str))
    => #t)

  )


(parameterise ((check-test-name 'prefix))

  (check
      (string-prefix-length "abcdefg" "abcd123")
    => 4)

  (check
      (string-prefix-length "aBcdefg" "abcd123")
    => 1)

  (check
      (string-prefix-length "efg" "123")
    => 0)

  (check
      (string-prefix-length "a" "a")
    => 1)

  (check
      (string-prefix-length "1" "2")
    => 0)

  (check
      (string-prefix-length "" "abcd123")
    => 0)

  (check
      (string-prefix-length "abcdefg" "")
    => 0)

;;; --------------------------------------------------------------------

  (check
      (string-suffix-length "efgabcd" "123abcd")
    => 4)

  (check
      (string-suffix-length "efgabcd" "123abCd")
    => 1)

  (check
      (string-suffix-length "efg" "123")
    => 0)

  (check
      (string-suffix-length "a" "a")
    => 1)

  (check
      (string-suffix-length "1" "2")
    => 0)

  (check
      (string-suffix-length "" "abcd123")
    => 0)

  (check
      (string-suffix-length "abcdefg" "")
    => 0)

;;; --------------------------------------------------------------------

  (check
      (string-prefix-length-ci "aBcdefg" "aBcd123")
    => 4)

  (check
      (string-prefix-length-ci "aBcdefg" "abcd123")
    => 4)

  (check
      (string-prefix-length-ci "efg" "123")
    => 0)

  (check
      (string-prefix-length-ci "a" "a")
    => 1)

  (check
      (string-prefix-length-ci "1" "2")
    => 0)

  (check
      (string-prefix-length-ci "" "abcd123")
    => 0)

  (check
      (string-prefix-length-ci "abcdefg" "")
    => 0)

;;; --------------------------------------------------------------------

  (check
      (string-suffix-length-ci "efgabCd" "123abCd")
    => 4)

  (check
      (string-suffix-length-ci "efgabCd" "123abcd")
    => 4)

  (check
      (string-suffix-length-ci "efg" "123")
    => 0)

  (check
      (string-suffix-length-ci "a" "a")
    => 1)

  (check
      (string-suffix-length-ci "1" "2")
    => 0)

  (check
      (string-suffix-length-ci "" "abcd123")
    => 0)

  (check
      (string-suffix-length-ci "abcdefg" "")
    => 0)

;;; --------------------------------------------------------------------

  (check
      (string-prefix? "abcd" "abcd123")
    => #t)

  (check
      (string-prefix? "abcd" "aBcd123")
    => #f)

  (check
      (string-prefix? "efg" "123")
    => #f)

  (check
      (string-prefix? "" "123")
    => #t)

  (check
      (string-prefix? "efg" "")
    => #f)

  (check
      (string-prefix? "" "")
    => #t)

;;; --------------------------------------------------------------------

  (check
      (string-prefix-ci? "aBcd" "aBcd123")
    => #t)

  (check
      (string-prefix-ci? "abcd" "aBcd123")
    => #t)

  (check
      (string-prefix-ci? "efg" "123")
    => #f)

  (check
      (string-prefix-ci? "" "123")
    => #t)

  (check
      (string-prefix-ci? "efg" "")
    => #f)

  (check
      (string-prefix-ci? "" "")
    => #t)

;;; --------------------------------------------------------------------

  (check
      (string-suffix? "abcd" "123abcd")
    => #t)

  (check
      (string-suffix? "abcd" "123aBcd")
    => #f)

  (check
      (string-suffix? "efg" "123")
    => #f)

  (check
      (string-suffix? "" "123")
    => #t)

  (check
      (string-suffix? "efg" "")
    => #f)

  (check
      (string-suffix? "" "")
    => #t)

;;; --------------------------------------------------------------------

  (check
      (string-suffix-ci? "aBcd" "123aBcd")
    => #t)

  (check
      (string-suffix-ci? "abcd" "123aBcd")
    => #t)

  (check
      (string-suffix-ci? "efg" "123")
    => #f)

  (check
      (string-suffix-ci? "" "123")
    => #t)

  (check
      (string-suffix-ci? "efg" "")
    => #f)

  (check
      (string-suffix-ci? "" "")
    => #t)

  )


(parameterise ((check-test-name 'searching))

  (check
      (string-index "abcd" #\b)
    => 1)

  (check
      (string-index ("abcd" 1) #\b)
    => 1)

  (check
      (string-index "abcd" #\1)
    => #f)

  (check
      (string-index "" #\1)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (string-index "abcd" (char-set #\b #\B))
    => 1)

  (check
      (string-index ("abcd" 1) (char-set #\b #\B))
    => 1)

  (check
      (string-index "abcd" (char-set #\0 #\1))
    => #f)

  (check
      (string-index "" (char-set #\0 #\1))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (string-index "aBcd" char-upper-case?)
    => 1)

  (check
      (string-index ("aBcd" 1) char-upper-case?)
    => 1)

  (check
      (string-index "abcd" char-upper-case?)
    => #f)

  (check
      (string-index "" char-upper-case?)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (string-index-right "abcd" #\b)
    => 1)

  (check
      (string-index-right ("abcd" 1) #\b)
    => 1)

  (check
      (string-index-right "abcd" #\1)
    => #f)

  (check
      (string-index-right "" #\1)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (string-index-right "abcd" (char-set #\b #\B))
    => 1)

  (check
      (string-index-right ("abcd" 1) (char-set #\b #\B))
    => 1)

  (check
      (string-index-right "abcd" (char-set #\0 #\1))
    => #f)

  (check
      (string-index-right "" (char-set #\0 #\1))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (string-index-right "aBcd" char-upper-case?)
    => 1)

  (check
      (string-index-right ("aBcd" 1) char-upper-case?)
    => 1)

  (check
      (string-index-right "abcd" char-upper-case?)
    => #f)

  (check
      (string-index-right "" char-upper-case?)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (string-skip "bacd" #\b)
    => 1)

  (check
      (string-skip ("bacd" 1) #\b)
    => 1)

  (check
      (string-skip "1111" #\1)
    => #f)

  (check
      (string-skip "" #\1)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (string-skip "bacd" (char-set #\b #\B))
    => 1)

  (check
      (string-skip ("bacd" 1) (char-set #\b #\B))
    => 1)

  (check
      (string-skip "1010" (char-set #\0 #\1))
    => #f)

  (check
      (string-skip "" (char-set #\0 #\1))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (string-skip "Bacd" char-upper-case?)
    => 1)

  (check
      (string-skip ("Bacd" 1) char-upper-case?)
    => 1)

  (check
      (string-skip "ABCD" char-upper-case?)
    => #f)

  (check
      (string-skip "" char-upper-case?)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (string-skip-right "acdb" #\b)
    => 2)

  (check
      (string-skip-right ("acdb" 1) #\b)
    => 2)

  (check
      (string-skip-right "1111" #\1)
    => #f)

  (check
      (string-skip-right "" #\1)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (string-skip-right "acdb" (char-set #\b #\B))
    => 2)

  (check
      (string-skip-right ("acdb" 1) (char-set #\b #\B))
    => 2)

  (check
      (string-skip-right "0101" (char-set #\0 #\1))
    => #f)

  (check
      (string-skip-right "" (char-set #\0 #\1))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (string-skip-right "acdB" char-upper-case?)
    => 2)

  (check
      (string-skip-right ("acdB" 1) char-upper-case?)
    => 2)

  (check
      (string-skip-right "ABCD" char-upper-case?)
    => #f)

  (check
      (string-skip-right "" char-upper-case?)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (string-count "abcbd" #\b)
    => 2)

  (check
      (string-count ("abcd" 1) #\b)
    => 1)

  (check
      (string-count "abcd" #\1)
    => 0)

  (check
      (string-count "" #\1)
    => 0)

;;; --------------------------------------------------------------------

  (check
      (string-count "abcBd" (char-set #\b #\B))
    => 2)

  (check
      (string-count ("abcd" 1) (char-set #\b #\B))
    => 1)

  (check
      (string-count "abcd" (char-set #\0 #\1))
    => 0)

  (check
      (string-count "" (char-set #\0 #\1))
    => 0)

;;; --------------------------------------------------------------------

  (check
      (string-count "aBcAd" char-upper-case?)
    => 2)

  (check
      (string-count ("aBcd" 1) char-upper-case?)
    => 1)

  (check
      (string-count "abcd" char-upper-case?)
    => 0)

  (check
      (string-count "" char-upper-case?)
    => 0)

;;; --------------------------------------------------------------------

  (check
      (string-contains "ciao hello salut" "hello")
    => 5)

  (check
      (string-contains "ciao hello salut" "hola")
    => #f)

  (check
      (string-contains "ciao hello salut" "")
    => 0)

  (check
      (string-contains "" "hello")
    => #f)

;;; --------------------------------------------------------------------

  (check
      (string-contains-ci "ciAO HELLO saLUT" "hello")
    => 5)

  (check
      (string-contains-ci "ciao hello salut" "HOLA")
    => #f)

  (check
      (string-contains-ci "ciao hello salut" "")
    => 0)

  (check
      (string-contains-ci "" "hello")
    => #f)

  )


(parameterise ((check-test-name 'filtering))

  (check
      (string-delete "abcbd" #\b)
    => "acd")

  (check
      (string-delete "abcbd" #\0)
    => "abcbd")

  (check
      (string-delete "" #\b)
    => "")

  (check
      (string-delete "bbb" #\b)
    => "")

;;; --------------------------------------------------------------------

  (check
      (string-delete "abcbd" (char-set #\b #\B))
    => "acd")

  (check
      (string-delete "abcbd" (char-set #\0 #\1))
    => "abcbd")

  (check
      (string-delete "" (char-set #\b #\B))
    => "")

  (check
      (string-delete "BbB" (char-set #\b #\B))
    => "")

;;; --------------------------------------------------------------------

  (check
      (string-delete "aBcBd" char-upper-case?)
    => "acd")

  (check
      (string-delete "abcbd" char-upper-case?)
    => "abcbd")

  (check
      (string-delete "" char-upper-case?)
    => "")

;;; --------------------------------------------------------------------

  (check
      (string-filter "abcbd" #\b)
    => "bb")

  (check
      (string-filter "abcbd" #\0)
    => "")

  (check
      (string-filter "" #\b)
    => "")

;;; --------------------------------------------------------------------

  (check
      (string-filter "abcbd" (char-set #\b #\B))
    => "bb")

  (check
      (string-filter "abcbd" (char-set #\0 #\1))
    => "")

  (check
      (string-filter "" (char-set #\b #\B))
    => "")

;;; --------------------------------------------------------------------

  (check
      (string-filter "aBcBd" char-upper-case?)
    => "BB")

  (check
      (string-filter "abcbd" char-upper-case?)
    => "")

  (check
      (string-filter "" char-upper-case?)
    => "")

  )


(parameterise ((check-test-name 'lists))

  (check
      (string->list* "abcd")
    => '(#\a #\b #\c #\d))

  (check
      (string->list* ("abcd" 1 3))
    => '(#\b #\c))

  (check
      (string->list* "")
    => '())

;;; --------------------------------------------------------------------

  (check
      (reverse-list->string '(#\a #\b #\c #\d))
    => "dcba")

  (check
      (reverse-list->string '())
    => "")

  )

;;; --------------------------------------------------------------------

(parameterise ((check-test-name 'tokenize))

  (check
      (string-tokenize "ciao hello salut"
		       (char-set #\a #\c #\e #\i #\h #\l #\o #\s #\t #\u))
    => '("ciao" "hello" "salut"))

  (check
      (string-tokenize "" (char-set #\a #\c #\e #\i #\h #\l #\o #\s #\t #\u))
    => '())

  (check
      (string-tokenize "ciao hello salut" (char-set))
    => '())

  )

;;; --------------------------------------------------------------------

(parameterise ((check-test-name 'join))

  (check
      (string-join '("c" "i" "a" "o") "," 'infix)
    => "c,i,a,o")

  (check
      (string-join '("c" "i" "a" "o") "," 'strict-infix)
    => "c,i,a,o")

  (check
      (string-join '("c" "i" "a" "o") "," 'suffix)
    => "c,i,a,o,")

  (check
      (string-join '("c" "i" "a" "o") "," 'prefix)
    => ",c,i,a,o")

;;; --------------------------------------------------------------------

  (check
      (string-join '() "," 'infix)
    => "")

  (check
      (guard (exc ((assertion-violation? exc)
		   #t))
	(string-join '() "," 'strict-infix))
    => #t)

  (check
      (string-join '() "," 'suffix)
    => "")

  (check
      (string-join '() "," 'prefix)
    => "")

;;; --------------------------------------------------------------------

  (check
      (string-join '("c") "," 'infix)
    => "c")

  (check
      (string-join '("c") "," 'strict-infix)
    => "c")

  (check
      (string-join '("c") "," 'suffix)
    => "c,")

  (check
      (string-join '("c") "," 'prefix)
    => ",c")

;;; --------------------------------------------------------------------

  (check
      (string-join '("") "," 'infix)
    => "")

  (check
      (string-join '("") "," 'strict-infix)
    => "")

  (check
      (string-join '("") "," 'suffix)
    => ",")

  (check
      (string-join '("") "," 'prefix)
    => ",")

;;; --------------------------------------------------------------------

  (check
      (string-join '("c" "i" "a" "o") "" 'infix)
    => "ciao")

  (check
      (string-join '("c" "i" "a" "o") "" 'strict-infix)
    => "ciao")

  (check
      (string-join '("c" "i" "a" "o") "" 'suffix)
    => "ciao")

  (check
      (string-join '("c" "i" "a" "o") "" 'prefix)
    => "ciao")

;;; --------------------------------------------------------------------

  (check
      (string-join '("c" "i" "a" "o") ",;;" 'infix)
    => "c,;;i,;;a,;;o")

  (check
      (string-join '("c" "i" "a" "o") ",;;" 'strict-infix)
    => "c,;;i,;;a,;;o")

  (check
      (string-join '("c" "i" "a" "o") ",;;" 'suffix)
    => "c,;;i,;;a,;;o,;;")

  (check
      (string-join '("c" "i" "a" "o") ",;;" 'prefix)
    => ",;;c,;;i,;;a,;;o")

  )


(parameterise ((check-test-name 'xsubstring))

  (check
      (xsubstring "ciao " 0 5)
    => "ciao ")

  (check
      (xsubstring "ciao " 0 9)
    => "ciao ciao")

  (check
      (xsubstring "ciao " -5 5)
    => "ciao ciao ")

  (check
      (xsubstring "ciao " 2 4)
    => "ao")

  (check
      (xsubstring "ciao " -3 7)
    => "ao ciao ci")

  (check (xsubstring "abcdef" 1 7) => "bcdefa")
  (check (xsubstring "abcdef" 2 8) => "cdefab")
  (check (xsubstring "abcdef" 3 9) => "defabc")
  (check (xsubstring "abcdef" 4 10) => "efabcd")
  (check (xsubstring "abcdef" 5 11) => "fabcde")

  (check (xsubstring "abcdef" -1 5) => "fabcde")
  (check (xsubstring "abcdef" -2 4) => "efabcd")
  (check (xsubstring "abcdef" -3 3) => "defabc")
  (check (xsubstring "abcdef" -4 2) => "cdefab")
  (check (xsubstring "abcdef" -5 1) => "bcdefa")

  (check
      (xsubstring "ciao " 3 3)
    => "")

  (check
      (guard (exc ((assertion-violation? exc)
		   #t))
	(xsubstring "" 0 5))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let ((result (string-copy "01234")))
	(string-xcopy! result "ciao " 0 5)
	result)
    => "ciao ")

  (check
      (let ((result (string-copy "012345678")))
	(string-xcopy! result "ciao " 0 9)
	result)
    => "ciao ciao")

  (check
      (let ((result (string-copy "0123456789")))
	(string-xcopy! result "ciao " -5 5)
	result)
    => "ciao ciao ")

  (check
      (let ((result (string-copy "01")))
	(string-xcopy! result "ciao " 2 4)
	result)
    => "ao")

  (check
      (let ((result (string-copy "0123456789")))
	(string-xcopy! result "ciao " -3 7)
	result)
    => "ao ciao ci")

  (check
      (guard (exc ((assertion-violation? exc) #t))
	  (string-xcopy! "" "" 0 5))
    => #t)

  )


(parameterise ((check-test-name 'filling))

  (check
      (let* ((str (string-copy "abcd")))
	(string-fill*! str #\b)
	str)
    => "bbbb")

  (check
      (let* ((str (string-copy "accd")))
	(string-fill*! (str 1 3) #\b)
	str)
    => "abbd")

  (check
      (let* ((str (string-copy "")))
	(string-fill*! (str 0 0) #\b)
	str)
    => "")

  )


(parameterise ((check-test-name 'reverse))

  (check
      (string-reverse "abcd")
    => "dcba")

  (check
      (string-reverse "")
    => "")

;;; --------------------------------------------------------------------

  (check
      (let* ((str (string-copy "abcd")))
	(string-reverse! str)
	str)
    => "dcba")

  (check
      (let* ((str (string-copy "")))
	(string-reverse! str)
	str)
    => "")

  )


(parameterise ((check-test-name 'concatenate))

  (check
      (string-concatenate '("ciao" " " "hello" " " "salut"))
    => "ciao hello salut")

  (check
      (string-concatenate '())
    => "")

;;; --------------------------------------------------------------------

  (check
      (string-concatenate-reverse '("ciao" " " "hello" " " "salut") " hola" 3)
    => "salut hello ciao ho")

  (check
      (string-concatenate-reverse '("ciao" " " "hello" " " "salut") " hola")
    => "salut hello ciao hola")

  (check
      (string-concatenate-reverse '("ciao" " " "hello" " " "salut"))
    => "salut hello ciao")

  (check
      (string-concatenate-reverse '())
    => "")

  )


(parameterise ((check-test-name 'replace))

  (check
      (string-replace "abcd" "1234")
    => "1234")

  (check
      (string-replace ("abcd" 2 2) "1234")
    => "ab1234cd")

  (check
      (string-replace ("abcd" 2 2) "")
    => "abcd")

  (check
      (string-replace ("abcd" 1 3) "1234")
    => "a1234d")

  (check
      (string-replace ("abcd" 0 3) "1234")
    => "1234d")

  (check
      (string-replace ("abcd" 1 4) "1234")
    => "a1234")

  )


;;;; done

(check-report)

;;; end of file