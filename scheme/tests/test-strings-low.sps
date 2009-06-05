;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for the strings library
;;;Date: Thu Jun  4, 2009
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

(import (scheme)
  (strings-low)
  (char-sets)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing strings - low level\n")


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
	(%string-every 123 "abc" 0 2))
    => 'string-every)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aaaa")
	     (beg 0)
	     (end (string-length str)))
	(%string-every #\a str beg end))
    => #t)

  (check
      (let* ((str "aaaab")
	     (beg 0)
	     (end (string-length str)))
	(%string-every #\a str beg end))
    => #f)

  (check
      (let* ((str "aabaa")
	     (beg 0)
	     (end (string-length str)))
	(%string-every #\a str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aaaa")
	     (beg 0)
	     (end (string-length str)))
	(%string-every (char-set #\a) str beg end))
    => #t)

  (check
      (let* ((str "aaaab")
	     (beg 0)
	     (end (string-length str)))
	(%string-every (char-set #\a) str beg end))
    => #f)

  (check
      (let* ((str "aabaa")
	     (beg 0)
	     (end (string-length str)))
	(%string-every (char-set #\a) str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aaaa")
	     (beg 0)
	     (end (string-length str)))
	(%string-every char-alphabetic? str beg end))
    => #t)

  (check
      (let* ((str "aaaa2")
	     (beg 0)
	     (end (string-length str)))
	(%string-every char-alphabetic? str beg end))
    => #f)

  (check
      (let* ((str "aa2aa")
	     (beg 0)
	     (end (string-length str)))
	(%string-every char-alphabetic? str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (guard (exc ((assertion-violation? exc)
		   (condition-who exc)))
	(%string-any 123 "abc" 0 2))
    => 'string-any)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "ddadd")
	     (beg 0)
	     (end (string-length str)))
	(%string-any #\a str beg end))
    => #t)

  (check
      (let* ((str "dddda")
	     (beg 0)
	     (end (string-length str)))
	(%string-any #\a str beg end))
    => #t)

  (check
      (let* ((str "ddd")
	     (beg 0)
	     (end (string-length str)))
	(%string-any #\a str beg end))
    => #f)


;;; --------------------------------------------------------------------

  (check
      (let* ((str "dddaddd")
	     (beg 0)
	     (end (string-length str)))
	(%string-any (char-set #\a) str beg end))
    => #t)

  (check
      (let* ((str "ddda")
	     (beg 0)
	     (end (string-length str)))
	(%string-any (char-set #\a) str beg end))
    => #t)

  (check
      (let* ((str "dddd")
	     (beg 0)
	     (end (string-length str)))
	(%string-any (char-set #\a) str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "11a11")
	     (beg 0)
	     (end (string-length str)))
	(%string-any char-alphabetic? str beg end))
    => #t)

  (check
      (let* ((str "11111a")
	     (beg 0)
	     (end (string-length str)))
	(%string-any char-alphabetic? str beg end))
    => #t)

  (check
      (let* ((str "1111")
	     (beg 0)
	     (end (string-length str)))
	(%string-any char-alphabetic? str beg end))
    => #f)

  )


(parameterise ((check-test-name 'mapping))

  (check
      (let* ((str "aaaa")
	     (beg 0)
	     (end (string-length str)))
	(%string-map char-upcase str beg end))
    => "AAAA")

  (check
      (let* ((str "")
	     (beg 0)
	     (end (string-length str)))
	(%string-map char-upcase str beg end))
    => "")

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aaaa")
	     (beg 0)
	     (end (string-length str)))
	(%string-map! char-upcase str beg end)
	str)
    => "AAAA")

  (check
      (let* ((str "")
	     (beg 0)
	     (end (string-length str)))
	(%string-map! char-upcase str beg end)
	str)
    => "")

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aaaa")
	     (beg 0)
	     (end (string-length str))
	     (result ""))
	(%string-for-each*
	 (lambda (ch)
	   (set! result
		 (string-append result
				(number->string (char->integer (char-upcase ch))))))
	 str beg end)
	result)
    => "65656565")

  (check
      (let* ((str "")
	     (beg 0)
	     (end (string-length str))
	     (result ""))
	(%string-for-each*
	 (lambda (ch)
	   (set! result
		 (string-append result
				(number->string (char->integer (char-upcase ch))))))
	 str beg end)
	result)
    => "")

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aaaa")
	     (beg 0)
	     (end (string-length str))
	     (result '()))
	(%string-for-each-index
	 (lambda (idx)
	   (set! result (cons idx result)))
	 str beg end)
	result)
    => '(3 2 1 0))

  (check
      (let* ((str "")
	     (beg 0)
	     (end (string-length str))
	     (result '()))
	(%string-for-each-index
	 (lambda (idx)
	   (set! result (cons idx result)))
	 str beg end)
	result)
    => '())

  )


(parameterise ((check-test-name 'folding))

  (check
      (let* ((str "abcd")
	     (beg 0)
	     (end (string-length str)))
	(%string-fold cons '() str beg end))
    => '(#\d #\c #\b #\a))

  (check
      (let* ((str "")
	     (beg 0)
	     (end (string-length str)))
	(%string-fold cons '() str beg end))
    => '())

;;; --------------------------------------------------------------------

  (check
      (let* ((str "abcd")
	     (beg 0)
	     (end (string-length str)))
	(%string-fold-right cons '() str beg end))
    => '(#\a #\b #\c #\d))

  (check
      (let* ((str "")
	     (beg 0)
	     (end (string-length str)))
	(%string-fold-right cons '() str beg end))
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


(parameterise ((check-test-name 'prefix))

  (check
      (let* ((str1 "abcdefg") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd123") (beg2 0) (end2 (string-length str2)))
	(%string-prefix-length str1 beg1 end1 str2 beg2 end2))
    => 4)

  (check
      (let* ((str1 "aBcdefg") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd123") (beg2 0) (end2 (string-length str2)))
	(%string-prefix-length str1 beg1 end1 str2 beg2 end2))
    => 1)

  (check
      (let* ((str1 "efg") (beg1 0) (end1 (string-length str1))
	     (str2 "123") (beg2 0) (end2 (string-length str2)))
	(%string-prefix-length str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 "a") (beg1 0) (end1 (string-length str1))
	     (str2 "a") (beg2 0) (end2 (string-length str2)))
	(%string-prefix-length str1 beg1 end1 str2 beg2 end2))
    => 1)

  (check
      (let* ((str1 "1") (beg1 0) (end1 (string-length str1))
	     (str2 "2") (beg2 0) (end2 (string-length str2)))
	(%string-prefix-length str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 "") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd123") (beg2 0) (end2 (string-length str2)))
	(%string-prefix-length str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 "abcdefg") (beg1 0) (end1 (string-length str1))
	     (str2 "") (beg2 0) (end2 (string-length str2)))
	(%string-prefix-length str1 beg1 end1 str2 beg2 end2))
    => 0)

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 "efgabcd") (beg1 0) (end1 (string-length str1))
	     (str2 "123abcd") (beg2 0) (end2 (string-length str2)))
	(%string-suffix-length str1 beg1 end1 str2 beg2 end2))
    => 4)

  (check
      (let* ((str1 "efgabcd") (beg1 0) (end1 (string-length str1))
	     (str2 "123abCd") (beg2 0) (end2 (string-length str2)))
	(%string-suffix-length str1 beg1 end1 str2 beg2 end2))
    => 1)

  (check
      (let* ((str1 "efg") (beg1 0) (end1 (string-length str1))
	     (str2 "123") (beg2 0) (end2 (string-length str2)))
	(%string-suffix-length str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 "a") (beg1 0) (end1 (string-length str1))
	     (str2 "a") (beg2 0) (end2 (string-length str2)))
	(%string-suffix-length str1 beg1 end1 str2 beg2 end2))
    => 1)

  (check
      (let* ((str1 "1") (beg1 0) (end1 (string-length str1))
	     (str2 "2") (beg2 0) (end2 (string-length str2)))
	(%string-suffix-length str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 "") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd123") (beg2 0) (end2 (string-length str2)))
	(%string-suffix-length str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 "abcdefg") (beg1 0) (end1 (string-length str1))
	     (str2 "") (beg2 0) (end2 (string-length str2)))
	(%string-suffix-length str1 beg1 end1 str2 beg2 end2))
    => 0)

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 "aBcdefg") (beg1 0) (end1 (string-length str1))
	     (str2 "aBcd123") (beg2 0) (end2 (string-length str2)))
	(%string-prefix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 4)

  (check
      (let* ((str1 "aBcdefg") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd123") (beg2 0) (end2 (string-length str2)))
	(%string-prefix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 4)

  (check
      (let* ((str1 "efg") (beg1 0) (end1 (string-length str1))
	     (str2 "123") (beg2 0) (end2 (string-length str2)))
	(%string-prefix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 "a") (beg1 0) (end1 (string-length str1))
	     (str2 "a") (beg2 0) (end2 (string-length str2)))
	(%string-prefix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 1)

  (check
      (let* ((str1 "1") (beg1 0) (end1 (string-length str1))
	     (str2 "2") (beg2 0) (end2 (string-length str2)))
	(%string-prefix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 "") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd123") (beg2 0) (end2 (string-length str2)))
	(%string-prefix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 "abcdefg") (beg1 0) (end1 (string-length str1))
	     (str2 "") (beg2 0) (end2 (string-length str2)))
	(%string-prefix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 0)

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 "efgabCd") (beg1 0) (end1 (string-length str1))
	     (str2 "123abCd") (beg2 0) (end2 (string-length str2)))
	(%string-suffix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 4)

  (check
      (let* ((str1 "efgabCd") (beg1 0) (end1 (string-length str1))
	     (str2 "123abcd") (beg2 0) (end2 (string-length str2)))
	(%string-suffix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 4)

  (check
      (let* ((str1 "efg") (beg1 0) (end1 (string-length str1))
	     (str2 "123") (beg2 0) (end2 (string-length str2)))
	(%string-suffix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 "a") (beg1 0) (end1 (string-length str1))
	     (str2 "a") (beg2 0) (end2 (string-length str2)))
	(%string-suffix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 1)

  (check
      (let* ((str1 "1") (beg1 0) (end1 (string-length str1))
	     (str2 "2") (beg2 0) (end2 (string-length str2)))
	(%string-suffix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 "") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd123") (beg2 0) (end2 (string-length str2)))
	(%string-suffix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 "abcdefg") (beg1 0) (end1 (string-length str1))
	     (str2 "") (beg2 0) (end2 (string-length str2)))
	(%string-suffix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 0)

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd123") (beg2 0) (end2 (string-length str2)))
	(%string-prefix? str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "aBcd123") (beg2 0) (end2 (string-length str2)))
	(%string-prefix? str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "efg") (beg1 0) (end1 (string-length str1))
	     (str2 "123") (beg2 0) (end2 (string-length str2)))
	(%string-prefix? str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "") (beg1 0) (end1 (string-length str1))
	     (str2 "123") (beg2 0) (end2 (string-length str2)))
	(%string-prefix? str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 "efg") (beg1 0) (end1 (string-length str1))
	     (str2 "") (beg2 0) (end2 (string-length str2)))
	(%string-prefix? str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "") (beg1 0) (end1 (string-length str1))
	     (str2 "") (beg2 0) (end2 (string-length str2)))
	(%string-prefix? str1 beg1 end1 str2 beg2 end2))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 "aBcd") (beg1 0) (end1 (string-length str1))
	     (str2 "aBcd123") (beg2 0) (end2 (string-length str2)))
	(%string-prefix-ci? str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "aBcd123") (beg2 0) (end2 (string-length str2)))
	(%string-prefix-ci? str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 "efg") (beg1 0) (end1 (string-length str1))
	     (str2 "123") (beg2 0) (end2 (string-length str2)))
	(%string-prefix-ci? str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "") (beg1 0) (end1 (string-length str1))
	     (str2 "123") (beg2 0) (end2 (string-length str2)))
	(%string-prefix-ci? str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 "efg") (beg1 0) (end1 (string-length str1))
	     (str2 "") (beg2 0) (end2 (string-length str2)))
	(%string-prefix-ci? str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "") (beg1 0) (end1 (string-length str1))
	     (str2 "") (beg2 0) (end2 (string-length str2)))
	(%string-prefix-ci? str1 beg1 end1 str2 beg2 end2))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "123abcd") (beg2 0) (end2 (string-length str2)))
	(%string-suffix? str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "123aBcd") (beg2 0) (end2 (string-length str2)))
	(%string-suffix? str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "efg") (beg1 0) (end1 (string-length str1))
	     (str2 "123") (beg2 0) (end2 (string-length str2)))
	(%string-suffix? str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "") (beg1 0) (end1 (string-length str1))
	     (str2 "123") (beg2 0) (end2 (string-length str2)))
	(%string-suffix? str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 "efg") (beg1 0) (end1 (string-length str1))
	     (str2 "") (beg2 0) (end2 (string-length str2)))
	(%string-suffix? str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "") (beg1 0) (end1 (string-length str1))
	     (str2 "") (beg2 0) (end2 (string-length str2)))
	(%string-suffix? str1 beg1 end1 str2 beg2 end2))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 "aBcd") (beg1 0) (end1 (string-length str1))
	     (str2 "123aBcd") (beg2 0) (end2 (string-length str2)))
	(%string-suffix-ci? str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "123aBcd") (beg2 0) (end2 (string-length str2)))
	(%string-suffix-ci? str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 "efg") (beg1 0) (end1 (string-length str1))
	     (str2 "123") (beg2 0) (end2 (string-length str2)))
	(%string-suffix-ci? str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "") (beg1 0) (end1 (string-length str1))
	     (str2 "123") (beg2 0) (end2 (string-length str2)))
	(%string-suffix-ci? str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 "efg") (beg1 0) (end1 (string-length str1))
	     (str2 "") (beg2 0) (end2 (string-length str2)))
	(%string-suffix-ci? str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "") (beg1 0) (end1 (string-length str1))
	     (str2 "") (beg2 0) (end2 (string-length str2)))
	(%string-suffix-ci? str1 beg1 end1 str2 beg2 end2))
    => #t)

  )


(parameterise ((check-test-name 'comparison-case-sensitive))

  (check
      (let* ((str1 "abcdefg") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd123") (beg2 0) (end2 (string-length str2)))
	(%string-compare str1 beg1 end1 str2 beg2 end2 values values values))
    => 4)

  (check
      (let* ((str1 "abcdef") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd123") (beg2 0) (end2 (string-length str2)))
	(%string-compare str1 beg1 end1 str2 beg2 end2 values values values))
    => 4)

  (check
      (let* ((str1 "efg") (beg1 0) (end1 (string-length str1))
	     (str2 "123") (beg2 0) (end2 (string-length str2)))
	(%string-compare str1 beg1 end1 str2 beg2 end2 values values values))
    => 0)

  (check
      (let* ((str1 "") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd") (beg2 0) (end2 (string-length str2)))
	(%string-compare str1 beg1 end1 str2 beg2 end2 values values values))
    => 0)

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "") (beg2 0) (end2 (string-length str2)))
	(%string-compare str1 beg1 end1 str2 beg2 end2 values values values))
    => 0)

  (check
      (let* ((str1 "abcdA") (beg1 0) (end1 (string-length str1))
	     (str2 "abcdA") (beg2 0) (end2 (string-length str2)))
	(%string-compare str1 beg1 end1 str2 beg2 end2
			 (lambda (idx) 'less) (lambda (idx) 'equal) (lambda (idx) 'greater)))
    => 'equal)

  (check
      (let* ((str1 "abcdA") (beg1 0) (end1 (string-length str1))
	     (str2 "abcdB") (beg2 0) (end2 (string-length str2)))
	(%string-compare str1 beg1 end1 str2 beg2 end2
			 (lambda (idx) 'less) (lambda (idx) 'equal) (lambda (idx) 'greater)))
    => 'less)

  (check
      (let* ((str1 "abcdB") (beg1 0) (end1 (string-length str1))
	     (str2 "abcdA") (beg2 0) (end2 (string-length str2)))
	(%string-compare str1 beg1 end1 str2 beg2 end2
			 (lambda (idx) 'less) (lambda (idx) 'equal) (lambda (idx) 'greater)))
    => 'greater)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "abcd")
	     (beg1 0) (end1 (string-length str))
	     (beg2 0) (end2 (string-length str)))
	(%string= str beg1 end1 str beg2 end2))
    => #t)

  (check
      (let* ((str1 "12abcd") (beg1 2) (end1 (string-length str1))
	     (str2 "abcd") (beg2 0) (end2 (string-length str2)))
	(%string= str1 beg1 end1 str2 beg2 end2))
    => 6)

  (check
      (let* ((str1 "abc") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd") (beg2 0) (end2 (string-length str2)))
	(%string= str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "abc") (beg2 0) (end2 (string-length str2)))
	(%string= str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "ABcd") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd") (beg2 0) (end2 (string-length str2)))
	(%string= str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "a2cd") (beg2 0) (end2 (string-length str2)))
	(%string= str1 beg1 end1 str2 beg2 end2))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd") (beg2 0) (end2 (string-length str2)))
	(%string<> str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "abc") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd") (beg2 0) (end2 (string-length str2)))
	(%string<> str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "abc") (beg2 0) (end2 (string-length str2)))
	(%string<> str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 "ABcd") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd") (beg2 0) (end2 (string-length str2)))
	(%string<> str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "a2cd") (beg2 0) (end2 (string-length str2)))
	(%string<> str1 beg1 end1 str2 beg2 end2))
    => 1)

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd") (beg2 0) (end2 (string-length str2)))
	(%string< str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "abc") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd") (beg2 0) (end2 (string-length str2)))
	(%string< str1 beg1 end1 str2 beg2 end2))
    => 3)

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "abc") (beg2 0) (end2 (string-length str2)))
	(%string< str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "ABcd") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd") (beg2 0) (end2 (string-length str2)))
	(%string< str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "a2cd") (beg2 0) (end2 (string-length str2)))
	(%string< str1 beg1 end1 str2 beg2 end2))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd") (beg2 0) (end2 (string-length str2)))
	(%string<= str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 "abc") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd") (beg2 0) (end2 (string-length str2)))
	(%string<= str1 beg1 end1 str2 beg2 end2))
    => 3)

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "abc") (beg2 0) (end2 (string-length str2)))
	(%string<= str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "ABcd") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd") (beg2 0) (end2 (string-length str2)))
	(%string<= str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "a2cd") (beg2 0) (end2 (string-length str2)))
	(%string<= str1 beg1 end1 str2 beg2 end2))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd") (beg2 0) (end2 (string-length str2)))
	(%string> str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "abc") (beg2 0) (end2 (string-length str2)))
	(%string> str1 beg1 end1 str2 beg2 end2))
    => 3)

  (check
      (let* ((str1 "abc") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd") (beg2 0) (end2 (string-length str2)))
	(%string> str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "ABcd") (beg2 0) (end2 (string-length str2)))
	(%string> str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 "a2cd") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd") (beg2 0) (end2 (string-length str2)))
	(%string> str1 beg1 end1 str2 beg2 end2))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd") (beg2 0) (end2 (string-length str2)))
	(%string>= str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "abc") (beg2 0) (end2 (string-length str2)))
	(%string>= str1 beg1 end1 str2 beg2 end2))
    => 3)

  (check
      (let* ((str1 "abc") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd") (beg2 0) (end2 (string-length str2)))
	(%string>= str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "ABcd") (beg2 0) (end2 (string-length str2)))
	(%string>= str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 "a2cd") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd") (beg2 0) (end2 (string-length str2)))
	(%string>= str1 beg1 end1 str2 beg2 end2))
    => #f)

  )


(parameterise ((check-test-name 'comparison-case-insensitive))

  (check
      (let* ((str1 "aBcdefg") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd123") (beg2 0) (end2 (string-length str2)))
	(%string-compare-ci str1 beg1 end1 str2 beg2 end2 values values values))
    => 4)

  (check
      (let* ((str1 "efg") (beg1 0) (end1 (string-length str1))
	     (str2 "123") (beg2 0) (end2 (string-length str2)))
	(%string-compare-ci str1 beg1 end1 str2 beg2 end2 values values values))
    => 0)

  (check
      (let* ((str1 "") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd") (beg2 0) (end2 (string-length str2)))
	(%string-compare-ci str1 beg1 end1 str2 beg2 end2 values values values))
    => 0)

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "") (beg2 0) (end2 (string-length str2)))
	(%string-compare-ci str1 beg1 end1 str2 beg2 end2 values values values))
    => 0)

  (check
      (let* ((str1 "abcdA") (beg1 0) (end1 (string-length str1))
	     (str2 "abcda") (beg2 0) (end2 (string-length str2)))
	(%string-compare-ci str1 beg1 end1 str2 beg2 end2
			    (lambda (idx) 'less) (lambda (idx) 'equal) (lambda (idx) 'greater)))
    => 'equal)

  (check
      (let* ((str1 "abcdA") (beg1 0) (end1 (string-length str1))
	     (str2 "abcdb") (beg2 0) (end2 (string-length str2)))
	(%string-compare-ci str1 beg1 end1 str2 beg2 end2
			    (lambda (idx) 'less) (lambda (idx) 'equal) (lambda (idx) 'greater)))
    => 'less)

  (check
      (let* ((str1 "abcdb") (beg1 0) (end1 (string-length str1))
	     (str2 "abcdA") (beg2 0) (end2 (string-length str2)))
	(%string-compare-ci str1 beg1 end1 str2 beg2 end2
			    (lambda (idx) 'less) (lambda (idx) 'equal) (lambda (idx) 'greater)))
    => 'greater)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "abcd")
	     (beg1 0) (end1 (string-length str))
	     (beg2 0) (end2 (string-length str)))
	(%string-ci= str beg1 end1 str beg2 end2))
    => #t)

  (check
      (let* ((str1 "12abcd") (beg1 2) (end1 (string-length str1))
	     (str2 "abcd") (beg2 0) (end2 (string-length str2)))
	(%string-ci= str1 beg1 end1 str2 beg2 end2))
    => 6)

  (check
      (let* ((str1 "abc") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd") (beg2 0) (end2 (string-length str2)))
	(%string-ci= str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "abc") (beg2 0) (end2 (string-length str2)))
	(%string-ci= str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "ABcd") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd") (beg2 0) (end2 (string-length str2)))
	(%string-ci= str1 beg1 end1 str2 beg2 end2))
    => 4)

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "a2cd") (beg2 0) (end2 (string-length str2)))
	(%string-ci= str1 beg1 end1 str2 beg2 end2))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd") (beg2 0) (end2 (string-length str2)))
	(%string-ci<> str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "abc") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd") (beg2 0) (end2 (string-length str2)))
	(%string-ci<> str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "abc") (beg2 0) (end2 (string-length str2)))
	(%string-ci<> str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 "ABcd") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd") (beg2 0) (end2 (string-length str2)))
	(%string-ci<> str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "a2cd") (beg2 0) (end2 (string-length str2)))
	(%string-ci<> str1 beg1 end1 str2 beg2 end2))
    => 1)

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd") (beg2 0) (end2 (string-length str2)))
	(%string-ci< str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "abc") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd") (beg2 0) (end2 (string-length str2)))
	(%string-ci< str1 beg1 end1 str2 beg2 end2))
    => 3)

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "abc") (beg2 0) (end2 (string-length str2)))
	(%string-ci< str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "ABcd") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd") (beg2 0) (end2 (string-length str2)))
	(%string-ci< str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "a2cd") (beg2 0) (end2 (string-length str2)))
	(%string-ci< str1 beg1 end1 str2 beg2 end2))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd") (beg2 0) (end2 (string-length str2)))
	(%string-ci<= str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 "abc") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd") (beg2 0) (end2 (string-length str2)))
	(%string-ci<= str1 beg1 end1 str2 beg2 end2))
    => 3)

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "abc") (beg2 0) (end2 (string-length str2)))
	(%string-ci<= str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "ABcd") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd") (beg2 0) (end2 (string-length str2)))
	(%string-ci<= str1 beg1 end1 str2 beg2 end2))
    => 4)

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "a2cd") (beg2 0) (end2 (string-length str2)))
	(%string-ci<= str1 beg1 end1 str2 beg2 end2))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd") (beg2 0) (end2 (string-length str2)))
	(%string-ci> str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "abc") (beg2 0) (end2 (string-length str2)))
	(%string-ci> str1 beg1 end1 str2 beg2 end2))
    => 3)

  (check
      (let* ((str1 "abc") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd") (beg2 0) (end2 (string-length str2)))
	(%string-ci> str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "ABcd") (beg2 0) (end2 (string-length str2)))
	(%string-ci> str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "a2cd") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd") (beg2 0) (end2 (string-length str2)))
	(%string-ci> str1 beg1 end1 str2 beg2 end2))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd") (beg2 0) (end2 (string-length str2)))
	(%string-ci>= str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "abc") (beg2 0) (end2 (string-length str2)))
	(%string-ci>= str1 beg1 end1 str2 beg2 end2))
    => 3)

  (check
      (let* ((str1 "abc") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd") (beg2 0) (end2 (string-length str2)))
	(%string-ci>= str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	     (str2 "ABcd") (beg2 0) (end2 (string-length str2)))
	(%string-ci>= str1 beg1 end1 str2 beg2 end2))
    => 4)

  (check
      (let* ((str1 "a2cd") (beg1 0) (end1 (string-length str1))
	     (str2 "abcd") (beg2 0) (end2 (string-length str2)))
	(%string-ci>= str1 beg1 end1 str2 beg2 end2))
    => #f)

  )




;;;; done

(check-report)

;;; end of file
