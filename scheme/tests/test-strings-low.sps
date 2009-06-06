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

(import (nausicaa)
  (strings low)
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

  (cond-expand
   (ikarus (check
	       (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
		      (str2 "abcd") (beg2 0) (end2 (string-length str2)))
		 (%string<= str1 beg1 end1 str2 beg2 end2))
	     => 4))
   (else (check
	     (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
		    (str2 "abcd") (beg2 0) (end2 (string-length str2)))
	       (%string<= str1 beg1 end1 str2 beg2 end2))
	   => #t)))

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

  (cond-expand
   (ikarus
    (check
	(let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
	       (str2 "abcd") (beg2 0) (end2 (string-length str2)))
	  (%string>= str1 beg1 end1 str2 beg2 end2))
      => 4))
   (else (check
	     (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
		    (str2 "abcd") (beg2 0) (end2 (string-length str2)))
	       (%string>= str1 beg1 end1 str2 beg2 end2))
	   => #t)))

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

  (cond-expand
   (ikarus (check
	       (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
		      (str2 "abcd") (beg2 0) (end2 (string-length str2)))
		 (%string-ci<= str1 beg1 end1 str2 beg2 end2))
	     => 4))
   (else (check
	     (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
		    (str2 "abcd") (beg2 0) (end2 (string-length str2)))
	       (%string-ci<= str1 beg1 end1 str2 beg2 end2))
	   => #t)))

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

  (cond-expand
   (ikarus (check
	       (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
		      (str2 "abcd") (beg2 0) (end2 (string-length str2)))
		 (%string-ci>= str1 beg1 end1 str2 beg2 end2))
	     => 4))
   (else (check
	     (let* ((str1 "abcd") (beg1 0) (end1 (string-length str1))
		    (str2 "abcd") (beg2 0) (end2 (string-length str2)))
	       (%string-ci>= str1 beg1 end1 str2 beg2 end2))
	   => #t)))

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


(parameterise ((check-test-name 'case-hacking))

  (check
      (let* ((str (string-copy "abcd")) (beg 0) (end (string-length str)))
	(%string-titlecase! str beg end)
	str)
    => "Abcd")

  (check
      (let* ((str (string-copy "123abcd")) (beg 0) (end (string-length str)))
	(%string-titlecase! str beg end)
	str)
    => "123Abcd")

  (check
      (let* ((str (string-copy "---abcd")) (beg 0) (end (string-length str)))
	(%string-titlecase! str beg end)
	str)
    => "---Abcd")

  (check
      (let* ((str (string-copy "abcd efgh")) (beg 0) (end (string-length str)))
	(%string-titlecase! str beg end)
	str)
    => "Abcd Efgh")

  )


(parameterise ((check-test-name 'selecting))

  (check
      (let* ((str "abcd") (beg 0) (end (string-length str)))
	(%string-take 2 str beg end))
    => "ab")

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-take 0 str beg end))
    => "")

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(let* ((str "abcd") (beg 0) (end (string-length str)))
	  (%string-take 5 str beg end)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "abcd") (beg 0) (end (string-length str)))
	(%string-take-right 2 str beg end))
    => "cd")

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-take-right 0 str beg end))
    => "")

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(let* ((str "abcd") (beg 0) (end (string-length str)))
	  (%string-take-right 5 str beg end)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "abcd") (beg 0) (end (string-length str)))
	(%string-drop 2 str beg end))
    => "cd")

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-drop 0 str beg end))
    => "")

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(let* ((str "abcd") (beg 0) (end (string-length str)))
	  (%string-drop 5 str beg end)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "abcd") (beg 0) (end (string-length str)))
	(%string-drop-right 2 str beg end))
    => "ab")

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-drop-right 0 str beg end))
    => "")

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(let* ((str "abcd") (beg 0) (end (string-length str)))
	  (%string-drop-right 5 str beg end)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aaabcd") (beg 0) (end (string-length str)))
	(%string-trim #\a str beg end))
    => "bcd")

  (check
      (let* ((str "bcd") (beg 0) (end (string-length str)))
	(%string-trim #\a str beg end))
    => "bcd")

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-trim #\a str beg end))
    => "")

  (check
      (let* ((str "aaabcd") (beg 0) (end (string-length str)))
	(%string-trim (char-set #\a #\b) str beg end))
    => "cd")

  (check
      (let* ((str "bcd") (beg 0) (end (string-length str)))
	(%string-trim (char-set #\a #\b) str beg end))
    => "cd")

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-trim (char-set #\a #\b) str beg end))
    => "")

  (check
      (let* ((str "AAAbcd") (beg 0) (end (string-length str)))
	(%string-trim char-upper-case? str beg end))
    => "bcd")

  (check
      (let* ((str "bcd") (beg 0) (end (string-length str)))
	(%string-trim char-upper-case? str beg end))
    => "bcd")

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-trim char-upper-case? str beg end))
    => "")

;;; --------------------------------------------------------------------

  (check
      (let* ((str "bcdaaa") (beg 0) (end (string-length str)))
	(%string-trim-right #\a str beg end))
    => "bcd")

  (check
      (let* ((str "bcd") (beg 0) (end (string-length str)))
	(%string-trim-right #\a str beg end))
    => "bcd")

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-trim-right #\a str beg end))
    => "")

  (check
      (let* ((str "cdbaaa") (beg 0) (end (string-length str)))
	(%string-trim-right (char-set #\a #\b) str beg end))
    => "cd")

  (check
      (let* ((str "cdb") (beg 0) (end (string-length str)))
	(%string-trim-right (char-set #\a #\b) str beg end))
    => "cd")

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-trim-right (char-set #\a #\b) str beg end))
    => "")

  (check
      (let* ((str "bcdAAA") (beg 0) (end (string-length str)))
	(%string-trim-right char-upper-case? str beg end))
    => "bcd")

  (check
      (let* ((str "bcd") (beg 0) (end (string-length str)))
	(%string-trim-right char-upper-case? str beg end))
    => "bcd")

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-trim-right char-upper-case? str beg end))
    => "")

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aaabcdaaa") (beg 0) (end (string-length str)))
	(%string-trim-both #\a str beg end))
    => "bcd")

  (check
      (let* ((str "bcd") (beg 0) (end (string-length str)))
	(%string-trim-both #\a str beg end))
    => "bcd")

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-trim-both #\a str beg end))
    => "")

  (check
      (let* ((str "aaabcdaa") (beg 0) (end (string-length str)))
	(%string-trim-both (char-set #\a #\b) str beg end))
    => "cd")

  (check
      (let* ((str "bcdb") (beg 0) (end (string-length str)))
	(%string-trim-both (char-set #\a #\b) str beg end))
    => "cd")

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-trim-both (char-set #\a #\b) str beg end))
    => "")

  (check
      (let* ((str "AAAbcdAAA") (beg 0) (end (string-length str)))
	(%string-trim-both char-upper-case? str beg end))
    => "bcd")

  (check
      (let* ((str "bcd") (beg 0) (end (string-length str)))
	(%string-trim-both char-upper-case? str beg end))
    => "bcd")

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-trim-both char-upper-case? str beg end))
    => "")

;;; --------------------------------------------------------------------

  (check
      (let* ((str "abc") (beg 0) (end (string-length str)))
	(%string-pad 3 #\0 str beg end))
    => "abc")

  (check
      (let* ((str "abc") (beg 0) (end (string-length str)))
	(%string-pad 5 #\0 str beg end))
    => "00abc")

  (check
      (let* ((str "abc") (beg 0) (end (string-length str)))
	(%string-pad 2 #\0 str beg end))
    => "bc")

  (check
      (let* ((str "abc") (beg 0) (end (string-length str)))
	(%string-pad 0 #\0 str beg end))
    => "")

;;; --------------------------------------------------------------------

  (check
      (let* ((str "abc") (beg 0) (end (string-length str)))
	(%string-pad-right 3 #\0 str beg end))
    => "abc")

  (check
      (let* ((str "abc") (beg 0) (end (string-length str)))
	(%string-pad-right 5 #\0 str beg end))
    => "abc00")

  (check
      (let* ((str "abc") (beg 0) (end (string-length str)))
	(%string-pad-right 2 #\0 str beg end))
    => "ab")

  (check
      (let* ((str "abc") (beg 0) (end (string-length str)))
	(%string-pad-right 0 #\0 str beg end))
    => "")

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 "abcd") (beg1 0) (str2 (string-copy "12")))
	(%string-copy! str2 0 str1 beg1 (+ 2 beg1))
	str2)
    => "ab")

  (check
      (let* ((str1 "abcd") (beg1 0) (str2 ""))
	(%string-copy! str2 0 str1 beg1 beg1)
	str2)
    => "")

  (check
      (guard (exc ((assertion-violation? exc)
		   #t))
	(let* ((str1 "abcd") (beg1 0) (str2 (string-copy "12")))
	  (%string-copy! str2 3 str1 beg1 (+ 2 beg1))
	  str2))
    => #t)

  )


(parameterise ((check-test-name 'filtering))

  (check
      (let* ((str "abcbd") (beg 0) (end (string-length str)))
	(%string-delete #\b str beg end))
    => "acd")

  (check
      (let* ((str "abcbd") (beg 0) (end (string-length str)))
	(%string-delete #\0 str beg end))
    => "abcbd")

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-delete #\b str beg end))
    => "")

;;; --------------------------------------------------------------------

  (check
      (let* ((str "abcbd") (beg 0) (end (string-length str)))
	(%string-delete (char-set #\b #\B) str beg end))
    => "acd")

  (check
      (let* ((str "abcbd") (beg 0) (end (string-length str)))
	(%string-delete (char-set #\0 #\1) str beg end))
    => "abcbd")

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-delete (char-set #\b #\B) str beg end))
    => "")

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aBcBd") (beg 0) (end (string-length str)))
	(%string-delete char-upper-case? str beg end))
    => "acd")

  (check
      (let* ((str "abcbd") (beg 0) (end (string-length str)))
	(%string-delete char-upper-case? str beg end))
    => "abcbd")

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-delete char-upper-case? str beg end))
    => "")

;;; --------------------------------------------------------------------

  (check
      (let* ((str "abcbd") (beg 0) (end (string-length str)))
	(%string-filter #\b str beg end))
    => "bb")

  (check
      (let* ((str "abcbd") (beg 0) (end (string-length str)))
	(%string-filter #\0 str beg end))
    => "")

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-filter #\b str beg end))
    => "")

;;; --------------------------------------------------------------------

  (check
      (let* ((str "abcbd") (beg 0) (end (string-length str)))
	(%string-filter (char-set #\b #\B) str beg end))
    => "bb")

  (check
      (let* ((str "abcbd") (beg 0) (end (string-length str)))
	(%string-filter (char-set #\0 #\1) str beg end))
    => "")

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-filter (char-set #\b #\B) str beg end))
    => "")

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aBcBd") (beg 0) (end (string-length str)))
	(%string-filter char-upper-case? str beg end))
    => "BB")

  (check
      (let* ((str "abcbd") (beg 0) (end (string-length str)))
	(%string-filter char-upper-case? str beg end))
    => "")

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-filter char-upper-case? str beg end))
    => "")

  )


(parameterise ((check-test-name 'searching))

  (check
      (let* ((str "abcd") (beg 0) (end (string-length str)))
	(%string-index #\b str beg end))
    => 1)

  (check
      (let* ((str "abcd") (end (string-length str)))
	(%string-index #\b str 1 end))
    => 1)

  (check
      (let* ((str "abcd") (beg 0) (end (string-length str)))
	(%string-index #\1 str beg end))
    => #f)

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-index #\1 str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "abcd") (beg 0) (end (string-length str)))
	(%string-index (char-set #\b #\B) str beg end))
    => 1)

  (check
      (let* ((str "abcd") (end (string-length str)))
	(%string-index (char-set #\b #\B) str 1 end))
    => 1)

  (check
      (let* ((str "abcd") (beg 0) (end (string-length str)))
	(%string-index (char-set #\0 #\1) str beg end))
    => #f)

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-index (char-set #\0 #\1) str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aBcd") (beg 0) (end (string-length str)))
	(%string-index char-upper-case? str beg end))
    => 1)

  (check
      (let* ((str "aBcd") (end (string-length str)))
	(%string-index char-upper-case? str 1 end))
    => 1)

  (check
      (let* ((str "abcd") (beg 0) (end (string-length str)))
	(%string-index char-upper-case? str beg end))
    => #f)

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-index char-upper-case? str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "abcd") (beg 0) (end (string-length str)))
	(%string-index-right #\b str beg end))
    => 1)

  (check
      (let* ((str "abcd") (end (string-length str)))
	(%string-index-right #\b str 1 end))
    => 1)

  (check
      (let* ((str "abcd") (beg 0) (end (string-length str)))
	(%string-index-right #\1 str beg end))
    => #f)

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-index-right #\1 str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "abcd") (beg 0) (end (string-length str)))
	(%string-index-right (char-set #\b #\B) str beg end))
    => 1)

  (check
      (let* ((str "abcd") (end (string-length str)))
	(%string-index-right (char-set #\b #\B) str 1 end))
    => 1)

  (check
      (let* ((str "abcd") (beg 0) (end (string-length str)))
	(%string-index-right (char-set #\0 #\1) str beg end))
    => #f)

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-index-right (char-set #\0 #\1) str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aBcd") (beg 0) (end (string-length str)))
	(%string-index-right char-upper-case? str beg end))
    => 1)

  (check
      (let* ((str "aBcd") (end (string-length str)))
	(%string-index-right char-upper-case? str 1 end))
    => 1)

  (check
      (let* ((str "abcd") (beg 0) (end (string-length str)))
	(%string-index-right char-upper-case? str beg end))
    => #f)

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-index-right char-upper-case? str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "bacd") (beg 0) (end (string-length str)))
	(%string-skip #\b str beg end))
    => 1)

  (check
      (let* ((str "bacd") (end (string-length str)))
	(%string-skip #\b str 1 end))
    => 1)

  (check
      (let* ((str "1111") (beg 0) (end (string-length str)))
	(%string-skip #\1 str beg end))
    => #f)

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-skip #\1 str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "bacd") (beg 0) (end (string-length str)))
	(%string-skip (char-set #\b #\B) str beg end))
    => 1)

  (check
      (let* ((str "bacd") (end (string-length str)))
	(%string-skip (char-set #\b #\B) str 1 end))
    => 1)

  (check
      (let* ((str "1010") (beg 0) (end (string-length str)))
	(%string-skip (char-set #\0 #\1) str beg end))
    => #f)

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-skip (char-set #\0 #\1) str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "Bacd") (beg 0) (end (string-length str)))
	(%string-skip char-upper-case? str beg end))
    => 1)

  (check
      (let* ((str "Bacd") (end (string-length str)))
	(%string-skip char-upper-case? str 1 end))
    => 1)

  (check
      (let* ((str "ABCD") (beg 0) (end (string-length str)))
	(%string-skip char-upper-case? str beg end))
    => #f)

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-skip char-upper-case? str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "acdb") (beg 0) (end (string-length str)))
	(%string-skip-right #\b str beg end))
    => 2)

  (check
      (let* ((str "acdb") (end (string-length str)))
	(%string-skip-right #\b str 1 end))
    => 2)

  (check
      (let* ((str "1111") (beg 0) (end (string-length str)))
	(%string-skip-right #\1 str beg end))
    => #f)

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-skip-right #\1 str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "acdb") (beg 0) (end (string-length str)))
	(%string-skip-right (char-set #\b #\B) str beg end))
    => 2)

  (check
      (let* ((str "acdb") (end (string-length str)))
	(%string-skip-right (char-set #\b #\B) str 1 end))
    => 2)

  (check
      (let* ((str "0101") (beg 0) (end (string-length str)))
	(%string-skip-right (char-set #\0 #\1) str beg end))
    => #f)

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-skip-right (char-set #\0 #\1) str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "acdB") (beg 0) (end (string-length str)))
	(%string-skip-right char-upper-case? str beg end))
    => 2)

  (check
      (let* ((str "acdB") (end (string-length str)))
	(%string-skip-right char-upper-case? str 1 end))
    => 2)

  (check
      (let* ((str "ABCD") (beg 0) (end (string-length str)))
	(%string-skip-right char-upper-case? str beg end))
    => #f)

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-skip-right char-upper-case? str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "abcbd") (beg 0) (end (string-length str)))
	(%string-count #\b str beg end))
    => 2)

  (check
      (let* ((str "abcd") (end (string-length str)))
	(%string-count #\b str 1 end))
    => 1)

  (check
      (let* ((str "abcd") (beg 0) (end (string-length str)))
	(%string-count #\1 str beg end))
    => 0)

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-count #\1 str beg end))
    => 0)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "abcBd") (beg 0) (end (string-length str)))
	(%string-count (char-set #\b #\B) str beg end))
    => 2)

  (check
      (let* ((str "abcd") (end (string-length str)))
	(%string-count (char-set #\b #\B) str 1 end))
    => 1)

  (check
      (let* ((str "abcd") (beg 0) (end (string-length str)))
	(%string-count (char-set #\0 #\1) str beg end))
    => 0)

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-count (char-set #\0 #\1) str beg end))
    => 0)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aBcAd") (beg 0) (end (string-length str)))
	(%string-count char-upper-case? str beg end))
    => 2)

  (check
      (let* ((str "aBcd") (end (string-length str)))
	(%string-count char-upper-case? str 1 end))
    => 1)

  (check
      (let* ((str "abcd") (beg 0) (end (string-length str)))
	(%string-count char-upper-case? str beg end))
    => 0)

  (check
      (let* ((str "") (beg 0) (end (string-length str)))
	(%string-count char-upper-case? str beg end))
    => 0)

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 "ciao hello salut") (beg1 0) (end1 (string-length str1))
	     (str2 "hello") (beg2 0) (end2 (string-length str2)))
	(%string-contains str1 beg1 end1 str2 beg2 end2))
    => 5)

  (check
      (let* ((str1 "ciao hello salut") (beg1 0) (end1 (string-length str1))
	     (str2 "hola") (beg2 0) (end2 (string-length str2)))
	(%string-contains str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "ciao hello salut") (beg1 0) (end1 (string-length str1))
	     (str2 "") (beg2 0) (end2 (string-length str2)))
	(%string-contains str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 "") (beg1 0) (end1 (string-length str1))
	     (str2 "hello") (beg2 0) (end2 (string-length str2)))
	(%string-contains str1 beg1 end1 str2 beg2 end2))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 "ciAO HELLO saLUT") (beg1 0) (end1 (string-length str1))
	     (str2 "hello") (beg2 0) (end2 (string-length str2)))
	(%string-contains-ci str1 beg1 end1 str2 beg2 end2))
    => 5)

  (check
      (let* ((str1 "ciao hello salut") (beg1 0) (end1 (string-length str1))
	     (str2 "HOLA") (beg2 0) (end2 (string-length str2)))
	(%string-contains-ci str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "ciao hello salut") (beg1 0) (end1 (string-length str1))
	     (str2 "") (beg2 0) (end2 (string-length str2)))
	(%string-contains-ci str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 "") (beg1 0) (end1 (string-length str1))
	     (str2 "hello") (beg2 0) (end2 (string-length str2)))
	(%string-contains-ci str1 beg1 end1 str2 beg2 end2))
    => #f)

  )


(parameterise ((check-test-name 'filling))

  (check
      (let* ((str (string-copy "abcd")) (beg 0) (end (string-length str)))
	(%string-fill*! #\b str beg end)
	str)
    => "bbbb")

  (check
      (let* ((str (string-copy "accd")))
	(%string-fill*! #\b str 1 3)
	str)
    => "abbd")

  (check
      (let* ((str (string-copy "")))
	(%string-fill*! #\b str 0 0)
	str)
    => "")

  )


;;;; done

(check-report)

;;; end of file
