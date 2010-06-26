;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for the bytevectors u8 library
;;;Date: Sat Jun 26, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (bytevectors u8low)
  (char-sets)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing bytevectors u8 - low level\n")


(parameterise ((check-test-name 'predicates))

  (check
      (bytevector-u8-null? "ciao")
    => #f)

  (check
      (bytevector-u8-null? "")
    => #t)

;;; --------------------------------------------------------------------

  (check
      (guard (exc ((assertion-violation? exc)
		   (condition-who exc)))
	(%bytevector-u8-every 123 "abc" 0 2))
    => '%bytevector-u8-every)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aaaa")
	     (beg 0)
	     (end (bytevector-u8-length str)))
	(%bytevector-u8-every #\a str beg end))
    => #t)

  (check
      (let* ((str "aaaab")
	     (beg 0)
	     (end (bytevector-u8-length str)))
	(%bytevector-u8-every #\a str beg end))
    => #f)

  (check
      (let* ((str "aabaa")
	     (beg 0)
	     (end (bytevector-u8-length str)))
	(%bytevector-u8-every #\a str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aaaa")
	     (beg 0)
	     (end (bytevector-u8-length str)))
	(%bytevector-u8-every (char-set #\a) str beg end))
    => #t)

  (check
      (let* ((str "aaaab")
	     (beg 0)
	     (end (bytevector-u8-length str)))
	(%bytevector-u8-every (char-set #\a) str beg end))
    => #f)

  (check
      (let* ((str "aabaa")
	     (beg 0)
	     (end (bytevector-u8-length str)))
	(%bytevector-u8-every (char-set #\a) str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aaaa")
	     (beg 0)
	     (end (bytevector-u8-length str)))
	(%bytevector-u8-every char-alphabetic? str beg end))
    => #t)

  (check
      (let* ((str "aaaa2")
	     (beg 0)
	     (end (bytevector-u8-length str)))
	(%bytevector-u8-every char-alphabetic? str beg end))
    => #f)

  (check
      (let* ((str "aa2aa")
	     (beg 0)
	     (end (bytevector-u8-length str)))
	(%bytevector-u8-every char-alphabetic? str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (guard (exc ((assertion-violation? exc)
		   (condition-who exc)))
	(%bytevector-u8-any 123 "abc" 0 2))
    => '%bytevector-u8-any)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "ddadd")
	     (beg 0)
	     (end (bytevector-u8-length str)))
	(%bytevector-u8-any #\a str beg end))
    => #t)

  (check
      (let* ((str "dddda")
	     (beg 0)
	     (end (bytevector-u8-length str)))
	(%bytevector-u8-any #\a str beg end))
    => #t)

  (check
      (let* ((str "ddd")
	     (beg 0)
	     (end (bytevector-u8-length str)))
	(%bytevector-u8-any #\a str beg end))
    => #f)


;;; --------------------------------------------------------------------

  (check
      (let* ((str "dddaddd")
	     (beg 0)
	     (end (bytevector-u8-length str)))
	(%bytevector-u8-any (char-set #\a) str beg end))
    => #t)

  (check
      (let* ((str "ddda")
	     (beg 0)
	     (end (bytevector-u8-length str)))
	(%bytevector-u8-any (char-set #\a) str beg end))
    => #t)

  (check
      (let* ((str "dddd")
	     (beg 0)
	     (end (bytevector-u8-length str)))
	(%bytevector-u8-any (char-set #\a) str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "11a11")
	     (beg 0)
	     (end (bytevector-u8-length str)))
	(%bytevector-u8-any char-alphabetic? str beg end))
    => #t)

  (check
      (let* ((str "11111a")
	     (beg 0)
	     (end (bytevector-u8-length str)))
	(%bytevector-u8-any char-alphabetic? str beg end))
    => #t)

  (check
      (let* ((str "1111")
	     (beg 0)
	     (end (bytevector-u8-length str)))
	(%bytevector-u8-any char-alphabetic? str beg end))
    => #f)

  )


(parameterise ((check-test-name 'comparison-lexicographic-case-sensitive))

  (check
      (let* ((str1 "abcdefg") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "abcd123") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-compare str1 beg1 end1 str2 beg2 end2 values values values))
    => 4)

  (check
      (let* ((str1 "abcdef") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "abcd123") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-compare str1 beg1 end1 str2 beg2 end2 values values values))
    => 4)

  (check
      (let* ((str1 "efg") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "123") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-compare str1 beg1 end1 str2 beg2 end2 values values values))
    => 0)

  (check
      (let* ((str1 "") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "abcd") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-compare str1 beg1 end1 str2 beg2 end2 values values values))
    => 0)

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-compare str1 beg1 end1 str2 beg2 end2 values values values))
    => 0)

  (check
      (let* ((str1 "abcdA") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "abcdA") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-compare str1 beg1 end1 str2 beg2 end2
			 (lambda (idx) 'less) (lambda (idx) 'equal) (lambda (idx) 'greater)))
    => 'equal)

  (check
      (let* ((str1 "abcdA") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "abcdB") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-compare str1 beg1 end1 str2 beg2 end2
			 (lambda (idx) 'less) (lambda (idx) 'equal) (lambda (idx) 'greater)))
    => 'less)

  (check
      (let* ((str1 "abcdB") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "abcdA") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-compare str1 beg1 end1 str2 beg2 end2
			 (lambda (idx) 'less) (lambda (idx) 'equal) (lambda (idx) 'greater)))
    => 'greater)

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((str "abcd")
	  (beg1 0) (end1 (bytevector-u8-length str))
	  (beg2 0) (end2 (bytevector-u8-length str)))
     (%bytevector-u8= str beg1 end1 str beg2 end2)))

  (check-for-true
   (let* ((str1 "12abcd") (beg1 2) (end1 (bytevector-u8-length str1))
	  (str2 "abcd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8= str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "abc") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "abcd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8= str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "abc") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8= str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "ABcd") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "abcd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8= str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "a2cd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8= str1 beg1 end1 str2 beg2 end2)))

;;; --------------------------------------------------------------------

  (check-for-false
   (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "abcd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8<> str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "abc") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "abcd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8<> str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "abc") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8<> str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "ABcd") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "abcd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8<> str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "a2cd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8<> str1 beg1 end1 str2 beg2 end2)))

;;; --------------------------------------------------------------------

  ;;STR1 is less than STR2:
  ;;
  ;;* If the character at the  mismatch index from STR1 is less than the
  ;;  character at the mismatch index from STR2.
  ;;
  ;;* The bytevector-u8s are equal up to the end of STR1 and STR2 is longer.

  (check-for-false
   (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "abcd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8< str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "abc")  (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "abcd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8< str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "abc") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8< str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "ABcd") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "abcd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8< str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "ABCD") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "abcd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8< str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "ABCD") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "A2CD") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8< str1 beg1 end1 str2 beg2 end2)))

;;; --------------------------------------------------------------------

  ;;STR1 is less than, or equal to, STR2:
  ;;
  ;;* If the bytevector-u8s are equal.
  ;;
  ;;* If the character at the  mismatch index from STR1 is less than the
  ;;  character at the mismatch index from STR2.
  ;;
  ;;* The bytevector-u8s are equal up to the end of STR1 and STR2 is longer.

  (check-for-true
   (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "abcd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8<= str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "abc") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "abcd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8<= str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "abc")  (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8<= str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "ABcd") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "abcd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8<= str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "a2cd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8<= str1 beg1 end1 str2 beg2 end2)))

;;; --------------------------------------------------------------------

  ;;STR1 is greater than STR2:
  ;;
  ;;* If the  character at the mismatch index from  STR1 is greater than
  ;;  the character at the mismatch index from STR2.
  ;;
  ;;* The bytevector-u8s are equal up to the end of STR2 and STR1 is longer.

  (check-for-false
   (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "abcd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8> str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "abc") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8> str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "abc") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "abcd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8> str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "ABcd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8> str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "a2cd") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "abcd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8> str1 beg1 end1 str2 beg2 end2)))

;;; --------------------------------------------------------------------

  ;;STR1 is greater than, or equal to, STR2:
  ;;
  ;;* If the bytevector-u8s are equal.
  ;;
  ;;* If the  character at the mismatch index from  STR1 is greater than
  ;;  the character at the mismatch index from STR2.
  ;;
  ;;* The bytevector-u8s are equal up to the end of STR2 and STR1 is longer.

  (check-for-true
   (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "abcd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8>= str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "abc") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8>= str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "abc") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "abcd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8>= str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "ABcd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8>= str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "ABCD") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8>= str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "a2cd") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "abcd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8>= str1 beg1 end1 str2 beg2 end2)))

  #t)


(parameterise ((check-test-name 'comparison-lexicographic-case-insensitive))

  (check
      (let* ((str1 "aBcdefg") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "abcd123") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-compare-ci str1 beg1 end1 str2 beg2 end2 values values values))
    => 4)

  (check
      (let* ((str1 "efg") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "123") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-compare-ci str1 beg1 end1 str2 beg2 end2 values values values))
    => 0)

  (check
      (let* ((str1 "") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "abcd") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-compare-ci str1 beg1 end1 str2 beg2 end2 values values values))
    => 0)

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-compare-ci str1 beg1 end1 str2 beg2 end2 values values values))
    => 0)

  (check
      (let* ((str1 "abcdA") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "abcda") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-compare-ci str1 beg1 end1 str2 beg2 end2
			    (lambda (idx) 'less) (lambda (idx) 'equal) (lambda (idx) 'greater)))
    => 'equal)

  (check
      (let* ((str1 "abcdA") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "abcdb") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-compare-ci str1 beg1 end1 str2 beg2 end2
			    (lambda (idx) 'less) (lambda (idx) 'equal) (lambda (idx) 'greater)))
    => 'less)

  (check
      (let* ((str1 "abcdb") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "abcdA") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-compare-ci str1 beg1 end1 str2 beg2 end2
			    (lambda (idx) 'less) (lambda (idx) 'equal) (lambda (idx) 'greater)))
    => 'greater)

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((str "abcd")
	  (beg1 0) (end1 (bytevector-u8-length str))
	  (beg2 0) (end2 (bytevector-u8-length str)))
     (%bytevector-u8-ci= str beg1 end1 str beg2 end2)))

  (check-for-true
   (let* ((str1 "12abcd") (beg1 2) (end1 (bytevector-u8-length str1))
	  (str2 "abcd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8-ci= str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "abc") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "abcd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8-ci= str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "abc") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8-ci= str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "ABcd") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "abcd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8-ci= str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "a2cd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8-ci= str1 beg1 end1 str2 beg2 end2)))

;;; --------------------------------------------------------------------

  (check-for-false
   (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "abcd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8-ci<> str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "abc") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "abcd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8-ci<> str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "abc") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8-ci<> str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "ABcd") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "abcd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8-ci<> str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "a2cd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8-ci<> str1 beg1 end1 str2 beg2 end2)))

;;; --------------------------------------------------------------------

  (check-for-false
   (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "abcd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8-ci< str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "abc") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "abcd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8-ci< str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "abc") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8-ci< str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "ABcd") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "abcd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8-ci< str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "a2cd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8-ci< str1 beg1 end1 str2 beg2 end2)))

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "abcd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8-ci<= str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "abc") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "abcd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8-ci<= str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "abc") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8-ci<= str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "ABcd") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "abcd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8-ci<= str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "a2cd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8-ci<= str1 beg1 end1 str2 beg2 end2)))

;;; --------------------------------------------------------------------

  (check-for-false
   (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "abcd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8-ci> str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "abc") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8-ci> str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "abc") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "abcd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8-ci> str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "ABcd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8-ci> str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "a2cd") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "abcd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8-ci> str1 beg1 end1 str2 beg2 end2)))

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "abcd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8-ci>= str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "abc") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8-ci>= str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "abc") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "abcd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8-ci>= str1 beg1 end1 str2 beg2 end2)))

  (check-for-true
   (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "ABcd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8-ci>= str1 beg1 end1 str2 beg2 end2)))

  (check-for-false
   (let* ((str1 "a2cd") (beg1 0) (end1 (bytevector-u8-length str1))
	  (str2 "abcd") (beg2 0) (end2 (bytevector-u8-length str2)))
     (%bytevector-u8-ci>= str1 beg1 end1 str2 beg2 end2)))

  #t)


(parameterise ((check-test-name 'comparison-dictionary-case-sensitive))

  (check (%bytevector-u8-dictionary=? "" "")				=> #t)
  (check (%bytevector-u8-dictionary=? "a" "")				=> #f)
  (check (%bytevector-u8-dictionary=? "" "a")				=> #f)
  (check (%bytevector-u8-dictionary=? "ab" "a")			=> #f)
  (check (%bytevector-u8-dictionary=? "a" "ab")			=> #f)
  (check (%bytevector-u8-dictionary=? "ciao" "ciao")			=> #t)
  (check (%bytevector-u8-dictionary=? "ciao1" "ciao")			=> #f)
  (check (%bytevector-u8-dictionary=? "ciao" "ciao1")			=> #f)

  (check (%bytevector-u8-dictionary=? "ci ao" "ciao")			=> #t)
  (check (%bytevector-u8-dictionary=? "ciao" "ci ao")			=> #t)
  (check (%bytevector-u8-dictionary=? "ci\tao" "ciao")			=> #t)
  (check (%bytevector-u8-dictionary=? "ciao" "ci\tao")			=> #t)
  (check (%bytevector-u8-dictionary=? "ci\nao" "ciao")			=> #t)
  (check (%bytevector-u8-dictionary=? "ciao" "ci\nao")			=> #t)
  (check (%bytevector-u8-dictionary=? "ci\vao" "ciao")			=> #t)
  (check (%bytevector-u8-dictionary=? "ciao" "ci\tao")			=> #t)
  (check (%bytevector-u8-dictionary=? "ci\fao" "ciao")			=> #t)
  (check (%bytevector-u8-dictionary=? "ciao" "ci\fao")			=> #t)
  (check (%bytevector-u8-dictionary=? "ci\rao" "ciao")			=> #t)
  (check (%bytevector-u8-dictionary=? "ciao" "ci\rao")			=> #t)

;;; --------------------------------------------------------------------

  (check (%bytevector-u8-dictionary<? "" "")				=> #f)
  (check (%bytevector-u8-dictionary<? "a" "")				=> #f)
  (check (%bytevector-u8-dictionary<? "" "a")				=> #t)
  (check (%bytevector-u8-dictionary<? "ab" "a")			=> #f)
  (check (%bytevector-u8-dictionary<? "a" "ab")			=> #t)
  (check (%bytevector-u8-dictionary<? "ciao" "ciao")			=> #f)
  (check (%bytevector-u8-dictionary<? "ciao1" "ciao")			=> #f)
  (check (%bytevector-u8-dictionary<? "ciao" "ciao1")			=> #t)

  (check (%bytevector-u8-dictionary<? "ci ao" "ciao")			=> #f)
  (check (%bytevector-u8-dictionary<? "ciao" "ci ao")			=> #f)
  (check (%bytevector-u8-dictionary<? "ci\tao" "ciao")			=> #f)
  (check (%bytevector-u8-dictionary<? "ciao" "ci\tao")			=> #f)
  (check (%bytevector-u8-dictionary<? "ci\nao" "ciao")			=> #f)
  (check (%bytevector-u8-dictionary<? "ciao" "ci\nao")			=> #f)
  (check (%bytevector-u8-dictionary<? "ci\vao" "ciao")			=> #f)
  (check (%bytevector-u8-dictionary<? "ciao" "ci\tao")			=> #f)
  (check (%bytevector-u8-dictionary<? "ci\fao" "ciao")			=> #f)
  (check (%bytevector-u8-dictionary<? "ciao" "ci\fao")			=> #f)
  (check (%bytevector-u8-dictionary<? "ci\rao" "ciao")			=> #f)
  (check (%bytevector-u8-dictionary<? "ciao" "ci\rao")			=> #f)

;;; --------------------------------------------------------------------

  (check (%bytevector-u8-dictionary<=? "" "")				=> #t)
  (check (%bytevector-u8-dictionary<=? "a" "")				=> #f)
  (check (%bytevector-u8-dictionary<=? "" "a")				=> #t)
  (check (%bytevector-u8-dictionary<=? "ab" "a")			=> #f)
  (check (%bytevector-u8-dictionary<=? "a" "ab")			=> #t)
  (check (%bytevector-u8-dictionary<=? "ciao" "ciao")			=> #t)
  (check (%bytevector-u8-dictionary<=? "ciao1" "ciao")			=> #f)
  (check (%bytevector-u8-dictionary<=? "ciao" "ciao1")			=> #t)

  (check (%bytevector-u8-dictionary<=? "ci ao" "ciao")			=> #t)
  (check (%bytevector-u8-dictionary<=? "ciao" "ci ao")			=> #t)
  (check (%bytevector-u8-dictionary<=? "ci\tao" "ciao")		=> #t)
  (check (%bytevector-u8-dictionary<=? "ciao" "ci\tao")		=> #t)
  (check (%bytevector-u8-dictionary<=? "ci\nao" "ciao")		=> #t)
  (check (%bytevector-u8-dictionary<=? "ciao" "ci\nao")		=> #t)
  (check (%bytevector-u8-dictionary<=? "ci\vao" "ciao")		=> #t)
  (check (%bytevector-u8-dictionary<=? "ciao" "ci\tao")		=> #t)
  (check (%bytevector-u8-dictionary<=? "ci\fao" "ciao")		=> #t)
  (check (%bytevector-u8-dictionary<=? "ciao" "ci\fao")		=> #t)
  (check (%bytevector-u8-dictionary<=? "ci\rao" "ciao")		=> #t)
  (check (%bytevector-u8-dictionary<=? "ciao" "ci\rao")		=> #t)

;;; --------------------------------------------------------------------

  (check (%bytevector-u8-dictionary>? "" "")				=> #f)
  (check (%bytevector-u8-dictionary>? "a" "")				=> #t)
  (check (%bytevector-u8-dictionary>? "" "a")				=> #f)
  (check (%bytevector-u8-dictionary>? "ab" "a")			=> #t)
  (check (%bytevector-u8-dictionary>? "a" "ab")			=> #f)
  (check (%bytevector-u8-dictionary>? "ciao" "ciao")			=> #f)
  (check (%bytevector-u8-dictionary>? "ciao1" "ciao")			=> #t)
  (check (%bytevector-u8-dictionary>? "ciao" "ciao1")			=> #f)

  (check (%bytevector-u8-dictionary>? "ci ao" "ciao")			=> #f)
  (check (%bytevector-u8-dictionary>? "ciao" "ci ao")			=> #f)
  (check (%bytevector-u8-dictionary>? "ci\tao" "ciao")			=> #f)
  (check (%bytevector-u8-dictionary>? "ciao" "ci\tao")			=> #f)
  (check (%bytevector-u8-dictionary>? "ci\nao" "ciao")			=> #f)
  (check (%bytevector-u8-dictionary>? "ciao" "ci\nao")			=> #f)
  (check (%bytevector-u8-dictionary>? "ci\vao" "ciao")			=> #f)
  (check (%bytevector-u8-dictionary>? "ciao" "ci\tao")			=> #f)
  (check (%bytevector-u8-dictionary>? "ci\fao" "ciao")			=> #f)
  (check (%bytevector-u8-dictionary>? "ciao" "ci\fao")			=> #f)
  (check (%bytevector-u8-dictionary>? "ci\rao" "ciao")			=> #f)
  (check (%bytevector-u8-dictionary>? "ciao" "ci\rao")			=> #f)

;;; --------------------------------------------------------------------

  (check (%bytevector-u8-dictionary>=? "" "")				=> #t)
  (check (%bytevector-u8-dictionary>=? "a" "")				=> #t)
  (check (%bytevector-u8-dictionary>=? "" "a")				=> #f)
  (check (%bytevector-u8-dictionary>=? "ab" "a")			=> #t)
  (check (%bytevector-u8-dictionary>=? "a" "ab")			=> #f)
  (check (%bytevector-u8-dictionary>=? "ciao" "ciao")			=> #t)
  (check (%bytevector-u8-dictionary>=? "ciao1" "ciao")			=> #t)
  (check (%bytevector-u8-dictionary>=? "ciao" "ciao1")			=> #f)

  (check (%bytevector-u8-dictionary>=? "ci ao" "ciao")			=> #t)
  (check (%bytevector-u8-dictionary>=? "ciao" "ci ao")			=> #t)
  (check (%bytevector-u8-dictionary>=? "ci\tao" "ciao")		=> #t)
  (check (%bytevector-u8-dictionary>=? "ciao" "ci\tao")		=> #t)
  (check (%bytevector-u8-dictionary>=? "ci\nao" "ciao")		=> #t)
  (check (%bytevector-u8-dictionary>=? "ciao" "ci\nao")		=> #t)
  (check (%bytevector-u8-dictionary>=? "ci\vao" "ciao")		=> #t)
  (check (%bytevector-u8-dictionary>=? "ciao" "ci\tao")		=> #t)
  (check (%bytevector-u8-dictionary>=? "ci\fao" "ciao")		=> #t)
  (check (%bytevector-u8-dictionary>=? "ciao" "ci\fao")		=> #t)
  (check (%bytevector-u8-dictionary>=? "ci\rao" "ciao")		=> #t)
  (check (%bytevector-u8-dictionary>=? "ciao" "ci\rao")		=> #t)

  #t)


(parameterise ((check-test-name 'comparison-dictionary-case-insensitive))

  (check (%bytevector-u8-dictionary-ci=? "" "")			=> #t)
  (check (%bytevector-u8-dictionary-ci=? "a" "")			=> #f)
  (check (%bytevector-u8-dictionary-ci=? "" "a")			=> #f)
  (check (%bytevector-u8-dictionary-ci=? "ab" "a")			=> #f)
  (check (%bytevector-u8-dictionary-ci=? "a" "ab")			=> #f)
  (check (%bytevector-u8-dictionary-ci=? "ciao" "ciao")		=> #t)
  (check (%bytevector-u8-dictionary-ci=? "ciao1" "ciao")		=> #f)
  (check (%bytevector-u8-dictionary-ci=? "ciao" "ciao1")		=> #f)
  (check (%bytevector-u8-dictionary-ci=? "CIAO" "ciao")		=> #t)
  (check (%bytevector-u8-dictionary-ci=? "CIAO1" "ciao")		=> #f)
  (check (%bytevector-u8-dictionary-ci=? "CIAO" "ciao1")		=> #f)

  (check (%bytevector-u8-dictionary-ci=? "ci ao" "ciao")		=> #t)
  (check (%bytevector-u8-dictionary-ci=? "ciao" "ci ao")		=> #t)
  (check (%bytevector-u8-dictionary-ci=? "ci\tao" "ciao")		=> #t)
  (check (%bytevector-u8-dictionary-ci=? "ciao" "ci\tao")		=> #t)
  (check (%bytevector-u8-dictionary-ci=? "ci\nao" "ciao")		=> #t)
  (check (%bytevector-u8-dictionary-ci=? "ciao" "ci\nao")		=> #t)
  (check (%bytevector-u8-dictionary-ci=? "ci\vao" "ciao")		=> #t)
  (check (%bytevector-u8-dictionary-ci=? "ciao" "ci\tao")		=> #t)
  (check (%bytevector-u8-dictionary-ci=? "ci\fao" "ciao")		=> #t)
  (check (%bytevector-u8-dictionary-ci=? "ciao" "ci\fao")		=> #t)
  (check (%bytevector-u8-dictionary-ci=? "ci\rao" "ciao")		=> #t)
  (check (%bytevector-u8-dictionary-ci=? "ciao" "ci\rao")		=> #t)

;;; --------------------------------------------------------------------

  (check (%bytevector-u8-dictionary-ci<? "" "")			=> #f)
  (check (%bytevector-u8-dictionary-ci<? "a" "")			=> #f)
  (check (%bytevector-u8-dictionary-ci<? "" "a")			=> #t)
  (check (%bytevector-u8-dictionary-ci<? "ab" "a")			=> #f)
  (check (%bytevector-u8-dictionary-ci<? "a" "ab")			=> #t)
  (check (%bytevector-u8-dictionary-ci<? "ciao" "ciao")		=> #f)
  (check (%bytevector-u8-dictionary-ci<? "ciao1" "ciao")		=> #f)
  (check (%bytevector-u8-dictionary-ci<? "ciao" "ciao1")		=> #t)
  (check (%bytevector-u8-dictionary-ci<? "CIAO" "ciao")		=> #f)
  (check (%bytevector-u8-dictionary-ci<? "CIAO1" "ciao")		=> #f)
  (check (%bytevector-u8-dictionary-ci<? "CIAO" "ciao1")		=> #t)

  (check (%bytevector-u8-dictionary-ci<? "ci ao" "ciao")		=> #f)
  (check (%bytevector-u8-dictionary-ci<? "ciao" "ci ao")		=> #f)
  (check (%bytevector-u8-dictionary-ci<? "ci\tao" "ciao")		=> #f)
  (check (%bytevector-u8-dictionary-ci<? "ciao" "ci\tao")		=> #f)
  (check (%bytevector-u8-dictionary-ci<? "ci\nao" "ciao")		=> #f)
  (check (%bytevector-u8-dictionary-ci<? "ciao" "ci\nao")		=> #f)
  (check (%bytevector-u8-dictionary-ci<? "ci\vao" "ciao")		=> #f)
  (check (%bytevector-u8-dictionary-ci<? "ciao" "ci\tao")		=> #f)
  (check (%bytevector-u8-dictionary-ci<? "ci\fao" "ciao")		=> #f)
  (check (%bytevector-u8-dictionary-ci<? "ciao" "ci\fao")		=> #f)
  (check (%bytevector-u8-dictionary-ci<? "ci\rao" "ciao")		=> #f)
  (check (%bytevector-u8-dictionary-ci<? "ciao" "ci\rao")		=> #f)

;;; --------------------------------------------------------------------

  (check (%bytevector-u8-dictionary-ci<=? "" "")			=> #t)
  (check (%bytevector-u8-dictionary-ci<=? "a" "")			=> #f)
  (check (%bytevector-u8-dictionary-ci<=? "" "a")			=> #t)
  (check (%bytevector-u8-dictionary-ci<=? "ab" "a")			=> #f)
  (check (%bytevector-u8-dictionary-ci<=? "a" "ab")			=> #t)
  (check (%bytevector-u8-dictionary-ci<=? "ciao" "ciao")		=> #t)
  (check (%bytevector-u8-dictionary-ci<=? "ciao1" "ciao")		=> #f)
  (check (%bytevector-u8-dictionary-ci<=? "ciao" "ciao1")		=> #t)
  (check (%bytevector-u8-dictionary-ci<=? "CIAO" "ciao")		=> #t)
  (check (%bytevector-u8-dictionary-ci<=? "CIAO1" "ciao")		=> #f)
  (check (%bytevector-u8-dictionary-ci<=? "CIAO" "ciao1")		=> #t)

  (check (%bytevector-u8-dictionary-ci<=? "ci ao" "ciao")		=> #t)
  (check (%bytevector-u8-dictionary-ci<=? "ciao" "ci ao")		=> #t)
  (check (%bytevector-u8-dictionary-ci<=? "ci\tao" "ciao")		=> #t)
  (check (%bytevector-u8-dictionary-ci<=? "ciao" "ci\tao")		=> #t)
  (check (%bytevector-u8-dictionary-ci<=? "ci\nao" "ciao")		=> #t)
  (check (%bytevector-u8-dictionary-ci<=? "ciao" "ci\nao")		=> #t)
  (check (%bytevector-u8-dictionary-ci<=? "ci\vao" "ciao")		=> #t)
  (check (%bytevector-u8-dictionary-ci<=? "ciao" "ci\tao")		=> #t)
  (check (%bytevector-u8-dictionary-ci<=? "ci\fao" "ciao")		=> #t)
  (check (%bytevector-u8-dictionary-ci<=? "ciao" "ci\fao")		=> #t)
  (check (%bytevector-u8-dictionary-ci<=? "ci\rao" "ciao")		=> #t)
  (check (%bytevector-u8-dictionary-ci<=? "ciao" "ci\rao")		=> #t)

;;; --------------------------------------------------------------------

  (check (%bytevector-u8-dictionary-ci>? "" "")			=> #f)
  (check (%bytevector-u8-dictionary-ci>? "a" "")			=> #t)
  (check (%bytevector-u8-dictionary-ci>? "" "a")			=> #f)
  (check (%bytevector-u8-dictionary-ci>? "ab" "a")			=> #t)
  (check (%bytevector-u8-dictionary-ci>? "a" "ab")			=> #f)
  (check (%bytevector-u8-dictionary-ci>? "ciao" "ciao")		=> #f)
  (check (%bytevector-u8-dictionary-ci>? "ciao1" "ciao")		=> #t)
  (check (%bytevector-u8-dictionary-ci>? "ciao" "ciao1")		=> #f)
  (check (%bytevector-u8-dictionary-ci>? "CIAO" "ciao")		=> #f)
  (check (%bytevector-u8-dictionary-ci>? "CIAO1" "ciao")		=> #t)
  (check (%bytevector-u8-dictionary-ci>? "CIAO" "ciao1")		=> #f)

  (check (%bytevector-u8-dictionary-ci>? "ci ao" "ciao")		=> #f)
  (check (%bytevector-u8-dictionary-ci>? "ciao" "ci ao")		=> #f)
  (check (%bytevector-u8-dictionary-ci>? "ci\tao" "ciao")		=> #f)
  (check (%bytevector-u8-dictionary-ci>? "ciao" "ci\tao")		=> #f)
  (check (%bytevector-u8-dictionary-ci>? "ci\nao" "ciao")		=> #f)
  (check (%bytevector-u8-dictionary-ci>? "ciao" "ci\nao")		=> #f)
  (check (%bytevector-u8-dictionary-ci>? "ci\vao" "ciao")		=> #f)
  (check (%bytevector-u8-dictionary-ci>? "ciao" "ci\tao")		=> #f)
  (check (%bytevector-u8-dictionary-ci>? "ci\fao" "ciao")		=> #f)
  (check (%bytevector-u8-dictionary-ci>? "ciao" "ci\fao")		=> #f)
  (check (%bytevector-u8-dictionary-ci>? "ci\rao" "ciao")		=> #f)
  (check (%bytevector-u8-dictionary-ci>? "ciao" "ci\rao")		=> #f)

;;; --------------------------------------------------------------------

  (check (%bytevector-u8-dictionary-ci>=? "" "")			=> #t)
  (check (%bytevector-u8-dictionary-ci>=? "a" "")			=> #t)
  (check (%bytevector-u8-dictionary-ci>=? "" "a")			=> #f)
  (check (%bytevector-u8-dictionary-ci>=? "ab" "a")			=> #t)
  (check (%bytevector-u8-dictionary-ci>=? "a" "ab")			=> #f)
  (check (%bytevector-u8-dictionary-ci>=? "ciao" "ciao")		=> #t)
  (check (%bytevector-u8-dictionary-ci>=? "ciao1" "ciao")		=> #t)
  (check (%bytevector-u8-dictionary-ci>=? "ciao" "ciao1")		=> #f)
  (check (%bytevector-u8-dictionary-ci>=? "CIAO" "ciao")		=> #t)
  (check (%bytevector-u8-dictionary-ci>=? "CIAO1" "ciao")		=> #t)
  (check (%bytevector-u8-dictionary-ci>=? "CIAO" "ciao1")		=> #f)

  (check (%bytevector-u8-dictionary-ci>=? "ci ao" "ciao")		=> #t)
  (check (%bytevector-u8-dictionary-ci>=? "ciao" "ci ao")		=> #t)
  (check (%bytevector-u8-dictionary-ci>=? "ci\tao" "ciao")		=> #t)
  (check (%bytevector-u8-dictionary-ci>=? "ciao" "ci\tao")		=> #t)
  (check (%bytevector-u8-dictionary-ci>=? "ci\nao" "ciao")		=> #t)
  (check (%bytevector-u8-dictionary-ci>=? "ciao" "ci\nao")		=> #t)
  (check (%bytevector-u8-dictionary-ci>=? "ci\vao" "ciao")		=> #t)
  (check (%bytevector-u8-dictionary-ci>=? "ciao" "ci\tao")		=> #t)
  (check (%bytevector-u8-dictionary-ci>=? "ci\fao" "ciao")		=> #t)
  (check (%bytevector-u8-dictionary-ci>=? "ciao" "ci\fao")		=> #t)
  (check (%bytevector-u8-dictionary-ci>=? "ci\rao" "ciao")		=> #t)
  (check (%bytevector-u8-dictionary-ci>=? "ciao" "ci\rao")		=> #t)

  #t)


(parameterise ((check-test-name 'comparison-lexicographic-bytevector-u8/number-case-sensitive))

  (check (%bytevector-u8/numbers=? "" "")				=> #t)
  (check (%bytevector-u8/numbers=? "a" "")				=> #f)
  (check (%bytevector-u8/numbers=? "" "a")				=> #f)
  (check (%bytevector-u8/numbers=? "a" "a")				=> #t)
  (check (%bytevector-u8/numbers=? "1" "")				=> #f)
  (check (%bytevector-u8/numbers=? "" "1")				=> #f)
  (check (%bytevector-u8/numbers=? "1" "1")				=> #t)
  (check (%bytevector-u8/numbers=? "1" "2")				=> #f)
  (check (%bytevector-u8/numbers=? "2" "1")				=> #f)
  (check (%bytevector-u8/numbers=? "a" "ab")				=> #f)
  (check (%bytevector-u8/numbers=? "ab" "a")				=> #f)
  (check (%bytevector-u8/numbers=? "a" "a1")				=> #f)
  (check (%bytevector-u8/numbers=? "a1" "a")				=> #f)
  (check (%bytevector-u8/numbers=? "1" "1a")				=> #f)
  (check (%bytevector-u8/numbers=? "1a" "1")				=> #f)

  (check (%bytevector-u8/numbers=? "123" "45")				=> #f)
  (check (%bytevector-u8/numbers=? "45" "123")				=> #f)
  (check (%bytevector-u8/numbers=? "ciao3" "ciao10")			=> #f)
  (check (%bytevector-u8/numbers=? "ciao10" "ciao3")			=> #f)
  (check (%bytevector-u8/numbers=? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (%bytevector-u8/numbers=? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (%bytevector-u8/numbers=? "foo4bar3zab" "foo4bar10")		=> #f)
  (check (%bytevector-u8/numbers=? "foo4bar10" "foo4bar3zab")		=> #f)
  (check (%bytevector-u8/numbers=? "foo12" "12foo")			=> #f)
  (check (%bytevector-u8/numbers=? "12foo" "foo12")			=> #f)
  (check (%bytevector-u8/numbers=? "12bar" "foobar")			=> #f)
  (check (%bytevector-u8/numbers=? "12.3" "12.3")			=> #t)
  (check (%bytevector-u8/numbers=? "12.3" "12.10")			=> #f)
  (check (%bytevector-u8/numbers=? "12.10" "12.3")			=> #f)
  (check (%bytevector-u8/numbers=? "12.3" "12,10")			=> #f)
  (check (%bytevector-u8/numbers=? "12,10" "12.3")			=> #f)

;;; --------------------------------------------------------------------

  (check (%bytevector-u8/numbers<>? "" "")				=> #f)
  (check (%bytevector-u8/numbers<>? "a" "")				=> #t)
  (check (%bytevector-u8/numbers<>? "" "a")				=> #t)
  (check (%bytevector-u8/numbers<>? "a" "a")				=> #f)
  (check (%bytevector-u8/numbers<>? "1" "")				=> #t)
  (check (%bytevector-u8/numbers<>? "" "1")				=> #t)
  (check (%bytevector-u8/numbers<>? "1" "1")				=> #f)
  (check (%bytevector-u8/numbers<>? "1" "2")				=> #t)
  (check (%bytevector-u8/numbers<>? "2" "1")				=> #t)
  (check (%bytevector-u8/numbers<>? "a" "ab")				=> #t)
  (check (%bytevector-u8/numbers<>? "ab" "a")				=> #t)
  (check (%bytevector-u8/numbers<>? "a" "a1")				=> #t)
  (check (%bytevector-u8/numbers<>? "a1" "a")				=> #t)
  (check (%bytevector-u8/numbers<>? "1" "1a")				=> #t)
  (check (%bytevector-u8/numbers<>? "1a" "1")				=> #t)

  (check (%bytevector-u8/numbers<>? "123" "45")			=> #t)
  (check (%bytevector-u8/numbers<>? "45" "123")			=> #t)
  (check (%bytevector-u8/numbers<>? "ciao3" "ciao10")			=> #t)
  (check (%bytevector-u8/numbers<>? "ciao10" "ciao3")			=> #t)
  (check (%bytevector-u8/numbers<>? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (%bytevector-u8/numbers<>? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (%bytevector-u8/numbers<>? "foo4bar3zab" "foo4bar10")		=> #t)
  (check (%bytevector-u8/numbers<>? "foo4bar10" "foo4bar3zab")		=> #t)
  (check (%bytevector-u8/numbers<>? "foo12" "12foo")			=> #t)
  (check (%bytevector-u8/numbers<>? "12foo" "foo12")			=> #t)
  (check (%bytevector-u8/numbers<>? "12bar" "foobar")			=> #t)
  (check (%bytevector-u8/numbers<>? "12.3" "12.3")			=> #f)
  (check (%bytevector-u8/numbers<>? "12.3" "12.10")			=> #t)
  (check (%bytevector-u8/numbers<>? "12.10" "12.3")			=> #t)
  (check (%bytevector-u8/numbers<>? "12.3" "12,10")			=> #t)
  (check (%bytevector-u8/numbers<>? "12,10" "12.3")			=> #t)

;;; --------------------------------------------------------------------

  (check (%bytevector-u8/numbers<? "" "")				=> #f)
  (check (%bytevector-u8/numbers<? "a" "")				=> #f)
  (check (%bytevector-u8/numbers<? "" "a")				=> #t)
  (check (%bytevector-u8/numbers<? "a" "a")				=> #f)
  (check (%bytevector-u8/numbers<? "1" "")				=> #f)
  (check (%bytevector-u8/numbers<? "" "1")				=> #t)
  (check (%bytevector-u8/numbers<? "1" "1")				=> #f)
  (check (%bytevector-u8/numbers<? "1" "2")				=> #t)
  (check (%bytevector-u8/numbers<? "2" "1")				=> #f)
  (check (%bytevector-u8/numbers<? "a" "ab")				=> #t)
  (check (%bytevector-u8/numbers<? "ab" "a")				=> #f)
  (check (%bytevector-u8/numbers<? "a" "a1")				=> #t)
  (check (%bytevector-u8/numbers<? "a1" "a")				=> #f)
  (check (%bytevector-u8/numbers<? "1" "1a")				=> #t)
  (check (%bytevector-u8/numbers<? "1a" "1")				=> #f)

  (check (%bytevector-u8/numbers<? "123" "45")				=> #f)
  (check (%bytevector-u8/numbers<? "45" "123")				=> #t)
  (check (%bytevector-u8/numbers<? "ciao3" "ciao10")			=> #t)
  (check (%bytevector-u8/numbers<? "ciao10" "ciao3")			=> #f)
  (check (%bytevector-u8/numbers<? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (%bytevector-u8/numbers<? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (%bytevector-u8/numbers<? "foo4bar3zab" "foo4bar10")		=> #t)
  (check (%bytevector-u8/numbers<? "foo4bar10" "foo4bar3zab")		=> #f)
  (check (%bytevector-u8/numbers<? "foo12" "12foo")			=> #f)
  (check (%bytevector-u8/numbers<? "12foo" "foo12")			=> #t)
  (check (%bytevector-u8/numbers<? "12bar" "foobar")			=> #t)
  (check (%bytevector-u8/numbers<? "12.3" "12.3")			=> #f)
  (check (%bytevector-u8/numbers<? "12.3" "12.10")			=> #t)
  (check (%bytevector-u8/numbers<? "12.10" "12.3")			=> #f)
  (check (%bytevector-u8/numbers<? "12.3" "12,10")			=> #f)
  (check (%bytevector-u8/numbers<? "12,10" "12.3")			=> #t)

;;; --------------------------------------------------------------------

  (check (%bytevector-u8/numbers<=? "" "")				=> #t)
  (check (%bytevector-u8/numbers<=? "a" "")				=> #f)
  (check (%bytevector-u8/numbers<=? "" "a")				=> #t)
  (check (%bytevector-u8/numbers<=? "a" "a")				=> #t)
  (check (%bytevector-u8/numbers<=? "1" "")				=> #f)
  (check (%bytevector-u8/numbers<=? "" "1")				=> #t)
  (check (%bytevector-u8/numbers<=? "1" "1")				=> #t)
  (check (%bytevector-u8/numbers<=? "1" "2")				=> #t)
  (check (%bytevector-u8/numbers<=? "2" "1")				=> #f)
  (check (%bytevector-u8/numbers<=? "a" "ab")				=> #t)
  (check (%bytevector-u8/numbers<=? "ab" "a")				=> #f)
  (check (%bytevector-u8/numbers<=? "a" "a1")				=> #t)
  (check (%bytevector-u8/numbers<=? "a1" "a")				=> #f)
  (check (%bytevector-u8/numbers<=? "1" "1a")				=> #t)
  (check (%bytevector-u8/numbers<=? "1a" "1")				=> #f)

  (check (%bytevector-u8/numbers<=? "123" "45")			=> #f)
  (check (%bytevector-u8/numbers<=? "45" "123")			=> #t)
  (check (%bytevector-u8/numbers<=? "ciao3" "ciao10")			=> #t)
  (check (%bytevector-u8/numbers<=? "ciao10" "ciao3")			=> #f)
  (check (%bytevector-u8/numbers<=? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (%bytevector-u8/numbers<=? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (%bytevector-u8/numbers<=? "foo4bar3zab" "foo4bar10")		=> #t)
  (check (%bytevector-u8/numbers<=? "foo4bar10" "foo4bar3zab")		=> #f)
  (check (%bytevector-u8/numbers<=? "foo12" "12foo")			=> #f)
  (check (%bytevector-u8/numbers<=? "12foo" "foo12")			=> #t)
  (check (%bytevector-u8/numbers<=? "12bar" "foobar")			=> #t)
  (check (%bytevector-u8/numbers<=? "12.3" "12.3")			=> #t)
  (check (%bytevector-u8/numbers<=? "12.3" "12.10")			=> #t)
  (check (%bytevector-u8/numbers<=? "12.10" "12.3")			=> #f)
  (check (%bytevector-u8/numbers<=? "12.3" "12,10")			=> #f)
  (check (%bytevector-u8/numbers<=? "12,10" "12.3")			=> #t)

;;; --------------------------------------------------------------------

  (check (%bytevector-u8/numbers>? "" "")				=> #f)
  (check (%bytevector-u8/numbers>? "a" "")				=> #t)
  (check (%bytevector-u8/numbers>? "" "a")				=> #f)
  (check (%bytevector-u8/numbers>? "a" "a")				=> #f)
  (check (%bytevector-u8/numbers>? "1" "")				=> #t)
  (check (%bytevector-u8/numbers>? "" "1")				=> #f)
  (check (%bytevector-u8/numbers>? "1" "1")				=> #f)
  (check (%bytevector-u8/numbers>? "1" "2")				=> #f)
  (check (%bytevector-u8/numbers>? "2" "1")				=> #t)
  (check (%bytevector-u8/numbers>? "a" "ab")				=> #f)
  (check (%bytevector-u8/numbers>? "ab" "a")				=> #t)
  (check (%bytevector-u8/numbers>? "a" "a1")				=> #f)
  (check (%bytevector-u8/numbers>? "a1" "a")				=> #t)
  (check (%bytevector-u8/numbers>? "1" "1a")				=> #f)
  (check (%bytevector-u8/numbers>? "1a" "1")				=> #t)

  (check (%bytevector-u8/numbers>? "123" "45")				=> #t)
  (check (%bytevector-u8/numbers>? "45" "123")				=> #f)
  (check (%bytevector-u8/numbers>? "ciao3" "ciao10")			=> #f)
  (check (%bytevector-u8/numbers>? "ciao10" "ciao3")			=> #t)
  (check (%bytevector-u8/numbers>? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (%bytevector-u8/numbers>? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (%bytevector-u8/numbers>? "foo4bar3zab" "foo4bar10")		=> #f)
  (check (%bytevector-u8/numbers>? "foo4bar10" "foo4bar3zab")		=> #t)
  (check (%bytevector-u8/numbers>? "foo12" "12foo")			=> #t)
  (check (%bytevector-u8/numbers>? "12foo" "foo12")			=> #f)
  (check (%bytevector-u8/numbers>? "12bar" "foobar")			=> #f)
  (check (%bytevector-u8/numbers>? "12.3" "12.3")			=> #f)
  (check (%bytevector-u8/numbers>? "12.3" "12.10")			=> #f)
  (check (%bytevector-u8/numbers>? "12.10" "12.3")			=> #t)
  (check (%bytevector-u8/numbers>? "12.3" "12,10")			=> #t)
  (check (%bytevector-u8/numbers>? "12,10" "12.3")			=> #f)

;;; --------------------------------------------------------------------

  (check (%bytevector-u8/numbers>=? "" "")				=> #t)
  (check (%bytevector-u8/numbers>=? "a" "")				=> #t)
  (check (%bytevector-u8/numbers>=? "" "a")				=> #f)
  (check (%bytevector-u8/numbers>=? "a" "a")				=> #t)
  (check (%bytevector-u8/numbers>=? "1" "")				=> #t)
  (check (%bytevector-u8/numbers>=? "" "1")				=> #f)
  (check (%bytevector-u8/numbers>=? "1" "1")				=> #t)
  (check (%bytevector-u8/numbers>=? "1" "2")				=> #f)
  (check (%bytevector-u8/numbers>=? "2" "1")				=> #t)
  (check (%bytevector-u8/numbers>=? "a" "ab")				=> #f)
  (check (%bytevector-u8/numbers>=? "ab" "a")				=> #t)
  (check (%bytevector-u8/numbers>=? "a" "a1")				=> #f)
  (check (%bytevector-u8/numbers>=? "a1" "a")				=> #t)
  (check (%bytevector-u8/numbers>=? "1" "1a")				=> #f)
  (check (%bytevector-u8/numbers>=? "1a" "1")				=> #t)

  (check (%bytevector-u8/numbers>=? "123" "45")			=> #t)
  (check (%bytevector-u8/numbers>=? "45" "123")			=> #f)
  (check (%bytevector-u8/numbers>=? "ciao3" "ciao10")			=> #f)
  (check (%bytevector-u8/numbers>=? "ciao10" "ciao3")			=> #t)
  (check (%bytevector-u8/numbers>=? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (%bytevector-u8/numbers>=? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (%bytevector-u8/numbers>=? "foo4bar3zab" "foo4bar10")		=> #f)
  (check (%bytevector-u8/numbers>=? "foo4bar10" "foo4bar3zab")		=> #t)
  (check (%bytevector-u8/numbers>=? "foo12" "12foo")			=> #t)
  (check (%bytevector-u8/numbers>=? "12foo" "foo12")			=> #f)
  (check (%bytevector-u8/numbers>=? "12bar" "foobar")			=> #f)
  (check (%bytevector-u8/numbers>=? "12.3" "12.3")			=> #t)
  (check (%bytevector-u8/numbers>=? "12.3" "12.10")			=> #f)
  (check (%bytevector-u8/numbers>=? "12.10" "12.3")			=> #t)
  (check (%bytevector-u8/numbers>=? "12.3" "12,10")			=> #t)
  (check (%bytevector-u8/numbers>=? "12,10" "12.3")			=> #f)

;;; --------------------------------------------------------------------

  (check
      (list-sort %bytevector-u8/numbers<? (quote ("foo123" "foo42" "foo7")))
    => '("foo7" "foo42" "foo123"))

  #t)


(parameterise ((check-test-name 'comparison-lexicographic-bytevector-u8/number-case-insensitive))

  (check (%bytevector-u8/numbers-ci=? "" "")				=> #t)
  (check (%bytevector-u8/numbers-ci=? "a" "")				=> #f)
  (check (%bytevector-u8/numbers-ci=? "" "a")				=> #f)
  (check (%bytevector-u8/numbers-ci=? "a" "a")				=> #t)
  (check (%bytevector-u8/numbers-ci=? "1" "")				=> #f)
  (check (%bytevector-u8/numbers-ci=? "" "1")				=> #f)
  (check (%bytevector-u8/numbers-ci=? "1" "1")				=> #t)
  (check (%bytevector-u8/numbers-ci=? "1" "2")				=> #f)
  (check (%bytevector-u8/numbers-ci=? "2" "1")				=> #f)
  (check (%bytevector-u8/numbers-ci=? "a" "ab")			=> #f)
  (check (%bytevector-u8/numbers-ci=? "ab" "a")			=> #f)
  (check (%bytevector-u8/numbers-ci=? "a" "a1")			=> #f)
  (check (%bytevector-u8/numbers-ci=? "a1" "a")			=> #f)
  (check (%bytevector-u8/numbers-ci=? "1" "1a")			=> #f)
  (check (%bytevector-u8/numbers-ci=? "1a" "1")			=> #f)
  (check (%bytevector-u8/numbers-ci=? "a" "A")				=> #t)
  (check (%bytevector-u8/numbers-ci=? "A" "a")				=> #t)

  (check (%bytevector-u8/numbers-ci=? "123" "45")			=> #f)
  (check (%bytevector-u8/numbers-ci=? "45" "123")			=> #f)
  (check (%bytevector-u8/numbers-ci=? "ciao3" "ciao10")		=> #f)
  (check (%bytevector-u8/numbers-ci=? "ciao10" "ciao3")		=> #f)
  (check (%bytevector-u8/numbers-ci=? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (%bytevector-u8/numbers-ci=? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (%bytevector-u8/numbers-ci=? "foo4bar3zab" "foo4bar10")	=> #f)
  (check (%bytevector-u8/numbers-ci=? "foo4bar10" "foo4bar3zab")	=> #f)
  (check (%bytevector-u8/numbers-ci=? "foo12" "12foo")			=> #f)
  (check (%bytevector-u8/numbers-ci=? "12foo" "foo12")			=> #f)
  (check (%bytevector-u8/numbers-ci=? "12bar" "foobar")		=> #f)
  (check (%bytevector-u8/numbers-ci=? "12.3" "12.3")			=> #t)
  (check (%bytevector-u8/numbers-ci=? "12.3" "12.10")			=> #f)
  (check (%bytevector-u8/numbers-ci=? "12.10" "12.3")			=> #f)
  (check (%bytevector-u8/numbers-ci=? "12.3" "12,10")			=> #f)
  (check (%bytevector-u8/numbers-ci=? "12,10" "12.3")			=> #f)

;;; --------------------------------------------------------------------

  (check (%bytevector-u8/numbers-ci<>? "" "")				=> #f)
  (check (%bytevector-u8/numbers-ci<>? "a" "")				=> #t)
  (check (%bytevector-u8/numbers-ci<>? "" "a")				=> #t)
  (check (%bytevector-u8/numbers-ci<>? "a" "a")			=> #f)
  (check (%bytevector-u8/numbers-ci<>? "1" "")				=> #t)
  (check (%bytevector-u8/numbers-ci<>? "" "1")				=> #t)
  (check (%bytevector-u8/numbers-ci<>? "1" "1")			=> #f)
  (check (%bytevector-u8/numbers-ci<>? "1" "2")			=> #t)
  (check (%bytevector-u8/numbers-ci<>? "2" "1")			=> #t)
  (check (%bytevector-u8/numbers-ci<>? "a" "ab")			=> #t)
  (check (%bytevector-u8/numbers-ci<>? "ab" "a")			=> #t)
  (check (%bytevector-u8/numbers-ci<>? "a" "a1")			=> #t)
  (check (%bytevector-u8/numbers-ci<>? "a1" "a")			=> #t)
  (check (%bytevector-u8/numbers-ci<>? "1" "1a")			=> #t)
  (check (%bytevector-u8/numbers-ci<>? "1a" "1")			=> #t)
  (check (%bytevector-u8/numbers-ci<>? "A" "a")			=> #f)
  (check (%bytevector-u8/numbers-ci<>? "a" "A")			=> #f)

  (check (%bytevector-u8/numbers-ci<>? "123" "45")			=> #t)
  (check (%bytevector-u8/numbers-ci<>? "45" "123")			=> #t)
  (check (%bytevector-u8/numbers-ci<>? "ciao3" "ciao10")		=> #t)
  (check (%bytevector-u8/numbers-ci<>? "ciao10" "ciao3")		=> #t)
  (check (%bytevector-u8/numbers-ci<>? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (%bytevector-u8/numbers-ci<>? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (%bytevector-u8/numbers-ci<>? "foo4bar3zab" "foo4bar10")	=> #t)
  (check (%bytevector-u8/numbers-ci<>? "foo4bar10" "foo4bar3zab")	=> #t)
  (check (%bytevector-u8/numbers-ci<>? "foo12" "12foo")		=> #t)
  (check (%bytevector-u8/numbers-ci<>? "12foo" "foo12")		=> #t)
  (check (%bytevector-u8/numbers-ci<>? "12bar" "foobar")		=> #t)
  (check (%bytevector-u8/numbers-ci<>? "12.3" "12.3")			=> #f)
  (check (%bytevector-u8/numbers-ci<>? "12.3" "12.10")			=> #t)
  (check (%bytevector-u8/numbers-ci<>? "12.10" "12.3")			=> #t)
  (check (%bytevector-u8/numbers-ci<>? "12.3" "12,10")			=> #t)
  (check (%bytevector-u8/numbers-ci<>? "12,10" "12.3")			=> #t)

;;; --------------------------------------------------------------------

  (check (%bytevector-u8/numbers-ci<? "" "")				=> #f)
  (check (%bytevector-u8/numbers-ci<? "a" "")				=> #f)
  (check (%bytevector-u8/numbers-ci<? "" "a")				=> #t)
  (check (%bytevector-u8/numbers-ci<? "a" "a")				=> #f)
  (check (%bytevector-u8/numbers-ci<? "1" "")				=> #f)
  (check (%bytevector-u8/numbers-ci<? "" "1")				=> #t)
  (check (%bytevector-u8/numbers-ci<? "1" "1")				=> #f)
  (check (%bytevector-u8/numbers-ci<? "1" "2")				=> #t)
  (check (%bytevector-u8/numbers-ci<? "2" "1")				=> #f)
  (check (%bytevector-u8/numbers-ci<? "a" "ab")			=> #t)
  (check (%bytevector-u8/numbers-ci<? "ab" "a")			=> #f)
  (check (%bytevector-u8/numbers-ci<? "a" "a1")			=> #t)
  (check (%bytevector-u8/numbers-ci<? "a1" "a")			=> #f)
  (check (%bytevector-u8/numbers-ci<? "1" "1a")			=> #t)
  (check (%bytevector-u8/numbers-ci<? "1a" "1")			=> #f)
  (check (%bytevector-u8/numbers-ci<? "a" "A")				=> #f)
  (check (%bytevector-u8/numbers-ci<? "A" "a")				=> #f)

  (check (%bytevector-u8/numbers-ci<? "123" "45")			=> #f)
  (check (%bytevector-u8/numbers-ci<? "45" "123")			=> #t)
  (check (%bytevector-u8/numbers-ci<? "ciao3" "ciao10")		=> #t)
  (check (%bytevector-u8/numbers-ci<? "ciao10" "ciao3")		=> #f)
  (check (%bytevector-u8/numbers-ci<? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (%bytevector-u8/numbers-ci<? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (%bytevector-u8/numbers-ci<? "foo4bar3zab" "foo4bar10")	=> #t)
  (check (%bytevector-u8/numbers-ci<? "foo4bar10" "foo4bar3zab")	=> #f)
  (check (%bytevector-u8/numbers-ci<? "foo12" "12foo")			=> #f)
  (check (%bytevector-u8/numbers-ci<? "12foo" "foo12")			=> #t)
  (check (%bytevector-u8/numbers-ci<? "12bar" "foobar")		=> #t)
  (check (%bytevector-u8/numbers-ci<? "12.3" "12.3")			=> #f)
  (check (%bytevector-u8/numbers-ci<? "12.3" "12.10")			=> #t)
  (check (%bytevector-u8/numbers-ci<? "12.10" "12.3")			=> #f)
  (check (%bytevector-u8/numbers-ci<? "12.3" "12,10")			=> #f)
  (check (%bytevector-u8/numbers-ci<? "12,10" "12.3")			=> #t)

;;; --------------------------------------------------------------------

  (check (%bytevector-u8/numbers-ci<=? "" "")				=> #t)
  (check (%bytevector-u8/numbers-ci<=? "a" "")				=> #f)
  (check (%bytevector-u8/numbers-ci<=? "" "a")				=> #t)
  (check (%bytevector-u8/numbers-ci<=? "a" "a")			=> #t)
  (check (%bytevector-u8/numbers-ci<=? "1" "")				=> #f)
  (check (%bytevector-u8/numbers-ci<=? "" "1")				=> #t)
  (check (%bytevector-u8/numbers-ci<=? "1" "1")			=> #t)
  (check (%bytevector-u8/numbers-ci<=? "1" "2")			=> #t)
  (check (%bytevector-u8/numbers-ci<=? "2" "1")			=> #f)
  (check (%bytevector-u8/numbers-ci<=? "a" "ab")			=> #t)
  (check (%bytevector-u8/numbers-ci<=? "ab" "a")			=> #f)
  (check (%bytevector-u8/numbers-ci<=? "a" "a1")			=> #t)
  (check (%bytevector-u8/numbers-ci<=? "a1" "a")			=> #f)
  (check (%bytevector-u8/numbers-ci<=? "1" "1a")			=> #t)
  (check (%bytevector-u8/numbers-ci<=? "1a" "1")			=> #f)
  (check (%bytevector-u8/numbers-ci<=? "a" "A")			=> #t)
  (check (%bytevector-u8/numbers-ci<=? "A" "a")			=> #t)

  (check (%bytevector-u8/numbers-ci<=? "123" "45")			=> #f)
  (check (%bytevector-u8/numbers-ci<=? "45" "123")			=> #t)
  (check (%bytevector-u8/numbers-ci<=? "ciao3" "ciao10")		=> #t)
  (check (%bytevector-u8/numbers-ci<=? "ciao10" "ciao3")		=> #f)
  (check (%bytevector-u8/numbers-ci<=? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (%bytevector-u8/numbers-ci<=? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (%bytevector-u8/numbers-ci<=? "foo4bar3zab" "foo4bar10")	=> #t)
  (check (%bytevector-u8/numbers-ci<=? "foo4bar10" "foo4bar3zab")	=> #f)
  (check (%bytevector-u8/numbers-ci<=? "foo12" "12foo")		=> #f)
  (check (%bytevector-u8/numbers-ci<=? "12foo" "foo12")		=> #t)
  (check (%bytevector-u8/numbers-ci<=? "12bar" "foobar")		=> #t)
  (check (%bytevector-u8/numbers-ci<=? "12.3" "12.3")			=> #t)
  (check (%bytevector-u8/numbers-ci<=? "12.3" "12.10")			=> #t)
  (check (%bytevector-u8/numbers-ci<=? "12.10" "12.3")			=> #f)
  (check (%bytevector-u8/numbers-ci<=? "12.3" "12,10")			=> #f)
  (check (%bytevector-u8/numbers-ci<=? "12,10" "12.3")			=> #t)

;;; --------------------------------------------------------------------

  (check (%bytevector-u8/numbers-ci>? "" "")				=> #f)
  (check (%bytevector-u8/numbers-ci>? "a" "")				=> #t)
  (check (%bytevector-u8/numbers-ci>? "" "a")				=> #f)
  (check (%bytevector-u8/numbers-ci>? "a" "a")				=> #f)
  (check (%bytevector-u8/numbers-ci>? "1" "")				=> #t)
  (check (%bytevector-u8/numbers-ci>? "" "1")				=> #f)
  (check (%bytevector-u8/numbers-ci>? "1" "1")				=> #f)
  (check (%bytevector-u8/numbers-ci>? "1" "2")				=> #f)
  (check (%bytevector-u8/numbers-ci>? "2" "1")				=> #t)
  (check (%bytevector-u8/numbers-ci>? "a" "ab")			=> #f)
  (check (%bytevector-u8/numbers-ci>? "ab" "a")			=> #t)
  (check (%bytevector-u8/numbers-ci>? "a" "a1")			=> #f)
  (check (%bytevector-u8/numbers-ci>? "a1" "a")			=> #t)
  (check (%bytevector-u8/numbers-ci>? "1" "1a")			=> #f)
  (check (%bytevector-u8/numbers-ci>? "1a" "1")			=> #t)
  (check (%bytevector-u8/numbers-ci>? "a" "A")				=> #f)
  (check (%bytevector-u8/numbers-ci>? "A" "a")				=> #f)

  (check (%bytevector-u8/numbers-ci>? "123" "45")			=> #t)
  (check (%bytevector-u8/numbers-ci>? "45" "123")			=> #f)
  (check (%bytevector-u8/numbers-ci>? "ciao3" "ciao10")		=> #f)
  (check (%bytevector-u8/numbers-ci>? "ciao10" "ciao3")		=> #t)
  (check (%bytevector-u8/numbers-ci>? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (%bytevector-u8/numbers-ci>? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (%bytevector-u8/numbers-ci>? "foo4bar3zab" "foo4bar10")	=> #f)
  (check (%bytevector-u8/numbers-ci>? "foo4bar10" "foo4bar3zab")	=> #t)
  (check (%bytevector-u8/numbers-ci>? "foo12" "12foo")			=> #t)
  (check (%bytevector-u8/numbers-ci>? "12foo" "foo12")			=> #f)
  (check (%bytevector-u8/numbers-ci>? "12bar" "foobar")		=> #f)
  (check (%bytevector-u8/numbers-ci>? "12.3" "12.3")			=> #f)
  (check (%bytevector-u8/numbers-ci>? "12.3" "12.10")			=> #f)
  (check (%bytevector-u8/numbers-ci>? "12.10" "12.3")			=> #t)
  (check (%bytevector-u8/numbers-ci>? "12.3" "12,10")			=> #t)
  (check (%bytevector-u8/numbers-ci>? "12,10" "12.3")			=> #f)

;;; --------------------------------------------------------------------

  (check (%bytevector-u8/numbers-ci>=? "" "")				=> #t)
  (check (%bytevector-u8/numbers-ci>=? "a" "")				=> #t)
  (check (%bytevector-u8/numbers-ci>=? "" "a")				=> #f)
  (check (%bytevector-u8/numbers-ci>=? "a" "a")			=> #t)
  (check (%bytevector-u8/numbers-ci>=? "1" "")				=> #t)
  (check (%bytevector-u8/numbers-ci>=? "" "1")				=> #f)
  (check (%bytevector-u8/numbers-ci>=? "1" "1")			=> #t)
  (check (%bytevector-u8/numbers-ci>=? "1" "2")			=> #f)
  (check (%bytevector-u8/numbers-ci>=? "2" "1")			=> #t)
  (check (%bytevector-u8/numbers-ci>=? "a" "ab")			=> #f)
  (check (%bytevector-u8/numbers-ci>=? "ab" "a")			=> #t)
  (check (%bytevector-u8/numbers-ci>=? "a" "a1")			=> #f)
  (check (%bytevector-u8/numbers-ci>=? "a1" "a")			=> #t)
  (check (%bytevector-u8/numbers-ci>=? "1" "1a")			=> #f)
  (check (%bytevector-u8/numbers-ci>=? "1a" "1")			=> #t)
  (check (%bytevector-u8/numbers-ci>=? "a" "A")			=> #t)
  (check (%bytevector-u8/numbers-ci>=? "A" "a")			=> #t)

  (check (%bytevector-u8/numbers-ci>=? "123" "45")			=> #t)
  (check (%bytevector-u8/numbers-ci>=? "45" "123")			=> #f)
  (check (%bytevector-u8/numbers-ci>=? "ciao3" "ciao10")		=> #f)
  (check (%bytevector-u8/numbers-ci>=? "ciao10" "ciao3")		=> #t)
  (check (%bytevector-u8/numbers-ci>=? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (%bytevector-u8/numbers-ci>=? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (%bytevector-u8/numbers-ci>=? "foo4bar3zab" "foo4bar10")	=> #f)
  (check (%bytevector-u8/numbers-ci>=? "foo4bar10" "foo4bar3zab")	=> #t)
  (check (%bytevector-u8/numbers-ci>=? "foo12" "12foo")		=> #t)
  (check (%bytevector-u8/numbers-ci>=? "12foo" "foo12")		=> #f)
  (check (%bytevector-u8/numbers-ci>=? "12bar" "foobar")		=> #f)
  (check (%bytevector-u8/numbers-ci>=? "12.3" "12.3")			=> #t)
  (check (%bytevector-u8/numbers-ci>=? "12.3" "12.10")			=> #f)
  (check (%bytevector-u8/numbers-ci>=? "12.10" "12.3")			=> #t)
  (check (%bytevector-u8/numbers-ci>=? "12.3" "12,10")			=> #t)
  (check (%bytevector-u8/numbers-ci>=? "12,10" "12.3")			=> #f)

  #t)


(parameterise ((check-test-name 'comparison-dictionary-bytevector-u8/number-case-sensitive))

  (check (%bytevector-u8/numbers-dictionary=? "" "")				=> #t)
  (check (%bytevector-u8/numbers-dictionary=? "a" "")				=> #f)
  (check (%bytevector-u8/numbers-dictionary=? "" "a")				=> #f)
  (check (%bytevector-u8/numbers-dictionary=? "a" "a")				=> #t)
  (check (%bytevector-u8/numbers-dictionary=? "1" "")				=> #f)
  (check (%bytevector-u8/numbers-dictionary=? "" "1")				=> #f)
  (check (%bytevector-u8/numbers-dictionary=? "1" "1")				=> #t)
  (check (%bytevector-u8/numbers-dictionary=? "1" "2")				=> #f)
  (check (%bytevector-u8/numbers-dictionary=? "2" "1")				=> #f)
  (check (%bytevector-u8/numbers-dictionary=? "a" "ab")			=> #f)
  (check (%bytevector-u8/numbers-dictionary=? "ab" "a")			=> #f)
  (check (%bytevector-u8/numbers-dictionary=? "a" "a1")			=> #f)
  (check (%bytevector-u8/numbers-dictionary=? "a1" "a")			=> #f)
  (check (%bytevector-u8/numbers-dictionary=? "1" "1a")			=> #f)
  (check (%bytevector-u8/numbers-dictionary=? "1a" "1")			=> #f)

  (check (%bytevector-u8/numbers-dictionary=? "123" "45")			=> #f)
  (check (%bytevector-u8/numbers-dictionary=? "45" "123")			=> #f)
  (check (%bytevector-u8/numbers-dictionary=? "ciao3" "ciao10")		=> #f)
  (check (%bytevector-u8/numbers-dictionary=? "ciao10" "ciao3")		=> #f)
  (check (%bytevector-u8/numbers-dictionary=? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (%bytevector-u8/numbers-dictionary=? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (%bytevector-u8/numbers-dictionary=? "foo4bar3zab" "foo4bar10")	=> #f)
  (check (%bytevector-u8/numbers-dictionary=? "foo4bar10" "foo4bar3zab")	=> #f)
  (check (%bytevector-u8/numbers-dictionary=? "foo12" "12foo")			=> #f)
  (check (%bytevector-u8/numbers-dictionary=? "12foo" "foo12")			=> #f)
  (check (%bytevector-u8/numbers-dictionary=? "12bar" "foobar")		=> #f)
  (check (%bytevector-u8/numbers-dictionary=? "12.3" "12.3")			=> #t)
  (check (%bytevector-u8/numbers-dictionary=? "12.3" "12.10")			=> #f)
  (check (%bytevector-u8/numbers-dictionary=? "12.10" "12.3")			=> #f)
  (check (%bytevector-u8/numbers-dictionary=? "12.3" "12,10")			=> #f)
  (check (%bytevector-u8/numbers-dictionary=? "12,10" "12.3")			=> #f)

  (check (%bytevector-u8/numbers-dictionary=? "fo o4b\tar3\nza\rb10" "foo4bar3zab2")	=> #f)
  (check (%bytevector-u8/numbers-dictionary=? "foo4bar3zab2" "fo o4b\tar3\nza\rb10")	=> #f)

;;; --------------------------------------------------------------------

  (check (%bytevector-u8/numbers-dictionary<>? "" "")				=> #f)
  (check (%bytevector-u8/numbers-dictionary<>? "a" "")				=> #t)
  (check (%bytevector-u8/numbers-dictionary<>? "" "a")				=> #t)
  (check (%bytevector-u8/numbers-dictionary<>? "a" "a")			=> #f)
  (check (%bytevector-u8/numbers-dictionary<>? "1" "")				=> #t)
  (check (%bytevector-u8/numbers-dictionary<>? "" "1")				=> #t)
  (check (%bytevector-u8/numbers-dictionary<>? "1" "1")			=> #f)
  (check (%bytevector-u8/numbers-dictionary<>? "1" "2")			=> #t)
  (check (%bytevector-u8/numbers-dictionary<>? "2" "1")			=> #t)
  (check (%bytevector-u8/numbers-dictionary<>? "a" "ab")			=> #t)
  (check (%bytevector-u8/numbers-dictionary<>? "ab" "a")			=> #t)
  (check (%bytevector-u8/numbers-dictionary<>? "a" "a1")			=> #t)
  (check (%bytevector-u8/numbers-dictionary<>? "a1" "a")			=> #t)
  (check (%bytevector-u8/numbers-dictionary<>? "1" "1a")			=> #t)
  (check (%bytevector-u8/numbers-dictionary<>? "1a" "1")			=> #t)

  (check (%bytevector-u8/numbers-dictionary<>? "123" "45")			=> #t)
  (check (%bytevector-u8/numbers-dictionary<>? "45" "123")			=> #t)
  (check (%bytevector-u8/numbers-dictionary<>? "ciao3" "ciao10")		=> #t)
  (check (%bytevector-u8/numbers-dictionary<>? "ciao10" "ciao3")		=> #t)
  (check (%bytevector-u8/numbers-dictionary<>? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (%bytevector-u8/numbers-dictionary<>? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (%bytevector-u8/numbers-dictionary<>? "foo4bar3zab" "foo4bar10")	=> #t)
  (check (%bytevector-u8/numbers-dictionary<>? "foo4bar10" "foo4bar3zab")	=> #t)
  (check (%bytevector-u8/numbers-dictionary<>? "foo12" "12foo")		=> #t)
  (check (%bytevector-u8/numbers-dictionary<>? "12foo" "foo12")		=> #t)
  (check (%bytevector-u8/numbers-dictionary<>? "12bar" "foobar")		=> #t)
  (check (%bytevector-u8/numbers-dictionary<>? "12.3" "12.3")			=> #f)
  (check (%bytevector-u8/numbers-dictionary<>? "12.3" "12.10")			=> #t)
  (check (%bytevector-u8/numbers-dictionary<>? "12.10" "12.3")			=> #t)
  (check (%bytevector-u8/numbers-dictionary<>? "12.3" "12,10")			=> #t)
  (check (%bytevector-u8/numbers-dictionary<>? "12,10" "12.3")			=> #t)

  (check (%bytevector-u8/numbers-dictionary<>? "fo o4b\tar3\nza\rb10" "foo4bar3zab2")	=> #t)
  (check (%bytevector-u8/numbers-dictionary<>? "foo4bar3zab2" "fo o4b\tar3\nza\rb10")	=> #t)

;;; --------------------------------------------------------------------

  (check (%bytevector-u8/numbers-dictionary<? "" "")				=> #f)
  (check (%bytevector-u8/numbers-dictionary<? "a" "")				=> #f)
  (check (%bytevector-u8/numbers-dictionary<? "" "a")				=> #t)
  (check (%bytevector-u8/numbers-dictionary<? "a" "a")				=> #f)
  (check (%bytevector-u8/numbers-dictionary<? "1" "")				=> #f)
  (check (%bytevector-u8/numbers-dictionary<? "" "1")				=> #t)
  (check (%bytevector-u8/numbers-dictionary<? "1" "1")				=> #f)
  (check (%bytevector-u8/numbers-dictionary<? "1" "2")				=> #t)
  (check (%bytevector-u8/numbers-dictionary<? "2" "1")				=> #f)
  (check (%bytevector-u8/numbers-dictionary<? "a" "ab")			=> #t)
  (check (%bytevector-u8/numbers-dictionary<? "ab" "a")			=> #f)
  (check (%bytevector-u8/numbers-dictionary<? "a" "a1")			=> #t)
  (check (%bytevector-u8/numbers-dictionary<? "a1" "a")			=> #f)
  (check (%bytevector-u8/numbers-dictionary<? "1" "1a")			=> #t)
  (check (%bytevector-u8/numbers-dictionary<? "1a" "1")			=> #f)

  (check (%bytevector-u8/numbers-dictionary<? "123" "45")			=> #f)
  (check (%bytevector-u8/numbers-dictionary<? "45" "123")			=> #t)
  (check (%bytevector-u8/numbers-dictionary<? "ciao3" "ciao10")		=> #t)
  (check (%bytevector-u8/numbers-dictionary<? "ciao10" "ciao3")		=> #f)
  (check (%bytevector-u8/numbers-dictionary<? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (%bytevector-u8/numbers-dictionary<? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (%bytevector-u8/numbers-dictionary<? "foo4bar3zab" "foo4bar10")	=> #t)
  (check (%bytevector-u8/numbers-dictionary<? "foo4bar10" "foo4bar3zab")	=> #f)
  (check (%bytevector-u8/numbers-dictionary<? "foo12" "12foo")			=> #f)
  (check (%bytevector-u8/numbers-dictionary<? "12foo" "foo12")			=> #t)
  (check (%bytevector-u8/numbers-dictionary<? "12bar" "foobar")		=> #t)
  (check (%bytevector-u8/numbers-dictionary<? "12.3" "12.3")			=> #f)
  (check (%bytevector-u8/numbers-dictionary<? "12.3" "12.10")			=> #t)
  (check (%bytevector-u8/numbers-dictionary<? "12.10" "12.3")			=> #f)
  (check (%bytevector-u8/numbers-dictionary<? "12.3" "12,10")			=> #f)
  (check (%bytevector-u8/numbers-dictionary<? "12,10" "12.3")			=> #t)

  (check 'this (%bytevector-u8/numbers-dictionary<? "fo o4b\tar3\nza\rb10" "foo4bar3zab2")	=> #f)
  (check (%bytevector-u8/numbers-dictionary<? "foo4bar3zab2" "fo o4b\tar3\nza\rb10")	=> #t)

;;; --------------------------------------------------------------------

  (check (%bytevector-u8/numbers-dictionary<=? "" "")				=> #t)
  (check (%bytevector-u8/numbers-dictionary<=? "a" "")				=> #f)
  (check (%bytevector-u8/numbers-dictionary<=? "" "a")				=> #t)
  (check (%bytevector-u8/numbers-dictionary<=? "a" "a")			=> #t)
  (check (%bytevector-u8/numbers-dictionary<=? "1" "")				=> #f)
  (check (%bytevector-u8/numbers-dictionary<=? "" "1")				=> #t)
  (check (%bytevector-u8/numbers-dictionary<=? "1" "1")			=> #t)
  (check (%bytevector-u8/numbers-dictionary<=? "1" "2")			=> #t)
  (check (%bytevector-u8/numbers-dictionary<=? "2" "1")			=> #f)
  (check (%bytevector-u8/numbers-dictionary<=? "a" "ab")			=> #t)
  (check (%bytevector-u8/numbers-dictionary<=? "ab" "a")			=> #f)
  (check (%bytevector-u8/numbers-dictionary<=? "a" "a1")			=> #t)
  (check (%bytevector-u8/numbers-dictionary<=? "a1" "a")			=> #f)
  (check (%bytevector-u8/numbers-dictionary<=? "1" "1a")			=> #t)
  (check (%bytevector-u8/numbers-dictionary<=? "1a" "1")			=> #f)

  (check (%bytevector-u8/numbers-dictionary<=? "123" "45")			=> #f)
  (check (%bytevector-u8/numbers-dictionary<=? "45" "123")			=> #t)
  (check (%bytevector-u8/numbers-dictionary<=? "ciao3" "ciao10")		=> #t)
  (check (%bytevector-u8/numbers-dictionary<=? "ciao10" "ciao3")		=> #f)
  (check (%bytevector-u8/numbers-dictionary<=? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (%bytevector-u8/numbers-dictionary<=? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (%bytevector-u8/numbers-dictionary<=? "foo4bar3zab" "foo4bar10")	=> #t)
  (check (%bytevector-u8/numbers-dictionary<=? "foo4bar10" "foo4bar3zab")	=> #f)
  (check (%bytevector-u8/numbers-dictionary<=? "foo12" "12foo")		=> #f)
  (check (%bytevector-u8/numbers-dictionary<=? "12foo" "foo12")		=> #t)
  (check (%bytevector-u8/numbers-dictionary<=? "12bar" "foobar")		=> #t)
  (check (%bytevector-u8/numbers-dictionary<=? "12.3" "12.3")			=> #t)
  (check (%bytevector-u8/numbers-dictionary<=? "12.3" "12.10")			=> #t)
  (check (%bytevector-u8/numbers-dictionary<=? "12.10" "12.3")			=> #f)
  (check (%bytevector-u8/numbers-dictionary<=? "12.3" "12,10")			=> #f)
  (check (%bytevector-u8/numbers-dictionary<=? "12,10" "12.3")			=> #t)

  (check (%bytevector-u8/numbers-dictionary<=? "fo o4b\tar3\nza\rb10" "foo4bar3zab2")	=> #f)
  (check (%bytevector-u8/numbers-dictionary<=? "foo4bar3zab2" "fo o4b\tar3\nza\rb10")	=> #t)

;;; --------------------------------------------------------------------

  (check (%bytevector-u8/numbers-dictionary>? "" "")				=> #f)
  (check (%bytevector-u8/numbers-dictionary>? "a" "")				=> #t)
  (check (%bytevector-u8/numbers-dictionary>? "" "a")				=> #f)
  (check (%bytevector-u8/numbers-dictionary>? "a" "a")				=> #f)
  (check (%bytevector-u8/numbers-dictionary>? "1" "")				=> #t)
  (check (%bytevector-u8/numbers-dictionary>? "" "1")				=> #f)
  (check (%bytevector-u8/numbers-dictionary>? "1" "1")				=> #f)
  (check (%bytevector-u8/numbers-dictionary>? "1" "2")				=> #f)
  (check (%bytevector-u8/numbers-dictionary>? "2" "1")				=> #t)
  (check (%bytevector-u8/numbers-dictionary>? "a" "ab")			=> #f)
  (check (%bytevector-u8/numbers-dictionary>? "ab" "a")			=> #t)
  (check (%bytevector-u8/numbers-dictionary>? "a" "a1")			=> #f)
  (check (%bytevector-u8/numbers-dictionary>? "a1" "a")			=> #t)
  (check (%bytevector-u8/numbers-dictionary>? "1" "1a")			=> #f)
  (check (%bytevector-u8/numbers-dictionary>? "1a" "1")			=> #t)

  (check (%bytevector-u8/numbers-dictionary>? "123" "45")			=> #t)
  (check (%bytevector-u8/numbers-dictionary>? "45" "123")			=> #f)
  (check (%bytevector-u8/numbers-dictionary>? "ciao3" "ciao10")		=> #f)
  (check (%bytevector-u8/numbers-dictionary>? "ciao10" "ciao3")		=> #t)
  (check (%bytevector-u8/numbers-dictionary>? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (%bytevector-u8/numbers-dictionary>? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (%bytevector-u8/numbers-dictionary>? "foo4bar3zab" "foo4bar10")	=> #f)
  (check (%bytevector-u8/numbers-dictionary>? "foo4bar10" "foo4bar3zab")	=> #t)
  (check (%bytevector-u8/numbers-dictionary>? "foo12" "12foo")			=> #t)
  (check (%bytevector-u8/numbers-dictionary>? "12foo" "foo12")			=> #f)
  (check (%bytevector-u8/numbers-dictionary>? "12bar" "foobar")		=> #f)
  (check (%bytevector-u8/numbers-dictionary>? "12.3" "12.3")			=> #f)
  (check (%bytevector-u8/numbers-dictionary>? "12.3" "12.10")			=> #f)
  (check (%bytevector-u8/numbers-dictionary>? "12.10" "12.3")			=> #t)
  (check (%bytevector-u8/numbers-dictionary>? "12.3" "12,10")			=> #t)
  (check (%bytevector-u8/numbers-dictionary>? "12,10" "12.3")			=> #f)

  (check (%bytevector-u8/numbers-dictionary>? "fo o4b\tar3\nza\rb10" "foo4bar3zab2")	=> #t)
  (check (%bytevector-u8/numbers-dictionary>? "foo4bar3zab2" "fo o4b\tar3\nza\rb10")	=> #f)

;;; --------------------------------------------------------------------

  (check (%bytevector-u8/numbers-dictionary>=? "" "")				=> #t)
  (check (%bytevector-u8/numbers-dictionary>=? "a" "")				=> #t)
  (check (%bytevector-u8/numbers-dictionary>=? "" "a")				=> #f)
  (check (%bytevector-u8/numbers-dictionary>=? "a" "a")			=> #t)
  (check (%bytevector-u8/numbers-dictionary>=? "1" "")				=> #t)
  (check (%bytevector-u8/numbers-dictionary>=? "" "1")				=> #f)
  (check (%bytevector-u8/numbers-dictionary>=? "1" "1")			=> #t)
  (check (%bytevector-u8/numbers-dictionary>=? "1" "2")			=> #f)
  (check (%bytevector-u8/numbers-dictionary>=? "2" "1")			=> #t)
  (check (%bytevector-u8/numbers-dictionary>=? "a" "ab")			=> #f)
  (check (%bytevector-u8/numbers-dictionary>=? "ab" "a")			=> #t)
  (check (%bytevector-u8/numbers-dictionary>=? "a" "a1")			=> #f)
  (check (%bytevector-u8/numbers-dictionary>=? "a1" "a")			=> #t)
  (check (%bytevector-u8/numbers-dictionary>=? "1" "1a")			=> #f)
  (check (%bytevector-u8/numbers-dictionary>=? "1a" "1")			=> #t)

  (check (%bytevector-u8/numbers-dictionary>=? "123" "45")			=> #t)
  (check (%bytevector-u8/numbers-dictionary>=? "45" "123")			=> #f)
  (check (%bytevector-u8/numbers-dictionary>=? "ciao3" "ciao10")		=> #f)
  (check (%bytevector-u8/numbers-dictionary>=? "ciao10" "ciao3")		=> #t)
  (check (%bytevector-u8/numbers-dictionary>=? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (%bytevector-u8/numbers-dictionary>=? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (%bytevector-u8/numbers-dictionary>=? "foo4bar3zab" "foo4bar10")	=> #f)
  (check (%bytevector-u8/numbers-dictionary>=? "foo4bar10" "foo4bar3zab")	=> #t)
  (check (%bytevector-u8/numbers-dictionary>=? "foo12" "12foo")		=> #t)
  (check (%bytevector-u8/numbers-dictionary>=? "12foo" "foo12")		=> #f)
  (check (%bytevector-u8/numbers-dictionary>=? "12bar" "foobar")		=> #f)
  (check (%bytevector-u8/numbers-dictionary>=? "12.3" "12.3")			=> #t)
  (check (%bytevector-u8/numbers-dictionary>=? "12.3" "12.10")			=> #f)
  (check (%bytevector-u8/numbers-dictionary>=? "12.10" "12.3")			=> #t)
  (check (%bytevector-u8/numbers-dictionary>=? "12.3" "12,10")			=> #t)
  (check (%bytevector-u8/numbers-dictionary>=? "12,10" "12.3")			=> #f)

  (check (%bytevector-u8/numbers-dictionary>=? "fo o4b\tar3\nza\rb10" "foo4bar3zab2")	=> #t)
  (check (%bytevector-u8/numbers-dictionary>=? "foo4bar3zab2" "fo o4b\tar3\nza\rb10")	=> #f)

  #t)


(parameterise ((check-test-name 'comparison-dictionary-bytevector-u8/number-case-insensitive))

  (check (%bytevector-u8/numbers-dictionary-ci=? "" "")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci=? "a" "")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci=? "" "a")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci=? "a" "a")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci=? "1" "")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci=? "" "1")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci=? "1" "1")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci=? "1" "2")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci=? "2" "1")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci=? "a" "ab")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci=? "ab" "a")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci=? "a" "a1")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci=? "a1" "a")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci=? "1" "1a")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci=? "1a" "1")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci=? "a" "A")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci=? "A" "a")				=> #t)

  (check (%bytevector-u8/numbers-dictionary-ci=? "123" "45")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci=? "45" "123")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci=? "ciao3" "ciao10")			=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci=? "ciao10" "ciao3")			=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci=? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci=? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci=? "foo4bar3zab" "foo4bar10")		=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci=? "foo4bar10" "foo4bar3zab")		=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci=? "foo12" "12foo")			=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci=? "12foo" "foo12")			=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci=? "12bar" "foobar")			=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci=? "12.3" "12.3")			=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci=? "12.3" "12.10")			=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci=? "12.10" "12.3")			=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci=? "12.3" "12,10")			=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci=? "12,10" "12.3")			=> #f)

;;; --------------------------------------------------------------------

  (check (%bytevector-u8/numbers-dictionary-ci<>? "" "")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci<>? "a" "")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<>? "" "a")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<>? "a" "a")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci<>? "1" "")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<>? "" "1")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<>? "1" "1")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci<>? "1" "2")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<>? "2" "1")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<>? "a" "ab")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<>? "ab" "a")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<>? "a" "a1")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<>? "a1" "a")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<>? "1" "1a")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<>? "1a" "1")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<>? "A" "a")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci<>? "a" "A")				=> #f)

  (check (%bytevector-u8/numbers-dictionary-ci<>? "123" "45")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<>? "45" "123")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<>? "ciao3" "ciao10")			=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<>? "ciao10" "ciao3")			=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<>? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<>? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<>? "foo4bar3zab" "foo4bar10")		=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<>? "foo4bar10" "foo4bar3zab")		=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<>? "foo12" "12foo")			=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<>? "12foo" "foo12")			=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<>? "12bar" "foobar")			=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<>? "12.3" "12.3")			=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci<>? "12.3" "12.10")			=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<>? "12.10" "12.3")			=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<>? "12.3" "12,10")			=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<>? "12,10" "12.3")			=> #t)

;;; --------------------------------------------------------------------

  (check (%bytevector-u8/numbers-dictionary-ci<? "" "")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci<? "a" "")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci<? "" "a")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<? "a" "a")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci<? "1" "")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci<? "" "1")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<? "1" "1")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci<? "1" "2")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<? "2" "1")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci<? "a" "ab")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<? "ab" "a")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci<? "a" "a1")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<? "a1" "a")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci<? "1" "1a")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<? "1a" "1")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci<? "a" "A")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci<? "A" "a")				=> #f)

  (check (%bytevector-u8/numbers-dictionary-ci<? "123" "45")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci<? "45" "123")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<? "ciao3" "ciao10")			=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<? "ciao10" "ciao3")			=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci<? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci<? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<? "foo4bar3zab" "foo4bar10")		=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<? "foo4bar10" "foo4bar3zab")		=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci<? "foo12" "12foo")			=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci<? "12foo" "foo12")			=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<? "12bar" "foobar")			=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<? "12.3" "12.3")			=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci<? "12.3" "12.10")			=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<? "12.10" "12.3")			=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci<? "12.3" "12,10")			=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci<? "12,10" "12.3")			=> #t)

;;; --------------------------------------------------------------------

  (check (%bytevector-u8/numbers-dictionary-ci<=? "" "")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<=? "a" "")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci<=? "" "a")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<=? "a" "a")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<=? "1" "")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci<=? "" "1")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<=? "1" "1")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<=? "1" "2")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<=? "2" "1")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci<=? "a" "ab")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<=? "ab" "a")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci<=? "a" "a1")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<=? "a1" "a")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci<=? "1" "1a")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<=? "1a" "1")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci<=? "a" "A")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<=? "A" "a")				=> #t)

  (check (%bytevector-u8/numbers-dictionary-ci<=? "123" "45")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci<=? "45" "123")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<=? "ciao3" "ciao10")			=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<=? "ciao10" "ciao3")			=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci<=? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci<=? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<=? "foo4bar3zab" "foo4bar10")		=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<=? "foo4bar10" "foo4bar3zab")		=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci<=? "foo12" "12foo")			=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci<=? "12foo" "foo12")			=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<=? "12bar" "foobar")			=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<=? "12.3" "12.3")			=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<=? "12.3" "12.10")			=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci<=? "12.10" "12.3")			=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci<=? "12.3" "12,10")			=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci<=? "12,10" "12.3")			=> #t)

;;; --------------------------------------------------------------------

  (check (%bytevector-u8/numbers-dictionary-ci>? "" "")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci>? "a" "")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci>? "" "a")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci>? "a" "a")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci>? "1" "")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci>? "" "1")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci>? "1" "1")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci>? "1" "2")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci>? "2" "1")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci>? "a" "ab")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci>? "ab" "a")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci>? "a" "a1")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci>? "a1" "a")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci>? "1" "1a")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci>? "1a" "1")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci>? "a" "A")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci>? "A" "a")				=> #f)

  (check (%bytevector-u8/numbers-dictionary-ci>? "123" "45")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci>? "45" "123")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci>? "ciao3" "ciao10")			=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci>? "ciao10" "ciao3")			=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci>? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci>? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci>? "foo4bar3zab" "foo4bar10")		=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci>? "foo4bar10" "foo4bar3zab")		=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci>? "foo12" "12foo")			=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci>? "12foo" "foo12")			=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci>? "12bar" "foobar")			=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci>? "12.3" "12.3")			=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci>? "12.3" "12.10")			=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci>? "12.10" "12.3")			=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci>? "12.3" "12,10")			=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci>? "12,10" "12.3")			=> #f)

;;; --------------------------------------------------------------------

  (check (%bytevector-u8/numbers-dictionary-ci>=? "" "")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci>=? "a" "")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci>=? "" "a")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci>=? "a" "a")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci>=? "1" "")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci>=? "" "1")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci>=? "1" "1")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci>=? "1" "2")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci>=? "2" "1")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci>=? "a" "ab")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci>=? "ab" "a")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci>=? "a" "a1")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci>=? "a1" "a")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci>=? "1" "1a")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci>=? "1a" "1")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci>=? "a" "A")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci>=? "A" "a")				=> #t)

  (check (%bytevector-u8/numbers-dictionary-ci>=? "123" "45")				=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci>=? "45" "123")				=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci>=? "ciao3" "ciao10")			=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci>=? "ciao10" "ciao3")			=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci>=? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci>=? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci>=? "foo4bar3zab" "foo4bar10")		=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci>=? "foo4bar10" "foo4bar3zab")		=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci>=? "foo12" "12foo")			=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci>=? "12foo" "foo12")			=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci>=? "12bar" "foobar")			=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci>=? "12.3" "12.3")			=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci>=? "12.3" "12.10")			=> #f)
  (check (%bytevector-u8/numbers-dictionary-ci>=? "12.10" "12.3")			=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci>=? "12.3" "12,10")			=> #t)
  (check (%bytevector-u8/numbers-dictionary-ci>=? "12,10" "12.3")			=> #f)

  #t)


(parameterise ((check-test-name 'mapping))

  (check
      (let* ((str "aaaa")
	     (beg 0)
	     (end (bytevector-u8-length str)))
	(%subbytevector-u8-map char-upcase str beg end))
    => "AAAA")

  (check
      (let* ((str "")
	     (beg 0)
	     (end (bytevector-u8-length str)))
	(%subbytevector-u8-map char-upcase str beg end))
    => "")

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aaaa")
	     (beg 0)
	     (end (bytevector-u8-length str)))
	(%subbytevector-u8-map! char-upcase str beg end)
	str)
    => "AAAA")

  (check
      (let* ((str "")
	     (beg 0)
	     (end (bytevector-u8-length str)))
	(%subbytevector-u8-map! char-upcase str beg end)
	str)
    => "")

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aaaa")
	     (beg 0)
	     (end (bytevector-u8-length str))
	     (result ""))
	(%subbytevector-u8-for-each
	 (lambda (ch)
	   (set! result
		 (bytevector-u8-append result
				(number->bytevector-u8 (char->integer (char-upcase ch))))))
	 str beg end)
	result)
    => "65656565")

  (check
      (let* ((str "")
	     (beg 0)
	     (end (bytevector-u8-length str))
	     (result ""))
	(%subbytevector-u8-for-each
	 (lambda (ch)
	   (set! result
		 (bytevector-u8-append result
				(number->bytevector-u8 (char->integer (char-upcase ch))))))
	 str beg end)
	result)
    => "")

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aaaa")
	     (beg 0)
	     (end (bytevector-u8-length str))
	     (result '()))
	(%subbytevector-u8-for-each-index
	 (lambda (idx)
	   (set! result (cons idx result)))
	 str beg end)
	result)
    => '(3 2 1 0))

  (check
      (let* ((str "")
	     (beg 0)
	     (end (bytevector-u8-length str))
	     (result '()))
	(%subbytevector-u8-for-each-index
	 (lambda (idx)
	   (set! result (cons idx result)))
	 str beg end)
	result)
    => '())

  )


(parameterise ((check-test-name 'case))

  (check
      (let* ((str (bytevector-u8-copy "abcd")) (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-titlecase*! str beg end)
	str)
    => "Abcd")

  (check
      (let* ((str (bytevector-u8-copy "123abcd")) (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-titlecase*! str beg end)
	str)
    => "123Abcd")

  (check
      (let* ((str (bytevector-u8-copy "---abcd")) (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-titlecase*! str beg end)
	str)
    => "---Abcd")

  (check
      (let* ((str (bytevector-u8-copy "abcd efgh")) (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-titlecase*! str beg end)
	str)
    => "Abcd Efgh")

  )


(parameterise ((check-test-name 'folding))

  (check
      (let* ((str "abcd")
	     (beg 0)
	     (end (bytevector-u8-length str)))
	(%subbytevector-u8-fold-left cons '() str beg end))
    => '(#\d #\c #\b #\a))

  (check
      (let* ((str "")
	     (beg 0)
	     (end (bytevector-u8-length str)))
	(%subbytevector-u8-fold-left cons '() str beg end))
    => '())

;;; --------------------------------------------------------------------

  (check
      (let* ((str "abcd")
	     (beg 0)
	     (end (bytevector-u8-length str)))
	(%subbytevector-u8-fold-right cons '() str beg end))
    => '(#\a #\b #\c #\d))

  (check
      (let* ((str "")
	     (beg 0)
	     (end (bytevector-u8-length str)))
	(%subbytevector-u8-fold-right cons '() str beg end))
    => '())

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-unfold null? car cdr '(#\a #\b #\c #\d))
    => "abcd")

  (check
      (bytevector-u8-unfold null? car cdr '())
    => "")

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-unfold-right null? car cdr '(#\a #\b #\c #\d))
    => "dcba")

  (check
      (bytevector-u8-unfold-right null? car cdr '())
    => "")

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-tabulate (lambda (idx) (integer->char (+ 65 idx))) 4)
    => "ABCD")

  (check
      (bytevector-u8-tabulate integer->char 0)
    => "")

  )


(parameterise ((check-test-name 'selecting))

  (check
      (let* ((str "abcd") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-take 2 str beg end))
    => "ab")

  (check
      (let* ((str "") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-take 0 str beg end))
    => "")

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(let* ((str "abcd") (beg 0) (end (bytevector-u8-length str)))
	  (%bytevector-u8-take 5 str beg end)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "abcd") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-take-right 2 str beg end))
    => "cd")

  (check
      (let* ((str "") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-take-right 0 str beg end))
    => "")

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(let* ((str "abcd") (beg 0) (end (bytevector-u8-length str)))
	  (%bytevector-u8-take-right 5 str beg end)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "abcd") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-drop 2 str beg end))
    => "cd")

  (check
      (let* ((str "") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-drop 0 str beg end))
    => "")

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(let* ((str "abcd") (beg 0) (end (bytevector-u8-length str)))
	  (%bytevector-u8-drop 5 str beg end)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "abcd") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-drop-right 2 str beg end))
    => "ab")

  (check
      (let* ((str "") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-drop-right 0 str beg end))
    => "")

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(let* ((str "abcd") (beg 0) (end (bytevector-u8-length str)))
	  (%bytevector-u8-drop-right 5 str beg end)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aaabcd") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-trim #\a str beg end))
    => "bcd")

  (check
      (let* ((str "bcd") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-trim #\a str beg end))
    => "bcd")

  (check
      (let* ((str "") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-trim #\a str beg end))
    => "")

  (check
      (let* ((str "aaabcd") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-trim (char-set #\a #\b) str beg end))
    => "cd")

  (check
      (let* ((str "bcd") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-trim (char-set #\a #\b) str beg end))
    => "cd")

  (check
      (let* ((str "") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-trim (char-set #\a #\b) str beg end))
    => "")

  (check
      (let* ((str "AAAbcd") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-trim char-upper-case? str beg end))
    => "bcd")

  (check
      (let* ((str "bcd") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-trim char-upper-case? str beg end))
    => "bcd")

  (check
      (let* ((str "") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-trim char-upper-case? str beg end))
    => "")

;;; --------------------------------------------------------------------

  (check
      (let* ((str "bcdaaa") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-trim-right #\a str beg end))
    => "bcd")

  (check
      (let* ((str "bcd") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-trim-right #\a str beg end))
    => "bcd")

  (check
      (let* ((str "") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-trim-right #\a str beg end))
    => "")

  (check
      (let* ((str "cdbaaa") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-trim-right (char-set #\a #\b) str beg end))
    => "cd")

  (check
      (let* ((str "cdb") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-trim-right (char-set #\a #\b) str beg end))
    => "cd")

  (check
      (let* ((str "") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-trim-right (char-set #\a #\b) str beg end))
    => "")

  (check
      (let* ((str "bcdAAA") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-trim-right char-upper-case? str beg end))
    => "bcd")

  (check
      (let* ((str "bcd") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-trim-right char-upper-case? str beg end))
    => "bcd")

  (check
      (let* ((str "") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-trim-right char-upper-case? str beg end))
    => "")

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aaabcdaaa") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-trim-both #\a str beg end))
    => "bcd")

  (check
      (let* ((str "bcd") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-trim-both #\a str beg end))
    => "bcd")

  (check
      (let* ((str "") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-trim-both #\a str beg end))
    => "")

  (check
      (let* ((str "aaabcdaa") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-trim-both (char-set #\a #\b) str beg end))
    => "cd")

  (check
      (let* ((str "bcdb") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-trim-both (char-set #\a #\b) str beg end))
    => "cd")

  (check
      (let* ((str "") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-trim-both (char-set #\a #\b) str beg end))
    => "")

  (check
      (let* ((str "AAAbcdAAA") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-trim-both char-upper-case? str beg end))
    => "bcd")

  (check
      (let* ((str "bcd") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-trim-both char-upper-case? str beg end))
    => "bcd")

  (check
      (let* ((str "") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-trim-both char-upper-case? str beg end))
    => "")

;;; --------------------------------------------------------------------

  (check
      (let* ((str "abc") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-pad 3 #\0 str beg end))
    => "abc")

  (check
      (let* ((str "abc") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-pad 5 #\0 str beg end))
    => "00abc")

  (check
      (let* ((str "abc") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-pad 2 #\0 str beg end))
    => "bc")

  (check
      (let* ((str "abc") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-pad 0 #\0 str beg end))
    => "")

;;; --------------------------------------------------------------------

  (check
      (let* ((str "abc") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-pad-right 3 #\0 str beg end))
    => "abc")

  (check
      (let* ((str "abc") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-pad-right 5 #\0 str beg end))
    => "abc00")

  (check
      (let* ((str "abc") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-pad-right 2 #\0 str beg end))
    => "ab")

  (check
      (let* ((str "abc") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-pad-right 0 #\0 str beg end))
    => "")

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 "abcd") (beg1 0) (str2 (bytevector-u8-copy "12")))
	(%bytevector-u8-copy*! str2 0 str1 beg1 (+ 2 beg1))
	str2)
    => "ab")

  (check
      (let* ((str1 "abcd") (beg1 0) (str2 ""))
	(%bytevector-u8-copy*! str2 0 str1 beg1 beg1)
	str2)
    => "")

  (check
      (guard (exc ((assertion-violation? exc)
		   #t))
	(let* ((str1 "abcd") (beg1 0) (str2 (bytevector-u8-copy "12")))
	  (%bytevector-u8-copy*! str2 3 str1 beg1 (+ 2 beg1))
	  str2))
    => #t)

  )


(parameterise ((check-test-name 'prefix))

  (check
      (let* ((str1 "abcdefg") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "abcd123") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-prefix-length str1 beg1 end1 str2 beg2 end2))
    => 4)

  (check
      (let* ((str1 "aBcdefg") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "abcd123") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-prefix-length str1 beg1 end1 str2 beg2 end2))
    => 1)

  (check
      (let* ((str1 "efg") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "123") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-prefix-length str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 "a") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "a") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-prefix-length str1 beg1 end1 str2 beg2 end2))
    => 1)

  (check
      (let* ((str1 "1") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "2") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-prefix-length str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 "") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "abcd123") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-prefix-length str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 "abcdefg") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-prefix-length str1 beg1 end1 str2 beg2 end2))
    => 0)

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 "efgabcd") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "123abcd") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-suffix-length str1 beg1 end1 str2 beg2 end2))
    => 4)

  (check
      (let* ((str1 "efgabcd") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "123abCd") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-suffix-length str1 beg1 end1 str2 beg2 end2))
    => 1)

  (check
      (let* ((str1 "efg") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "123") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-suffix-length str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 "a") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "a") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-suffix-length str1 beg1 end1 str2 beg2 end2))
    => 1)

  (check
      (let* ((str1 "1") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "2") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-suffix-length str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 "") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "abcd123") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-suffix-length str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 "abcdefg") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-suffix-length str1 beg1 end1 str2 beg2 end2))
    => 0)

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 "aBcdefg") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "aBcd123") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-prefix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 4)

  (check
      (let* ((str1 "aBcdefg") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "abcd123") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-prefix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 4)

  (check
      (let* ((str1 "efg") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "123") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-prefix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 "a") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "a") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-prefix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 1)

  (check
      (let* ((str1 "1") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "2") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-prefix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 "") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "abcd123") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-prefix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 "abcdefg") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-prefix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 0)

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 "efgabCd") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "123abCd") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-suffix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 4)

  (check
      (let* ((str1 "efgabCd") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "123abcd") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-suffix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 4)

  (check
      (let* ((str1 "efg") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "123") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-suffix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 "a") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "a") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-suffix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 1)

  (check
      (let* ((str1 "1") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "2") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-suffix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 "") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "abcd123") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-suffix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 "abcdefg") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-suffix-length-ci str1 beg1 end1 str2 beg2 end2))
    => 0)

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "abcd123") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-prefix? str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "aBcd123") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-prefix? str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "efg") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "123") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-prefix? str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "123") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-prefix? str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 "efg") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-prefix? str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-prefix? str1 beg1 end1 str2 beg2 end2))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 "aBcd") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "aBcd123") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-prefix-ci? str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "aBcd123") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-prefix-ci? str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 "efg") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "123") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-prefix-ci? str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "123") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-prefix-ci? str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 "efg") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-prefix-ci? str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-prefix-ci? str1 beg1 end1 str2 beg2 end2))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "123abcd") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-suffix? str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "123aBcd") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-suffix? str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "efg") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "123") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-suffix? str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "123") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-suffix? str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 "efg") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-suffix? str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-suffix? str1 beg1 end1 str2 beg2 end2))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 "aBcd") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "123aBcd") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-suffix-ci? str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "123aBcd") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-suffix-ci? str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 "efg") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "123") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-suffix-ci? str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "123") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-suffix-ci? str1 beg1 end1 str2 beg2 end2))
    => #t)

  (check
      (let* ((str1 "efg") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-suffix-ci? str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-suffix-ci? str1 beg1 end1 str2 beg2 end2))
    => #t)

  )


(parameterise ((check-test-name 'searching))

  (check
      (let* ((str "abcd") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-index #\b str beg end))
    => 1)

  (check
      (let* ((str "abcd") (end (bytevector-u8-length str)))
	(%bytevector-u8-index #\b str 1 end))
    => 1)

  (check
      (let* ((str "abcd") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-index #\1 str beg end))
    => #f)

  (check
      (let* ((str "") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-index #\1 str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "abcd") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-index (char-set #\b #\B) str beg end))
    => 1)

  (check
      (let* ((str "abcd") (end (bytevector-u8-length str)))
	(%bytevector-u8-index (char-set #\b #\B) str 1 end))
    => 1)

  (check
      (let* ((str "abcd") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-index (char-set #\0 #\1) str beg end))
    => #f)

  (check
      (let* ((str "") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-index (char-set #\0 #\1) str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aBcd") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-index char-upper-case? str beg end))
    => 1)

  (check
      (let* ((str "aBcd") (end (bytevector-u8-length str)))
	(%bytevector-u8-index char-upper-case? str 1 end))
    => 1)

  (check
      (let* ((str "abcd") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-index char-upper-case? str beg end))
    => #f)

  (check
      (let* ((str "") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-index char-upper-case? str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "abcd") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-index-right #\b str beg end))
    => 1)

  (check
      (let* ((str "abcd") (end (bytevector-u8-length str)))
	(%bytevector-u8-index-right #\b str 1 end))
    => 1)

  (check
      (let* ((str "abcd") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-index-right #\1 str beg end))
    => #f)

  (check
      (let* ((str "") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-index-right #\1 str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "abcd") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-index-right (char-set #\b #\B) str beg end))
    => 1)

  (check
      (let* ((str "abcd") (end (bytevector-u8-length str)))
	(%bytevector-u8-index-right (char-set #\b #\B) str 1 end))
    => 1)

  (check
      (let* ((str "abcd") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-index-right (char-set #\0 #\1) str beg end))
    => #f)

  (check
      (let* ((str "") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-index-right (char-set #\0 #\1) str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aBcd") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-index-right char-upper-case? str beg end))
    => 1)

  (check
      (let* ((str "aBcd") (end (bytevector-u8-length str)))
	(%bytevector-u8-index-right char-upper-case? str 1 end))
    => 1)

  (check
      (let* ((str "abcd") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-index-right char-upper-case? str beg end))
    => #f)

  (check
      (let* ((str "") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-index-right char-upper-case? str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "bacd") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-skip #\b str beg end))
    => 1)

  (check
      (let* ((str "bacd") (end (bytevector-u8-length str)))
	(%bytevector-u8-skip #\b str 1 end))
    => 1)

  (check
      (let* ((str "1111") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-skip #\1 str beg end))
    => #f)

  (check
      (let* ((str "") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-skip #\1 str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "bacd") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-skip (char-set #\b #\B) str beg end))
    => 1)

  (check
      (let* ((str "bacd") (end (bytevector-u8-length str)))
	(%bytevector-u8-skip (char-set #\b #\B) str 1 end))
    => 1)

  (check
      (let* ((str "1010") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-skip (char-set #\0 #\1) str beg end))
    => #f)

  (check
      (let* ((str "") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-skip (char-set #\0 #\1) str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "Bacd") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-skip char-upper-case? str beg end))
    => 1)

  (check
      (let* ((str "Bacd") (end (bytevector-u8-length str)))
	(%bytevector-u8-skip char-upper-case? str 1 end))
    => 1)

  (check
      (let* ((str "ABCD") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-skip char-upper-case? str beg end))
    => #f)

  (check
      (let* ((str "") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-skip char-upper-case? str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "acdb") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-skip-right #\b str beg end))
    => 2)

  (check
      (let* ((str "acdb") (end (bytevector-u8-length str)))
	(%bytevector-u8-skip-right #\b str 1 end))
    => 2)

  (check
      (let* ((str "1111") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-skip-right #\1 str beg end))
    => #f)

  (check
      (let* ((str "") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-skip-right #\1 str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "acdb") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-skip-right (char-set #\b #\B) str beg end))
    => 2)

  (check
      (let* ((str "acdb") (end (bytevector-u8-length str)))
	(%bytevector-u8-skip-right (char-set #\b #\B) str 1 end))
    => 2)

  (check
      (let* ((str "0101") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-skip-right (char-set #\0 #\1) str beg end))
    => #f)

  (check
      (let* ((str "") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-skip-right (char-set #\0 #\1) str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "acdB") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-skip-right char-upper-case? str beg end))
    => 2)

  (check
      (let* ((str "acdB") (end (bytevector-u8-length str)))
	(%bytevector-u8-skip-right char-upper-case? str 1 end))
    => 2)

  (check
      (let* ((str "ABCD") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-skip-right char-upper-case? str beg end))
    => #f)

  (check
      (let* ((str "") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-skip-right char-upper-case? str beg end))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "abcbd") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-count #\b str beg end))
    => 2)

  (check
      (let* ((str "abcd") (end (bytevector-u8-length str)))
	(%bytevector-u8-count #\b str 1 end))
    => 1)

  (check
      (let* ((str "abcd") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-count #\1 str beg end))
    => 0)

  (check
      (let* ((str "") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-count #\1 str beg end))
    => 0)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "abcBd") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-count (char-set #\b #\B) str beg end))
    => 2)

  (check
      (let* ((str "abcd") (end (bytevector-u8-length str)))
	(%bytevector-u8-count (char-set #\b #\B) str 1 end))
    => 1)

  (check
      (let* ((str "abcd") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-count (char-set #\0 #\1) str beg end))
    => 0)

  (check
      (let* ((str "") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-count (char-set #\0 #\1) str beg end))
    => 0)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aBcAd") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-count char-upper-case? str beg end))
    => 2)

  (check
      (let* ((str "aBcd") (end (bytevector-u8-length str)))
	(%bytevector-u8-count char-upper-case? str 1 end))
    => 1)

  (check
      (let* ((str "abcd") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-count char-upper-case? str beg end))
    => 0)

  (check
      (let* ((str "") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-count char-upper-case? str beg end))
    => 0)

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 "ciao hello salut") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "hello") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-contains str1 beg1 end1 str2 beg2 end2))
    => 5)

  (check
      (let* ((str1 "ciao hello salut") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "hola") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-contains str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "ciao hello salut") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-contains str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 "") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "hello") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-contains str1 beg1 end1 str2 beg2 end2))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str1 "ciAO HELLO saLUT") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "hello") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-contains-ci str1 beg1 end1 str2 beg2 end2))
    => 5)

  (check
      (let* ((str1 "ciao hello salut") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "HOLA") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-contains-ci str1 beg1 end1 str2 beg2 end2))
    => #f)

  (check
      (let* ((str1 "ciao hello salut") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-contains-ci str1 beg1 end1 str2 beg2 end2))
    => 0)

  (check
      (let* ((str1 "") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "hello") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-contains-ci str1 beg1 end1 str2 beg2 end2))
    => #f)

  )


(parameterise ((check-test-name 'filtering))

  (check
      (let* ((str "abcbd") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-delete #\b str beg end))
    => "acd")

  (check
      (let* ((str "abcbd") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-delete #\0 str beg end))
    => "abcbd")

  (check
      (let* ((str "") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-delete #\b str beg end))
    => "")

;;; --------------------------------------------------------------------

  (check
      (let* ((str "abcbd") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-delete (char-set #\b #\B) str beg end))
    => "acd")

  (check
      (let* ((str "abcbd") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-delete (char-set #\0 #\1) str beg end))
    => "abcbd")

  (check
      (let* ((str "") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-delete (char-set #\b #\B) str beg end))
    => "")

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aBcBd") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-delete char-upper-case? str beg end))
    => "acd")

  (check
      (let* ((str "abcbd") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-delete char-upper-case? str beg end))
    => "abcbd")

  (check
      (let* ((str "") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-delete char-upper-case? str beg end))
    => "")

;;; --------------------------------------------------------------------

  (check
      (let* ((str "abcbd") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-filter #\b str beg end))
    => "bb")

  (check
      (let* ((str "abcbd") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-filter #\0 str beg end))
    => "")

  (check
      (let* ((str "") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-filter #\b str beg end))
    => "")

;;; --------------------------------------------------------------------

  (check
      (let* ((str "abcbd") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-filter (char-set #\b #\B) str beg end))
    => "bb")

  (check
      (let* ((str "abcbd") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-filter (char-set #\0 #\1) str beg end))
    => "")

  (check
      (let* ((str "") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-filter (char-set #\b #\B) str beg end))
    => "")

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aBcBd") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-filter char-upper-case? str beg end))
    => "BB")

  (check
      (let* ((str "abcbd") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-filter char-upper-case? str beg end))
    => "")

  (check
      (let* ((str "") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-filter char-upper-case? str beg end))
    => "")

  )


(parameterise ((check-test-name 'lists))

  (check
      (let* ((str (bytevector-u8-copy "abcd")) (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8->list* str beg end))
    => '(#\a #\b #\c #\d))

  (check
      (let* ((str (bytevector-u8-copy "")) (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8->list* str beg end))
    => '())

;;; --------------------------------------------------------------------

  (check
      (reverse-list->bytevector-u8 '(#\a #\b #\c #\d))
    => "dcba")

  (check
      (reverse-list->bytevector-u8 '())
    => "")

  )

;;; --------------------------------------------------------------------

(parameterise ((check-test-name 'tokenize))

  (check
      (let* ((str "ciao hello salut") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-tokenize (char-set #\a #\c #\e #\i #\h #\l #\o #\s #\t #\u)
			  str beg end))
    => '("ciao" "hello" "salut")
)
  (check
      (let* ((str "") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-tokenize (char-set #\a #\c #\e #\i #\h #\l #\o #\s #\t #\u)
			  str beg end))
    => '())

  (check
      (let* ((str "ciao hello salut") (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-tokenize (char-set) str beg end))
    => '())

  )

;;; --------------------------------------------------------------------

(parameterise ((check-test-name 'join))

  (check
      (%bytevector-u8-join '("c" "i" "a" "o") "," 'infix)
    => "c,i,a,o")

  (check
      (%bytevector-u8-join '("c" "i" "a" "o") "," 'strict-infix)
    => "c,i,a,o")

  (check
      (%bytevector-u8-join '("c" "i" "a" "o") "," 'suffix)
    => "c,i,a,o,")

  (check
      (%bytevector-u8-join '("c" "i" "a" "o") "," 'prefix)
    => ",c,i,a,o")

;;; --------------------------------------------------------------------

  (check
      (%bytevector-u8-join '() "," 'infix)
    => "")

  (check
      (guard (exc ((assertion-violation? exc)
		   #t))
	(%bytevector-u8-join '() "," 'strict-infix))
    => #t)

  (check
      (%bytevector-u8-join '() "," 'suffix)
    => "")

  (check
      (%bytevector-u8-join '() "," 'prefix)
    => "")

;;; --------------------------------------------------------------------

  (check
      (%bytevector-u8-join '("c") "," 'infix)
    => "c")

  (check
      (%bytevector-u8-join '("c") "," 'strict-infix)
    => "c")

  (check
      (%bytevector-u8-join '("c") "," 'suffix)
    => "c,")

  (check
      (%bytevector-u8-join '("c") "," 'prefix)
    => ",c")

;;; --------------------------------------------------------------------

  (check
      (%bytevector-u8-join '("c" "i" "a" "o") "" 'infix)
    => "ciao")

  (check
      (%bytevector-u8-join '("c" "i" "a" "o") "" 'strict-infix)
    => "ciao")

  (check
      (%bytevector-u8-join '("c" "i" "a" "o") "" 'suffix)
    => "ciao")

  (check
      (%bytevector-u8-join '("c" "i" "a" "o") "" 'prefix)
    => "ciao")

;;; --------------------------------------------------------------------

  (check
      (%bytevector-u8-join '("c" "i" "a" "o") ",;;" 'infix)
    => "c,;;i,;;a,;;o")

  (check
      (%bytevector-u8-join '("c" "i" "a" "o") ",;;" 'strict-infix)
    => "c,;;i,;;a,;;o")

  (check
      (%bytevector-u8-join '("c" "i" "a" "o") ",;;" 'suffix)
    => "c,;;i,;;a,;;o,;;")

  (check
      (%bytevector-u8-join '("c" "i" "a" "o") ",;;" 'prefix)
    => ",;;c,;;i,;;a,;;o")

  )


(parameterise ((check-test-name 'replicating))

  (check
      (let* ((str "ciao ") (beg 0) (end (bytevector-u8-length str)))
	(%xsubbytevector-u8 0 5 str beg end))
    => "ciao ")

  (check
      (let* ((str "ciao ") (beg 0) (end (bytevector-u8-length str)))
	(%xsubbytevector-u8 0 9 str beg end))
    => "ciao ciao")

  (check
      (let* ((str "ciao ") (beg 0) (end (bytevector-u8-length str)))
	(%xsubbytevector-u8 -5 5 str beg end))
    => "ciao ciao ")

  (check
      (let* ((str "ciao ") (beg 0) (end (bytevector-u8-length str)))
	(%xsubbytevector-u8 2 4 str beg end))
    => "ao")

  (check
      (let* ((str "ciao ") (beg 0) (end (bytevector-u8-length str)))
	(%xsubbytevector-u8 -3 7 str beg end))
    => "ao ciao ci")

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(let ((str "ciao "))
	  (%xsubbytevector-u8 -3 7 str 3 3)))
    => #t)

  (check
      (guard (exc ((assertion-violation? exc)
		   #t))
	(let* ((str "") (beg 0) (end (bytevector-u8-length str)))
	  (%xsubbytevector-u8 0 5 str beg end)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "ciao ") (beg 0) (end (bytevector-u8-length str))
	     (result (bytevector-u8-copy "01234")))
	(%bytevector-u8-xcopy! 0 5 result 0 (bytevector-u8-length result) str beg end)
	result)
    => "ciao ")

  (check
      (let* ((str "ciao ") (beg 0) (end (bytevector-u8-length str))
	     (result (bytevector-u8-copy "012345678")))
	(%bytevector-u8-xcopy! 0 9 result 0 (bytevector-u8-length result) str beg end)
	result)
    => "ciao ciao")

  (check
      (let* ((str "ciao ") (beg 0) (end (bytevector-u8-length str))
	     (result (bytevector-u8-copy "0123456789")))
	(%bytevector-u8-xcopy! -5 5 result 0 (bytevector-u8-length result) str beg end)
	result)
    => "ciao ciao ")

  (check
      (let* ((str "ciao ") (beg 0) (end (bytevector-u8-length str))
	     (result (bytevector-u8-copy "01")))
	(%bytevector-u8-xcopy! 2 4 result 0 (bytevector-u8-length result) str beg end)
	result)
    => "ao")

  (check
      (let* ((str "ciao ") (beg 0) (end (bytevector-u8-length str))
	     (result (bytevector-u8-copy "0123456789")))
	(%bytevector-u8-xcopy! -3 7 result 0 (bytevector-u8-length result) str beg end)
	result)
    => "ao ciao ci")

  (check
      (guard (exc ((assertion-violation? exc)
		   #t))
	(let* ((str "") (beg 0) (end (bytevector-u8-length str))
	     (result (bytevector-u8-copy "")))
	  (%bytevector-u8-xcopy! 0 5 result 0 (bytevector-u8-length result) str beg end)))
    => #t)

  )


(parameterise ((check-test-name 'filling))

  (check
      (let* ((str (bytevector-u8-copy "abcd")) (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-fill*! #\b str beg end)
	str)
    => "bbbb")

  (check
      (let* ((str (bytevector-u8-copy "accd")))
	(%bytevector-u8-fill*! #\b str 1 3)
	str)
    => "abbd")

  (check
      (let* ((str (bytevector-u8-copy "")))
	(%bytevector-u8-fill*! #\b str 0 0)
	str)
    => "")

  )


(parameterise ((check-test-name 'reverse))

  (check
      (let* ((str (bytevector-u8-copy "abcd")) (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-reverse str beg end))
    => "dcba")

  (check
      (let* ((str (bytevector-u8-copy "")) (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-reverse str beg end))
    => "")

;;; --------------------------------------------------------------------

  (check
      (let* ((str (bytevector-u8-copy "abcd")) (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-reverse! str beg end)
	str)
    => "dcba")

  (check
      (let* ((str (bytevector-u8-copy "")) (beg 0) (end (bytevector-u8-length str)))
	(%bytevector-u8-reverse! str beg end)
	str)
    => "")

  )


(parameterise ((check-test-name 'concatenate))

  (check
      (bytevector-u8-concatenate '("ciao" " " "hello" " " "salut"))
    => "ciao hello salut")

  (check
      (bytevector-u8-concatenate '())
    => "")

;;; --------------------------------------------------------------------

  (check
      (%bytevector-u8-concatenate-reverse '("ciao" " " "hello" " " "salut") " hola" (bytevector-u8-length " hola"))
    => "salut hello ciao hola")

  (check
      (%bytevector-u8-concatenate-reverse '("ciao" " " "hello" " " "salut") " hola" 3)
    => "salut hello ciao ho")

  (check
      (%bytevector-u8-concatenate-reverse '() "" 0)
    => "")


  )


(parameterise ((check-test-name 'replace))

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "1234") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-replace str1 beg1 end1 str2 beg2 end2))
    => "1234")

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "1234") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-replace str1 2 2 str2 beg2 end2))
    => "ab1234cd")

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-replace str1 2 2 str2 beg2 end2))
    => "abcd")

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "1234") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-replace str1 1 3 str2 beg2 end2))
    => "a1234d")

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "1234") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-replace str1 0 3 str2 beg2 end2))
    => "1234d")

  (check
      (let* ((str1 "abcd") (beg1 0) (end1 (bytevector-u8-length str1))
	     (str2 "1234") (beg2 0) (end2 (bytevector-u8-length str2)))
	(%bytevector-u8-replace str1 1 4 str2 beg2 end2))
    => "a1234")


  )


;;;; done

(check-report)

;;; end of file
