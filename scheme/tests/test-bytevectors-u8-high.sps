;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for the bytevector u8 library
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
  (bytevectors u8)
  (char-sets)
  (checks)
  (rnrs mutable-strings))

(check-set-mode! 'report-failed)
(display "*** testing bytevectors u8\n")


(parameterise ((check-test-name 'views))

  (check
      (subbytevector-u8* (string->utf8 "ciao"))
    => (string->utf8 "ciao"))

;;; --------------------------------------------------------------------

  (check
      (subbytevector-u8* (view (string->utf8 "ciao")))
    => (string->utf8 "ciao"))

  (check
      (subbytevector-u8* (view (string->utf8 "ciao") (start 2)))
    => "ao")

  (check
      (subbytevector-u8* (view (string->utf8 "ciao") (start 0) (past 4)))
    => (string->utf8 "ciao"))

  (check
      (subbytevector-u8* (view (string->utf8 "ciao") (start 0) (past 0)))
    => "")

  (check
      (subbytevector-u8* (view (string->utf8 "ciao") (start 1) (past 1)))
    => "")

  (check
      (subbytevector-u8* (view (string->utf8 "ciao") (start 0) (past 1)))
    => "c")

  (check
      (subbytevector-u8* (view (string->utf8 "ciao") (past 2)))
    => "ci")

  )


(parameterise ((check-test-name 'constructors))

  (check
      (bytevector-u8-append "0123")
    => "0123")

  (check
      (bytevector-u8-append "0123" "45678")
    => "012345678")

  (check
      (bytevector-u8-append "")
    => "")

  (check
      (bytevector-u8-append "" "")
    => "")

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-tabulate (lambda (idx) (integer->char (+ 65 idx))) 4)
    => "ABCD")

  (check
      (bytevector-u8-tabulate integer->char 0)
    => "")

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-concatenate '((string->utf8 "ciao") " " "hello" " " "salut"))
    => "ciao hello salut")

  (check
      (bytevector-u8-concatenate '())
    => "")

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-concatenate-reverse '((string->utf8 "ciao") " " "hello" " " "salut") " hola" 3)
    => "salut hello ciao ho")

  (check
      (bytevector-u8-concatenate-reverse '((string->utf8 "ciao") " " "hello" " " "salut") " hola")
    => "salut hello ciao hola")

  (check
      (bytevector-u8-concatenate-reverse '((string->utf8 "ciao") " " "hello" " " "salut"))
    => "salut hello ciao")

  (check
      (bytevector-u8-concatenate-reverse '())
    => "")

  )


(parameterise ((check-test-name 'predicates))

  (check
      (bytevector-u8-null? (string->utf8 "ciao"))
    => #f)

  (check
      (bytevector-u8-null? "")
    => #t)

;;; --------------------------------------------------------------------

  (check
      (guard (exc ((assertion-violation? exc)
		   (condition-who exc)))
	(bytevector-u8-every 123 "abc"))
    => '%bytevector-u8-every)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aaaa"))
	(bytevector-u8-every #\a str))
    => #t)

  (check
      (let* ((str "aaaab"))
	(bytevector-u8-every #\a str))
    => #f)

  (check
      (let* ((str "aabaa"))
	(bytevector-u8-every #\a str))
    => #f)

  (check
      (let* ((str ""))
	(bytevector-u8-every #\a str))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aaaa"))
	(bytevector-u8-every (char-set #\a) str))
    => #t)

  (check
      (let* ((str "aaaab"))
	(bytevector-u8-every (char-set #\a) str))
    => #f)

  (check
      (let* ((str "aabaa"))
	(bytevector-u8-every (char-set #\a) str))
    => #f)

  (check
      (let* ((str ""))
	(bytevector-u8-every (char-set #\a) str))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "aaaa"))
	(bytevector-u8-every char-alphabetic? str))
    => #t)

  (check
      (let* ((str "aaaa2"))
	(bytevector-u8-every char-alphabetic? str))
    => #f)

  (check
      (let* ((str "aa2aa"))
	(bytevector-u8-every char-alphabetic? str))
    => #f)

  (check
      (let* ((str ""))
	(bytevector-u8-every char-alphabetic? str))
    => #f)

  (check
      (let* ((str "1234"))
	(bytevector-u8-every (lambda (x) x) str))
    => #\4)

;;; --------------------------------------------------------------------

  (check
      (guard (exc ((assertion-violation? exc)
		   (condition-who exc)))
	(bytevector-u8-any 123 "abc"))
    => '%bytevector-u8-any)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "ddadd"))
	(bytevector-u8-any #\a str))
    => #t)

  (check
      (let* ((str "dddda"))
	(bytevector-u8-any #\a str))
    => #t)

  (check
      (let* ((str "ddd"))
	(bytevector-u8-any #\a str))
    => #f)

  (check
      (let* ((str ""))
	(bytevector-u8-any #\a str))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "dddaddd"))
	(bytevector-u8-any (char-set #\a) str))
    => #t)

  (check
      (let* ((str "ddda"))
	(bytevector-u8-any (char-set #\a) str))
    => #t)

  (check
      (let* ((str "dddd"))
	(bytevector-u8-any (char-set #\a) str))
    => #f)

  (check
      (let* ((str ""))
	(bytevector-u8-any (char-set #\a) str))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let* ((str "11a11"))
	(bytevector-u8-any char-alphabetic? str))
    => #t)

  (check
      (let* ((str "11111a"))
	(bytevector-u8-any char-alphabetic? str))
    => #t)

  (check
      (let* ((str "1111"))
	(bytevector-u8-any char-alphabetic? str))
    => #f)

  (check
      (let* ((str ""))
	(bytevector-u8-any char-alphabetic? str))
    => #f)

  (check
      (let* ((str "1234"))
	(bytevector-u8-any (lambda (x) x) str))
    => #\1)

  )


(parameterise ((check-test-name 'comparison-case-sensitive))

  (check
      (bytevector-u8-compare "abcdefg" "abcd123" values values values)
    => 4)

  (check
      (bytevector-u8-compare "abcdef" "abcd123" values values values)
    => 4)

  (check
      (bytevector-u8-compare "efg" "123" values values values)
    => 0)

  (check
      (bytevector-u8-compare "" "abcd" values values values)
    => 0)

  (check
      (bytevector-u8-compare "abcd" "" values values values)
    => 0)

  (check
      (bytevector-u8-compare "abcdA" "abcdA"
		      (lambda (idx) 'less)
		      (lambda (idx) 'equal)
		      (lambda (idx) 'greater))
    => 'equal)

  (check
      (bytevector-u8-compare "abcdA" "abcdB"
		      (lambda (idx) 'less)
		      (lambda (idx) 'equal)
		      (lambda (idx) 'greater))
    => 'less)

  (check
      (bytevector-u8-compare "abcdB" "abcdA"
		      (lambda (idx) 'less)
		      (lambda (idx) 'equal)
		      (lambda (idx) 'greater))
    => 'greater)

;;; --------------------------------------------------------------------

  (check-for-true
   (let* ((str "abcd"))
     (bytevector-u8= str str)))

  (check-for-true
   (bytevector-u8= (view "12abcd" (start 2)) "abcd"))

  (check-for-false
   (bytevector-u8= "abc" "abcd"))

  (check-for-false
   (bytevector-u8= "abcd" "abc"))

  (check-for-false
   (bytevector-u8= "ABcd" "abcd"))

  (check-for-false
   (bytevector-u8= "abcd" "a2cd"))

;;; --------------------------------------------------------------------

  (check-for-false
   (bytevector-u8<> "abcd" "abcd"))

  (check-for-true
   (bytevector-u8<> "abc" "abcd"))

  (check-for-true
   (bytevector-u8<> "abcd" "abc"))

  (check-for-true
   (bytevector-u8<> "ABcd" "abcd"))

  (check-for-true
   (bytevector-u8<> "abcd" "a2cd"))

;;; --------------------------------------------------------------------

  (check-for-false
   (bytevector-u8< "abcd" "abcd"))

  (check-for-true
   (bytevector-u8< "abc" "abcd"))

  (check-for-false
   (bytevector-u8< "abcd" "abc"))

  (check-for-true
   (bytevector-u8< "ABcd" "abcd"))

  (check-for-false
   (bytevector-u8< "abcd" "a2cd"))

;;; --------------------------------------------------------------------

  (check-for-true
   (bytevector-u8<= "abcd" "abcd"))

  (check-for-true
   (bytevector-u8<= "abc" "abcd"))

  (check-for-false
   (bytevector-u8<= "abcd" "abc"))

  (check-for-true
   (bytevector-u8<= "ABcd" "abcd"))

  (check-for-false
   (bytevector-u8<= "abcd" "a2cd"))

;;; --------------------------------------------------------------------

  (check-for-false
   (bytevector-u8> "abcd" "abcd"))

  (check-for-true
   (bytevector-u8> "abcd" "abc"))

  (check-for-false
   (bytevector-u8> "abc" "abcd"))

  (check-for-true
   (bytevector-u8> "abcd" "ABcd"))

  (check-for-false
   (bytevector-u8> "a2cd" "abcd"))

;;; --------------------------------------------------------------------

  (check-for-true
   (bytevector-u8>= "abcd" "abcd"))

  (check-for-true
   (bytevector-u8>= "abcd" "abc"))

  (check-for-false
   (bytevector-u8>= "abc" "abcd"))

  (check-for-true
   (bytevector-u8>= "abcd" "ABcd"))

  (check-for-false
   (bytevector-u8>= "a2cd" "abcd"))

  )


(parameterise ((check-test-name 'comparison-case-insensitive))

  (check
      (bytevector-u8-compare-ci "aBcdefg" "abcd123" values values values)
    => 4)

  (check
      (bytevector-u8-compare-ci "efg" "123" values values values)
    => 0)

  (check
      (bytevector-u8-compare-ci "" "abcd" values values values)
    => 0)

  (check
      (bytevector-u8-compare-ci "abcd" "" values values values)
    => 0)

  (check
      (bytevector-u8-compare-ci "abcdA" "abcda"
			 (lambda (idx) 'less) (lambda (idx) 'equal) (lambda (idx) 'greater))
    => 'equal)

  (check
      (bytevector-u8-compare-ci "abcdA" "abcdb"
			 (lambda (idx) 'less) (lambda (idx) 'equal) (lambda (idx) 'greater))
    => 'less)

  (check
      (bytevector-u8-compare-ci "abcdb" "abcdA"
			 (lambda (idx) 'less) (lambda (idx) 'equal) (lambda (idx) 'greater))
    => 'greater)

;;; --------------------------------------------------------------------

  (check-for-true
   (bytevector-u8-ci= "abcd" "abcd"))

  (check-for-true
   (bytevector-u8-ci= (view "12abcd" (start 2)) "abcd"))

  (check-for-false
   (bytevector-u8-ci= "abc" "abcd"))

  (check-for-false
   (bytevector-u8-ci= "abcd" "abc"))

  (check-for-true
   (bytevector-u8-ci= "ABcd" "abcd"))

  (check-for-false
   (bytevector-u8-ci= "abcd" "a2cd"))

;;; --------------------------------------------------------------------

  (check-for-false
   (bytevector-u8-ci<> "abcd" "abcd"))

  (check-for-true
   (bytevector-u8-ci<> "abc" "abcd"))

  (check-for-true
   (bytevector-u8-ci<> "abcd" "abc"))

  (check-for-false
   (bytevector-u8-ci<> "ABcd" "abcd"))

  (check-for-true
   (bytevector-u8-ci<> "abcd" "a2cd"))

;;; --------------------------------------------------------------------

  (check-for-false
   (bytevector-u8-ci< "abcd" "abcd"))

  (check-for-true
   (bytevector-u8-ci< "abc" "abcd"))

  (check-for-false
   (bytevector-u8-ci< "abcd" "abc"))

  (check-for-false
   (bytevector-u8-ci< "ABcd" "abcd"))

  (check-for-false
   (bytevector-u8-ci< "abcd" "a2cd"))

;;; --------------------------------------------------------------------

  (check-for-true
   (bytevector-u8-ci<= "abcd" "abcd"))

  (check-for-true
   (bytevector-u8-ci<= "abc" "abcd"))

  (check-for-false
   (bytevector-u8-ci<= "abcd" "abc"))

  (check-for-true
   (bytevector-u8-ci<= "ABcd" "abcd"))

  (check-for-false
   (bytevector-u8-ci<= "abcd" "a2cd"))

;;; --------------------------------------------------------------------

  (check-for-false
   (bytevector-u8-ci> "abcd" "abcd"))

  (check-for-true
   (bytevector-u8-ci> "abcd" "abc"))

  (check-for-false
   (bytevector-u8-ci> "abc" "abcd"))

  (check-for-false
   (bytevector-u8-ci> "abcd" "ABcd"))

  (check-for-false
   (bytevector-u8-ci> "a2cd" "abcd"))

;;; --------------------------------------------------------------------

  (check-for-true
   (bytevector-u8-ci>= "abcd" "abcd"))

  (check-for-true
   (bytevector-u8-ci>= "abcd" "abc"))

  (check-for-false
   (bytevector-u8-ci>= "abc" "abcd"))

  (check-for-true
   (bytevector-u8-ci>= "abcd" "ABcd"))

  (check-for-false
   (bytevector-u8-ci>= "a2cd" "abcd"))

  #t)


(parameterise ((check-test-name 'comparison-dictionary-case-sensitive))

  (check (bytevector-u8-dictionary=? "" "")				=> #t)
  (check (bytevector-u8-dictionary=? "a" "")				=> #f)
  (check (bytevector-u8-dictionary=? "" "a")				=> #f)
  (check (bytevector-u8-dictionary=? "ab" "a")				=> #f)
  (check (bytevector-u8-dictionary=? "a" "ab")				=> #f)
  (check (bytevector-u8-dictionary=? (string->utf8 "ciao") (string->utf8 "ciao"))			=> #t)
  (check (bytevector-u8-dictionary=? "ciao1" (string->utf8 "ciao"))			=> #f)
  (check (bytevector-u8-dictionary=? (string->utf8 "ciao") "ciao1")			=> #f)

  (check (bytevector-u8-dictionary=? "ci ao" (string->utf8 "ciao"))			=> #t)
  (check (bytevector-u8-dictionary=? (string->utf8 "ciao") "ci ao")			=> #t)
  (check (bytevector-u8-dictionary=? "ci\tao" (string->utf8 "ciao"))			=> #t)
  (check (bytevector-u8-dictionary=? (string->utf8 "ciao") "ci\tao")			=> #t)
  (check (bytevector-u8-dictionary=? "ci\nao" (string->utf8 "ciao"))			=> #t)
  (check (bytevector-u8-dictionary=? (string->utf8 "ciao") "ci\nao")			=> #t)
  (check (bytevector-u8-dictionary=? "ci\vao" (string->utf8 "ciao"))			=> #t)
  (check (bytevector-u8-dictionary=? (string->utf8 "ciao") "ci\tao")			=> #t)
  (check (bytevector-u8-dictionary=? "ci\fao" (string->utf8 "ciao"))			=> #t)
  (check (bytevector-u8-dictionary=? (string->utf8 "ciao") "ci\fao")			=> #t)
  (check (bytevector-u8-dictionary=? "ci\rao" (string->utf8 "ciao"))			=> #t)
  (check (bytevector-u8-dictionary=? (string->utf8 "ciao") "ci\rao")			=> #t)

;;; --------------------------------------------------------------------

  (check (bytevector-u8-dictionary<? "" "")				=> #f)
  (check (bytevector-u8-dictionary<? "a" "")				=> #f)
  (check (bytevector-u8-dictionary<? "" "a")				=> #t)
  (check (bytevector-u8-dictionary<? "ab" "a")				=> #f)
  (check (bytevector-u8-dictionary<? "a" "ab")				=> #t)
  (check (bytevector-u8-dictionary<? (string->utf8 "ciao") (string->utf8 "ciao"))			=> #f)
  (check (bytevector-u8-dictionary<? "ciao1" (string->utf8 "ciao"))			=> #f)
  (check (bytevector-u8-dictionary<? (string->utf8 "ciao") "ciao1")			=> #t)

  (check (bytevector-u8-dictionary<? "ci ao" (string->utf8 "ciao"))			=> #f)
  (check (bytevector-u8-dictionary<? (string->utf8 "ciao") "ci ao")			=> #f)
  (check (bytevector-u8-dictionary<? "ci\tao" (string->utf8 "ciao"))			=> #f)
  (check (bytevector-u8-dictionary<? (string->utf8 "ciao") "ci\tao")			=> #f)
  (check (bytevector-u8-dictionary<? "ci\nao" (string->utf8 "ciao"))			=> #f)
  (check (bytevector-u8-dictionary<? (string->utf8 "ciao") "ci\nao")			=> #f)
  (check (bytevector-u8-dictionary<? "ci\vao" (string->utf8 "ciao"))			=> #f)
  (check (bytevector-u8-dictionary<? (string->utf8 "ciao") "ci\tao")			=> #f)
  (check (bytevector-u8-dictionary<? "ci\fao" (string->utf8 "ciao"))			=> #f)
  (check (bytevector-u8-dictionary<? (string->utf8 "ciao") "ci\fao")			=> #f)
  (check (bytevector-u8-dictionary<? "ci\rao" (string->utf8 "ciao"))			=> #f)
  (check (bytevector-u8-dictionary<? (string->utf8 "ciao") "ci\rao")			=> #f)

;;; --------------------------------------------------------------------

  (check (bytevector-u8-dictionary<=? "" "")				=> #t)
  (check (bytevector-u8-dictionary<=? "a" "")				=> #f)
  (check (bytevector-u8-dictionary<=? "" "a")				=> #t)
  (check (bytevector-u8-dictionary<=? "ab" "a")			=> #f)
  (check (bytevector-u8-dictionary<=? "a" "ab")			=> #t)
  (check (bytevector-u8-dictionary<=? (string->utf8 "ciao") (string->utf8 "ciao"))			=> #t)
  (check (bytevector-u8-dictionary<=? "ciao1" (string->utf8 "ciao"))			=> #f)
  (check (bytevector-u8-dictionary<=? (string->utf8 "ciao") "ciao1")			=> #t)

  (check (bytevector-u8-dictionary<=? "ci ao" (string->utf8 "ciao"))			=> #t)
  (check (bytevector-u8-dictionary<=? (string->utf8 "ciao") "ci ao")			=> #t)
  (check (bytevector-u8-dictionary<=? "ci\tao" (string->utf8 "ciao"))			=> #t)
  (check (bytevector-u8-dictionary<=? (string->utf8 "ciao") "ci\tao")			=> #t)
  (check (bytevector-u8-dictionary<=? "ci\nao" (string->utf8 "ciao"))			=> #t)
  (check (bytevector-u8-dictionary<=? (string->utf8 "ciao") "ci\nao")			=> #t)
  (check (bytevector-u8-dictionary<=? "ci\vao" (string->utf8 "ciao"))			=> #t)
  (check (bytevector-u8-dictionary<=? (string->utf8 "ciao") "ci\tao")			=> #t)
  (check (bytevector-u8-dictionary<=? "ci\fao" (string->utf8 "ciao"))			=> #t)
  (check (bytevector-u8-dictionary<=? (string->utf8 "ciao") "ci\fao")			=> #t)
  (check (bytevector-u8-dictionary<=? "ci\rao" (string->utf8 "ciao"))			=> #t)
  (check (bytevector-u8-dictionary<=? (string->utf8 "ciao") "ci\rao")			=> #t)

;;; --------------------------------------------------------------------

  (check (bytevector-u8-dictionary>? "" "")				=> #f)
  (check (bytevector-u8-dictionary>? "a" "")				=> #t)
  (check (bytevector-u8-dictionary>? "" "a")				=> #f)
  (check (bytevector-u8-dictionary>? "ab" "a")				=> #t)
  (check (bytevector-u8-dictionary>? "a" "ab")				=> #f)
  (check (bytevector-u8-dictionary>? (string->utf8 "ciao") (string->utf8 "ciao"))			=> #f)
  (check (bytevector-u8-dictionary>? "ciao1" (string->utf8 "ciao"))			=> #t)
  (check (bytevector-u8-dictionary>? (string->utf8 "ciao") "ciao1")			=> #f)

  (check (bytevector-u8-dictionary>? "ci ao" (string->utf8 "ciao"))			=> #f)
  (check (bytevector-u8-dictionary>? (string->utf8 "ciao") "ci ao")			=> #f)
  (check (bytevector-u8-dictionary>? "ci\tao" (string->utf8 "ciao"))			=> #f)
  (check (bytevector-u8-dictionary>? (string->utf8 "ciao") "ci\tao")			=> #f)
  (check (bytevector-u8-dictionary>? "ci\nao" (string->utf8 "ciao"))			=> #f)
  (check (bytevector-u8-dictionary>? (string->utf8 "ciao") "ci\nao")			=> #f)
  (check (bytevector-u8-dictionary>? "ci\vao" (string->utf8 "ciao"))			=> #f)
  (check (bytevector-u8-dictionary>? (string->utf8 "ciao") "ci\tao")			=> #f)
  (check (bytevector-u8-dictionary>? "ci\fao" (string->utf8 "ciao"))			=> #f)
  (check (bytevector-u8-dictionary>? (string->utf8 "ciao") "ci\fao")			=> #f)
  (check (bytevector-u8-dictionary>? "ci\rao" (string->utf8 "ciao"))			=> #f)
  (check (bytevector-u8-dictionary>? (string->utf8 "ciao") "ci\rao")			=> #f)

;;; --------------------------------------------------------------------

  (check (bytevector-u8-dictionary>=? "" "")				=> #t)
  (check (bytevector-u8-dictionary>=? "a" "")				=> #t)
  (check (bytevector-u8-dictionary>=? "" "a")				=> #f)
  (check (bytevector-u8-dictionary>=? "ab" "a")			=> #t)
  (check (bytevector-u8-dictionary>=? "a" "ab")			=> #f)
  (check (bytevector-u8-dictionary>=? (string->utf8 "ciao") (string->utf8 "ciao"))			=> #t)
  (check (bytevector-u8-dictionary>=? "ciao1" (string->utf8 "ciao"))			=> #t)
  (check (bytevector-u8-dictionary>=? (string->utf8 "ciao") "ciao1")			=> #f)

  (check (bytevector-u8-dictionary>=? "ci ao" (string->utf8 "ciao"))			=> #t)
  (check (bytevector-u8-dictionary>=? (string->utf8 "ciao") "ci ao")			=> #t)
  (check (bytevector-u8-dictionary>=? "ci\tao" (string->utf8 "ciao"))			=> #t)
  (check (bytevector-u8-dictionary>=? (string->utf8 "ciao") "ci\tao")			=> #t)
  (check (bytevector-u8-dictionary>=? "ci\nao" (string->utf8 "ciao"))			=> #t)
  (check (bytevector-u8-dictionary>=? (string->utf8 "ciao") "ci\nao")			=> #t)
  (check (bytevector-u8-dictionary>=? "ci\vao" (string->utf8 "ciao"))			=> #t)
  (check (bytevector-u8-dictionary>=? (string->utf8 "ciao") "ci\tao")			=> #t)
  (check (bytevector-u8-dictionary>=? "ci\fao" (string->utf8 "ciao"))			=> #t)
  (check (bytevector-u8-dictionary>=? (string->utf8 "ciao") "ci\fao")			=> #t)
  (check (bytevector-u8-dictionary>=? "ci\rao" (string->utf8 "ciao"))			=> #t)
  (check (bytevector-u8-dictionary>=? (string->utf8 "ciao") "ci\rao")			=> #t)

  #t)


(parameterise ((check-test-name 'comparison-dictionary-case-insensitive))

  (check (bytevector-u8-dictionary-ci=? "" "")				=> #t)
  (check (bytevector-u8-dictionary-ci=? "a" "")			=> #f)
  (check (bytevector-u8-dictionary-ci=? "" "a")			=> #f)
  (check (bytevector-u8-dictionary-ci=? "ab" "a")			=> #f)
  (check (bytevector-u8-dictionary-ci=? "a" "ab")			=> #f)
  (check (bytevector-u8-dictionary-ci=? (string->utf8 "ciao") (string->utf8 "ciao"))			=> #t)
  (check (bytevector-u8-dictionary-ci=? "ciao1" (string->utf8 "ciao"))		=> #f)
  (check (bytevector-u8-dictionary-ci=? (string->utf8 "ciao") "ciao1")		=> #f)
  (check (bytevector-u8-dictionary-ci=? (STRING->UTF8 "CIAO") (string->utf8 "ciao"))			=> #t)
  (check (bytevector-u8-dictionary-ci=? "CIAO1" (string->utf8 "ciao"))		=> #f)
  (check (bytevector-u8-dictionary-ci=? (STRING->UTF8 "CIAO") "ciao1")		=> #f)

  (check (bytevector-u8-dictionary-ci=? "ci ao" (string->utf8 "ciao"))		=> #t)
  (check (bytevector-u8-dictionary-ci=? (string->utf8 "ciao") "ci ao")		=> #t)
  (check (bytevector-u8-dictionary-ci=? "ci\tao" (string->utf8 "ciao"))		=> #t)
  (check (bytevector-u8-dictionary-ci=? (string->utf8 "ciao") "ci\tao")		=> #t)
  (check (bytevector-u8-dictionary-ci=? "ci\nao" (string->utf8 "ciao"))		=> #t)
  (check (bytevector-u8-dictionary-ci=? (string->utf8 "ciao") "ci\nao")		=> #t)
  (check (bytevector-u8-dictionary-ci=? "ci\vao" (string->utf8 "ciao"))		=> #t)
  (check (bytevector-u8-dictionary-ci=? (string->utf8 "ciao") "ci\tao")		=> #t)
  (check (bytevector-u8-dictionary-ci=? "ci\fao" (string->utf8 "ciao"))		=> #t)
  (check (bytevector-u8-dictionary-ci=? (string->utf8 "ciao") "ci\fao")		=> #t)
  (check (bytevector-u8-dictionary-ci=? "ci\rao" (string->utf8 "ciao"))		=> #t)
  (check (bytevector-u8-dictionary-ci=? (string->utf8 "ciao") "ci\rao")		=> #t)

;;; --------------------------------------------------------------------

  (check (bytevector-u8-dictionary-ci<? "" "")				=> #f)
  (check (bytevector-u8-dictionary-ci<? "a" "")			=> #f)
  (check (bytevector-u8-dictionary-ci<? "" "a")			=> #t)
  (check (bytevector-u8-dictionary-ci<? "ab" "a")			=> #f)
  (check (bytevector-u8-dictionary-ci<? "a" "ab")			=> #t)
  (check (bytevector-u8-dictionary-ci<? (string->utf8 "ciao") (string->utf8 "ciao"))			=> #f)
  (check (bytevector-u8-dictionary-ci<? "ciao1" (string->utf8 "ciao"))		=> #f)
  (check (bytevector-u8-dictionary-ci<? (string->utf8 "ciao") "ciao1")		=> #t)
  (check (bytevector-u8-dictionary-ci<? (STRING->UTF8 "CIAO") (string->utf8 "ciao"))			=> #f)
  (check (bytevector-u8-dictionary-ci<? "CIAO1" (string->utf8 "ciao"))		=> #f)
  (check (bytevector-u8-dictionary-ci<? (STRING->UTF8 "CIAO") "ciao1")		=> #t)

  (check (bytevector-u8-dictionary-ci<? "ci ao" (string->utf8 "ciao"))		=> #f)
  (check (bytevector-u8-dictionary-ci<? (string->utf8 "ciao") "ci ao")		=> #f)
  (check (bytevector-u8-dictionary-ci<? "ci\tao" (string->utf8 "ciao"))		=> #f)
  (check (bytevector-u8-dictionary-ci<? (string->utf8 "ciao") "ci\tao")		=> #f)
  (check (bytevector-u8-dictionary-ci<? "ci\nao" (string->utf8 "ciao"))		=> #f)
  (check (bytevector-u8-dictionary-ci<? (string->utf8 "ciao") "ci\nao")		=> #f)
  (check (bytevector-u8-dictionary-ci<? "ci\vao" (string->utf8 "ciao"))		=> #f)
  (check (bytevector-u8-dictionary-ci<? (string->utf8 "ciao") "ci\tao")		=> #f)
  (check (bytevector-u8-dictionary-ci<? "ci\fao" (string->utf8 "ciao"))		=> #f)
  (check (bytevector-u8-dictionary-ci<? (string->utf8 "ciao") "ci\fao")		=> #f)
  (check (bytevector-u8-dictionary-ci<? "ci\rao" (string->utf8 "ciao"))		=> #f)
  (check (bytevector-u8-dictionary-ci<? (string->utf8 "ciao") "ci\rao")		=> #f)

;;; --------------------------------------------------------------------

  (check (bytevector-u8-dictionary-ci<=? "" "")			=> #t)
  (check (bytevector-u8-dictionary-ci<=? "a" "")			=> #f)
  (check (bytevector-u8-dictionary-ci<=? "" "a")			=> #t)
  (check (bytevector-u8-dictionary-ci<=? "ab" "a")			=> #f)
  (check (bytevector-u8-dictionary-ci<=? "a" "ab")			=> #t)
  (check (bytevector-u8-dictionary-ci<=? (string->utf8 "ciao") (string->utf8 "ciao"))		=> #t)
  (check (bytevector-u8-dictionary-ci<=? "ciao1" (string->utf8 "ciao"))		=> #f)
  (check (bytevector-u8-dictionary-ci<=? (string->utf8 "ciao") "ciao1")		=> #t)
  (check (bytevector-u8-dictionary-ci<=? (STRING->UTF8 "CIAO") (string->utf8 "ciao"))		=> #t)
  (check (bytevector-u8-dictionary-ci<=? "CIAO1" (string->utf8 "ciao"))		=> #f)
  (check (bytevector-u8-dictionary-ci<=? (STRING->UTF8 "CIAO") "ciao1")		=> #t)

  (check (bytevector-u8-dictionary-ci<=? "ci ao" (string->utf8 "ciao"))		=> #t)
  (check (bytevector-u8-dictionary-ci<=? (string->utf8 "ciao") "ci ao")		=> #t)
  (check (bytevector-u8-dictionary-ci<=? "ci\tao" (string->utf8 "ciao"))		=> #t)
  (check (bytevector-u8-dictionary-ci<=? (string->utf8 "ciao") "ci\tao")		=> #t)
  (check (bytevector-u8-dictionary-ci<=? "ci\nao" (string->utf8 "ciao"))		=> #t)
  (check (bytevector-u8-dictionary-ci<=? (string->utf8 "ciao") "ci\nao")		=> #t)
  (check (bytevector-u8-dictionary-ci<=? "ci\vao" (string->utf8 "ciao"))		=> #t)
  (check (bytevector-u8-dictionary-ci<=? (string->utf8 "ciao") "ci\tao")		=> #t)
  (check (bytevector-u8-dictionary-ci<=? "ci\fao" (string->utf8 "ciao"))		=> #t)
  (check (bytevector-u8-dictionary-ci<=? (string->utf8 "ciao") "ci\fao")		=> #t)
  (check (bytevector-u8-dictionary-ci<=? "ci\rao" (string->utf8 "ciao"))		=> #t)
  (check (bytevector-u8-dictionary-ci<=? (string->utf8 "ciao") "ci\rao")		=> #t)

;;; --------------------------------------------------------------------

  (check (bytevector-u8-dictionary-ci>? "" "")				=> #f)
  (check (bytevector-u8-dictionary-ci>? "a" "")			=> #t)
  (check (bytevector-u8-dictionary-ci>? "" "a")			=> #f)
  (check (bytevector-u8-dictionary-ci>? "ab" "a")			=> #t)
  (check (bytevector-u8-dictionary-ci>? "a" "ab")			=> #f)
  (check (bytevector-u8-dictionary-ci>? (string->utf8 "ciao") (string->utf8 "ciao"))			=> #f)
  (check (bytevector-u8-dictionary-ci>? "ciao1" (string->utf8 "ciao"))		=> #t)
  (check (bytevector-u8-dictionary-ci>? (string->utf8 "ciao") "ciao1")		=> #f)
  (check (bytevector-u8-dictionary-ci>? (STRING->UTF8 "CIAO") (string->utf8 "ciao"))			=> #f)
  (check (bytevector-u8-dictionary-ci>? "CIAO1" (string->utf8 "ciao"))		=> #t)
  (check (bytevector-u8-dictionary-ci>? (STRING->UTF8 "CIAO") "ciao1")		=> #f)

  (check (bytevector-u8-dictionary-ci>? "ci ao" (string->utf8 "ciao"))		=> #f)
  (check (bytevector-u8-dictionary-ci>? (string->utf8 "ciao") "ci ao")		=> #f)
  (check (bytevector-u8-dictionary-ci>? "ci\tao" (string->utf8 "ciao"))		=> #f)
  (check (bytevector-u8-dictionary-ci>? (string->utf8 "ciao") "ci\tao")		=> #f)
  (check (bytevector-u8-dictionary-ci>? "ci\nao" (string->utf8 "ciao"))		=> #f)
  (check (bytevector-u8-dictionary-ci>? (string->utf8 "ciao") "ci\nao")		=> #f)
  (check (bytevector-u8-dictionary-ci>? "ci\vao" (string->utf8 "ciao"))		=> #f)
  (check (bytevector-u8-dictionary-ci>? (string->utf8 "ciao") "ci\tao")		=> #f)
  (check (bytevector-u8-dictionary-ci>? "ci\fao" (string->utf8 "ciao"))		=> #f)
  (check (bytevector-u8-dictionary-ci>? (string->utf8 "ciao") "ci\fao")		=> #f)
  (check (bytevector-u8-dictionary-ci>? "ci\rao" (string->utf8 "ciao"))		=> #f)
  (check (bytevector-u8-dictionary-ci>? (string->utf8 "ciao") "ci\rao")		=> #f)

;;; --------------------------------------------------------------------

  (check (bytevector-u8-dictionary-ci>=? "" "")			=> #t)
  (check (bytevector-u8-dictionary-ci>=? "a" "")			=> #t)
  (check (bytevector-u8-dictionary-ci>=? "" "a")			=> #f)
  (check (bytevector-u8-dictionary-ci>=? "ab" "a")			=> #t)
  (check (bytevector-u8-dictionary-ci>=? "a" "ab")			=> #f)
  (check (bytevector-u8-dictionary-ci>=? (string->utf8 "ciao") (string->utf8 "ciao"))		=> #t)
  (check (bytevector-u8-dictionary-ci>=? "ciao1" (string->utf8 "ciao"))		=> #t)
  (check (bytevector-u8-dictionary-ci>=? (string->utf8 "ciao") "ciao1")		=> #f)
  (check (bytevector-u8-dictionary-ci>=? (STRING->UTF8 "CIAO") (string->utf8 "ciao"))		=> #t)
  (check (bytevector-u8-dictionary-ci>=? "CIAO1" (string->utf8 "ciao"))		=> #t)
  (check (bytevector-u8-dictionary-ci>=? (STRING->UTF8 "CIAO") "ciao1")		=> #f)

  (check (bytevector-u8-dictionary-ci>=? "ci ao" (string->utf8 "ciao"))		=> #t)
  (check (bytevector-u8-dictionary-ci>=? (string->utf8 "ciao") "ci ao")		=> #t)
  (check (bytevector-u8-dictionary-ci>=? "ci\tao" (string->utf8 "ciao"))		=> #t)
  (check (bytevector-u8-dictionary-ci>=? (string->utf8 "ciao") "ci\tao")		=> #t)
  (check (bytevector-u8-dictionary-ci>=? "ci\nao" (string->utf8 "ciao"))		=> #t)
  (check (bytevector-u8-dictionary-ci>=? (string->utf8 "ciao") "ci\nao")		=> #t)
  (check (bytevector-u8-dictionary-ci>=? "ci\vao" (string->utf8 "ciao"))		=> #t)
  (check (bytevector-u8-dictionary-ci>=? (string->utf8 "ciao") "ci\tao")		=> #t)
  (check (bytevector-u8-dictionary-ci>=? "ci\fao" (string->utf8 "ciao"))		=> #t)
  (check (bytevector-u8-dictionary-ci>=? (string->utf8 "ciao") "ci\fao")		=> #t)
  (check (bytevector-u8-dictionary-ci>=? "ci\rao" (string->utf8 "ciao"))		=> #t)
  (check (bytevector-u8-dictionary-ci>=? (string->utf8 "ciao") "ci\rao")		=> #t)

  #t)


(parameterise ((check-test-name 'comparison-bytevector-u8/number-case-sensitive))

  (check (bytevector-u8/numbers=? "" "")				=> #t)
  (check (bytevector-u8/numbers=? "a" "")				=> #f)
  (check (bytevector-u8/numbers=? "" "a")				=> #f)
  (check (bytevector-u8/numbers=? "a" "a")				=> #t)
  (check (bytevector-u8/numbers=? "1" "")				=> #f)
  (check (bytevector-u8/numbers=? "" "1")				=> #f)
  (check (bytevector-u8/numbers=? "1" "1")				=> #t)
  (check (bytevector-u8/numbers=? "1" "2")				=> #f)
  (check (bytevector-u8/numbers=? "2" "1")				=> #f)
  (check (bytevector-u8/numbers=? "a" "ab")				=> #f)
  (check (bytevector-u8/numbers=? "ab" "a")				=> #f)
  (check (bytevector-u8/numbers=? "a" "a1")				=> #f)
  (check (bytevector-u8/numbers=? "a1" "a")				=> #f)
  (check (bytevector-u8/numbers=? "1" "1a")				=> #f)
  (check (bytevector-u8/numbers=? "1a" "1")				=> #f)

  (check (bytevector-u8/numbers=? "123" "45")				=> #f)
  (check (bytevector-u8/numbers=? "45" "123")				=> #f)
  (check (bytevector-u8/numbers=? "ciao3" "ciao10")			=> #f)
  (check (bytevector-u8/numbers=? "ciao10" "ciao3")			=> #f)
  (check (bytevector-u8/numbers=? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (bytevector-u8/numbers=? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (bytevector-u8/numbers=? "foo4bar3zab" "foo4bar10")		=> #f)
  (check (bytevector-u8/numbers=? "foo4bar10" "foo4bar3zab")		=> #f)
  (check (bytevector-u8/numbers=? "foo12" "12foo")			=> #f)
  (check (bytevector-u8/numbers=? "12foo" "foo12")			=> #f)
  (check (bytevector-u8/numbers=? "12bar" "foobar")			=> #f)
  (check (bytevector-u8/numbers=? "12.3" "12.3")			=> #t)
  (check (bytevector-u8/numbers=? "12.3" "12.10")			=> #f)
  (check (bytevector-u8/numbers=? "12.10" "12.3")			=> #f)
  (check (bytevector-u8/numbers=? "12.3" "12,10")			=> #f)
  (check (bytevector-u8/numbers=? "12,10" "12.3")			=> #f)

;;; --------------------------------------------------------------------

  (check (bytevector-u8/numbers<>? "" "")				=> #f)
  (check (bytevector-u8/numbers<>? "a" "")				=> #t)
  (check (bytevector-u8/numbers<>? "" "a")				=> #t)
  (check (bytevector-u8/numbers<>? "a" "a")				=> #f)
  (check (bytevector-u8/numbers<>? "1" "")				=> #t)
  (check (bytevector-u8/numbers<>? "" "1")				=> #t)
  (check (bytevector-u8/numbers<>? "1" "1")				=> #f)
  (check (bytevector-u8/numbers<>? "1" "2")				=> #t)
  (check (bytevector-u8/numbers<>? "2" "1")				=> #t)
  (check (bytevector-u8/numbers<>? "a" "ab")				=> #t)
  (check (bytevector-u8/numbers<>? "ab" "a")				=> #t)
  (check (bytevector-u8/numbers<>? "a" "a1")				=> #t)
  (check (bytevector-u8/numbers<>? "a1" "a")				=> #t)
  (check (bytevector-u8/numbers<>? "1" "1a")				=> #t)
  (check (bytevector-u8/numbers<>? "1a" "1")				=> #t)

  (check (bytevector-u8/numbers<>? "123" "45")				=> #t)
  (check (bytevector-u8/numbers<>? "45" "123")				=> #t)
  (check (bytevector-u8/numbers<>? "ciao3" "ciao10")			=> #t)
  (check (bytevector-u8/numbers<>? "ciao10" "ciao3")			=> #t)
  (check (bytevector-u8/numbers<>? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (bytevector-u8/numbers<>? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (bytevector-u8/numbers<>? "foo4bar3zab" "foo4bar10")		=> #t)
  (check (bytevector-u8/numbers<>? "foo4bar10" "foo4bar3zab")		=> #t)
  (check (bytevector-u8/numbers<>? "foo12" "12foo")			=> #t)
  (check (bytevector-u8/numbers<>? "12foo" "foo12")			=> #t)
  (check (bytevector-u8/numbers<>? "12bar" "foobar")			=> #t)
  (check (bytevector-u8/numbers<>? "12.3" "12.3")			=> #f)
  (check (bytevector-u8/numbers<>? "12.3" "12.10")			=> #t)
  (check (bytevector-u8/numbers<>? "12.10" "12.3")			=> #t)
  (check (bytevector-u8/numbers<>? "12.3" "12,10")			=> #t)
  (check (bytevector-u8/numbers<>? "12,10" "12.3")			=> #t)

;;; --------------------------------------------------------------------

  (check (bytevector-u8/numbers<? "" "")				=> #f)
  (check (bytevector-u8/numbers<? "a" "")				=> #f)
  (check (bytevector-u8/numbers<? "" "a")				=> #t)
  (check (bytevector-u8/numbers<? "a" "a")				=> #f)
  (check (bytevector-u8/numbers<? "1" "")				=> #f)
  (check (bytevector-u8/numbers<? "" "1")				=> #t)
  (check (bytevector-u8/numbers<? "1" "1")				=> #f)
  (check (bytevector-u8/numbers<? "1" "2")				=> #t)
  (check (bytevector-u8/numbers<? "2" "1")				=> #f)
  (check (bytevector-u8/numbers<? "a" "ab")				=> #t)
  (check (bytevector-u8/numbers<? "ab" "a")				=> #f)
  (check (bytevector-u8/numbers<? "a" "a1")				=> #t)
  (check (bytevector-u8/numbers<? "a1" "a")				=> #f)
  (check (bytevector-u8/numbers<? "1" "1a")				=> #t)
  (check (bytevector-u8/numbers<? "1a" "1")				=> #f)

  (check (bytevector-u8/numbers<? "123" "45")				=> #f)
  (check (bytevector-u8/numbers<? "45" "123")				=> #t)
  (check (bytevector-u8/numbers<? "ciao3" "ciao10")			=> #t)
  (check (bytevector-u8/numbers<? "ciao10" "ciao3")			=> #f)
  (check (bytevector-u8/numbers<? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (bytevector-u8/numbers<? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (bytevector-u8/numbers<? "foo4bar3zab" "foo4bar10")		=> #t)
  (check (bytevector-u8/numbers<? "foo4bar10" "foo4bar3zab")		=> #f)
  (check (bytevector-u8/numbers<? "foo12" "12foo")			=> #f)
  (check (bytevector-u8/numbers<? "12foo" "foo12")			=> #t)
  (check (bytevector-u8/numbers<? "12bar" "foobar")			=> #t)
  (check (bytevector-u8/numbers<? "12.3" "12.3")			=> #f)
  (check (bytevector-u8/numbers<? "12.3" "12.10")			=> #t)
  (check (bytevector-u8/numbers<? "12.10" "12.3")			=> #f)
  (check (bytevector-u8/numbers<? "12.3" "12,10")			=> #f)
  (check (bytevector-u8/numbers<? "12,10" "12.3")			=> #t)

;;; --------------------------------------------------------------------

  (check (bytevector-u8/numbers<=? "" "")				=> #t)
  (check (bytevector-u8/numbers<=? "a" "")				=> #f)
  (check (bytevector-u8/numbers<=? "" "a")				=> #t)
  (check (bytevector-u8/numbers<=? "a" "a")				=> #t)
  (check (bytevector-u8/numbers<=? "1" "")				=> #f)
  (check (bytevector-u8/numbers<=? "" "1")				=> #t)
  (check (bytevector-u8/numbers<=? "1" "1")				=> #t)
  (check (bytevector-u8/numbers<=? "1" "2")				=> #t)
  (check (bytevector-u8/numbers<=? "2" "1")				=> #f)
  (check (bytevector-u8/numbers<=? "a" "ab")				=> #t)
  (check (bytevector-u8/numbers<=? "ab" "a")				=> #f)
  (check (bytevector-u8/numbers<=? "a" "a1")				=> #t)
  (check (bytevector-u8/numbers<=? "a1" "a")				=> #f)
  (check (bytevector-u8/numbers<=? "1" "1a")				=> #t)
  (check (bytevector-u8/numbers<=? "1a" "1")				=> #f)

  (check (bytevector-u8/numbers<=? "123" "45")				=> #f)
  (check (bytevector-u8/numbers<=? "45" "123")				=> #t)
  (check (bytevector-u8/numbers<=? "ciao3" "ciao10")			=> #t)
  (check (bytevector-u8/numbers<=? "ciao10" "ciao3")			=> #f)
  (check (bytevector-u8/numbers<=? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (bytevector-u8/numbers<=? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (bytevector-u8/numbers<=? "foo4bar3zab" "foo4bar10")		=> #t)
  (check (bytevector-u8/numbers<=? "foo4bar10" "foo4bar3zab")		=> #f)
  (check (bytevector-u8/numbers<=? "foo12" "12foo")			=> #f)
  (check (bytevector-u8/numbers<=? "12foo" "foo12")			=> #t)
  (check (bytevector-u8/numbers<=? "12bar" "foobar")			=> #t)
  (check (bytevector-u8/numbers<=? "12.3" "12.3")			=> #t)
  (check (bytevector-u8/numbers<=? "12.3" "12.10")			=> #t)
  (check (bytevector-u8/numbers<=? "12.10" "12.3")			=> #f)
  (check (bytevector-u8/numbers<=? "12.3" "12,10")			=> #f)
  (check (bytevector-u8/numbers<=? "12,10" "12.3")			=> #t)

;;; --------------------------------------------------------------------

  (check (bytevector-u8/numbers>? "" "")				=> #f)
  (check (bytevector-u8/numbers>? "a" "")				=> #t)
  (check (bytevector-u8/numbers>? "" "a")				=> #f)
  (check (bytevector-u8/numbers>? "a" "a")				=> #f)
  (check (bytevector-u8/numbers>? "1" "")				=> #t)
  (check (bytevector-u8/numbers>? "" "1")				=> #f)
  (check (bytevector-u8/numbers>? "1" "1")				=> #f)
  (check (bytevector-u8/numbers>? "1" "2")				=> #f)
  (check (bytevector-u8/numbers>? "2" "1")				=> #t)
  (check (bytevector-u8/numbers>? "a" "ab")				=> #f)
  (check (bytevector-u8/numbers>? "ab" "a")				=> #t)
  (check (bytevector-u8/numbers>? "a" "a1")				=> #f)
  (check (bytevector-u8/numbers>? "a1" "a")				=> #t)
  (check (bytevector-u8/numbers>? "1" "1a")				=> #f)
  (check (bytevector-u8/numbers>? "1a" "1")				=> #t)

  (check (bytevector-u8/numbers>? "123" "45")				=> #t)
  (check (bytevector-u8/numbers>? "45" "123")				=> #f)
  (check (bytevector-u8/numbers>? "ciao3" "ciao10")			=> #f)
  (check (bytevector-u8/numbers>? "ciao10" "ciao3")			=> #t)
  (check (bytevector-u8/numbers>? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (bytevector-u8/numbers>? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (bytevector-u8/numbers>? "foo4bar3zab" "foo4bar10")		=> #f)
  (check (bytevector-u8/numbers>? "foo4bar10" "foo4bar3zab")		=> #t)
  (check (bytevector-u8/numbers>? "foo12" "12foo")			=> #t)
  (check (bytevector-u8/numbers>? "12foo" "foo12")			=> #f)
  (check (bytevector-u8/numbers>? "12bar" "foobar")			=> #f)
  (check (bytevector-u8/numbers>? "12.3" "12.3")			=> #f)
  (check (bytevector-u8/numbers>? "12.3" "12.10")			=> #f)
  (check (bytevector-u8/numbers>? "12.10" "12.3")			=> #t)
  (check (bytevector-u8/numbers>? "12.3" "12,10")			=> #t)
  (check (bytevector-u8/numbers>? "12,10" "12.3")			=> #f)

;;; --------------------------------------------------------------------

  (check (bytevector-u8/numbers>=? "" "")				=> #t)
  (check (bytevector-u8/numbers>=? "a" "")				=> #t)
  (check (bytevector-u8/numbers>=? "" "a")				=> #f)
  (check (bytevector-u8/numbers>=? "a" "a")				=> #t)
  (check (bytevector-u8/numbers>=? "1" "")				=> #t)
  (check (bytevector-u8/numbers>=? "" "1")				=> #f)
  (check (bytevector-u8/numbers>=? "1" "1")				=> #t)
  (check (bytevector-u8/numbers>=? "1" "2")				=> #f)
  (check (bytevector-u8/numbers>=? "2" "1")				=> #t)
  (check (bytevector-u8/numbers>=? "a" "ab")				=> #f)
  (check (bytevector-u8/numbers>=? "ab" "a")				=> #t)
  (check (bytevector-u8/numbers>=? "a" "a1")				=> #f)
  (check (bytevector-u8/numbers>=? "a1" "a")				=> #t)
  (check (bytevector-u8/numbers>=? "1" "1a")				=> #f)
  (check (bytevector-u8/numbers>=? "1a" "1")				=> #t)

  (check (bytevector-u8/numbers>=? "123" "45")				=> #t)
  (check (bytevector-u8/numbers>=? "45" "123")				=> #f)
  (check (bytevector-u8/numbers>=? "ciao3" "ciao10")			=> #f)
  (check (bytevector-u8/numbers>=? "ciao10" "ciao3")			=> #t)
  (check (bytevector-u8/numbers>=? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (bytevector-u8/numbers>=? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (bytevector-u8/numbers>=? "foo4bar3zab" "foo4bar10")		=> #f)
  (check (bytevector-u8/numbers>=? "foo4bar10" "foo4bar3zab")		=> #t)
  (check (bytevector-u8/numbers>=? "foo12" "12foo")			=> #t)
  (check (bytevector-u8/numbers>=? "12foo" "foo12")			=> #f)
  (check (bytevector-u8/numbers>=? "12bar" "foobar")			=> #f)
  (check (bytevector-u8/numbers>=? "12.3" "12.3")			=> #t)
  (check (bytevector-u8/numbers>=? "12.3" "12.10")			=> #f)
  (check (bytevector-u8/numbers>=? "12.10" "12.3")			=> #t)
  (check (bytevector-u8/numbers>=? "12.3" "12,10")			=> #t)
  (check (bytevector-u8/numbers>=? "12,10" "12.3")			=> #f)

;;; --------------------------------------------------------------------

  (check
      (list-sort bytevector-u8/numbers<? (quote ("foo123" "foo42" "foo7")))
    => '("foo7" "foo42" "foo123"))

  #t)


(parameterise ((check-test-name 'comparison-bytevector-u8/number-case-insensitive))

  (check (bytevector-u8/numbers-ci=? "" "")				=> #t)
  (check (bytevector-u8/numbers-ci=? "a" "")				=> #f)
  (check (bytevector-u8/numbers-ci=? "" "a")				=> #f)
  (check (bytevector-u8/numbers-ci=? "a" "a")				=> #t)
  (check (bytevector-u8/numbers-ci=? "1" "")				=> #f)
  (check (bytevector-u8/numbers-ci=? "" "1")				=> #f)
  (check (bytevector-u8/numbers-ci=? "1" "1")				=> #t)
  (check (bytevector-u8/numbers-ci=? "1" "2")				=> #f)
  (check (bytevector-u8/numbers-ci=? "2" "1")				=> #f)
  (check (bytevector-u8/numbers-ci=? "a" "ab")				=> #f)
  (check (bytevector-u8/numbers-ci=? "ab" "a")				=> #f)
  (check (bytevector-u8/numbers-ci=? "a" "a1")				=> #f)
  (check (bytevector-u8/numbers-ci=? "a1" "a")				=> #f)
  (check (bytevector-u8/numbers-ci=? "1" "1a")				=> #f)
  (check (bytevector-u8/numbers-ci=? "1a" "1")				=> #f)
  (check (bytevector-u8/numbers-ci=? "a" "A")				=> #t)
  (check (bytevector-u8/numbers-ci=? "A" "a")				=> #t)

  (check (bytevector-u8/numbers-ci=? "123" "45")			=> #f)
  (check (bytevector-u8/numbers-ci=? "45" "123")			=> #f)
  (check (bytevector-u8/numbers-ci=? "ciao3" "ciao10")			=> #f)
  (check (bytevector-u8/numbers-ci=? "ciao10" "ciao3")			=> #f)
  (check (bytevector-u8/numbers-ci=? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (bytevector-u8/numbers-ci=? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (bytevector-u8/numbers-ci=? "foo4bar3zab" "foo4bar10")	=> #f)
  (check (bytevector-u8/numbers-ci=? "foo4bar10" "foo4bar3zab")	=> #f)
  (check (bytevector-u8/numbers-ci=? "foo12" "12foo")			=> #f)
  (check (bytevector-u8/numbers-ci=? "12foo" "foo12")			=> #f)
  (check (bytevector-u8/numbers-ci=? "12bar" "foobar")			=> #f)
  (check (bytevector-u8/numbers-ci=? "12.3" "12.3")			=> #t)
  (check (bytevector-u8/numbers-ci=? "12.3" "12.10")			=> #f)
  (check (bytevector-u8/numbers-ci=? "12.10" "12.3")			=> #f)
  (check (bytevector-u8/numbers-ci=? "12.3" "12,10")			=> #f)
  (check (bytevector-u8/numbers-ci=? "12,10" "12.3")			=> #f)

;;; --------------------------------------------------------------------

  (check (bytevector-u8/numbers-ci<>? "" "")				=> #f)
  (check (bytevector-u8/numbers-ci<>? "a" "")				=> #t)
  (check (bytevector-u8/numbers-ci<>? "" "a")				=> #t)
  (check (bytevector-u8/numbers-ci<>? "a" "a")				=> #f)
  (check (bytevector-u8/numbers-ci<>? "1" "")				=> #t)
  (check (bytevector-u8/numbers-ci<>? "" "1")				=> #t)
  (check (bytevector-u8/numbers-ci<>? "1" "1")				=> #f)
  (check (bytevector-u8/numbers-ci<>? "1" "2")				=> #t)
  (check (bytevector-u8/numbers-ci<>? "2" "1")				=> #t)
  (check (bytevector-u8/numbers-ci<>? "a" "ab")			=> #t)
  (check (bytevector-u8/numbers-ci<>? "ab" "a")			=> #t)
  (check (bytevector-u8/numbers-ci<>? "a" "a1")			=> #t)
  (check (bytevector-u8/numbers-ci<>? "a1" "a")			=> #t)
  (check (bytevector-u8/numbers-ci<>? "1" "1a")			=> #t)
  (check (bytevector-u8/numbers-ci<>? "1a" "1")			=> #t)
  (check (bytevector-u8/numbers-ci<>? "A" "a")				=> #f)
  (check (bytevector-u8/numbers-ci<>? "a" "A")				=> #f)

  (check (bytevector-u8/numbers-ci<>? "123" "45")			=> #t)
  (check (bytevector-u8/numbers-ci<>? "45" "123")			=> #t)
  (check (bytevector-u8/numbers-ci<>? "ciao3" "ciao10")		=> #t)
  (check (bytevector-u8/numbers-ci<>? "ciao10" "ciao3")		=> #t)
  (check (bytevector-u8/numbers-ci<>? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (bytevector-u8/numbers-ci<>? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (bytevector-u8/numbers-ci<>? "foo4bar3zab" "foo4bar10")	=> #t)
  (check (bytevector-u8/numbers-ci<>? "foo4bar10" "foo4bar3zab")	=> #t)
  (check (bytevector-u8/numbers-ci<>? "foo12" "12foo")			=> #t)
  (check (bytevector-u8/numbers-ci<>? "12foo" "foo12")			=> #t)
  (check (bytevector-u8/numbers-ci<>? "12bar" "foobar")		=> #t)
  (check (bytevector-u8/numbers-ci<>? "12.3" "12.3")			=> #f)
  (check (bytevector-u8/numbers-ci<>? "12.3" "12.10")			=> #t)
  (check (bytevector-u8/numbers-ci<>? "12.10" "12.3")			=> #t)
  (check (bytevector-u8/numbers-ci<>? "12.3" "12,10")			=> #t)
  (check (bytevector-u8/numbers-ci<>? "12,10" "12.3")			=> #t)

;;; --------------------------------------------------------------------

  (check (bytevector-u8/numbers-ci<? "" "")				=> #f)
  (check (bytevector-u8/numbers-ci<? "a" "")				=> #f)
  (check (bytevector-u8/numbers-ci<? "" "a")				=> #t)
  (check (bytevector-u8/numbers-ci<? "a" "a")				=> #f)
  (check (bytevector-u8/numbers-ci<? "1" "")				=> #f)
  (check (bytevector-u8/numbers-ci<? "" "1")				=> #t)
  (check (bytevector-u8/numbers-ci<? "1" "1")				=> #f)
  (check (bytevector-u8/numbers-ci<? "1" "2")				=> #t)
  (check (bytevector-u8/numbers-ci<? "2" "1")				=> #f)
  (check (bytevector-u8/numbers-ci<? "a" "ab")				=> #t)
  (check (bytevector-u8/numbers-ci<? "ab" "a")				=> #f)
  (check (bytevector-u8/numbers-ci<? "a" "a1")				=> #t)
  (check (bytevector-u8/numbers-ci<? "a1" "a")				=> #f)
  (check (bytevector-u8/numbers-ci<? "1" "1a")				=> #t)
  (check (bytevector-u8/numbers-ci<? "1a" "1")				=> #f)
  (check (bytevector-u8/numbers-ci<? "a" "A")				=> #f)
  (check (bytevector-u8/numbers-ci<? "A" "a")				=> #f)

  (check (bytevector-u8/numbers-ci<? "123" "45")			=> #f)
  (check (bytevector-u8/numbers-ci<? "45" "123")			=> #t)
  (check (bytevector-u8/numbers-ci<? "ciao3" "ciao10")			=> #t)
  (check (bytevector-u8/numbers-ci<? "ciao10" "ciao3")			=> #f)
  (check (bytevector-u8/numbers-ci<? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (bytevector-u8/numbers-ci<? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (bytevector-u8/numbers-ci<? "foo4bar3zab" "foo4bar10")	=> #t)
  (check (bytevector-u8/numbers-ci<? "foo4bar10" "foo4bar3zab")	=> #f)
  (check (bytevector-u8/numbers-ci<? "foo12" "12foo")			=> #f)
  (check (bytevector-u8/numbers-ci<? "12foo" "foo12")			=> #t)
  (check (bytevector-u8/numbers-ci<? "12bar" "foobar")			=> #t)
  (check (bytevector-u8/numbers-ci<? "12.3" "12.3")			=> #f)
  (check (bytevector-u8/numbers-ci<? "12.3" "12.10")			=> #t)
  (check (bytevector-u8/numbers-ci<? "12.10" "12.3")			=> #f)
  (check (bytevector-u8/numbers-ci<? "12.3" "12,10")			=> #f)
  (check (bytevector-u8/numbers-ci<? "12,10" "12.3")			=> #t)

;;; --------------------------------------------------------------------

  (check (bytevector-u8/numbers-ci<=? "" "")				=> #t)
  (check (bytevector-u8/numbers-ci<=? "a" "")				=> #f)
  (check (bytevector-u8/numbers-ci<=? "" "a")				=> #t)
  (check (bytevector-u8/numbers-ci<=? "a" "a")				=> #t)
  (check (bytevector-u8/numbers-ci<=? "1" "")				=> #f)
  (check (bytevector-u8/numbers-ci<=? "" "1")				=> #t)
  (check (bytevector-u8/numbers-ci<=? "1" "1")				=> #t)
  (check (bytevector-u8/numbers-ci<=? "1" "2")				=> #t)
  (check (bytevector-u8/numbers-ci<=? "2" "1")				=> #f)
  (check (bytevector-u8/numbers-ci<=? "a" "ab")			=> #t)
  (check (bytevector-u8/numbers-ci<=? "ab" "a")			=> #f)
  (check (bytevector-u8/numbers-ci<=? "a" "a1")			=> #t)
  (check (bytevector-u8/numbers-ci<=? "a1" "a")			=> #f)
  (check (bytevector-u8/numbers-ci<=? "1" "1a")			=> #t)
  (check (bytevector-u8/numbers-ci<=? "1a" "1")			=> #f)
  (check (bytevector-u8/numbers-ci<=? "a" "A")				=> #t)
  (check (bytevector-u8/numbers-ci<=? "A" "a")				=> #t)

  (check (bytevector-u8/numbers-ci<=? "123" "45")			=> #f)
  (check (bytevector-u8/numbers-ci<=? "45" "123")			=> #t)
  (check (bytevector-u8/numbers-ci<=? "ciao3" "ciao10")		=> #t)
  (check (bytevector-u8/numbers-ci<=? "ciao10" "ciao3")		=> #f)
  (check (bytevector-u8/numbers-ci<=? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (bytevector-u8/numbers-ci<=? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (bytevector-u8/numbers-ci<=? "foo4bar3zab" "foo4bar10")	=> #t)
  (check (bytevector-u8/numbers-ci<=? "foo4bar10" "foo4bar3zab")	=> #f)
  (check (bytevector-u8/numbers-ci<=? "foo12" "12foo")			=> #f)
  (check (bytevector-u8/numbers-ci<=? "12foo" "foo12")			=> #t)
  (check (bytevector-u8/numbers-ci<=? "12bar" "foobar")		=> #t)
  (check (bytevector-u8/numbers-ci<=? "12.3" "12.3")			=> #t)
  (check (bytevector-u8/numbers-ci<=? "12.3" "12.10")			=> #t)
  (check (bytevector-u8/numbers-ci<=? "12.10" "12.3")			=> #f)
  (check (bytevector-u8/numbers-ci<=? "12.3" "12,10")			=> #f)
  (check (bytevector-u8/numbers-ci<=? "12,10" "12.3")			=> #t)

;;; --------------------------------------------------------------------

  (check (bytevector-u8/numbers-ci>? "" "")				=> #f)
  (check (bytevector-u8/numbers-ci>? "a" "")				=> #t)
  (check (bytevector-u8/numbers-ci>? "" "a")				=> #f)
  (check (bytevector-u8/numbers-ci>? "a" "a")				=> #f)
  (check (bytevector-u8/numbers-ci>? "1" "")				=> #t)
  (check (bytevector-u8/numbers-ci>? "" "1")				=> #f)
  (check (bytevector-u8/numbers-ci>? "1" "1")				=> #f)
  (check (bytevector-u8/numbers-ci>? "1" "2")				=> #f)
  (check (bytevector-u8/numbers-ci>? "2" "1")				=> #t)
  (check (bytevector-u8/numbers-ci>? "a" "ab")				=> #f)
  (check (bytevector-u8/numbers-ci>? "ab" "a")				=> #t)
  (check (bytevector-u8/numbers-ci>? "a" "a1")				=> #f)
  (check (bytevector-u8/numbers-ci>? "a1" "a")				=> #t)
  (check (bytevector-u8/numbers-ci>? "1" "1a")				=> #f)
  (check (bytevector-u8/numbers-ci>? "1a" "1")				=> #t)
  (check (bytevector-u8/numbers-ci>? "a" "A")				=> #f)
  (check (bytevector-u8/numbers-ci>? "A" "a")				=> #f)

  (check (bytevector-u8/numbers-ci>? "123" "45")			=> #t)
  (check (bytevector-u8/numbers-ci>? "45" "123")			=> #f)
  (check (bytevector-u8/numbers-ci>? "ciao3" "ciao10")			=> #f)
  (check (bytevector-u8/numbers-ci>? "ciao10" "ciao3")			=> #t)
  (check (bytevector-u8/numbers-ci>? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (bytevector-u8/numbers-ci>? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (bytevector-u8/numbers-ci>? "foo4bar3zab" "foo4bar10")	=> #f)
  (check (bytevector-u8/numbers-ci>? "foo4bar10" "foo4bar3zab")	=> #t)
  (check (bytevector-u8/numbers-ci>? "foo12" "12foo")			=> #t)
  (check (bytevector-u8/numbers-ci>? "12foo" "foo12")			=> #f)
  (check (bytevector-u8/numbers-ci>? "12bar" "foobar")			=> #f)
  (check (bytevector-u8/numbers-ci>? "12.3" "12.3")			=> #f)
  (check (bytevector-u8/numbers-ci>? "12.3" "12.10")			=> #f)
  (check (bytevector-u8/numbers-ci>? "12.10" "12.3")			=> #t)
  (check (bytevector-u8/numbers-ci>? "12.3" "12,10")			=> #t)
  (check (bytevector-u8/numbers-ci>? "12,10" "12.3")			=> #f)

;;; --------------------------------------------------------------------

  (check (bytevector-u8/numbers-ci>=? "" "")				=> #t)
  (check (bytevector-u8/numbers-ci>=? "a" "")				=> #t)
  (check (bytevector-u8/numbers-ci>=? "" "a")				=> #f)
  (check (bytevector-u8/numbers-ci>=? "a" "a")				=> #t)
  (check (bytevector-u8/numbers-ci>=? "1" "")				=> #t)
  (check (bytevector-u8/numbers-ci>=? "" "1")				=> #f)
  (check (bytevector-u8/numbers-ci>=? "1" "1")				=> #t)
  (check (bytevector-u8/numbers-ci>=? "1" "2")				=> #f)
  (check (bytevector-u8/numbers-ci>=? "2" "1")				=> #t)
  (check (bytevector-u8/numbers-ci>=? "a" "ab")			=> #f)
  (check (bytevector-u8/numbers-ci>=? "ab" "a")			=> #t)
  (check (bytevector-u8/numbers-ci>=? "a" "a1")			=> #f)
  (check (bytevector-u8/numbers-ci>=? "a1" "a")			=> #t)
  (check (bytevector-u8/numbers-ci>=? "1" "1a")			=> #f)
  (check (bytevector-u8/numbers-ci>=? "1a" "1")			=> #t)
  (check (bytevector-u8/numbers-ci>=? "a" "A")				=> #t)
  (check (bytevector-u8/numbers-ci>=? "A" "a")				=> #t)

  (check (bytevector-u8/numbers-ci>=? "123" "45")			=> #t)
  (check (bytevector-u8/numbers-ci>=? "45" "123")			=> #f)
  (check (bytevector-u8/numbers-ci>=? "ciao3" "ciao10")		=> #f)
  (check (bytevector-u8/numbers-ci>=? "ciao10" "ciao3")		=> #t)
  (check (bytevector-u8/numbers-ci>=? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (bytevector-u8/numbers-ci>=? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (bytevector-u8/numbers-ci>=? "foo4bar3zab" "foo4bar10")	=> #f)
  (check (bytevector-u8/numbers-ci>=? "foo4bar10" "foo4bar3zab")	=> #t)
  (check (bytevector-u8/numbers-ci>=? "foo12" "12foo")			=> #t)
  (check (bytevector-u8/numbers-ci>=? "12foo" "foo12")			=> #f)
  (check (bytevector-u8/numbers-ci>=? "12bar" "foobar")		=> #f)
  (check (bytevector-u8/numbers-ci>=? "12.3" "12.3")			=> #t)
  (check (bytevector-u8/numbers-ci>=? "12.3" "12.10")			=> #f)
  (check (bytevector-u8/numbers-ci>=? "12.10" "12.3")			=> #t)
  (check (bytevector-u8/numbers-ci>=? "12.3" "12,10")			=> #t)
  (check (bytevector-u8/numbers-ci>=? "12,10" "12.3")			=> #f)

  #t)


(parameterise ((check-test-name 'comparison-dictionary-bytevector-u8/number-case-sensitive))

  (check (bytevector-u8/numbers-dictionary=? "" "")				=> #t)
  (check (bytevector-u8/numbers-dictionary=? "a" "")				=> #f)
  (check (bytevector-u8/numbers-dictionary=? "" "a")				=> #f)
  (check (bytevector-u8/numbers-dictionary=? "a" "a")				=> #t)
  (check (bytevector-u8/numbers-dictionary=? "1" "")				=> #f)
  (check (bytevector-u8/numbers-dictionary=? "" "1")				=> #f)
  (check (bytevector-u8/numbers-dictionary=? "1" "1")				=> #t)
  (check (bytevector-u8/numbers-dictionary=? "1" "2")				=> #f)
  (check (bytevector-u8/numbers-dictionary=? "2" "1")				=> #f)
  (check (bytevector-u8/numbers-dictionary=? "a" "ab")				=> #f)
  (check (bytevector-u8/numbers-dictionary=? "ab" "a")				=> #f)
  (check (bytevector-u8/numbers-dictionary=? "a" "a1")				=> #f)
  (check (bytevector-u8/numbers-dictionary=? "a1" "a")				=> #f)
  (check (bytevector-u8/numbers-dictionary=? "1" "1a")				=> #f)
  (check (bytevector-u8/numbers-dictionary=? "1a" "1")				=> #f)

  (check (bytevector-u8/numbers-dictionary=? "123" "45")			=> #f)
  (check (bytevector-u8/numbers-dictionary=? "45" "123")			=> #f)
  (check (bytevector-u8/numbers-dictionary=? "ciao3" "ciao10")			=> #f)
  (check (bytevector-u8/numbers-dictionary=? "ciao10" "ciao3")			=> #f)
  (check (bytevector-u8/numbers-dictionary=? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (bytevector-u8/numbers-dictionary=? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (bytevector-u8/numbers-dictionary=? "foo4bar3zab" "foo4bar10")	=> #f)
  (check (bytevector-u8/numbers-dictionary=? "foo4bar10" "foo4bar3zab")	=> #f)
  (check (bytevector-u8/numbers-dictionary=? "foo12" "12foo")			=> #f)
  (check (bytevector-u8/numbers-dictionary=? "12foo" "foo12")			=> #f)
  (check (bytevector-u8/numbers-dictionary=? "12bar" "foobar")			=> #f)
  (check (bytevector-u8/numbers-dictionary=? "12.3" "12.3")			=> #t)
  (check (bytevector-u8/numbers-dictionary=? "12.3" "12.10")			=> #f)
  (check (bytevector-u8/numbers-dictionary=? "12.10" "12.3")			=> #f)
  (check (bytevector-u8/numbers-dictionary=? "12.3" "12,10")			=> #f)
  (check (bytevector-u8/numbers-dictionary=? "12,10" "12.3")			=> #f)

  (check (bytevector-u8/numbers-dictionary=? "fo o4b\tar3\nza\rb10" "foo4bar3zab2")	=> #f)
  (check (bytevector-u8/numbers-dictionary=? "foo4bar3zab2" "fo o4b\tar3\nza\rb10")	=> #f)

;;; --------------------------------------------------------------------

  (check (bytevector-u8/numbers-dictionary<>? "" "")				=> #f)
  (check (bytevector-u8/numbers-dictionary<>? "a" "")				=> #t)
  (check (bytevector-u8/numbers-dictionary<>? "" "a")				=> #t)
  (check (bytevector-u8/numbers-dictionary<>? "a" "a")				=> #f)
  (check (bytevector-u8/numbers-dictionary<>? "1" "")				=> #t)
  (check (bytevector-u8/numbers-dictionary<>? "" "1")				=> #t)
  (check (bytevector-u8/numbers-dictionary<>? "1" "1")				=> #f)
  (check (bytevector-u8/numbers-dictionary<>? "1" "2")				=> #t)
  (check (bytevector-u8/numbers-dictionary<>? "2" "1")				=> #t)
  (check (bytevector-u8/numbers-dictionary<>? "a" "ab")			=> #t)
  (check (bytevector-u8/numbers-dictionary<>? "ab" "a")			=> #t)
  (check (bytevector-u8/numbers-dictionary<>? "a" "a1")			=> #t)
  (check (bytevector-u8/numbers-dictionary<>? "a1" "a")			=> #t)
  (check (bytevector-u8/numbers-dictionary<>? "1" "1a")			=> #t)
  (check (bytevector-u8/numbers-dictionary<>? "1a" "1")			=> #t)

  (check (bytevector-u8/numbers-dictionary<>? "123" "45")			=> #t)
  (check (bytevector-u8/numbers-dictionary<>? "45" "123")			=> #t)
  (check (bytevector-u8/numbers-dictionary<>? "ciao3" "ciao10")		=> #t)
  (check (bytevector-u8/numbers-dictionary<>? "ciao10" "ciao3")		=> #t)
  (check (bytevector-u8/numbers-dictionary<>? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (bytevector-u8/numbers-dictionary<>? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (bytevector-u8/numbers-dictionary<>? "foo4bar3zab" "foo4bar10")	=> #t)
  (check (bytevector-u8/numbers-dictionary<>? "foo4bar10" "foo4bar3zab")	=> #t)
  (check (bytevector-u8/numbers-dictionary<>? "foo12" "12foo")			=> #t)
  (check (bytevector-u8/numbers-dictionary<>? "12foo" "foo12")			=> #t)
  (check (bytevector-u8/numbers-dictionary<>? "12bar" "foobar")		=> #t)
  (check (bytevector-u8/numbers-dictionary<>? "12.3" "12.3")			=> #f)
  (check (bytevector-u8/numbers-dictionary<>? "12.3" "12.10")			=> #t)
  (check (bytevector-u8/numbers-dictionary<>? "12.10" "12.3")			=> #t)
  (check (bytevector-u8/numbers-dictionary<>? "12.3" "12,10")			=> #t)
  (check (bytevector-u8/numbers-dictionary<>? "12,10" "12.3")			=> #t)

  (check (bytevector-u8/numbers-dictionary<>? "fo o4b\tar3\nza\rb10" "foo4bar3zab2")	=> #t)
  (check (bytevector-u8/numbers-dictionary<>? "foo4bar3zab2" "fo o4b\tar3\nza\rb10")	=> #t)

;;; --------------------------------------------------------------------

  (check (bytevector-u8/numbers-dictionary<? "" "")				=> #f)
  (check (bytevector-u8/numbers-dictionary<? "a" "")				=> #f)
  (check (bytevector-u8/numbers-dictionary<? "" "a")				=> #t)
  (check (bytevector-u8/numbers-dictionary<? "a" "a")				=> #f)
  (check (bytevector-u8/numbers-dictionary<? "1" "")				=> #f)
  (check (bytevector-u8/numbers-dictionary<? "" "1")				=> #t)
  (check (bytevector-u8/numbers-dictionary<? "1" "1")				=> #f)
  (check (bytevector-u8/numbers-dictionary<? "1" "2")				=> #t)
  (check (bytevector-u8/numbers-dictionary<? "2" "1")				=> #f)
  (check (bytevector-u8/numbers-dictionary<? "a" "ab")				=> #t)
  (check (bytevector-u8/numbers-dictionary<? "ab" "a")				=> #f)
  (check (bytevector-u8/numbers-dictionary<? "a" "a1")				=> #t)
  (check (bytevector-u8/numbers-dictionary<? "a1" "a")				=> #f)
  (check (bytevector-u8/numbers-dictionary<? "1" "1a")				=> #t)
  (check (bytevector-u8/numbers-dictionary<? "1a" "1")				=> #f)

  (check (bytevector-u8/numbers-dictionary<? "123" "45")			=> #f)
  (check (bytevector-u8/numbers-dictionary<? "45" "123")			=> #t)
  (check (bytevector-u8/numbers-dictionary<? "ciao3" "ciao10")			=> #t)
  (check (bytevector-u8/numbers-dictionary<? "ciao10" "ciao3")			=> #f)
  (check (bytevector-u8/numbers-dictionary<? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (bytevector-u8/numbers-dictionary<? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (bytevector-u8/numbers-dictionary<? "foo4bar3zab" "foo4bar10")	=> #t)
  (check (bytevector-u8/numbers-dictionary<? "foo4bar10" "foo4bar3zab")	=> #f)
  (check (bytevector-u8/numbers-dictionary<? "foo12" "12foo")			=> #f)
  (check (bytevector-u8/numbers-dictionary<? "12foo" "foo12")			=> #t)
  (check (bytevector-u8/numbers-dictionary<? "12bar" "foobar")			=> #t)
  (check (bytevector-u8/numbers-dictionary<? "12.3" "12.3")			=> #f)
  (check (bytevector-u8/numbers-dictionary<? "12.3" "12.10")			=> #t)
  (check (bytevector-u8/numbers-dictionary<? "12.10" "12.3")			=> #f)
  (check (bytevector-u8/numbers-dictionary<? "12.3" "12,10")			=> #f)
  (check (bytevector-u8/numbers-dictionary<? "12,10" "12.3")			=> #t)

  (check 'this (bytevector-u8/numbers-dictionary<? "fo o4b\tar3\nza\rb10" "foo4bar3zab2")	=> #f)
  (check (bytevector-u8/numbers-dictionary<? "foo4bar3zab2" "fo o4b\tar3\nza\rb10")	=> #t)

;;; --------------------------------------------------------------------

  (check (bytevector-u8/numbers-dictionary<=? "" "")				=> #t)
  (check (bytevector-u8/numbers-dictionary<=? "a" "")				=> #f)
  (check (bytevector-u8/numbers-dictionary<=? "" "a")				=> #t)
  (check (bytevector-u8/numbers-dictionary<=? "a" "a")				=> #t)
  (check (bytevector-u8/numbers-dictionary<=? "1" "")				=> #f)
  (check (bytevector-u8/numbers-dictionary<=? "" "1")				=> #t)
  (check (bytevector-u8/numbers-dictionary<=? "1" "1")				=> #t)
  (check (bytevector-u8/numbers-dictionary<=? "1" "2")				=> #t)
  (check (bytevector-u8/numbers-dictionary<=? "2" "1")				=> #f)
  (check (bytevector-u8/numbers-dictionary<=? "a" "ab")			=> #t)
  (check (bytevector-u8/numbers-dictionary<=? "ab" "a")			=> #f)
  (check (bytevector-u8/numbers-dictionary<=? "a" "a1")			=> #t)
  (check (bytevector-u8/numbers-dictionary<=? "a1" "a")			=> #f)
  (check (bytevector-u8/numbers-dictionary<=? "1" "1a")			=> #t)
  (check (bytevector-u8/numbers-dictionary<=? "1a" "1")			=> #f)

  (check (bytevector-u8/numbers-dictionary<=? "123" "45")			=> #f)
  (check (bytevector-u8/numbers-dictionary<=? "45" "123")			=> #t)
  (check (bytevector-u8/numbers-dictionary<=? "ciao3" "ciao10")		=> #t)
  (check (bytevector-u8/numbers-dictionary<=? "ciao10" "ciao3")		=> #f)
  (check (bytevector-u8/numbers-dictionary<=? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (bytevector-u8/numbers-dictionary<=? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (bytevector-u8/numbers-dictionary<=? "foo4bar3zab" "foo4bar10")	=> #t)
  (check (bytevector-u8/numbers-dictionary<=? "foo4bar10" "foo4bar3zab")	=> #f)
  (check (bytevector-u8/numbers-dictionary<=? "foo12" "12foo")			=> #f)
  (check (bytevector-u8/numbers-dictionary<=? "12foo" "foo12")			=> #t)
  (check (bytevector-u8/numbers-dictionary<=? "12bar" "foobar")		=> #t)
  (check (bytevector-u8/numbers-dictionary<=? "12.3" "12.3")			=> #t)
  (check (bytevector-u8/numbers-dictionary<=? "12.3" "12.10")			=> #t)
  (check (bytevector-u8/numbers-dictionary<=? "12.10" "12.3")			=> #f)
  (check (bytevector-u8/numbers-dictionary<=? "12.3" "12,10")			=> #f)
  (check (bytevector-u8/numbers-dictionary<=? "12,10" "12.3")			=> #t)

  (check (bytevector-u8/numbers-dictionary<=? "fo o4b\tar3\nza\rb10" "foo4bar3zab2")	=> #f)
  (check (bytevector-u8/numbers-dictionary<=? "foo4bar3zab2" "fo o4b\tar3\nza\rb10")	=> #t)

;;; --------------------------------------------------------------------

  (check (bytevector-u8/numbers-dictionary>? "" "")				=> #f)
  (check (bytevector-u8/numbers-dictionary>? "a" "")				=> #t)
  (check (bytevector-u8/numbers-dictionary>? "" "a")				=> #f)
  (check (bytevector-u8/numbers-dictionary>? "a" "a")				=> #f)
  (check (bytevector-u8/numbers-dictionary>? "1" "")				=> #t)
  (check (bytevector-u8/numbers-dictionary>? "" "1")				=> #f)
  (check (bytevector-u8/numbers-dictionary>? "1" "1")				=> #f)
  (check (bytevector-u8/numbers-dictionary>? "1" "2")				=> #f)
  (check (bytevector-u8/numbers-dictionary>? "2" "1")				=> #t)
  (check (bytevector-u8/numbers-dictionary>? "a" "ab")				=> #f)
  (check (bytevector-u8/numbers-dictionary>? "ab" "a")				=> #t)
  (check (bytevector-u8/numbers-dictionary>? "a" "a1")				=> #f)
  (check (bytevector-u8/numbers-dictionary>? "a1" "a")				=> #t)
  (check (bytevector-u8/numbers-dictionary>? "1" "1a")				=> #f)
  (check (bytevector-u8/numbers-dictionary>? "1a" "1")				=> #t)

  (check (bytevector-u8/numbers-dictionary>? "123" "45")			=> #t)
  (check (bytevector-u8/numbers-dictionary>? "45" "123")			=> #f)
  (check (bytevector-u8/numbers-dictionary>? "ciao3" "ciao10")			=> #f)
  (check (bytevector-u8/numbers-dictionary>? "ciao10" "ciao3")			=> #t)
  (check (bytevector-u8/numbers-dictionary>? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (bytevector-u8/numbers-dictionary>? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (bytevector-u8/numbers-dictionary>? "foo4bar3zab" "foo4bar10")	=> #f)
  (check (bytevector-u8/numbers-dictionary>? "foo4bar10" "foo4bar3zab")	=> #t)
  (check (bytevector-u8/numbers-dictionary>? "foo12" "12foo")			=> #t)
  (check (bytevector-u8/numbers-dictionary>? "12foo" "foo12")			=> #f)
  (check (bytevector-u8/numbers-dictionary>? "12bar" "foobar")			=> #f)
  (check (bytevector-u8/numbers-dictionary>? "12.3" "12.3")			=> #f)
  (check (bytevector-u8/numbers-dictionary>? "12.3" "12.10")			=> #f)
  (check (bytevector-u8/numbers-dictionary>? "12.10" "12.3")			=> #t)
  (check (bytevector-u8/numbers-dictionary>? "12.3" "12,10")			=> #t)
  (check (bytevector-u8/numbers-dictionary>? "12,10" "12.3")			=> #f)

  (check (bytevector-u8/numbers-dictionary>? "fo o4b\tar3\nza\rb10" "foo4bar3zab2")	=> #t)
  (check (bytevector-u8/numbers-dictionary>? "foo4bar3zab2" "fo o4b\tar3\nza\rb10")	=> #f)

;;; --------------------------------------------------------------------

  (check (bytevector-u8/numbers-dictionary>=? "" "")				=> #t)
  (check (bytevector-u8/numbers-dictionary>=? "a" "")				=> #t)
  (check (bytevector-u8/numbers-dictionary>=? "" "a")				=> #f)
  (check (bytevector-u8/numbers-dictionary>=? "a" "a")				=> #t)
  (check (bytevector-u8/numbers-dictionary>=? "1" "")				=> #t)
  (check (bytevector-u8/numbers-dictionary>=? "" "1")				=> #f)
  (check (bytevector-u8/numbers-dictionary>=? "1" "1")				=> #t)
  (check (bytevector-u8/numbers-dictionary>=? "1" "2")				=> #f)
  (check (bytevector-u8/numbers-dictionary>=? "2" "1")				=> #t)
  (check (bytevector-u8/numbers-dictionary>=? "a" "ab")			=> #f)
  (check (bytevector-u8/numbers-dictionary>=? "ab" "a")			=> #t)
  (check (bytevector-u8/numbers-dictionary>=? "a" "a1")			=> #f)
  (check (bytevector-u8/numbers-dictionary>=? "a1" "a")			=> #t)
  (check (bytevector-u8/numbers-dictionary>=? "1" "1a")			=> #f)
  (check (bytevector-u8/numbers-dictionary>=? "1a" "1")			=> #t)

  (check (bytevector-u8/numbers-dictionary>=? "123" "45")			=> #t)
  (check (bytevector-u8/numbers-dictionary>=? "45" "123")			=> #f)
  (check (bytevector-u8/numbers-dictionary>=? "ciao3" "ciao10")		=> #f)
  (check (bytevector-u8/numbers-dictionary>=? "ciao10" "ciao3")		=> #t)
  (check (bytevector-u8/numbers-dictionary>=? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (bytevector-u8/numbers-dictionary>=? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (bytevector-u8/numbers-dictionary>=? "foo4bar3zab" "foo4bar10")	=> #f)
  (check (bytevector-u8/numbers-dictionary>=? "foo4bar10" "foo4bar3zab")	=> #t)
  (check (bytevector-u8/numbers-dictionary>=? "foo12" "12foo")			=> #t)
  (check (bytevector-u8/numbers-dictionary>=? "12foo" "foo12")			=> #f)
  (check (bytevector-u8/numbers-dictionary>=? "12bar" "foobar")		=> #f)
  (check (bytevector-u8/numbers-dictionary>=? "12.3" "12.3")			=> #t)
  (check (bytevector-u8/numbers-dictionary>=? "12.3" "12.10")			=> #f)
  (check (bytevector-u8/numbers-dictionary>=? "12.10" "12.3")			=> #t)
  (check (bytevector-u8/numbers-dictionary>=? "12.3" "12,10")			=> #t)
  (check (bytevector-u8/numbers-dictionary>=? "12,10" "12.3")			=> #f)

  (check (bytevector-u8/numbers-dictionary>=? "fo o4b\tar3\nza\rb10" "foo4bar3zab2")	=> #t)
  (check (bytevector-u8/numbers-dictionary>=? "foo4bar3zab2" "fo o4b\tar3\nza\rb10")	=> #f)

  #t)


(parameterise ((check-test-name 'comparison-dictionary-bytevector-u8/number-case-insensitive))

  (check (bytevector-u8/numbers-dictionary-ci=? "" "")				=> #t)
  (check (bytevector-u8/numbers-dictionary-ci=? "a" "")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci=? "" "a")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci=? "a" "a")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci=? "1" "")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci=? "" "1")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci=? "1" "1")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci=? "1" "2")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci=? "2" "1")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci=? "a" "ab")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci=? "ab" "a")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci=? "a" "a1")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci=? "a1" "a")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci=? "1" "1a")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci=? "1a" "1")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci=? "a" "A")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci=? "A" "a")			=> #t)

  (check (bytevector-u8/numbers-dictionary-ci=? "123" "45")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci=? "45" "123")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci=? "ciao3" "ciao10")		=> #f)
  (check (bytevector-u8/numbers-dictionary-ci=? "ciao10" "ciao3")		=> #f)
  (check (bytevector-u8/numbers-dictionary-ci=? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (bytevector-u8/numbers-dictionary-ci=? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (bytevector-u8/numbers-dictionary-ci=? "foo4bar3zab" "foo4bar10")	=> #f)
  (check (bytevector-u8/numbers-dictionary-ci=? "foo4bar10" "foo4bar3zab")	=> #f)
  (check (bytevector-u8/numbers-dictionary-ci=? "foo12" "12foo")		=> #f)
  (check (bytevector-u8/numbers-dictionary-ci=? "12foo" "foo12")		=> #f)
  (check (bytevector-u8/numbers-dictionary-ci=? "12bar" "foobar")		=> #f)
  (check (bytevector-u8/numbers-dictionary-ci=? "12.3" "12.3")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci=? "12.3" "12.10")		=> #f)
  (check (bytevector-u8/numbers-dictionary-ci=? "12.10" "12.3")		=> #f)
  (check (bytevector-u8/numbers-dictionary-ci=? "12.3" "12,10")		=> #f)
  (check (bytevector-u8/numbers-dictionary-ci=? "12,10" "12.3")		=> #f)

;;; --------------------------------------------------------------------

  (check (bytevector-u8/numbers-dictionary-ci<>? "" "")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci<>? "a" "")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<>? "" "a")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<>? "a" "a")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci<>? "1" "")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<>? "" "1")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<>? "1" "1")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci<>? "1" "2")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<>? "2" "1")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<>? "a" "ab")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<>? "ab" "a")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<>? "a" "a1")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<>? "a1" "a")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<>? "1" "1a")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<>? "1a" "1")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<>? "A" "a")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci<>? "a" "A")			=> #f)

  (check (bytevector-u8/numbers-dictionary-ci<>? "123" "45")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<>? "45" "123")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<>? "ciao3" "ciao10")		=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<>? "ciao10" "ciao3")		=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<>? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<>? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<>? "foo4bar3zab" "foo4bar10")	=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<>? "foo4bar10" "foo4bar3zab")	=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<>? "foo12" "12foo")		=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<>? "12foo" "foo12")		=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<>? "12bar" "foobar")		=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<>? "12.3" "12.3")		=> #f)
  (check (bytevector-u8/numbers-dictionary-ci<>? "12.3" "12.10")		=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<>? "12.10" "12.3")		=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<>? "12.3" "12,10")		=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<>? "12,10" "12.3")		=> #t)

;;; --------------------------------------------------------------------

  (check (bytevector-u8/numbers-dictionary-ci<? "" "")				=> #f)
  (check (bytevector-u8/numbers-dictionary-ci<? "a" "")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci<? "" "a")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<? "a" "a")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci<? "1" "")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci<? "" "1")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<? "1" "1")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci<? "1" "2")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<? "2" "1")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci<? "a" "ab")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<? "ab" "a")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci<? "a" "a1")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<? "a1" "a")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci<? "1" "1a")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<? "1a" "1")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci<? "a" "A")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci<? "A" "a")			=> #f)

  (check (bytevector-u8/numbers-dictionary-ci<? "123" "45")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci<? "45" "123")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<? "ciao3" "ciao10")		=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<? "ciao10" "ciao3")		=> #f)
  (check (bytevector-u8/numbers-dictionary-ci<? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (bytevector-u8/numbers-dictionary-ci<? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<? "foo4bar3zab" "foo4bar10")	=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<? "foo4bar10" "foo4bar3zab")	=> #f)
  (check (bytevector-u8/numbers-dictionary-ci<? "foo12" "12foo")		=> #f)
  (check (bytevector-u8/numbers-dictionary-ci<? "12foo" "foo12")		=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<? "12bar" "foobar")		=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<? "12.3" "12.3")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci<? "12.3" "12.10")		=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<? "12.10" "12.3")		=> #f)
  (check (bytevector-u8/numbers-dictionary-ci<? "12.3" "12,10")		=> #f)
  (check (bytevector-u8/numbers-dictionary-ci<? "12,10" "12.3")		=> #t)

;;; --------------------------------------------------------------------

  (check (bytevector-u8/numbers-dictionary-ci<=? "" "")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<=? "a" "")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci<=? "" "a")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<=? "a" "a")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<=? "1" "")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci<=? "" "1")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<=? "1" "1")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<=? "1" "2")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<=? "2" "1")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci<=? "a" "ab")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<=? "ab" "a")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci<=? "a" "a1")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<=? "a1" "a")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci<=? "1" "1a")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<=? "1a" "1")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci<=? "a" "A")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<=? "A" "a")			=> #t)

  (check (bytevector-u8/numbers-dictionary-ci<=? "123" "45")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci<=? "45" "123")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<=? "ciao3" "ciao10")		=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<=? "ciao10" "ciao3")		=> #f)
  (check (bytevector-u8/numbers-dictionary-ci<=? "foo4bar3zab10" "foo4bar3zab2")	=> #f)
  (check (bytevector-u8/numbers-dictionary-ci<=? "foo4bar3zab2" "foo4bar3zab10")	=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<=? "foo4bar3zab" "foo4bar10")	=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<=? "foo4bar10" "foo4bar3zab")	=> #f)
  (check (bytevector-u8/numbers-dictionary-ci<=? "foo12" "12foo")		=> #f)
  (check (bytevector-u8/numbers-dictionary-ci<=? "12foo" "foo12")		=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<=? "12bar" "foobar")		=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<=? "12.3" "12.3")		=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<=? "12.3" "12.10")		=> #t)
  (check (bytevector-u8/numbers-dictionary-ci<=? "12.10" "12.3")		=> #f)
  (check (bytevector-u8/numbers-dictionary-ci<=? "12.3" "12,10")		=> #f)
  (check (bytevector-u8/numbers-dictionary-ci<=? "12,10" "12.3")		=> #t)

;;; --------------------------------------------------------------------

  (check (bytevector-u8/numbers-dictionary-ci>? "" "")				=> #f)
  (check (bytevector-u8/numbers-dictionary-ci>? "a" "")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci>? "" "a")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci>? "a" "a")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci>? "1" "")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci>? "" "1")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci>? "1" "1")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci>? "1" "2")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci>? "2" "1")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci>? "a" "ab")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci>? "ab" "a")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci>? "a" "a1")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci>? "a1" "a")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci>? "1" "1a")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci>? "1a" "1")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci>? "a" "A")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci>? "A" "a")			=> #f)

  (check (bytevector-u8/numbers-dictionary-ci>? "123" "45")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci>? "45" "123")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci>? "ciao3" "ciao10")		=> #f)
  (check (bytevector-u8/numbers-dictionary-ci>? "ciao10" "ciao3")		=> #t)
  (check (bytevector-u8/numbers-dictionary-ci>? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (bytevector-u8/numbers-dictionary-ci>? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (bytevector-u8/numbers-dictionary-ci>? "foo4bar3zab" "foo4bar10")	=> #f)
  (check (bytevector-u8/numbers-dictionary-ci>? "foo4bar10" "foo4bar3zab")	=> #t)
  (check (bytevector-u8/numbers-dictionary-ci>? "foo12" "12foo")		=> #t)
  (check (bytevector-u8/numbers-dictionary-ci>? "12foo" "foo12")		=> #f)
  (check (bytevector-u8/numbers-dictionary-ci>? "12bar" "foobar")		=> #f)
  (check (bytevector-u8/numbers-dictionary-ci>? "12.3" "12.3")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci>? "12.3" "12.10")		=> #f)
  (check (bytevector-u8/numbers-dictionary-ci>? "12.10" "12.3")		=> #t)
  (check (bytevector-u8/numbers-dictionary-ci>? "12.3" "12,10")		=> #t)
  (check (bytevector-u8/numbers-dictionary-ci>? "12,10" "12.3")		=> #f)

;;; --------------------------------------------------------------------

  (check (bytevector-u8/numbers-dictionary-ci>=? "" "")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci>=? "a" "")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci>=? "" "a")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci>=? "a" "a")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci>=? "1" "")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci>=? "" "1")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci>=? "1" "1")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci>=? "1" "2")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci>=? "2" "1")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci>=? "a" "ab")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci>=? "ab" "a")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci>=? "a" "a1")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci>=? "a1" "a")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci>=? "1" "1a")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci>=? "1a" "1")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci>=? "a" "A")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci>=? "A" "a")			=> #t)

  (check (bytevector-u8/numbers-dictionary-ci>=? "123" "45")			=> #t)
  (check (bytevector-u8/numbers-dictionary-ci>=? "45" "123")			=> #f)
  (check (bytevector-u8/numbers-dictionary-ci>=? "ciao3" "ciao10")		=> #f)
  (check (bytevector-u8/numbers-dictionary-ci>=? "ciao10" "ciao3")		=> #t)
  (check (bytevector-u8/numbers-dictionary-ci>=? "foo4bar3zab10" "foo4bar3zab2")	=> #t)
  (check (bytevector-u8/numbers-dictionary-ci>=? "foo4bar3zab2" "foo4bar3zab10")	=> #f)
  (check (bytevector-u8/numbers-dictionary-ci>=? "foo4bar3zab" "foo4bar10")	=> #f)
  (check (bytevector-u8/numbers-dictionary-ci>=? "foo4bar10" "foo4bar3zab")	=> #t)
  (check (bytevector-u8/numbers-dictionary-ci>=? "foo12" "12foo")		=> #t)
  (check (bytevector-u8/numbers-dictionary-ci>=? "12foo" "foo12")		=> #f)
  (check (bytevector-u8/numbers-dictionary-ci>=? "12bar" "foobar")		=> #f)
  (check (bytevector-u8/numbers-dictionary-ci>=? "12.3" "12.3")		=> #t)
  (check (bytevector-u8/numbers-dictionary-ci>=? "12.3" "12.10")		=> #f)
  (check (bytevector-u8/numbers-dictionary-ci>=? "12.10" "12.3")		=> #t)
  (check (bytevector-u8/numbers-dictionary-ci>=? "12.3" "12,10")		=> #t)
  (check (bytevector-u8/numbers-dictionary-ci>=? "12,10" "12.3")		=> #f)

  #t)


(parameterise ((check-test-name 'mapping))

  (check
      (let ((str (bytevector-u8-copy "abcd")))
	(bytevector-u8-map! (lambda (i ch) (char-upcase ch))
		     str)
	str)
    => "ABCD")

  (check
      (let ((str (bytevector-u8-copy "abcd")))
	(bytevector-u8-map! (lambda (i ch-a ch-b) (if (even? i) ch-a ch-b))
		     str "0123")
	str)
    => "a1c3")

  (check
      (let ((str (bytevector-u8-copy "")))
	(bytevector-u8-map! (lambda (i ch) (char-upcase ch))
		     str)
	str)
    => "")

;;; --------------------------------------------------------------------

  (check
      (let ((str (bytevector-u8-copy "abcd")))
	(bytevector-u8-map*! (lambda (i ch) (char-upcase ch))
		      str)
	str)
    => "ABCD")

  (check
      (let ((str (bytevector-u8-copy "abcd")))
	(bytevector-u8-map*! (lambda (i ch-a ch-b) (if (even? i) ch-a ch-b))
		      str "01234")
	str)
    => "a1c3")

  (check
      (let ((str (bytevector-u8-copy "")))
	(bytevector-u8-map*! (lambda (i ch) (char-upcase ch))
		      str)
	str)
    => "")

;;; --------------------------------------------------------------------

  (check
      (cadr (with-result
	     (bytevector-u8-for-each* (lambda (i ch) (add-result (list i ch)))
			       "abcd")))
    => '((0 #\a)
	 (1 #\b)
	 (2 #\c)
	 (3 #\d)))

  (check
      (cadr (with-result
	     (bytevector-u8-for-each* (lambda (i ch-a ch-b) (add-result (list i ch-a ch-b)))
			       "abcd" "01234")))
    => '((0 #\a #\0)
	 (1 #\b #\1)
	 (2 #\c #\2)
	 (3 #\d #\3)))

  (check
      (cadr (with-result
	     (bytevector-u8-for-each* (lambda (i ch) (add-result (list i ch)))
			       "")))
    => '())

;;; --------------------------------------------------------------------

  (check
      (subbytevector-u8-map (lambda (ch) (char-upcase ch))
		     "abcd")
    => "ABCD")


  (check
      (subbytevector-u8-map (lambda (ch) (char-upcase ch))
		     (view "abcd" (start 1) (past 3)))
    => "BC")

  (check
      (subbytevector-u8-map (lambda (ch) (char-upcase ch))
		     "")
    => "")

;;; --------------------------------------------------------------------

  (check
      (let ((str (bytevector-u8-copy "abcd")))
	(subbytevector-u8-map! (lambda (ch) (char-upcase ch))
			str)
	str)
    => "ABCD")

  (check
      (let ((str (bytevector-u8-copy "abcd")))
	(subbytevector-u8-map! (lambda (ch) (char-upcase ch))
			(view str (start 1) (past 3)))
	str)
    => "aBCd")

  (check
      (let ((str ""))
	(subbytevector-u8-map! (lambda (ch) (char-upcase ch))
			str)
	str)
    => "")

;;; --------------------------------------------------------------------

  (check
      (cadr (with-result
	     (subbytevector-u8-for-each add-result
				 "abcd")))
    => '(#\a #\b #\c #\d))

  (check
      (cadr (with-result
	     (subbytevector-u8-for-each add-result
				 (view "abcd" (start 1) (past 3)))))
    => '(#\b #\c))

  (check
      (cadr (with-result
	     (subbytevector-u8-for-each add-result "")))
    => '())


  )


(parameterise ((check-test-name 'case))

  (check
      (bytevector-u8-upcase* "abcd")
    => "ABCD")

  (check
      (bytevector-u8-upcase* "123abcd")
    => "123ABCD")

  (check
      (bytevector-u8-upcase* "---abcd")
    => "---ABCD")

  (check
      (bytevector-u8-upcase* "abcd efgh")
    => "ABCD EFGH")

;;; --------------------------------------------------------------------

  (check
      (let* ((str (bytevector-u8-copy "abcd")))
	(bytevector-u8-upcase*! str)
	str)
    => "ABCD")

  (check
      (let* ((str (bytevector-u8-copy "123abcd")))
	(bytevector-u8-upcase*! str)
	str)
    => "123ABCD")

  (check
      (let* ((str (bytevector-u8-copy "---abcd")))
	(bytevector-u8-upcase*! str)
	str)
    => "---ABCD")

  (check
      (let* ((str (bytevector-u8-copy "abcd efgh")))
	(bytevector-u8-upcase*! str)
	str)
    => "ABCD EFGH")

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-downcase* "ABCD")
    => "abcd")

  (check
      (bytevector-u8-downcase* "123AbcD")
    => "123abcd")

  (check
      (bytevector-u8-downcase* "---aBCd")
    => "---abcd")

  (check
      (bytevector-u8-downcase* "abcd EFGH")
    => "abcd efgh")

;;; --------------------------------------------------------------------

  (check
      (let* ((str (bytevector-u8-copy "aBCd")))
	(bytevector-u8-downcase*! str)
	str)
    => "abcd")

  (check
      (let* ((str (bytevector-u8-copy "123ABcd")))
	(bytevector-u8-downcase*! str)
	str)
    => "123abcd")

  (check
      (let* ((str (bytevector-u8-copy "---aBCD")))
	(bytevector-u8-downcase*! str)
	str)
    => "---abcd")

  (check
      (let* ((str (bytevector-u8-copy "abCD Efgh")))
	(bytevector-u8-downcase*! str)
	str)
    => "abcd efgh")

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-titlecase* "abcd")
    => "Abcd")

  (check
      (bytevector-u8-titlecase* "123abcd")
    => "123Abcd")

  (check
      (bytevector-u8-titlecase* "---abcd")
    => "---Abcd")

  (check
      (bytevector-u8-titlecase* "abcd efgh")
    => "Abcd Efgh")

  (check
      (bytevector-u8-titlecase* (view "greasy fried chicken" (start 2)))
    => "Easy Fried Chicken")

;;; --------------------------------------------------------------------

  (check
      (let* ((str (bytevector-u8-copy "abcd")))
	(bytevector-u8-titlecase*! str)
	str)
    => "Abcd")

  (check
      (let* ((str (bytevector-u8-copy "123abcd")))
	(bytevector-u8-titlecase*! str)
	str)
    => "123Abcd")

  (check
      (let* ((str (bytevector-u8-copy "---abcd")))
	(bytevector-u8-titlecase*! str)
	str)
    => "---Abcd")

  (check
      (let* ((str (bytevector-u8-copy "abcd efgh")))
	(bytevector-u8-titlecase*! str)
	str)
    => "Abcd Efgh")

  (check
      (let ((str (bytevector-u8-copy "greasy fried chicken")))
	(bytevector-u8-titlecase*! (view str (start 2)))
	str)
    => "grEasy Fried Chicken")

  )


(parameterise ((check-test-name 'folding))

  (check
      (bytevector-u8-fold-left (lambda (i nil x) (cons x nil)) '() "abcd")
    => '(#\d #\c #\b #\a))

  (check
      (bytevector-u8-fold-left (lambda (i nil x y) (cons (cons x y) nil)) '()
			"abcd"
			"ABCD")
    => '((#\d . #\D)
	 (#\c . #\C)
	 (#\b . #\B)
	 (#\a . #\A)))

  (check
      (bytevector-u8-fold-left (lambda (i nil x) (cons x nil)) '() "")
    => '())

  (check
      (bytevector-u8-fold-left (lambda (i count c)
			  (if (char-upper-case? c)
			      (+ count 1)
			    count))
			0
			"ABCdefGHi")
    => 5)

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-fold-right (lambda (i nil x) (cons x nil)) '() "abcd")
    => '(#\a #\b #\c #\d))

  (check
      (bytevector-u8-fold-right (lambda (i nil x y) (cons (cons x y) nil)) '()
			 "abcd"
			 "ABCD")
    => '((#\a . #\A)
	 (#\b . #\B)
	 (#\c . #\C)
	 (#\d . #\D)))

  (check
      (bytevector-u8-fold-right (lambda (i nil x) (cons x nil)) '() "")
    => '())

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-fold-left* (lambda (i nil x) (cons x nil)) '() "abcd")
    => '(#\d #\c #\b #\a))

  (check
      (bytevector-u8-fold-left* (lambda (i nil x y) (cons (cons x y) nil)) '()
			 "abcd"
			 "ABCDE")
    => '((#\d . #\D)
	 (#\c . #\C)
	 (#\b . #\B)
	 (#\a . #\A)))

  (check
      (bytevector-u8-fold-left* (lambda (i nil x) (cons x nil)) '() "")
    => '())

  (check
      (bytevector-u8-fold-left* (lambda (i count c)
			   (if (char-upper-case? c)
			       (+ count 1)
			     count))
			 0
			 "ABCdefGHi")
    => 5)

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-fold-right* (lambda (i nil x) (cons x nil)) '() "abcd")
    => '(#\a #\b #\c #\d))

  (check
      (bytevector-u8-fold-right* (lambda (i nil x y) (cons (cons x y) nil)) '()
			  "abcd"
			  "ABCDE")
    => '((#\a . #\A)
	 (#\b . #\B)
	 (#\c . #\C)
	 (#\d . #\D)))

  (check
      (bytevector-u8-fold-right* (lambda (i nil x) (cons x nil)) '() "")
    => '())

;;; --------------------------------------------------------------------

  (check
      (subbytevector-u8-fold-left cons '() "abcd")
    => '(#\d #\c #\b #\a))

  (check
      (subbytevector-u8-fold-left cons '() "")
    => '())

  (check
      (subbytevector-u8-fold-left (lambda (c count)
			     (if (char-upper-case? c)
				 (+ count 1)
			       count))
			   0
			   "ABCdefGHi")
    => 5)

  (check
      (let* ((str "abc\\de\\f\\ghi")
	     (ans-len (subbytevector-u8-fold-left
		       (lambda (c sum)
			 (+ sum (if (char=? c #\\) 2 1)))
		       0 str))
	     (ans (make-bytevector-u8 ans-len)))
	(subbytevector-u8-fold-left
	 (lambda (c i)
	   (let ((i (if (char=? c #\\)
			(begin
			  (bytevector-u8-set! ans i #\\)
			  (+ i 1))
		      i)))
	     (bytevector-u8-set! ans i c)
	     (+ i 1)))
	 0 str)
	ans)
    => "abc\\\\de\\\\f\\\\ghi")

;;; --------------------------------------------------------------------

  (check
      (subbytevector-u8-fold-right cons '() "abcd")
    => '(#\a #\b #\c #\d))

  (check
      (subbytevector-u8-fold-right cons '() "")
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

  )


(parameterise ((check-test-name 'selecting))

  (check
      (bytevector-u8-take "abcd" 2)
    => "ab")

  (check
      (bytevector-u8-take "" 0)
    => "")

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(bytevector-u8-take "abcd" 5))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-take-right "abcd" 2)
    => "cd")

  (check
      (bytevector-u8-take-right "" 0)
    => "")

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(bytevector-u8-take-right "abcd" 5))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-drop "abcd" 2)
    => "cd")

  (check
      (bytevector-u8-drop "" 0)
    => "")

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(bytevector-u8-drop "abcd" 5))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-drop-right "abcd" 2)
    => "ab")

  (check
      (bytevector-u8-drop-right "" 0)
    => "")

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(bytevector-u8-drop-right "abcd" 5))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-trim "aaabcd" #\a)
    => "bcd")

  (check
      (bytevector-u8-trim "bcd" #\a)
    => "bcd")

  (check
      (bytevector-u8-trim "" #\a)
    => "")

  (check
      (bytevector-u8-trim "aaabcd" (char-set #\a #\b))
    => "cd")

  (check
      (bytevector-u8-trim "bcd" (char-set #\a #\b))
    => "cd")

  (check
      (bytevector-u8-trim "" (char-set #\a #\b))
    => "")

  (check
      (bytevector-u8-trim "AAAbcd" char-upper-case?)
    => "bcd")

  (check
      (bytevector-u8-trim "bcd" char-upper-case?)
    => "bcd")

  (check
      (bytevector-u8-trim "" char-upper-case?)
    => "")

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-trim-right "bcdaaa" #\a)
    => "bcd")

  (check
      (bytevector-u8-trim-right "bcd" #\a)
    => "bcd")

  (check
      (bytevector-u8-trim-right "" #\a)
    => "")

  (check
      (bytevector-u8-trim-right "cdbaaa" (char-set #\a #\b))
    => "cd")

  (check
      (bytevector-u8-trim-right "cdb" (char-set #\a #\b))
    => "cd")

  (check
      (bytevector-u8-trim-right "" (char-set #\a #\b))
    => "")

  (check
      (bytevector-u8-trim-right "bcdAAA" char-upper-case?)
    => "bcd")

  (check
      (bytevector-u8-trim-right "bcd" char-upper-case?)
    => "bcd")

  (check
      (bytevector-u8-trim-right "" char-upper-case?)
    => "")

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-trim-both "aaabcdaaa" #\a)
    => "bcd")

  (check
      (bytevector-u8-trim-both "bcd" #\a)
    => "bcd")

  (check
      (bytevector-u8-trim-both "" #\a)
    => "")

  (check
      (bytevector-u8-trim-both "aaabcdaa" (char-set #\a #\b))
    => "cd")

  (check
      (bytevector-u8-trim-both "bcdb" (char-set #\a #\b))
    => "cd")

  (check
      (bytevector-u8-trim-both "" (char-set #\a #\b))
    => "")

  (check
      (bytevector-u8-trim-both "AAAbcdAAA" char-upper-case?)
    => "bcd")

  (check
      (bytevector-u8-trim-both "bcd" char-upper-case?)
    => "bcd")

  (check
      (bytevector-u8-trim-both "" char-upper-case?)
    => "")

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-pad "abc" 3 #\0)
    => "abc")

  (check
      (bytevector-u8-pad "abc" 5 #\0)
    => "00abc")

  (check
      (bytevector-u8-pad "abc" 5)
    => "  abc")

  (check
      (bytevector-u8-pad "abc" 2 #\0)
    => "bc")

  (check
      (bytevector-u8-pad "abc" 0 #\0)
    => "")

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-pad-right "abc" 3 #\0)
    => "abc")

  (check
      (bytevector-u8-pad-right "abc" 5 #\0)
    => "abc00")

  (check
      (bytevector-u8-pad-right "abc" 2 #\0)
    => "ab")

  (check
      (bytevector-u8-pad-right "abc" 0 #\0)
    => "")

  )


(parameterise ((check-test-name 'prefix))

  (check
      (bytevector-u8-prefix-length "abcdefg" "abcd123")
    => 4)

  (check
      (bytevector-u8-prefix-length "aBcdefg" "abcd123")
    => 1)

  (check
      (bytevector-u8-prefix-length "efg" "123")
    => 0)

  (check
      (bytevector-u8-prefix-length "a" "a")
    => 1)

  (check
      (bytevector-u8-prefix-length "1" "2")
    => 0)

  (check
      (bytevector-u8-prefix-length "" "abcd123")
    => 0)

  (check
      (bytevector-u8-prefix-length "abcdefg" "")
    => 0)

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-suffix-length "efgabcd" "123abcd")
    => 4)

  (check
      (bytevector-u8-suffix-length "efgabcd" "123abCd")
    => 1)

  (check
      (bytevector-u8-suffix-length "efg" "123")
    => 0)

  (check
      (bytevector-u8-suffix-length "a" "a")
    => 1)

  (check
      (bytevector-u8-suffix-length "1" "2")
    => 0)

  (check
      (bytevector-u8-suffix-length "" "abcd123")
    => 0)

  (check
      (bytevector-u8-suffix-length "abcdefg" "")
    => 0)

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-prefix-length-ci "aBcdefg" "aBcd123")
    => 4)

  (check
      (bytevector-u8-prefix-length-ci "aBcdefg" "abcd123")
    => 4)

  (check
      (bytevector-u8-prefix-length-ci "efg" "123")
    => 0)

  (check
      (bytevector-u8-prefix-length-ci "a" "a")
    => 1)

  (check
      (bytevector-u8-prefix-length-ci "1" "2")
    => 0)

  (check
      (bytevector-u8-prefix-length-ci "" "abcd123")
    => 0)

  (check
      (bytevector-u8-prefix-length-ci "abcdefg" "")
    => 0)

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-suffix-length-ci "efgabCd" "123abCd")
    => 4)

  (check
      (bytevector-u8-suffix-length-ci "efgabCd" "123abcd")
    => 4)

  (check
      (bytevector-u8-suffix-length-ci "efg" "123")
    => 0)

  (check
      (bytevector-u8-suffix-length-ci "a" "a")
    => 1)

  (check
      (bytevector-u8-suffix-length-ci "1" "2")
    => 0)

  (check
      (bytevector-u8-suffix-length-ci "" "abcd123")
    => 0)

  (check
      (bytevector-u8-suffix-length-ci "abcdefg" "")
    => 0)

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-prefix? "abcd" "abcd123")
    => #t)

  (check
      (bytevector-u8-prefix? "abcd" "aBcd123")
    => #f)

  (check
      (bytevector-u8-prefix? "efg" "123")
    => #f)

  (check
      (bytevector-u8-prefix? "" "123")
    => #t)

  (check
      (bytevector-u8-prefix? "efg" "")
    => #f)

  (check
      (bytevector-u8-prefix? "" "")
    => #t)

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-prefix-ci? "aBcd" "aBcd123")
    => #t)

  (check
      (bytevector-u8-prefix-ci? "abcd" "aBcd123")
    => #t)

  (check
      (bytevector-u8-prefix-ci? "efg" "123")
    => #f)

  (check
      (bytevector-u8-prefix-ci? "" "123")
    => #t)

  (check
      (bytevector-u8-prefix-ci? "efg" "")
    => #f)

  (check
      (bytevector-u8-prefix-ci? "" "")
    => #t)

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-suffix? "abcd" "123abcd")
    => #t)

  (check
      (bytevector-u8-suffix? "abcd" "123aBcd")
    => #f)

  (check
      (bytevector-u8-suffix? "efg" "123")
    => #f)

  (check
      (bytevector-u8-suffix? "" "123")
    => #t)

  (check
      (bytevector-u8-suffix? "efg" "")
    => #f)

  (check
      (bytevector-u8-suffix? "" "")
    => #t)

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-suffix-ci? "aBcd" "123aBcd")
    => #t)

  (check
      (bytevector-u8-suffix-ci? "abcd" "123aBcd")
    => #t)

  (check
      (bytevector-u8-suffix-ci? "efg" "123")
    => #f)

  (check
      (bytevector-u8-suffix-ci? "" "123")
    => #t)

  (check
      (bytevector-u8-suffix-ci? "efg" "")
    => #f)

  (check
      (bytevector-u8-suffix-ci? "" "")
    => #t)

  )


(parameterise ((check-test-name 'searching))

  (check
      (bytevector-u8-index "abcd" #\b)
    => 1)

  (check
      (bytevector-u8-index (view "abcd" (start 1)) #\b)
    => 1)

  (check
      (bytevector-u8-index "abcd" #\1)
    => #f)

  (check
      (bytevector-u8-index "" #\1)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-index "abcd" (char-set #\b #\B))
    => 1)

  (check
      (bytevector-u8-index (view "abcd" (start 1)) (char-set #\b #\B))
    => 1)

  (check
      (bytevector-u8-index "abcd" (char-set #\0 #\1))
    => #f)

  (check
      (bytevector-u8-index "" (char-set #\0 #\1))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-index "aBcd" char-upper-case?)
    => 1)

  (check
      (bytevector-u8-index (view "aBcd" (start 1)) char-upper-case?)
    => 1)

  (check
      (bytevector-u8-index "abcd" char-upper-case?)
    => #f)

  (check
      (bytevector-u8-index "" char-upper-case?)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-index-right "abcd" #\b)
    => 1)

  (check
      (bytevector-u8-index-right (view "abcd" (start 1)) #\b)
    => 1)

  (check
      (bytevector-u8-index-right "abcd" #\1)
    => #f)

  (check
      (bytevector-u8-index-right "" #\1)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-index-right "abcd" (char-set #\b #\B))
    => 1)

  (check
      (bytevector-u8-index-right (view "abcd" (start 1)) (char-set #\b #\B))
    => 1)

  (check
      (bytevector-u8-index-right "abcd" (char-set #\0 #\1))
    => #f)

  (check
      (bytevector-u8-index-right "" (char-set #\0 #\1))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-index-right "aBcd" char-upper-case?)
    => 1)

  (check
      (bytevector-u8-index-right (view "aBcd" (start 1)) char-upper-case?)
    => 1)

  (check
      (bytevector-u8-index-right "abcd" char-upper-case?)
    => #f)

  (check
      (bytevector-u8-index-right "" char-upper-case?)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-skip "bacd" #\b)
    => 1)

  (check
      (bytevector-u8-skip (view "bacd" (start 1)) #\b)
    => 1)

  (check
      (bytevector-u8-skip "1111" #\1)
    => #f)

  (check
      (bytevector-u8-skip "" #\1)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-skip "bacd" (char-set #\b #\B))
    => 1)

  (check
      (bytevector-u8-skip (view "bacd" (start 1)) (char-set #\b #\B))
    => 1)

  (check
      (bytevector-u8-skip "1010" (char-set #\0 #\1))
    => #f)

  (check
      (bytevector-u8-skip "" (char-set #\0 #\1))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-skip "Bacd" char-upper-case?)
    => 1)

  (check
      (bytevector-u8-skip (view "Bacd" (start 1)) char-upper-case?)
    => 1)

  (check
      (bytevector-u8-skip "ABCD" char-upper-case?)
    => #f)

  (check
      (bytevector-u8-skip "" char-upper-case?)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-skip-right "acdb" #\b)
    => 2)

  (check
      (bytevector-u8-skip-right (view "acdb" (start 1)) #\b)
    => 2)

  (check
      (bytevector-u8-skip-right "1111" #\1)
    => #f)

  (check
      (bytevector-u8-skip-right "" #\1)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-skip-right "acdb" (char-set #\b #\B))
    => 2)

  (check
      (bytevector-u8-skip-right (view "acdb" (start 1)) (char-set #\b #\B))
    => 2)

  (check
      (bytevector-u8-skip-right "0101" (char-set #\0 #\1))
    => #f)

  (check
      (bytevector-u8-skip-right "" (char-set #\0 #\1))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-skip-right "acdB" char-upper-case?)
    => 2)

  (check
      (bytevector-u8-skip-right (view "acdB" (start 1)) char-upper-case?)
    => 2)

  (check
      (bytevector-u8-skip-right "ABCD" char-upper-case?)
    => #f)

  (check
      (bytevector-u8-skip-right "" char-upper-case?)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-count "abcbd" #\b)
    => 2)

  (check
      (bytevector-u8-count (view "abcd" (start 1)) #\b)
    => 1)

  (check
      (bytevector-u8-count "abcd" #\1)
    => 0)

  (check
      (bytevector-u8-count "" #\1)
    => 0)

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-count "abcBd" (char-set #\b #\B))
    => 2)

  (check
      (bytevector-u8-count (view "abcd" (start 1)) (char-set #\b #\B))
    => 1)

  (check
      (bytevector-u8-count "abcd" (char-set #\0 #\1))
    => 0)

  (check
      (bytevector-u8-count "" (char-set #\0 #\1))
    => 0)

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-count "aBcAd" char-upper-case?)
    => 2)

  (check
      (bytevector-u8-count (view "aBcd" (start 1)) char-upper-case?)
    => 1)

  (check
      (bytevector-u8-count "abcd" char-upper-case?)
    => 0)

  (check
      (bytevector-u8-count "" char-upper-case?)
    => 0)

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-contains "ciao hello salut" "hello")
    => 5)

  (check
      (bytevector-u8-contains "ciao hello salut" "hola")
    => #f)

  (check
      (bytevector-u8-contains "ciao hello salut" "")
    => 0)

  (check
      (bytevector-u8-contains "" "hello")
    => #f)

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-contains-ci "ciAO HELLO saLUT" "hello")
    => 5)

  (check
      (bytevector-u8-contains-ci "ciao hello salut" "HOLA")
    => #f)

  (check
      (bytevector-u8-contains-ci "ciao hello salut" "")
    => 0)

  (check
      (bytevector-u8-contains-ci "" "hello")
    => #f)

  )


(parameterise ((check-test-name 'filtering))

  (check
      (bytevector-u8-delete "abcbd" #\b)
    => "acd")

  (check
      (bytevector-u8-delete "abcbd" #\0)
    => "abcbd")

  (check
      (bytevector-u8-delete "" #\b)
    => "")

  (check
      (bytevector-u8-delete "bbb" #\b)
    => "")

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-delete "abcbd" (char-set #\b #\B))
    => "acd")

  (check
      (bytevector-u8-delete "abcbd" (char-set #\0 #\1))
    => "abcbd")

  (check
      (bytevector-u8-delete "" (char-set #\b #\B))
    => "")

  (check
      (bytevector-u8-delete "BbB" (char-set #\b #\B))
    => "")

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-delete "aBcBd" char-upper-case?)
    => "acd")

  (check
      (bytevector-u8-delete "abcbd" char-upper-case?)
    => "abcbd")

  (check
      (bytevector-u8-delete "" char-upper-case?)
    => "")

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-filter "abcbd" #\b)
    => "bb")

  (check
      (bytevector-u8-filter "abcbd" #\0)
    => "")

  (check
      (bytevector-u8-filter "" #\b)
    => "")

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-filter "abcbd" (char-set #\b #\B))
    => "bb")

  (check
      (bytevector-u8-filter "abcbd" (char-set #\0 #\1))
    => "")

  (check
      (bytevector-u8-filter "" (char-set #\b #\B))
    => "")

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-filter "aBcBd" char-upper-case?)
    => "BB")

  (check
      (bytevector-u8-filter "abcbd" char-upper-case?)
    => "")

  (check
      (bytevector-u8-filter "" char-upper-case?)
    => "")

  )


(parameterise ((check-test-name 'lists))

  (check
      (bytevector-u8->list* "abcd")
    => '(#\a #\b #\c #\d))

  (check
      (bytevector-u8->list* (view "abcd" (start 1) (past 3)))
    => '(#\b #\c))

  (check
      (bytevector-u8->list* "")
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
      (bytevector-u8-tokenize "ciao hello salut"
		       (char-set #\a #\c #\e #\i #\h #\l #\o #\s #\t #\u))
    => '((string->utf8 "ciao") "hello" "salut"))

  (check
      (bytevector-u8-tokenize "" (char-set #\a #\c #\e #\i #\h #\l #\o #\s #\t #\u))
    => '())

  (check
      (bytevector-u8-tokenize "ciao hello salut" (char-set))
    => '())

  (check
      (bytevector-u8-tokenize "Help make programs run, run, RUN!"
		       (char-set-complement (char-set #\space)
					    char-set:ascii))
    => '("Help" "make" "programs" "run," "run," "RUN!"))

  #f)

;;; --------------------------------------------------------------------

(parameterise ((check-test-name 'join))

  (check
      (bytevector-u8-join '("c" "i" "a" "o") "," 'infix)
    => "c,i,a,o")

  (check
      (bytevector-u8-join '("c" "i" "a" "o") "," 'strict-infix)
    => "c,i,a,o")

  (check
      (bytevector-u8-join '("c" "i" "a" "o") "," 'suffix)
    => "c,i,a,o,")

  (check
      (bytevector-u8-join '("c" "i" "a" "o") "," 'prefix)
    => ",c,i,a,o")

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-join '() "," 'infix)
    => "")

  (check
      (guard (exc ((assertion-violation? exc)
		   #t))
	(bytevector-u8-join '() "," 'strict-infix))
    => #t)

  (check
      (bytevector-u8-join '() "," 'suffix)
    => "")

  (check
      (bytevector-u8-join '() "," 'prefix)
    => "")

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-join '("c") "," 'infix)
    => "c")

  (check
      (bytevector-u8-join '("c") "," 'strict-infix)
    => "c")

  (check
      (bytevector-u8-join '("c") "," 'suffix)
    => "c,")

  (check
      (bytevector-u8-join '("c") "," 'prefix)
    => ",c")

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-join '("") "," 'infix)
    => "")

  (check
      (bytevector-u8-join '("") "," 'strict-infix)
    => "")

  (check
      (bytevector-u8-join '("") "," 'suffix)
    => ",")

  (check
      (bytevector-u8-join '("") "," 'prefix)
    => ",")

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-join '("c" "i" "a" "o") "" 'infix)
    => (string->utf8 "ciao"))

  (check
      (bytevector-u8-join '("c" "i" "a" "o") "" 'strict-infix)
    => (string->utf8 "ciao"))

  (check
      (bytevector-u8-join '("c" "i" "a" "o") "" 'suffix)
    => (string->utf8 "ciao"))

  (check
      (bytevector-u8-join '("c" "i" "a" "o") "" 'prefix)
    => (string->utf8 "ciao"))

;;; --------------------------------------------------------------------

  (check
      (bytevector-u8-join '("c" "i" "a" "o") ",;;" 'infix)
    => "c,;;i,;;a,;;o")

  (check
      (bytevector-u8-join '("c" "i" "a" "o") ",;;" 'strict-infix)
    => "c,;;i,;;a,;;o")

  (check
      (bytevector-u8-join '("c" "i" "a" "o") ",;;" 'suffix)
    => "c,;;i,;;a,;;o,;;")

  (check
      (bytevector-u8-join '("c" "i" "a" "o") ",;;" 'prefix)
    => ",;;c,;;i,;;a,;;o")

  )


(parameterise ((check-test-name 'xsubbytevector-u8))

  (check
      (xsubbytevector-u8 "ciao " 0 5)
    => "ciao ")

  (check
      (xsubbytevector-u8 "ciao " 0 9)
    => "ciao ciao")

  (check
      (xsubbytevector-u8 "ciao " -5 5)
    => "ciao ciao ")

  (check
      (xsubbytevector-u8 "ciao " 2 4)
    => "ao")

  (check
      (xsubbytevector-u8 "ciao " -3 7)
    => "ao ciao ci")

  (check (xsubbytevector-u8 "abcdef" 1 7) => "bcdefa")
  (check (xsubbytevector-u8 "abcdef" 2 8) => "cdefab")
  (check (xsubbytevector-u8 "abcdef" 3 9) => "defabc")
  (check (xsubbytevector-u8 "abcdef" 4 10) => "efabcd")
  (check (xsubbytevector-u8 "abcdef" 5 11) => "fabcde")

  (check (xsubbytevector-u8 "abcdef" -1 5) => "fabcde")
  (check (xsubbytevector-u8 "abcdef" -2 4) => "efabcd")
  (check (xsubbytevector-u8 "abcdef" -3 3) => "defabc")
  (check (xsubbytevector-u8 "abcdef" -4 2) => "cdefab")
  (check (xsubbytevector-u8 "abcdef" -5 1) => "bcdefa")

  (check
      (xsubbytevector-u8 "ciao " 3 3)
    => "")

  (check
      (guard (exc ((assertion-violation? exc)
		   #t))
	(xsubbytevector-u8 "" 0 5))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let ((result (bytevector-u8-copy "01234")))
	(bytevector-u8-xcopy! result "ciao " 0 5)
	result)
    => "ciao ")

  (check
      (let ((result (bytevector-u8-copy "012345678")))
	(bytevector-u8-xcopy! result "ciao " 0 9)
	result)
    => "ciao ciao")

  (check
      (let ((result (bytevector-u8-copy "0123456789")))
	(bytevector-u8-xcopy! result "ciao " -5 5)
	result)
    => "ciao ciao ")

  (check
      (let ((result (bytevector-u8-copy "01")))
	(bytevector-u8-xcopy! result "ciao " 2 4)
	result)
    => "ao")

  (check
      (let ((result (bytevector-u8-copy "0123456789")))
	(bytevector-u8-xcopy! result "ciao " -3 7)
	result)
    => "ao ciao ci")

  (check
      (guard (exc ((assertion-violation? exc) #t))
	  (bytevector-u8-xcopy! "" "" 0 5))
    => #t)

  )


(parameterise ((check-test-name 'filling))

  (check
      (let* ((str (bytevector-u8-copy "abcd")))
	(bytevector-u8-fill*! str #\b)
	str)
    => "bbbb")

  (check
      (let* ((str (bytevector-u8-copy "accd")))
	(bytevector-u8-fill*! (view str (start 1) (past 3)) #\b)
	str)
    => "abbd")

  (check
      (let* ((str (bytevector-u8-copy "")))
	(bytevector-u8-fill*! (view str (start 0) (past 0)) #\b)
	str)
    => "")

  )


(parameterise ((check-test-name 'reverse))

  (check
      (bytevector-u8-reverse "abcd")
    => "dcba")

  (check
      (bytevector-u8-reverse "")
    => "")

;;; --------------------------------------------------------------------

  (check
      (let* ((str (bytevector-u8-copy "abcd")))
	(bytevector-u8-reverse! str)
	str)
    => "dcba")

  (check
      (let* ((str (bytevector-u8-copy "")))
	(bytevector-u8-reverse! str)
	str)
    => "")

  )


(parameterise ((check-test-name 'replace))

  (check
      (bytevector-u8-replace "abcd" "1234")
    => "1234")

  (check
      (bytevector-u8-replace (view "abcd" (start 2) (past 2)) "1234")
    => "ab1234cd")

  (check
      (bytevector-u8-replace (view "abcd" (start 2) (past 2)) "")
    => "abcd")

  (check
      (bytevector-u8-replace (view "abcd" (start 1) (past 3)) "1234")
    => "a1234d")

  (check
      (bytevector-u8-replace (view "abcd" (start 0) (past 3)) "1234")
    => "1234d")

  (check
      (bytevector-u8-replace (view "abcd" (start 1) (past 4)) "1234")
    => "a1234")

  )


(parameterise ((check-test-name 'mutating))

  (check
      (let* ((str (bytevector-u8-copy "12")))
	;; not enough room in destination bytevector-u8
	(guard (exc ((assertion-violation? exc) #t))
	  (bytevector-u8-copy*! (view str (start 3))
			 (view "abcd" (past 2)))))
    => #t)

  (check
      ;; whole bytevector-u8 copy
      (let* ((str (bytevector-u8-copy "123")))
	(bytevector-u8-copy*! str "abc")
	str)
    => "abc")

  (check
      ;; zero-elements bytevector-u8 copy
      (let* ((str (bytevector-u8-copy "123")))
	(bytevector-u8-copy*! str (view "abc" (start 2) (past 2)))
	str)
    => "123")

  (check
      ;; one-element bytevector-u8 copy
      (let* ((str (bytevector-u8-copy "123")))
	(bytevector-u8-copy*! str (view "abc" (start 1) (past 2)))
	str)
    => "b23")

  (check
      ;; two-elements bytevector-u8 copy
      (let* ((str (bytevector-u8-copy "12")))
	(bytevector-u8-copy*! str (view "abcd" (past 2)))
	str)
    => "ab")

  (check
      (let ((str ""))
	(bytevector-u8-copy*! str (view "abcd" (start 0) (past 0)))
	str)
    => "")

  (check
      ;; over the same bytevector-u8, full
      (let* ((str (bytevector-u8-copy "0123456789")))
	(bytevector-u8-copy*! str str)
	str)
    => "0123456789")

  (check
      ;; over the same bytevector-u8, in place
      (let* ((str (bytevector-u8-copy "0123456789")))
	(bytevector-u8-copy*! (view str (start 5)) (view str (start 5)))
	str)
    => "0123456789")

  (check
      ;; over the same bytevector-u8, backwards
      (let* ((str (bytevector-u8-copy "0123456789")))
	(bytevector-u8-copy*! (view str (start 2))
		       (view str (start 4) (past 8)))
	str)
    => "0145676789")

  (check
      ;; over the same bytevector-u8, backwards
      (let* ((str (bytevector-u8-copy "0123456789")))
	(bytevector-u8-copy*! (view str (start 0))
		       (view str (start 4) (past 8)))
	str)
    => "4567456789")

  (check
      ;; over the same bytevector-u8, forwards
      (let* ((str (bytevector-u8-copy "0123456789")))
	(bytevector-u8-copy*! (view str (start 4))
		       (view str (start 2) (past 6)))
	str)
    => "0123234589")

  (check
      ;; over the same bytevector-u8, forwards
      (let* ((str (bytevector-u8-copy "0123456789")))
	(bytevector-u8-copy*! (view str (start 6))
		       (view str (start 2) (past 6)))
	str)
    => "0123452345")

;;; --------------------------------------------------------------------

  (check
      (let* ((str (bytevector-u8-copy "12")))
	;; not enough room in destination bytevector-u8
	;;(bytevector-u8-reverse-copy*! (str 3) (view '#(#\a #\b #\c #\d) (past 2)))
	(guard (exc ((assertion-violation? exc) #t))
	  (bytevector-u8-reverse-copy*! (view str (start 3))
				 (view "abcd" (past 2)))))
    => #t)

  (check
      ;; whole bytevector-u8 copy
      (let* ((str (bytevector-u8-copy "123")))
	(bytevector-u8-reverse-copy*! str "abc")
	str)
    => "cba")

  (check
      ;; zero-elements bytevector-u8 copy
      (let* ((str (bytevector-u8-copy "123")))
	(bytevector-u8-reverse-copy*! str (view "abc" (start 2) (past 2)))
	str)
    => "123")

  (check
      ;; one-element bytevector-u8 copy
      (let* ((str (bytevector-u8-copy "123")))
	(bytevector-u8-reverse-copy*! str (view "abc" (start 1) (past 2)))
	str)
    => "b23")

  (check
      ;; two-elements bytevector-u8 copy
      (let* ((str (bytevector-u8-copy "12")))
	(bytevector-u8-reverse-copy*! str (view "abcd" (past 2)))
	str)
    => "ba")

  (check
      (let ((str ""))
	(bytevector-u8-reverse-copy*! str (view "abcd" (start 0) (past 0)))
	str)
    => "")

  (check
      ;; over the same bytevector-u8, full
      (let* ((str (bytevector-u8-copy "0123456789")))
	(bytevector-u8-reverse-copy*! str str)
	str)
    => "9876543210")

  (check
      ;; over the same bytevector-u8
      (let* ((str (bytevector-u8-copy "0123456789")))
	(bytevector-u8-reverse-copy*! (view str (start 5))
			       (view str (start 5)))
	str)
    => "0123498765")

  (check
      ;; over the same bytevector-u8, backwards
      (let* ((str (bytevector-u8-copy "0123456789")))
	(bytevector-u8-reverse-copy*! (view str (start 2))
			       (view str (start 4) (past 8)))
	str)
    => "0176546789")

  (check
      ;; over the same bytevector-u8, backwards
      (let* ((str (bytevector-u8-copy "0123456789")))
	(bytevector-u8-reverse-copy*! (view str (start 0))
			       (view str (start 4) (past 8)))
	str)
    => "7654456789")

  (check
      ;; over the same bytevector-u8, forwards
      (let* ((str (bytevector-u8-copy "0123456789")))
	(bytevector-u8-reverse-copy*! (view str (start 4))
			       (view str (start 2) (past 6)))
	str)
    => "0123543289")

  (check
      ;; over the same bytevector-u8, forwards
      (let* ((str (bytevector-u8-copy "0123456789")))
	(bytevector-u8-reverse-copy*! (view str (start 6))
			       (view str (start 2) (past 6)))
	str)
    => "0123455432")

;;; --------------------------------------------------------------------

  (check
      (let ((str (bytevector-u8-copy "012345")))
	(bytevector-u8-swap! str 2 4)
	str)
    => "014325")

  (check
      (let ((str (bytevector-u8-copy "012345")))
	(bytevector-u8-swap! str 2 2)
	str)
    => "012345")

  (check
      (guard (exc ((assertion-violation? exc) #t))
	(bytevector-u8-swap! "" 0 1))
    => #t)

  )


;;;; done

(check-report)

;;; end of file
