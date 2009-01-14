;;;
;;;Part of: Nausicaa/SRFI
;;;Contents: tests for format-lib
;;;Date: Sun Jan 11, 2009
;;;
;;;Abstract
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

(import (r6rs)
  (srfi cond-expand)
  (check-lib)
  (format-lib))

(check-set-mode! 'report-failed)



;;;; basic

(check
    (format "ciao")
  => "ciao")

(check
    (format "ciao ~s" 123)
  => "ciao 123")

;;; --------------------------------------------------------------------

(check
    (format #f "ciao")
  => "ciao")

(check
    (format #f "ciao")
  => "ciao")

(check
    (begin
      (format #t "ciao, this is text output to the current output port\n")
      #t)
  => #t)

(check
    (begin
      (format 1 "ciao, this is text output to the current error port\n")
      #t)
  => #t)

(check
    (call-with-string-output-port
	(lambda (port)
	  (format port "ciao")))
  => "ciao")


;;;; generic object output

(check
    (format #f "ciao ~a" 123)
  => "ciao 123")

(check
    (format #f "ciao ~s" 123)
  => "ciao 123")

;;; --------------------------------------------------------------------

(check
    (format #f "ciao ~:a" 123)
  => "ciao 123")

(check
    (format #f "ciao ~:a" display)
  => (cond-expand
      (ypsilon  "ciao \"#<subr display>\"")
      (larceny	"ciao \"#<PROCEDURE display>\"")
      (ikarus	"ciao \"#<procedure display>\"")
      (mosh	"ciao \"#<subr display>\"")))

(check
    (format #f "ciao ~:s" 123)
  => "ciao 123")

(check
    (format #f "ciao ~:s" display)
  => (cond-expand
      (ypsilon  "ciao \"#<subr display>\"")
      (larceny	"ciao \"#<PROCEDURE display>\"")
      (ikarus	"ciao \"#<procedure display>\"")
      (mosh	"ciao \"#<subr display>\"")))

;;; --------------------------------------------------------------------

(check
    (list (format "~5a" 123)
	  (format "~5s" 123))
  => '("123  "
       "123  "))

(check
    (list (format "~5@a" 123)
	  (format "~5@s" 123))
  => '("  123"
       "  123"))

(check
    (list (format "~5,,,'.a" 123)
	  (format "~5,,,'.s" 123))
  => '("123.."
       "123.."))

(check
    (list (format "~5,,,'.@a" 123)
	  (format "~5,,,'.@s" 123))
  => '("..123"
       "..123"))

(check
    (list (format "~5,,4,'.@a" 123)
	  (format "~5,,4,'.@s" 123))
  => '("....123"
       "....123"))
;;;     1234

(check
    (list (format "~10,,,'a@a" 123)
	  (format "~10,,,'a@s" 123))
  => '("aaaaaaa123"
       "aaaaaaa123"))
;;;     1234567

(check
    (list (format "~10,3,,'u@a" 123)
	  (format "~10,3,,'u@s" 123))
  => '("uuuuuuuuu123"
       "uuuuuuuuu123"))
;;;     123456789

(check
    (list (format "~11,2,,'u@a" 123)
	  (format "~11,2,,'u@s" 123))
  => '("uuuuuuuu123"
       "uuuuuuuu123"))
;;;     12345678

(check
    (list (format "~8,2,,'u@a" 1)
	  (format "~8,2,,'u@s" 1))
  => '("uuuuuuuu1"
       "uuuuuuuu1"))
;;;     12345678



;;;; character

(check
    (format "~c" #\A)
  => "A")

(check
    (format "~@c" #\A)
  => "#\\A")

(check
    (format "~:c" #\newline)
  => "^J")

(check
    (format "~:c" #\linefeed)
  => "^J")

(check
    (format "~65c")
  => "A")



;;;; integers

(check
    (format "~d" 123)
  => "123")

(check
    (format "~x ~x" 3 10)
  => "3 a")

(check
    (format "~o" 509)
  => "775")

(check
    (format "~b" 6)
  => "110")

;;; --------------------------------------------------------------------

(check
    (format "~d" -123)
  => "-123")

(check
    (format "~x ~x" -3 -10)
  => "-3 -a")

(check
    (format "~o" -509)
  => "-775")

(check
    (format "~b" -6)
  => "-110")

;;; --------------------------------------------------------------------

(check
    (format "~5d" 123)
  => "  123")
;;;   12345

(check
    (format "~5x" 11)
  => "    b")
;;;   12345

(check
    (format "~5o" 509)
  => "  775")
;;;   12345

(check
    (format "~5b" 6)
  => "  110")
;;;   12345

;;; --------------------------------------------------------------------

(check
    (format "~5,'.d" 123)
  => "..123")
;;;   12345

(check
    (format "~5,'.x" 11)
  => "....b")
;;;   12345

(check
    (format "~5,'.o" 509)
  => "..775")
;;;   12345

(check
    (format "~5,'.b" 6)
  => "..110")
;;;   12345

;;; --------------------------------------------------------------------

(check
    (format "~@d" 0)
  => "+0")

(check
    (format "~@d" 123)
  => "+123")

(check
    (format "~@x ~@x" 3 10)
  => "+3 +a")

(check
    (format "~@o" 509)
  => "+775")

(check
    (format "~@b" 6)
  => "+110")

;;; --------------------------------------------------------------------

(check
    (format "~:d" 123456789)
  => "123,456,789")

(check
    (format "~:x" #x123456789)
  => "123,456,789")

(check
    (format "~:o" #o123456712)
  => "123,456,712")

(check
    (format "~:b" #b10101100)
  => "10,101,100")

(check
    (format "~,,'b,2:d" 123456789)
  => "1b23b45b67b89")

(check
    (format "~,,'b,2:x" #x123456789)
  => "1b23b45b67b89")

(check
    (format "~,,'b,2:o" #o123456712)
  => "1b23b45b67b12")

(check
    (format "~,,'b,2:b" #b10101100)
  => "10b10b11b00")

;;; --------------------------------------------------------------------

(check
    (format "~x" 65261)
  => "feed")

(check
    (format "~:@(~x~)" 65261)
  => "FEED")


;;;; integers in words

(check
    (format "~r" 123)
  => "one hundred twenty-three")

(check
    (format "~r" -123)
  => "minus one hundred twenty-three")

(check
    (format "~r" 1000000)
  => "one million")

(check
    (format "~r" 1000000000)
  => "one billion")

(check
    (format "~r" 1000000000000)
  => "one trillion")

;;; --------------------------------------------------------------------

(check
    (format "~:r" 123)
  => "one hundred twenty-third")

(check
    (format "~:r" 9)
  => "ninth")

;;; --------------------------------------------------------------------

(check
    (format "~@r" 89)
  => "LXXXIX")

(check
    (format "~:@r" 89)
  => "LXXXVIIII")

;;; --------------------------------------------------------------------

(check
    (format "~3r" 0)
  => "0")

(check
    (format "~3r" 1)
  => "1")

(check
    (format "~3r" 2)
  => "2")

(check
    (format "~3r" 3)
  => "10")

(check
    (format "~3r" 4)
  => "11")

(check
    (format "~3r" 27)
  => "1000")

(check
    (format "~3,5r" 26)
  => "  222")

;;; --------------------------------------------------------------------

(check
    (format "~4r" 0)
  => "0")

(check
    (format "~4r" 1)
  => "1")

(check
    (format "~4r" 2)
  => "2")

(check
    (format "~4r" 3)
  => "3")

(check
    (format "~4r" 4)
  => "10")

(check
    (format "~4r" 9)
  => "21")




;;;; floats

(check
    (format "~f" 123)
  => "123.0")

(check
    (format "~f" 123.0)
  => "123.0")

(check
    (format "~f" 123.4)
  => "123.4")

(check
    (format "~f" 1e-1)
  => "0.1")

(check
    (format "~f" +inf.0)
  => "+inf.0")

(check
    (format "~f" -inf.0)
  => "-inf.0")

(check
    (format "~f" +nan.0)
  => "+nan.0")

;;; --------------------------------------------------------------------
;;; @ modifier

(check
    (format "~@f" 123)
  => "+123.0")

(check
    (format "~@f" 123.0)
  => "+123.0")

(check
    (format "~@f" 123.4)
  => "+123.4")

(check
    (format "~@f" 1e-1)
  => "+0.1")

(check
    (format "~@f" -123.0)
  => "-123.0")

;;; --------------------------------------------------------------------
;;; width and padding

(check
    (format "~10f" 123.456)
  => "   123.456")
;;;   0123456789

(check
    (format "~10,,,,'.f" 123.456)
  => "...123.456")
;;;   0123456789

(check
    (format "~10,,,,'.f" 123.456789123)
  => "123.456789")

(check
    (format "~5,,,,'.f" 1e9)
  => "1000000000.0")

(check
    (format "~5,,,,'.f" 1000000000.123456)
  => "1000000000.123456")

;;; --------------------------------------------------------------------
;;; strings

(check
    (format "~12,2f" "1.2345")
  => "        1.23")

(check
    (format "~f" "#d1.2345")
  => "1.2345")

(check
    (format "~f" "1.2345")
  => "1.2345")

(check
    (format "~f" "1.23e4")
  => "12300.0")

(check
    (format "~f" "-1.23")
  => "-1.23")

(check
    (format "~f" "+1.23")
  => "1.23")

(check
    (guard (exc (else 'error))
      (format "~12,2f" "1.23+45"))
  => 'error)

(check
    (guard (exc (else 'error))
      (format "~12,2f" "1.23-45"))
  => 'error)

(check
    (guard (exc (else 'error))
      (format "~12,2f" "1.2345e6-1"))
  => 'error)

(check
    (guard (exc (else 'error))
      (format "~12,2f" "1.2345e6+1"))
  => 'error)

(check
    (guard (exc (else 'error))
      (format "~f" "1.2.3e61"))
  => 'error)

(check
    (guard (exc (else 'error))
      (format "~f" "1..3e61"))
  => 'error)

(check
    (guard (exc (else 'error))
      (format "~f" "1.23e6.1"))
  => 'error)

(check
    (guard (exc (else 'error))
      (format "~f" "1.23e6e1"))
  => 'error)

(check
    (guard (exc (else 'error))
      (format "~f" "1.23a61"))
  => 'error)




;;; escape sequence ~f and decimals

(check
    (format "~6,3f" 1/3)
  => " 0.333")

(check
    (format "~8,3f" 12.3456)
  => "  12.346")

(check
    (format "~6,3f" 123.3456)
  => "123.346")

(check
    (format "~4,3f" 123.3456)
  => "123.346")

(check
    (format "~8,1f" 32e-45)
  => "     0.0")
;;;   01234567

(check
    (format "~8,2f" 32e10)
  => "320000000000.00")
;;;   012345678901

;;; --------------------------------------------------------------------

(check
    (format "~8,2f" 3.4567e11)
  => "345670000000.00")
;;;   012345678901

(check
    (format "~6,2f" 3.14159)
  => "  3.14")

(check
    (format "~6,1f" 3.14159)
  => "   3.1")

(check
    (format "~6,0f" 3.14159)
  => "    3.")

(check
    (format "~5,1f" 0)
  => "  0.0")

(check
    (format "~10,7f" 3.14159)
  => " 3.1415900")

(check
    (format "~10,7f" -3.14159)
  => "-3.1415900")

(check
    (format "~6,3f" 0.0)
  => " 0.000")

(check
    (format "~6,4f" 0.007)
  => "0.0070")

(check
    (format "~8,4f" 0.007)
  => "  0.0070")

(check
    (format "~6,3f" 0.007)
  => " 0.007")

(check
    (format "~6,2f" 0.007)
  => "  0.01")

(check
    (format "~3,2f" 0.007)
  => ".01")

(check
    (format "~3,2f" -0.007)
  => "-.01")

(check
    (format "~6,3f" 12345.6789)
  => "12345.679")

;;; --------------------------------------------------------------------


;;;; rounding with ~f

(check
    (format "~12,2f" 1.2345)
  => "        1.23")
;;;   012345678901

(check
    (format "~12,3f" 1.2345)
  => "       1.234")
;;;   012345678901

(check
    (format "~,2f" 0.007)
  => "0.01")

(check
    (format "~,5f" 12.456e999)
  => "+inf.0")

(check
    (format "~,5f" -12.456e999)
  => "-inf.0")

(check
    (format "~,5f" 12.456e10)
  => "124560000000.00000")
;;;     0123456789

(check
    (format "~,5f" 12.456)
  => "12.45600")

(check
    (format "~,5f" 12.456)
  => "12.45600")

;;; --------------------------------------------------------------------

(check
    (format "~,1f" 12.44)
  => "12.4")

(check
    (format "~,1f" 12.46)
  => "12.5")

;;When 5 is the last digit: the number is rounded with the last digit in
;;the result being the nearest even.
(check
    (format "~,1f" 12.45)
  => "12.4")

(check
    (format "~,1f" 12.451)
  => "12.5")

(check
    (format "~,1f" 12.454)
  => "12.5")

(check
    (format "~,1f" 12.456)
  => "12.5")

;;Not so weird if you think of it!
(check
    (format "~,1f" 12.449)
  => "12.4")

;;Rounding 55 is done to the nearest even which is 60.
(check
    (format "~,2f" 12.455)
  => "12.46")

(check
    (format "~,1f" 12.455)
  => "12.5")

(check
    (format "~,1f" 12.4555)
  => "12.5")

(check
    (format "~,1f" 12.4555)
  => "12.5")

(check
    (format "~,1f" 12.45555)
  => "12.5")

;;; --------------------------------------------------------------------

(check
    (format "~,0f" 12.456789)
  => "12.")

;;Rounding 12.456789  to 1  digit in the  fractional part is  like doing
;;these steps:
;;
;; 12.456789 -> 12.45679 -> 12.4568 -> 12.457 -> 12.46 -> 12.5
;;
(check
    (format "~,1f" 12.456789)
  => "12.5")

(check
    (format "~,2f" 12.456789)
  => "12.46")

(check
    (format "~,3f" 12.456789)
  => "12.457")

(check
    (format "~,4f" 12.456789)
  => "12.4568")

(check
    (format "~,5f" 12.456789)
  => "12.45679")

(check
    (format "~,6f" 12.456789)
  => "12.456789")

;;; --------------------------------------------------------------------

;;We want  the same behaviour requested  by R6RS for ROUND,  and by IEEE
;;754 for rounding to nearest.  When rounding an in-the-middle digit, we
;;round it to even.
(check (format "~,0f" 0.5) => "0.")
(check (format "~,0f" 1.5) => "2.")
(check (format "~,0f" 2.5) => "2.")
(check (format "~,0f" 3.5) => "4.")
(check (format "~,0f" 4.5) => "4.")
(check (format "~,0f" 5.5) => "6.")
(check (format "~,0f" 6.5) => "6.")
(check (format "~,0f" 7.5) => "8.")
(check (format "~,0f" 8.5) => "8.")
(check (format "~,0f" 9.5) => "10.")

(check (format "~,0f" 0.0)  => "0.")
(check (format "~,0f" 0.3)  => "0.")
(check (format "~,0f" 0.51) => "1.")
(check (format "~,0f" 0.7)  => "1.")

;;Remember that the  dot and fractional part are  truncated only if this
;;makes  the output  fit the  requested WIDTH,  else they  are  kept and
;;rounded only if a number of digits after the dot was requested.
(check (format "~1,0f" 0.0)  => "0.")
(check (format "~1,0f" 1.4)  => "1.")
(check (format "~1,0f" 1.5)  => "2.")
(check (format "~1,0f" 1.6)  => "2.")

(check (format "~1f"   0.123) => "0.123")
(check (format "~1,2f" 0.123) => ".12")
(check (format "~1,2f" 1.123) => "1.12")
(check (format "~2,2f" 0.123) => ".12")



;;;; complex numbers

(check
    (format "~i" 1+2i)
  => "1.0+2.0i")

(check
    (format "~,3i" (sqrt -3.8))
  => "0.000+1.949i")

(check
    (format "~10,3i" (sqrt -3.8))
  => "     0.000    +1.949i")
;;;   0123456789
;;;             0123456789

(check
    (format "~8,3i" (sqrt -3.8))
  => "   0.000  +1.949i")
;;;   01234567
;;;           01234567




;;;; done

(check-report)

;;; end of file
