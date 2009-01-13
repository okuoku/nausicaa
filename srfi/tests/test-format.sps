;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
;;;Copyright (c) 2008 Derick Eddington
;;;Copyright (C) 2003 Kenneth A Dickey.
;;;
;;;Some test comes from the SRFI itself other from SLIB.
;;;
;;;Permission is hereby granted, free of charge, to any person obtaining
;;;a  copy of  this  software and  associated  documentation files  (the
;;;"Software"), to  deal in the Software  without restriction, including
;;;without limitation  the rights to use, copy,  modify, merge, publish,
;;;distribute, sublicense,  and/or sell copies  of the Software,  and to
;;;permit persons to whom the Software is furnished to do so, subject to
;;;the following conditions:
;;;
;;;The  above  copyright notice  and  this  permission  notice shall  be
;;;included in all copies or substantial portions of the Software.
;;;
;;;Except  as  contained  in  this  notice, the  name(s)  of  the  above
;;;copyright holders  shall not be  used in advertising or  otherwise to
;;;promote  the sale,  use or  other dealings  in this  Software without
;;;prior written authorization.
;;;
;;;THE  SOFTWARE IS  PROVIDED "AS  IS",  WITHOUT WARRANTY  OF ANY  KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT.  IN NO EVENT  SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY,  WHETHER IN AN
;;;ACTION OF  CONTRACT, TORT  OR OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION  WITH THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.



#!r6rs
(import
  (rnrs)
  (rnrs mutable-pairs)
  (srfi format)
  (check-lib)
  (srfi sharing))

(check-set-mode! 'report-failed)
(display "*** testing format\n")


;;;; misc

(check
    (format "~h")
  => "(format [<port>] <format-string> [<arg>...]) -- <port> is #t, #f or an output-port
OPTION  [MNEMONIC]      DESCRIPTION     -- Implementation Assumes ASCII Text Encoding
~H      [Help]          output this text
~A      [Any]           (display arg) for humans
~S      [Slashified]    (write arg) for parsers
~W      [WriteCircular] like ~s but outputs circular and recursive data structures
~~      [tilde]         output a tilde
~T      [Tab]           output a tab character
~%      [Newline]       output a newline character
~&      [Freshline]     output a newline character if the previous output was not a newline
~D      [Decimal]       the arg is a number which is output in decimal radix
~X      [heXadecimal]   the arg is a number which is output in hexdecimal radix
~O      [Octal]         the arg is a number which is output in octal radix
~B      [Binary]        the arg is a number which is output in binary radix
~w,dF   [Fixed]         the arg is a string or number which has width w and d digits after the decimal
~C      [Character]     charater arg is output by write-char
~_      [Space]         a single space character is output
~Y      [Yuppify]       the list arg is pretty-printed to the output
~?      [Indirection]   recursive format: next 2 args are format-string and list of arguments
~K      [Indirection]   same as ~?
")

(check
    (format "Hello, ~a" "World!")
  => "Hello, World!")

(check
    (format "Error, list is too short: ~s" '(one "two" 3))
  => "Error, list is too short: (one \"two\" 3)")

(check
    (format "test me")
  => "test me")

(check
    (format "~a ~s ~a ~s" 'this 'is "a" "test")
  => "this is a \"test\"")

(check
    (format #f "#d~d #x~x #o~o #b~b~%" 32 32 32 32)
  => "#d32 #x20 #o40 #b100000\n")

(check
    (format "~a ~? ~a" 'a "~s" '(new) 'test)
  => "a new test")

(check
    (format #f "~&1~&~&2~&~&~&3~%")
  => "\n1\n2\n3\n")

(check
    (format #f "~a ~? ~a ~%" 3 " ~s ~s " '(2 2) 3)
  => "3  2 2  3 \n")

(check
    (let* ((ell		(let ((ell (list 'a 'b 'c)))
			  (set-cdr! (cddr ell) ell)
			  ell))
	   (it		(format "~w" ell))
	   (thing	(read/ss (open-string-input-port it))))
      (list (eq? (car ell) (car thing))
	    (eq? (cadr ell) (cadr thing))
	    (eq? (caddr ell) (caddr thing))
	    (eq? (cadddr ell) (cadddr thing))))
  => '(#t #t #t #t))

(check
    (format "~a~a~&" (list->string (list #\newline)) "")
  => "\n")

(check
    (format "abc")
  => "abc")

(check
    (format "~a" 10)
  => "10")

(check
    (format "~a" -1.2)
  => "-1.2")

(check
    (format "~a" 'a)
  => "a")

(check
    (format "~a" #t)
  => "#t")

(check
    (format "~a" #f)
  => "#f")

(check
    (format "~a" "abc")
  => "abc")

(check
    (format "~a" '#(1 2 3))
  => "#(1 2 3)")

(check
    (format "~a" '())
  => "()")

(check
    (format "~a" '(a))
  => "(a)")

(check
    (format "~a" '(a b))
  => "(a b)")

(check
    (format "~a" '(a (b c) d))
  => "(a (b c) d)")

(check
    (format "~a" '(a . b))
  => "(a . b)")

(check
    (format "~a" '(a (b c . d)))
  => "(a (b c . d))")

;;; --------------------------------------------------------------------

(check
    (format "~a ~a" 10 20)
  => "10 20")

(check
    (format "~a abc ~a def" 10 20)
  => "10 abc 20 def")

;;; --------------------------------------------------------------------

(check
    (format "~d" 100)
  => "100")

(check
    (format "~x" 100)
  => "64")

(check
    (format "~o" 100)
  => "144")

(check
    (format "~b" 100)
  => "1100100")

;;; --------------------------------------------------------------------

(check
    (format "~c" #\a)
  => "a")

;;; --------------------------------------------------------------------

(check
    (format "~~~~")
  => "~~")

;;; --------------------------------------------------------------------

(check
    (format "~%")
  => "\n")

(check
    (format "~&")
  => "\n")

(check
    (format "abc~&")
  => "abc\n")

(check
    (format "abc~&def")
  => "abc\ndef")

(check
    (format "~&")
  => "\n")

(check
    (format "~_~_~_")
  => "   ")

;;; --------------------------------------------------------------------

(check
    (format "~a ~? ~a" 10 "~a ~a" '(20 30) 40)
  => "10 20 30 40")

;;; --------------------------------------------------------------------

(check
    (format "~s" "abc")
  => "\"abc\"")

(check
    (format "~s" "abc \\ abc")
  => "\"abc \\\\ abc\"")

(check
    (format "~a" "abc \\ abc")
  => "abc \\ abc")

(check
    (format "~s" "abc \" abc")
  => "\"abc \\\" abc\"")

(check
    (format "~a" "abc \" abc")
  => "abc \" abc")

(check
    (format "~s" #\space)
  => "#\\space")

(check
    (format "~s" #\newline)
  => "#\\newline")

(check
    (format "~s" #\linefeed)
  => "#\\newline")

(check
    (format "~s" #\a)
  => "#\\a")

(check
    (format "~s" '(a "b" c))
  => "(a \"b\" c)")

(check
    (format "~a" '(a "b" c))
  => "(a b c)")



;;;; floating point numbers

(check
    (format "~F" 123)
  => "123")

(check
    (format "~F" 123.456)
  => "123.456")

(check
    (format "~F" 123+456i)
  => "123+456i")

;;; --------------------------------------------------------------------

(check
    (format "~F" +inf.0)
  => "+inf.0")
(check
    (format "~F" -inf.0)
  => "-inf.0")

(check
    (format "~F" +nan.0)
  => "+nan.0")

;;; --------------------------------------------------------------------

(check
    (format "~4F" 12)
  => "  12")

(check
    (format "~4F" 1234)
  => "1234")

(check
    (format "~4F" 123456)
  => "123456")

(check
    (format "~6F" 32)
  => "    32")

(check
    (format "~6F" 32.)
  => "  32.0")

(check
    (format "~8F" 32e45)
  => "  3.2e46")

(check
    (format "~12F" 1.2345)
  => "      1.2345")

(check
    (format "~6f" 23.4)
  => "  23.4")

(check
    (format "~6f" 1234.5)
  => "1234.5")

(check
    (format "~6f" 12345678)
  => "12345678")

(check
    (format "~6,2f" 123.56789)
  => "123.57")

(check
    (format "~6f" 123.0)
  => " 123.0")

(check
    (format "~6f" -123.0)
  => "-123.0")

(check
    (format "~6f" 0.0)
  => "   0.0")

(check
    (format "~3,1f" 3.141)
  => "3.1")

(check
    (format "~2,0f" 3.141)
  => "3.")

(check
    (format "~1f" 3.141)
  => "3.141")

(check
    (format "~f" 123.56789)
  => "123.56789")

(check
    (format "~f" -314.0)
  => "-314.0")

(check
    (format "~f" 1e4)
  => "10000.0")

(check
    (format "~f" -1.23e10)
  => "-1230000000.0")

(check
    (format "~f" -1.23e-10)
  => "-0.000000000123")

(check
    (format "~f" 1e-4)
  => "0.0001")

;;; --------------------------------------------------------------------

(check
    (format "~6,2F" 32)
  => " 32.00")

(check
    (format "~8F" 32e20)
  => "  3.2e21")

(check
    (format "~8F" 32e5)
  => "3200000.0")

(check
    (format "~8F" 32e2)
  => "  3200.0")

;;; --------------------------------------------------------------------

(check
    (format "~6,3F" 1/3)
  => " 0.333")

(check
    (format "~8,3F" 12.3456)
  => "  12.346")

(check
    (format "~6,3F" 123.3456)
  => "123.346")

(check
    (format "~4,3F" 123.3456)
  => "123.346")

(check
    (format "~8,1F" 32e-45)
  => " 3.2e-44")

(check
    (format "~8,2F" 32e10)
  => " 3.20e11")

(check
    (format "~12,2F" 1.2345)
  => "        1.23")

(check
    (format "~12,3F" 1.2345)
  => "       1.234")

(check
    (format "~20,3F" (sqrt -3.8))
  => "        0.000+1.949i")

(check
    (format "~8,3F" (sqrt -3.8))
  => "0.000+1.949i")

(check
    (format "~8,2F" 3.4567e11)
  => " 3.46e11")

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
  => "0.01")

(check
    (format "~3,2f" -0.007)
  => "-0.01")

(check
    (format "~6,3f" 12345.6789)
  => "12345.679")

;;; --------------------------------------------------------------------

(check
    (format "~8,3F" (sqrt -3.8))
  => "0.000+1.949i")




;;;; done

(check-report)


;;; end of file
