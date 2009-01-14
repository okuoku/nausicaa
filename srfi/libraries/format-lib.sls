;;;"format.scm" Common LISP text output formatter for SLIB
;;;Written 1992-1994 by Dirk Lutzebaeck (lutzeb@cs.tu-berlin.de)
;;;Assimilated into Guile May 1999
;;;
;;;This code is in the public domain.
;;;
;;;Authors of  the original version (<  1.4) were Ken  Dickey and Aubrey
;;;Jaffer.
;;;
;;;Ported to R6RS Scheme by Marco Maggi.
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
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
(library (format-lib)
  (export format)
  (import (rename (rnrs)
		  (infinite? rnrs:infinite?)
		  (nan? rnrs:nan?))
    (only (rnrs r5rs)
	  remainder
	  quotient)
    (only (rnrs mutable-strings)
	  string-set!)
    (format-lib compat))



;;;; configuration

;;Symbols are  converted by  SYMBOL->STRING so the  case of  the printed
;;symbols is implementation dependent.  FORMAT:SYMBOL-CASE-CONV is a one
;;arg  closure  which  is  either #f  (no  conversion),  STRING-UPCASE!,
;;STRING-DOWNCASE! or STRING-CAPITALIZE!.
(define format:symbol-case-conv #f)

;;As  FORMAT:SYMBOL-CASE-CONV  but  applies  for the  representation  of
;;implementation internal objects.
(define format:iobj-case-conv #f)

;;The character prefixing the exponent value in "~e" printing.
(define format:expch #\E)

;;Detects if the scheme system implements flonums (see at eof).
(define format:floats #t)

;;Detects if the scheme system implements complex numbers.
(define format:complex-numbers #t)

(define format:version "4.0")



;;;; constants

(define format:ascii-non-printable-charnames
  '#("nul" "soh" "stx" "etx" "eot" "enq" "ack" "bel"
     "bs"  "ht"  "nl"  "vt"  "np"  "cr"  "so"  "si"
     "dle" "dc1" "dc2" "dc3" "dc4" "nak" "syn" "etb"
     "can" "em"  "sub" "esc" "fs"  "gs"  "rs"  "us" "space"))

(define format:parameter-characters
  '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\- #\+ #\v #\# #\'))

(define format:space-ch (char->integer #\space))
(define format:zero-ch	(char->integer #\0))

;;Roman numerals (from dorai@cs.rice.edu).
(define format:roman-alist
  '((1000 #\M) (500 #\D) (100 #\C) (50 #\L)
    (10 #\X) (5 #\V) (1 #\I)))

(define format:roman-boundary-values
  '(100 100 10 10 1 1 #f))

;;Cardinals & ordinals (from dorai@cs.rice.edu).
(define format:cardinal-ones-list
  '(#f
    "one"	"two"		"three"
    "four"	"five"		"six"
    "seven"	"eight"		"nine"
    "ten"	"eleven"	"twelve"
    "thirteen"	"fourteen"	"fifteen"
    "sixteen"	"seventeen"	"eighteen"
    "nineteen"))

(define format:cardinal-tens-list
  '(#f
    #f		"twenty"	"thirty"
    "forty"	"fifty"		"sixty"
    "seventy"	"eighty"	"ninety"))

(define format:cardinal-thousand-block-list
  '(""
    " thousand"		" million"		" billion"
    " trillion"		" quadrillion"		" quintillion"
    " sextillion"	" septillion"		" octillion"
    " nonillion"	" decillion"		" undecillion"
    " duodecillion"	" tredecillion"		" quattuordecillion"
    " quindecillion"	" sexdecillion"		" septendecillion"
    " octodecillion"	" novemdecillion"	" vigintillion"))

(define format:ordinal-ones-list
  '(#f
    "first"		"second"		"third"
    "fourth"		"fifth"			"sixth"
    "seventh"		"eighth"		"ninth"
    "tenth"		"eleventh"		"twelfth"
    "thirteenth"	"fourteenth"		"fifteenth"
    "sixteenth"		"seventeenth"		"eighteenth"
    "nineteenth"))

(define format:ordinal-tens-list
  '(#f
    #f			"twentieth"		"thirtieth"
    "fortieth"		"fiftieth"		"sixtieth"
    "seventieth"	"eightieth"		"ninetieth"))



;;;; porting and miscellaneous helpers

(define (port-column port)
  0)

(define string-capitalize string-titlecase)

;;Return the  first K  elements from the  list ELL; this  function comes
;;from SRFI lists.
(define (take ell k)
  (let loop ((ell ell)
	     (k   k))
    (if (zero? k)
	'()
      (cons (car ell)
	    (loop (cdr ell) (- k 1))))))

;;Return true if S1 is the prefix in S2.
(define (string-prefix? s1 s2)
  (or (eq? s1 s2)
      (let ((len1 (string-length s1)))
	(and (<= len1 (string-length s2))
	     (string=? s1 (substring s2 0 len1))))))

;;Return the index of CH in STR.
(define (string-index str ch)
  (let ((len (string-length str)))
    (do ((i 0 (+ 1 i)))
	((or (= i len) (char=? ch (string-ref str i)))
	 (if (= i len) #f i)))))

;;Convert a string to its  representation with the first alphabetic char
;;capitalised.   We iterate  over  the chars  rather  than extracting  a
;;substring so that we can apply CHAR-ALPHABETIC?.
;;
;;Some  profiling is  needed to  understand if  an  implementation using
;;SUBSTRING is more efficient with a given Scheme implementation.
;;
;;Usage examples:
;;
;; "hello"	-> "Hello"
;; "hELLO"	-> "Hello"
;; "*hello"	-> "*Hello"
;; "hello you"	-> "Hello you"
;;
(define (string-capitalize-first str)
  (let ((cap-str		(string-copy str))
	(non-first-alpha	#f)
	(str-len		(string-length str)))
    (do ((i 0 (+ i 1)))
	((= i str-len)
	 cap-str)
      (let ((c (string-ref str i)))
	(when (char-alphabetic? c)
	  (if non-first-alpha
	      (string-set! cap-str i (char-downcase c))
	    (begin
	      (set! non-first-alpha #t)
	      (string-set! cap-str i (char-upcase c)))))))))

;;Return the string representation of  the integer NUM in the RADIX.  It
;;extends R6RS  NUMBER->STRING to support any  radix, not only  2, 8, 10
;;and 16.
(define (number->string/radix num radix)
  (unless (and (integer? num) (exact? num))
    (assertion-violation 'number->string
      "only integers can be converted to a base different from 10"
      num))
  (unless (and (integer? radix) (exact? radix) (positive? radix))
    (assertion-violation 'number->string
      "the radix has to be a strictly positive exact integer"
      radix))
  (case radix
    ((2 8 10 16)
     (number->string num radix))
    (else
     (let loop ((num num)
		(res '()))
       (if (< num radix)
	   (apply string-append (number->string num) res)
	 (loop (exact (floor (inexact (/ num radix))))
	       (cons (number->string (remainder num radix)) res)))))))

(define-syntax increment!
  (syntax-rules ()
    ((_ ?varname ?step)
     (set! ?varname (+ ?varname ?step)))))

;;Extract an  escape sequence  parameter from a  list of  parameters and
;;return it.
(define (format:par parameters number-of-parameters
		    index default parameter-name)
  (if (<= number-of-parameters index)
      default
    (let ((par (list-ref parameters index)))
      (if par
	  (if parameter-name
	      (if (< par 0)
		  (assertion-violation 'format:par
		    (string-append "parameter "
				   (string-upcase parameter-name)
				   " for escape sequence ~s must be a positive integer")
		    par)
		par)
	    par)
	default))))

;;R6RS states that FINITE?, INFINITE? and NAN? reject complex numbers.
(define (infinite? num)
  (or (rnrs:infinite? (real-part num))
      (rnrs:infinite? (imag-part num))))
(define (nan? num)
  (or (rnrs:nan? (real-part num))
      (rnrs:nan? (imag-part num))))


;;;; incipit
(define (format . args)

  ;;Current format output port.
  (define format:port #f)

  ;;Current format output TTY column.
  (define format:output-col 0)

  ;;Flush output at end of formatting.
  (define format:flush-output #f)

  (define format:case-conversion #f)

  ;;Current format string parsing position.
  (define format:pos 0)

  ;;Current   format  argument   position   this  is   global  for   error
  ;;presentation.
  (define format:arg-pos 0)

  ;;When  FORMAT:READ-PROOF  is  true,  FORMAT:OBJ->STR will  wrap  result
  ;;strings starting with "#<" in an extra pair of double quotes.
  (define format:read-proof #f)

;;; --------------------------------------------------------------------
;;; Floating point numbers related stuff.

  ;;Maximum number of digits.
  (define format:fn-max 400)

  ;;Floating point  number buffer for  the mantissa.  It is  filled with
  ;;the  string  representation  of  the  mantissa.  If  the  flonum  is
  ;;"12.345e67", this buffer is filled with "12345".
  ;;
  ;;Notice that the dot is not stored in the buffer.  The sign (positive
  ;;or negative) also  is not stored in the buffer,  rather it is marked
  ;;by FORMAT:FN-POS?.
  (define format:fn-str #f)

  ;;The index  of the first  *unused* char in the  FORMAT:FN-STR buffer.
  ;;It is also the length of the mantissa string in FORMAT:FN-STR.
  (define format:fn-len 0)

  ;;The index of  the digit in FORMAT:FN-STR that  comes right after the
  ;;dot.  If  the flonum is  "12.345e67", the mantissa buffer  is filled
  ;;with "12345"  and this  variable is set  to 2,  so '3' is  the digit
  ;;right after the dot.
  (define format:fn-dot #f)

  ;;Set to #t if the mantissa is positive, to #f otherwise.
  (define format:fn-pos? #t)

  ;; max. number of exponent digits
  (define format:en-max 10)

  ;;Floating point  number buffer for  the exponent.  It is  filled with
  ;;the  string  representation  of  the  exponent.  If  the  flonum  is
  ;;"12.345e67", this buffer is filled with "67".
  ;;
  ;;Notice that  The sign  (positive or negative)  is NOT stored  in the
  ;;buffer, rather it is marked by FORMAT:EN-POS?.
  (define format:en-str #f)

  ;;The index  of the first  *unused* char in the  FORMAT:EN-STR buffer.
  ;;It is also the length of the exponent string in FORMAT:EN-STR.
  (define format:en-len 0)

  ;;Set to #t if the exponent is positive, to #f otherwise.
  (define format:en-pos? #t)



;;;; helpers, output to destination functions

;;Print a string with case conversion.  Update the output column.
(define (format:out-str str)
  (display (if format:case-conversion
	       (format:case-conversion str)
	     str)
	   format:port)
  (increment! format:output-col (string-length str)))

;;Print  a single  character with  case conversion.   Update  the output
;;column.
(define (format:out-char ch)
  (if format:case-conversion
      (display (format:case-conversion (string ch)) format:port)
    (write-char ch format:port))
  (set! format:output-col
	(if (char=? ch #\newline)
	    0
	  (+ format:output-col 1))))

;;Print a substring.  Update the output column.
(define (format:out-substr str i n)
  (display (substring str i n) format:port)
  (increment! format:output-col n))

;;Print a string filled with the same char.  Update the output column.
(define (format:out-fill n ch)
  (format:out-str (make-string n ch))
  (increment! format:output-col n))



;;;; helpers, dispatching to destination port

;;Invoked at the  end of the FORMAT body; ARGS is  the list of arguments
;;given to FORMAT.  This function parses ARGS to select the output port,
;;then invokes FORMAT:FORMAT to start the formatting.
(define (format:dispatch-to-destination-port args)
  (when (< (length args) 1)
    (assertion-violation 'format:dispatch-to-destination-port
      "not enough arguments"))
  (let ((args (if (string? (car args))
		  (cons #f args)
		args)))
    (let ((destination		(car args))
	  (format-string	(cadr args))
	  (arglist		(cddr args)))
      (cond
       ((boolean? destination)
	(if destination
	    (format:format (current-output-port) format-string arglist)
	  (call-with-string-output-port
	      (lambda (port)
		(format:format port format-string arglist)))))
       ((output-port? destination)
	(format:format destination format-string arglist))
       ((number? destination)
	(format:format (current-error-port) format-string arglist))
       (else
	(assertion-violation 'format:dispatch-to-destination-port
	  "invalid destination" destination))))))

;;Setup the internal state for  this invocation to FORMAT, then hand the
;;arguments to FORMAT:FORMAT-WORK.
(define (format:format port format-string arglist)
  (set! format:port port)
  (let* ((col (port-column port)))
    (when col
      (set! format:output-col col)))

  (let ((arg-pos (format:format-work format-string arglist))
	(arg-len (length arglist)))
    (if (> arg-pos arg-len)
	(begin
	  (set! format:arg-pos (+ arg-len 1))
	  (display format:arg-pos)
	  (error 'format:format
	    "missing argument" (- arg-pos arg-len)))
      (when format:flush-output
	(flush-output-port port)))))



;;;; helpers, any object to string

;;Convert  an arbitrary object  OBJ to  a string.   If USE-WRITE  is true
;;WRITE style is selected, else DISPLAY style is used.
(define (format:obj->str obj use-write)
  (let ((res (call-with-string-output-port
		 (lambda (port) ((if use-write write display)
				 obj port)))))
    (if (and format:read-proof (string-prefix? "#<" res))
	(call-with-string-output-port
	    (lambda (port) (write res port)))
      res)))

;;Print any object with padding  chars.  It is the implementation of the
;;"~s" and "~a" escape sequences.
;;
;;If PAD-LEFT is  false the object string is  printed first, followed by
;;the  padding (if any);  else the  padding (if  any) is  printed first,
;;followed by the object string.
(define (format:out-obj-padded pad-left obj use-write parameters)
  (if (null? parameters)
      (format:out-str (format:obj->str obj use-write))
    (let ((l (length parameters)))
      (let ((minwidth	(format:par parameters l 0 0 "minwidth"))
	    (padinc	(format:par parameters l 1 1 "padinc"))
	    (minpad	(format:par parameters l 2 0 "minpad"))
	    (padchar	(integer->char
			 (format:par parameters l 3 format:space-ch #f)))
	    (objstr	(format:obj->str obj use-write)))
	(unless pad-left
	  (format:out-str objstr))
	(do ((objstr-len (string-length objstr))
	     (i minpad (+ i padinc)))
	    ((>= (+ objstr-len i) minwidth)
	     (format:out-fill i padchar)))
	(when pad-left
	  (format:out-str objstr))))))



;;;; helpers, character to string

;;Convert a  character into a slashified  string as done  by WRITE.  The
;;procedure is dependent on the integer representation of characters and
;;assumes a character number according to the ASCII character set.
(define (format:char->str ch)
  (let ((int-rep (char->integer ch)))
    (when (< int-rep 0) ; if chars are [-128...+127]
      (increment! int-rep 256))
    (string-append
     "#\\"
     (cond
      ((char=? ch #\newline)
       "newline")
      ((and (>= int-rep 0) (<= int-rep 32))
       (vector-ref format:ascii-non-printable-charnames int-rep))
      ((= int-rep 127)
       "del")
      ((>= int-rep 128) ; octal representation
       (number->string int-rep 8))
      (else
       (string ch))))))


;;;; helpers, integer numbers to string

;;Print a padded integer number.
(define (format:out-num-padded modifier number pars radix)
  (when (not (integer? number))
    (assertion-violation 'format:out-num-padded
      "number argument not an integer"
      number))
  ;;We need  the STRING-DOWNCASE because Ikarus converts  hex numbers to
  ;;string in uppercase, but the specs for "~x" are to yield a lowercase
  ;;number.
  (let ((numstr (string-downcase (number->string/radix number radix))))
    (if (and (null? pars) (not modifier))
	(format:out-str numstr)
      (let ((l		(length pars))
	    (numstr-len (string-length numstr)))
	(let ((mincol		(format:par pars l 0 #f "mincol"))
	      (padchar		(integer->char
				 (format:par pars l 1 format:space-ch #f)))
	      (commachar	(integer->char
				 (format:par pars l 2 (char->integer #\,) #f)))
	      (commawidth	(format:par pars l 3 3 "commawidth")))
	  (when mincol
	    (let ((numlen numstr-len)) ; calc. the output len of number
	      (when (and (memq modifier '(at colon-at))
			 (>= number 0))
		(set! numlen (+ numlen 1)))
	      (when (memq modifier '(colon colon-at))
		(set! numlen (+ (quotient (- numstr-len
					     (if (< number 0) 2 1))
					  commawidth)
				numlen)))
	      (when (> mincol numlen)
		(format:out-fill (- mincol numlen) padchar))))
	  (when (and (memq modifier '(at colon-at))
		     (>= number 0))
	    (format:out-char #\+))
	  (if (memq modifier '(colon colon-at)) ; insert comma character
	      (let ((start (remainder numstr-len commawidth))
		    (ns (if (< number 0) 1 0)))
		(format:out-substr numstr 0 start)
		(do ((i start (+ i commawidth)))
		    ((>= i numstr-len))
		  (when (> i ns)
		    (format:out-char commachar))
		  (format:out-substr numstr i (+ i commawidth))))
	    (format:out-str numstr)))))))


;;;; helpers, integer numbers to roman string

;;Return  the old  roman string  representation of  a  strictly positive
;;integer number.
(define (format:num->old-roman n)
  (unless (and (integer? n) (>= n 1))
    (error 'format:num->old-roman
      "only strictly positive integers can be romanized" n))

  (let loop ((n n)
	     (romans format:roman-alist)
	     (s '()))
    (if (null? romans)
	(list->string (reverse s))
      (let ((roman-val (caar romans))
	    (roman-dgt (cadar romans)))
	(do ((q (quotient n roman-val) (- q 1))
	     (s s (cons roman-dgt s)))
	    ((= q 0)
	     (loop (remainder n roman-val) (cdr romans) s)))))))

;;Return the roman string representation of a positive integer number.
(define (format:num->roman n)
  (unless (and (integer? n) (> n 0))
    (error 'format:num->roman
      "only positive integers can be romanized" n))

  (let loop ((n n)
	     (romans format:roman-alist)
	     (boundaries format:roman-boundary-values)
	     (s '()))
    (if (null? romans)
	(list->string (reverse s))
      (let ((roman-val (caar romans))
	    (roman-dgt (cadar romans))
	    (bdry (car boundaries)))
	(let loop2 ((q (quotient n roman-val))
		    (r (remainder n roman-val))
		    (s s))
	  (if (= q 0)
	      (if (and bdry (>= r (- roman-val bdry)))
		  (loop (remainder r bdry) (cdr romans)
			(cdr boundaries)
			(cons roman-dgt
			      (append
			       (cdr (assv bdry romans))
			       s)))
		(loop r (cdr romans) (cdr boundaries) s))
	    (loop2 (- q 1) r (cons roman-dgt s))))))))



;;;; helpers, integer numbers to word strings

;;This  procedure  is inspired  by  the  Bruno  Haible's CLisp  function
;;FORMAT-SMALL-CARDINAL, which  converts numbers in the range  1 to 999,
;;and is used for converting each thousand-block in a larger number
(define (format:num->cardinal999 n)
  (let* ((hundreds	(quotient  n 100))
	 (tens+ones	(remainder n 100))
	 (tens		(quotient  tens+ones 10))
	 (ones		(remainder tens+ones 10)))
    (append
     (if (> hundreds 0)
	 (append
	  (string->list
	   (list-ref format:cardinal-ones-list hundreds))
	  (string->list" hundred")
	  (if (> tens+ones 0) '(#\space) '()))
       '())
     (if (< tens+ones 20)
	 (if (> tens+ones 0)
	     (string->list
	      (list-ref format:cardinal-ones-list tens+ones))
	   '())
       (append
	(string->list
	 (list-ref format:cardinal-tens-list tens))
	(if (> ones 0)
	    (cons #\-
		  (string->list
		   (list-ref format:cardinal-ones-list ones)))
	  '()))))))

;;Return the  string representation of an integer  number using cardinal
;;words.
(define (format:num->cardinal n)
  (cond
   ((not (integer? n))
    (error 'format:num->cardinal
	"only integers can be converted to English cardinals"
	n))
   ((= n 0)
    "zero")
   ((< n 0)
    (string-append "minus " (format:num->cardinal (- n))))
   (else
    (let ((power3-word-limit (length format:cardinal-thousand-block-list)))
      (let loop ((n n)
		 (power3 0)
		 (s '()))
	(if (= n 0)
	    (list->string s)
	  (let ((n-before-block (quotient  n 1000))
		(n-after-block  (remainder n 1000)))
	    (loop n-before-block
		  (+ power3 1)
		  (if (> n-after-block 0)
		      (append
		       (if (> n-before-block 0)
			   (string->list ", ") '())
		       (format:num->cardinal999 n-after-block)
		       (if (< power3 power3-word-limit)
			   (string->list
			    (list-ref
			     format:cardinal-thousand-block-list
			     power3))
			 (append
			  (string->list " times ten to the ")
			  (string->list
			   (format:num->ordinal
			    (* power3 3)))
			  (string->list " power")))
		       s)
		    s)))))))))

;;Return the  string representation of  an integer number  using ordinal
;;words.
(define (format:num->ordinal n)
  (cond
   ((not (integer? n))
    (error 'format:num->ordinal
	"only integers can be converted to English ordinals"
	n))
   ((= n 0)
    "zeroth")
   ((< n 0)
    (string-append "minus " (format:num->ordinal (- n))))
   (else
    (let ((hundreds	(quotient  n 100))
	  (tens+ones	(remainder n 100)))
      (string-append
       (if (> hundreds 0)
	   (string-append
	    (format:num->cardinal (* hundreds 100))
	    (if (= tens+ones 0) "th" " "))
	 "")
       (if (= tens+ones 0) ""
	 (if (< tens+ones 20)
	     (list-ref format:ordinal-ones-list tens+ones)
	   (let ((tens (quotient tens+ones 10))
		 (ones (remainder tens+ones 10)))
	     (if (= ones 0)
		 (list-ref format:ordinal-tens-list tens)
	       (string-append
		(list-ref format:cardinal-tens-list tens)
		"-"
		(list-ref format:ordinal-ones-list ones)))))))))))



;;;; helpers, special numbers

;;Print the string representation of infinity and not-a-number.
;;
;;Infinity  and not-a-number  are  always printed  exactly as  "+inf.0",
;;"-inf.0" or "+nan.0", suitably justified in their field.  We insist on
;;printing this exact form so that the numbers can be read back in.
(define (format:out-inf-nan number width digits edigits overch padch)
  (let* ((str		(number->string number))
	 (len		(string-length str))
	 (dot		(string-index str #\.))
	 (digits	(+ (or digits 0)
			   (if edigits (+ edigits 2) 0))))
    (if (and width overch (< width len))
	(format:out-fill width (integer->char overch))
      (let* ((leftpad (if width
			  (max (- width (max len (+ dot 1 digits))) 0)
			0))
	     (rightpad (if width
			   (max (- width leftpad len) 0)
			 0))
	     (padch (integer->char (or padch format:space-ch))))
	(format:out-fill leftpad padch)
	(format:out-str str)
	(format:out-fill rightpad padch)))))



;;;; helpers, miscellaneous functions for floating point numbers

;;See the documentation of  FORMAT:PARSE-FLOAT below for more details on
;;flonums handling.

;;Return  an  integer  number  representing  the current  value  of  the
;;exponent buffer.
(define (format:en-int)
  (if (= format:en-len 0)
      0
    (do ((i 0 (+ i 1))
	 (n 0))
	((= i format:en-len)
	 (if format:en-pos?
	     n
	   (- n)))
      (set! n (+ (* n 10)
		 (- (char->integer (string-ref format:en-str i))
		    format:zero-ch))))))

;;Store an integer number into the exponent buffer FORMAT:EN-STR, update
;;FORMAT:EN-LEN and FORMAT:EN-POS?  accordingly.
(define (format:en-set en)
  (unless (integer? en)
    (error 'format:en-set
      "invalid value for floating point number exponent"
      en))
  (set! format:en-len 0)
  (set! format:en-pos? (>= en 0))
  (let* ((en-str (number->string en))
	 (en-len (string-length en-str)))
    (do ((i 0 (+ i 1)))
	((= i en-len))
      (let ((ch (string-ref en-str i)))
	(when (char-numeric? ch)
	  (string-set! format:en-str format:en-len ch)
	  (increment! format:en-len 1))))))

;;Fill the mantissa buffer with zeros, update FORMAT:FN-LEN accordingly.
;;Examples:
;;
;;  (set! format:fn-str "123")
;;  (format:fn-zfill #t 3)
;;  format:fn-str => "000123"
;;
;;  (set! format:fn-str "123")
;;  (format:fn-zfill #f 3)
;;  format:fn-str => "123000"
;;
(define (format:fn-zfill left? n)
  (when (> (+ n format:fn-len) format:fn-max) ; from the left or right
    (error 'format:fn-zfill
      "number is too long to format (enlarge format:fn-max)"))
  (increment! format:fn-len n)
  (if left?
      (do ((i format:fn-len (- i 1))) ; fill n 0s to left
	  ((< i 0))
	(string-set! format:fn-str i
		     (if (< i n)
			 #\0
		       (string-ref format:fn-str (- i n)))))
    (do ((i (- format:fn-len n) (+ i 1))) ; fill n 0s to the right
	((= i format:fn-len))
      (string-set! format:fn-str i #\0))))

;;Shift  left  current  by  N  positions  the  mantissa  buffer,  update
;;FORMAT:FN-LEN accordingly.   It is used  to remove leading  zeros from
;;the mantissa buffer:
;;
;;  "000123" -> "123"
;;
(define (format:fn-shiftleft n)
  (when (> n format:fn-len)
    (error 'format:fn-shiftleft
      "internal error in format:fn-shiftleft"
      n format:fn-len))
  (do ((i n (+ i 1)))
      ((= i format:fn-len)
       (increment! format:fn-len (- n)))
    (string-set! format:fn-str (- i n) (string-ref format:fn-str i))))

;;Round the current value of FORMAT:FN-STR to the number of digits after
;;the dot selected by DIGITS.
;;
;;We assume  that there are at least  DIGITS+FORMAT:FN-DOT characters in
;;the mantissa buffer.
;;
;;We want  the same behaviour requested  by R6RS for ROUND,  and by IEEE
;;754 for rounding to nearest.  When rounding an in-the-middle digit: we
;;round it to even.
;;
;;Examples of carry normalisation:
;;
;;  "0.9" -> "1.0"
;;  "9.9" -> "10.0"
;;
(define (format:fn-round digits)
  (increment! digits format:fn-dot)
  (do ((i digits (- i 1))
       (carry 5))
      ((or (= carry 0) (< i 0))
       (set! format:fn-len digits)
       (when (= carry 1)
	 ;;This  body  prepends  a   "1"  to  the  mantissa  buffer  and
	 ;;increments the dot position.   This way it performs the carry
	 ;;normalisation for the roundings like:
	 ;;
	 ;;	"9.9" -> "10.0"
	 ;;
	 (format:fn-zfill #t 1)
	 (string-set! format:fn-str 0 #\1)
	 (increment! format:fn-dot 1)))
    (set! carry (+ (- (char->integer (string-ref format:fn-str i))
		      format:zero-ch)
		   carry))
    (string-set! format:fn-str i (integer->char
				  (if (< carry 10)
				      (+ carry format:zero-ch)
				    (+ (- carry 10)
				       format:zero-ch))))
    (set! carry (if (< carry 10) 0 1))))

;;Print to the destination the mantissa part of the number.
;;
;;If MODIFIER is true: the plus sign is output.
;;
;;If ADD-LEADING-ZERO? is true: a  leading zero is output if the integer
;;part of the mantissa is zero ("0.123"), else it is not (".123").
;;
(define (format:fn-out modifier add-leading-zero?)
  (if format:fn-pos?
      (when (eq? modifier 'at)
	(format:out-char #\+))
    (format:out-char #\-))
  (if (= format:fn-dot 0)
      (when add-leading-zero?
	(format:out-char #\0))
    (format:out-substr format:fn-str 0 format:fn-dot));integer part
  (format:out-char #\.)
  (format:out-substr format:fn-str format:fn-dot format:fn-len));fractional part

;;Print to the destination  the exponent-start character followed by the
;;exponent string.
;;
;;The EDIGITS argument is the requested minimum width of the digits part
;;of  the  exponent, exponent-start  char  and  sign  excluded.  If  the
;;current string in FORMAT:EN-STR is shorter than EDIGITS: padding zeros
;;are  output before  the digits.   If EDIGITS  is #f:  the  exponent is
;;output without padding.
;;
;;The EXPCH argument selects the exponent-start character.  It should be
;;#\e  or  #\E,  but  can   be  #f  to  select  FORMAT:EXPCH,  which  is
;;configurable at library installation time.
;;
(define (format:en-out edigits expch)
  (format:out-char (if expch (integer->char expch) format:expch))
  (format:out-char (if format:en-pos? #\+ #\-))
  (when edigits
    (when (< format:en-len edigits)
      (format:out-fill (- edigits format:en-len) #\0)))
  (format:out-substr format:en-str 0 format:en-len))

;;Strip trailing zeros  but one from the mantissa  buffer.  The mantissa
;;buffer is "updated" by mutating the value in FORMAT:FN-LEN.  Examples:
;;
;;  (set! format:fn-dot 1)
;;  (set! format:fn-len 6)
;;  (set! format:fn-str "123000")
;;  (format:fn-strip)
;;  format:fn-len => 4
;;
;;  (set! format:fn-dot 4)
;;  (set! format:fn-len 6)
;;  (set! format:fn-str "123000")
;;  (format:fn-strip)
;;  format:fn-len => 5
;;
(define (format:fn-strip)
  (string-set! format:fn-str format:fn-len #\0)
  (do ((i format:fn-len (- i 1)))
      ((or (not (char=? #\0 (string-ref format:fn-str i)))
	   (<= i format:fn-dot))
       (set! format:fn-len (+ i 1)))))

;;Count  leading zeros  in  the  mantissa buffer.  Examples:
;;
;;  (set! format:fn-str "000123")
;;  (format:fn-count-leading-zeros) => 3
;;
;;  (set! format:fn-str "0")
;;  (format:fn-count-leading-zeros) => 0
;;
;;Return zero if the mantissa is actually zero, that is "0".
(define (format:fn-count-leading-zeros)
  (do ((i 0 (+ i 1)))
      ((or (= i format:fn-len)
	   (not (char=? #\0 (string-ref format:fn-str i))))
       (if (= i format:fn-len)
	   0
	 i))))


;;;; helpers, parsing of floating point numbers

;;Parse  the floating  point  number string  representation in  NUM-STR,
;;filling the FORMAT:FN-* and FORMAT:EN-* variables with the result.
;;
;;The string rep in NUM-STR is expected to be one of the following:
;;
;;  "12"		"+12"		"-12"
;;  "12.345"		"+12.345"	"-12.345"
;;  "12.345e67"		"+12.345e67"	"-12.345e67"
;;  "12.345E67"		"+12.345E67"	"-12.345E67"
;;  "12.345e-67"	"+12.345e-67"	"-12.345e-67"
;;  "12.345E-67"	"+12.345E-67"	"-12.345E-67"
;;
;;everything  before the  'e'  or  'E' char  is  called "mantissa",  and
;;everything   after  is   called  "exponent".    We  accept   a  string
;;representation that starts with "#d"  (which is the prefix for decimal
;;representations).
;;
;;Notice that the  integer part of the mantissa may  be missing, that is
;;".123" is a valid string rep for "0.123".
;;
;;Notice that the  fractional part of the mantissa  may be missing, that
;;is "12." is a valid string rep for "12.0".
;;
(define (format:parse-float num-str fixed? scale)
  (set! format:fn-pos?	#t)
  (set! format:fn-len	0)
  (set! format:fn-dot	#f)
  (set! format:en-pos?	#t)
  (set! format:en-len	0)

  (when (string-prefix? "#d" num-str)
    (set! num-str (substring num-str 2 (string-length num-str))))

  (let (
	;;This is #t while parsing  the mantissa, and becomes #f if/when
	;;the exponential is found.
	(mantissa?		#t)

	;;Once  the first  char of  the  mantissa or  exponent has  been
	;;parsed, some  characters are  no more allowed.   The following
	;;variables detect this.
	(mantissa-started?	#f)
	(exponent-started?	#f)

	;;Set to  #t if  all the  digits in the  mantissa are  zeros, #f
	;;otherwise.  It is used to detect a true zero like "0.0000".
	(all-zeros?		#t)

	;;The number of zeros at the beginning of the mantissa buffer.
	(left-zeros		0)

	(num-len		(string-length num-str))

	(error-message		"invalid character in number string representation"))

    (do ((i 0 (+ i 1)))
	((= i num-len)
	 (when (not format:fn-dot)
	   (set! format:fn-dot format:fn-len))
	 (when all-zeros?
	   (set! left-zeros    0)
	   (set! format:fn-dot 0)
	   (set! format:fn-len 1))

	 ;;Now format the parsed values according to FORMAT's need.
	 (if fixed?

	     (begin
	       ;;Fixed format "m.nnn" or ".nnn".
	       (when (and (> left-zeros 0)
			  (> format:fn-dot 0))
		 (if (> format:fn-dot left-zeros)
		     (begin ; norm 0{0}nn.mm to nn.mm
		       (format:fn-shiftleft left-zeros)
		       (increment! format:fn-dot (- left-zeros))
		       (set! left-zeros 0))
		   (begin ; normalize 0{0}.nnn to .nnn
		     (format:fn-shiftleft format:fn-dot)
		     (increment! left-zeros (- format:fn-dot))
		     (set! format:fn-dot 0))))
	       (when (or (not (= scale 0)) (> format:en-len 0))
		 (let ((shift (+ scale (format:en-int))))
		   (cond
		    (all-zeros? #t)
		    ((> (+ format:fn-dot shift) format:fn-len)
		     (format:fn-zfill
		      #f (- shift (- format:fn-len format:fn-dot)))
		     (set! format:fn-dot format:fn-len))
		    ((< (+ format:fn-dot shift) 0)
		     (format:fn-zfill #t (- (- shift) format:fn-dot))
		     (set! format:fn-dot 0))
		    (else
		     (if (> left-zeros 0)
			 (if (<= left-zeros shift) ; shift always > 0 here
			     (format:fn-shiftleft shift) ; shift out 0s
			   (begin
			     (format:fn-shiftleft left-zeros)
			     (set! format:fn-dot (- shift left-zeros))))
		       (set! format:fn-dot (+ format:fn-dot shift))))))))

	   ;;Exponential format "m.nnnEee".
	   (let ((negexp (if (> left-zeros 0)
			     (- left-zeros format:fn-dot -1)
			   (if (= format:fn-dot 0) 1 0))))
	     (if (> left-zeros 0)
		 (begin ; normalize 0{0}.nnn to n.nn
		   (format:fn-shiftleft left-zeros)
		   (set! format:fn-dot 1))
	       (when (= format:fn-dot 0)
		 (set! format:fn-dot 1)))
	     (format:en-set (- (+ (- format:fn-dot scale)
				  (format:en-int))
			       negexp))
	     (cond
	      (all-zeros?
	       (format:en-set 0)
	       (set! format:fn-dot 1))
	      ((< scale 0) ; leading zero
	       (format:fn-zfill #t (- scale))
	       (set! format:fn-dot 0))
	      ((> scale format:fn-dot)
	       (format:fn-zfill #f (- scale format:fn-dot))
	       (set! format:fn-dot scale))
	      (else
	       (set! format:fn-dot scale)))))
	 #t)

      ;;DO body.
      (let ((ch (string-ref num-str i)))
	(cond

	 ((char-numeric? ch)
	  ;;Store the numeric char in the mantissa buffer FORMAT:FN-STR,
	  ;;or   in  the   exponential  buffer   FORMAT:EN-STR.   Update
	  ;;FORMAT:FN-LEN or FORMAT:EN-LEN accordingly.
	  (if mantissa?
	      (begin
		(set! mantissa-started? #t)
		(if (char=? ch #\0)
		    (when all-zeros?
		      (increment! left-zeros 1))
		  (set! all-zeros? #f))
		(string-set! format:fn-str format:fn-len ch)
		(increment! format:fn-len 1))
	    (begin
	      (set! exponent-started? #t)
	      (string-set! format:en-str format:en-len ch)
	      (increment! format:en-len 1))))

	 ((or (char=? ch #\-) (char=? ch #\+))
	  ;;Record the sign of the mantissa or exponent.  Raise an error
	  ;;if  the  sign  comes  inside  the  mantissa  or  inside  the
	  ;;exponent.
	  (if mantissa?
	      (if mantissa-started?
		  (error 'format:parse-float error-message num-str)
		(begin
		  (set! format:fn-pos? (char=? ch #\+))
		  (set! mantissa-started? #t)))
	    (if exponent-started?
		(error 'format:parse-float error-message num-str)
	      (begin
		(set! format:en-pos? (char=? ch #\+))
		(set! exponent-started? #t)))))

	 ((char=? ch #\.)
	  ;;Record the  index of  the first digit  after the dot  in the
	  ;;mantissa buffer.  Raise  an error if the dot  is found twice
	  ;;or if we are not parsing the mantissa.
	  (when (or format:fn-dot (not mantissa?))
	    (error 'format:parse-float error-message num-str))
	  (set! format:fn-dot format:fn-len))

	 ((or (char=? ch #\e) (char=? ch #\E))
	  ;;Record the end of mantissa  and start of exponent.  Raise an
	  ;;error if we are already parsing the exponent.
	  (unless mantissa?
	    (error 'format:parse-float error-message num-str))
	  (set! mantissa? #f))

	 (else
	  (error 'format:parse-float error-message ch)))))))



;;;; helpers, floating point numbers: fixed-point format

;;Print the  fixed point string representation  of a number.   It is the
;;implementation of the "~f" escape sequence.
(define (format:out-fixed modifier number parameters)
  (when (not (or (and (number? number)
		      (real? number))
		 (string? number)))
    (error 'format:out-fixed
      "argument is not a real number or a number string"
      number))

  (let ((l (length parameters)))
    (let ((width	(format:par parameters l 0 #f "width"))
	  (digits	(format:par parameters l 1 #f "digits"))
	  (scale	(format:par parameters l 2 0  #f))
	  (overch	(format:par parameters l 3 #f #f))
	  (padch	(format:par parameters l 4 format:space-ch #f)))

      (cond

       ((and (not (string? number))
	     (or (infinite? number) (nan? number)))
	(format:out-inf-nan number width digits #f overch padch))

       (digits
	;;The  call  to FORMAT:PARSE-FLOAT  updates  the internal  state
	;;variables "format:fn-*".
	(format:parse-float (if (string? number)
				number
			      (number->string (inexact number)))
			    #t scale)
	(if (<= (- format:fn-len format:fn-dot) digits)
	    (format:fn-zfill #f (- digits (- format:fn-len format:fn-dot)))
	  (format:fn-round digits))
	(if width
	    (let ((numlen (+ format:fn-len 1)))
	      (when (or (not format:fn-pos?) (eq? modifier 'at))
		(increment! numlen 1))
	      (when (and (= format:fn-dot 0) (> width (+ digits 1)))
		(increment! numlen 1))
	      (when (< numlen width)
		(format:out-fill (- width numlen) (integer->char padch)))
	      (if (and overch (> numlen width))
		  (format:out-fill width (integer->char overch))
		(format:fn-out modifier (> width (+ digits 1)))))
	  (format:fn-out modifier #t)))

       (else
	;;The  call  to FORMAT:PARSE-FLOAT  updates  the internal  state
	;;variables "format:fn-*".
	(format:parse-float (if (string? number)
				number
			      (number->string (inexact number)))
			    #t scale)
	(format:fn-strip)
	(if width
	    (let ((numlen (+ format:fn-len 1)))
	      (when (or (not format:fn-pos?) (eq? modifier 'at))
		(set! numlen (+ numlen 1)))
	      (when (= format:fn-dot 0)
		(set! numlen (+ numlen 1)))
	      (when (< numlen width)
		(format:out-fill (- width numlen) (integer->char padch)))
	      (if (> numlen width) ; adjust precision if possible
		  (let ((dot-index (- numlen
				      (- format:fn-len format:fn-dot))))
		    (if (> dot-index width)
			(if overch ; numstr too big for required width
			    (format:out-fill width (integer->char overch))
			  (format:fn-out modifier #t))
		      (begin
			(format:fn-round (- width dot-index))
			(format:fn-out modifier #t))))
		(format:fn-out modifier #t)))
	  (format:fn-out modifier #t)))))))


;;;; helpers, floating point numbers: exponential format

;;Print the  exponential string representation  of a number.  It  is the
;;implementation of the "~e" escape sequence.
(define (format:out-expon modifier number parameters)
  (when (not (or (and (number? number)
		      (real? number))
		 (string? number)))
    (error 'format:out-expon
      "argument is not a number"
      number))

  (let ((l (length parameters)))
    (let ((width	(format:par parameters l 0 #f "width"))
	  (digits	(format:par parameters l 1 #f "digits"))
	  (edigits	(format:par parameters l 2 #f "exponent digits"))
	  (scale	(format:par parameters l 3 1  #f))
	  (overch	(format:par parameters l 4 #f #f))
	  (padch	(format:par parameters l 5 format:space-ch #f))
	  (expch	(format:par parameters l 6 #f #f)))

      (cond

       ((and (not (string? number))
	     (or (infinite? number) (nan? number)))
	(format:out-inf-nan number width digits edigits overch padch))

       (digits	; fixed precision

	(let ((digits (if (> scale 0)
			  (if (< scale (+ digits 2))
			      (+ (- digits scale) 1)
			    0)
			digits)))
	  ;;The  call to FORMAT:PARSE-FLOAT  updates the  internal state
	  ;;variables "format:fn-*".
	  (format:parse-float (if (string? number)
				  number
				(number->string number))
			      #f scale)
	  (if (<= (- format:fn-len format:fn-dot) digits)
	      (format:fn-zfill #f (- digits (- format:fn-len format:fn-dot)))
	    (format:fn-round digits))
	  (if width
	      (if (and edigits overch (> format:en-len edigits))
		  (format:out-fill width (integer->char overch))
		(let ((numlen (+ format:fn-len 3))) ; .E+
		  (when (or (not format:fn-pos?) (eq? modifier 'at))
		    (set! numlen (+ numlen 1)))
		  (when (and (= format:fn-dot 0) (> width (+ digits 1)))
		    (set! numlen (+ numlen 1)))
		  (set! numlen
			(+ numlen
			   (if (and edigits (>= edigits format:en-len))
			       edigits
			     format:en-len)))
		  (when (< numlen width)
		    (format:out-fill (- width numlen) (integer->char padch)))
		  (if (and overch (> numlen width))
		      (format:out-fill width (integer->char overch))
		    (begin
		      (format:fn-out modifier (> width (- numlen 1)))
		      (format:en-out edigits expch)))))
	    (begin
	      (format:fn-out modifier #t)
	      (format:en-out edigits expch)))))

       (else
	;;The  call  to FORMAT:PARSE-FLOAT  updates  the internal  state
	;;variables "format:fn-*".
	(format:parse-float (if (string? number)
				number
			      (number->string number))
			    #f scale)
	(format:fn-strip)
	(if width
	    (if (and edigits overch (> format:en-len edigits))
		(format:out-fill width (integer->char overch))
	      (let ((numlen (+ format:fn-len 3))) ; .E+
		(when (or (not format:fn-pos?) (eq? modifier 'at))
		  (increment! numlen 1))
		(when (= format:fn-dot 0)
		  (increment! numlen 1))
		(set! numlen
		      (+ numlen
			 (if (and edigits (>= edigits format:en-len))
			     edigits
			   format:en-len)))
		(when (< numlen width)
		  (format:out-fill (- width numlen) (integer->char padch)))
		(if (> numlen width) ; adjust precision if possible
		    (let ((f (- format:fn-len format:fn-dot))) ; fract len
		      (if (> (- numlen f) width)
			  (if overch ; numstr too big for required width
			      (format:out-fill width
					       (integer->char overch))
			    (begin
			      (format:fn-out modifier #t)
			      (format:en-out edigits expch)))
			(begin
			  (format:fn-round (+ (- f numlen) width))
			  (format:fn-out modifier #t)
			  (format:en-out edigits expch))))
		  (begin
		    (format:fn-out modifier #t)
		    (format:en-out edigits expch)))))
	  (begin
	    (format:fn-out modifier #t)
	    (format:en-out edigits expch))))))))



;;;; helpers, floating point numbers: general format

;;Print  the general  string  representation  of a  number.   It is  the
;;implementation of  the "~g"  escape sequence.
(define (format:out-general modifier number parameters)
  (when (not (or (and (number? number)
		      (real? number))
		 (string? number)))
    (error 'format:out-general
      "argument is not a number or a number string"
      number))

  (let ((l (length parameters)))
    (let ((width	(if (> l 0) (list-ref parameters 0) #f))
	  (digits	(if (> l 1) (list-ref parameters 1) #f))
	  (edigits	(if (> l 2) (list-ref parameters 2) #f))
	  (overch	(if (> l 4) (list-ref parameters 4) #f))
	  (padch	(if (> l 5) (list-ref parameters 5) #f)))
      (cond

       ((and (not (string? number))
	     (or (infinite? number) (nan? number)))
	;;FIXME: this isn't right.  (But why? MarcoMaggi)
	(format:out-inf-nan number width digits edigits overch padch))

       (else
	;;The  call  to FORMAT:PARSE-FLOAT  updates  the internal  state
	;;variables "format:fn-*".
	(format:parse-float (if (string? number)
				number
			      (number->string number))
			    #t 0)
	(format:fn-strip)
	;;For  the following algorithm  see Steele's  CL book  page 395.
	;;NUMBER less than (abs 1.0) ?
	(let* ((ee	(if edigits (+ edigits 2) 4))
	       (ww	(if width (- width ee) #f))
	       (n	(if (= format:fn-dot 0)
			    (- (format:fn-count-leading-zeros))
			  format:fn-dot))
	       (d	(if digits
			    digits
			  (max format:fn-len (min n 7)))) ; q = format:fn-len
	       (dd	(- d n)))
	  (if (<= 0 dd d)
	      (begin
		(format:out-fixed modifier number (list ww dd #f overch padch))
		(format:out-fill ee #\space)) ;~@T not implemented yet
	    (format:out-expon modifier number parameters))))))))



;;;; helpers, floating point numbers: dollar format

;;Print  the  dollar string  representation  of  a  number.  It  is  the
;;implementation of the "~$" escape sequence.
(define (format:out-dollar modifier number parameters)
  (when (not (or (number? number) (string? number)))
    (error 'format:out-dollar
      "argument is not a number or a number string"
      number))

  (let ((l (length parameters)))
    (let ((digits	(format:par parameters l 0 2 "digits"))
	  (mindig	(format:par parameters l 1 1 "mindig"))
	  (width	(format:par parameters l 2 0 "width"))
	  (padch	(format:par parameters l 3 format:space-ch #f)))

      (cond
       ((or (infinite? number) (nan? number))
	(format:out-inf-nan number width digits #f #f padch))

       (else
	;;The  call  to FORMAT:PARSE-FLOAT  updates  the internal  state
	;;variables "format:fn-*".
	(format:parse-float (if (string? number)
				number
			      (number->string number))
			    #t 0)
	(if (<= (- format:fn-len format:fn-dot) digits)
	    (format:fn-zfill #f (- digits (- format:fn-len format:fn-dot)))
	  (format:fn-round digits))
	(let ((numlen (+ format:fn-len 1)))
	  (when (or (not format:fn-pos?) (memq modifier '(at colon-at)))
	    (increment! numlen 1))
	  (when (and mindig (> mindig format:fn-dot))
	    (increment! numlen (- mindig format:fn-dot)))
	  (when (and (= format:fn-dot 0) (not mindig))
	    (increment! numlen 1))
	  (if (< numlen width)
	      (case modifier
		((colon)
		 (if (not format:fn-pos?)
		     (format:out-char #\-))
		 (format:out-fill (- width numlen) (integer->char padch)))
		((at)
		 (format:out-fill (- width numlen) (integer->char padch))
		 (format:out-char (if format:fn-pos? #\+ #\-)))
		((colon-at)
		 (format:out-char (if format:fn-pos? #\+ #\-))
		 (format:out-fill (- width numlen) (integer->char padch)))
		(else
		 (format:out-fill (- width numlen) (integer->char padch))
		 (if (not format:fn-pos?)
		     (format:out-char #\-))))
	    (if format:fn-pos?
		(if (memq modifier '(at colon-at)) (format:out-char #\+))
	      (format:out-char #\-))))
	(when (and mindig (> mindig format:fn-dot))
	  (format:out-fill (- mindig format:fn-dot) #\0))
	(when (and (= format:fn-dot 0) (not mindig))
	  (format:out-char #\0))
	(format:out-substr format:fn-str 0 format:fn-dot)
	(format:out-char #\.)
	(format:out-substr format:fn-str format:fn-dot format:fn-len))))))



;;;; helpers, tabulation

(define (format:tabulate modifier parameters)
  (let ((l (length parameters)))
    (let ((colnum	(format:par parameters l 0 1 "colnum"))
	  (padinc	(format:par parameters l 1 1 "padinc"))
	  (padch	(integer->char
			 (format:par parameters l 2 format:space-ch #f))))
      (case modifier
	((colon colon-at)
	 (error 'format:tabulate
	   "unsupported modifier for escape sequence ~t"
	   modifier))
	((at)	; relative tabulation
	 (format:out-fill
	  (if (= padinc 0)
	      colnum ; colnum = colrel
	    (do ((c 0 (+ c padinc))
		 (col (+ format:output-col colnum)))
		((>= c col)
		 (- c format:output-col))))
	  padch))
	(else	; absolute tabulation
	 (format:out-fill
	  (cond
	   ((< format:output-col colnum)
	    (- colnum format:output-col))
	   ((= padinc 0)
	    0)
	   (else
	    (do ((c colnum (+ c padinc)))
		((>= c format:output-col)
		 (- c format:output-col)))))
	  padch))))))


;;;; actual formatting

(define (format:format-work format-string arglist)
  (letrec
      ((format-string-len (string-length format-string))
       (arg-pos 0)		  ;argument position in arglist
       (arg-len (length arglist)) ;number of arguments
       (modifier #f)		  ;'colon | 'at | 'colon-at | #f
       (params '())		  ;directive parameter list
       (param-value-found #f)	  ;a directive parameter value found
       (conditional-nest 0)	  ;conditional nesting level
       (clause-pos 0)		  ;last cond. clause beginning char pos
       (clause-default #f)	  ;conditional default clause string
       (clauses '())		  ;conditional clause string list
       (conditional-type #f)	  ;reflects the contional modifiers
       (conditional-arg #f)	  ;argument to apply the conditional
       (iteration-nest 0)	  ;iteration nesting level
       (iteration-pos 0)	  ;iteration string beginning char pos
       (iteration-type #f)	  ;reflects the iteration modifiers
       (max-iterations #f)	  ;maximum number of iterations
       (recursive-pos-save format:pos))

    ;;Gets the next char from FORMAT-STRING.
    (define (next-char)
      (let ((ch (peek-next-char)))
	(set! format:pos (+ 1 format:pos))
	ch))

    (define (peek-next-char)
      (if (>= format:pos format-string-len)
	  (error "illegal format string")
	(string-ref format-string format:pos)))

    (define (one-positive-integer? params)
      (cond
       ((null? params)
	#f)
       ((and (integer? (car params))
	     (>= (car params) 0)
	     (= (length params) 1))
	#t)
       (else
	(error 'format:format-work
	  "one positive integer parameter expected"))))

    (define (next-arg)
      (when (>= arg-pos arg-len)
	(set! format:arg-pos (+ arg-len 1))
	(error 'format:format-work
	  "missing argument(s)"))
      (add-arg-pos 1)
      (list-ref arglist (- arg-pos 1)))

    (define (prev-arg)
      (add-arg-pos -1)
      (when (negative? arg-pos)
	(error 'format:format-work
	    "missing backward argument(s)"))
      (list-ref arglist arg-pos))

    (define (rest-args)
      (let loop ((l arglist) (k arg-pos)) ; list-tail definition
	(if (= k 0)
	    l
	  (loop (cdr l) (- k 1)))))

    (define (add-arg-pos n)
      (set! arg-pos (+ n arg-pos))
      (set! format:arg-pos arg-pos))

    ;;Dispatches the format-string.
    (define (anychar-dispatch)
      (if (>= format:pos format-string-len)
	  arg-pos ; used for ~? continuance
	(let ((char (next-char)))
	  (cond
	   ((char=? char #\~)
	    (set! modifier #f)
	    (set! params '())
	    (set! param-value-found #f)
	    (tilde-dispatch))
	   (else
	    (if (and (zero? conditional-nest)
		     (zero? iteration-nest))
		(format:out-char char))
	    (anychar-dispatch))))))

    (define (tilde-dispatch)
      (cond
       ((>= format:pos format-string-len)
	(format:out-str "~") ; tilde at end of
		; string is just
		; output
	arg-pos) ; used for ~?
		; continuance
       ((and (or (zero? conditional-nest)
		 (memv (peek-next-char) ; find conditional
		; directives
		       (append '(#\[ #\] #\; #\: #\@ #\^)
			       format:parameter-characters)))
	     (or (zero? iteration-nest)
		 (memv (peek-next-char) ; find iteration
		; directives
		       (append '(#\{ #\} #\: #\@ #\^)
			       format:parameter-characters))))
	(case (char-upcase (next-char))
	  ;; format directives
	  ((#\A) ; Any -- for humans
	   (set! format:read-proof
		 (memq modifier '(colon colon-at)))
	   (format:out-obj-padded (memq modifier '(at colon-at))
				  (next-arg) #f params)
	   (anychar-dispatch))
	  ((#\S) ; Slashified -- for parsers
	   (set! format:read-proof
		 (memq modifier '(colon colon-at)))
	   (format:out-obj-padded (memq modifier '(at colon-at))
				  (next-arg) #t params)
	   (anychar-dispatch))
	  ((#\D) ; Decimal
	   (format:out-num-padded modifier (next-arg) params 10)
	   (anychar-dispatch))
	  ((#\X) ; Hexadecimal
	   (format:out-num-padded modifier (next-arg) params 16)
	   (anychar-dispatch))
	  ((#\O) ; Octal
	   (format:out-num-padded modifier (next-arg) params 8)
	   (anychar-dispatch))
	  ((#\B) ; Binary
	   (format:out-num-padded modifier (next-arg) params 2)
	   (anychar-dispatch))
	  ((#\R)
	   (if (null? params)
	       (format:out-obj-padded ; Roman, cardinal, ordinal numerals
		#f
		((case modifier
		   ((at) format:num->roman)
		   ((colon-at) format:num->old-roman)
		   ((colon) format:num->ordinal)
		   (else format:num->cardinal))
		 (next-arg))
		#f params)
	     (format:out-num-padded ; any Radix
	      modifier (next-arg) (cdr params) (car params)))
	   (anychar-dispatch))
	  ((#\F) ; Fixed-format floating-point
	   (if format:floats
	       (format:out-fixed modifier (next-arg) params)
	     (format:out-str (number->string (next-arg))))
	   (anychar-dispatch))
	  ((#\E) ; Exponential floating-point
	   (if format:floats
	       (format:out-expon modifier (next-arg) params)
	     (format:out-str (number->string (next-arg))))
	   (anychar-dispatch))
	  ((#\G) ; General floating-point
	   (if format:floats
	       (format:out-general modifier (next-arg) params)
	     (format:out-str (number->string (next-arg))))
	   (anychar-dispatch))
	  ((#\$) ; Dollars floating-point
	   (if format:floats
	       (format:out-dollar modifier (next-arg) params)
	     (format:out-str (number->string (next-arg))))
	   (anychar-dispatch))
	  ((#\I) ; Complex numbers
	   (when (not format:complex-numbers)
	     (error "complex numbers not supported by this scheme system"))
	   (let ((z (next-arg)))
	     (when (not (complex? z))
	       (error "argument not a complex number"))
	     (format:out-fixed modifier (real-part z) params)
	     (format:out-fixed 'at (imag-part z) params)
	     (format:out-char #\i))
	   (anychar-dispatch))
	  ((#\C) ; Character
	   (let ((ch (if (one-positive-integer? params)
			 (integer->char (car params))
		       (next-arg))))
	     (when (not (char? ch))
	       (error "escape sequence ~c expects a character"))
	     (case modifier
	       ((at)
		(format:out-str (format:char->str ch)))
	       ((colon)
		(let ((c (char->integer ch)))
		  (if (< c 0)
		      (set! c (+ c 256))) ; compensate
		; complement
		; impl.
		  (cond
		   ((< c #x20) ; assumes that control
		; chars are < #x20
		    (format:out-char #\^)
		    (format:out-char
		     (integer->char (+ c #x40))))
		   ((>= c #x7f)
		    (format:out-str "#\\")
		    (format:out-str (number->string c 8)))
		   (else
		    (format:out-char ch)))))
	       (else (format:out-char ch))))
	   (anychar-dispatch))
	  ((#\P) ; Plural
	   (if (memq modifier '(colon colon-at))
	       (prev-arg))
	   (let ((arg (next-arg)))
	     (when (not (number? arg))
	       (error "escape sequence ~p expects a number argument"))
	     (if (= arg 1)
		 (when (memq modifier '(at colon-at))
		   (format:out-char #\y))
	       (if (memq modifier '(at colon-at))
		   (format:out-str "ies")
		 (format:out-char #\s))))
	   (anychar-dispatch))
	  ((#\~) ; Tilde
	   (if (one-positive-integer? params)
	       (format:out-fill (car params) #\~)
	     (format:out-char #\~))
	   (anychar-dispatch))
	  ((#\%) ; Newline
	   (if (one-positive-integer? params)
	       (format:out-fill (car params) #\newline)
	     (format:out-char #\newline))
	   (set! format:output-col 0)
	   (anychar-dispatch))
	  ((#\&) ; Fresh line
	   (if (one-positive-integer? params)
	       (begin
		 (if (> (car params) 0)
		     (format:out-fill (- (car params)
					 (if (>
					      format:output-col
					      0) 0 1))
				      #\newline))
		 (set! format:output-col 0))
	     (if (> format:output-col 0)
		 (format:out-char #\newline)))
	   (anychar-dispatch))
	  ((#\_) ; Space character
	   (if (one-positive-integer? params)
	       (format:out-fill (car params) #\space)
	     (format:out-char #\space))
	   (anychar-dispatch))
	  ((#\/) ; Tabulator character
	   (if (one-positive-integer? params)
	       (format:out-fill (car params) #\tab)
	     (format:out-char #\tab))
	   (anychar-dispatch))
	  ((#\|) ; Page seperator
	   (if (one-positive-integer? params)
	       (format:out-fill (car params) #\page)
	     (format:out-char #\page))
	   (set! format:output-col 0)
	   (anychar-dispatch))
	  ((#\T) ; Tabulate
	   (format:tabulate modifier params)
	   (anychar-dispatch))
	  ((#\Y) ; Pretty-print
	   (pretty-print (next-arg) format:port)
	   (set! format:output-col 0)
	   (anychar-dispatch))
	  ((#\? #\K) ; Indirection (is "~K" in T-Scheme)
	   (cond
	    ((memq modifier '(colon colon-at))
	     (error "illegal modifier in escape sequence ~?"))
	    ((eq? modifier 'at)
	     (let* ((frmt (next-arg))
		    (args (rest-args)))
	       (add-arg-pos (format:format-work frmt args))))
	    (else
	     (let* ((frmt (next-arg))
		    (args (next-arg)))
	       (format:format-work frmt args))))
	   (anychar-dispatch))
	  ((#\!) ; Flush output
	   (set! format:flush-output #t)
	   (anychar-dispatch))
	  ((#\newline) ; Continuation lines
	   (if (eq? modifier 'at)
	       (format:out-char #\newline))
	   (if (< format:pos format-string-len)
	       (do ((ch (peek-next-char) (peek-next-char)))
		   ((or (not (char-whitespace? ch))
			(= format:pos (- format-string-len 1))))
		 (if (eq? modifier 'colon)
		     (format:out-char (next-char))
		   (next-char))))
	   (anychar-dispatch))
	  ((#\*) ; Argument jumping
	   (case modifier
	     ((colon) ; jump backwards
	      (if (one-positive-integer? params)
		  (do ((i 0 (+ i 1)))
		      ((= i (car params)))
		    (prev-arg))
		(prev-arg)))
	     ((at) ; jump absolute
	      (set! arg-pos (if (one-positive-integer? params)
				(car params) 0)))
	     ((colon-at)
	      (error "illegal modifier `:@' in escape sequence ~*"))
	     (else ; jump forward
	      (if (one-positive-integer? params)
		  (do ((i 0 (+ i 1)))
		      ((= i (car params)))
		    (next-arg))
		(next-arg))))
	   (anychar-dispatch))
	  ((#\() ; Case conversion begin
	   (set! format:case-conversion
		 (case modifier
		   ((at) string-capitalize-first)
		   ((colon) string-capitalize)
		   ((colon-at) string-upcase)
		   (else string-downcase)))
	   (anychar-dispatch))
	  ((#\)) ; Case conversion end
	   (when (not format:case-conversion)
	     (error "missing escape sequence ~("))
	   (set! format:case-conversion #f)
	   (anychar-dispatch))
	  ((#\[) ; Conditional begin
	   (set! conditional-nest (+ conditional-nest 1))
	   (cond
	    ((= conditional-nest 1)
	     (set! clause-pos format:pos)
	     (set! clause-default #f)
	     (set! clauses '())
	     (set! conditional-type
		   (case modifier
		     ((at) 'if-then)
		     ((colon) 'if-else-then)
		     ((colon-at)
		      (error "illegal modifier in escape sequence ~["))
		     (else 'num-case)))
	     (set! conditional-arg
		   (if (one-positive-integer? params)
		       (car params)
		     (next-arg)))))
	   (anychar-dispatch))
	  ((#\;) ; Conditional separator
	   (when (zero? conditional-nest)
	     (error "escape sequence ~; not in ~[~] conditional"))
	   (when (not (null? params))
	     (error "no parameter allowed in ~~;"))
	   (when (= conditional-nest 1)
	     (let ((clause-str
		    (cond
		     ((eq? modifier 'colon)
		      (set! clause-default #t)
		      (substring format-string clause-pos
				 (- format:pos 3)))
		     ((memq modifier '(at colon-at))
		      (error "illegal modifier in escape sequence ~;"))
		     (else
		      (substring format-string clause-pos
				 (- format:pos 2))))))
	       (set! clauses (append clauses (list clause-str)))
	       (set! clause-pos format:pos)))
	   (anychar-dispatch))
	  ((#\]) ; Conditional end
	   (when (zero? conditional-nest)
	     (error "missing escape sequence ~["))
	   (set! conditional-nest (- conditional-nest 1))
	   (when modifier
	     (error "no modifier allowed in escape sequence ~]"))
	   (when (not (null? params))
	     (error "no parameter allowed in escape sequence ~]"))
	   (cond
	    ((zero? conditional-nest)
	     (let ((clause-str (substring format-string clause-pos
					  (- format:pos 2))))
	       (if clause-default
		   (set! clause-default clause-str)
		 (set! clauses (append clauses (list clause-str)))))
	     (case conditional-type
	       ((if-then)
		(if conditional-arg
		    (format:format-work (car clauses)
					(list conditional-arg))))
	       ((if-else-then)
		(add-arg-pos
		 (format:format-work (if conditional-arg
					 (cadr clauses)
				       (car clauses))
				     (rest-args))))
	       ((num-case)
		(when (or (not (integer? conditional-arg))
			  (< conditional-arg 0))
		  (error "argument not a positive integer"))
		(when (not (and (>= conditional-arg (length clauses))
				(not clause-default)))
		  (add-arg-pos
		   (format:format-work
		    (if (>= conditional-arg (length clauses))
			clause-default
		      (list-ref clauses conditional-arg))
		    (rest-args))))))))
	   (anychar-dispatch))
	  ((#\{) ; Iteration begin
	   (set! iteration-nest (+ iteration-nest 1))
	   (cond
	    ((= iteration-nest 1)
	     (set! iteration-pos format:pos)
	     (set! iteration-type
		   (case modifier
		     ((at) 'rest-args)
		     ((colon) 'sublists)
		     ((colon-at) 'rest-sublists)
		     (else 'list)))
	     (set! max-iterations (if (one-positive-integer? params)
				      (car params) #f))))
	   (anychar-dispatch))
	  ((#\}) ; Iteration end
	   (when (zero? iteration-nest)
	     (error "missing in escape sequence ~{"))
	   (set! iteration-nest (- iteration-nest 1))
	   (case modifier
	     ((colon)
	      (when (not max-iterations)
		(set! max-iterations 1)))
	     ((colon-at at)
	      (error "illegal modifier")))
	   (when (not (null? params))
	     (error "no parameters allowed in escape sequence ~}"))
	   (if (zero? iteration-nest)
	       (let ((iteration-str
		      (substring format-string iteration-pos
				 (- format:pos (if modifier 3 2)))))
		 (if (string=? iteration-str "")
		     (set! iteration-str (next-arg)))
		 (case iteration-type
		   ((list)
		    (let ((args (next-arg))
			  (args-len 0))
		      (when (not (list? args))
			(error "expected a list argument"))
		      (set! args-len (length args))
		      (do ((arg-pos 0 (+ arg-pos
					 (format:format-work
					  iteration-str
					  (list-tail args arg-pos))))
			   (i 0 (+ i 1)))
			  ((or (>= arg-pos args-len)
			       (and max-iterations
				    (>= i max-iterations)))))))
		   ((sublists)
		    (let ((args (next-arg))
			  (args-len 0))
		      (when (not (list? args))
			(error "expected a list argument"))
		      (set! args-len (length args))
		      (do ((arg-pos 0 (+ arg-pos 1)))
			  ((or (>= arg-pos args-len)
			       (and max-iterations
				    (>= arg-pos max-iterations))))
			(let ((sublist (list-ref args arg-pos)))
			  (when (not (list? sublist))
			    (error "expected a list of lists argument"))
			  (format:format-work iteration-str sublist)))))
		   ((rest-args)
		    (let* ((args (rest-args))
			   (args-len (length args))
			   (usedup-args
			    (do ((arg-pos 0 (+ arg-pos
					       (format:format-work
						iteration-str
						(list-tail
						 args arg-pos))))
				 (i 0 (+ i 1)))
				((or (>= arg-pos args-len)
				     (and max-iterations
					  (>= i max-iterations)))
				 arg-pos))))
		      (add-arg-pos usedup-args)))
		   ((rest-sublists)
		    (let* ((args (rest-args))
			   (args-len (length args))
			   (usedup-args
			    (do ((arg-pos 0 (+ arg-pos 1)))
				((or (>= arg-pos args-len)
				     (and max-iterations
					  (>= arg-pos max-iterations)))
				 arg-pos)
			      (let ((sublist (list-ref args arg-pos)))
				(when (not (list? sublist))
				  (error "expected list arguments"))
				(format:format-work iteration-str sublist)))))
		      (add-arg-pos usedup-args)))
		   (else
		    (error "internal error in escape sequence ~}")))))
	   (anychar-dispatch))
	  ((#\^) ; Up and out
	   (let* ((continue
		   (cond
		    ((not (null? params))
		     (not
		      (case (length params)
			((1) (zero? (car params)))
			((2) (= (list-ref params 0) (list-ref params 1)))
			((3) (<= (list-ref params 0)
				 (list-ref params 1)
				 (list-ref params 2)))
			(else
			 (error "too much parameters")))))
		    (format:case-conversion ; if conversion stop conversion
		     (set! format:case-conversion string-copy) #t)
		    ((= iteration-nest 1) #t)
		    ((= conditional-nest 1) #t)
		    ((>= arg-pos arg-len)
		     (set! format:pos format-string-len) #f)
		    (else #t))))
	     (if continue
		 (anychar-dispatch))))

	  ;; format directive modifiers and parameters

	  ((#\@) ; `@' modifier
	   (when (memq modifier '(at colon-at))
	     (error "double `@' modifier"))
	   (set! modifier (if (eq? modifier 'colon) 'colon-at 'at))
	   (tilde-dispatch))
	  ((#\:) ; `:' modifier
	   (when (memq modifier '(colon colon-at))
	     (error "double escape sequence `:' modifier"))
	   (set! modifier (if (eq? modifier 'at) 'colon-at 'colon))
	   (tilde-dispatch))
	  ((#\') ; Character parameter
	   (when modifier
	     (error "misplaced escape sequence modifier"))
	   (set! params (append params (list (char->integer (next-char)))))
	   (set! param-value-found #t)
	   (tilde-dispatch))
	  ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\- #\+) ; num. paramtr
	   (when modifier
	     (error "misplaced escape sequence modifier"))
	   (let ((num-str-beg (- format:pos 1))
		 (num-str-end format:pos))
	     (do ((ch (peek-next-char) (peek-next-char)))
		 ((not (char-numeric? ch)))
	       (next-char)
	       (set! num-str-end (+ 1 num-str-end)))
	     (set! params
		   (append params
			   (list (string->number
				  (substring format-string
					     num-str-beg
					     num-str-end))))))
	   (set! param-value-found #t)
	   (tilde-dispatch))
	  ((#\V) ; Variable parameter from next argum.
	   (when modifier
	     (error "misplaced escape sequence modifier"))
	   (set! params (append params (list (next-arg))))
	   (set! param-value-found #t)
	   (tilde-dispatch))
	  ((#\#) ; Parameter is number of remaining args
	   (when param-value-found
	     (error "misplaced '#'"))
	   (when modifier
	     (error "misplaced escape sequence modifier"))
	   (set! params (append params (list (length (rest-args)))))
	   (set! param-value-found #t)
	   (tilde-dispatch))
	  ((#\,) ; Parameter separators
	   (when modifier
	     (error "misplaced escape sequence modifier"))
	   (if (not param-value-found)
	       (set! params (append params '(#f)))) ; append empty paramtr
	   (set! param-value-found #f)
	   (tilde-dispatch))
	  ((#\Q) ; Inquiry messages
	   (if (eq? modifier 'colon)
	       (format:out-str format:version)
	     (let ((nl (string #\newline)))
	       (format:out-str
		(string-append
		 "SLIB Common LISP format version " format:version nl
		 "  (C) copyright 1992-1994 by Dirk Lutzebaeck" nl
		 "  please send bug reports to `lutzeb@cs.tu-berlin.de'"
		 nl))))
	   (anychar-dispatch))
	  (else ; Unknown tilde directive
	   (error "unknown control character"
	     (string-ref format-string (- format:pos 1))))))
       (else
	(anychar-dispatch)))) ; in case of conditional

    (set! format:pos 0)
    (set! format:arg-pos 0)
    (anychar-dispatch) ; start the formatting
    (set! format:pos recursive-pos-save)

    ;;Return the position in the arguments list.
    arg-pos))


;;;; body of FORMAT

(set! format:fn-str (make-string format:fn-max)) ; number buffer
(set! format:en-str (make-string format:en-max)) ; exponent buffer

(format:dispatch-to-destination-port args)


;;;; done

) ;; end of FORMAT defininition

) ;; end of LIBRARY form

;;; end of file
