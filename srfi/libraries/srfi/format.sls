;;; FILE       "format.sls"
;;; IMPLEMENTS SRFI-48: Intermediary format strings
;;;            http://srfi.schemers.org/srfi-48/srfi-48.html
;;; AUTHOR     Ken Dickey
;;; UPDATED    Syntax updated for R6RS February 2008 by Ken Dickey
;;; LANGUAGE   R6RS but specific to Ikarus Scheme
;;;
;;;Small changes by  Derick Eddington to make the  beginning of `format'
;;;more effecient and more abstracted.
;;;
;;;Adapted by Marco Maggi  for Nausicaa inclusion: removed dependence of
;;;string-ports.
;;;
;;;Copyright (C) Kenneth A Dickey (2003).  All Rights Reserved.
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
;;;THE  SOFTWARE IS  PROVIDED "AS  IS",  WITHOUT WARRANTY  OF ANY  KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT. IN  NO EVENT SHALL THE AUTHORS  OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY,  WHETHER IN AN
;;;ACTION OF  CONTRACT, TORT  OR OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION  WITH THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.
;;;
;;;The implementation  below requires  SRFI-6 (Basic string  ports), and
;;;SRFI-38 (External Representation for Data With Shared Structure).



;; *** NOTE ***
;;
;;Some of the  helper functions and macros are  duplicated and tested in
;;the  test file  "test-helpers-format.sps".

#!r6rs
(library (srfi format)
  (export
    format)
  (import (rnrs)
    (srfi receive)
    (srfi sharing)
    (only (srfi strings)
	  string-index)
    (srfi format compat))


;;;; localised stuff

(define (localised-decimal-separator)
  ".")



;;;; main functions

(define (format arg0 . arg*)
  (if (string? arg0)
      (receive (port getter)
	  (open-string-output-port)
	(_format port arg0 arg* getter))
    (if (null? arg*)
	(problem "too few arguments" (list arg0))
      (receive (port getter)
	  (cond ((eq? arg0 #f) (open-string-output-port))
		((eq? arg0 #t) (values (current-output-port) (lambda () #t)))
		((output-port? arg0) (values arg0 (lambda () #t)))
		(else (problem "bad output-port argument" arg0)))
	(let ((arg1 (car arg*)))
	  (if (string? arg1)
	      (_format port arg1 (cdr arg*) (if arg0
						(lambda (dummy)
						  (values))
					      getter))
	    (problem "not a string" arg1)))))))

(define (_format port format-string args return-value)
  (let ((unused-args (format-help port format-string args)))
    (if (not (null? unused-args))
	(problem "unused arguments" unused-args)
      (return-value))))


;;;; miscellaneous helpers

(define documentation-string
  "(format [<port>] <format-string> [<arg>...]) -- <port> is #t, #f or an output-port
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

(define-syntax problem
  (syntax-rules ()
    ((_ ?message ?irritants)
     (assertion-violation 'format ?message ?irritants))
    ((_ ?message)
     (assertion-violation 'format ?message))))

(define-syntax require-an-arg
  (syntax-rules ()
    ((_ ?args)
     (when (null? ?args)
       (problem "too few arguments")))))

;;Return  a string  of  LEN char  with  STR anchored  to  the right  and
;;left-padded with CHARs.  If the length of STR is greater than LEN: STR
;;is returned.
(define (make-padded-string str len char)
  (let ((off (- len (string-length str))))
    (if (positive? off)
	(string-append (make-string off char) str)
      str)))



;;Return a string  representing a floating point number,  with the given
;;number of digits in the fractional part.
;;
;;DIGITS must be the number  of digits requested in the fractional part.
;;PRE-STR must  be the string representing the  integral part.  FRAC-STR
;;must  be string  representing the  original fractional  part.  EXP-STR
;;must be  the string representing  the exponential part,  starting with
;;the character #\e.
;;
;;All the string arguments can be the empty string.  The DIGITS argument
;;can be zero.
;;
;;If  PRE-STR has  more  digits  than requested  by  DIGITS: PRE-STR  is
;;truncated and the last digit rounded.
;;
(define (compose-with-digits digits pre-str frac-str exp-str)
  (let ((frac-len (string-length frac-str)))
    (cond
     ((< frac-len digits) ;; grow frac part, pad with zeros
      (string-append pre-str (localised-decimal-separator)
		     frac-str (make-string (- digits frac-len) #\0)
		     exp-str))
     ((= frac-len digits) ;; frac-part is exactly the right size
      (string-append pre-str (localised-decimal-separator) frac-str exp-str))
     (else ;; must round to shrink it
      (let* ((first-part	(substring frac-str 0 digits))
	     (last-part	(substring frac-str digits frac-len))
	     (temp-str	(number->string
			 (round (string->number
				 (string-append first-part (localised-decimal-separator)
						last-part)))))
	     (dot-pos	(string-index  temp-str #\.))
	     (carry?	(and (> dot-pos digits)
			     (> (round (string->number
					(string-append "0." frac-str)))
				0)))
	     (new-frac	(substring temp-str 0 digits)))
	(string-append
	 (if carry?
	     (number->string (+ 1 (string->number pre-str)))
	   pre-str)
	 (localised-decimal-separator) new-frac exp-str))))))



(define (format-fixed number-or-string width digits)
  (cond

   ((real? number-or-string)
    (format-fixed-number number-or-string width digits))

   ((complex? number-or-string)
    (let* ((num		(real-part number-or-string))
	   (real	(if digits (inexact num) num))
	   (imag	(imag-part number-or-string)))
      (make-padded-string
       (string-append (format-fixed-number real 0 digits)
		      (if (negative? imag) "" "+")
		      (format-fixed-number imag 0 digits)
		      "i")
       width
       #\space)))

   ((string? number-or-string)
    (make-padded-string number-or-string width #\space))

   (else
    (problem "~F requires a number or a string" number-or-string))))

(define (format-fixed-number number width digits)
  (if digits
      (let* ((num-str	(number->string (if (rational? number)
					    (inexact number)
					  number)))
	     (dot-idx	(string-index  num-str #\.))
	     (exp-idx	(string-index  num-str #\e))
	     (length	(string-length num-str))
	     (pre-str	(cond
			 ((and exp-idx (not dot-idx))
			  (substring num-str 0 exp-idx))
			 (dot-idx
			  (substring num-str 0 dot-idx))
			 (else
			  num-str)))
	     (exp-str	(if exp-idx
			    (substring num-str exp-idx length)
			  ""))
	     (frac-str (let ((dot-idx (or dot-idx -1)))
			 (if exp-idx
			     (substring num-str
					(+ dot-idx 1)
					exp-idx)
			   (substring num-str (+ dot-idx 1) length)))))
	(make-padded-string (if dot-idx
				(compose-with-digits digits pre-str
						     frac-str exp-str)
			      (string-append pre-str exp-str))
			    width #\space))
    (make-padded-string (number->string number) width #\space)))



(define (format-help p format-strg arglist)

  (define length-of-format-string (string-length format-strg))

  (define (anychar-dispatch pos arglist last-was-newline)
    (if (>= pos length-of-format-string)
	arglist ; return unused args
      (let ( (char (string-ref format-strg pos)) )
	(if (eqv? char #\~)
	    (tilde-dispatch (+ pos 1) arglist last-was-newline)
	  (begin
	    (write-char char p)
	    (anychar-dispatch (+ pos 1) arglist #f))))))

  (define (has-newline? whatever last-was-newline)
    (or (eqv? whatever #\newline)
	(and (string? whatever)
	     (let ((len (string-length whatever)))
	       (if (zero? len)
		   last-was-newline
		 (eqv? #\newline
		       (string-ref whatever (- len 1))))))))

  (define (tilde-dispatch pos arglist last-was-newline)
    (if (>= pos length-of-format-string)
	(begin
	  (write-char #\~ p) ;; tilde at end of string is just output
	  arglist)	       ;; return unused args
      (case (char-upcase (string-ref format-strg pos))

	((#\A) ; Any -- for humans
	 (require-an-arg arglist)
	 (let ((whatever (car arglist)))
	   (display whatever p)
	   (anychar-dispatch (+ pos 1)
			     (cdr arglist)
			     (has-newline? whatever last-was-newline))))

	((#\S) ; Slashified -- for parsers
	 (require-an-arg arglist)
	 (let ((whatever (car arglist)))
	   (case whatever
	     ;;Normalise  to newline  because  #\newline and  #\linefeed
	     ;;have EQUAL?  values and different implementations default
	     ;;to  one  or  the  other  (example:  Ypsilon  prints  only
	     ;;#\linefeed, Larceny only #\newline).
	     ((#\newline #\linefeed)
	      (display "#\\newline" p))
	     (else
	      (write whatever p)))
	   (anychar-dispatch (+ pos 1)
			     (cdr arglist)
			     (has-newline? whatever last-was-newline))))

	((#\W)
	 (require-an-arg arglist)
	 (let ((whatever (car arglist)))
	   (write-with-shared-structure whatever p) ;; srfi-38
	   (anychar-dispatch (+ pos 1)
			     (cdr arglist)
			     (has-newline? whatever last-was-newline))))

	((#\D) ; Decimal
	 (require-an-arg arglist)
	 (display (number->string (car arglist) 10) p)
	 (anychar-dispatch (+ pos 1) (cdr arglist) #f))

	((#\X) ; HeXadecimal
	 (require-an-arg arglist)
	 (display (number->string (car arglist) 16) p)
	 (anychar-dispatch (+ pos 1) (cdr arglist) #f))

	((#\O) ; Octal
	 (require-an-arg arglist)
	 (display (number->string (car arglist)  8) p)
	 (anychar-dispatch (+ pos 1) (cdr arglist) #f))

	((#\B) ; Binary
	 (require-an-arg arglist)
	 (display (number->string (car arglist)  2) p)
	 (anychar-dispatch (+ pos 1) (cdr arglist) #f))

	((#\C) ; Character
	 (require-an-arg arglist)
	 (write-char (car arglist) p)
	 (anychar-dispatch (+ pos 1)
			   (cdr arglist)
			   (eqv? (car arglist) #\newline)))

	((#\~) ; Tilde
	 (write-char #\~ p)
	 (anychar-dispatch (+ pos 1) arglist #f))

	((#\%) ; Newline
	 (newline p)
	 (anychar-dispatch (+ pos 1) arglist #t))

	((#\&)			; Freshline
	 (when (not last-was-newline) ;; (unless last-was-newline ..
	   (newline p))
	 (anychar-dispatch (+ pos 1) arglist #t))

	((#\_) ; Space
	 (write-char #\space p)
	 (anychar-dispatch (+ pos 1) arglist #f))

	((#\T) ; Tab -- IMPLEMENTATION DEPENDENT ENCODING
	 (write-char ascii-tab p)
	 (anychar-dispatch (+ pos 1) arglist #f))

	((#\Y) ; Pretty-print
	 (pretty-print (car arglist) p)
	 (anychar-dispatch (+ pos 1) (cdr arglist) #f))

	((#\F)
	 (require-an-arg arglist)
	 (display (format-fixed (car arglist) 0 #f) p)
	 (anychar-dispatch (+ pos 1) (cdr arglist) #f))

	((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
	 ;; gather "~w(,d)F" w and d digits
	 (let loop ((index		(+ pos 1))
		    (w-digits		(list (string-ref format-strg pos)))
		    (d-digits		'())
		    (in-width?	#t))
	   (if (>= index length-of-format-string)
	       (problem "improper numeric format directive" format-strg)
	     (let ((next-char (string-ref format-strg index)))
	       (cond ((char-numeric? next-char)
		      (if in-width?
			  (loop (+ index 1)
				(cons next-char w-digits)
				d-digits
				in-width?)
			(loop (+ index 1)
			      w-digits
			      (cons next-char d-digits)
			      in-width?)))
		     ((char=? (char-upcase next-char) #\F)
		      (let ((width	(string->number
					 (list->string
					  (reverse w-digits))))
			    (digits (if (zero? (length d-digits))
					#f
				      (string->number
				       (list->string (reverse d-digits))))))
			(display (format-fixed (car arglist) width digits) p)
			(anychar-dispatch (+ index 1) (cdr arglist) #f)))
		     ((char=? next-char #\,)
		      (if in-width?
			  (loop (+ index 1)
				w-digits
				d-digits
				#f)
			(problem "too many commas in directive" format-strg)))
		     (else
		      (problem "~w,dF directive ill-formed" format-strg)))))))

	((#\? #\K) ; indirection -- take next arg as format string
	 (cond     ;  and following arg as list of format args
	  ((< (length arglist) 2)
	   (problem "less arguments than specified for ~?" arglist))
	  ((not (string? (car arglist)))
	   (problem "~? requires a string" (car arglist)))
	  (else
	   (format-help p (car arglist) (cadr arglist))
	   (anychar-dispatch (+ pos 1) (cddr arglist) #f))))

	((#\H) ; Help
	 (display documentation-string p)
	 (anychar-dispatch (+ pos 1) arglist #t))

	(else
	 (problem "unknown tilde escape" (string-ref format-strg pos))))))

  (anychar-dispatch 0 arglist #f))



;;;; done

)

;;; end of file
