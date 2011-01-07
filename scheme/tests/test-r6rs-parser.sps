;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for R6RS parser
;;;Date: Fri Jan  7, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!r6rs
(import (nausicaa)
  (nausicaa silex lexer)
  (nausicaa parser-tools lexical-token)
  (nausicaa parser-tools source-location)
  (nausicaa r6rs lexer)
  (nausicaa r6rs parser)
  (nausicaa checks))

(check-set-mode! 'report-failed)
(display "*** testing R6RS parser\n")


(parametrise ((check-test-name	'simple-datums))

  (define (error-handler message (T <lexical-token>))
    (raise
     (condition (make-lexical-violation)
		(make-message-condition
		 (string-append message
				" line " (number->string T.location.line)
				" column " (number->string T.location.column)))
		(make-irritants-condition `(,T.value)))))

  (define (parse string)
    (let* ((IS		(lexer-make-IS (string: string) (counters: 'all)))
	   (lexer	(make-token-lexer IS))
	   (parser	(make-r6rs-parser)))
      (parser lexer error-handler #f)))

  (check
      (parse "ciao")
    => 'ciao)

  (check
      (parse "\"ciao\"")
    => "ciao")

  (check
      (parse "123")
    => 123)

  (check
      (parse "#t")
    => #t)

  (check
      (parse "#f")
    => #f)

  (check
      (parse "#\\newline")
    => #\newline)

  (check
      (parse "#\\A")
    => #\A)

  #t)


(parametrise ((check-test-name	'sexps)
	      (debugging	#t))

  (define (error-handler message (T <lexical-token>))
    (raise
     (condition (make-lexical-violation)
		(make-message-condition
		 (string-append message
				" line " (number->string T.location.line)
				" column " (number->string T.location.column)))
		(make-irritants-condition `(,T.value)))))

  (define (parse string)
    (let* ((IS		(lexer-make-IS (string: string) (counters: 'all)))
	   (lexer	(make-token-lexer IS))
	   (parser	(make-r6rs-parser)))
      (parser lexer error-handler #f)))

;;; --------------------------------------------------------------------
;;; pairs

  (check
      (parse "(1 . 2)")
    => '(1 . 2))

  (check
      (parse "(ciao . \"mamma\")")
    => '(ciao . "mamma"))

  (check
      (parse "[1 . 2]")
    => '[1 . 2])

  (check
      (parse "(ciao . \"mamma\")")
    => '[ciao . "mamma"])

;;; --------------------------------------------------------------------
;;; lists

  (check
      (parse "()")
    => '())

  (check
      (parse "(#t)")
    => '(#t))

  (check
      (parse "(1 2 3)")
    => '(1 2 3))

  (check
      (parse "(ciao \"mamma\" #\\A)")
    => '(ciao "mamma" #\A))

  (check
      (parse "[]")
    => '())

  (check
      (parse "[#t]")
    => '(#t))

  (check
      (parse "[1 2 3]")
    => '(1 2 3))

  (check
      (parse "[ciao \"mamma\" #\\A]")
    => '(ciao "mamma" #\A))

;;; --------------------------------------------------------------------
;;; vectors

  (check
      (parse "#()")
    => '#())

  (check
      (parse "#(#t)")
    => '#(#t))

  (check
      (parse "#(1 2 3)")
    => '#(1 2 3))

  (check
      (parse "#(ciao \"mamma\" #\\A)")
    => '#(ciao "mamma" #\A))

;;; --------------------------------------------------------------------
;;; bytevectors

;;Remember that  bytevectors cannot be quasiquoted, they  must be quoted
;;and can hold only u8 exact integers.

  (check
      (parse "#vu8()")
    => '#vu8())

  (check
      (parse "#vu8(1)")
    => '#vu8(1))

  (check
      (parse "#vu8(1 2 3)")
    => '#vu8(1 2 3))

  (check
      (guard (E ((lexical-violation? E)
;;;		 (write (condition-message E))(newline)
		 (condition-irritants E))
		(else
		 (debug-print-condition "error " E)
		 E))
	(parse "#vu8(1 ciao 3)"))
    => '(ciao))

  #t)


(parametrise ((check-test-name	'quoting)
	      (debugging	#t))

  (define (error-handler message (T <lexical-token>))
    (raise
     (condition (make-lexical-violation)
		(make-message-condition
		 (string-append message
				" line " (number->string T.location.line)
				" column " (number->string T.location.column)))
		(make-irritants-condition `(,T.value)))))

  (define (parse string)
    (let* ((IS		(lexer-make-IS (string: string) (counters: 'all)))
	   (lexer	(make-token-lexer IS))
	   (parser	(make-r6rs-parser)))
      (parser lexer error-handler #f)))

;;; --------------------------------------------------------------------
;;; quote

  (check
      (parse "'1")
    => '(quote 1))

  (check
      (parse "'()")
    => '(quote ()))

  (check
      (parse "'(1 . 2)")
    => '(quote (1 . 2)))

  #t)


(parametrise ((check-test-name	'errors)
	      (debugging	#t))

  (define (error-handler message (T <lexical-token>))
    (raise
     (condition (make-lexical-violation)
		(make-message-condition
		 (string-append message
				" line " (number->string T.location.line)
				" column " (number->string T.location.column)))
		(make-irritants-condition `(,T.value)))))

  (define (parse string)
    (let* ((IS		(lexer-make-IS (string: string) (counters: 'all)))
	   (lexer	(make-token-lexer IS))
	   (parser	(make-r6rs-parser)))
      (parser lexer error-handler #f)))

  (define-syntax test-lexical-error
    (syntax-rules ()
      ((_ ?string ?expected)
       (check
	   (guard (E ((lexical-violation? E)
;;;		      (write (condition-message E))(newline)
		      (condition-irritants E))
		     (else
		      (debug-print-condition "error " E)
		      E))
	     (parse ?string))
	 => '(?expected)))))

;;; --------------------------------------------------------------------
;;; paren and bracket mismatch

  (test-lexical-error "(1 2]" #\])
  (test-lexical-error "[1 2)" #\))

  #t)


(parametrise ((check-test-name	'comments)
	      (debugging	#f))

  (define (error-handler message (T <lexical-token>))
    (raise
     (condition (make-lexical-violation)
		(make-message-condition
		 (string-append message
				" line " (number->string T.location.line)
				" column " (number->string T.location.column)))
		(make-irritants-condition `(,T.value)))))

  (define (parse string)
    (let* ((IS		(lexer-make-IS (string: string) (counters: 'all)))
	   (true-lexer	(make-token-lexer* IS))
	   (lexer	(lambda ()
			  (let (((T <lexical-token>) (true-lexer)))
			    (debug "c: ~s, v: ~s" T.category T.value)
			    T)))
	   (parser	(make-r6rs-parser)))
      (parser lexer error-handler #f)))

;;; --------------------------------------------------------------------
;;; comments

;;Notice that we are using a lexer built by MAKE-TOKEN-LEXER* which does
;;not discard WHITESPACE and LINENEDING tokens.

  (check	;single line comment
      (parse ";; ciao")
    => '(";; ciao"))

  (check	;multiple line comments
      (parse ";; ciao\n;; mamma\n;; sto bene")
    => '(";; ciao\n" ";; mamma\n" ";; sto bene"))

  (check
      (parse "#!r6rs")
    => '("#!r6rs"))

  (check	;mixed comments
      (parse "#!r6rs #!ciao #!mamma")
    => '("#!r6rs" " " "#!ciao" " " "#!mamma"))

  #t)


;;;; done

(check-report)

;;; end of file
