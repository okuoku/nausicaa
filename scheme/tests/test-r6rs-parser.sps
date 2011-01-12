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
  (only (nausicaa parser-tools)
	<lexical-token>
	<source-location>)
  (prefix (nausicaa r6rs lexer) r6.)
  (prefix (nausicaa r6rs parser) r6.)
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
	   (lexer	(r6.make-token-lexer IS))
	   (parser	(r6.make-r6rs-parser)))
      (parser lexer error-handler #f)))

  (check
      (parse "ciao")
    => '(ciao))

  (check
      (parse "\"ciao\"")
    => '("ciao"))

  (check
      (parse "123")
    => '(123))

  (check
      (parse "#t")
    => '(#t))

  (check
      (parse "#f")
    => '(#f))

  (check
      (parse "#\\newline")
    => '(#\newline))

  (check
      (parse "#\\A")
    => '(#\A))

;;; --------------------------------------------------------------------

  (check
      (parse "ciao 123 #\\A")
    => '(ciao 123 #\A))

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
	   (lexer	(r6.make-token-lexer IS))
	   (parser	(r6.make-r6rs-parser)))
      (parser lexer error-handler #f)))

;;; --------------------------------------------------------------------
;;; pairs

  (check
      (parse "(1 . 2)")
    => '((1 . 2)))

  (check
      (parse "(ciao . \"mamma\")")
    => '((ciao . "mamma")))

  (check
      (parse "[1 . 2]")
    => '([1 . 2]))

  (check
      (parse "(ciao . \"mamma\")")
    => '([ciao . "mamma"]))

;;; --------------------------------------------------------------------
;;; lists

  (check
      (parse "()")
    => '(()))

  (check
      (parse "(#t)")
    => '((#t)))

  (check
      (parse "(1 2 3)")
    => '((1 2 3)))

  (check
      (parse "(ciao \"mamma\" #\\A)")
    => '((ciao "mamma" #\A)))

  (check
      (parse "[]")
    => '(()))

  (check
      (parse "[#t]")
    => '((#t)))

  (check
      (parse "[1 2 3]")
    => '((1 2 3)))

  (check
      (parse "[ciao \"mamma\" #\\A]")
    => '((ciao "mamma" #\A)))

;;; --------------------------------------------------------------------
;;; vectors

  (check
      (parse "#()")
    => '(#()))

  (check
      (parse "#(#t)")
    => '(#(#t)))

  (check
      (parse "#(1 2 3)")
    => '(#(1 2 3)))

  (check
      (parse "#(ciao \"mamma\" #\\A)")
    => '(#(ciao "mamma" #\A)))

;;; --------------------------------------------------------------------
;;; bytevectors

;;Remember that  bytevectors cannot be quasiquoted, they  must be quoted
;;and can hold only u8 exact integers.

  (check
      (parse "#vu8()")
    => '(#vu8()))

  (check
      (parse "#vu8(1)")
    => '(#vu8(1)))

  (check
      (parse "#vu8(1 2 3)")
    => '(#vu8(1 2 3)))

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
	   (lexer	(r6.make-token-lexer IS))
	   (parser	(r6.make-r6rs-parser)))
      (parser lexer error-handler #f)))

;;; --------------------------------------------------------------------
;;; quote

  (check
      (parse "'1")
    => '((quote 1)))

  (check
      (parse "'()")
    => '((quote ())))

  (check
      (parse "'(1 . 2)")
    => '((quote (1 . 2))))

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
	   (lexer	(r6.make-token-lexer IS))
	   (parser	(r6.make-r6rs-parser)))
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


(parametrise ((check-test-name		'comments)
	      (debugging		#f)
	      (r6.list-of-datums-maker	(lambda (yypushback yycustom datums)
					  datums)))

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
	   (true-lexer	(r6.make-token-lexer IS
			  (r6.comments #t)
			  (r6.blanks #t)
			  (r6.sharpbang #t)))
	   (lexer	(lambda ()
			  (let (((T <lexical-token>) (true-lexer)))
			    (debug "c: ~s, v: ~s" T.category T.value)
			    T)))
	   (parser	(r6.make-r6rs-parser)))
      (parser lexer error-handler #f)))

  (define (doit string)
    (let ((L (parse string)))
      (if (is-a? (car L) r6.<interlexeme-space>)
	  (let (((T r6.<interlexeme-space>) (car L)))
	    T.atmospheres)
	L)))

;;; --------------------------------------------------------------------
;;; comments

  (check	;single line comment
      (doit ";; ciao")
    => '(";; ciao"))

  (check	;multiple line comments
      (doit ";; ciao\n;; mamma\n;; sto bene")
    => '(";; ciao\n" ";; mamma\n" ";; sto bene"))

  (check
      (doit "#!r6rs")
    => '("#!r6rs"))

  (check	;mixed sharpbangs
      (doit "#!r6rs #!ciao #!mamma")
    => '("#!r6rs" " " "#!ciao" " " "#!mamma"))

  (check	;nested comment
      (doit "#| ciao\nmamma |#")
    => '("#| ciao\nmamma |#"))

;;; --------------------------------------------------------------------
;;; commented datums

  ;; commented identifier
  (let* (((L <list>)                 (parse "#;ciao"))
	 ((R r6.<interlexeme-space>) L.car)
	 ((C r6.<commented-datum>)   R.atmospheres.car))
    (check C.datum => 'ciao)
    (check C.interlexeme-space => #f)
    #f)

  ;; commented identifier, with space
  (let* (((L <list>)                 (parse "#;\tciao"))
	 ((R r6.<interlexeme-space>) L.car)
	 ((C r6.<commented-datum>)   R.atmospheres.car))
    (check C.datum => 'ciao)
    (check C.interlexeme-space.atmospheres => '("\t"))
    #f)

  #t)


;;;; done

(check-report)

;;; end of file
