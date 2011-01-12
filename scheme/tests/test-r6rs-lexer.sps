;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for R6RS lexer
;;;Date: Wed Dec 22, 2010
;;;
;;;Abstract
;;;
;;;	The original tests for number conversion are from the test suite
;;;	of Ikarus Scheme by Abdulaziz Ghuloum.
;;;
;;;Copyright (c) 2010, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
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
  (nausicaa checks))

(check-set-mode! 'report-failed)
(display "*** testing R6RS lexer\n")


;;;; helpers

(define (equal-number-results? x y)
  ;;Used to compare numbers.
  ;;
  (define (== x y)
    (define epsilon 1e-6)
    (cond ((nan? x) (nan? y))
	  ((zero? x)
	   (zero? y)
	   ;;(and (= x y) (= (atan 0.0 x) (atan 0.0 y)))
	   )
	  ((infinite? x)
	   (and (infinite? y)
		(or (and (positive? x) (positive? y))
		    (and (negative? x) (negative? y)))))
	  (else
	   (and (or (and (exact? x) (exact? y))
		    (and (inexact? x) (inexact? y)))
		;;(= x y)
		(< (abs (- x y)) epsilon)
		))))
  (cond ((and (number? x) (number? y))
	 (and (== (real-part x) (real-part y))
	      (== (imag-part x) (imag-part y))))
	(else (equal? x y))))

(define-constant inf+
  (fl/ (inexact 1) (inexact 0)))

(define-constant inf-
  (fl/ (inexact -1) (inexact 0)))

(define-syntax test-lexical-error
  (syntax-rules ()
    ((_ ?lexer ?string)
     (check
	 (guard (E ((lexical-violation? E)
;;;		      (display (condition-message E))(newline)
		    (condition-irritants E))
		   (else E))
	   (?lexer ?string))
       => '((*lexer-error* ?string))))
    ((_ ?name ?lexer ?string)
     (check ?name
       (guard (E ((lexical-violation? E)
;;;		      (display (condition-message E))(newline)
		  (condition-irritants E))
		 (else E))
	 (?lexer ?string))
       => '((*lexer-error* ?string))))
    ))


(parametrise ((check-test-name	'string-tokeniser))

  (define (tokenise string)
    (let* ((IS		(lexer-make-IS (string: string) (counters: 'all)))
	   (lexer	(lexer-make-lexer r6.r6rs-string-lexer-table IS))
	   (result	'()))
      (do (((T <lexical-token>) (lexer) (lexer)))
	  (T.special?
	   (reverse `(,(if (is-a? T <lexical-token>)
			   T.category
			 T)
		      . ,result)))
	(set-cons! result T))))

;;; --------------------------------------------------------------------
;;; All the test strings must end with a double-quote char.

  (check	;empty string
      (tokenise "\"")
    => '(STRING *eoi*))

  (check
      (tokenise "\\\"\"")
    => '(#\" STRING *eoi*))

  (check
      (tokenise "\\\\/\"")
    => '(#\\ "/" STRING *eoi*))

  (check
      (tokenise "\\a\"")
    => '(#\x7 STRING *eoi*))

  (check
      (tokenise "\\b\"")
    => '(#\x8 STRING *eoi*))

  (check
      (tokenise "\\t\"")
    => '(#\x9 STRING *eoi*))

  (check
      (tokenise "\\n\"")
    => '(#\xA STRING *eoi*))

  (check
      (tokenise "\\v\"")
    => '(#\xB STRING *eoi*))

  (check
      (tokenise "\\f\"")
    => '(#\xC STRING *eoi*))

  (check
      (tokenise "\\r\"")
    => '(#\xD STRING *eoi*))

  (check
      (tokenise "\\\"\"")
    => '(#\" STRING *eoi*))

  (check
      (tokenise "\\\\\"")
    => '(#\\ STRING *eoi*))

  (check
      (tokenise "inizio\\\"/\\b\\f\\n\\r\\tfine\"")
    => '("inizio" #\" "/" #\backspace #\page #\newline #\return #\tab "fine" STRING *eoi*))

  (check
      (tokenise "\\x005C;\"")
    => '(#\\ STRING *eoi*))

  (check
      (tokenise "\\xA;\"")
    => '(#\newline STRING *eoi*))

  (check
      (tokenise "x\\xA;x\"")
    => '("x" #\newline "x" STRING *eoi*))

  (check
      (tokenise "x\\xA;\\x9;\"")
    => '("x" #\newline #\tab STRING *eoi*))

  (check
      (tokenise "\\x0063;\\x0069;\\x0061;\\x006f;\"")
    => '(#\c #\i #\a #\o STRING *eoi*))

  (check	;a string
      (tokenise "ciao\"")
    => '("ciao" STRING *eoi*))

  (check
      ;;Nested double quotes.  The Scheme string "\\\"" is seen as \" by
      ;;the lexer and the backslash quoting character is removed.
      (tokenise "ciao \\\"hello\\\" salut\"")
    => '("ciao " #\" "hello" #\" " salut"  STRING *eoi*))

  (check	;intraline space
      (tokenise "ciao \\\nmamma\"")
    => '("ciao " "mamma" STRING *eoi*))

  (check	;intraline space
      (tokenise "ciao \\   \n   mamma\"")
    => '("ciao " "mamma" STRING *eoi*))

  (check	;intraline space, real usage example
      (tokenise "ciao \\
mamma\"")
    => '("ciao " "mamma" STRING *eoi*))

;;; ------------------------------------------------------------
;;; the following are from the R6RS document

  (check
      (tokenise "\\x41;bc\"")
    => '(#\A "bc" STRING *eoi*))

  (check
      (tokenise "\\x41; bc\"")
    => '(#\A " bc" STRING *eoi*))


  (check
      (tokenise "\\x41bc;\"")
    => '(#\x41bc STRING *eoi*))

  (check
      (tokenise "\\x41")
    => '(*lexer-error*))

  (check
      (tokenise "\\x;")
    => '(*lexer-error*))

  (check
      (tokenise "\\x41bx;")
    => '(*lexer-error*))

  (check
      (tokenise "\\x00000041;\"")
    => '(#\A STRING *eoi*))

  (check
      (tokenise "\\x0010FFFF;\"")
    => '(#\x10FFFF STRING *eoi*))

  (check	;inline-hex-escape out of range
      (tokenise "\\x00110000;")
    => '(*lexer-error*))

  (check
      (tokenise "\\x000000001;\"")
    => '(#\x0001 STRING *eoi*))

  (check	;&lexical exception, in excluded range
      (tokenise "\\xD800;")
    => '(*lexer-error*))

  (check	;&lexical exception, in excluded range
      (tokenise "\\xDFFF;")
    => '(*lexer-error*))

  (check
      (tokenise "A
bc\"")
    => '("A
bc" STRING *eoi*))

;;; --------------------------------------------------------------------
;;; errors

  (check	;missing ending #\; for inline-hex-escape
      (tokenise "\\x00\"")
    => '(*lexer-error*))

  #t)


(parametrise ((check-test-name	'string-reader))

  (define (parse string)
    (r6.read-string (lexer-make-IS (string: string) (counters: 'all))))

;;; All the test strings must end with a double-quote char.

  (check	;empty string
      (parse "\"")
    => "")

  (check
      (parse "\\\"\"")
    => "\"")

  (check
      (parse "\\\\/\"")
    => "\\/")

  (check
      (parse "\\a\"")
    => "\a")

  (check
      (parse "\\b\"")
    => "\b")

  (check
      (parse "\\t\"")
    => "\t")

  (check
      (parse "\\n\"")
    => "\n")

  (check
      (parse "\\v\"")
    => "\v")

  (check
      (parse "\\f\"")
    => "\f")

  (check
      (parse "\\r\"")
    => "\r")

  (check
      (parse "\\\"\"")
    => "\"")

  (check
      (parse "\\\\\"")
    => "\\")

  (check
      (parse "inizio\\\"/\\b\\f\\n\\r\\tfine\"")
    => "inizio\"/\b\f\n\r\tfine")

  (check
      (parse "\\x005C;\"")
    => "\\")

  (check
      (parse "\\xA;\"")
    => "\n")

  (check
      (parse "x\\xA;x\"")
    => "x\xA;x")

  (check
      (parse "x\\xA;\\x9;\"")
    => "x\xA;\x9;")

  (check
      (parse "\\x0063;\\x0069;\\x0061;\\x006f;\"")
    => "\x0063;\x0069;\x0061;\x006f;")

  (check	;a string
      (parse "ciao\"")
    => "ciao")

  (check
      ;;Nested double quotes.  The Scheme string "\\\"" is seen as \" by
      ;;the lexer and the backslash quoting character is removed.
      (parse "ciao \\\"hello\\\" salut\"")
    => "ciao \"hello\" salut")

  (check	;intraline space
      (parse "ciao \\\nmamma\"")
    => "ciao mamma")

  (check	;intraline space
      (parse "ciao \
mamma\"")
    => "ciao mamma")

;;; ------------------------------------------------------------
;;; the following are from the R6RS document

  (check
      (parse "\\x41;bc\"")
    => "\x41;bc")

  (check
      (parse "\\x41; bc\"")
    => "\x41; bc")


  (check
      (parse "\\x41bc;\"")
    => "\x41bc;")

  (check
      (guard (E ((lexical-violation? E)
;;;		 (display (condition-message E))(newline)
		 (condition-irritants E))
		(else E))
	(parse "\\x41"))
    => '("\\x41"))

  (check
      (guard (E ((lexical-violation? E)
;;;		 (display (condition-message E))(newline)
		 (condition-irritants E))
		(else E))
	(parse "\\x;"))
    => '("\\x;"))

  (check
      (guard (E ((lexical-violation? E)
;;;		 (display (condition-message E))(newline)
		 (condition-irritants E))
		(else E))
	(parse "\\x41bx;"))
    => '("\\x41bx;"))

  (check
      (parse "\\x00000041;\"")
    => "\x00000041;")

  (check
      (parse "\\x0010FFFF;\"")
    => "\x0010FFFF;")

  (check	;inline-hex-escape out of range
      (guard (E ((lexical-violation? E)
;;;		 (display (condition-message E))(newline)
		 (condition-irritants E))
		(else E))
	(parse "\\x00110000;"))
    => '("\\x00110000;"))

  (check
      (parse "\\x000000001;\"")
    => "\x000000001;")

  (check	;&lexical exception, in excluded range
      (guard (E ((lexical-violation? E)
;;;		 (display (condition-message E))(newline)
		 (condition-irritants E))
		(else E))
	(parse "\\xD800;"))
    => '("\\xD800;"))

  (check	;&lexical exception, in excluded range
      (guard (E ((lexical-violation? E)
;;;		 (display (condition-message E))(newline)
		 (condition-irritants E))
		(else E))
	(parse "\\xDFFF;"))
    => '("\\xDFFF;"))

  #t)


(parametrise ((check-test-name	'nested-comment-tokeniser))

  (define (tokenise string)
    (let* ((IS		(lexer-make-IS (string: string) (counters: 'all)))
	   (lexer	(lexer-make-lexer r6.r6rs-nested-comment-lexer-table IS))
	   (result	'()))
      (do (((T <lexical-token>) (lexer) (lexer)))
	  (T.special?
	   (reverse `(,(if (is-a? T <lexical-token>)
			   T.category
			 T)
		      . ,result)))
	(set-cons! result T))))

  (check
      (tokenise "#||#")
    => '(OPEN CLOSE *eoi*))

  (check
      (tokenise "#|# |#")
    => '(OPEN #\# #\space CLOSE *eoi*))

  (check
      (tokenise "#|||#")
    => '(OPEN #\| CLOSE *eoi*))

  (check
      (tokenise "#|ciao|#")
    => '(OPEN #\c #\i #\a #\o CLOSE *eoi*))

  (check
      (tokenise "#|ciao\nmamma|#")
    => '(OPEN #\c #\i #\a #\o #\newline #\m #\a #\m #\m #\a CLOSE *eoi*))

  (check
      (tokenise "#|#|#|#|")
    => '(OPEN OPEN OPEN OPEN *eoi*))

  (check
      (tokenise "|#|#|#|#")
    => '(CLOSE CLOSE CLOSE CLOSE *eoi*))

  #t)


(parametrise ((check-test-name	'nested-comment-reader))

  (define (parse string)
    (r6.read-nested-comment (lexer-make-IS (string: string) (counters: 'all))))

  (define-syntax identity
    (syntax-rules ()
      ((_ ?string)
       (check
	   (parse ?string)
	 => (string-append "#|" ?string)))))

  (identity "|#")
  (identity "#||#|#")
  (identity "ciao |#")
  (identity "ciao #| mamma |# ciao |#")

  (check
      (guard (E ((lexical-violation? E)
;;;		 (display (condition-message E))(newline)
		 (condition-irritants E))
		(else E))
	(parse ""))
    => (list (eof-object)))

  (check
      (guard (E ((lexical-violation? E)
;;;		 (display (condition-message E))(newline)
		 (condition-irritants E))
		(else E))
	(parse "ciao #| mamma |#"))
    => (list (eof-object)))

  #t)


(parametrise ((check-test-name	'line-comment-tokeniser))

  (define (tokenise string)
    (let* ((IS		(lexer-make-IS (string: string) (counters: 'all)))
	   (lexer	(lexer-make-lexer r6.r6rs-line-comment-lexer-table IS))
	   (result	'()))
      (do (((T <lexical-token>) (lexer) (lexer)))
	  (T.special?
	   (reverse `(,(if (is-a? T <lexical-token>)
			   T.category
			 T)
		      . ,result)))
	(set-cons! result T))))

  (define-syntax identity
    (syntax-rules ()
      ((_ ?string)
       (check
	   (tokenise ?string)
	 => '(?string *eoi*)))))

;;; --------------------------------------------------------------------

  (check
      (tokenise "")
    => '(*eoi*))

  (identity ";\n")
  (identity ";;\n")
  (identity ";;;\n")
  (identity ";;; ciao\n")
  (identity ";;; ciao\r")

  ;; no line end
  (identity ";")
  (identity ";;")
  (identity ";;;")
  (identity ";;; ciao")
  (identity ";;; ciao")

  #t)


(parametrise ((check-test-name	'line-comment-reader))

  (define (parse string)
    (r6.read-line-comment (lexer-make-IS (string: string) (counters: 'all))))

  (define-syntax identity
    (syntax-rules ()
      ((_ ?string)
       (check
	   (parse ?string)
	 => ?string))))

;;; --------------------------------------------------------------------

  (identity ";\n")
  (identity ";;\n")
  (identity ";;;\n")
  (identity ";;; ciao\n")
  (identity ";;; ciao\r")

  ;; no line ending
  (identity ";")
  (identity ";;")
  (identity ";;;")
  (identity ";;; ciao")

  #t)


(parametrise ((check-test-name	'character-tokeniser))

  (define (tokenise string)
    (let* ((IS		(lexer-make-IS (string: string) (counters: 'all)))
	   (lexer	(lexer-make-lexer r6.r6rs-character-lexer-table IS))
	   (result	'()))
      (do (((T <lexical-token>) (lexer) (lexer)))
	  (T.special?
	   (reverse `(,(if (is-a? T <lexical-token>)
			   T.category
			 T)
		      . ,result)))
	(set-cons! result T))))

  (check
      (tokenise "")
    => '(*eoi*))

  (check
      (tokenise "#\\1")
    => '(#\1 *eoi*))

  (check
      (tokenise "#\\A")
    => '(#\A *eoi*))

;;; --------------------------------------------------------------------

  (check
      (tokenise "#\\x005C")
    => '(#\x005C *eoi*))

  (check
      (tokenise "#\\xA")
    => '(#\xA *eoi*))

  (check
      (tokenise "#\\x0063")
    => '(#\c *eoi*))

;;; --------------------------------------------------------------------

  (check
      (tokenise "#\\nul")
    => '(#\nul *eoi*))

  (check
      (tokenise "#\\alarm")
    => '(#\alarm *eoi*))

  (check
      (tokenise "#\\backspace")
    => '(#\backspace *eoi*))

  (check
      (tokenise "#\\tab")
    => '(#\tab *eoi*))

  (check
      (tokenise "#\\linefeed")
    => '(#\linefeed *eoi*))

  (check
      (tokenise "#\\newline")
    => '(#\newline *eoi*))

  (check
      (tokenise "#\\vtab")
    => '(#\vtab *eoi*))

  (check
      (tokenise "#\\page")
    => '(#\page *eoi*))

  (check
      (tokenise "#\\return")
    => '(#\return *eoi*))

  (check
      (tokenise "#\\esc")
    => '(#\esc *eoi*))

  (check
      (tokenise "#\\space")
    => '(#\space *eoi*))

  (check
      (tokenise "#\\delete")
    => '(#\delete *eoi*))

;;; --------------------------------------------------------------------
;;; errors

  (check
      (guard (E ((lexical-violation? E)
		 (condition-irritants E))
		(else E))
	(tokenise "c"))
    => '(*lexer-error*))

  (check
      (guard (E ((lexical-violation? E)
		 (condition-irritants E))
		(else E))
	(tokenise "#\\ciao"))
    => '(*lexer-error*))

  #t)


(parametrise ((check-test-name	'character-reader))

  (define (parse string)
    (r6.read-character (lexer-make-IS (string: string) (counters: 'all))))

  (check
      (parse "#\\1")
    => #\1)

  (check
      (parse "#\\A")
    => #\A)

;;; --------------------------------------------------------------------

  (check
      (parse "#\\x005C")
    => #\x005C)

  (check
      (parse "#\\xA")
    => #\xA)

  (check
      (parse "#\\x0063")
    => #\c)

;;; --------------------------------------------------------------------

  (check
      (parse "#\\nul")
    => #\nul)

  (check
      (parse "#\\alarm")
    => #\alarm)

  (check
      (parse "#\\backspace")
    => #\backspace)

  (check
      (parse "#\\tab")
    => #\tab)

  (check
      (parse "#\\linefeed")
    => #\linefeed)

  (check
      (parse "#\\newline")
    => #\newline)

  (check
      (parse "#\\vtab")
    => #\vtab)

  (check
      (parse "#\\page")
    => #\page)

  (check
      (parse "#\\return")
    => #\return)

  (check
      (parse "#\\esc")
    => #\esc)

  (check
      (parse "#\\space")
    => #\space)

  (check
      (parse "#\\delete")
    => #\delete)

;;; --------------------------------------------------------------------
;;; errors

  (check
      (guard (E ((lexical-violation? E)
;;;		 (display (condition-message E))(newline)
		 (condition-irritants E))
		(else E))
	(parse ""))
    => `(,(eof-object)))

  (check
      (guard (E ((lexical-violation? E)
;;;		 (display (condition-message E))(newline)
		 (condition-irritants E))
		(else E))
	(parse "c"))
    => '("c"))

  (check
      (guard (E ((lexical-violation? E)
;;;		 (display (condition-message E))(newline)
		 (condition-irritants E))
		(else E))
	(parse "#\\ciao"))
    => '("#\\ciao"))

  #t)


(parametrise ((check-test-name	'identifier-tokeniser))

  (define (tokenise string)
    (let* ((IS		(lexer-make-IS (string: string) (counters: 'all)))
	   (lexer	(lexer-make-lexer r6.r6rs-identifier-lexer-table IS))
	   (result	'()))
      (do (((T <lexical-token>) (lexer) (lexer)))
	  (T.special?
	   (reverse `(,(if (is-a? T <lexical-token>)
			   T.category
			 T)
		      . ,result)))
	(set-cons! result T))))

  (check
      (tokenise "")
    => '(*eoi*))

  (check
      (tokenise "c")
    => '("c" *eoi*))

  (check
      (tokenise "ciao")
    => '("ciao" *eoi*))

  (check
      (tokenise "ciao-mamma")
    => '("ciao-mamma" *eoi*))

  (check
      (tokenise "->ciao")
    => '("->ciao" *eoi*))

  (check
      (tokenise "?ciao")
    => '("?ciao" *eoi*))

  (check
      (tokenise "?")
    => '("?" *eoi*))

  (check
      (tokenise "+")
    => '("+" *eoi*))

  (check
      (tokenise "-")
    => '("-" *eoi*))

  (check
      (tokenise "...")
    => '("..." *eoi*))

  (check
      (tokenise "->")
    => '("->" *eoi*))

  (check
      (tokenise "ciao123")
    => '("ciao123" *eoi*))

  (check
      (tokenise "ciao123+-.@")
    => '("ciao123+-.@" *eoi*))

  (check
      (tokenise "ciao123+-.@ciao")
    => '("ciao123+-.@ciao" *eoi*))

  (check	;char with Unicode category Nd
      (tokenise "ciao\\x0CE7;mamma")
    => `("ciao\\x0CE7;mamma" *eoi*))

  (check	;char with Unicode category Mc
      (tokenise "ciao\\x09BF;mamma")
    => `("ciao\\x09BF;mamma" *eoi*))

  (check	;char with Unicode category Me
      (tokenise "ciao\\x20E4;mamma")
    => `("ciao\\x20E4;mamma" *eoi*))

;;; --------------------------------------------------------------------

  (check
      (tokenise "@this")
    => '(*lexer-error*))

  (check
      (tokenise "123")
    => '(*lexer-error*))

  #t)


(parametrise ((check-test-name	'identifier-reader))

  (define (parse string)
    (r6.read-identifier (lexer-make-IS (string: string) (counters: 'all))))

  (check
      (parse "c")
    => "c")

  (check
      (parse "ciao")
    => "ciao")

  (check
      (parse "ciao-mamma")
    => "ciao-mamma")

  (check
      (parse "->ciao")
    => "->ciao")

  (check
      (parse "?ciao")
    => "?ciao")

  (check
      (parse "?")
    => "?")

  (check
      (parse "+")
    => "+")

  (check
      (parse "-")
    => "-")

  (check
      (parse "...")
    => "...")

  (check
      (parse "->")
    => "->")

  (check
      (parse "ciao123")
    => "ciao123")

  (check
      (parse "ciao123+-.@")
    => "ciao123+-.@")

  (check
      (parse "ciao123+-.@ciao")
    => "ciao123+-.@ciao")

  (check	;char with Unicode category Nd
      (parse "ciao\\x0CE7;mamma")
    => "ciao\\x0CE7;mamma")

  (check	;char with Unicode category Mc
      (parse "ciao\\x09BF;mamma")
    => "ciao\\x09BF;mamma")

  (check	;char with Unicode category Me
      (parse "ciao\\x20E4;mamma")
    => "ciao\\x20E4;mamma")

;;; --------------------------------------------------------------------

  (check
      (guard (E ((lexical-violation? E)
;;;		 (display (condition-message E)) (newline)
		 (condition-irritants E))
		(else E))
	(parse ""))
    => `(,(eof-object)))

  (check
      (guard (E ((lexical-violation? E)
;;;		 (display (condition-message E)) (newline)
		 (condition-irritants E))
		(else E))
	(parse "@this"))
    => '("@this"))

  (check
      (guard (E ((lexical-violation? E)
;;;		 (display (condition-message E)) (newline)
		 (condition-irritants E))
		(else E))
	(parse "123"))
    => '("123"))

  #t)


(parametrise ((check-test-name	'number-tokeniser))

  (define (tokenise string)
    (let* ((IS		(lexer-make-IS (string: string) (counters: 'all)))
	   (lexer	(lexer-make-lexer r6.r6rs-number-lexer-table IS))
	   (result	'()))
      (do (((T <lexical-token>) (lexer) (lexer)))
	  (T.special?
	   (reverse `(,(if (is-a? T <lexical-token>)
			   T.category
			 T)
		      . ,result)))
	(set-cons! result T))))

  (check
      (tokenise "123")
    => '(123 *eoi*))

  (check
      (tokenise "10")
    => '(10 *eoi*))

  (check
      (tokenise "#e10")
    => '(#e10 *eoi*))

  (check
      (tokenise "#i10")
    => '(#i10 *eoi*))

  #t)


(parametrise ((check-test-name	'number-reader))

  (define (parse string)
    (r6.read-number (lexer-make-IS (string: string) (counters: 'all))))

  (define (generated-tests)
    ;;DO NOT TRY TO CONVERT THIS FUNCTION TO A MACRO!!!  I already tried
    ;;and it generates so much code that it fills up the memory.
    ;;
    (define (gen ls1 ls2 comp1 comp2)
      (apply append (map (lambda (x1)
			   (map (lambda (x2)
				  (cons (comp1 (car x1) (car x2))
					(comp2 (cdr x1) (cdr x2))))
			     ls2))
		      ls1)))
    (define (gensa ls1 ls2 comp)
      (gen ls1 ls2 string-append comp))
    (define suffixed-int
      '(("0" . 0)
	("1" . 1)
	("1." . 1.0)
	("1.0" . 1.0)
	(".5" . 0.5)
	("0.5" . 0.5)))
    (define exponents
      '(("e0" . 1.0)
	("e+0" . 1.0)
	("e-0" . 1.0)
	("e-1" . 0.1)))
    (define decimal10
      (append suffixed-int
	      (gensa suffixed-int exponents *)))
    (define naninf
      '(("nan.0" . +nan.0)
	("inf.0" . +inf.0)))
    (define ureal
      (append
       decimal10
       (gensa decimal10 '(("|53" . #f)) (lambda (x _) (inexact x)))))
    (define sign
      '(("+" . +1)
	("-" . -1)))
;;; <real> = <sign> <ureal>
;;;        | + <naninf>
;;;        | - <naninf>
    (define sreal
      (append (gensa sign ureal *)
	      (gensa sign naninf *)))
    (define real
      (append ureal sreal))
;;;<complex> = <real>
;;;          | <real> @ <real>
;;;          | <real> <creal>
;;;          | <creal>
;;; <creal> = <seal> i
;;;         | +i
;;;         | -i
    (define comps
      (append (gensa sreal '(("i" . #f)) (lambda (x f) x))
	      '(("+i" . 1)
		("-i" . -1))))
    (define creal
      (map (lambda (x) (cons (car x) (make-rectangular 0 (cdr x)))) comps))
    (define complex
      (append real creal
	      (gensa real comps make-rectangular)
	      (gen real real (lambda (x y) (string-append x "@" y)) make-polar)))
    (display (string-append "generating " (number->string (length complex)) " number tests\n"))
    (map (lambda (x)
;;;	   (write `(test ,(car x) ,(cdr x)))(newline)
	   (test (car x) (cdr x)))
      complex))

;;; --------------------------------------------------------------------

  (define-syntax test
    (syntax-rules ()
      ((_ ?string ?expected)
       (check
	   (parse ?string)
	 (=> equal-number-results?) ?expected))))

  (define-syntax test-lexical-error
    (syntax-rules ()
      ((_ ?string)
       (check
	   (guard (E ((lexical-violation? E)
;;;		      (display (condition-message E))(newline)
		      (condition-irritants E))
		     (else E))
	     (parse ?string))
	 => '(?string)))))

;;; --------------------------------------------------------------------

  (test "10" 10)
  (test "1" 1)
  (test "-17" -17)
  (test "12" 12)
  (test "+12" +12)
  (test "-12" -12)
  (test "+13476238746782364786237846872346782364876238477" 13476238746782364786237846872346782364876238477)
  (test "+inf.0" inf+)
  (test "-inf.0" inf-)
  (test "+i" (make-rectangular 0 +1))
  (test "-i" (make-rectangular 0 -1))
  (test "+15i" (make-rectangular 0 +15))
  (test "-15i" (make-rectangular 0 -15))
  (test "12/7" (/ 12 7))
  (test "-12/7" (/ -12 7))
  (test "+12/7" (/ 12 7))
  (test "-12/7i" (make-rectangular 0 (/ -12 7)))
  (test "+12/7i" (make-rectangular 0 (/ 12 7)))
  (test "12/7+7i" (make-rectangular (/ 12 7) (/ 7 1)))
  (test "12/7+7/5i" (make-rectangular (/ 12 7) (/ 7 5)))
  (test "12/7-7/5i" (make-rectangular (/ 12 7) (/ -7 5)))
  (test "12." (inexact 12))
  (test "#e12." 12)
  (test "12.5" (inexact (/ 125 10)))
  (test "#e12.5123" (/ 125123 10000))
  (test "#i125123/10000" (inexact (/ 125123 10000)))
  (test "+inf.0i" (make-rectangular 0 inf+))
  (test "-inf.0i" (make-rectangular 0 inf-))

  (test "1/2" (/ 1 2))
  (test "-1/2" (/ 1 -2))
  (test "#x24" 36)
  (test "#x-24" -36)
  (test "#b+00000110110" 54)
  (test "#b-00000110110/10" -27)
  (test "#e10" 10)
  (test "#e1" 1)
  (test "#e-17" -17)
  (test "#e#x24" 36)
  (test "#e#x-24" -36)
  (test "#e#b+00000110110" 54)
  (test "#e#b-00000110110/10" -27)
  (test "#x#e24" 36)
  (test "#x#e-24" -36)
  (test "#b#e+00000110110" 54)
  (test "#b#e-00000110110/10" -27)
  (test "#e1e1000" (expt 10 1000))
  (test "#e-1e1000" (- (expt 10 1000)))
  (test "#e1e-1000" (expt 10 -1000))
  (test "#e-1e-1000" (- (expt 10 -1000)))
  (test "#i1e100" (inexact (expt 10 100)))
  (test "#i1e1000" (inexact (expt 10 1000)))
  (test "#i-1e1000" (inexact (- (expt 10 1000))))
  (test "1e100" (inexact (expt 10 100)))
  (test "1.0e100" (inexact (expt 10 100)))
  (test "1.e100" (inexact (expt 10 100)))
  (test "0.1e100" (inexact (expt 10 99)))
  (test ".1e100" (inexact (expt 10 99)))
  (test "+1e100" (inexact (expt 10 100)))
  (test "+1.0e100" (inexact (expt 10 100)))
  (test "+1.e100" (inexact (expt 10 100)))
  (test "+0.1e100" (inexact (expt 10 99)))
  (test "+.1e100" (inexact (expt 10 99)))
  (test "-1e100" (inexact   (- (expt 10 100))))
  (test "-1.0e100" (inexact (- (expt 10 100))))
  (test "-1.e100" (inexact  (- (expt 10 100))))
  (test "-0.1e100" (inexact (- (expt 10 99))))
  (test "-.1e100" (inexact  (- (expt 10 99))))

  (test "8+6.0i" (make-rectangular 8 6.0))
  (test "8.0+6i" (make-rectangular 8.0 6))
  (test "+8+6.0i" (make-rectangular 8 6.0))
  (test "+8.0+6i" (make-rectangular 8.0 6))
  (test "-8+6.0i" (make-rectangular -8 6.0))
  (test "-8.0+6i" (make-rectangular -8.0 6))

  (test "8-6.0i" (make-rectangular 8 -6.0))
  (test "8.0-6i" (make-rectangular 8.0 -6))
  (test "+8-6.0i" (make-rectangular 8 -6.0))
  (test "+8.0-6i" (make-rectangular 8.0 -6))
  (test "-8-6.0i" (make-rectangular -8 -6.0))
  (test "-8.0-6i" (make-rectangular -8.0 -6))

  (test "+0i" 0)
  (test "-0i" 0)

  (test "+1i" (make-rectangular 0 1))
  (test "-1i" (make-rectangular 0 -1))

  (test "8+nan.0i" (make-rectangular 8 +nan.0))
  (test "8.0+nan.0i" (make-rectangular 8.0 +nan.0))
  (test "+8+nan.0i" (make-rectangular 8 +nan.0))
  (test "+8.0+nan.0i" (make-rectangular 8.0 +nan.0))
  (test "-8+nan.0i" (make-rectangular -8 +nan.0))
  (test "-8.0+nan.0i" (make-rectangular -8.0 +nan.0))
  (test "8-nan.0i" (make-rectangular 8 -nan.0))
  (test "8.0-nan.0i" (make-rectangular 8.0 -nan.0))
  (test "+8-nan.0i" (make-rectangular 8 -nan.0))
  (test "+8.0-nan.0i" (make-rectangular 8.0 -nan.0))
  (test "-8-nan.0i" (make-rectangular -8 -nan.0))
  (test "-8.0-nan.0i" (make-rectangular -8.0 -nan.0))
  (test "+nan.0+6.0i" (make-rectangular +nan.0 6.0))
  (test "+nan.0+6i" (make-rectangular +nan.0 6))
  (test "+nan.0+6.0i" (make-rectangular +nan.0 6.0))
  (test "+nan.0+6i" (make-rectangular +nan.0 6))
  (test "-nan.0+6.0i" (make-rectangular -nan.0 6.0))
  (test "-nan.0+6i" (make-rectangular -nan.0 6))
  (test "+nan.0-6.0i" (make-rectangular +nan.0 -6.0))
  (test "+nan.0-6i" (make-rectangular +nan.0 -6))
  (test "+nan.0-6.0i" (make-rectangular +nan.0 -6.0))
  (test "+nan.0-6i" (make-rectangular +nan.0 -6))
  (test "-nan.0-6.0i" (make-rectangular -nan.0 -6.0))
  (test "-nan.0-6i" (make-rectangular -nan.0 -6))
  (test "+nan.0+nan.0i" (make-rectangular +nan.0 +nan.0))
  (test "+nan.0-nan.0i" (make-rectangular +nan.0 -nan.0))
  (test "-nan.0+nan.0i" (make-rectangular -nan.0 +nan.0))
  (test "-nan.0-nan.0i" (make-rectangular -nan.0 -nan.0))

  (test "+nan.0+i" (make-rectangular +nan.0 +1))
  (test "+nan.0-i" (make-rectangular +nan.0 -1))
  (test "-nan.0+i" (make-rectangular -nan.0 +1))
  (test "-nan.0-i" (make-rectangular -nan.0 -1))

  (test "8+inf.0i" (make-rectangular 8 +inf.0))
  (test "8.0+inf.0i" (make-rectangular 8.0 +inf.0))
  (test "+8+inf.0i" (make-rectangular 8 +inf.0))
  (test "+8.0+inf.0i" (make-rectangular 8.0 +inf.0))
  (test "-8+inf.0i" (make-rectangular -8 +inf.0))
  (test "-8.0+inf.0i" (make-rectangular -8.0 +inf.0))
  (test "8-inf.0i" (make-rectangular 8 -inf.0))
  (test "8.0-inf.0i" (make-rectangular 8.0 -inf.0))
  (test "+8-inf.0i" (make-rectangular 8 -inf.0))
  (test "+8.0-inf.0i" (make-rectangular 8.0 -inf.0))
  (test "-8-inf.0i" (make-rectangular -8 -inf.0))
  (test "-8.0-inf.0i" (make-rectangular -8.0 -inf.0))
  (test "+inf.0+6.0i" (make-rectangular +inf.0 6.0))
  (test "+inf.0+6i" (make-rectangular +inf.0 6))
  (test "+inf.0+6.0i" (make-rectangular +inf.0 6.0))
  (test "+inf.0+6i" (make-rectangular +inf.0 6))
  (test "-inf.0+6.0i" (make-rectangular -inf.0 6.0))
  (test "-inf.0+6i" (make-rectangular -inf.0 6))
  (test "+inf.0-6.0i" (make-rectangular +inf.0 -6.0))
  (test "+inf.0-6i" (make-rectangular +inf.0 -6))
  (test "+inf.0-6.0i" (make-rectangular +inf.0 -6.0))
  (test "+inf.0-6i" (make-rectangular +inf.0 -6))
  (test "-inf.0-6.0i" (make-rectangular -inf.0 -6.0))
  (test "-inf.0-6i" (make-rectangular -inf.0 -6))
  (test "+inf.0+inf.0i" (make-rectangular +inf.0 +inf.0))
  (test "+inf.0-inf.0i" (make-rectangular +inf.0 -inf.0))
  (test "-inf.0+inf.0i" (make-rectangular -inf.0 +inf.0))
  (test "-inf.0-inf.0i" (make-rectangular -inf.0 -inf.0))

  (test "+inf.0+i" (make-rectangular +inf.0 +1))
  (test "+inf.0-i" (make-rectangular +inf.0 -1))
  (test "-inf.0+i" (make-rectangular -inf.0 +1))
  (test "-inf.0-i" (make-rectangular -inf.0 -1))

  (test "8+6e20i" (make-rectangular +8 +6e20))
  (test "8-6e20i" (make-rectangular +8 -6e20))
  (test "8e20+6i" (make-rectangular +8e20 +6))
  (test "8e20-6i" (make-rectangular +8e20 -6))
  (test "+8+6e20i" (make-rectangular +8 +6e20))
  (test "+8-6e20i" (make-rectangular +8 -6e20))
  (test "+8e20+6i" (make-rectangular +8e20 +6))
  (test "+8e20-6i" (make-rectangular +8e20 -6))
  (test "-8+6e20i" (make-rectangular -8 +6e20))
  (test "-8-6e20i" (make-rectangular -8 -6e20))
  (test "-8e20+6i" (make-rectangular -8e20 +6))
  (test "-8e20-6i" (make-rectangular -8e20 -6))

  (test "8e10+6e20i" (make-rectangular +8e10 +6e20))
  (test "8e10-6e20i" (make-rectangular +8e10 -6e20))
  (test "+8e10+6e20i" (make-rectangular +8e10 +6e20))
  (test "+8e10-6e20i" (make-rectangular +8e10 -6e20))
  (test "-8e10+6e20i" (make-rectangular -8e10 +6e20))
  (test "-8e10-6e20i" (make-rectangular -8e10 -6e20))

  (test "-0e-10" -0.0)
  (test "-0e-0" -0.0)
  (test "#d-0e-10-0e-0i" (make-rectangular -0.0 -0.0))
  (test "-0.i" (make-rectangular 0.0 -0.0))
  (test "#d#e-0.0f-0-.0s-0i" 0)

  (test "+.234e4i" (make-rectangular 0 0.234e4))
  (test "+.234e-5i" (make-rectangular 0 0.234e-5))
  (test "+.234i" (make-rectangular 0 0.234))

  (cond-expand
   ;;Petite Chez Scheme has some  problems with these tests: they may be
   ;;related to  a known  problem with exponentials  (Tue Jan  4, 2011).
   ;;Exclude Petite so that running the  tests with it is fast (good for
   ;;debugging).
   ((or petite mosh) #f)
   (else
    (generated-tests)))

;;; --------------------------------------------------------------------

  (test-lexical-error "0i")
  (test-lexical-error "1i+")
  (test-lexical-error "12/7i")
  (test-lexical-error "i")
  (test-lexical-error "/")
  (test-lexical-error "12/0")
  (test-lexical-error "+12/0")
  (test-lexical-error "-12/0")
  (test-lexical-error "12/0000")
  (test-lexical-error "+12/0000")
  (test-lexical-error "-12/0000")
  (test-lexical-error "12+")
  (test-lexical-error "+12+")
  (test-lexical-error "-12+")
  (test-lexical-error "12+")
  (test-lexical-error "+12+")
  (test-lexical-error "-12+")

  #t)


(parametrise ((check-test-name	'full-table-identifiers)
	      (debugging	#f))

  (define (tokenise string)
    (let* ((IS		(lexer-make-IS (string: string) (counters: 'all)))
	   (lexer	(lexer-make-lexer r6.r6rs-lexer-table IS))
	   (result	'()))
      (do (((T <lexical-token>) (lexer) (lexer)))
	  (T.special?
	   (reverse `(,(if T.lexer-error?
			   `(,T.category ,T.value)
			 T.category). ,result)))
	(debug "id token ~s >>~s<<" T.category T.value)
	(set-cons! result (list T.category T.value)))))

  (check
      (tokenise "")
    => '(*eoi*))

  (check
      (tokenise "c")
    => '((IDENTIFIER "c") *eoi*))

  (check
      (tokenise "ciao")
    => '((IDENTIFIER "ciao") *eoi*))

  (check
      (tokenise "ciao-mamma")
    => '((IDENTIFIER "ciao-mamma") *eoi*))

  (check
      (tokenise "->ciao")
    => '((IDENTIFIER "->ciao") *eoi*))

  (check
      (tokenise "?ciao")
    => '((IDENTIFIER "?ciao") *eoi*))

  (check
      (tokenise "?")
    => '((IDENTIFIER "?") *eoi*))

  (check
      (tokenise "+")
    => '((IDENTIFIER "+") *eoi*))

  (check
      (tokenise "-")
    => '((IDENTIFIER "-") *eoi*))

  (check
      (tokenise "...")
    => '((IDENTIFIER "...") *eoi*))

  (check
      (tokenise "->")
    => '((IDENTIFIER "->") *eoi*))

  (check
      (tokenise "ciao123")
    => '((IDENTIFIER "ciao123") *eoi*))

  (check
      (tokenise "ciao123+-.@")
    => '((IDENTIFIER "ciao123+-.@") *eoi*))

  (check
      (tokenise "ciao123+-.@ciao")
    => '((IDENTIFIER "ciao123+-.@ciao") *eoi*))

  (check	;char with Unicode category Nd
      (tokenise "ciao\\x0CE7;mamma")
    => '((IDENTIFIER "ciao\\x0CE7;mamma") *eoi*))

  (check	;char with Unicode category Mc
      (tokenise "ciao\\x09BF;mamma")
    => '((IDENTIFIER "ciao\\x09BF;mamma") *eoi*))

  (check	;char with Unicode category Me
      (tokenise "ciao\\x20E4;mamma")
    => '((IDENTIFIER "ciao\\x20E4;mamma") *eoi*))

;;; --------------------------------------------------------------------

  (check
      (tokenise "@this")
    => '((*lexer-error* "@this")))

  #t)


(parametrise ((check-test-name	'full-table-characters)
	      (debugging	#f))

  (define (tokenise string)
    (let* ((IS		(lexer-make-IS (string: string) (counters: 'all)))
	   (lexer	(lexer-make-lexer r6.r6rs-lexer-table IS))
	   (result	'()))
      (do (((T <lexical-token>) (lexer) (lexer)))
	  (T.special?
	   (reverse `(,(if T.lexer-error?
			   `(,T.category ,T.value)
			 T.category). ,result)))
	(debug "char token ~s >>~s<<" T.category T.value)
	(set-cons! result (list T.category T.value)))))

  (check
      (tokenise "")
    => '(*eoi*))

  (check
      (tokenise "#\\1")
    => '((CHARACTER #\1) *eoi*))

  (check
      (tokenise "#\\A")
    => '((CHARACTER #\A) *eoi*))

;;; --------------------------------------------------------------------

  (check
      (tokenise "#\\x005C")
    => '((CHARACTER #\x005C) *eoi*))

  (check
      (tokenise "#\\xA")
    => '((CHARACTER #\xA) *eoi*))

  (check
      (tokenise "#\\x0063")
    => '((CHARACTER #\c) *eoi*))

;;; --------------------------------------------------------------------

  (check
      (tokenise "#\\nul")
    => '((CHARACTER #\nul) *eoi*))

  (check
      (tokenise "#\\alarm")
    => '((CHARACTER #\alarm) *eoi*))

  (check
      (tokenise "#\\backspace")
    => '((CHARACTER #\backspace) *eoi*))

  (check
      (tokenise "#\\tab")
    => '((CHARACTER #\tab) *eoi*))

  (check
      (tokenise "#\\linefeed")
    => '((CHARACTER #\linefeed) *eoi*))

  (check
      (tokenise "#\\newline")
    => '((CHARACTER #\newline) *eoi*))

  (check
      (tokenise "#\\vtab")
    => '((CHARACTER #\vtab) *eoi*))

  (check
      (tokenise "#\\page")
    => '((CHARACTER #\page) *eoi*))

  (check
      (tokenise "#\\return")
    => '((CHARACTER #\return) *eoi*))

  (check
      (tokenise "#\\esc")
    => '((CHARACTER #\esc) *eoi*))

  (check
      (tokenise "#\\space")
    => '((CHARACTER #\space) *eoi*))

  (check
      (tokenise "#\\delete")
    => '((CHARACTER #\delete) *eoi*))

;;; --------------------------------------------------------------------
;;; errors

  (check
      (guard (E ((lexical-violation? E)
		 (condition-irritants E))
		(else E))
	(tokenise "#\\ciao"))
    => '((*lexer-error* "#\\ciao")))

  #t)


(parametrise ((check-test-name	'full-table-numbers)
	      (debugging	#f))

  (define (tokenise string)
    (let* ((IS		(lexer-make-IS (string: string) (counters: 'all)))
	   (lexer	(lexer-make-lexer r6.r6rs-lexer-table IS))
	   (result	'()))
      (do (((T <lexical-token>) (lexer) (lexer)))
	  (T.special?
	   (reverse `(,(if T.lexer-error?
			   `(,T.category ,T.value)
			 T.category). ,result)))
	(debug "number token ~s >>~s<<" T.category T.value)
	(set-cons! result (list T.category T.value)))))

  (define (equal-results? a b)
    (and (= (length a) (length b))
	 (for-all (lambda (la lb)
		    (or (eq? la lb)
			(and (eq? (car la) (car lb))
			     (equal-number-results? (cadr la) (cadr lb)))))
	   a b)))

  (define-syntax test
    (syntax-rules ()
      ((_ ?string ?expected)
       (check
	   (tokenise ?string)
	 (=> equal-results?)
	 `((NUMBER ,?expected) *eoi*)))
      ((_ ?name ?string ?expected)
       (check ?name
	   (tokenise ?string)
	 (=> equal-results?)
	 `((NUMBER ,?expected) *eoi*)))
      ))

  (define-syntax test-lexical-error
    (syntax-rules ()
      ((_ ?string)
       (check
	   (guard (E ((lexical-violation? E)
;;;		      (display (condition-message E))(newline)
		      (condition-irritants E))
		     (else E))
	     (tokenise ?string))
	 => '((*lexer-error* ?string))))
      ((_ ?name ?string)
       (check ?name
	   (guard (E ((lexical-violation? E)
;;;		      (display (condition-message E))(newline)
		      (condition-irritants E))
		     (else E))
	     (tokenise ?string))
	 => '((*lexer-error* ?string))))
      ))

;;; --------------------------------------------------------------------

  (check
      (tokenise "")
    => '(*eoi*))

;;; --------------------------------------------------------------------

  (test "10" 10)
  (test "1" 1)
  (test "-17" -17)
  (test "12" 12)
  (test "+12" +12)
  (test "-12" -12)
  (test "+13476238746782364786237846872346782364876238477" 13476238746782364786237846872346782364876238477)
  (test "+inf.0" inf+)
  (test "-inf.0" inf-)
  (test "+i" (make-rectangular 0 +1))
  (test "-i" (make-rectangular 0 -1))
  (test "+15i" (make-rectangular 0 +15))
  (test "-15i" (make-rectangular 0 -15))
  (test "12/7" (/ 12 7))
  (test "-12/7" (/ -12 7))
  (test "+12/7" (/ 12 7))
  (test "-12/7i" (make-rectangular 0 (/ -12 7)))
  (test "+12/7i" (make-rectangular 0 (/ 12 7)))
  (test "12/7+7i" (make-rectangular (/ 12 7) (/ 7 1)))
  (test "12/7+7/5i" (make-rectangular (/ 12 7) (/ 7 5)))
  (test "12/7-7/5i" (make-rectangular (/ 12 7) (/ -7 5)))
  (test "12." (inexact 12))
  (test "#e12." 12)
  (test "12.5" (inexact (/ 125 10)))
  (test "#e12.5123" (/ 125123 10000))
  (test "#i125123/10000" (inexact (/ 125123 10000)))
  (test "+inf.0i" (make-rectangular 0 inf+))
  (test "-inf.0i" (make-rectangular 0 inf-))

  (test "1/2" (/ 1 2))
  (test "-1/2" (/ 1 -2))
  (test "#x24" 36)
  (test "#x-24" -36)
  (test "#b+00000110110" 54)
  (test "#b-00000110110/10" -27)
  (test "#e10" 10)
  (test "#e1" 1)
  (test "#e-17" -17)
  (test "#e#x24" 36)
  (test "#e#x-24" -36)
  (test "#e#b+00000110110" 54)
  (test "#e#b-00000110110/10" -27)
  (test "#x#e24" 36)
  (test "#x#e-24" -36)
  (test "#b#e+00000110110" 54)
  (test "#b#e-00000110110/10" -27)
  (test "#e1e1000" (expt 10 1000))
  (test "#e-1e1000" (- (expt 10 1000)))
  (test "#e1e-1000" (expt 10 -1000))
  (test "#e-1e-1000" (- (expt 10 -1000)))
  (test "#i1e100" (inexact (expt 10 100)))
  (test "#i1e1000" (inexact (expt 10 1000)))
  (test "#i-1e1000" (inexact (- (expt 10 1000))))
  (test "1e100" (inexact (expt 10 100)))
  (test "1.0e100" (inexact (expt 10 100)))
  (test "1.e100" (inexact (expt 10 100)))
  (test "0.1e100" (inexact (expt 10 99)))
  (test ".1e100" (inexact (expt 10 99)))
  (test "+1e100" (inexact (expt 10 100)))
  (test "+1.0e100" (inexact (expt 10 100)))
  (test "+1.e100" (inexact (expt 10 100)))
  (test "+0.1e100" (inexact (expt 10 99)))
  (test "+.1e100" (inexact (expt 10 99)))
  (test "-1e100" (inexact   (- (expt 10 100))))
  (test "-1.0e100" (inexact (- (expt 10 100))))
  (test "-1.e100" (inexact  (- (expt 10 100))))
  (test "-0.1e100" (inexact (- (expt 10 99))))
  (test "-.1e100" (inexact  (- (expt 10 99))))

  (test "8+6.0i" (make-rectangular 8 6.0))
  (test "8.0+6i" (make-rectangular 8.0 6))
  (test "+8+6.0i" (make-rectangular 8 6.0))
  (test "+8.0+6i" (make-rectangular 8.0 6))
  (test "-8+6.0i" (make-rectangular -8 6.0))
  (test "-8.0+6i" (make-rectangular -8.0 6))

  (test "8-6.0i" (make-rectangular 8 -6.0))
  (test "8.0-6i" (make-rectangular 8.0 -6))
  (test "+8-6.0i" (make-rectangular 8 -6.0))
  (test "+8.0-6i" (make-rectangular 8.0 -6))
  (test "-8-6.0i" (make-rectangular -8 -6.0))
  (test "-8.0-6i" (make-rectangular -8.0 -6))

  (test "+0i" 0)
  (test "-0i" 0)

  (test "+1i" (make-rectangular 0 1))
  (test "-1i" (make-rectangular 0 -1))

  (test "8+nan.0i" (make-rectangular 8 +nan.0))
  (test "8.0+nan.0i" (make-rectangular 8.0 +nan.0))
  (test "+8+nan.0i" (make-rectangular 8 +nan.0))
  (test "+8.0+nan.0i" (make-rectangular 8.0 +nan.0))
  (test "-8+nan.0i" (make-rectangular -8 +nan.0))
  (test "-8.0+nan.0i" (make-rectangular -8.0 +nan.0))
  (test "8-nan.0i" (make-rectangular 8 -nan.0))
  (test "8.0-nan.0i" (make-rectangular 8.0 -nan.0))
  (test "+8-nan.0i" (make-rectangular 8 -nan.0))
  (test "+8.0-nan.0i" (make-rectangular 8.0 -nan.0))
  (test "-8-nan.0i" (make-rectangular -8 -nan.0))
  (test "-8.0-nan.0i" (make-rectangular -8.0 -nan.0))
  (test "+nan.0+6.0i" (make-rectangular +nan.0 6.0))
  (test "+nan.0+6i" (make-rectangular +nan.0 6))
  (test "+nan.0+6.0i" (make-rectangular +nan.0 6.0))
  (test "+nan.0+6i" (make-rectangular +nan.0 6))
  (test "-nan.0+6.0i" (make-rectangular -nan.0 6.0))
  (test "-nan.0+6i" (make-rectangular -nan.0 6))
  (test "+nan.0-6.0i" (make-rectangular +nan.0 -6.0))
  (test "+nan.0-6i" (make-rectangular +nan.0 -6))
  (test "+nan.0-6.0i" (make-rectangular +nan.0 -6.0))
  (test "+nan.0-6i" (make-rectangular +nan.0 -6))
  (test "-nan.0-6.0i" (make-rectangular -nan.0 -6.0))
  (test "-nan.0-6i" (make-rectangular -nan.0 -6))
  (test "+nan.0+nan.0i" (make-rectangular +nan.0 +nan.0))
  (test "+nan.0-nan.0i" (make-rectangular +nan.0 -nan.0))
  (test "-nan.0+nan.0i" (make-rectangular -nan.0 +nan.0))
  (test "-nan.0-nan.0i" (make-rectangular -nan.0 -nan.0))

  (test "+nan.0+i" (make-rectangular +nan.0 +1))
  (test "+nan.0-i" (make-rectangular +nan.0 -1))
  (test "-nan.0+i" (make-rectangular -nan.0 +1))
  (test "-nan.0-i" (make-rectangular -nan.0 -1))

  (test "8+inf.0i" (make-rectangular 8 +inf.0))
  (test "8.0+inf.0i" (make-rectangular 8.0 +inf.0))
  (test "+8+inf.0i" (make-rectangular 8 +inf.0))
  (test "+8.0+inf.0i" (make-rectangular 8.0 +inf.0))
  (test "-8+inf.0i" (make-rectangular -8 +inf.0))
  (test "-8.0+inf.0i" (make-rectangular -8.0 +inf.0))
  (test "8-inf.0i" (make-rectangular 8 -inf.0))
  (test "8.0-inf.0i" (make-rectangular 8.0 -inf.0))
  (test "+8-inf.0i" (make-rectangular 8 -inf.0))
  (test "+8.0-inf.0i" (make-rectangular 8.0 -inf.0))
  (test "-8-inf.0i" (make-rectangular -8 -inf.0))
  (test "-8.0-inf.0i" (make-rectangular -8.0 -inf.0))
  (test "+inf.0+6.0i" (make-rectangular +inf.0 6.0))
  (test "+inf.0+6i" (make-rectangular +inf.0 6))
  (test "+inf.0+6.0i" (make-rectangular +inf.0 6.0))
  (test "+inf.0+6i" (make-rectangular +inf.0 6))
  (test "-inf.0+6.0i" (make-rectangular -inf.0 6.0))
  (test "-inf.0+6i" (make-rectangular -inf.0 6))
  (test "+inf.0-6.0i" (make-rectangular +inf.0 -6.0))
  (test "+inf.0-6i" (make-rectangular +inf.0 -6))
  (test "+inf.0-6.0i" (make-rectangular +inf.0 -6.0))
  (test "+inf.0-6i" (make-rectangular +inf.0 -6))
  (test "-inf.0-6.0i" (make-rectangular -inf.0 -6.0))
  (test "-inf.0-6i" (make-rectangular -inf.0 -6))
  (test "+inf.0+inf.0i" (make-rectangular +inf.0 +inf.0))
  (test "+inf.0-inf.0i" (make-rectangular +inf.0 -inf.0))
  (test "-inf.0+inf.0i" (make-rectangular -inf.0 +inf.0))
  (test "-inf.0-inf.0i" (make-rectangular -inf.0 -inf.0))

  (test "+inf.0+i" (make-rectangular +inf.0 +1))
  (test "+inf.0-i" (make-rectangular +inf.0 -1))
  (test "-inf.0+i" (make-rectangular -inf.0 +1))
  (test "-inf.0-i" (make-rectangular -inf.0 -1))

  (test "8+6e20i" (make-rectangular +8 +6e20))
  (test "8-6e20i" (make-rectangular +8 -6e20))
  (test "8e20+6i" (make-rectangular +8e20 +6))
  (test "8e20-6i" (make-rectangular +8e20 -6))
  (test "+8+6e20i" (make-rectangular +8 +6e20))
  (test "+8-6e20i" (make-rectangular +8 -6e20))
  (test "+8e20+6i" (make-rectangular +8e20 +6))
  (test "+8e20-6i" (make-rectangular +8e20 -6))
  (test "-8+6e20i" (make-rectangular -8 +6e20))
  (test "-8-6e20i" (make-rectangular -8 -6e20))
  (test "-8e20+6i" (make-rectangular -8e20 +6))
  (test "-8e20-6i" (make-rectangular -8e20 -6))

  (test "8e10+6e20i" (make-rectangular +8e10 +6e20))
  (test "8e10-6e20i" (make-rectangular +8e10 -6e20))
  (test "+8e10+6e20i" (make-rectangular +8e10 +6e20))
  (test "+8e10-6e20i" (make-rectangular +8e10 -6e20))
  (test "-8e10+6e20i" (make-rectangular -8e10 +6e20))
  (test "-8e10-6e20i" (make-rectangular -8e10 -6e20))

  (test "-0e-10" -0.0)
  (test "-0e-0" -0.0)
  (test "#d-0e-10-0e-0i" (make-rectangular -0.0 -0.0))
  (test "-0.i" (make-rectangular 0.0 -0.0))
  (test "#d#e-0.0f-0-.0s-0i" 0)

  (test "+.234e4i" (make-rectangular 0 0.234e4))
  (test "+.234e-5i" (make-rectangular 0 0.234e-5))
  (test "+.234i" (make-rectangular 0 0.234))

;;; --------------------------------------------------------------------

  (check
      (tokenise "i")
    => `((IDENTIFIER "i") *eoi*))

  (check
      (tokenise "/")
    => `((IDENTIFIER "/") *eoi*))

  (test-lexical-error "0i")
  (test-lexical-error "1i+")
  (test-lexical-error "12/7i")
  (test-lexical-error "12/0")
  (test-lexical-error "+12/0")
  (test-lexical-error "-12/0")
  (test-lexical-error "12/0000")
  (test-lexical-error "+12/0000")
  (test-lexical-error "-12/0000")
  (test-lexical-error "12+")
  (test-lexical-error "+12+")
  (test-lexical-error "-12+")
  (test-lexical-error "12+")
  (test-lexical-error "+12+")
  (test-lexical-error "-12+")

  #t)


(parametrise ((check-test-name	'full-table-line-comment)
	      (debugging	#f))

  (define (tokenise string)
    (let* ((IS		(lexer-make-IS (string: string) (counters: 'all)))
	   (lexer	(lexer-make-lexer r6.r6rs-lexer-table IS))
	   (result	'()))
      (do (((T <lexical-token>) (lexer) (lexer)))
	  (T.special?
	   (reverse `(,(if T.lexer-error?
			   `(,T.category ,T.value)
			 T.category). ,result)))
	(debug "line comment token ~s >>~s<<" T.category T.value)
	(set-cons! result (list T.category T.value)))))

  (define-syntax test
    (syntax-rules ()
      ((_ ?string)
       (check (tokenise ?string) => '((LINECOMMENT ?string) *eoi*)))))

;;; --------------------------------------------------------------------

  (check
      (tokenise "")
    => '(*eoi*))

  (test ";\n")
  (test ";;\n")
  (test ";;;\n")
  (test ";;; ciao\n")
  (test ";;; ciao\r")

  ;; no line end
  (test ";")
  (test ";;")
  (test ";;;")
  (test ";;; ciao")
  (test ";;; ciao")

  #t)


(parametrise ((check-test-name	'full-table-misc)
	      (debugging	#f))

  (define (tokenise string)
    (let* ((IS		(lexer-make-IS (string: string) (counters: 'all)))
	   (lexer	(lexer-make-lexer r6.r6rs-lexer-table IS))
	   (result	'()))
      (do (((T <lexical-token>) (lexer) (lexer)))
	  (T.special?
	   (reverse `(,(if T.lexer-error?
			   `(,T.category ,T.value)
			 T.category). ,result)))
	(debug "misc token ~s >>~s<<" T.category T.value)
	(set-cons! result (list T.category T.value)))))

;;; --------------------------------------------------------------------

  (check (tokenise "#t") => '((BOOLEAN #t) *eoi*))
  (check (tokenise "#T") => '((BOOLEAN #t) *eoi*))
  (check (tokenise "#f") => '((BOOLEAN #f) *eoi*))
  (check (tokenise "#F") => '((BOOLEAN #f) *eoi*))

  (check (tokenise "#t(") => '((BOOLEAN #t) (OPAREN #\() *eoi*))
  (check (tokenise "#f(") => '((BOOLEAN #f) (OPAREN #\() *eoi*))
  (check (tokenise "#t)") => '((BOOLEAN #t) (CPAREN #\)) *eoi*))
  (check (tokenise "#f)") => '((BOOLEAN #f) (CPAREN #\)) *eoi*))

  (test-lexical-error tokenise "#tciao")
  (test-lexical-error tokenise "#fciao")

;;; --------------------------------------------------------------------

  (check (tokenise "(")		=> '((OPAREN		#\() *eoi*))
  (check (tokenise ")")		=> '((CPAREN		#\)) *eoi*))
  (check (tokenise "[")		=> '((OBRACKET		#\[) *eoi*))
  (check (tokenise "]")		=> '((CBRACKET		#\]) *eoi*))
  (check (tokenise "'")		=> '((TICK		#\') *eoi*))
  (check (tokenise "`")		=> '((BACKTICK		#\`) *eoi*))
  (check (tokenise ",")		=> '((COMMA		#\,) *eoi*))
  (check (tokenise ",@")	=> '((COMMAAT		",@") *eoi*))
  (check (tokenise ".")		=> '((DOT		#\.) *eoi*))
  (check (tokenise "#(")	=> '((SHARPPAREN	"#(") *eoi*))
  (check (tokenise "#vu8(")	=> '((SHARPVU8PAREN	"#vu8(") *eoi*))
  (check (tokenise "#'")	=> '((SHARPTICK		"#'") *eoi*))
  (check (tokenise "#`")	=> '((SHARPBACKTICK	"#`") *eoi*))
  (check (tokenise "#,")	=> '((SHARPCOMMA	"#,") *eoi*))
  (check (tokenise "#,@")	=> '((SHARPCOMMAAT	"#,@") *eoi*))
  (check (tokenise "#;")	=> '((SHARPSEMICOLON	"#;") *eoi*))

  (check (tokenise "#!r6rs")	=> '((SHARPBANGR6RS	"#!r6rs") *eoi*))
  (check (tokenise "#!ciao")	=> '((SHARPBANG		"#!ciao") *eoi*))

  ;;Fine here, but in a full lexer a double quote must open a string.
  (check (tokenise "\"")	=> '((DOUBLEQUOTE	#\") *eoi*))

  #t)


(parametrise ((check-test-name	'full-lexer-discard-misc)
	      (debugging	#f))

;;;Use the lexer that discards blanks and comments.

  (define (tokenise string)
    (let* ((IS		(lexer-make-IS (string: string) (counters: 'all)))
	   (lexer	(r6.make-token-lexer IS)))
      (let next (((T <lexical-token>)	(lexer))
		 (result		'()))
	(cond (T.lexer-error?
	       (reverse `((,T.category ,T.value) . ,result)))
	      (T.end-of-input?
	       (reverse `(*eoi* . ,result)))
	      (else
	       (next (lexer) `((,T.category ,T.value) . ,result)))))))

;;; --------------------------------------------------------------------

  (check (tokenise "#t") => '((BOOLEAN #t) *eoi*))
  (check (tokenise "#T") => '((BOOLEAN #t) *eoi*))
  (check (tokenise "#f") => '((BOOLEAN #f) *eoi*))
  (check (tokenise "#F") => '((BOOLEAN #f) *eoi*))

  (check (tokenise "#t(") => '((BOOLEAN #t) (OPAREN #\() *eoi*))
  (check (tokenise "#f(") => '((BOOLEAN #f) (OPAREN #\() *eoi*))
  (check (tokenise "#t)") => '((BOOLEAN #t) (CPAREN #\)) *eoi*))
  (check (tokenise "#f)") => '((BOOLEAN #f) (CPAREN #\)) *eoi*))

  (test-lexical-error tokenise "#tciao")
  (test-lexical-error tokenise "#fciao")

;;; --------------------------------------------------------------------

  (check (tokenise "(")		=> '((OPAREN		#\() *eoi*))
  (check (tokenise ")")		=> '((CPAREN		#\)) *eoi*))
  (check (tokenise "[")		=> '((OBRACKET		#\[) *eoi*))
  (check (tokenise "]")		=> '((CBRACKET		#\]) *eoi*))
  (check (tokenise "'")		=> '((TICK		#\') *eoi*))
  (check (tokenise "`")		=> '((BACKTICK		#\`) *eoi*))
  (check (tokenise ",")		=> '((COMMA		#\,) *eoi*))
  (check (tokenise ",@")	=> '((COMMAAT		",@") *eoi*))
  (check (tokenise ".")		=> '((DOT		#\.) *eoi*))
  (check (tokenise "#(")	=> '((SHARPPAREN	"#(") *eoi*))
  (check (tokenise "#vu8(")	=> '((SHARPVU8PAREN	"#vu8(") *eoi*))
  (check (tokenise "#'")	=> '((SHARPTICK		"#'") *eoi*))
  (check (tokenise "#`")	=> '((SHARPBACKTICK	"#`") *eoi*))
  (check (tokenise "#,")	=> '((SHARPCOMMA	"#,") *eoi*))
  (check (tokenise "#,@")	=> '((SHARPCOMMAAT	"#,@") *eoi*))
  (check (tokenise "#;")	=> '((SHARPSEMICOLON	"#;") *eoi*))

  ;; try discarded tokesn
  (check (tokenise "#!r6rs")	=> '(*eoi*))
  (check (tokenise "#!ciao")	=> '(*eoi*))

  #t)


(parametrise ((check-test-name	'full-lexer-no-discard-misc)
	      (debugging	#f))

;;;Use the lexer that DOES NOT discard blanks and comments.

  (define (tokenise string)
    (let* ((IS		(lexer-make-IS (string: string) (counters: 'all)))
	   (lexer	(r6.make-token-lexer IS (r6.comments #t) (r6.blanks #t) (r6.sharpbang #t))))
      (let next (((T <lexical-token>)	(lexer))
		 (result		'()))
	(cond (T.lexer-error?
	       (reverse `((,T.category ,T.value) . ,result)))
	      (T.end-of-input?
	       (reverse `(*eoi* . ,result)))
	      (else
	       (next (lexer) `((,T.category ,T.value) . ,result)))))))

;;; --------------------------------------------------------------------

  (check (tokenise "#t") => '((BOOLEAN #t) *eoi*))
  (check (tokenise "#T") => '((BOOLEAN #t) *eoi*))
  (check (tokenise "#f") => '((BOOLEAN #f) *eoi*))
  (check (tokenise "#F") => '((BOOLEAN #f) *eoi*))

  (check (tokenise "#t(") => '((BOOLEAN #t) (OPAREN #\() *eoi*))
  (check (tokenise "#f(") => '((BOOLEAN #f) (OPAREN #\() *eoi*))
  (check (tokenise "#t)") => '((BOOLEAN #t) (CPAREN #\)) *eoi*))
  (check (tokenise "#f)") => '((BOOLEAN #f) (CPAREN #\)) *eoi*))

  (test-lexical-error tokenise "#tciao")
  (test-lexical-error tokenise "#fciao")

;;; --------------------------------------------------------------------

  (check (tokenise "(")		=> '((OPAREN		#\() *eoi*))
  (check (tokenise ")")		=> '((CPAREN		#\)) *eoi*))
  (check (tokenise "[")		=> '((OBRACKET		#\[) *eoi*))
  (check (tokenise "]")		=> '((CBRACKET		#\]) *eoi*))
  (check (tokenise "'")		=> '((TICK		#\') *eoi*))
  (check (tokenise "`")		=> '((BACKTICK		#\`) *eoi*))
  (check (tokenise ",")		=> '((COMMA		#\,) *eoi*))
  (check (tokenise ",@")	=> '((COMMAAT		",@") *eoi*))
  (check (tokenise ".")		=> '((DOT		#\.) *eoi*))
  (check (tokenise "#(")	=> '((SHARPPAREN	"#(") *eoi*))
  (check (tokenise "#vu8(")	=> '((SHARPVU8PAREN	"#vu8(") *eoi*))
  (check (tokenise "#'")	=> '((SHARPTICK		"#'") *eoi*))
  (check (tokenise "#`")	=> '((SHARPBACKTICK	"#`") *eoi*))
  (check (tokenise "#,")	=> '((SHARPCOMMA	"#,") *eoi*))
  (check (tokenise "#,@")	=> '((SHARPCOMMAAT	"#,@") *eoi*))
  (check (tokenise "#;")	=> '((SHARPSEMICOLON	"#;") *eoi*))

  (check (tokenise "#!r6rs")	=> '((SHARPBANGR6RS	"#!r6rs") *eoi*))
  (check (tokenise "#!ciao")	=> '((SHARPBANG		"#!ciao") *eoi*))

  #t)


(parametrise ((check-test-name	'full-lexer-discard-sexp)
	      (debugging	#f))

;;;Use the lexer that discards blanks and comments.

  (define (tokenise string)
    (let* ((IS		(lexer-make-IS (string: string) (counters: 'all)))
	   (lexer	(r6.make-token-lexer IS)))
      (let next (((T <lexical-token>)	(lexer))
		 (result		'()))
	(cond (T.lexer-error?
	       (reverse `((,T.category ,T.value) . ,result)))
	      (T.end-of-input?
	       (reverse `(*eoi* . ,result)))
	      (else
	       (next (lexer) `((,T.category ,T.value) . ,result)))))))

;;; --------------------------------------------------------------------

  (check
      (tokenise "( \"ciao\" ciao 123 )")
    => '((OPAREN #\()
	 (STRING "ciao")
	 (IDENTIFIER "ciao")
	 (NUMBER 123)
	 (CPAREN #\))
	 *eoi*))

  (check
      (tokenise "(\"ciao\"ciao)")
    => '((OPAREN #\()
	 (STRING "ciao")
	 (IDENTIFIER "ciao")
	 (CPAREN #\))
	 *eoi*))

  (check
      (tokenise "(1.2 1/2 +1.3i)")
    => '((OPAREN #\()
	 (NUMBER 1.2)
	 (NUMBER 1/2)
	 (NUMBER +1.3i)
	 (CPAREN #\))
	 *eoi*))

  (check
      (tokenise "([1.2 1/2 +1.3i])")
    => '((OPAREN #\()
	 (OBRACKET #\[)
	 (NUMBER 1.2)
	 (NUMBER 1/2)
	 (NUMBER +1.3i)
	 (CBRACKET #\])
	 (CPAREN #\))
	 *eoi*))

  (check
      (tokenise "ciao#| per la |#mamma")
    => '((IDENTIFIER "ciao")
	 (IDENTIFIER "mamma")
	 *eoi*))

  (let ((mt (lambda (yygetc yyungetc yytext yyline yycolumn yyoffset)
	      (make* <lexical-token>
		'THE-IDENTIFIER
		(make* <source-location>
		  (r6.current-input-source) yyline yycolumn yyoffset)
		(string->symbol yytext)
		(string-length yytext)))))
    (parametrise ((r6.identifier-token-maker mt))
      (check
	  (tokenise "( \"ciao\" ciao 123 )")
	=> '((OPAREN #\()
	     (STRING "ciao")
	     (THE-IDENTIFIER ciao)
	     (NUMBER 123)
	     (CPAREN #\))
	     *eoi*))))

;;; --------------------------------------------------------------------

  (check
      (tokenise "'ciao")
    => '((TICK #\')
	 (IDENTIFIER "ciao")
	 *eoi*))

  (check
      (tokenise "`ciao")
    => '((BACKTICK #\`)
	 (IDENTIFIER "ciao")
	 *eoi*))

  (check
      (tokenise ",ciao")
    => '((COMMA #\,)
	 (IDENTIFIER "ciao")
	 *eoi*))

  (check
      (tokenise ",@ciao")
    => '((COMMAAT ",@")
	 (IDENTIFIER "ciao")
	 *eoi*))

;;; --------------------------------------------------------------------

  (check
      (tokenise "#'ciao")
    => '((SHARPTICK "#'")
	 (IDENTIFIER "ciao")
	 *eoi*))

  (check
      (tokenise "#`ciao")
    => '((SHARPBACKTICK "#`")
	 (IDENTIFIER "ciao")
	 *eoi*))

  (check
      (tokenise "#,ciao")
    => '((SHARPCOMMA "#,")
	 (IDENTIFIER "ciao")
	 *eoi*))

  (check
      (tokenise "#,@ciao")
    => '((SHARPCOMMAAT "#,@")
	 (IDENTIFIER "ciao")
	 *eoi*))

  #t)


(parametrise ((check-test-name	'full-sexp-no-discard-sexp)
	      (debugging	#f))

;;;Use the lexer that DOES NOT discard blanks and comments.

  (define (tokenise string)
    (let* ((IS		(lexer-make-IS (string: string) (counters: 'all)))
	   (lexer	(r6.make-token-lexer IS (r6.comments #t) (r6.blanks #t) (r6.sharpbang #t))))
      (let next (((T <lexical-token>)	(lexer))
		 (result		'()))
	(cond (T.lexer-error?
	       (reverse `((,T.category ,T.value) . ,result)))
	      (T.end-of-input?
	       (reverse `(*eoi* . ,result)))
	      (else
	       (next (lexer) `((,T.category ,T.value) . ,result)))))))

;;; --------------------------------------------------------------------

  (check
      (tokenise "(\"ciao\"ciao)")
    => '((OPAREN #\()
	 (STRING "ciao")
	 (IDENTIFIER "ciao")
	 (CPAREN #\))
	 *eoi*))

  (check
      (tokenise "(1.2\t1/2\t+1.3i)")
    => '((OPAREN #\()
	 (NUMBER 1.2)
	 (WHITESPACE "\t")
	 (NUMBER 1/2)
	 (WHITESPACE "\t")
	 (NUMBER +1.3i)
	 (CPAREN #\))
	 *eoi*))

  (check
      (tokenise "([1.2 1/2 +1.3i])")
    => '((OPAREN #\()
	 (OBRACKET #\[)
	 (NUMBER 1.2)
	 (WHITESPACE " ")
	 (NUMBER 1/2)
	 (WHITESPACE " ")
	 (NUMBER +1.3i)
	 (CBRACKET #\])
	 (CPAREN #\))
	 *eoi*))

  (check
      (tokenise "ciao#| per la |#mamma")
    => '((IDENTIFIER "ciao")
	 (NESTEDCOMMENT "#| per la |#")
	 (IDENTIFIER "mamma")
	 *eoi*))

  (let ((mt (lambda (yygetc yyungetc yytext yyline yycolumn yyoffset)
	      (make* <lexical-token>
		'THE-IDENTIFIER
		(make* <source-location>
		  (r6.current-input-source) yyline yycolumn yyoffset)
		(string->symbol yytext)
		(string-length yytext)))))
    (parametrise ((r6.identifier-token-maker mt))
      (check
	  (tokenise "( \"ciao\" ciao 123 )")
	=> '((OPAREN #\()
	     (WHITESPACE " ")
	     (STRING "ciao")
	     (WHITESPACE " ")
	     (THE-IDENTIFIER ciao)
	     (WHITESPACE " ")
	     (NUMBER 123)
	     (WHITESPACE " ")
	     (CPAREN #\))
	     *eoi*))))

;;; --------------------------------------------------------------------

  (check
      (tokenise "'ciao")
    => '((TICK #\')
	 (IDENTIFIER "ciao")
	 *eoi*))

  (check
      (tokenise "`ciao")
    => '((BACKTICK #\`)
	 (IDENTIFIER "ciao")
	 *eoi*))

  (check
      (tokenise ",ciao")
    => '((COMMA #\,)
	 (IDENTIFIER "ciao")
	 *eoi*))

  (check
      (tokenise ",@ciao")
    => '((COMMAAT ",@")
	 (IDENTIFIER "ciao")
	 *eoi*))

;;; --------------------------------------------------------------------

  (check
      (tokenise "#'ciao")
    => '((SHARPTICK "#'")
	 (IDENTIFIER "ciao")
	 *eoi*))

  (check
      (tokenise "#`ciao")
    => '((SHARPBACKTICK "#`")
	 (IDENTIFIER "ciao")
	 *eoi*))

  (check
      (tokenise "#,ciao")
    => '((SHARPCOMMA "#,")
	 (IDENTIFIER "ciao")
	 *eoi*))

  (check
      (tokenise "#,@ciao")
    => '((SHARPCOMMAAT "#,@")
	 (IDENTIFIER "ciao")
	 *eoi*))

  #t)


;;;; done

(check-report)

;;; end of file
