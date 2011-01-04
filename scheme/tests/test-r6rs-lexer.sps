;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for R6RS lexer
;;;Date: Wed Dec 22, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (nausicaa checks))

(check-set-mode! 'report-failed)
(display "*** testing R6RS lexer\n")

(define eoi `(*eoi* . ,(eof-object)))


(parametrise ((check-test-name	'string-tokeniser))

  (define (tokenise string)
    (let* ((IS		(lexer-make-IS (string: string) (counters: 'all)))
	   (lexer	(lexer-make-lexer r6rs-string-lexer-table IS))
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
    (read-string (lexer-make-IS (string: string) (counters: 'all))))

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
	   (lexer	(lexer-make-lexer r6rs-nested-comment-lexer-table IS))
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
      (tokenise "#|#|#|#|")
    => '(OPEN OPEN OPEN OPEN *eoi*))

  (check
      (tokenise "|#|#|#|#")
    => '(CLOSE CLOSE CLOSE CLOSE *eoi*))

  #t)


(parametrise ((check-test-name	'nested-comment-reader))

  (define (parse string)
    (read-nested-comment (lexer-make-IS (string: string) (counters: 'all))))

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
	   (lexer	(lexer-make-lexer r6rs-line-comment-lexer-table IS))
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

;;; --------------------------------------------------------------------
;;; errors

  (check
      (guard (E ((lexical-violation? E)
		 (condition-irritants E))
		(else E))
	(tokenise "; ciao"))
    => '(*lexer-error*))

  (check
      (guard (E ((lexical-violation? E)
		 (condition-irritants E))
		(else E))
	(tokenise ";;;"))
    => '(*lexer-error*))

  #t)


(parametrise ((check-test-name	'line-comment-reader))

  (define (parse string)
    (read-line-comment (lexer-make-IS (string: string) (counters: 'all))))

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
	(parse "; ciao"))
    => '("; ciao"))

  (check
      (guard (E ((lexical-violation? E)
;;;		 (display (condition-message E))(newline)
		 (condition-irritants E))
		(else E))
	(parse ";;;"))
    => '(";;;"))

  #t)


(parametrise ((check-test-name	'character-tokeniser))

  (define (tokenise string)
    (let* ((IS		(lexer-make-IS (string: string) (counters: 'all)))
	   (lexer	(lexer-make-lexer r6rs-character-lexer-table IS))
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
    => '(#\c *lexer-error*))

  #t)


(parametrise ((check-test-name	'character-reader))

  (define (parse string)
    (read-character (lexer-make-IS (string: string) (counters: 'all))))

  (check
      (parse "#\\1")
    => #\1)

  (check
      (parse "#\\A")
    => #\A)

  (check
      (parse "#\\ciao")
    => #\c)

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

  #t)


(parametrise ((check-test-name	'identifier-tokeniser))

  (define (tokenise string)
    (let* ((IS		(lexer-make-IS (string: string) (counters: 'all)))
	   (lexer	(lexer-make-lexer r6rs-identifier-lexer-table IS))
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
    => '(c *eoi*))

  (check
      (tokenise "ciao")
    => '(ciao *eoi*))

  (check
      (tokenise "ciao-mamma")
    => '(ciao-mamma *eoi*))

  (check
      (tokenise "->ciao")
    => '(->ciao *eoi*))

  (check
      (tokenise "?ciao")
    => '(?ciao *eoi*))

  (check
      (tokenise "?")
    => '(? *eoi*))

  (check
      (tokenise "+")
    => '(+ *eoi*))

  (check
      (tokenise "-")
    => '(- *eoi*))

  (check
      (tokenise "...")
    => '(... *eoi*))

  (check
      (tokenise "->")
    => '(-> *eoi*))

  (check
      (tokenise "ciao123")
    => '(ciao123 *eoi*))

  (check
      (tokenise "ciao123+-.@")
    => '(ciao123+-.@ *eoi*))

  (check
      (tokenise "ciao123+-.@ciao")
    => '(ciao123+-.@ciao *eoi*))

  (check	;char with Unicode category Nd
      (tokenise "ciao\\x0CE7;mamma")
    => `(,(string->symbol "ciao\\x0CE7;mamma") *eoi*))

  (check	;char with Unicode category Mc
      (tokenise "ciao\\x09BF;mamma")
    => `(,(string->symbol "ciao\\x09BF;mamma") *eoi*))

  (check	;char with Unicode category Me
      (tokenise "ciao\\x20E4;mamma")
    => `(,(string->symbol "ciao\\x20E4;mamma") *eoi*))

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
    (read-identifier (lexer-make-IS (string: string) (counters: 'all))))

  (check
      (parse "c")
    => 'c)

  (check
      (parse "ciao")
    => 'ciao)

  (check
      (parse "ciao-mamma")
    => 'ciao-mamma)

  (check
      (parse "->ciao")
    => '->ciao)

  (check
      (parse "?ciao")
    => '?ciao)

  (check
      (parse "?")
    => '?)

  (check
      (parse "+")
    => '+)

  (check
      (parse "-")
    => '-)

  (check
      (parse "...")
    => '...)

  (check
      (parse "->")
    => '->)

  (check
      (parse "ciao123")
    => 'ciao123)

  (check
      (parse "ciao123+-.@")
    => 'ciao123+-.@)

  (check
      (parse "ciao123+-.@ciao")
    => 'ciao123+-.@ciao)

  (check	;char with Unicode category Nd
      (parse "ciao\\x0CE7;mamma")
    => (string->symbol "ciao\\x0CE7;mamma"))

  (check	;char with Unicode category Mc
      (parse "ciao\\x09BF;mamma")
    => (string->symbol "ciao\\x09BF;mamma"))

  (check	;char with Unicode category Me
      (parse "ciao\\x20E4;mamma")
    => (string->symbol "ciao\\x20E4;mamma"))

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


;;;; done

(check-report)

;;; end of file
