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

  (check				;empty string
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

  (check				;a string
      (tokenise "ciao\"")
    => '("ciao" STRING *eoi*))

  (check
      ;;Nested double quotes.  The Scheme string "\\\"" is seen as \" by
      ;;the lexer and the backslash quoting character is removed.
      (tokenise "ciao \\\"hello\\\" salut\"")
    => '("ciao " #\" "hello" #\" " salut"  STRING *eoi*))

  (check				;intraline space
      (tokenise "ciao \\\nmamma\"")
    => '("ciao " "mamma" STRING *eoi*))

  (check				;intraline space
      (tokenise "ciao \\   \n   mamma\"")
    => '("ciao " "mamma" STRING *eoi*))

  (check				;intraline space, real usage example
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

  (check		       ;missing ending #\; for inline-hex-escape
      (tokenise "\\x00\"")
    => '(*lexer-error*))

  #t)


(parametrise ((check-test-name	'string-parser))

  (define (parse string)
    (parse-string (lexer-make-IS (string: string) (counters: 'all))))

;;; All the test strings must end with a double-quote char.

  (check				;empty string
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

  (check				;a string
      (parse "ciao\"")
    => "ciao")

  (check
      ;;Nested double quotes.  The Scheme string "\\\"" is seen as \" by
      ;;the lexer and the backslash quoting character is removed.
      (parse "ciao \\\"hello\\\" salut\"")
    => "ciao \"hello\" salut")

  (check				;intraline space
      (parse "ciao \\\nmamma\"")
    => "ciao mamma")

  (check				;intraline space
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


;;;; done

(check-report)

;;; end of file
