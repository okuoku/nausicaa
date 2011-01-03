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


(parametrise ((check-test-name	'strings))

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


;;;; done

(check-report)

;;; end of file
