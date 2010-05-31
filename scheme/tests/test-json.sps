;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for the JSON library
;;;Date: Sun May 30, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(import (nausicaa)
  (json)
  (json string-lexer)
  (parser-tools lexical-token)
  (silex lexer)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing JSON\n")


(parameterise ((check-test-name 'lexer-string))

  (define (tokenise-string string)
    ;;This  is just  a  lexer, it  does  not check  for the  terminating
    ;;double-quote.
    (let* ((IS		(lexer-make-IS (:string string) (:counters 'all)))
	   (lexer	(lexer-make-lexer json-string-lexer-table IS))
	   (out		'()))
      (do ((token (lexer) (lexer)))
	  ((<lexical-token>?/end-of-input token)
	   (reverse out))
;;;(write token)(newline)
	(set! out (cons token out)))))

;;; --------------------------------------------------------------------

;;All the test strings must end with a double-quote char.

  (check	;empty string
      (tokenise-string "\"")
    => '(QUOTED-TEXT-CLOSE))

  (check
      (tokenise-string "\\\"\"")
    => '("\"" QUOTED-TEXT-CLOSE))

  (check
      (tokenise-string "\\/\"")
    => '("/" QUOTED-TEXT-CLOSE))

  (check
      (tokenise-string "\\b\"")
    => '("\b" QUOTED-TEXT-CLOSE))

  (check
      (tokenise-string "\\f\"")
    => '("\f" QUOTED-TEXT-CLOSE))

  (check
      (tokenise-string "\\n\"")
    => '("\n" QUOTED-TEXT-CLOSE))

  (check
      (tokenise-string "\\r\"")
    => '("\r" QUOTED-TEXT-CLOSE))

  (check
      (tokenise-string "\\t\"")
    => '("\t" QUOTED-TEXT-CLOSE))

  (check
      (tokenise-string "inizio\\\"\\/\\b\\f\\n\\r\\tfine\"")
    => '("inizio" "\"" "/" "\b" "\f" "\n" "\r" "\t" "fine" QUOTED-TEXT-CLOSE))

  (check
      (tokenise-string "\\u005C\"")
    => '("\\" QUOTED-TEXT-CLOSE))

  (check
      (tokenise-string "\\u0063\\u0069\\u0061\\u006f\"")
    => '("c" "i" "a" "o" QUOTED-TEXT-CLOSE))

  (check	;a string
      (tokenise-string "ciao\"")
    => '("ciao" QUOTED-TEXT-CLOSE))

  (check
      ;;A string with a quoted character in it.  The quoting is removed.
      (tokenise-string "a\"")
    => '("a" QUOTED-TEXT-CLOSE))

  (check
      ;;Nested double quotes.  The Scheme string "\\\"" is seen as \" by
      ;;the lexer and the backslash quoting character is removed.
      (tokenise-string "ciao \\\"hello\\\" salut\"")
    => '("ciao " "\"" "hello" "\"" " salut"  QUOTED-TEXT-CLOSE))

  #t)


(parameterise ((check-test-name 'lexer-json))

  (define (doit string)
    (map (lambda (token)
	   (cons (<lexical-token>-category token)
		 (<lexical-token>-value    token)))
      (json->tokens (lexer-make-IS (:string string) (:counters 'all)))))

;;; --------------------------------------------------------------------

  (check
      (doit "")
    => '())

  (check
      (doit " { \"Count\": 12 }")
    => '((BEGIN_OBJECT . #\{)
	 (STRING . "Count")
	 (NAME_SEPARATOR . #\:)
	 (NUMBER . 12)
	 (END_OBJECT . #\})))

  (check
    (doit "{
  \"Image\": {
    \"Width\":  800,
    \"Height\": 600,
    \"Title\":  \"View from 15th Floor\",
    \"Thumbnail\": {
      \"Url\":    \"http://www.example.com/image/481989943\",
      \"Height\": 125,
      \"Width\":  \"100\"
    },
    \"IDs\": [116, 943, 234, 38793]
  }
}
")
    => '((BEGIN_OBJECT . #\{)
	 (STRING . "Image")
	 (NAME_SEPARATOR . #\:)
	 (BEGIN_OBJECT . #\{)
	 (STRING . "Width")
	 (NAME_SEPARATOR . #\:)
	 (NUMBER . 800)
	 (VALUE_SEPARATOR . #\,)
	 (STRING . "Height")
	 (NAME_SEPARATOR . #\:)
	 (NUMBER . 600)
	 (VALUE_SEPARATOR . #\,)
	 (STRING . "Title")
	 (NAME_SEPARATOR . #\:)
	 (STRING . "View from 15th Floor")
	 (VALUE_SEPARATOR . #\,)
	 (STRING . "Thumbnail")
	 (NAME_SEPARATOR . #\:)
	 (BEGIN_OBJECT . #\{)
	 (STRING . "Url")
	 (NAME_SEPARATOR . #\:)
	 (STRING . "http://www.example.com/image/481989943")
	 (VALUE_SEPARATOR . #\,)
	 (STRING . "Height")
	 (NAME_SEPARATOR . #\:)
	 (NUMBER . 125)
	 (VALUE_SEPARATOR . #\,)
	 (STRING . "Width")
	 (NAME_SEPARATOR . #\:)
	 (STRING . "100")
	 (END_OBJECT . #\})
	 (VALUE_SEPARATOR . #\,)
	 (STRING . "IDs")
	 (NAME_SEPARATOR . #\:)
	 (BEGIN_ARRAY . #\[)
	 (NUMBER . 116)
	 (VALUE_SEPARATOR . #\,)
	 (NUMBER . 943)
	 (VALUE_SEPARATOR . #\,)
	 (NUMBER . 234)
	 (VALUE_SEPARATOR . #\,)
	 (NUMBER . 38793)
	 (END_ARRAY . #\])
	 (END_OBJECT . #\})
	 (END_OBJECT . #\})))

  #t)


(parameterise ((check-test-name 'parser))

  (define (doit string)
    (let* ((IS		(lexer-make-IS (:string string) (:counters 'all)))
	   (lexer	(make-json-rfc-lexer IS))
	   (parser	(make-json-parser))
	   (handler	(lambda (msg tok) (list 'error-handler msg tok))))
      (parser (lambda ()
		(let ((token (lexer)))
;;;		  (write token)(newline)
		  token))
	      handler)))

;;; --------------------------------------------------------------------

  (check
      (doit "{
  \"Image\": {
    \"Width\":  800,
    \"Height\": 600,
    \"Title\":  \"View from 15th Floor\",
    \"Thumbnail\": {
      \"Url\":    \"http://www.example.com/image/481989943\",
      \"Height\": 125,
      \"Width\":  \"100\"
    },
    \"IDs\": [116, 943, 234, 38793]
  }
}
")
    => '(("Image"
	  ("Width" . 800)
	  ("Height" . 600)
	  ("Title" . "View from 15th Floor")
	  ("Thumbnail" . (("Url" . "http://www.example.com/image/481989943")
			  ("Height" . 125)
			  ("Width" . "100")))
	  ("IDs" . #(116 943 234 38793)))))

  (check
      (doit "{ \"Hello\" : true }")
    => '(("Hello" . #t)))

  (check
      (doit "{ \"Hello\" : false }")
    => '(("Hello" . #f)))

  (check
      (doit "{ \"Hello\" : null }")
    => '(("Hello" . ())))

  (check
      (doit "{ \"Hello\" : true, \"Ciao\": false }")
    => '(("Hello" . #t)
	 ("Ciao" . #f)))

  #t)


(parameterise ((check-test-name 'string-encode))

  (check
      (json-encode-string "")
    => "")

  (check
      (json-encode-string "ciao")
    => "ciao")

  (check
      (json-encode-string "\"")
    => "\\\"")

  (check
      (json-encode-string "/")
    => "\\/")

  (check
      (json-encode-string "\b")
    => "\\b")

  (check
      (json-encode-string "\f")
    => "\\f")

  (check
      (json-encode-string "\n")
    => "\\n")

  (check
      (json-encode-string "\r")
    => "\\r")

  (check
      (json-encode-string "\t")
    => "\\t")

  (check
      (json-encode-string " ")
    => " ")

  (check
      (json-encode-string "\x127;")
    => "\\u0127")

  (check
      (json-encode-string "\x1234;")
    => "\\u1234")

  #t)


(parameterise ((check-test-name 'string-decode))

  (check	;empty string
      (json-decode-string "")
    => "")

  (check
      (json-decode-string "\\\"")
    => "\"")

  (check
      (json-decode-string "\\/")
    => "/")

  (check
      (json-decode-string "\\b")
    => "\b")

  (check
      (json-decode-string "\\f")
    => "\f")

  (check
      (json-decode-string "\\n")
    => "\n")

  (check
      (json-decode-string "\\r")
    => "\r")

  (check
      (json-decode-string "\\t")
    => "\t")

  (check
      (json-decode-string "inizio\\\"\\/\\b\\f\\n\\r\\tfine")
    => "inizio\"/\b\f\n\r\tfine")

  (check
      (json-decode-string "\\u005C")
    => "\\")

  (check
      (json-decode-string "\\u0063\\u0069\\u0061\\u006f")
    => "ciao")

  (check
      (json-decode-string "ciao")
    => "ciao")

  (check
      (json-decode-string "a")
    => "a")

  (check
      (json-decode-string "ciao \\\"hello\\\" salut")
    => "ciao \"hello\" salut")

  #t)


(parameterise ((check-test-name 'generator))

  (check
      (json-make-pair "ciao" 123)
    => "\"ciao\": 123")

  (check
      (json-make-pair "ciao" "hello")
    => "\"ciao\": \"hello\"")

  (check
      (json-make-pair "ciao" #t)
    => "\"ciao\": true")

  (check
      (json-make-pair "ciao" #f)
    => "\"ciao\": false")

  (check
      (json-make-pair "ciao" '())
    => "\"ciao\": null")

  (check
      (json-make-pair "hey" "{ \"ciao\": 123 }" #f)
    => "\"hey\": { \"ciao\": 123 }")

;;; --------------------------------------------------------------------

  (check
      (json-make-object "\"ciao\": 123")
    => "{ \"ciao\": 123 }")

  (check
      (json-make-object "\"ciao\": 123"
			"\"ciao\": \"hello\"")
    => "{ \"ciao\": 123, \"ciao\": \"hello\" }")

;;; --------------------------------------------------------------------

  (check
      (json-make-array '())
    => "[  ]")

  (check
      (json-make-array '("12" "34" "56"))
    => "[ 12, 34, 56 ]")

  #;(check
      (json-make-array '#())
    => "[  ]")

  (check
      (json-make-array '#("12" "34" "56"))
    => "[ 12, 34, 56 ]")

  #t)


;;;; done

(check-report)

;;; end of file
