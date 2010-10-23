;;; -*- coding: utf-8 -*-
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


#!r6rs
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
    (let* ((IS		(lexer-make-IS (string: string) (counters: 'all)))
	   (lexer	(lexer-make-lexer json-string-lexer-table IS))
	   (out		'()))
      (do ((token (lexer) (lexer)))
	  ((<lexical-token>?/special token)
	   (let (((T <lexical-token>) token))
	     (reverse (cons (cons T.category T.value) out))))
;;;(write token)(newline)
	(set! out (cons token out)))))

  (define eoi `(*eoi* . ,(eof-object)))

;;; --------------------------------------------------------------------

;;All the test strings must end with a double-quote char.

  (check	;empty string
      (tokenise-string "\"")
    => `(QUOTED-TEXT-CLOSE ,eoi))

  (check
      (tokenise-string "\\\"\"")
    => `("\"" QUOTED-TEXT-CLOSE ,eoi))

  (check
      (tokenise-string "\\/\"")
    => `("/" QUOTED-TEXT-CLOSE ,eoi))

  (check
      (tokenise-string "\\b\"")
    => `("\b" QUOTED-TEXT-CLOSE ,eoi))

  (check
      (tokenise-string "\\f\"")
    => `("\f" QUOTED-TEXT-CLOSE ,eoi))

  (check
      (tokenise-string "\\n\"")
    => `("\n" QUOTED-TEXT-CLOSE ,eoi))

  (check
      (tokenise-string "\\r\"")
    => `("\r" QUOTED-TEXT-CLOSE ,eoi))

  (check
      (tokenise-string "\\t\"")
    => `("\t" QUOTED-TEXT-CLOSE ,eoi))

  (check
      (tokenise-string "inizio\\\"\\/\\b\\f\\n\\r\\tfine\"")
    => `("inizio" "\"" "/" "\b" "\f" "\n" "\r" "\t" "fine" QUOTED-TEXT-CLOSE ,eoi))

  (check
      (tokenise-string "\\u005C\"")
    => `("\\" QUOTED-TEXT-CLOSE ,eoi))

  (check
      (tokenise-string "\\u0063\\u0069\\u0061\\u006f\"")
    => `("c" "i" "a" "o" QUOTED-TEXT-CLOSE ,eoi))

  (check	;a string
      (tokenise-string "ciao\"")
    => `("ciao" QUOTED-TEXT-CLOSE ,eoi))

  (check
      ;;A string with a quoted character in it.  The quoting is removed.
      (tokenise-string "a\"")
    => `("a" QUOTED-TEXT-CLOSE ,eoi))

  (check
      ;;Nested double quotes.  The Scheme string "\\\"" is seen as \" by
      ;;the lexer and the backslash quoting character is removed.
      (tokenise-string "ciao \\\"hello\\\" salut\"")
    => `("ciao " "\"" "hello" "\"" " salut"  QUOTED-TEXT-CLOSE ,eoi))

;;; --------------------------------------------------------------------

  (check
      (tokenise-string "\\u00\"")
    => `((*lexer-error* . "\\u00")))

  (check
      (tokenise-string "ciao")
    => `("ciao" ,eoi))

  #t)


(parameterise ((check-test-name 'lexer-tokens))

  (define (doit string)
    (map (lambda (token)
	   (cons (<lexical-token>-category token)
		 (<lexical-token>-value    token)))
      (json->tokens (lexer-make-IS (string: string) (counters: 'all)))))

;;; --------------------------------------------------------------------

  (check
      (doit "")
    => `((*eoi* . ,(eof-object))))

  (check
      (doit " { \"Count\": 12 }")
    => `((BEGIN_OBJECT . #\{)
	 (STRING . "Count")
	 (NAME_SEPARATOR . #\:)
	 (NUMBER . 12)
	 (END_OBJECT . #\})
	 (*eoi* . ,(eof-object))))

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
    => `((BEGIN_OBJECT . #\{)
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
	 (END_OBJECT . #\})
	 (*eoi* . ,(eof-object))))

;;; --------------------------------------------------------------------

  (check
      (doit " { \"Count\" . 12 }")
    => `((BEGIN_OBJECT . #\{)
	 (STRING . "Count")
	 (*lexer-error* . " . 12 }")))

  (check
      (doit " { \"Count\" . 12, \"ciao\": false }")
    => `((BEGIN_OBJECT . #\{)
	 (STRING . "Count")
	 (*lexer-error* . " . 12, \"ci...")))

  (check	;bad value in string
      (doit "{ \"\\u00\"")
    => '((BEGIN_OBJECT . #\{)
	 (*lexer-error* . "\\u00")))

  (check	;unexpected end of input while parsing string
      (doit "{ \"ciao")
    => '((BEGIN_OBJECT . #\{)
	 (*lexer-error* . "\"ciao")))

  #t)


(parameterise ((check-test-name 'rfc-lexer-parser))

  (define (doit string)
    (let* ((IS		(lexer-make-IS (string: string) (counters: 'all)))
	   (lexer	(make-json-rfc-lexer IS))
	   (parser	(make-json-sexp-parser)))
      (parser (lambda ()
		(let ((token (lexer)))
;;;		  (write token)(newline)
		  token))
	      (make-json-error-handler 'json-parser))))

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

  (check
      (doit "{
  \"glossary\": {
    \"title\": \"example glossary\",
    \"GlossDiv\": {
      \"title\": \"S\",
      \"GlossList\": {
        \"GlossEntry\": {
          \"ID\": \"SGML\",
          \"SortAs\": \"SGML\",
          \"GlossTerm\": \"Standard Generalized Markup Language\",
          \"Acronym\": \"SGML\",
          \"Abbrev\": \"ISO 8879:1986\",
          \"GlossDef\": {
            \"para\": \"A meta-markup language, used to create markup languages such as DocBook.\",
            \"GlossSeeAlso\": [\"GML\", \"XML\"]
          },
          \"GlossSee\": \"markup\"
        }
      }
    }
  }
}
")
    => '(("glossary" .
	  (("title" . "example glossary")
	   ("GlossDiv" .
	    (("title" . "S")
	     ("GlossList" . (("GlossEntry" .
			      (("ID" . "SGML")
			       ("SortAs" . "SGML")
			       ("GlossTerm" . "Standard Generalized Markup Language")
			       ("Acronym" . "SGML")
			       ("Abbrev" . "ISO 8879:1986")
			       ("GlossDef" .
				(("para" . "A meta-markup language, used to create markup languages such as DocBook.")
				 ("GlossSeeAlso" . #("GML" "XML"))))
			       ("GlossSee" . "markup")))))))))))

;;; --------------------------------------------------------------------

  (check
      (guard (E ((json-parser-error-condition? E)
;;;(write (condition-message E))(newline)
		 (condition-irritants E))
		(else #f))
	(doit " { \"Count\" . 12, \"ciao\": false }"))
    => '(" . 12, \"ci..."))

  (check
      (guard (E ((json-parser-error-condition? E)
;;;(write (condition-message E))(newline)
		 (condition-irritants E))
		(else #f))
	(doit " { \"Count\" , 12, \"ciao\": false }"))
    => '(","))

  (check
      (guard (E ((json-parser-error-condition? E)
;;;(write (condition-message E))(newline)
		 (condition-irritants E))
		(else #f))
	(doit " { 12, \"ciao\": false }"))
    => '("12"))

  #t)


(parameterise ((check-test-name 'extended-lexer-parser))

  (define (doit string)
    (let* ((IS		(lexer-make-IS (string: string) (counters: 'all)))
	   (lexer	(make-json-extended-lexer IS))
	   (parser	(make-json-sexp-parser))
	   (handler	(lambda (msg tok) (list 'error-handler msg tok))))
      (parser (lambda ()
		(let ((token (lexer)))
;;;		  (write token)(newline)
		  token))
	      handler)))

;;; --------------------------------------------------------------------

  (check
      (doit "{ \"ciao\" : Infinity }")
    => '(("ciao" . +inf.0)))

  (check
      (doit "{ \"ciao\" : -Infinity }")
    => '(("ciao" . -inf.0)))

  (check
      (doit "{ \"ciao\" : NaN }")
    => '(("ciao" . +nan.0)))

  (check
      (doit "{ \"ciao\" : #o0444 }")
    => '(("ciao" . 292)))

  (check
      (doit "{ \"ciao\" : #b0111 }")
    => '(("ciao" . 7)))

  (check
      (doit "{ \"ciao\" : #x0FF }")
    => '(("ciao" . 255)))

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

  (check
      (doit "{
  \"glossary\": {
    \"title\": \"example glossary\",
    \"GlossDiv\": {
      \"title\": \"S\",
      \"GlossList\": {
        \"GlossEntry\": {
          \"ID\": \"SGML\",
          \"SortAs\": \"SGML\",
          \"GlossTerm\": \"Standard Generalized Markup Language\",
          \"Acronym\": \"SGML\",
          \"Abbrev\": \"ISO 8879:1986\",
          \"GlossDef\": {
            \"para\": \"A meta-markup language, used to create markup languages such as DocBook.\",
            \"GlossSeeAlso\": [\"GML\", \"XML\"]
          },
          \"GlossSee\": \"markup\"
        }
      }
    }
  }
}
")
    => '(("glossary" .
	  (("title" . "example glossary")
	   ("GlossDiv" .
	    (("title" . "S")
	     ("GlossList" . (("GlossEntry" .
			      (("ID" . "SGML")
			       ("SortAs" . "SGML")
			       ("GlossTerm" . "Standard Generalized Markup Language")
			       ("Acronym" . "SGML")
			       ("Abbrev" . "ISO 8879:1986")
			       ("GlossDef" .
				(("para" . "A meta-markup language, used to create markup languages such as DocBook.")
				 ("GlossSeeAlso" . #("GML" "XML"))))
			       ("GlossSee" . "markup")))))))))))

  #t)


(parameterise ((check-test-name 'event-lexer-parser))

  (define (doit string)
    (let* ((IS		(lexer-make-IS (string: string) (counters: 'all)))
	   (lexer	(make-json-rfc-lexer IS))
	   (result	'())
	   (handler	(lambda args
			  (set-cons! result args)))
	   (parser	(make-json-event-parser
			 (begin-object:		handler)
			 (end-object:		handler)
			 (begin-array:		handler)
			 (end-array:		handler)
			 (begin-pair:		handler)
			 (end-pair:		handler)
			 (atom:			handler))))
      (parser lexer
	      ;; (lambda ()
	      ;; 	(let ((token (lexer)))
	      ;; 	  (write token)(newline)
	      ;; 	  token))
	      (make-json-error-handler 'json-parser))
      (reverse result)))

;;; --------------------------------------------------------------------

  (check
      (doit "{ }")
    => '((begin-object)
	 (end-object)))

  (check
      (doit "{ \"key\" : 123 }")
    => '((begin-object)
	 (begin-pair "key")
	 (number 123)
	 (end-pair)
	 (end-object)))

  (check
      (doit "{ \"key\" : \"value\" }")
    => '((begin-object)
	 (begin-pair "key")
	 (string "value")
	 (end-pair)
	 (end-object)))

  (check
      (doit "{ \"key\" : true, \"yek\": false }")
    => '((begin-object)
	 (begin-pair "key")
	 (true #t)
	 (end-pair)
	 (begin-pair "yek")
	 (false #f)
	 (end-pair)
	 (end-object)))

  (check
      (doit "{ \"key\" : true, \"yek\": false, \"wop\": null }")
    => '((begin-object)
	 (begin-pair "key")
	 (true #t)
	 (end-pair)
	 (begin-pair "yek")
	 (false #f)
	 (end-pair)
	 (begin-pair "wop")
	 (null ())
	 (end-pair)
	 (end-object)))

;;; --------------------------------------------------------------------

  (check
      (doit "[ ]")
    => '((begin-array)
	 (end-array)))

  (check
      (doit "[ 1 ]")
    => '((begin-array)
	 (number 1)
	 (end-array)))

  (check
      (doit "[ 1, 2, 3 ]")
    => '((begin-array)
	 (number 1)
	 (number 2)
	 (number 3)
	 (end-array)))

  (check
      (doit "[ 1, \"ciao\", true ]")
    => '((begin-array)
	 (number 1)
	 (string "ciao")
	 (true #t)
	 (end-array)))

  (check
      (doit "[ 1, { \"ciao\": 123 }, true ]")
    => '((begin-array)
	 (number 1)
	 (begin-object)
	 (begin-pair "ciao")
	 (number 123)
	 (end-pair)
	 (end-object)
	 (true #t)
	 (end-array)))

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
    => '((begin-object)
	 (begin-pair "Image")
	 (begin-object)
	 (begin-pair "Width") (number 800) (end-pair)
	 (begin-pair "Height") (number 600) (end-pair)
	 (begin-pair "Title") (string "View from 15th Floor") (end-pair)
	 (begin-pair "Thumbnail")
	 (begin-object)
	 (begin-pair "Url") (string "http://www.example.com/image/481989943") (end-pair)
	 (begin-pair "Height") (number 125) (end-pair)
	 (begin-pair "Width") (string "100") (end-pair)
	 (end-object)
	 (end-pair)
	 (begin-pair "IDs")
	 (begin-array) (number 116) (number 943) (number 234) (number 38793) (end-array)
	 (end-pair)
	 (end-object)
	 (end-pair)
	 (end-object)))

;;; --------------------------------------------------------------------

  (check
      (guard (E ((json-parser-error-condition? E)
;;;(write (condition-message E))(newline)
		 (condition-irritants E))
		(else #f))
	(doit " { \"Count\" . 12, \"ciao\": false }"))
    => '(" . 12, \"ci..."))

  (check
      (guard (E ((json-parser-error-condition? E)
;;;(write (condition-message E))(newline)
		 (condition-irritants E))
		(else #f))
	(doit " { \"Count\" , 12, \"ciao\": false }"))
    => '(","))

  (check
      (guard (E ((json-parser-error-condition? E)
;;;(write (condition-message E))(newline)
		 (condition-irritants E))
		(else #f))
	(doit " { 12, \"ciao\": false }"))
    => '("12"))

  (check	;;end of input while lexing string
      (guard (E ((json-parser-error-condition? E)
;;;(write (condition-message E))(newline)
		 (condition-irritants E))
		(else #f))
	(doit " { \"ciao"))
    => '("\"ciao"))

  (check
      (guard (E ((json-parser-error-condition? E)
;;;(write (condition-message E))(newline)
		 (condition-irritants E))
		(else #f))
	(doit " { \"ciao\": }"))
    => '("}"))

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
      (json-make-pair* "ciao" 123)
    => "\"ciao\": 123")

  (check
      (json-make-pair* "ciao" +nan.0)
    => "\"ciao\": NaN")

  (check
      (json-make-pair* "ciao" +inf.0)
    => "\"ciao\": Infinity")

  (check
      (json-make-pair* "ciao" -inf.0)
    => "\"ciao\": -Infinity")

  (check
      (json-make-pair* "ciao" "hello")
    => "\"ciao\": \"hello\"")

  (check
      (json-make-pair* "ciao" #t)
    => "\"ciao\": true")

  (check
      (json-make-pair* "ciao" #f)
    => "\"ciao\": false")

  (check
      (json-make-pair* "ciao" '())
    => "\"ciao\": null")

  (check
      (json-make-pair* "hey" "{ \"ciao\": 123 }" #f)
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

  (check
      (json-make-array '#())
    => "[  ]")

  (check
      (json-make-array '#("12" "34" "56"))
    => "[ 12, 34, 56 ]")

  #t)


;;;; done

(check-report)

;;; end of file
