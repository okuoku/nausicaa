;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: lexer and parser for JSON
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


(library (json)
  (export
    make-json-rfc-lexer		make-json-extended-lexer
    json->tokens
    make-json-sexp-parser	make-json-error-handler
    json-encode-string		json-decode-string
    json-make-pair		json-make-pair*
    json-make-object
    json-make-array

    &json-parser-error
    make-json-parser-error-condition
    json-parser-error-condition?)
  (import (nausicaa)
    (makers)
    (conditions)
    (silex lexer)
    (json string-lexer)
    (json rfc-lexer)
    (json extended-lexer)
    (json sexp-parser)
    (parser-tools lexical-token)
    (parser-tools source-location))


(define-condition &json-parser-error
  (parent &error))

(define (make-json-error-handler who)
  (lambda (message (token <lexical-token>))
    (raise
     (condition
      (make-json-parser-error-condition)
      (make-who-condition who)
      (make-message-condition
       (let (((pos <source-location>) token.location))
	 (string-append "invalid JSON input at line " (number->string pos.line)
			" column " (number->string pos.column)
			" offset " (number->string pos.offset)
			": " message)))
      (make-irritants-condition (list (cond ((string? token.value)
					     token.value)
					    ((number? token.value)
					     (number->string token.value))
					    ((char? token.value)
					     (string token.value))
					    ((eqv? #t token.value)
					     "true")
					    ((eqv? #f token.value)
					     "false")
					    ((null? token.value)
					     "null")
					    (else
					     (assertion-violation who
					       "internal error: unexpected invalid token value"
					       token.value)))))))))


;;;; lexers

(define (make-json-rfc-lexer IS)
  (%make-json-lexer IS json-rfc-lexer-table))

(define (make-json-extended-lexer IS)
  (%make-json-lexer IS json-extended-lexer-table))

(define (%make-json-lexer IS table)
  (let ((lexer (lexer-make-lexer table IS)))
    (lambda ()
      (let (((token <lexical-token>) (lexer)))
	(if (eq? 'QUOTED-TEXT-OPEN token.category)
	    (%lex-string IS token)
	  token)))))

(define (%lex-string IS (opening-token <lexical-token>))
  (let-values (((lexer)		(lexer-make-lexer json-string-lexer-table IS))
	       ((port getter)	(open-string-output-port)))
    (let loop ((token (lexer)))
      (cond ((eq? token 'QUOTED-TEXT-CLOSE)
	     (let ((text (getter)))
	       (make-<lexical-token> 'STRING opening-token.location text (string-length text))))
	    ((<lexical-token>?/end-of-input token)
	     (let ((text opening-token.value))
	       (make-<lexical-token> '*lexer-error* opening-token.location
				     text (string-length text))))
	    ((<lexical-token>?/lexer-error token)
	     token)
	    (else
	     (display token port)
	     (loop (lexer)))))))

(define (json->tokens IS)
  (let ((lexer (make-json-rfc-lexer IS)))
    (let loop ((token		(lexer))
	       (list-of-tokens	'()))
      (if (<lexical-token>?/special token)
	  (reverse (cons token list-of-tokens))
	(loop (lexer) (cons token list-of-tokens))))))


;;;; event parser

(define-maker make-json-event-parser
  event-parser
  ((:begin-object	#f)
   (:end-object		#f)
   (:begin-array	#f)
   (:end-array		#f)
   (:begin-pair		#f)
   (:atom		#f)))

(define (event-parser begin-object-handler end-object-handler
		      begin-array-handler  end-array-handler
		      begin-pair-handler atom-handler)

  (define who 'json-event-parser)

  (define-syntax case-token
    (syntax-rules ()
      ((_ ?token ?error-message ((?category ...) . ?body) ...)
       (let (((token <lexical-token>) ?token))
	 (case token.category
	   ((?category ...) . ?body)
	   ...
	   (else
	    (%error ?error-message token)))))))

  (define %error
    (make-json-error-handler who))

  (lambda (lexer)

    (define (%parse-object)
      ;;Parse an object.  To be  called after the BEGIN_OBJECT token has
      ;;been parsed.
      ;;
      (begin-object-handler 'begin-object)
      (let next-pair ()
	(unless (%parse-pair)
	  (let (((token <lexical-token>) (lexer)))
	    (case-token
	     token "expected end of object or value separator after pair"
	     ((VALUE_SEPARATOR)
	      (next-pair)))))))

    (define (%parse-array)
      ;;Parse  the list  of  array  elements.  To  be  called after  the
      ;;BEGIN_ARRAY token has been parsed.
      ;;
      (begin-array-handler 'begin-array)
      (let next-value ()
	(%parse-value)
	(case-token
	 (lexer) "expected value separator or end of array structural character"
	 ((VALUE_SEPARATOR)
	  (next-value))
	 ((END_ARRAY)
	  (end-array-handler 'end-array)))))

    (define (%parse-pair)
      ;;Parse a pair in an  object.  To be called after the BEGIN_OBJECT
      ;;or VALUE_SEPARATOR  token has been  parsed.  Return true  if the
      ;;END_OBJECT token was found.
      ;;
      (let (((token <lexical-token>) (lexer)))
	(case-token
	 token "expected end of object or string as name of pair"
	 ((STRING)
	  (case-token
	   (lexer) "expected name separator after pair's name"
	   ((NAME_SEPARATOR)
	    (begin-pair-handler 'begin-pair token.value)
	    (%parse-value)
	    #f)))
	 ((END_OBJECT)
	  #t))))

    (define (%parse-value)
      ;;Parse the value of a pair or an array element.
      ;;
      (let (((token <lexical-token>) (lexer)))
	(case-token
	 token "expected value"
	 ((FALSE)
	  (atom-handler 'false #f))
	 ((TRUE)
	  (atom-handler 'true #t))
	 ((NULL)
	  (atom-handler 'null '()))
	 ((NUMBER)
	  (atom-handler 'number token.value))
	 ((STRING)
	  (atom-handler 'string token.value))
	 ((BEGIN_OBJECT)
	  (%parse-object))
	 ((BEGIN_ARRAY)
	  (%parse-array)))))

    (case-token (lexer) "expected object or array"
		((BEGIN_OBJECT)
		 (%parse-object))
		((BEGIN_ARRAY)
		 (%parse-array)))))


;;;; string encoding and decoding

(define (json-encode-string in-str)
  (let-values (((port getter) (open-string-output-port)))
    (string-for-each (lambda (ch)
		       (display (case ch
				  ((#\")		"\\\"")
				  ((#\\)		"\\\\")
				  ((#\/)		"\\/")
				  ((#\backspace)	"\\b")
				  ((#\page)		"\\f")
				  ((#\linefeed)		"\\n")
				  ((#\return)		"\\r")
				  ((#\tab)		"\\t")
				  (else
				   (let ((n (char->integer ch)))
				     (if (<= 32 n 126)
					 ch
				       (let ((hex (number->string n 16)))
					 (string-append "\\u"
							(case (string-length hex)
							  ((0)  "0000")
							  ((1)  "000")
							  ((2)  "00")
							  ((3)  "0")
							  (else ""))
							hex))))))
				port))
		     in-str)
    (getter)))

(define (json-decode-string in-string)
  (let* ((IS	(lexer-make-IS (:string in-string) (:counters 'all)))
	 (lexer	(lexer-make-lexer json-string-lexer-table IS)))
    (let-values (((port getter) (open-string-output-port)))
      (do ((token (lexer) (lexer)))
	  ((<lexical-token>?/end-of-input token)
	   (getter))
	(display token port)))))


;;;; generator

(define json-make-pair
  (case-lambda
   ((name value)
    (json-make-pair name value #t))
   ((name value encode-value?)
    (assert (string? name))
    (string-append "\"" (json-encode-string name) "\": "
		   (cond ((string? value)
			  (if encode-value?
			      (string-append "\"" (json-encode-string value) "\"")
			    value))
			 ((number? value)
			  (if (or (nan? value) (infinite? value))
			      (error 'json-make-pair
				"attempt to encode NaN or infinite number, which is invalid JSON" value)
			    (number->string value)))
			 ((eqv? #t value)
			  "true")
			 ((eqv? #f value)
			  "false")
			 ((null? value)
			  "null")
			 )))))

(define json-make-pair*
  (case-lambda
   ((name value)
    (json-make-pair* name value #t))
   ((name value encode-value?)
    (assert (string? name))
    (string-append "\"" (json-encode-string name) "\": "
		   (cond ((string? value)
			  (if encode-value?
			      (string-append "\"" (json-encode-string value) "\"")
			    value))
			 ((number? value)
			  (cond ((nan? value)
				 "NaN")
				((infinite? value)
				 (if (positive? value)
				     "Infinity"
				   "-Infinity"))
				(else
				 (number->string value))))
			 ((eqv? #t value)
			  "true")
			 ((eqv? #f value)
			  "false")
			 ((null? value)
			  "null")
			 )))))

(define (json-make-object . the-pairs)
  (assert (for-all string? the-pairs))
  (let-values (((port getter) (open-string-output-port)))
    (display "{ " port)
    (unless (null? the-pairs)
      (display (car the-pairs) port)
      (for-each (lambda (val)
		  (display ", " port)
		  (display val port))
	(cdr the-pairs)))
    (display " }" port)
    (getter)))

(define (json-make-array the-values)
  (let ((list-of-values (cond ((list? the-values)
			       the-values)
			      ((vector? the-values)
			       (vector->list the-values))
			      (else
			       (assertion-violation 'json-make-array
				 "expected list or vector of values for JSON array")))))
    (let-values (((port getter) (open-string-output-port)))
      (display "[ " port)
      (unless (null? list-of-values)
	(display (car list-of-values) port)
	(for-each (lambda (val)
		    (display ", " port)
		    (display val port))
	  (cdr list-of-values)))
      (display " ]" port)
      (getter))))


;;;; done

)

;;; end of file
