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
    make-json-parser
    json-encode-string		json-decode-string
    json-make-pair		json-make-pair*
    json-make-object
    json-make-array)
  (import (nausicaa)
    (silex lexer)
    (json string-lexer)
    (json rfc-lexer)
    (json extended-lexer)
    (json parser)
    (parser-tools lexical-token)
    (parser-tools source-location))


(define (make-json-rfc-lexer IS)
  (%make-json-lexer IS json-rfc-lexer-table))

(define (make-json-extended-lexer IS)
  (%make-json-lexer IS json-extended-lexer-table))

(define (%make-json-lexer IS table)
  (let ((lexer (lexer-make-lexer table IS)))
    (lambda ()
      (let ((token (lexer)))
	(if (eq? 'QUOTED-TEXT-OPEN (<lexical-token>-category token))
	    (%lex-string IS token)
	  token)))))

(define (%lex-string IS opening-token)
  (let-values (((lexer)		(lexer-make-lexer json-string-lexer-table IS))
	       ((port getter)	(open-string-output-port)))
    (do ((token (lexer) (lexer)))
	((eq? token 'QUOTED-TEXT-CLOSE)
	 (let ((pos  (<lexical-token>-location opening-token))
	       (text (getter)))
	   (make-<lexical-token> 'STRING
				 (make-<source-location>
				  (<source-location>-input  pos)
				  (<source-location>-line   pos)
				  (<source-location>-column pos)
				  (<source-location>-offset pos))
				 text (string-length text))))
      (if (<lexical-token>?/end-of-input token)
	  (error 'json-string-lexer "unexpected end of input while parsing string")
	(display token port)))))

(define (json->tokens IS)
  (let ((lexer (make-json-rfc-lexer IS)))
    (let loop ((token		(lexer))
	       (list-of-tokens	'()))
      (if (<lexical-token>?/special token)
	  (reverse list-of-tokens)
	(loop (lexer) (cons token list-of-tokens))))))


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
