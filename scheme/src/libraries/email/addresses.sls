;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: email address parser
;;;Date: Thu Jul 30, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
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
(library (email addresses)
  (export

    ;; lexers
    make-address-lexer		address->tokens

    ;; parsers
    address-parse-domain
    address-parse-local-part
    address-parse-addr-spec

    ;; domain data type
    make-domain			domain?
    domain-subdomains
    domain-display		domain-write		domain->string

    ;; local part data type
    make-local-part		local-part?
    local-part-subparts
    local-part-display		local-part-write	local-part->string

    ;; addr-spec data type
    make-addr-spec		addr-spec?
    addr-spec-local-part	addr-spec-domain
    addr-spec-display		addr-spec-write		addr-spec->string

    )
  (import (rnrs)
    (silex lexer)
    (email address-strings-lexer)
    (email address-comments-lexer)
    (email address-domain-literals-lexer)
    (email address-lexer)
    (parameters)
    (strings))


(define (address->tokens . options)
  (let ((lexer		(apply make-address-lexer options))
	(list-of-tokens	'()))
    (do ((token (lexer) (lexer)))
	((not token)
	 (reverse list-of-tokens))
      (set! list-of-tokens (cons token list-of-tokens)))))

(define (make-address-lexer . options)
  (let* ((IS	(apply lexer-make-IS (append (list :counters 'all) options)))
	 (lexer	(lexer-make-lexer email-address-table IS)))
    (lambda ()
      (lex-dispatch-token (lexer) IS))))

;;; --------------------------------------------------------------------

(define (lex-dispatch-token token IS)
  (define (lex-comment-token IS)
    (let ((lexer (lexer-make-lexer email-address-comments-table IS)))
      (do ((token  (lexer) (lexer))
	   (result ""))
	  ((not token)
	   (cons 'comment result))
	(set! result (string-append result
				    (if (eq? token 'comment)
					(string-append "(" (lex-comment-token IS) ")")
				      token))))))

  (define (lex-quoted-text-token IS)
    (let ((lexer (lexer-make-lexer email-address-strings-table IS)))
      (do ((token (lexer) (lexer))
	   (text  ""))
	  ((not token)
	   (cons 'quoted-text text))
	(set! text (string-append text token)))))

  (define (lex-domain-literal-token IS)
    (let ((lexer (lexer-make-lexer email-address-domain-literals-table IS)))
      (do ((token (lexer) (lexer))
	   (dtext  ""))
	  ((not token)
	   (cons 'domain-literal dtext))
	(set! dtext (string-append dtext token)))))

  (case token
    ((quoted-text)	(lex-quoted-text-token IS))
    ((comment)		(lex-comment-token IS))
    ((domain-literal)	(lex-domain-literal-token IS))
    (else		token)))


;;;; address domain record

(define-record-type domain
  (fields (immutable subdomains)))

(define domain-display
  (case-lambda
   ((domain)
    (domain-display domain (current-output-port)))
   ((domain port)
    (display (string-append "#<domain -- "
			    (string-join (domain-subdomains domain) ".")
			    ">")
	     port))))

(define domain-write
  (case-lambda
   ((domain)
    (domain-display domain (current-output-port)))
   ((domain port)
    (display "(make-domain " port)
    (write (domain-subdomains domain) port)
    (display ")" port))))

(define (domain->string domain)
  (string-join (domain-subdomains domain) "."))


;;;; address local part record

(define-record-type local-part
  (fields (immutable subparts)))

(define local-part-display
  (case-lambda
   ((local-part)
    (local-part-display local-part (current-output-port)))
   ((local-part port)
    (display (string-append "#<local-part -- "
			    (string-join (local-part-subparts local-part) ".")
			    ">")
	     port))))

(define local-part-write
  (case-lambda
   ((local-part)
    (local-part-display local-part (current-output-port)))
   ((local-part port)
    (display "(make-local-part " port)
    (write (local-part-subparts local-part) port)
    (display ")" port))))

(define (local-part->string local-part)
  (string-join (local-part-subparts local-part) "."))


;;;; address addr-spec record

(define-record-type addr-spec
  (fields (immutable local-part)
	  (immutable domain)))

(define addr-spec-display
  (case-lambda
   ((addr-spec)
    (addr-spec-display addr-spec (current-output-port)))
   ((addr-spec port)
    (display (string-append "#<addr-spec -- "
			    (addr-spec->string addr-spec)
			    ">")
	     port))))

(define addr-spec-write
  (case-lambda
   ((addr-spec)
    (addr-spec-display addr-spec (current-output-port)))
   ((addr-spec port)
    (display "(make-addr-spec " port)
    (local-part-write (addr-spec-local-part addr-spec) port)
    (display " " port)
    (domain-write (addr-spec-domain addr-spec) port)
    (display ")" port))))

(define (addr-spec->string addr-spec)
  (string-append (local-part->string (addr-spec-local-part addr-spec))
		 "@"
		 (domain->string (addr-spec-domain addr-spec))))


;;;; parser helpers

(define proc-name
  ;;Set  to the  current parser  function name.   It is  used  by helper
  ;;functions to report errors.
  ;;
  (make-parameter #f))

(define parsing-object-descr
  ;;Set to  a string describing  the address component we  are currently
  ;;parsing.  It is used by helper functions to report errors.
  ;;
  (make-parameter #f))

(define (assert-expected-token token token-type token-pred token-type-descr)
  ;;Assert that  TOKEN is of  type TOKEN-TYPE and  satisfies TOKEN-PRED.
  ;;If not raise an assertion violation.
  ;;
  (assert-token token)
  (unless (and (eq? token-type (car token))
	       (token-pred (cdr token)))
    (assertion-violation (proc-name)
      (string-append "expected " token-type-descr " while parsing " (parsing-object-descr))
      token)))

(define (assert-token token)
  ;;Assert that TOKEN is true.  If not raise an assertion violation.
  ;;
  (unless token
    (assertion-violation (proc-name)
      (string-append "found end of address while parsing " (parsing-object-descr)))))


;;;; parser functions

(define (address-parse-domain lexer)
  (parameterise ((proc-name		'address-parse-domain)
		 (parsing-object-descr	"address domain"))
    (parse-dotted-strings lexer make-domain "subdomain string")))

(define (address-parse-local-part lexer)
  (parameterise ((proc-name		'address-parse-local-part)
		 (parsing-object-descr	"address local part"))
    (parse-dotted-strings lexer make-local-part "local part string")))

(define (parse-dotted-strings lexer make-object atom-descr)
  ;;Parse a sequence of tokens:
  ;;
  ;;	atom (character atom)*
  ;;
  ;;in which the characters are dots.
  ;;
  (let ((token-first (lexer)))
    (assert-expected-token token-first 'atom string? atom-descr)
    (let loop ((list-of-strings (list (cdr token-first))))
      (let ((token-dot (lexer)))
	(if (not token-dot)
	    (make-object (reverse list-of-strings))
	  (begin
	    (assert-expected-token token-dot 'character
				   (lambda (obj) (char=? #\. obj))
				   "dot separator")
	    (let ((token-atom (lexer)))
	      (assert-expected-token token-atom 'atom string? atom-descr)
	      (loop (cons (cdr token-atom) list-of-strings)))))))))

(define (address-parse-addr-spec lexer)
  ;;Parse the sequence:
  ;;
  ;;	local-part #\@ domain
  ;;
  (parameterise ((proc-name		'address-parse-addr-spec)
		 (parsing-object-descr	"addr spec"))
    (let ((domain (address-parse-domain lexer)))
      (write domain)(newline)
      (assert-expected-token (lexer) 'character
			     (lambda (obj) (char=? #\@ obj))
			     "at separator")
      (let ((local-part (address-parse-local-part lexer)))
	(make-addr-spec (make-local-part local-part)
			(make-domain     domain))))))


;;;; done

)

;;; end of file
