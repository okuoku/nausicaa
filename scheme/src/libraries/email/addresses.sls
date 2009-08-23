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
;;     address-parse-domain
;;     address-parse-local-part
;;     address-parse-addr-spec

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
    (lalr common)
    (email addresses quoted-text-lexer)
    (email addresses comments-lexer)
    (email addresses domain-literals-lexer)
    (email addresses lexer)
;;;    (email addresses parser)
    (debugging)
    (strings))


;;;; helpers

(define %at-string		"@")
(define %colon-string		":")
(define %comma-string		",")
(define %dot-string		".")
(define %semicolon-string	";")
(define %space-string		" ")

(define-syntax define-inline
  (syntax-rules ()
    ((_ (?name ?arg ...) ?form0 ?form ...)
     (define-syntax ?name
       (syntax-rules ()
	 ((_ ?arg ...)
	  (begin ?form0 ?form ...)))))))


(define (make-address-lexer IS)
  (let ((lexers		(list (lexer-make-lexer address-table IS)))
	(dispatchers	'()))

    (define (main-dispatch lexer)
      (debug "main-dispatch enter")
      (let ((token (lexer)))
	(debug "main-dispactch token: ~s" token)
	(case (lexical-token-category token)
	  ((QUOTED-TEXT-OPEN)
	   (lex-quoted-text-token IS token))
	  ((COMMENT-OPEN)
	   (lex-comment-token IS token))
	  ((DOMAIN-LITERAL-OPEN)
	   (push-lexer-and-dispatcher (lexer-make-lexer domain-literals-table IS)
				      domain-literal-dispatch)
	   token)
	  (else
	   token))))

    (define (domain-literal-dispatch lexer)
      (let ((token (lexer)))
	(case (lexical-token-category token)
	  ((DOMAIN-LITERAL-CLOSE *lexer-error*)
	   (pop-lexer-and-dispatcher)
	   token)
	  (else
	   token))))

    (define (lex-comment-token IS opening-token)
      ;;To be called after the lexer returned a "COMMENT-OPEN" lexical
      ;;token, which must be in OPENING-TOKEN.
      ;;
      ;;Accumulate  the text  of  the comment  into  a single  string,
      ;;including the nested comments.   Return the whole comment as a
      ;;string.
      ;;
      (let* ((lexer	(lexer-make-lexer comments-table IS))
	     (text  ""))
	(do ((token  (lexer) (lexer)))
	    ((eq? token 'COMMENT-CLOSE)
	     (make-lexical-token 'COMMENT
				 (make-source-location #f
						       ((lexer-get-func-line   IS))
						       ((lexer-get-func-column IS))
						       ((lexer-get-func-offset IS))
						       (string-length text))
				 text))
	  (set! text (string-append
		      text
		      (if (eq? token 'COMMENT-OPEN)
			  (string-append "(" (lex-comment-token IS) ")")
			token))))))

    (define (lex-quoted-text-token IS opening-token)
      ;;To  be  called after  the  lexer  returned a  "QUOTED-TEXT-OPEN"
      ;;lexical token, which must be in OPENING-TOKEN.
      ;;
      ;;Accumulate the quoted text into  a string.  Return a new lexical
      ;;token record with category QUOTED-TEXT.
      ;;
      (let ((lexer (lexer-make-lexer quoted-text-table IS))
	    (text  ""))
	(do ((token (lexer) (lexer)))
	    ((eq? token 'QUOTED-TEXT-CLOSE)
	     (let ((pos (lexical-token-source opening-token)))
	       (make-lexical-token 'QUOTED-TEXT
				   (make-source-location
				    (source-location-input  pos)
				    (source-location-line   pos)
				    (source-location-column pos)
				    (source-location-offset pos)
				    (string-length text))
				   text)))
	  (set! text (string-append text token)))))

    (define-inline (push-lexer-and-dispatcher lex disp)
      (set! dispatchers (cons disp dispatchers))
      (set! lexers      (cons lex lexers)))

    (define-inline (pop-lexer-and-dispatcher)
      (set! dispatchers (cdr dispatchers))
      (set! lexers      (cdr lexers)))

    (set! dispatchers (list main-dispatch))
    (lambda ()
      (debug "enter lexer")
      ((car dispatchers) (car lexers)))))


;;;; utilities

(define (address->tokens IS)
  (let ((lexer (make-address-lexer IS)))
    (let loop ((token		(lexer))
	       (list-of-tokens	'()))
      (if (lexical-token?/end-of-input token)
	  (reverse list-of-tokens)
	(loop (lexer) (cons token list-of-tokens))))))


;;;; address domain record

(define-record-type domain
  (fields (immutable subdomains)))

(define domain-display
  (case-lambda
   ((domain)
    (domain-display domain (current-output-port)))
   ((domain port)
    (display (string-append "#<domain -- "
			    (string-join (domain-subdomains domain) %dot-string)
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
  (string-join (domain-subdomains domain) %dot-string))


;;;; address domain-literal record

(define-record-type domain-literal
  (fields (immutable decimal-1)
	  (immutable decimal-2)
	  (immutable decimal-3)
	  (immutable decimal-4)))

(define domain-literal-display
  (case-lambda
   ((domain)
    (domain-literal-display domain (current-output-port)))
   ((domain port)
    (display (string-append "#<domain-literal -- ["
			    (domain-literal->string domain)
			    "]>")
	     port))))

(define domain-literal-write
  (case-lambda
   ((domain)
    (domain-literal-display domain (current-output-port)))
   ((domain port)
    (display "(make-domain-literal " port)
    (domain-literal-decimal-1 domain)
    %space-string
    (domain-literal-decimal-2 domain)
    %space-string
    (domain-literal-decimal-3 domain)
    %space-string
    (domain-literal-decimal-4 domain)
    (display ")" port))))

(define (domain-literal->string domain)
  (string-append (domain-literal-decimal-1 domain)
		 %dot-string
		 (domain-literal-decimal-2 domain)
		 %dot-string
		 (domain-literal-decimal-3 domain)
		 %dot-string
		 (domain-literal-decimal-4 domain)))


;;;; address local part record

(define-record-type local-part
  (fields (immutable subparts)))

(define local-part-display
  (case-lambda
   ((local-part)
    (local-part-display local-part (current-output-port)))
   ((local-part port)
    (display (string-append "#<local-part -- "
			    (string-join (local-part-subparts local-part) %dot-string)
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
  (string-join (local-part-subparts local-part) %dot-string))


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



;;;; done

)

;;; end of file
