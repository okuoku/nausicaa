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
    make-address-lexer		address->tokens		 address-lexer-allows-comments
    make-address-parser

    ;; domain data type
    make-domain			domain?			domain?/or-false
    assert-domain		assert-domain/or-false
    domain-subdomains		domain-literal?
    domain-display		domain-write		domain->string

    ;; local part data type
    make-local-part		local-part?
    local-part-subparts
    local-part-display		local-part-write	local-part->string

    ;; addr-spec data type
    make-addr-spec		addr-spec?
    addr-spec-local-part	addr-spec-domain
    addr-spec-display		addr-spec-write		addr-spec->string

    ;; route data type
    make-route			route?			route-domains
    route-display		route-write		route->string

    ;; route address data type
    make-mailbox		mailbox?
    mailbox-display-name	mailbox-route		mailbox-addr-spec
    mailbox-display		mailbox-write		mailbox->string

    ;; group data type
    make-group			group?
    group-display-name		group-mailboxes
    group-display		group-write		group->string
    )
  (import (rnrs)
    (silex lexer)
    (lalr common)
    (email addresses common)
    (email addresses quoted-text-lexer)
    (email addresses comments-lexer)
    (email addresses domain-literals-lexer)
    (email addresses lexer)
    (email addresses parser)
;;;    (debugging)
    (parameters)
    (parser-tools lexical-token)
    (parser-tools source-location))


;;;; helpers

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
      (let ((token (lexer)))
	(case (<lexical-token>-category token)
	  ((QUOTED-TEXT-OPEN)
	   (lex-quoted-text-token IS token))
	  ((COMMENT-OPEN)
	   (let ((comment-token (lex-comment-token IS token)))
	     (if (address-lexer-allows-comments)
		 comment-token
	       (main-dispatch lexer))))
	  ((DOMAIN-LITERAL-OPEN)
	   (push-lexer-and-dispatcher (lexer-make-lexer domain-literals-table IS)
				      domain-literal-dispatch)
	   token)
	  (else
	   token))))

    (define (domain-literal-dispatch lexer)
      (let ((token (lexer)))
	(case (<lexical-token>-category token)
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
	     (make-<lexical-token> 'COMMENT
				   (make-<source-location> #f
							   ((lexer-get-func-line   IS))
							   ((lexer-get-func-column IS))
							   ((lexer-get-func-offset IS)))
				   text (string-length text)))
	  (set! text (string-append
		      text
		      (if (eq? token 'COMMENT-OPEN)
			  (string-append "(" (lex-comment-token IS token) ")")
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
	     (let ((pos (<lexical-token>-source opening-token)))
	       (make-<lexical-token> 'QUOTED-TEXT
				     (make-<source-location>
				      (<source-location>-input  pos)
				      (<source-location>-line   pos)
				      (<source-location>-column pos)
				      (<source-location>-offset pos))
				     text (string-length text))))
	  (set! text (string-append text token)))))

    (define-inline (push-lexer-and-dispatcher lex disp)
      (set! dispatchers (cons disp dispatchers))
      (set! lexers      (cons lex lexers)))

    (define-inline (pop-lexer-and-dispatcher)
      (set! dispatchers (cdr dispatchers))
      (set! lexers      (cdr lexers)))

    (set! dispatchers (list main-dispatch))
    (lambda ()
      ((car dispatchers) (car lexers)))))

(define address-lexer-allows-comments
  (make-parameter #f))


;;;; utilities

(define (address->tokens IS)
  (let ((lexer (make-address-lexer IS)))
    (let loop ((token		(lexer))
	       (list-of-tokens	'()))
      (if (<lexical-token>?/special token)
	  (reverse list-of-tokens)
	(loop (lexer) (cons token list-of-tokens))))))


;;;; done

)

;;; end of file
