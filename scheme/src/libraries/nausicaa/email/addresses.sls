;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: email address parser
;;;Date: Thu Jul 30, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa email addresses)
  (export

    ;; lexers
    make-address-lexer		address->tokens		 address-lexer-allows-comments
    make-address-parser

    ;; domain data type
    <domain>
    make-<domain>
    <domain>?			<domain>?/or-false
    assert-<domain>		assert-<domain>/or-false
    <domain>-subdomains		<domain>-literal?

    ;; local part data type
    <local-part>
    make-<local-part>
    <local-part>?
    <local-part>-subparts

    ;; addr-spec data type
    <addr-spec>
    make-<addr-spec>
    <addr-spec>?
    <addr-spec>-local-part	<addr-spec>-domain

    ;; route data type
    <route>
    make-<route>
    <route>?
    <route>-domains

    ;; route address data type
    <mailbox>
    make-<mailbox>
    <mailbox>?
    <mailbox>-display-name	<mailbox>-route		<mailbox>-addr-spec

    ;; group data type
    <group>
    make-<group>
    <group>?
    <group>-display-name	<group>-mailboxes)
  (import (nausicaa)
    (nausicaa email addresses common)
    (nausicaa email addresses quoted-text-lexer)
    (nausicaa email addresses comments-lexer)
    (nausicaa email addresses domain-literals-lexer)
    (nausicaa email addresses lexer)
    (nausicaa email addresses parser)
    (prefix (nausicaa silex lexer) lex.)
    (nausicaa parser-tools))


(define (make-address-lexer IS)
  (let ((lexers		(list (lex.make-lexer address-table IS)))
	(dispatchers	'())
	(allow-comments	(address-lexer-allows-comments)))

    (define (main-dispatch lexer)
      (let (((token <lexical-token>) (lexer)))
	(case token.category
	  ((QUOTED-TEXT-OPEN)
	   (lex-quoted-text-token IS token))
	  ((COMMENT-OPEN)
	   (let ((comment-token (lex-comment-token IS token)))
	     (if allow-comments
		 comment-token
	       (main-dispatch lexer))))
	  ((DOMAIN-LITERAL-OPEN)
	   (push-lexer-and-dispatcher (lex.make-lexer domain-literals-table IS)
				      domain-literal-dispatch)
	   token)
	  (else
	   token))))

    (define (domain-literal-dispatch lexer)
      (let (((token <lexical-token>) (lexer)))
	(case token.category
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
      (let* ((lexer	(lex.make-lexer comments-table IS))
	     (text  ""))
	(do ((token  (lexer) (lexer)))
	    ((eq? token 'COMMENT-CLOSE)
	     (make* <lexical-token> 'COMMENT
		    (make* <source-location>
		      #f
		      ((lex.lexer-get-func-line   IS))
		      ((lex.lexer-get-func-column IS))
		      ((lex.lexer-get-func-offset IS)))
		    text (string-length text)))
	  (set! text (string-append
		      text
		      (if (eq? token 'COMMENT-OPEN)
			  (string-append "(" (lex-comment-token IS token) ")")
			token))))))

    (define (lex-quoted-text-token IS (opening-token <lexical-token>))
      ;;To  be  called after  the  lexer  returned a  "QUOTED-TEXT-OPEN"
      ;;lexical token, which must be in OPENING-TOKEN.
      ;;
      ;;Accumulate the quoted text into  a string.  Return a new lexical
      ;;token record with category QUOTED-TEXT.
      ;;
      (let ((lexer (lex.make-lexer quoted-text-table IS))
	    (text  ""))
	(do ((token (lexer) (lexer)))
	    ((eq? token 'QUOTED-TEXT-CLOSE)
	     (let (((pos <source-location>) opening-token.location))
	       (make* <lexical-token> 'QUOTED-TEXT
		      (make* <source-location>
			pos.input pos.line pos.column pos.offset)
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
    (let loop (((T <lexical-token>)	(lexer))
	       (list-of-tokens		'()))
      (if T.special?
	  (reverse list-of-tokens)
	(loop (lexer) (cons T list-of-tokens))))))


;;;; done

)

;;; end of file
