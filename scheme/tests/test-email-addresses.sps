;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for the email addresses library
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


(import (nausicaa)
  (email addresses)
  (silex lexer)
  (email address-quoted-string-lexer)
  (email address-comments-lexer)
  (email address-domain-literals-lexer)
  (email address-lexer)
  (checks)
  (lalr common)
  (rnrs mutable-strings))

(check-set-mode! 'report-failed)
(display "*** testing email addresses\n")


(parameterise ((check-test-name 'quoted-string-lexer))

  (define (tokenise-string string)
    ;;This  is just  a  lexer, it  does  not check  for the  terminating
    ;;double-quote.
    (let* ((IS		(lexer-make-IS :string string :counters 'all))
	   (lexer	(lexer-make-lexer email-address-quoted-string-table IS))
	   (out		'()))
      (do ((token (lexer) (lexer)))
	  ((lexical-token?/end-of-input token)
	   (reverse out))
	(set! out (cons token out)))))

;;; --------------------------------------------------------------------

  (check	;empty string
      (tokenise-string "")
    => '())

  (check	;empty string
      (tokenise-string "\"")
    => '(QUOTED-STRING-CLOSE))

  (check	;a string
      (tokenise-string "ciao")
    => '("ciao"))

  (check	;a string
      (tokenise-string "ciao\"")
    => '("ciao" QUOTED-STRING-CLOSE))

  (check	;a string
      (tokenise-string "\\a\"")
    => '("\\a" QUOTED-STRING-CLOSE))

  (check ;a string with utf-8 character
      (tokenise-string "cioé\"")
    => '("cioé" QUOTED-STRING-CLOSE))

  (check	;nested double quotes
      (tokenise-string "ciao \\\"hello\\\" salut\"")
    => '("ciao \\\"hello\\\" salut"  QUOTED-STRING-CLOSE))

  #t)


(parameterise ((check-test-name 'comment-lexer))

  (define (tokenise-comment string)
    ((recursion (lex IS)
       (let ((lexer (lexer-make-lexer email-address-comments-table IS))
	     (text  ""))
	 (do ((token  (lexer) (lexer)))
	     ((eq? token 'COMMENT-CLOSE)
	      text)
	   (set! text (string-append
		       text
		       (if (eq? token 'COMMENT-OPEN)
			   (string-append "(" (lex IS) ")")
			 token))))))
     (lexer-make-IS :string string :counters 'all)))

;;; --------------------------------------------------------------------

  (check
      (tokenise-comment "ciao)")
    => "ciao")

  (check
      (tokenise-comment "ciao (hello) salut)")
    => "ciao (hello) salut")

  (check
      (tokenise-comment "((ciao (hello) salut)))")
    => "((ciao (hello) salut))")

  (check
      (tokenise-comment "((ciao (((((()))))) salut)))")
    => "((ciao (((((()))))) salut))")

  #t)


(parameterise ((check-test-name 'domain-literal-lexer))

  (define (tokenise-domain-literal string)
    (let* ((IS    (lexer-make-IS :string string :counters 'all))
	   (lexer (lexer-make-lexer email-address-domain-literals-table IS)))
      (let loop ((token (lexer))
		 (toks  '()))
	(if (lexical-token?/end-of-input token)
	    (reverse toks)
	  (loop (lexer) (cons token toks))))))

;;; --------------------------------------------------------------------

  (check
      (map lexical-token-category (tokenise-domain-literal "]"))
    => '(DOMAIN-LITERAL-CLOSE))

  (check
      (map lexical-token-category (tokenise-domain-literal "123]"))
    => '(DOMAIN-LITERAL-INTEGER DOMAIN-LITERAL-CLOSE))

  (check
      (map lexical-token-category (tokenise-domain-literal ".123]"))
    => '(DOT DOMAIN-LITERAL-INTEGER DOMAIN-LITERAL-CLOSE))

  (check
      (map lexical-token-category (tokenise-domain-literal "1.2.3.4]"))
    => '(DOMAIN-LITERAL-INTEGER
	 DOT DOMAIN-LITERAL-INTEGER
	 DOT DOMAIN-LITERAL-INTEGER
	 DOT DOMAIN-LITERAL-INTEGER
	 DOMAIN-LITERAL-CLOSE))

  #t)


(parameterise ((check-test-name 'lexer))

  (check
      (address->tokens :string "simons@rhein.de")
    => '((atom . "simons")
	 (character . #\@)
	 (atom . "rhein")
	 (character . #\.)
	 (atom . "de")))

  (check
      (address->tokens :string "<simons@rhein.de>")
    => '((character . #\<)
	 (atom . "simons")
	 (character . #\@)
	 (atom . "rhein")
	 (character . #\.)
	 (atom . "de")
	 (character . #\>)))

  (check
      (address->tokens :string "\"Peter Simons\" <simons@rhein.de>")
    => '((quoted-text . "Peter Simons")
	 (character . #\<)
	 (atom . "simons")
	 (character . #\@)
	 (atom . "rhein")
	 (character . #\.)
	 (atom . "de")
	 (character . #\>)))

  (check
      (address->tokens :string "Peter Simons <simons@rhein.de>")
    => '((atom . "Peter")
	 (atom . "Simons")
	 (character . #\<)
	 (atom . "simons")
	 (character . #\@)
	 (atom . "rhein")
	 (character . #\.)
	 (atom . "de")
	 (character . #\>)))

  (check
      (address->tokens :string "testing my parser : peter.simons@gmd.de,
            (peter.)simons@rhein.de ,,,,,
         testing my parser <simons@ieee.org>,
         it rules <@peti.gmd.de,@listserv.gmd.de:simons @ cys .de>
         ;
         ,
         peter.simons@acm.org")
    => '((atom . "testing")
	 (atom . "my")
	 (atom . "parser")
	 (character . #\:)
	 (atom . "peter")
	 (character . #\.)
	 (atom . "simons")
	 (character . #\@)
	 (atom . "gmd")
	 (character . #\.)
	 (atom . "de")
	 (character . #\,)
	 (comment . "peter.")
	 (atom . "simons")
	 (character . #\@)
	 (atom . "rhein")
	 (character . #\.)
	 (atom . "de")
	 (character . #\,)
	 (character . #\,)
	 (character . #\,)
	 (character . #\,)
	 (character . #\,)
	 (atom . "testing")
	 (atom . "my")
	 (atom . "parser")
	 (character . #\<)
	 (atom . "simons")
	 (character . #\@)
	 (atom . "ieee")
	 (character . #\.)
	 (atom . "org")
	 (character . #\>)
	 (character . #\,)
	 (atom . "it")
	 (atom . "rules")
	 (character . #\<)
	 (character . #\@)
	 (atom . "peti")
	 (character . #\.)
	 (atom . "gmd")
	 (character . #\.)
	 (atom . "de")
	 (character . #\,)
	 (character . #\@)
	 (atom . "listserv")
	 (character . #\.)
	 (atom . "gmd")
	 (character . #\.)
	 (atom . "de")
	 (character . #\:)
	 (atom . "simons")
	 (character . #\@)
	 (atom . "cys")
	 (character . #\.)
	 (atom . "de")
	 (character . #\>)
	 (character . #\;)
	 (character . #\,)
	 (atom . "peter")
	 (character . #\.)
	 (atom . "simons")
	 (character . #\@)
	 (atom . "acm")
	 (character . #\.)
	 (atom . "org")))

  (check
      (address->tokens :string "=?ISO-8859-15?Q?Andr=E9s_Garc=EDa?= <fandom@spamme.telefonica.net>")
    => '((atom . "=?ISO-8859-15?Q?Andr=E9s_Garc=EDa?=")
	 (character . #\<)
	 (atom . "fandom")
	 (character . #\@)
	 (atom . "spamme")
	 (character . #\.)
	 (atom . "telefonica")
	 (character . #\.)
	 (atom . "net")
	 (character . #\>)))

  (check
      (address->tokens :string "=?iso-8859-1?q?Ulrich_Sch=F6bel?= <ulrich@outvert.com>")
    => '((atom . "=?iso-8859-1?q?Ulrich_Sch=F6bel?=")
	 (character . #\<)
	 (atom . "ulrich")
	 (character . #\@)
	 (atom . "outvert")
	 (character . #\.)
	 (atom . "com")
	 (character . #\>)))

  (check
      (address->tokens :string " \"Steve Redler IV, Tcl2006 Conference Program Chair\" <steve@sr-tech.com>")
    => '((quoted-text . "Steve Redler IV, Tcl2006 Conference Program Chair")
	 (character . #\<)
	 (atom . "steve")
	 (character . #\@)
	 (atom . "sr-tech")
	 (character . #\.)
	 (atom . "com")
	 (character . #\>)))

  (check
      (address->tokens :string "\"Peter Simons\" (Peter Simons) <simons@rhein.de>")
    => '((quoted-text . "Peter Simons")
	 (comment . "Peter Simons")
	 (character . #\<)
	 (atom . "simons")
	 (character . #\@)
	 (atom . "rhein")
	 (character . #\.)
	 (atom . "de")
	 (character . #\>)))


  (check
      (address->tokens :string "simons@[rhein].de")
    => '((atom . "simons")
	 (character . #\@)
	 (domain-literal . "rhein")
	 (character . #\.)
	 (atom . "de")))

  ;; ------------------------------------------------------------

  (check
      (guard (exc (else (condition-message exc)))
	(address->tokens :string "simons[@rhein.de"))
    => "found end of input while parsing domain literal")

  #t)


;;;; parser, domain

;; (define (parse-domain string)
;;   (call-with-string-output-port
;;       (lambda (port)
;; 	(domain-display (address-parse-domain (make-address-lexer :string string))
;; 			port))))

;; (check
;;     (parse-domain "alpha")
;;   => "#<domain -- alpha>")

;; (check
;;     (parse-domain "alpha.beta.delta")
;;   => "#<domain -- alpha.beta.delta>")

;; (check
;;     (guard (exc (else (condition-message exc)))
;;       (parse-domain "alpha."))
;;   => "found end of address while parsing address domain")

;; (check
;;     (guard (exc (else (condition-message exc)))
;;       (parse-domain "alpha.beta."))
;;   => "found end of address while parsing address domain")



;;;; parser, local part

;; (define (parse-local-part string)
;;   (call-with-string-output-port
;;       (lambda (port)
;; 	(local-part-display (address-parse-local-part (make-address-lexer :string string))
;; 			    port))))

;; (check
;;     (parse-local-part "alpha")
;;   => "#<local-part -- alpha>")

;; (check
;;     (parse-local-part "alpha.beta.delta")
;;   => "#<local-part -- alpha.beta.delta>")

;; (check
;;     (guard (exc (else (condition-message exc)))
;;       (parse-local-part "alpha."))
;;   => "found end of address while parsing address local part")

;; (check
;;     (guard (exc (else (condition-message exc)))
;;       (parse-local-part "alpha.beta."))
;;   => "found end of address while parsing address local part")



;;;; parser, addr-spec

;; (define (parse-addr-spec string)
;;   (call-with-string-output-port
;;       (lambda (port)
;; 	(addr-spec-display (address-parse-addr-spec (make-address-lexer :string string))
;; 			   port))))

;; (write    (address->tokens :string "marcomaggi@gna.org"))(newline)

;; (check
;;     (parse-addr-spec "marcomaggi@gna.org")
;;   => "#<addr-spec -- marcomaggi@gna.org>")


;;;; done

(check-report)

;;; end of file
