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
  (silex lexer)
  (email address-strings-lexer)
  (email address-comments-lexer)
  (email address-domain-literals-lexer)
  (email address-lexer)
  (checks)
  (rnrs mutable-strings))

(check-set-mode! 'report-failed)
(display "*** testing email addresses\n")


;;;; quoted text lexer

(define (tokenise-string string)
  (let* ((IS	(lexer-make-IS :string string :counters 'all))
	 (lexer	(lexer-make-lexer email-address-strings-table IS)))
    (do ((token (lexer) (lexer))
	 (out   '()))
	((not token)
	 (reverse out))
      (set! out (cons token out)))))

;;; --------------------------------------------------------------------

(check ;empty string
    (guard (exc (else (condition-message exc)))
      (tokenise-string ""))
  => "found end of input while parsing quoted text")

(check ;empty string
    (tokenise-string "\"")
  => '())

(check ;a string
    (tokenise-string "ciao\"")
  => '("ciao"))

(check ;a string with utf-8 character
    (tokenise-string "cioé\"")
  => '("cioé"))

(check ;nested double quotes
    (tokenise-string "ciao \\\"hello\\\" salut\"")
  => '("ciao \\\"hello\\\" salut"))

(check ;a string
    (guard (exc (else (condition-message exc)))
      (tokenise-string "ciao"))
  => "found end of input while parsing quoted text")


;;;; comments lexer

(define (tokenise-comment string)
  (let ((IS (lexer-make-IS :string string :counters 'all)))
    (define (accumulate)
      (let ((lexer (lexer-make-lexer email-address-comments-table IS)))
  	(do ((token (lexer) (lexer))
	     (result ""))
	    ((not token)
	     result)
	  (set! result (string-append result
				      (if (eq? token 'comment)
					  (string-append "(" (accumulate) ")")
					token))))))
    (cons 'comment (accumulate))))

;;; --------------------------------------------------------------------

(check
    (tokenise-comment "ciao)")
  => '(comment . "ciao"))

(check
    (tokenise-comment "ciao (hello) salut)")
  => '(comment . "ciao (hello) salut"))

(check
    (tokenise-comment "((ciao (hello) salut)))")
  => '(comment . "((ciao (hello) salut))"))

(check
    (tokenise-comment "((ciao (((((()))))) salut)))")
  => '(comment . "((ciao (((((()))))) salut))"))


;;;; domain literals lexer

(define (tokenise-domain-literal string)
  (let ((IS (lexer-make-IS :string string :counters 'all)))
    (let ((lexer (lexer-make-lexer email-address-domain-literals-table IS)))
      (do ((token (lexer) (lexer))
	   (dtext  ""))
	  ((not token)
	   (cons 'domain-literal dtext))
	(set! dtext (string-append dtext token))))))

;;; --------------------------------------------------------------------

(check
    (tokenise-domain-literal "]")
  => '(domain-literal . ""))

(check
    (tokenise-domain-literal "ciao]")
  => '(domain-literal . "ciao"))

(check
    (tokenise-domain-literal "cia\\]o]")
  => '(domain-literal . "cia]o"))

(check
    (tokenise-domain-literal "cia\\[o]")
  => '(domain-literal . "cia[o"))

(check
    (tokenise-domain-literal "\\[ciao\\]]")
  => '(domain-literal . "[ciao]"))


;;;; full address lexer

(define (tokenise-address string)
  (let* ((IS	(lexer-make-IS :string string :counters 'all))
	 (lexer	(lexer-make-lexer email-address-table IS)))
    (define (accumulate-comment)
      (let ((lexer (lexer-make-lexer email-address-comments-table IS)))
	(do ((token  (lexer) (lexer))
	     (result ""))
	    ((not token)
	     result)
	  (set! result (string-append result
				      (if (eq? token 'comment)
					  (string-append "(" (accumulate-comment) ")")
					token))))))
    (do ((token (lexer) (lexer))
	 (res   '()))
	((not token)
	 (reverse res))
;;;      (write (list 'token token))(newline)
      (set! res (case token

		  ((quoted-text)
		   (let ((lexer (lexer-make-lexer email-address-strings-table IS)))
		     (do ((token (lexer) (lexer))
			  (text  ""))
			 ((not token)
			  (cons (cons 'quoted-text text) res))
		       (set! text (string-append text token)))))

		  ((comment)
		   (cons (cons 'comment (accumulate-comment)) res))

		  ((domain-literal)
		   (let ((lexer (lexer-make-lexer email-address-domain-literals-table IS)))
		     (do ((token (lexer) (lexer))
			  (dtext  ""))
			 ((not token)
			  (cons (cons 'domain-literal dtext) res))
		       (set! dtext (string-append dtext token)))))

		  (else
		   (cons token res)))))))

;;; --------------------------------------------------------------------

(check
    (tokenise-address "simons@rhein.de")
  => '((atom . "simons")
       (character . #\@)
       (atom . "rhein")
       (character . #\.)
       (atom . "de")))

(check
    (tokenise-address "<simons@rhein.de>")
  => '((character . #\<)
       (atom . "simons")
       (character . #\@)
       (atom . "rhein")
       (character . #\.)
       (atom . "de")
       (character . #\>)))

(check
    (tokenise-address "\"Peter Simons\" <simons@rhein.de>")
  => '((quoted-text . "Peter Simons")
       (character . #\<)
       (atom . "simons")
       (character . #\@)
       (atom . "rhein")
       (character . #\.)
       (atom . "de")
       (character . #\>)))

(check
    (tokenise-address "Peter Simons <simons@rhein.de>")
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
    (tokenise-address "testing my parser : peter.simons@gmd.de,
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
    (tokenise-address "=?ISO-8859-15?Q?Andr=E9s_Garc=EDa?= <fandom@spamme.telefonica.net>")
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
    (tokenise-address "=?iso-8859-1?q?Ulrich_Sch=F6bel?= <ulrich@outvert.com>")
  => '((atom . "=?iso-8859-1?q?Ulrich_Sch=F6bel?=")
       (character . #\<)
       (atom . "ulrich")
       (character . #\@)
       (atom . "outvert")
       (character . #\.)
       (atom . "com")
       (character . #\>)))

(check
    (tokenise-address " \"Steve Redler IV, Tcl2006 Conference Program Chair\" <steve@sr-tech.com>")
  => '((quoted-text . "Steve Redler IV, Tcl2006 Conference Program Chair")
       (character . #\<)
       (atom . "steve")
       (character . #\@)
       (atom . "sr-tech")
       (character . #\.)
       (atom . "com")
       (character . #\>)))

(check
    (tokenise-address "\"Peter Simons\" (Peter Simons) <simons@rhein.de>")
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
    (tokenise-address "simons@[rhein].de")
  => '((atom . "simons")
       (character . #\@)
       (domain-literal . "rhein")
       (character . #\.)
       (atom . "de")))

;; ------------------------------------------------------------

(check
    (guard (exc (else (condition-message exc)))
      (tokenise-address "simons[@rhein.de"))
  => "found end of input while parsing domain literal")


;;;; done

(check-report)

;;; end of file
