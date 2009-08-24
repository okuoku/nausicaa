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
  (checks)
  (email addresses)
  (email addresses quoted-text-lexer)
  (email addresses comments-lexer)
  (email addresses domain-literals-lexer)
  (email addresses common)
  (email addresses lexer)
  (email addresses parser)
  (silex lexer)
  (lalr common)
  (rnrs mutable-strings))

(check-set-mode! 'report-failed)
(display "*** testing email addresses\n")


(parameterise ((check-test-name		'common))

  (check
      (unquote-string "")
    => "")

  (check
      (unquote-string "\\")
    => "\\")

  (check
      (unquote-string "\\\\")
    => "\\")

  (check
      (unquote-string "\\\\\\")
    => "\\\\")

  (check
      (unquote-string "ciao")
    => "ciao")

  (check
      (unquote-string "ci\\ao")
    => "ciao")

  (check
      (unquote-string "\\c\\i\\a\\o")
    => "ciao")

  (check
      (unquote-string "ciao\\")
    => "ciao\\")

  #t)


(parameterise ((check-test-name 'quoted-text-lexer))

  (define (tokenise-string string)
    ;;This  is just  a  lexer, it  does  not check  for the  terminating
    ;;double-quote.
    (let* ((IS		(lexer-make-IS :string string :counters 'all))
	   (lexer	(lexer-make-lexer quoted-text-table IS))
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
    => '(QUOTED-TEXT-CLOSE))

  (check	;a string
      (tokenise-string "ciao")
    => '("ciao"))

  (check	;a string
      (tokenise-string "ciao\"")
    => '("ciao" QUOTED-TEXT-CLOSE))

  (check
      ;;A string with a quoted character in it.  The quoting is removed.
      (tokenise-string "\\a\"")
    => '("a" QUOTED-TEXT-CLOSE))

  (check ;a string with utf-8 character
      (tokenise-string "cioé\"")
    => '("cioé" QUOTED-TEXT-CLOSE))

  (check
      ;;Nested double quotes.  The Scheme string "\\\"" is seen as \" by
      ;;the lexer and the backslash quoting character is removed.
      (tokenise-string "ciao \\\"hello\\\" salut\"")
    => '("ciao \"hello\" salut"  QUOTED-TEXT-CLOSE))

  #t)


(parameterise ((check-test-name 'comment-lexer))

  (define (tokenise-comment string)
    ((recursion (lex IS)
       (let ((lexer (lexer-make-lexer comments-table IS))
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
	   (lexer (lexer-make-lexer domain-literals-table IS)))
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

  (define (doit string)
    (map (lambda (token)
	   (cons (lexical-token-category token)
		 (lexical-token-value    token)))
      (address->tokens (lexer-make-IS :string string :counters 'all))))

  (check	;a folding white space
      (doit "\r\n ")
    => '())

  (check
    (parameterise ((debugging #f))
      (doit "simons@rhein.de"))
    => '((ATOM . "simons")
	 (AT . #\@)
	 (ATOM . "rhein")
	 (DOT . #\.)
	 (ATOM . "de")))

  (check
      (doit "<simons@rhein.de>")
    => '((ANGLE-OPEN . #\<)
	 (ATOM . "simons")
	 (AT . #\@)
	 (ATOM . "rhein")
	 (DOT . #\.)
	 (ATOM . "de")
	 (ANGLE-CLOSE . #\>)))

  (check
      (doit "\"Peter Simons\" <simons@rhein.de>")
    => '((QUOTED-TEXT . "Peter Simons")
	 (ANGLE-OPEN . #\<)
	 (ATOM . "simons")
	 (AT . #\@)
	 (ATOM . "rhein")
	 (DOT . #\.)
	 (ATOM . "de")
	 (ANGLE-CLOSE . #\>)))

  (check
      (doit "Peter Simons <simons@rhein.de>")
    => '((ATOM . "Peter")
	 (ATOM . "Simons")
	 (ANGLE-OPEN . #\<)
	 (ATOM . "simons")
	 (AT . #\@)
	 (ATOM . "rhein")
	 (DOT . #\.)
	 (ATOM . "de")
	 (ANGLE-CLOSE . #\>)))

  (check
      (parameterise ((debugging #f))
	(doit "Peter Simons\r\n <simons@rhein.de>"))
    => '((ATOM . "Peter")
	 (ATOM . "Simons")
	 (ANGLE-OPEN . #\<)
	 (ATOM . "simons")
	 (AT . #\@)
	 (ATOM . "rhein")
	 (DOT . #\.)
	 (ATOM . "de")
	 (ANGLE-CLOSE . #\>)))

  (check
      (parameterise ((address-lexer-allows-comments #t))
	(doit "Peter Simons <sim(him)ons@rhein.de>"))
    => '((ATOM . "Peter")
	 (ATOM . "Simons")
	 (ANGLE-OPEN . #\<)
	 (ATOM . "sim")
	 (COMMENT . "him")
	 (ATOM . "ons")
	 (AT . #\@)
	 (ATOM . "rhein")
	 (DOT . #\.)
	 (ATOM . "de")
	 (ANGLE-CLOSE . #\>)))

  (check
      (parameterise ((address-lexer-allows-comments #t))
	(doit "testing my parser : peter.simons@gmd.de,\r
            (peter.)simons@rhein.de ,,,,,\r
         testing my parser <simons@ieee.org>,\r
         it rules <@peti.gmd.de,@listserv.gmd.de:simons @ cys .de>\r
         ;\r
         ,\r
         peter.simons@acm.org"))
    => '((ATOM . "testing")
	 (ATOM . "my")
	 (ATOM . "parser")
	 (COLON . #\:)
	 (ATOM . "peter")
	 (DOT . #\.)
	 (ATOM . "simons")
	 (AT . #\@)
	 (ATOM . "gmd")
	 (DOT . #\.)
	 (ATOM . "de")
	 (COMMA . #\,)
	 (COMMENT . "peter.")
	 (ATOM . "simons")
	 (AT . #\@)
	 (ATOM . "rhein")
	 (DOT . #\.)
	 (ATOM . "de")
	 (COMMA . #\,)
	 (COMMA . #\,)
	 (COMMA . #\,)
	 (COMMA . #\,)
	 (COMMA . #\,)
	 (ATOM . "testing")
	 (ATOM . "my")
	 (ATOM . "parser")
	 (ANGLE-OPEN . #\<)
	 (ATOM . "simons")
	 (AT . #\@)
	 (ATOM . "ieee")
	 (DOT . #\.)
	 (ATOM . "org")
	 (ANGLE-CLOSE . #\>)
	 (COMMA . #\,)
	 (ATOM . "it")
	 (ATOM . "rules")
	 (ANGLE-OPEN . #\<)
	 (AT . #\@)
	 (ATOM . "peti")
	 (DOT . #\.)
	 (ATOM . "gmd")
	 (DOT . #\.)
	 (ATOM . "de")
	 (COMMA . #\,)
	 (AT . #\@)
	 (ATOM . "listserv")
	 (DOT . #\.)
	 (ATOM . "gmd")
	 (DOT . #\.)
	 (ATOM . "de")
	 (COLON . #\:)
	 (ATOM . "simons")
	 (AT . #\@)
	 (ATOM . "cys")
	 (DOT . #\.)
	 (ATOM . "de")
	 (ANGLE-CLOSE . #\>)
	 (SEMICOLON . #\;)
	 (COMMA . #\,)
	 (ATOM . "peter")
	 (DOT . #\.)
	 (ATOM . "simons")
	 (AT . #\@)
	 (ATOM . "acm")
	 (DOT . #\.)
	 (ATOM . "org")))

  (check
      (doit "=?ISO-8859-15?Q?Andr=E9s_Garc=EDa?= <fandom@spamme.telefonica.net>")
    => '((ATOM . "=?ISO-8859-15?Q?Andr=E9s_Garc=EDa?=")
	 (ANGLE-OPEN . #\<)
	 (ATOM . "fandom")
	 (AT . #\@)
	 (ATOM . "spamme")
	 (DOT . #\.)
	 (ATOM . "telefonica")
	 (DOT . #\.)
	 (ATOM . "net")
	 (ANGLE-CLOSE . #\>)))

  (check
      (doit "=?iso-8859-1?q?Ulrich_Sch=F6bel?= <ulrich@outvert.com>")
    => '((ATOM . "=?iso-8859-1?q?Ulrich_Sch=F6bel?=")
	 (ANGLE-OPEN . #\<)
	 (ATOM . "ulrich")
	 (AT . #\@)
	 (ATOM . "outvert")
	 (DOT . #\.)
	 (ATOM . "com")
	 (ANGLE-CLOSE . #\>)))

  (check
      (doit " \"Steve Redler IV, Tcl2006 Conference Program Chair\" <steve@sr-tech.com>")
    => '((QUOTED-TEXT . "Steve Redler IV, Tcl2006 Conference Program Chair")
	 (ANGLE-OPEN . #\<)
	 (ATOM . "steve")
	 (AT . #\@)
	 (ATOM . "sr-tech")
	 (DOT . #\.)
	 (ATOM . "com")
	 (ANGLE-CLOSE . #\>)))

  (check
      (parameterise ((address-lexer-allows-comments #t))
	(doit "\"Peter Simons\" (Peter Simons) <simons@rhein.de>"))
    => '((QUOTED-TEXT . "Peter Simons")
	 (COMMENT . "Peter Simons")
	 (ANGLE-OPEN . #\<)
	 (ATOM . "simons")
	 (AT . #\@)
	 (ATOM . "rhein")
	 (DOT . #\.)
	 (ATOM . "de")
	 (ANGLE-CLOSE . #\>)))

  (check
      (doit "simons@[1.2.3.4]")
    => '((ATOM . "simons")
	 (AT . #\@)
	 (DOMAIN-LITERAL-OPEN . "[")
	 (DOMAIN-LITERAL-INTEGER . "1")
	 (DOT . ".")
	 (DOMAIN-LITERAL-INTEGER . "2")
	 (DOT . ".")
	 (DOMAIN-LITERAL-INTEGER . "3")
	 (DOT . ".")
	 (DOMAIN-LITERAL-INTEGER . "4")
	 (DOMAIN-LITERAL-CLOSE . "]")))

  #t)


;;;; domain data type

(check
    (domain? (make-domain #f '("alpha" "beta")))
  => #t)

(check
    (domain->string (make-domain #f '("alpha" "beta" "gamma")))
  => "alpha.beta.gamma")

(check
    (let-values (((port getter) (open-string-output-port)))
      (domain-display (make-domain #f '("alpha" "beta" "gamma")) port)
      (getter))
  => "#<domain -- alpha.beta.gamma>")

(check
    (let-values (((port getter) (open-string-output-port)))
      (domain-write (make-domain #f '("alpha" "beta" "gamma")) port)
      (getter))
  => "(make-domain #f (quote (\"alpha\" \"beta\" \"gamma\")))")

;;; --------------------------------------------------------------------

(check
    (domain? (make-domain #t '("1" "2" "3" "4")))
  => #t)

(check
    (domain->string (make-domain #t '("1" "2" "3" "4")))
  => "[1.2.3.4]")

(check
    (let-values (((port getter) (open-string-output-port)))
      (domain-display (make-domain #t '("1" "2" "3" "4")) port)
      (getter))
  => "#<domain -- [1.2.3.4]>")

(check
    (let-values (((port getter) (open-string-output-port)))
      (domain-write (make-domain #t '("1" "2" "3" "4")) port)
      (getter))
  => "(make-domain #t (quote (\"1\" \"2\" \"3\" \"4\")))")


;;;; local-part data type

(check
    (local-part? (make-local-part '("alpha" "beta")))
  => #t)

(check
    (local-part->string (make-local-part '("alpha" "beta" "gamma")))
  => "alpha.beta.gamma")

(check
    (let-values (((port getter) (open-string-output-port)))
      (local-part-display (make-local-part '("alpha" "beta" "gamma")) port)
      (getter))
  => "#<local-part -- alpha.beta.gamma>")

(check
    (let-values (((port getter) (open-string-output-port)))
      (local-part-write (make-local-part '("alpha" "beta" "gamma")) port)
      (getter))
  => "(make-local-part (quote (\"alpha\" \"beta\" \"gamma\")))")


;;;; addr-spec data type

(check
    (addr-spec? (make-addr-spec (make-local-part '("alpha" "beta"))
				(make-domain #f '("delta" "org"))))
  => #t)

(check
    (addr-spec->string (make-addr-spec (make-local-part '("alpha" "beta"))
				       (make-domain #f '("delta" "org"))))
  => "alpha.beta@delta.org")

(check
    (let-values (((port getter) (open-string-output-port)))
      (addr-spec-display (make-addr-spec (make-local-part '("alpha" "beta"))
					 (make-domain #f '("delta" "org"))) port)
      (getter))
  => "#<addr-spec -- alpha.beta@delta.org>")

(check
    (let-values (((port getter) (open-string-output-port)))
      (addr-spec-write (make-addr-spec (make-local-part '("alpha" "beta"))
				       (make-domain #f '("delta" "org"))) port)
      (getter))
  => "(make-addr-spec (make-local-part (quote (\"alpha\" \"beta\"))) (make-domain #f (quote (\"delta\" \"org\"))))")


;;;; route data type

(check
    (route? (make-route (list (make-domain #f '("alpha" "org"))
			      (make-domain #t '("1" "2" "3" "4"))
			      (make-domain #f '("beta" "com")))))
  => #t)

(check
    (route->string (make-route (list (make-domain #f '("alpha" "org"))
				     (make-domain #t '("1" "2" "3" "4"))
				     (make-domain #f '("beta" "com")))))
  => "alpha.org,[1.2.3.4],beta.com")

(check
    (let-values (((port getter) (open-string-output-port)))
      (route-display (make-route (list (make-domain #f '("alpha" "org"))
				       (make-domain #t '("1" "2" "3" "4"))
				       (make-domain #f '("beta" "com"))))
		     port)
      (getter))
  => "#<route -- alpha.org,[1.2.3.4],beta.com>")

(check
    (let-values (((port getter) (open-string-output-port)))
      (route-write (make-route (list (make-domain #f '("alpha" "org"))
				     (make-domain #t '("1" "2" "3" "4"))
				     (make-domain #f '("beta" "com"))))
		   port)
      (getter))
  => "(make-route (list (make-domain #f (quote (\"alpha\" \"org\"))) (make-domain #t (quote (\"1\" \"2\" \"3\" \"4\"))) (make-domain #f (quote (\"beta\" \"com\"))) ))")


;;;; mailbox data type

(let ((the-route	(make-route (list (make-domain #f '("alpha" "org"))
					  (make-domain #t '("1" "2" "3" "4"))
					  (make-domain #f '("beta" "com")))))
      (the-phrase	"the phrase")
      (the-addr-spec	(make-addr-spec (make-local-part '("alpha" "beta"))
					(make-domain #f '("delta" "org")))))

  (check
      (mailbox? (make-mailbox the-phrase the-route the-addr-spec))
    => #t)

  (check
      (mailbox->string (make-mailbox the-phrase the-route the-addr-spec))
    => "\"the phrase\" <alpha.org,[1.2.3.4],beta.com:alpha.beta@delta.org>")

  (check
      (let-values (((port getter) (open-string-output-port)))
	(mailbox-display (make-mailbox the-phrase the-route the-addr-spec) port)
	(getter))
    => "#<mailbox -- \"the phrase\" <alpha.org,[1.2.3.4],beta.com:alpha.beta@delta.org>>")

  (check
      (let-values (((port getter) (open-string-output-port)))
	(mailbox-write (make-mailbox the-phrase the-route the-addr-spec) port)
	(getter))
    => "(make-mailbox \"the phrase\" (make-route (list (make-domain #f (quote (\"alpha\" \"org\"))) (make-domain #t (quote (\"1\" \"2\" \"3\" \"4\"))) (make-domain #f (quote (\"beta\" \"com\"))) )) (make-addr-spec (make-local-part (quote (\"alpha\" \"beta\"))) (make-domain #f (quote (\"delta\" \"org\")))))")

  #t)


(parameterise ((check-test-name 'parser))

  (define (doit string)
    (let* ((IS		(lexer-make-IS :string string :counters 'all))
	   (lexer	(make-address-lexer IS))
	   (parser	(make-address-parser))
	   (handler	(lambda (msg tok) (list 'error-handler msg tok)))
	   (result	(parser lexer handler)))
      (let ((first (car result)))
	(cond ((mailbox? first)	(mailbox->string first))
	      ((group?   first)	(group->string first))
	      (else		result)))))

  (check
      (doit "simons@rhein.de")
    => "<simons@rhein.de>")

  (check
      (doit "<simons@rhein.de>")
    => "<simons@rhein.de>")

  (parameterise ((debugging #f))
    (check
	(doit "Peter Simons <simons@rhein.de>")
      => "\"Peter Simons\" <simons@rhein.de>"))

  (parameterise ((debugging #f))
    (check
	(doit "\"Peter Simons\" <simons@rhein.de>")
      => "\"Peter Simons\" <simons@rhein.de>"))

  (check
      (doit "Peter Simons\r\n <simons@rhein.de>")
    => "\"Peter Simons\" <simons@rhein.de>")

  (check
      (doit "testing my parser : peter.simons@gmd.de,\r
            (peter.)simons@rhein.de ,,,,,\r
         testing my parser <simons@ieee.org>,\r
         it rules <@peti.gmd.de,@listserv.gmd.de:simons @ cys .de>\r
         ;\r
         ,\r
         peter.simons@acm.org")
    => "\"testing my parser\" : <peter.simons@gmd.de>, <simons@rhein.de>, \"testing my parser\" <simons@ieee.org>, \"it rules\" <@peti.gmd.de,@listserv.gmd.de:simons@cys.de>;, <peter.simons@acm.org>")

  (check
      (doit "=?ISO-8859-15?Q?Andr=E9s_Garc=EDa?= <fandom@spamme.telefonica.net>")
    => "\"=?ISO-8859-15?Q?Andr=E9s_Garc=EDa?=\" <fandom@spamme.telefonica.net>")

  (check
      (doit "=?iso-8859-1?q?Ulrich_Sch=F6bel?= <ulrich@outvert.com>")
    => "\"=?iso-8859-1?q?Ulrich_Sch=F6bel?=\" <ulrich@outvert.com>")

  (check
      (doit " \"Steve Redler IV, Tcl2006 Conference Program Chair\" <steve@sr-tech.com>")
    => "\"Steve Redler IV, Tcl2006 Conference Program Chair\" <steve@sr-tech.com>")

  (check
      (doit "\"Peter Simons\" (Peter Simons) <simons@rhein.de>")
    => "\"Peter Simons\" <simons@rhein.de>")

  (check
      (doit "simons@[1.2.3.4]")
    => "<simons@[1.2.3.4]>")

  #t)


;;;; done

(check-report)

;;; end of file
