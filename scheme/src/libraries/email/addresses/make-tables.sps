;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: rebuild the email address lexer tables
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

(import (rnrs)
  (prefix (silex) silex:)
  (lalr))


(silex:lex silex::input-file		"quoted-text.l"
	   silex::output-file		"quoted-text-lexer.sls"
	   silex::library-spec		'(email addresses quoted-text-lexer)
	   silex::library-imports	'((lalr lr-driver)
					  (email addresses common))
	   silex::table-name		'quoted-text-table
	   silex::counters		'all)

(silex:lex silex::input-file		"comments.l"
	   silex::output-file		"comments-lexer.sls"
	   silex::library-spec		'(email addresses comments-lexer)
	   silex::library-imports	'((lalr lr-driver)
					  (email addresses common))
	   silex::table-name		'comments-table
	   silex::counters		'all)

(silex:lex silex::input-file		"domain-literals.l"
	   silex::output-file		"domain-literals-lexer.sls"
	   silex::library-spec		'(email addresses domain-literals-lexer)
	   silex::library-imports	'((lalr lr-driver)
					  (email addresses common))
	   silex::table-name		'domain-literals-table
	   silex::counters		'all)

(silex:lex silex::input-file		"lexer.l"
	   silex::output-file		"lexer.sls"
	   silex::library-spec		'(email addresses lexer)
	   silex::library-imports	'((lalr lr-driver)
					  (email addresses common))
	   silex::table-name		'address-table
	   silex::counters		'all)


(lalr-parser

 :output-file		"parser.sls"
 :parser-name		'make-address-parser
 :library-spec		'(email addresses parser)
 :library-imports	'((email addresses common) (strings))

 :terminals	'(DOT AT COMMA COLON SEMICOLON ATOM
		      ANGLE-OPEN ANGLE-CLOSE
		      DOMAIN
		      DOMAIN-LITERAL-OPEN DOMAIN-LITERAL-CLOSE DOMAIN-LITERAL-INTEGER
		      QUOTED-TEXT)

 :rules
 '((address		(group address-rest)	: (cons $1 $2)
			(mailbox address-rest)	: (cons $1 $2))
   (address-rest	(COMMA group address-rest)
						: (cons $2 $3)
			(COMMA mailbox address-rest)
						: (cons $2 $3)
			()			: '())

;;; --------------------------------------------------------------------

   (group		(display-name COLON SEMICOLON)
						: (make-group $1 '())
			(display-name COLON mailbox-list SEMICOLON)
						: (make-group $1 $2))

;;; --------------------------------------------------------------------

   (mailbox-list	(mailbox mailbox-list-rest)
						: (cons $1 $2))
   (mailbox-list-rest	(COMMA mailbox mailbox-list-rest)
						: (cons $1 $2)
			()			: '())

;;; --------------------------------------------------------------------

   (mailbox		(addr-spec)		: (make-mailbox #f #f $1)
			(ANGLE-OPEN addr-spec ANGLE-CLOSE)
						: (make-mailbox #f #f $2)
			(ANGLE-OPEN route COLON addr-spec ANGLE-CLOSE)
						: (make-mailbox #f $2 $4)
			(display-name ANGLE-OPEN addr-spec ANGLE-CLOSE)
						: (make-mailbox $1 #f $3)
			(display-name ANGLE-OPEN route COLON addr-spec ANGLE-CLOSE)
						: (make-mailbox $1 $3 $5))

;;; --------------------------------------------------------------------

   (route		(AT DOMAIN route-rest)	: (make-route (cons $2 $3)))
   (route-rest		(COMMA AT DOMAIN route-rest)
						: (cons $3 $4)
			()			: '())

;;; --------------------------------------------------------------------

   (addr-spec		(local-part AT domain)	: (make-addr-spec $1 $3))

;;; --------------------------------------------------------------------

   (domain		(domain-ref)		: $1
			(domain-literal)	: $1)

   (domain-ref		(ATOM domain-ref-rest)	: (make-domain #f (cons $1 $2)))
   (domain-ref-rest	(DOT ATOM domain-ref-rest)
						: (cons $2 $3)
			()			: '())

;;; --------------------------------------------------------------------

   (domain-literal	(DOMAIN-LITERAL-OPEN
			 DOMAIN-LITERAL-INTEGER DOT
			 DOMAIN-LITERAL-INTEGER DOT
			 DOMAIN-LITERAL-INTEGER DOT
			 DOMAIN-LITERAL-INTEGER
			 DOMAIN-LITERAL-CLOSE)
						: (make-domain #t (list $2 $4 $6 $8)))

;;; --------------------------------------------------------------------


   (local-part		(ATOM local-part-rest)	: (make-local-part (cons $1 $2)))
   (local-part-rest	(DOT ATOM local-part-rest)
						: (cons $2 $3)
			()			: '())

;;; --------------------------------------------------------------------

   (display-name	(phrase)		: (string-join $1 " ")
			(QUOTED-TEXT)		: $1)
   (phrase		(phrase-rest)		: $1)
   (phrase-rest		(ATOM phrase-rest)	: (cons $1 $2)
			()			: '())

   ))

;;; end of file
