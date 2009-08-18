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


(silex:lex silex::input-file		"address-quoted-string.l"
	   silex::output-file		"address-quoted-string-lexer.sls"
	   silex::library-spec		'(email address-quoted-string-lexer)
	   silex::library-imports	'((lalr lr-driver))
	   silex::table-name		'email-address-quoted-string-table
	   silex::counters		'all)

(silex:lex silex::input-file		"address-comments.l"
	   silex::output-file		"address-comments-lexer.sls"
	   silex::library-spec		'(email address-comments-lexer)
	   silex::library-imports	'((lalr lr-driver))
	   silex::table-name		'email-address-comments-table
	   silex::counters		'all)

(silex:lex silex::input-file		"address-domain-literals.l"
	   silex::output-file		"address-domain-literals-lexer.sls"
	   silex::library-spec		'(email address-domain-literals-lexer)
	   silex::library-imports	'((lalr lr-driver))
	   silex::table-name		'email-address-domain-literals-table
	   silex::counters		'all)

(silex:lex silex::input-file		"address-lexer.l"
	   silex::output-file		"address-lexer.sls"
	   silex::library-spec		'(email address-lexer)
	   silex::library-imports	'((lalr lr-driver))
	   silex::table-name		'email-address-table
	   silex::counters		'all)


(lalr-parser

 :output-file		"addresses-parser.sls"
 :parser-name		'make-email-address-parser
 :library-spec		'(email addresse-parser)

 :terminals	'(DOT AT COMMA COLON SEMICOLON ATOM
		      ANGLE-OPEN ANGLE-CLOSE
		      DOMAIN
		      DOMAIN-LITERAL-OPEN DOMAIN-LITERAL-CLOSE DOMAIN-LITERAL-INTEGER
		      QUOTED-STRING-OPEN QUOTED-STRING-CLOSE QUOTED-STRING)

 :rules
 '((phrase		(phrase-rest)			: $1)
   (phrase-rest		(ATOM phrase-rest)		: (cons $1 $2)
			()				: '())

   (domain		(domain-ref)			: $1
			(domain-literal)		: $1)


   (domain-ref		(ATOM domain-ref-rest)		: (make-domain (cons $1 $2)))
   (domain-ref-rest	(DOT ATOM domain-ref-rest)	: (cons $2 $3)
			()				: '())

   (domain-literal	(DOMAIN-LITERAL-OPEN
			 DOMAIN-LITERAL-INTEGER DOT
			 DOMAIN-LITERAL-INTEGER DOT
			 DOMAIN-LITERAL-INTEGER DOT
			 DOMAIN-LITERAL-INTEGER
			 DOMAIN-LITERAL-CLOSE)
			: (make-domain-literal $2 $4 $6 $8))

   (local-part		(ATOM local-part-rest)		: (make-local-part (cons $1 $2)))
   (local-part-rest	(DOT ATOM local-part-rest)	: (cons $2 $3)
			()				: '())

   (addr-spec		(local-part AT domain)		: (make-addr-spec $1 $3))

   (route		(route-rest)			: (make-route $1))
   (route-rest		(AT DOMAIN route-rest)		: (cons $2 $3)
			()				: '())

   (route-address	(ANGLE-OPEN route COLON addr-spec ANGLE-CLOSE)
			: (make-route-address $1 $3)
			(ANGLE-OPEN addr-spec ANGLE-CLOSE)
			: (make-route-address #f $1))

   (mailbox		(QUOTED-STRING route-address)	: (make-mailbox $1 $2)
			(route-address)			: (make-mailbox "" $2))

   (group		(phrase COLON mailbox group-rest SEMICOLON)
			: (make-group $1 (cons $3 $4))
			(COLON mailbox group-rest SEMICOLON)
			: (make-group "" (cons $2 $3)))
   (group-rest		(COLON mailbox group-rest)	: (cons $2 $3)
			()				: '())

   (address		(group address-rest)		: (cons $1 $2)
			(mailbox address-rest)		: (cons $1 $2))
   (address-rest	(COMMA group address-rest)	: (cons $2 $3)
			(COMMA mailbox address-rest)	: (cons $2 $3)
			()				: '())

   ))


;;; end of file
