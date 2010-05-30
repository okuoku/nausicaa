;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: rebuild the email address lexer tables
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


(import (rnrs)
  (keywords)
  (silex)
  (lalr))

(define-keywords
  :counters
  :dump-table
  :input-file
  :input-string
  :library-imports
  :library-spec
  :output-file
  :parser-name
  :rules
  :table-name
  :terminals
  )


(lex (:input-file	"quoted-text.l")
     (:output-file	"quoted-text-lexer.sls")
     (:library-spec	'(email addresses quoted-text-lexer))
     (:library-imports	'((lalr lr-driver)
			  (email addresses common)
			  (parser-tools lexical-token)
			  (parser-tools source-location)))
     (:table-name	'quoted-text-table)
     (:counters		'all))

(lex (:input-file	"comments.l")
     (:output-file	"comments-lexer.sls")
     (:library-spec	'(email addresses comments-lexer))
     (:library-imports	'((lalr lr-driver)
			  (email addresses common)
			  (parser-tools lexical-token)
			  (parser-tools source-location)))
     (:table-name	'comments-table)
     (:counters		'all))

(lex (:input-file	"domain-literals.l")
     (:output-file	"domain-literals-lexer.sls")
     (:library-spec	'(email addresses domain-literals-lexer))
     (:library-imports	'((lalr lr-driver)
			  (email addresses common)
			  (parser-tools lexical-token)
			  (parser-tools source-location)))
     (:table-name	'domain-literals-table)
     (:counters		'all))

(lex (:input-file	"lexer.l")
     (:output-file	"lexer.sls")
     (:library-spec	'(email addresses lexer))
     (:library-imports	'((lalr lr-driver)
			  (email addresses common)
			  (parser-tools lexical-token)
			  (parser-tools source-location)))
     (:table-name	'address-table)
     (:counters		'all))


(lalr-parser

 :output-file		"parser.sls"
 :parser-name		'make-address-parser
 :library-spec		'(email addresses parser)
 :library-imports	'((email addresses common) (strings))

 :terminals	'(DOT COMMA COLON SEMICOLON ATOM AT
		      ANGLE-OPEN ANGLE-CLOSE
		      DOMAIN-LITERAL-OPEN DOMAIN-LITERAL-CLOSE DOMAIN-LITERAL-INTEGER
		      QUOTED-TEXT)

 :rules
 '((address		(group address-rest)	: (cons $1 $2)
			(mailbox address-rest)	: (cons $1 $2)
			(address-rest)		: $1)
   (address-rest	(COMMA group address-rest)
			: (cons $2 $3)
			(COMMA mailbox address-rest)
			: (cons $2 $3)
			(COMMA address-rest)	: $2
			()			: '())

;;; --------------------------------------------------------------------

   (group		(display-name COLON SEMICOLON)
			: (make-<group> $1 '())
			(display-name COLON mailbox-list SEMICOLON)
			: (make-<group> $1 $3))

;;; --------------------------------------------------------------------

   (mailbox-list	(mailbox mailbox-list-rest)
			: (cons $1 $2)
			(mailbox-list-rest)	: $1)
   (mailbox-list-rest	(COMMA mailbox mailbox-list-rest)
			: (cons $2 $3)
			(COMMA mailbox-list-rest)
			: $2
			()			: '())

;;; --------------------------------------------------------------------

   (mailbox		(display-name ANGLE-OPEN route addr-spec ANGLE-CLOSE)
			: (make-<mailbox> $1 $3 $4)
			(display-name ANGLE-OPEN addr-spec ANGLE-CLOSE)
			: (make-<mailbox> $1 #f $3)
			(ANGLE-OPEN route addr-spec ANGLE-CLOSE)
			: (make-<mailbox> #f $2 $3)
			(ANGLE-OPEN addr-spec ANGLE-CLOSE)
			: (make-<mailbox> #f #f $2)
			(addr-spec)		: (make-<mailbox> #f #f $1))

;;; --------------------------------------------------------------------

   (route		(AT domain route-rest)	: (make-<route> (cons $2 $3)))
   (route-rest		(COMMA AT domain route-rest)
			: (cons $3 $4)
			(COLON)			: '())

;;; --------------------------------------------------------------------

   (addr-spec		(local-part AT domain)	: (make-<addr-spec> $1 $3))

;;; --------------------------------------------------------------------

   (domain		(domain-ref)		: $1
			(domain-literal)	: $1)

   (domain-ref		(ATOM domain-ref-rest)	: (make-<domain> #f (cons $1 $2)))
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
			: (make-<domain> #t (list $2 $4 $6 $8)))

;;; --------------------------------------------------------------------


   (local-part		(ATOM local-part-rest)	: (make-<local-part> (cons $1 $2)))
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
