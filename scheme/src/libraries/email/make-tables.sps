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
  (prefix (silex) silex:))


(silex:lex silex::input-file "address-strings.l"
	   silex::output-file "address-strings-lexer.sls"
	   silex::library-spec '(email address-strings-lexer)
	   silex::table-name 'email-address-strings-table
	   silex::counters 'all)

(silex:lex silex::input-file "address-comments.l"
	   silex::output-file "address-comments-lexer.sls"
	   silex::library-spec '(email address-comments-lexer)
	   silex::table-name 'email-address-comments-table
	   silex::counters 'all)

(silex:lex silex::input-file "address-domain-literals.l"
	   silex::output-file "address-domain-literals-lexer.sls"
	   silex::library-spec '(email address-domain-literals-lexer)
	   silex::table-name 'email-address-domain-literals-table
	   silex::counters 'all)

(silex:lex silex::input-file "address-lexer.l"
	   silex::output-file "address-lexer.sls"
	   silex::library-spec '(email address-lexer)
	   silex::table-name 'email-address-table
	   silex::counters 'all)


;;; end of file
