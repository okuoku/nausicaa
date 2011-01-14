;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: rebuild internal tables for SILex
;;;Date: Fri Jan 14, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  wilnl be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(import (nausicaa)
  (prefix (nausicaa silex) lex.))

(lex.lex (lex.input-file:	"class.l")
	 (lex.output-file:	"class.l.sls")
	 (lex.counters:		'all)
	 (lex.table-name:	'class-tables)
	 (lex.lexer-format:	'code))

(lex.lex (lex.input-file:	"regexp.l")
	 (lex.output-file:	"regexp.l.sls")
	 (lex.counters:		'all)
	 (lex.table-name:	'regexp-tables)
	 (lex.lexer-format:	'code))

;;; end of file
