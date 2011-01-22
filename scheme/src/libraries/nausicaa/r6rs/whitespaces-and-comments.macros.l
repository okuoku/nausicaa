;;; -*- mode: scheme; coding: utf-8; page-delimiter: "^;;page" -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: macros R6RS whitespace and comment lexers
;;;Date: Tue Jan 18, 2011
;;;
;;;Abstract
;;;
;;;	This file  needs the macro file "unicode.macros.l"  to be loaded
;;;	first.
;;;
;;;Copyright (c) 2010, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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

;;page
;;A <whitespace> character is one among:
;;
;;		\9	char tabulation
;;		\10	linefeed
;;		\11	line tabulation
;;		\12	form feed
;;		\13	carriage return
;;		\133	next line
;;
;;and any char whose Unicode category is Zs, Zl or Zp.
;;

WHITESPACE-CHARS	[\9\10\11\12\13\133]|{UNICODE-Zs}|{UNICODE-Zl}|{UNICODE-Zp}

WHITESPACE		{WHITESPACE-CHARS}+

;;<line ending> is:
;;
;;		\10		line feed
;;		\13		carriage return
;;		\133		next line
;;		\10\13		carriage return, line feed
;;		\13\133		carriage return, next line
;;		\8232		line separator
;;
LINEENDING		[\10\13\133\8232]|(\13\10)|(\13\133)

;;When a valid  R6RS lexeme is followed by this tail  it is wrong.  This
;;is the negation of the <delimiter> element.
TAIL_OF_WRONG		[^\9\10\11\12\13\32\160\5760\6158\8192\8193\8194\8195\8196\8197\8198\8199\8200\8201\8202\8239\8287\12288\8232\8233\(\)\[\]\"\;#]+

;;page
;;A simple comment is everything between a semicolon and a LINEENDING or
;;a paragraph separator (#x2029 = \8233).
;;
LINECOMMENT		";".*({LINEENDING}|\8233)
LINECOMMENT-NOEND	";"[^\10\13\133\8232\8233]*

;;Multiline comments can contain nested multiline comments, so we need a
;;sublexer.
;;
ONESTEDCOMMENT		"#|"

;;; end of file
