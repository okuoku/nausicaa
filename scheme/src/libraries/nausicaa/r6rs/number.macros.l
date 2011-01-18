;;; -*- mode: scheme; coding: utf-8; page-delimiter: "^;;page" -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: SILex macros for R6RS number lexer
;;;Date: Tue Jan 18, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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

NANINF			"nan.0"|"inf.0"
IMAG			i
DIGIT2			[01]
DIGIT8			[01234567]
DIGIT10			[0123456789]
DIGIT16			{DIGIT10}|[a-fA-F]
RADIX2			(#b)|(#B)
RADIX8			(#o)|(#O)
RADIX10			((#d)|(#D))?
RADIX16			(#x)|(#X)
EXACTNESS		((#i)|(#I)|(#e)|(#E))?
EXPONENT-MARKER		e|E|s|S|f|F|d|D|l|L
SIGN			(\+|\-)?
SUFFIX			({EXPONENT-MARKER}{SIGN}{DIGIT10}+)?
MANTISSA-WIDTH		(\|{DIGIT10}+)?

;;page

UINTEGER2		{DIGIT2}+
UREAL2			{UINTEGER2}|({UINTEGER2}/{UINTEGER2})
REAL2			({SIGN}{UREAL2})|(\+{NANINF})|(\-{NANINF})

PREFIX2			({RADIX2}{EXACTNESS})|({EXACTNESS}{RADIX2})
COMPLEX2		{REAL2}|{REAL2}@{REAL2}|({REAL2}\+{UREAL2}{IMAG})|({REAL2}\-{UREAL2}{IMAG})|({REAL2}\+{NANINF}{IMAG})|({REAL2}\-{NANINF}{IMAG})|({REAL2}\+{IMAG})|({REAL2}\-{IMAG})|(\+{UREAL2}{IMAG})|(\-{UREAL2}{IMAG})
NUMBER2			{PREFIX2}{COMPLEX2}

;;page

UINTEGER8		{DIGIT8}+
UREAL8			{UINTEGER8}|({UINTEGER8}/{UINTEGER8})
REAL8			({SIGN}{UREAL8})|(\+{NANINF})|(\-{NANINF})

PREFIX8			({RADIX8}{EXACTNESS})|({EXACTNESS}{RADIX8})
COMPLEX8		{REAL8}|{REAL8}@{REAL8}|({REAL8}\+{UREAL8}{IMAG})|({REAL8}\-{UREAL8}{IMAG})|({REAL8}\+{NANINF}{IMAG})|({REAL8}\-{NANINF}{IMAG})|({REAL8}\+{IMAG})|({REAL8}\-{IMAG})|(\+{UREAL8}{IMAG})|(\-{UREAL8}{IMAG})
NUMBER8			{PREFIX8}{COMPLEX8}

;;page

UINTEGER10		{DIGIT10}+
;;This rule is unique for base 10.
DECIMAL10		({UINTEGER10}{SUFFIX})|(\.{DIGIT10}+{SUFFIX})|({DIGIT10}+\.{DIGIT10}*{SUFFIX})|({DIGIT10}+\.{SUFFIX})
;;This rule is unique for base 10 in the last alternative.
UREAL10			({UINTEGER10})|({UINTEGER10}/{UINTEGER10})|({DECIMAL10}{MANTISSA-WIDTH})
REAL10			({SIGN}{UREAL10})|(\+{NANINF})|(\-{NANINF})

PREFIX10		({RADIX10}{EXACTNESS})|({EXACTNESS}{RADIX10})
;;The tail of this rule is unique for base 10.
COMPLEX10		{REAL10}|{REAL10}@{REAL10}|({REAL10}\+{UREAL10}{IMAG})|({REAL10}\-{UREAL10}{IMAG})|({REAL10}\+{NANINF}{IMAG})|({REAL10}\-{NANINF}{IMAG})|({REAL10}\+{IMAG})|({REAL10}\-{IMAG})|(\+{UREAL10}{IMAG})|(\-{UREAL10}{IMAG})|(\+{NANINF}{IMAG})|(\-{NANINF}{IMAG})|(\+{IMAG})|(\-{IMAG})
NUMBER10		{PREFIX10}{COMPLEX10}

;;page

UINTEGER16		{DIGIT16}+
UREAL16			{UINTEGER16}|({UINTEGER16}/{UINTEGER16})
REAL16			({SIGN}{UREAL16})|(\+{NANINF})|(\-{NANINF})

PREFIX16		({RADIX16}{EXACTNESS})|({EXACTNESS}{RADIX16})
COMPLEX16		{REAL16}|{REAL16}@{REAL16}|({REAL16}\+{UREAL16}{IMAG})|({REAL16}\-{UREAL16}{IMAG})|({REAL16}\+{NANINF}{IMAG})|({REAL16}\-{NANINF}{IMAG})|({REAL16}\+{IMAG})|({REAL16}\-{IMAG})|(\+{UREAL16}{IMAG})|(\-{UREAL16}{IMAG})
NUMBER16		{PREFIX16}{COMPLEX16}

NUMBER			{NUMBER2}|{NUMBER8}|{NUMBER10}|{NUMBER16}

;;; end of file
