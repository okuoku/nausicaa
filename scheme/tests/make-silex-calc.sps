;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: test program for SILex
;;;Date: Fri Jul 17, 2009
;;;
;;;Abstract
;;;
;;;	Run this program with:
;;;
;;;	   $ YPSILON_SITELIB=./path/to/libs ypsilon make-calc-test.sps
;;;
;;;	it will produce the "calc-test.l.sls" output file.
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
  (silex))

(define lexer-file	"calc.l")
(define tree-file	"calc-tree.sls")
(define code-file	"calc-code.sls")
(define portable-file	"calc-portable.sls")

(lex        lexer-file tree-file
	    'library-spec "(calc-tree)"
	    'table-name 'calc-lexer-table/tree)

(lex        lexer-file code-file 'code
	    'library-spec "(calc-code)"
	    'table-name 'calc-lexer-table/code)

(lex        lexer-file portable-file 'portable
	    'library-spec "(calc-portable)"
	    'table-name 'calc-lexer-table/portable)

;;; end of file
