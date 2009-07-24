;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for lalr
;;;Date: Thu Jul 16, 2009
;;;
;;;Abstract
;;;
;;;	Simple calculator in Scheme
;;;
;;;	  This  program  illustrates  the  use of  the  lalr-scm  parser
;;;	generator for Scheme. It is NOT robust, since calling a function
;;;	with the  wrong number of  arguments may generate an  error that
;;;	will cause the calculator to crash.
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
;;;Copyright (c) 2004 Dominique Boucher
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


#!r6rs
(import (rnrs)
  (lalr))

(define (display-result v)
  (when v
    (display v)
    (newline)))




(define calc-parser
  (lalr-parser
   '( ;; --- Options
     ;; output a parser, called calc-parser, in a separate file - calc.yy.scm,
     (output:    calc-parser "calc-parser.sls")
     ;; output the LALR table to calc.out
     (out-table: "calc-parser-tables.txt")
     ;; there should be no conflict
     (expect:    5)

     ;; --- token definitions
     (ID NUM = LPAREN RPAREN NEWLINE COMMA
	 (left: + -)
	 (left: * /)
	 (nonassoc: uminus))

     (lines    (lines line) : (display-result $2)
	       (line)       : (display-result $1))


     ;; --- rules
     (line     (assign NEWLINE)        : $1
	       (expr   NEWLINE)        : $1
	       (error  NEWLINE)        : #f)

     (assign   (ID = expr)             : (add-binding $1 $3))

     (expr     (expr + expr)           : (+ $1 $3)
	       (expr - expr)           : (- $1 $3)
	       (expr * expr)           : (* $1 $3)
	       (expr / expr)           : (/ $1 $3)
	       (- expr (prec: uminus)) : (- $2)
	       (ID)                    : (get-binding $1)
	       (ID LPAREN args RPAREN) : (invoke-proc $1 $3)
	       (NUM)                   : $1
	       (LPAREN expr RPAREN)    : $2)

     (args     ()                      : '()
	       (expr arg-rest)         : (cons $1 $2))

     (arg-rest (COMMA expr arg-rest)   : (cons $2 $3)
	       ()                      : '()))))

;;; end of file
