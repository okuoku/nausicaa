;;;
;;;Part of: Nausicaa/Silex
;;;Contents: tests for silex
;;;Date: Thu Jul 16, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
;;;
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
  (silex)
  (rnrs eval))

(check-set-mode! 'report-failed)
(display "*** testing silex\n")



(define in-file "arithmetics.l")
(define ou-file "arithmetics.l.sls")

(define parser "

  (define (parse-eof)
    #f)

  (define (parse-comma)
    cons)

  (lexer-init 'string \"1+2843+323*4113/545\")

  (do ((token (lexer) (lexer)))
      ((not token))
    (display token)
    (newline))
")

(define arithmetics "
decint          [0-9]+
binint          #[bB][01]+
octint          #[oO][0-7]+
hexint          #[xX][0-9A-Fa-f]+
integer		{decint}|{binint}|{octint}|{hexint}

exponent        ([eE][+\\-]?[0-9]+)
truereal	[0-9]+\\.|[0-9]*\\.[0-9]+{exponent}?|[0-9]+{exponent}
real		{truereal}|{integer}

imag		({decint}|{real})i

nan             \\-nan\\.0|\\+nan\\.0
inf             \\-inf\\.0|\\+int\\.0

initial         [a-zA-Z!$&:<=>?_~]
subsequent      {initial}|[0-9.@]
symbol          {initial}{subsequent}*

operator	[+\\-*/%\\^\\\\]

comma		,

%%
[ \\n\\t]+	;; skip blanks, tabs and newlines
{imag}		(string->number (string-append \"+\" yytext))
{real}		(string->number yytext)
{nan}		(string->number yytext)
{inf}		(string->number yytext)
{operator}	(case (string-ref yytext 0)
		  ((#\\+) +)
		  ((#\\-) -)
		  ((#\\*) *)
		  ((#\\/) /)
		  ((#\\%) mod)
		  ((#\\^) expt))
{symbol}	(string->symbol yytext)
{comma}		(parse-comma)

<<EOF>>		(parse-eof)
")

(when (file-exists? in-file)
  (delete-file in-file))

(when (file-exists? ou-file)
  (delete-file ou-file))

(with-output-to-file in-file
  (lambda ()
    (display arithmetics)))

(lex in-file ou-file 'counters 'all)

(define lexer (with-input-from-file ou-file
		(lambda ()
		  (get-string-all (current-input-port)))))

(define script (string-append "(let ()\n"
			      lexer "\n"
			      parser "\n"
			      ")"))

(define expr (read (open-string-input-port script)))

(eval expr (environment '(rnrs)
			'(rnrs mutable-strings)
			'(rnrs r5rs)))


;;;; done

(check-report)

;;; end of file
