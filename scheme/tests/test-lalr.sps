;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for lalr
;;;Date: Thu Jul 16, 2009
;;;
;;;Abstract
;;;
;;;	Simple calculator in Scheme.
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


(import (nausicaa)
  (prefix (silex) silex:)
  (lalr)
  (silex lexer)
  (calc-parser)
  (calc-parser-helper)
  (calc-parser-lexer)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing lalr\n")

(debugging #t)


(parameterise ((check-test-name 'calc))

  (define (doit string)
    (let* ((IS		(lexer-make-IS :string string :counters 'all))
	   (lexer	(lexer-make-lexer calc-parser-lexer-table IS))
	   (wrap-lexer	(lambda ()
			  (let ((token (lexer)))
			    (when #f
			      (write (list 'token-from-lexer token))
			      (newline))
			    token)))
	   (error-handler (lambda (message token)
			    (cond ((not (lexical-token? token))
				   (error #f message)) ;invalid token from lexer
				  ((lexical-token?/end-of-input token)
				   (error #f message)) ;unexpected end of input
				  (else
				   (let ((position (lexical-token-source token)))
				     (error #f (string-append
						message " line "
						(number->string (source-location-line position))
						" column "
						(number->string (source-location-column position)))
					    (lexical-token-value token)))))))
	   (parser	(make-calc-parser)))
      (parameterise ((table-of-variables (make-eq-hashtable))
		     (evaluated-expressions '()))
	(let ((v (parser wrap-lexer error-handler)))
	  (cons v (evaluated-expressions))))))

  (check
      (doit "1\n")
    => '(1 1))

  (check
      (doit "1 + 2 + 4 * 3 \n a = 2 \n a * 3 \n")
    => '(6 6 15))

  (check
      (doit "sin(1.2)\n")
    => (list (sin 1.2) (sin 1.2)))

  (check
      (doit "atan(1.2, 0.5)\n")
    => (list (atan 1.2 0.5) (atan 1.2 0.5)))

;;; --------------------------------------------------------------------

  (check
      (guard (exc (else (condition-message exc)))
	(doit "1 +"))
    => "unexpected end of input")

  (check
      (guard (exc (else (condition-message exc)))
	(doit "1 + ="))
    => "syntax error, unexpected token line 1 column 5")

  #t)


(parameterise ((check-test-name 'error-recovery))

  (define test-lexer-string-1 "
blanks		[ \\9]+
newline		[\\10\\13]+
decint          [0-9]+
initial         [a-zA-Z_]
subsequent      {initial}|[0-9.@]
symbol          {initial}{subsequent}*

%%

{blanks}	;ignore blanks
{newline}	(make-lexical-token 'NEWLINE
		 (make-source-location #f yyline yycolumn yyoffset (string-length yytext))
		 'NEWLINE)
{decint}	(make-lexical-token 'NUM
				    (make-source-location #f
							  yyline yycolumn yyoffset
							  (string-length yytext))
				    (string->number yytext))
{symbol}	(make-lexical-token 'ID
				    (make-source-location #f yyline yycolumn yyoffset
							  (string-length yytext))
				    (string->symbol yytext))
<<EOF>>		(make-lexical-token '*eoi*
		 (make-source-location #f yyline yycolumn yyoffset 0)
		 (eof-object))
<<ERROR>>	(assertion-violation #f \"invalid lexer token\")
")

  (define test-parser-sexpr-1
    '((lines (lines line)	: (let ((result $2))
				    (when result
				      (evaluated-expressions (cons result (evaluated-expressions)))))
	     (line)		: (let ((result $1))
				    (when result
				      (evaluated-expressions (cons result (evaluated-expressions))))))
      (line (NEWLINE)		: #\newline
            (NUM NEWLINE)	: $1
	    (error NEWLINE)	: #f)
		;either a line starts  with an expression or assignment,
		;or it  is an  error; in case  of error discard  all the
		;tokens up until the first newline
      ))

  (define (doit string)
    (parameterise ((evaluated-expressions '()))
      (let* ((lexer-table	(silex:lex silex::output-value #t
  				   silex::counters 'all
  				   silex::library-imports '((lalr common))
  				   silex::input-string test-lexer-string-1))
    	      (make-parser	(lalr-parser :output-value #t
  				     :library-imports '((calc-parser-helper))
  				     :terminals '(NUM ID NEWLINE)
  				     :rules test-parser-sexpr-1))
  	      (IS		(lexer-make-IS :string string :counters 'all))
  	      (lexer		(lexer-make-lexer lexer-table IS))
  	      (error-handler	(lambda (message token)
                                  (when #f
                                    (display message)(newline)
                                    (display token)(newline))
                                  (evaluated-expressions
                                    (cons `(error-token . ,(lexical-token-value token))
                                           (evaluated-expressions)))))
  	   (parser		(make-parser)))
  	(parser lexer error-handler)
  	(evaluated-expressions))))

  (check ;correct input
      (doit "1\n")
    => '(1))

  (check
      ;;The 1st line triggers an error, recovery happens, the 1st
      ;;newline is correctly parser; the 2nd line is correct.
      (doit "1 alpha \n 2 \n")
    => '(2 #\newline (error-token . alpha)))

  (check
      ;;The 1st line triggers an error, recovery happens, the newline
      ;;is correctly parsed.
      (doit "1 2 \n")
    => '(#\newline (error-token . 2)))

  (check
      ;;Unexpected end-of-input after the "1" (a newline was expected),
      ;;it triggers an error, end-of-input happens while trying to recover.
      (doit "1")
    => `((error-token . ,(eof-object))))

  (check
      ;;The symbol triggers the first error error, the unexpected
      ;;end-of-input happens while trying to recover.
      (doit "alpha")
    => `((error-token . ,(eof-object)) (error-token . alpha)))

  #t)


;;;; done

(check-report)

;;; end of file
