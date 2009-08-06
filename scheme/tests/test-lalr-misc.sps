;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for (lalr), miscellaneous stuff
;;;Date: Thu Aug  6, 2009
;;;
;;;Abstract
;;;
;;;	Simple calculator in Scheme.
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


(import (nausicaa)
  (prefix (silex) silex:)
  (lalr)
  (silex lexer)
  (sentinel)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing lalr misc\n")

(define eoi-token
  (make-lexical-token '*eoi* #f (eof-object)))


(parameterise ((check-test-name 'error-recovery))

  (define parser-terminals
    '(NUMBER ID NEWLINE))

  (define parser-non-terminals
    '((lines (lines line)	: (yycustom $2)
	     (line)		: (yycustom $1))
      (line (NEWLINE)		: #\newline
            (NUMBER NEWLINE)	: $1
	    (error NEWLINE)	: 'error-client-form)
		;either a line starts with a number or newline, or it is
		;an error; in case of error discard all the tokens until
		;the first newline
      ))

  (define (doit tokens)
    (let* ((make-parser		(lalr-parser :output-value #t
					     :terminals parser-terminals
					     :rules parser-non-terminals))
	   (lexer		(lambda ()
				  (if (null? tokens)
				      eoi-token
				    (let ((t (car tokens)))
				      (set! tokens (cdr tokens))
				      t))))
           (result		'())
           (yycustom		(lambda (value)
                                  (set! result (cons value result))))
	   (error-handler	(lambda (message token)
                                  (yycustom `(error-token . ,(lexical-token-value token)))))
           (parser		(make-parser)))
      (parser lexer error-handler yycustom)
      result))

  (check	;correct input
      (doit (list (make-lexical-token 'NUMBER  #f 1)
		  (make-lexical-token 'NEWLINE #f #\newline)
		  eoi-token))
    => '(1))

  (check
      ;;The ID triggers an error, recovery happens, the first NEWLINE is
      ;;correctly parser; the second line is correct.
      (doit (list (make-lexical-token 'NUMBER  #f 1)
                  (make-lexical-token 'ID      #f 'alpha)
                  (make-lexical-token 'NEWLINE #f #\newline)
                  (make-lexical-token 'NUMBER  #f 2)
                  (make-lexical-token 'NEWLINE #f #\newline)
		  eoi-token))
    => '(2 #\newline error-client-form (error-token . alpha)))

  (check
      ;;The  second  NUMBER triggers  an  error,  recovery happens,  the
      ;;newline is correctly parsed.
      (doit (list (make-lexical-token 'NUMBER  #f 1)
                  (make-lexical-token 'NUMBER  #f 2)
                  (make-lexical-token 'NEWLINE #f #\newline)
		  eoi-token))
    => '(#\newline error-client-form (error-token . 2)))

  (check
      ;;Unexpected  end-of-input   after  the  NUMBER   (a  newline  was
      ;;expected),  it  triggers an  error,  end-of-input happens  while
      ;;trying to recover.
      (doit (list (make-lexical-token 'NUMBER  #f 1)
		  eoi-token))
    => `((error-token . ,(eof-object))))

  (check
      ;;The ID triggers the  error, then unexpected end-of-input happens
      ;;while trying to recover.
      (doit (list (make-lexical-token 'ID #f 'alpha)
		  eoi-token))
    => `((error-token . ,(eof-object)) (error-token . alpha)))

  #t)


(parameterise ((check-test-name 'no-client-form))

  (define parser-terminals
    '(NUMBER COMMA NEWLINE))

  (define parser-non-terminals
    '((lines (lines line)		: (yycustom $2)
	     (line)			: (yycustom $1))
      (line (NEWLINE)			: #\newline
            (NUMBER NEWLINE)		: $1
            (COMMA NUMBER NEWLINE))
                ;this is a rule with no semantic action
      ))

  (define (doit tokens)
    (let* ((make-parser	(lalr-parser :output-value #t
				     :terminals parser-terminals
				     :rules parser-non-terminals))
	   (lexer		(lambda ()
				  (if (null? tokens)
				      eoi-token
				    (let ((t (car tokens)))
				      (set! tokens (cdr tokens))
				      t))))
           (result		'())
           (yycustom		(lambda (value)
                                  (set! result (cons value result))))
	   (error-handler	(lambda (message token)
                                  (yycustom `(error-token . ,(lexical-token-value token)))))
           (parser		(make-parser)))
  	(parser lexer error-handler yycustom)
        result))

  (check ;correct input
      (doit (list (make-lexical-token 'NUMBER  #f 1)
		  (make-lexical-token 'NEWLINE #f #\newline)
		  eoi-token))
    => '(1))

  (check ;correct input with comma, which is a rule with no client form
      (doit (list (make-lexical-token 'COMMA   #f #\,)
		  (make-lexical-token 'NUMBER  #f 1)
		  (make-lexical-token 'NEWLINE #f #\newline)
		  eoi-token))
    => (list sentinel))

  #t)


;;;; done

(check-report)

;;; end of file
