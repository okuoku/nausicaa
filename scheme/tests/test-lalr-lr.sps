;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for (lalr), miscellaneous stuff
;;;Date: Thu Aug  6, 2009
;;;
;;;Abstract
;;;
;;;	Miscellaneous tests for (lalr), LR driver.
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
  (lalr)
  (sentinel)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing lalr LR driver\n")

(define eoi-token
  (make-lexical-token '*eoi* #f (eof-object)))


(parameterise ((check-test-name 'error-recovery))

  (define parser-terminals
    '(NUMBER ID NEWLINE))

  (define parser-non-terminals
    '((script	(lines)			: #f)
      (lines	(lines line)		: (yycustom $2)
		(line)			: (yycustom $1))
      (line	(NEWLINE)		: #\newline
		(NUMBER NEWLINE)	: $1
		(error NEWLINE)		: 'error-client-form)))

  (define make-parser
    (lalr-parser :output-value #t :expect #f
		 :terminals parser-terminals
		 :rules parser-non-terminals))

  (define (doit tokens)
    (let* ((lexer		(lambda ()
				  (if (null? tokens)
				      eoi-token
				    (let ((t (car tokens)))
				      (set! tokens (cdr tokens))
				      t))))
           (result		'())
           (yycustom		(lambda (value)
                                  (set! result (cons value result))
				  'yycustom))
	   (error-handler	(lambda (message token)
				  ;;(display message)(newline)
                                  (yycustom `(error-token . ,(lexical-token-value token)))))
           (parser		(make-parser)))
      (parameterise ((debugging #f))
	(parser lexer error-handler yycustom))
      result))

  (when #f
    (lalr-parser :output-port (current-output-port)
		 :expect #f
		 :terminals parser-terminals
		 :rules parser-non-terminals)
    (newline)
    (newline))

  (check
      (doit (list (make-lexical-token 'NUMBER  #f 1)
		  (make-lexical-token 'NEWLINE #f #\newline)
		  eoi-token))
    => '(1))

  (check
      ;;The ID triggers an error, recovery happens, the first NEWLINE is
      ;;correctly parsed; the second line is correct.
      (doit (list (make-lexical-token 'NUMBER  #f 1)
                  (make-lexical-token 'ID      #f 'alpha)
                  (make-lexical-token 'NEWLINE #f #\newline)
                  (make-lexical-token 'NUMBER  #f 2)
                  (make-lexical-token 'NEWLINE #f #\newline)
		  eoi-token))
    => '(2 #\newline error-client-form (error-token . alpha)))

  (check
      ;;The  first ID  triggers an  error, recovery  happens,  the first
      ;;NEWLINE is correctly parsed; the second line is correct.
      (doit (list (make-lexical-token 'NUMBER  #f 1)
                  (make-lexical-token 'ID      #f 'alpha)
                  (make-lexical-token 'ID      #f 'beta)
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


(parameterise ((check-test-name 'no-semantic-clause))

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

  (define make-parser
    (lalr-parser :output-value #t :expect #f
		 :terminals parser-terminals
		 :rules parser-non-terminals))

  (define (doit tokens)
    (let* ((lexer		(lambda ()
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


(parameterise ((check-test-name 'single-expressions))

  ;;This is the grammar of the (lalr) documentation in Texinfo format.

  (define parser-terminals
    '(N O C (left: A) (left: M) (nonassoc: U)))

  (define parser-non-terminals
    '((E	(N)		: $1
		(E A E)		: ($2 $1 $3)
		(E M E)		: ($2 $1 $3)
		(A E (prec: U))	: ($1 $2)
		(O E C)		: $2)))

  (define make-parser
    (lalr-parser :output-value #t :expect #f
		 :terminals parser-terminals
		 :rules parser-non-terminals))

  (define (doit tokens)
    (let* ((lexer		(lambda ()
				  (let ((t (car tokens)))
				    (set! tokens (cdr tokens))
				    t)))
	   (error-handler	(lambda (message token)
				  ;;(display message)(newline)
                                  `(error-token . ,(lexical-token-value token))))
           (parser		(make-parser)))
      (parser lexer error-handler)))

  (when #f
    (lalr-parser :output-port (current-output-port)
		 :expect #f
;;;		 :dump-table "/tmp/marco/p"
		 :terminals parser-terminals
		 :rules parser-non-terminals)
    (newline)
    (newline))

  (check	;correct input
      (doit (list (make-lexical-token 'N #f 1)
		  eoi-token))
    => 1)

  (check	;correct input
      (doit (list (make-lexical-token 'A #f -)
		  (make-lexical-token 'N #f 1)
		  eoi-token))
    => -1)

  (check	;correct input
      (doit (list (make-lexical-token 'A #f +)
		  (make-lexical-token 'N #f 1)
		  eoi-token))
    => 1)

  (check	;correct input
      (doit (list (make-lexical-token 'N #f 1)
		  (make-lexical-token 'A #f +)
		  (make-lexical-token 'N #f 2)
		  eoi-token))
    => 3)

  (check	 ;correct input
      (doit (list (make-lexical-token 'N #f 1)
		  (make-lexical-token 'A #f +)
		  (make-lexical-token 'N #f 2)
		  (make-lexical-token 'M #f *)
		  (make-lexical-token 'N #f 3)
		  eoi-token))
    => 7)

  (check	;correct input
      (doit (list (make-lexical-token 'O #f #\()
		  (make-lexical-token 'N #f 1)
		  (make-lexical-token 'A #f +)
		  (make-lexical-token 'N #f 2)
		  (make-lexical-token 'C #f #\))
		  (make-lexical-token 'M #f *)
		  (make-lexical-token 'N #f 3)
		  eoi-token))
    => 9)

  #t)


(parameterise ((check-test-name 'associativity))

  (define parser-terminals
    '(N (left: A)
	(right: M)
	(nonassoc: U)))

  (define parser-non-terminals
    '((E	(N)		: $1
		(E A E)		: (list $1 $2 $3)
		(E M E)		: (list $1 $2 $3)
		(A E (prec: U))	: (list $1 $2))))

  (define make-parser
    (parameterise ((debugging	#f))
      (lalr-parser :output-value #t :expect 0
		   :terminals parser-terminals
		   :rules parser-non-terminals)))

  (define (doit tokens)
    (let* ((lexer		(lambda ()
				  (let ((t (car tokens)))
				    (set! tokens (cdr tokens))
				    t)))
	   (error-handler	(lambda (message token)
				  ;;(display message)(newline)
                                  `(error-token . ,(lexical-token-value token))))
           (parser		(make-parser)))
      (parser lexer error-handler)))

  (when #f
    (lalr-parser :output-port (current-output-port)
		 :expect #f
;;;		 :dump-table "/tmp/marco/p"
		 :terminals parser-terminals
		 :rules parser-non-terminals)
    (newline)
    (newline))

  (check
      (doit (list (make-lexical-token 'N #f 1)
		  eoi-token))
    => 1)

  (check
      (doit (list (make-lexical-token 'A #f '-)
		  (make-lexical-token 'N #f 1)
		  eoi-token))
    => '(- 1))

  (check
      (doit (list (make-lexical-token 'A #f '+)
		  (make-lexical-token 'N #f 1)
		  eoi-token))
    => '(+ 1))

  (check
      (doit (list (make-lexical-token 'N #f 1)
		  (make-lexical-token 'A #f '+)
		  (make-lexical-token 'N #f 2)
		  eoi-token))
    => '(1 + 2))

  (check
      (doit (list (make-lexical-token 'N #f 1)
		  (make-lexical-token 'A #f '+)
		  (make-lexical-token 'N #f 2)
		  (make-lexical-token 'M #f '*)
		  (make-lexical-token 'N #f 3)
		  eoi-token))
    => '(1 + (2 * 3)))

  (check	;left associative
      (doit (list (make-lexical-token 'N #f 1)
		  (make-lexical-token 'A #f '+)
		  (make-lexical-token 'N #f 2)
		  (make-lexical-token 'A #f '+)
		  (make-lexical-token 'N #f 3)
		  eoi-token))
    => '((1 + 2) + 3))

  (check	;right associative
      (doit (list (make-lexical-token 'N #f 1)
		  (make-lexical-token 'M #f '*)
		  (make-lexical-token 'N #f 2)
		  (make-lexical-token 'M #f '*)
		  (make-lexical-token 'N #f 3)
		  eoi-token))
    => '(1 * (2 * 3)))

  #t)


(parameterise ((check-test-name 'script-expression))

  ;;This is the grammar of the (lalr) documentation in Texinfo format.

  (define parser-terminals
    '(N O C T (left: A) (left: M) (nonassoc: U)))

  (define parser-non-terminals
    '((script	(lines)		: #f)

      (lines	(lines line)	: (yycustom $2)
		(line)		: (yycustom $1))

      (line	(T)		: #\newline
		(E T)		: $1
		(error T)	: #f)

      (E	(N)		: $1
		(E A E)		: ($2 $1 $3)
		(E M E)		: ($2 $1 $3)
		(A E (prec: U))	: ($1 $2)
		(O E C)		: $2)))

  (define make-parser
    (lalr-parser :output-value #t :expect #f
		 :terminals parser-terminals
		 :rules parser-non-terminals))

  (define (doit tokens)
    (let* ((lexer		(lambda ()
				  (let ((t (car tokens)))
				    (set! tokens (cdr tokens))
				    t)))
           (result		'())
           (yycustom		(lambda (value)
                                  (set! result (cons value result))
				  'yycustom))
	   (error-handler	(lambda (message token)
				  ;;(display message)(newline)
                                  (yycustom `(error-token . ,(lexical-token-value token)))))
           (parser		(make-parser)))
      (parser lexer error-handler yycustom)
      result))

  (when #f
    (lalr-parser :output-port (current-output-port)
		 :expect #f
;;;		 :dump-table "/tmp/marco/p"
		 :terminals parser-terminals
		 :rules parser-non-terminals)
    (newline)
    (newline))

  (check	;correct input
      (doit (list (make-lexical-token 'T #f #\newline)
		  eoi-token))
    => '(#\newline))

  (check	;correct input
      (doit (list (make-lexical-token 'N #f 1)
		  (make-lexical-token 'T #f #\newline)
		  eoi-token))
    => '(1))

  (check	;correct input
      (doit (list (make-lexical-token 'N #f 1)
		  (make-lexical-token 'A #f +)
		  (make-lexical-token 'N #f 2)
		  (make-lexical-token 'T #f #\newline)
		  eoi-token))
    => '(3))

  (check	 ;correct input
      (doit (list (make-lexical-token 'N #f 1)
		  (make-lexical-token 'A #f +)
		  (make-lexical-token 'N #f 2)
		  (make-lexical-token 'M #f *)
		  (make-lexical-token 'N #f 3)
		  (make-lexical-token 'T #f #\newline)
		  eoi-token))
    => '(7))

  (check	;correct input
      (doit (list (make-lexical-token 'O #f #\()
		  (make-lexical-token 'N #f 1)
		  (make-lexical-token 'A #f +)
		  (make-lexical-token 'N #f 2)
		  (make-lexical-token 'C #f #\))
		  (make-lexical-token 'M #f *)
		  (make-lexical-token 'N #f 3)
		  (make-lexical-token 'T #f #\newline)
		  eoi-token))
    => '(9))

  (check	;correct input
      (doit (list (make-lexical-token 'O #f #\()
		  (make-lexical-token 'N #f 1)
		  (make-lexical-token 'A #f +)
		  (make-lexical-token 'N #f 2)
		  (make-lexical-token 'C #f #\))
		  (make-lexical-token 'M #f *)
		  (make-lexical-token 'N #f 3)
		  (make-lexical-token 'T #f #\newline)
		  (make-lexical-token 'N #f 4)
		  (make-lexical-token 'M #f /)
		  (make-lexical-token 'N #f 5)
		  (make-lexical-token 'T #f #\newline)
		  eoi-token))
    => '(4/5 9))

  #t)


;;;; done

(check-report)

;;; end of file