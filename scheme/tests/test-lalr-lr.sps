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


;;;; helpers

(define eoi-token
  (make-lexical-token '*eoi* #f (eof-object)))

(define (make-lexer list-of-tokens)
  ;;Return a lexer closure  drawing tokens from the list LIST-OF-TOKENS.
  ;;When the list is empty, return the EOI-TOKEN.
  ;;
  (lambda ()
    (if (null? list-of-tokens)
	eoi-token
      (begin0
	  (car list-of-tokens)
	(set! list-of-tokens (cdr list-of-tokens))))))

(define (make-error-handler yycustom)
  ;;Return  an error  handler closure  that calls  YYCUSTOM with  a pair
  ;;describing the offending token.  To just return the pair invoke as:
  ;;
  ;;	(make-error-handler (lambda x x))
  ;;
  (lambda (message token)
    (yycustom `(error-handler . ,(lexical-token-value token)))))

(define (debug:print-tables doit? terminals non-terminals)
  (when doit?
    (let ((port (current-output-port)))
      (lalr-parser :output-port port
		   :expect #f
		   :terminals terminals
		   :rules non-terminals)
      (newline port)
      (newline port))))


(parameterise ((check-test-name 'basics))

;;;Test very basic grammars.

  (define (error-handler message token)
    (cons message (lexical-token-value token)))

  (define (doit-1 . tokens)
    ;;A grammar that only accept a single terminal as input.
    (let* ((lexer		(make-lexer tokens))
	   (make-parser		(make-lalr-parser :output-value #t
						  :expect #f
						  :terminals '(A)
						  :rules '((e (A) : $1))))
           (parser		(make-parser)))
      (parser lexer error-handler)))

  (define (doit-2 . tokens)
    ;;A grammar that only accept a single terminal or the EOI.
    (let* ((lexer		(make-lexer tokens))
	   (make-parser		(lalr-parser :output-value #t
					     :expect #f
					     :terminals '(A)
					     :rules '((e (A) : $1
							 ()  : 0))))
           (parser		(make-parser)))
      (parser lexer error-handler)))

  (define (doit-3 . tokens)
    ;;A grammar that accepts fixed sequences of a single terminal or the
    ;;EOI.
    (let* ((lexer		(make-lexer tokens))
	   (make-parser		(lalr-parser :output-value #t
					     :expect #f
					     :terminals '(A)
					     :rules '((e (A)     : (list $1)
							 (A A)   : (list $1 $2)
							 (A A A) : (list $1 $2 $3)
							 ()      : 0))))
           (parser		(make-parser)))
      (parser lexer error-handler)))

  (define (doit-4 . tokens)
    ;;A grammar accepting a sequence  of equal tokens.  The return value
    ;;is the value of the last parsed token.
    (let* ((lexer		(make-lexer tokens))
	   (make-parser		(lalr-parser :output-value #t
					     :expect #f
					     :terminals '(A)
					     :rules '((e (e A) : $2
							 (A)   : $1
							 ()    : 0))))
           (parser		(make-parser)))
      (parser lexer error-handler)))

  (define (doit-5 . tokens)
    ;;A grammar accepting a sequence  of equal tokens.  The return value
    ;;is the list of values.
    (let* ((lexer		(make-lexer tokens))
	   (make-parser		(lalr-parser :output-value #t
					     :expect #f
					     :terminals '(A)
					     :rules '((e (e A) : (cons $2 $1)
							 (A)   : (list $1)
							 ()    : 0))))
           (parser		(make-parser)))
      (parser lexer error-handler)))

;;; --------------------------------------------------------------------

  (check
      (doit-1 (make-lexical-token 'A #f 1))
    => 1)

  (check
      (doit-1)
    => `("unexpected end of input" . ,(eof-object)))

  (check
    ;;Parse correctly the first A  and reduce it.  The second A triggers
    ;;an  error which  empties  the  stack and  consumes  all the  input
    ;;tokens.  Finally, an unexpected end-of-input error is returned.
      (parameterise ((debugging #f))
	(doit-1 (make-lexical-token 'A #f 1)
		(make-lexical-token 'A #f 2)
		(make-lexical-token 'A #f 3)))
    => `("unexpected end of input" . ,(eof-object)))

;;; --------------------------------------------------------------------

  (check
      (parameterise ((debugging #f))
	(doit-2))
    => 0)

  (check
      (doit-2 (make-lexical-token 'A #f 1))
    => 1)

  (check
    ;;Parse correctly the first A  and reduce it.  The second A triggers
    ;;an  error which  empties  the  stack and  consumes  all the  input
    ;;tokens.  Finally, an unexpected end-of-input error is returned.
      (parameterise ((debugging #f))
	(doit-1 (make-lexical-token 'A #f 1)
		(make-lexical-token 'A #f 2)
		(make-lexical-token 'A #f 3)))
    => `("unexpected end of input" . ,(eof-object)))

;;; --------------------------------------------------------------------

  (check
      (doit-3 (make-lexical-token 'A #f 1))
    => '(1))

  (check
      (doit-3 (make-lexical-token 'A #f 1)
	      (make-lexical-token 'A #f 2))
    => '(1 2))

  (check
      (doit-3 (make-lexical-token 'A #f 1)
	      (make-lexical-token 'A #f 2)
	      (make-lexical-token 'A #f 3))
    => '(1 2 3))

  (check
      (doit-3)
    => 0)

;;; --------------------------------------------------------------------

  (check
      (doit-4)
    => 0)

  (check
      (doit-4 (make-lexical-token 'A #f 1))
    => 1)

  (check
      (doit-4 (make-lexical-token 'A #f 1)
	      (make-lexical-token 'A #f 2)
	      (make-lexical-token 'A #f 3))
    => 3)

;;; --------------------------------------------------------------------

  (check
      (doit-5)
    => 0)

  (check
      (doit-5 (make-lexical-token 'A #f 1))
    => '(1))

  (check
      (doit-5 (make-lexical-token 'A #f 1)
	      (make-lexical-token 'A #f 2))
    => '(2 1))

  (check
      (doit-5 (make-lexical-token 'A #f 1)
	      (make-lexical-token 'A #f 2)
	      (make-lexical-token 'A #f 3))
    => '(3 2 1))

  #t)


(parameterise ((check-test-name 'error-recovery-1))

;;;Test error recovery with a terminator terminal.

  (define terminals
    '(NUMBER BAD NEWLINE))

  (define non-terminals
    '((script	(lines)			: (reverse $1)
		()			: '())
      (lines	(lines line)		: (cons $2 $1)
		(line)			: (list $1))
      (line	(NEWLINE)		: (list 'line $1)
		(NUMBER NEWLINE)	: (list 'line $1 $2)
		(NUMBER NUMBER NEWLINE)	: (list 'line $1 $2 $3)

		;;This semantic  action will cause "(recover  $1 $2)" to
		;;be the result of the offending line.
		(error NEWLINE)		: (list 'recover $1 $2))))

  (define (doit . tokens)
    (let* ((lexer		(make-lexer tokens))
	   (error-handler	(make-error-handler (lambda x x)))
	   (make-parser		(lalr-parser :output-value #t :expect #f
					     :terminals terminals
					     :rules non-terminals))
           (parser		(make-parser)))
      (parser lexer error-handler)))

  (debug:print-tables #f terminals non-terminals)

;;; --------------------------------------------------------------------
;;; No errors, grammar tests.

  (check
      (doit)
    => '())

  (check
      (doit (make-lexical-token 'NEWLINE #f #\newline))
    => '((line #\newline)))

  (check
      (doit (make-lexical-token 'NUMBER  #f 1)
	    (make-lexical-token 'NEWLINE #f #\newline))
    => '((line 1 #\newline)))

  (check
      (doit (make-lexical-token 'NUMBER  #f 1)
	    (make-lexical-token 'NUMBER  #f 2)
	    (make-lexical-token 'NEWLINE #f #\newline))
    => '((line 1 2 #\newline)))

  (check
      (doit (make-lexical-token 'NUMBER  #f 1)
	    (make-lexical-token 'NEWLINE #f #\newline)
	    (make-lexical-token 'NUMBER  #f 2)
	    (make-lexical-token 'NEWLINE #f #\newline))
    => '((line 1 #\newline)
	 (line 2 #\newline)))

  (check
      (doit (make-lexical-token 'NUMBER  #f 1)
	    (make-lexical-token 'NEWLINE #f #\newline)
	    (make-lexical-token 'NUMBER  #f 2)
	    (make-lexical-token 'NEWLINE #f #\newline)
	    (make-lexical-token 'NUMBER  #f 3)
	    (make-lexical-token 'NEWLINE #f #\newline))
    => '((line 1 #\newline)
	 (line 2 #\newline)
	 (line 3 #\newline)))

  (check
      (doit (make-lexical-token 'NUMBER  #f 1)
	    (make-lexical-token 'NEWLINE #f #\newline)
	    (make-lexical-token 'NUMBER  #f 2)
	    (make-lexical-token 'NEWLINE #f #\newline)
	    (make-lexical-token 'NUMBER  #f 3)
	    (make-lexical-token 'NEWLINE #f #\newline)
	    (make-lexical-token 'NUMBER  #f 41)
	    (make-lexical-token 'NUMBER  #f 42)
	    (make-lexical-token 'NEWLINE #f #\newline))
    => '((line 1 #\newline)
	 (line 2 #\newline)
	 (line 3 #\newline)
	 (line 41 42 #\newline)))

;;; --------------------------------------------------------------------
;;; Successful error recovery.

  (check
      ;;The BAD  triggers an error, recovery happens,  the first NEWLINE
      ;;is  correctly  parsed as  recovery  token;  the  second line  is
      ;;correct.
      (doit (make-lexical-token 'NUMBER  #f 1)
	    (make-lexical-token 'BAD      #f 'alpha)
	    (make-lexical-token 'NEWLINE #f #\newline)
	    (make-lexical-token 'NUMBER  #f 2)
	    (make-lexical-token 'NEWLINE #f #\newline))
    => '((recover error #\newline)
	 (line 2 #\newline)))

  (check
      ;;The first  BAD triggers an error, recovery  happens skipping the
      ;;second  and  third  BADs,  the  first  NEWLINE  is  detected  as
      ;;synchronisation token; the second line is correct.
      (doit (make-lexical-token 'NUMBER  #f 1)
	    (make-lexical-token 'BAD     #f 'alpha)
	    (make-lexical-token 'BAD     #f 'beta)
	    (make-lexical-token 'BAD     #f 'delta)
	    (make-lexical-token 'NEWLINE #f #\newline)
	    (make-lexical-token 'NUMBER  #f 2)
	    (make-lexical-token 'NEWLINE #f #\newline))
    => '((recover error #\newline)
	 (line 2 #\newline)))

;;; --------------------------------------------------------------------
;;; Failed error recovery.

  (check
      ;;End-of-input is found after NUMBER.
      (doit (make-lexical-token 'NUMBER  #f 1))
    => `((error-handler . ,(eof-object))))

  (check
      ;;The BAD triggers the error, the stack is rewind up to the start,
      ;;then end-of-input happens while  trying to skip tokens until the
      ;;synchronisation  one is  found.  End-of-input  is  an acceptable
      ;;token after the start.
      (parameterise ((debugging #f))
	(doit (make-lexical-token 'NUMBER  #f 1)
	      (make-lexical-token 'BAD     #f 'alpha)
	      (make-lexical-token 'BAD     #f 'beta)
	      (make-lexical-token 'BAD     #f 'delta)))
    => '())

  (check
      ;;The BAD triggers the error, the stack is rewind up to the start,
      ;;then end-of-input happens while  trying to skip tokens until the
      ;;synchronisation  one is  found.  End-of-input  is  an acceptable
      ;;token after the start.
      (parameterise ((debugging #f))
	(doit (make-lexical-token 'BAD #f 'alpha)))
    => '())

  #t)


(parameterise ((check-test-name 'error-recovery-2))

;;;Test error recovery policy  when the synchronisation terminal has the
;;;same category of the lookahead that raises the error.

  (define make-parser
    (lalr-parser :output-value #t :expect #f
		 :terminals '(A B C)
		 :rules '((alphas (alpha)		: $1
				  (alphas alpha)	: $2)
			  (alpha (A B)		: (list $1 $2)
				 (C)		: $1
				 (error C)	: (list $1 $2)))))

  (define (doit . tokens)
    (let* ((lexer		(make-lexer tokens))
           (result		'())
	   (error-handler	(make-error-handler (lambda x x)))
           (parser		(make-parser)))
      (parser lexer error-handler)))

;;; --------------------------------------------------------------------
;;; No error, just grammar tests.

  (check
      (doit (make-lexical-token 'A #f 1)
	    (make-lexical-token 'B #f 2))
    => '(1 2))

  (check
      (doit (make-lexical-token 'C #f 3))
    => '3)

;;; --------------------------------------------------------------------
;;; Successful error recovery.

  (check
      ;;Error, recovery, eoi.
      (parameterise ((debugging #f))
	(doit (make-lexical-token 'A #f 1)
	      (make-lexical-token 'C #f 3)))
    => '(error 3))

  (check
    ;;Error, recovery, correct parse.
    (parameterise ((debugging #f))
      (doit (make-lexical-token 'A #f 1)
	    (make-lexical-token 'C #f 3)
	    (make-lexical-token 'A #f 1)
	    (make-lexical-token 'B #f 2)))
    => '(1 2))

  #t)


(parameterise ((check-test-name 'lexer-error))

;;;Test the lexer returning a non-token value.

  (define make-parser
    (lalr-parser :output-value #t :expect #f
		 :terminals '(A B C)
		 :rules '((alpha (A B)		: (list $1 $2)
				 (C)		: $1
				 (error C)	: (list $1 $2)))))

  (define (doit . tokens)
    (let* ((lexer		(make-lexer tokens))
           (result		'())
	   (yycustom		(lambda args args))
	   (error-handler	(lambda (message token)
				  (error #f message token)))
           (parser		(make-parser)))
      (parser lexer error-handler)))

;;; --------------------------------------------------------------------
;;; No error, just grammar tests.

  (check
      (doit (make-lexical-token 'A #f 1)
	    (make-lexical-token 'B #f 2))
    => '(1 2))

;;; --------------------------------------------------------------------

  (check
      (guard (exc (else (cons (condition-message exc)
			      (condition-irritants exc))))
	(doit (make-lexical-token 'A #f 1)
	      (make-lexical-token 'B #f 2)
	      123))
    => '("expected lexical token from lexer" 123))

  #t)


(parameterise ((check-test-name 'no-semantic-clause))

  (define terminals
    '(NUMBER COMMA NEWLINE))

  (define non-terminals
    '((lines (lines line)		: (yycustom $2)
	     (line)			: (yycustom $1))
      (line (NEWLINE)			: #\newline
            (NUMBER NEWLINE)		: $1
            (COMMA NUMBER NEWLINE))
                ;this is a rule with no semantic action
      ))

  (define make-parser
    (lalr-parser :output-value #t :expect #f
		 :terminals terminals
		 :rules non-terminals))

  (define (doit . tokens)
    (let* ((lexer		(make-lexer tokens))
           (result		'())
           (yycustom		(lambda (value)
                                  (set! result (cons value result))))
	   (error-handler	(make-error-handler yycustom))
           (parser		(make-parser)))
  	(parser lexer error-handler yycustom)
        result))

  (check	;correct input
      (doit (make-lexical-token 'NUMBER  #f 1)
	    (make-lexical-token 'NEWLINE #f #\newline))
    => '(1))

  (check  ;correct input with comma, which is a rule with no client form
      (doit (make-lexical-token 'COMMA   #f #\,)
	    (make-lexical-token 'NUMBER  #f 1)
	    (make-lexical-token 'NEWLINE #f #\newline))
    => (list sentinel))

  #t)


(parameterise ((check-test-name 'single-expressions))

  ;;This is the grammar of the (lalr) documentation in Texinfo format.

  (define terminals
    '(N O C (left: A) (left: M) (nonassoc: U)))

  (define non-terminals
    '((E	(N)		: $1
		(E A E)		: ($2 $1 $3)
		(E M E)		: ($2 $1 $3)
		(A E (prec: U))	: ($1 $2)
		(O E C)		: $2)))

  (define make-parser
    (lalr-parser :output-value #t :expect #f
		 :terminals terminals
		 :rules non-terminals))

  (define (doit . tokens)
    (let* ((lexer		(make-lexer tokens))
	   (error-handler	(make-error-handler (lambda x x)))
           (parser		(make-parser)))
      (parser lexer error-handler)))

  (debug:print-tables #f terminals non-terminals)

  (check	;correct input
      (doit (make-lexical-token 'N #f 1))
    => 1)

  (check	;correct input
      (doit (make-lexical-token 'A #f -)
	    (make-lexical-token 'N #f 1))
    => -1)

  (check	;correct input
      (doit (make-lexical-token 'A #f +)
	    (make-lexical-token 'N #f 1))
    => 1)

  (check	;correct input
      (doit (make-lexical-token 'N #f 1)
	    (make-lexical-token 'A #f +)
	    (make-lexical-token 'N #f 2))
    => 3)

  (check	;correct input
      (doit (make-lexical-token 'N #f 1)
	    (make-lexical-token 'A #f +)
	    (make-lexical-token 'N #f 2)
	    (make-lexical-token 'M #f *)
	    (make-lexical-token 'N #f 3))
    => 7)

  (check	;correct input
      (doit (make-lexical-token 'O #f #\()
	    (make-lexical-token 'N #f 1)
	    (make-lexical-token 'A #f +)
	    (make-lexical-token 'N #f 2)
	    (make-lexical-token 'C #f #\))
	    (make-lexical-token 'M #f *)
	    (make-lexical-token 'N #f 3))
    => 9)

  #t)


(parameterise ((check-test-name 'associativity-1))

;;;Expression language  with no associativity  attributes.  The conflict
;;;resolution rules:
;;;
;;; Shift/Reduce	-> Shift
;;; Reduce/Reduce	-> the Reduce that comes first in the grammar
;;;

  (define make-parser
    (lalr-parser
     :output-value #t :expect 0
     :terminals '(N A B M D)

     :rules '((E (E A E)	: (list $2 $1 $3)
		 (E B E)	: (list $2 $1 $3)
		 (E M E A E)	: (list $2 $1 $3)
		 (E D E B E)	: (list $2 $1 $3)
		 (N)		: $1))))

  (define (doit . tokens)
    (let* ((lexer		(make-lexer tokens))
	   (error-handler	(make-error-handler (lambda x x)))
           (parser		(make-parser)))
      (parser lexer error-handler)))

;;; --------------------------------------------------------------------
;;; Grammar tests, no associativity.

  (check
      (doit (make-lexical-token 'N #f 1))
    => 1)

  (check
      (doit (make-lexical-token 'A #f '+)
	    (make-lexical-token 'N #f 1))
    => '(+ 1))

  (check
      (doit (make-lexical-token 'B #f '-)
	    (make-lexical-token 'N #f 1))
    => '(- 1))

  (check
      (doit (make-lexical-token 'N #f 1)
	    (make-lexical-token 'A #f '+)
	    (make-lexical-token 'N #f 2))
    => '(+ 1 2))

  (check
      (doit (make-lexical-token 'N #f 1)
	    (make-lexical-token 'B #f '-)
	    (make-lexical-token 'N #f 2))
    => '(- 1 2))

  (check
      (doit (make-lexical-token 'N #f 1)
	    (make-lexical-token 'M #f '*)
	    (make-lexical-token 'N #f 2))
    => '(* 1 2))

  (check
      (doit (make-lexical-token 'N #f 1)
	    (make-lexical-token 'D #f '/)
	    (make-lexical-token 'N #f 2))
    => '(/ 1 2))

;;; --------------------------------------------------------------------
;;; Operator precedence.

  (check
      (doit (make-lexical-token 'N #f 1)
	    (make-lexical-token 'A #f '+)
	    (make-lexical-token 'N #f 2)
	    (make-lexical-token 'B #f '-)
	    (make-lexical-token 'N #f 3))
    => '(+ 1 (- 2 3)))

  (check
      (doit (make-lexical-token 'N #f 1)
	    (make-lexical-token 'A #f '+)
	    (make-lexical-token 'N #f 2)
	    (make-lexical-token 'M #f '*)
	    (make-lexical-token 'N #f 3))
    => '(+ 1 (* 2 3)))

  (check
      (doit (make-lexical-token 'N #f 1)
	    (make-lexical-token 'A #f '+)
	    (make-lexical-token 'N #f 2)
	    (make-lexical-token 'D #f '/)
	    (make-lexical-token 'N #f 3))
    => '(+ 1 (/ 2 3)))

;;; --------------------------------------------------------------------

  (check
      (doit (make-lexical-token 'N #f 1)
	    (make-lexical-token 'B #f '-)
	    (make-lexical-token 'N #f 2)
	    (make-lexical-token 'M #f '+)
	    (make-lexical-token 'N #f 3))
    => '(- 1 (+ 2 3)))

  (check
      (doit (make-lexical-token 'N #f 1)
	    (make-lexical-token 'B #f '-)
	    (make-lexical-token 'N #f 2)
	    (make-lexical-token 'M #f '*)
	    (make-lexical-token 'N #f 3))
    => '(- 1 (* 2 3)))

  (check
      (doit (make-lexical-token 'N #f 1)
	    (make-lexical-token 'B #f '-)
	    (make-lexical-token 'N #f 2)
	    (make-lexical-token 'D #f '/)
	    (make-lexical-token 'N #f 3))
    => '(- 1 (/ 2 3)))

;;; --------------------------------------------------------------------

  (check
      (doit (make-lexical-token 'N #f 1)
	    (make-lexical-token 'M #f '*)
	    (make-lexical-token 'N #f 2)
	    (make-lexical-token 'A #f '+)
	    (make-lexical-token 'N #f 3))
    => '(+ (* 1 2) 3))

  (check
      (doit (make-lexical-token 'N #f 1)
	    (make-lexical-token 'M #f '*)
	    (make-lexical-token 'N #f 2)
	    (make-lexical-token 'B #f '-)
	    (make-lexical-token 'N #f 3))
    => '(- (* 1 2) 3))

  (check
      (doit (make-lexical-token 'N #f 1)
	    (make-lexical-token 'M #f '*)
	    (make-lexical-token 'N #f 2)
	    (make-lexical-token 'A #f '/)
	    (make-lexical-token 'N #f 3))
    => '(/ (* 1 2) 3))

;;; --------------------------------------------------------------------

  (check
      (doit (make-lexical-token 'N #f 1)
	    (make-lexical-token 'D #f '/)
	    (make-lexical-token 'N #f 2)
	    (make-lexical-token 'A #f '+)
	    (make-lexical-token 'N #f 3))
    => '(+ (/ 1 2) 3))

  (check
      (doit (make-lexical-token 'N #f 1)
	    (make-lexical-token 'D #f '/)
	    (make-lexical-token 'N #f 2)
	    (make-lexical-token 'B #f '-)
	    (make-lexical-token 'N #f 3))
    => '(- (/ 1 2) 3))

  (check
      (doit (make-lexical-token 'N #f 1)
	    (make-lexical-token 'D #f '/)
	    (make-lexical-token 'N #f 2)
	    (make-lexical-token 'B #f '*)
	    (make-lexical-token 'N #f 3))
    => '(* (/ 1 2) 3))

;;; --------------------------------------------------------------------
;;; Associativity.

  (check	;left associative
      (doit (make-lexical-token 'N #f 1)
	    (make-lexical-token 'A #f '+)
	    (make-lexical-token 'N #f 2)
	    (make-lexical-token 'A #f '+)
	    (make-lexical-token 'N #f 3))
    => '(+ (+ 1 2) 3))

  (check	;left associative
      (doit (make-lexical-token 'N #f 1)
	    (make-lexical-token 'B #f '-)
	    (make-lexical-token 'N #f 2)
	    (make-lexical-token 'B #f '-)
	    (make-lexical-token 'N #f 3))
    => '(- (- 1 2) 3))

  (check	;right associative
      (doit (make-lexical-token 'N #f 1)
	    (make-lexical-token 'M #f '*)
	    (make-lexical-token 'N #f 2)
	    (make-lexical-token 'M #f '*)
	    (make-lexical-token 'N #f 3))
    => '(* 1 (* 2 3)))

  (check	;right associative
      (doit (make-lexical-token 'N #f 1)
	    (make-lexical-token 'D #f '/)
	    (make-lexical-token 'N #f 2)
	    (make-lexical-token 'D #f '/)
	    (make-lexical-token 'N #f 3))
    => '(/ 1 (/ 2 3)))

  (check	;left associative
      (doit (make-lexical-token 'N #f 1)
	    (make-lexical-token 'A #f '+)
	    (make-lexical-token 'N #f 2)
	    (make-lexical-token 'A #f '+)
	    (make-lexical-token 'N #f 3)
	    (make-lexical-token 'A #f '+)
	    (make-lexical-token 'N #f 4))
    => '(+ (+ (+ 1 2) 3) 4))

  (check	;left associative
      (doit (make-lexical-token 'N #f 1)
	    (make-lexical-token 'A #f '+)
	    (make-lexical-token 'N #f 2)
	    (make-lexical-token 'A #f '+)
	    (make-lexical-token 'N #f 3)
	    (make-lexical-token 'A #f '+)
	    (make-lexical-token 'N #f 4)
	    (make-lexical-token 'A #f '+)
	    (make-lexical-token 'N #f 5)
	    (make-lexical-token 'A #f '+)
	    (make-lexical-token 'N #f 6))
    => '(+ (+ (+ (+ (+ 1 2) 3) 4) 5) 6))

  (check	;right associative
      (doit (make-lexical-token 'N #f 1)
	    (make-lexical-token 'M #f '*)
	    (make-lexical-token 'N #f 2)
	    (make-lexical-token 'M #f '*)
	    (make-lexical-token 'N #f 3)
	    (make-lexical-token 'M #f '*)
	    (make-lexical-token 'N #f 4))
    => '(* 1 (* 2 (* 3 4))))

  (check	;right associative
      (doit (make-lexical-token 'N #f 1)
	    (make-lexical-token 'M #f '*)
	    (make-lexical-token 'N #f 2)
	    (make-lexical-token 'M #f '*)
	    (make-lexical-token 'N #f 3)
	    (make-lexical-token 'M #f '*)
	    (make-lexical-token 'N #f 4)
	    (make-lexical-token 'M #f '*)
	    (make-lexical-token 'N #f 5)
	    (make-lexical-token 'M #f '*)
	    (make-lexical-token 'N #f 6))
    => '(* 1 (* 2 (* 3 (* 4 (* 5 6))))))

;;; --------------------------------------------------------------------
;;; Non associativity.

  (check
      (doit (make-lexical-token 'N #f 1)
	    (make-lexical-token 'A #f '+)
	    (make-lexical-token 'N #f 2)
	    (make-lexical-token 'A #f '+)
	    (make-lexical-token 'A #f '+)
	    (make-lexical-token 'N #f 3))
    => '(+ (+ 1 2) (+ 3)))

  (check
      (doit (make-lexical-token 'N #f 1)
	    (make-lexical-token 'A #f '+)
	    (make-lexical-token 'N #f 2)
	    (make-lexical-token 'A #f '+)
	    (make-lexical-token 'B #f '-)
	    (make-lexical-token 'N #f 3))
    => '(+ (+ 1 2) (- 3)))

  (check
      (doit (make-lexical-token 'N #f 1)
	    (make-lexical-token 'A #f '+)
	    (make-lexical-token 'N #f 2)
	    (make-lexical-token 'B #f '-)
	    (make-lexical-token 'A #f '+)
	    (make-lexical-token 'N #f 3))
    => '(- (+ 1 2) (+ 3)))

  (check
      (doit (make-lexical-token 'N #f 1)
	    (make-lexical-token 'A #f '+)
	    (make-lexical-token 'N #f 2)
	    (make-lexical-token 'B #f '-)
	    (make-lexical-token 'B #f '-)
	    (make-lexical-token 'N #f 3))
    => '(- (+ 1 2) (- 3)))

  #t)


(parameterise ((check-test-name 'associativity-2))

;;;Fully working test of associativity.

  (define make-parser
    (lalr-parser
     :output-value #t :expect 0
     :terminals '(NUM (left:     ADD SUB)
		      (right:    MUL DIV)
		      (nonassoc: UNARY))

     :rules '((EXPR (EXPR ADD EXPR)          : (list $2 $1 $3)
		    (EXPR SUB EXPR)          : (list $2 $1 $3)
		    (EXPR MUL EXPR)          : (list $2 $1 $3)
		    (EXPR DIV EXPR)          : (list $2 $1 $3)
		    (NUM)                    : $1
		    (ADD NUM (prec: UNARY))  : (list $1 $2)
		    (SUB NUM (prec: UNARY))  : (list $1 $2)))))

  (define (doit . tokens)
    (let* ((lexer		(make-lexer tokens))
	   (error-handler	(make-error-handler (lambda x x)))
           (parser		(make-parser)))
      (parser lexer error-handler)))

;;; --------------------------------------------------------------------
;;; Grammar tests, no associativity.

  (check
      (doit (make-lexical-token 'NUM #f 1))
    => 1)

  (check
      (doit (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'NUM #f 1))
    => '(+ 1))

  (check
      (doit (make-lexical-token 'SUB #f '-)
	    (make-lexical-token 'NUM #f 1))
    => '(- 1))

  (check
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'NUM #f 2))
    => '(+ 1 2))

  (check
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'SUB #f '-)
	    (make-lexical-token 'NUM #f 2))
    => '(- 1 2))

  (check
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'MUL #f '*)
	    (make-lexical-token 'NUM #f 2))
    => '(* 1 2))

  (check
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'DIV #f '/)
	    (make-lexical-token 'NUM #f 2))
    => '(/ 1 2))

;;; --------------------------------------------------------------------
;;; Operator precedence.

  (check
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'SUB #f '-)
	    (make-lexical-token 'NUM #f 3))
    => '(- (+ 1 2) 3))

  (check
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'MUL #f '*)
	    (make-lexical-token 'NUM #f 3))
    => '(+ 1 (* 2 3)))

  (check
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'DIV #f '/)
	    (make-lexical-token 'NUM #f 3))
    => '(+ 1 (/ 2 3)))

;;; --------------------------------------------------------------------

  (check
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'SUB #f '-)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'MUL #f '+)
	    (make-lexical-token 'NUM #f 3))
    => '(- 1 (+ 2 3)))

  (check
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'SUB #f '-)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'MUL #f '*)
	    (make-lexical-token 'NUM #f 3))
    => '(- 1 (* 2 3)))

  (check
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'SUB #f '-)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'DIV #f '/)
	    (make-lexical-token 'NUM #f 3))
    => '(- 1 (/ 2 3)))

;;; --------------------------------------------------------------------

  (check
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'MUL #f '*)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'NUM #f 3))
    => '(+ (* 1 2) 3))

  (check
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'MUL #f '*)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'SUB #f '-)
	    (make-lexical-token 'NUM #f 3))
    => '(- (* 1 2) 3))

  (check
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'MUL #f '*)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'ADD #f '/)
	    (make-lexical-token 'NUM #f 3))
    => '(/ (* 1 2) 3))

;;; --------------------------------------------------------------------

  (check
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'DIV #f '/)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'NUM #f 3))
    => '(+ (/ 1 2) 3))

  (check
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'DIV #f '/)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'SUB #f '-)
	    (make-lexical-token 'NUM #f 3))
    => '(- (/ 1 2) 3))

  (check
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'DIV #f '/)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'SUB #f '*)
	    (make-lexical-token 'NUM #f 3))
    => '(* (/ 1 2) 3))

;;; --------------------------------------------------------------------
;;; Associativity.

  (check	;left associative
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'NUM #f 3))
    => '(+ (+ 1 2) 3))

  (check	;left associative
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'SUB #f '-)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'SUB #f '-)
	    (make-lexical-token 'NUM #f 3))
    => '(- (- 1 2) 3))

  (check	;right associative
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'MUL #f '*)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'MUL #f '*)
	    (make-lexical-token 'NUM #f 3))
    => '(* 1 (* 2 3)))

  (check	;right associative
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'DIV #f '/)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'DIV #f '/)
	    (make-lexical-token 'NUM #f 3))
    => '(/ 1 (/ 2 3)))

  (check	;left associative
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'NUM #f 3)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'NUM #f 4))
    => '(+ (+ (+ 1 2) 3) 4))

  (check	;left associative
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'NUM #f 3)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'NUM #f 4)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'NUM #f 5)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'NUM #f 6))
    => '(+ (+ (+ (+ (+ 1 2) 3) 4) 5) 6))

  (check	;right associative
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'MUL #f '*)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'MUL #f '*)
	    (make-lexical-token 'NUM #f 3)
	    (make-lexical-token 'MUL #f '*)
	    (make-lexical-token 'NUM #f 4))
    => '(* 1 (* 2 (* 3 4))))

  (check	;right associative
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'MUL #f '*)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'MUL #f '*)
	    (make-lexical-token 'NUM #f 3)
	    (make-lexical-token 'MUL #f '*)
	    (make-lexical-token 'NUM #f 4)
	    (make-lexical-token 'MUL #f '*)
	    (make-lexical-token 'NUM #f 5)
	    (make-lexical-token 'MUL #f '*)
	    (make-lexical-token 'NUM #f 6))
    => '(* 1 (* 2 (* 3 (* 4 (* 5 6))))))

;;; --------------------------------------------------------------------
;;; Non associativity.

  (check
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'NUM #f 3))
    => '(+ (+ 1 2) (+ 3)))

  (check
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'SUB #f '-)
	    (make-lexical-token 'NUM #f 3))
    => '(+ (+ 1 2) (- 3)))

  (check
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'SUB #f '-)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'NUM #f 3))
    => '(- (+ 1 2) (+ 3)))

  (check
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'SUB #f '-)
	    (make-lexical-token 'SUB #f '-)
	    (make-lexical-token 'NUM #f 3))
    => '(- (+ 1 2) (- 3)))

  #t)


#;(parameterise ((check-test-name 'associativity-3))

  (define make-parser
    (lalr-parser
     :output-value #t :expect 0
     :terminals '(NUM ADD SUB MUL DIV)

     :rules '((EXPR (EXPR ADD EXPR)          : (list $2 $1 $3)
;		    (EXPR ADD EXPR ADD EXPR) : (list $1 $2 $3 $4 $5)

		    (EXPR SUB EXPR)          : (list $2 $1 $3)
;		    (EXPR SUB EXPR SUB EXPR) : (list $1 $2 $3 $4 $5)

		    (EXPR MUL EXPR)          : (list $2 $1 $3)
;		    (EXPR MUL EXPR MUL EXPR) : (list $1 $2 $3 $4 $5)

		    (EXPR DIV EXPR)          : (list $2 $1 $3)
;		    (EXPR DIV EXPR DIV EXPR) : (list $1 $2 $3 $4 $5)

		    (NUM)                    : $1
		    (ADD NUM (prec: UNARY))  : (list $1 $2)
		    (SUB NUM (prec: UNARY))  : (list $1 $2)))))

  (define (doit . tokens)
    (let* ((lexer		(make-lexer tokens))
	   (error-handler	(make-error-handler (lambda x x)))
           (parser		(make-parser)))
      (parser lexer error-handler)))

;;; --------------------------------------------------------------------
;;; Grammar tests, no associativity.

  (check
      (doit (make-lexical-token 'NUM #f 1))
    => 1)

  (check
      (doit (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'NUM #f 1))
    => '(+ 1))

  (check
      (doit (make-lexical-token 'SUB #f '-)
	    (make-lexical-token 'NUM #f 1))
    => '(- 1))

  (check
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'NUM #f 2))
    => '(+ 1 2))

  (check
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'SUB #f '-)
	    (make-lexical-token 'NUM #f 2))
    => '(- 1 2))

  (check
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'MUL #f '*)
	    (make-lexical-token 'NUM #f 2))
    => '(* 1 2))

  (check
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'DIV #f '/)
	    (make-lexical-token 'NUM #f 2))
    => '(/ 1 2))

;;; --------------------------------------------------------------------
;;; Operator precedence.

  (check
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'SUB #f '-)
	    (make-lexical-token 'NUM #f 3))
    => '(- (+ 1 2) 3))

  (check
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'MUL #f '*)
	    (make-lexical-token 'NUM #f 3))
    => '(+ 1 (* 2 3)))

  (check
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'DIV #f '/)
	    (make-lexical-token 'NUM #f 3))
    => '(+ 1 (/ 2 3)))

;;; --------------------------------------------------------------------

  (check
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'SUB #f '-)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'MUL #f '+)
	    (make-lexical-token 'NUM #f 3))
    => '(- 1 (+ 2 3)))

  (check
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'SUB #f '-)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'MUL #f '*)
	    (make-lexical-token 'NUM #f 3))
    => '(- 1 (* 2 3)))

  (check
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'SUB #f '-)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'DIV #f '/)
	    (make-lexical-token 'NUM #f 3))
    => '(- 1 (/ 2 3)))

;;; --------------------------------------------------------------------

  (check
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'MUL #f '*)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'NUM #f 3))
    => '(+ (* 1 2) 3))

  (check
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'MUL #f '*)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'SUB #f '-)
	    (make-lexical-token 'NUM #f 3))
    => '(- (* 1 2) 3))

  (check
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'MUL #f '*)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'ADD #f '/)
	    (make-lexical-token 'NUM #f 3))
    => '(/ (* 1 2) 3))

;;; --------------------------------------------------------------------

  (check
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'DIV #f '/)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'NUM #f 3))
    => '(+ (/ 1 2) 3))

  (check
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'DIV #f '/)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'SUB #f '-)
	    (make-lexical-token 'NUM #f 3))
    => '(- (/ 1 2) 3))

  (check
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'DIV #f '/)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'SUB #f '*)
	    (make-lexical-token 'NUM #f 3))
    => '(* (/ 1 2) 3))

;;; --------------------------------------------------------------------
;;; Associativity.

  (check	;left associative
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'NUM #f 3))
    => '(+ (+ 1 2) 3))

  (check	;left associative
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'SUB #f '-)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'SUB #f '-)
	    (make-lexical-token 'NUM #f 3))
    => '(- (- 1 2) 3))

  (check	;right associative
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'MUL #f '*)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'MUL #f '*)
	    (make-lexical-token 'NUM #f 3))
    => '(* 1 (* 2 3)))

  (check	;right associative
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'DIV #f '/)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'DIV #f '/)
	    (make-lexical-token 'NUM #f 3))
    => '(/ 1 (/ 2 3)))

  (check	;left associative
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'NUM #f 3)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'NUM #f 4))
    => '(+ (+ (+ 1 2) 3) 4))

  (check	;left associative
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'NUM #f 3)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'NUM #f 4)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'NUM #f 5)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'NUM #f 6))
    => '(+ (+ (+ (+ (+ 1 2) 3) 4) 5) 6))

  (check	;right associative
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'MUL #f '*)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'MUL #f '*)
	    (make-lexical-token 'NUM #f 3)
	    (make-lexical-token 'MUL #f '*)
	    (make-lexical-token 'NUM #f 4))
    => '(* 1 (* 2 (* 3 4))))

  (check	;right associative
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'MUL #f '*)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'MUL #f '*)
	    (make-lexical-token 'NUM #f 3)
	    (make-lexical-token 'MUL #f '*)
	    (make-lexical-token 'NUM #f 4)
	    (make-lexical-token 'MUL #f '*)
	    (make-lexical-token 'NUM #f 5)
	    (make-lexical-token 'MUL #f '*)
	    (make-lexical-token 'NUM #f 6))
    => '(* 1 (* 2 (* 3 (* 4 (* 5 6))))))

;;; --------------------------------------------------------------------
;;; Non associativity.

  (check
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'NUM #f 3))
    => '(+ (+ 1 2) (+ 3)))

  (check
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'SUB #f '-)
	    (make-lexical-token 'NUM #f 3))
    => '(+ (+ 1 2) (- 3)))

  (check
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'SUB #f '-)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'NUM #f 3))
    => '(- (+ 1 2) (+ 3)))

  (check
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token 'ADD #f '+)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token 'SUB #f '-)
	    (make-lexical-token 'SUB #f '-)
	    (make-lexical-token 'NUM #f 3))
    => '(- (+ 1 2) (- 3)))

  #t)


(parameterise ((check-test-name 'script-expression))

  ;;This is the grammar of the (lalr) documentation in Texinfo format.

  (define terminals
    '(N O C T (left: A) (left: M) (nonassoc: U)))

  (define non-terminals
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
		 :terminals terminals
		 :rules non-terminals))

  (define (doit . tokens)
    (let* ((lexer		(make-lexer tokens))
           (result		'())
           (yycustom		(lambda (value)
                                  (set! result (cons value result))
				  'yycustom))
	   (error-handler	(make-error-handler yycustom))
           (parser		(make-parser)))
      (parser lexer error-handler yycustom)
      result))

  (debug:print-tables #f terminals non-terminals)

  (check	;correct input
      (doit (make-lexical-token 'T #f #\newline))
    => '(#\newline))

  (check	;correct input
      (doit (make-lexical-token 'N #f 1)
	    (make-lexical-token 'T #f #\newline))
    => '(1))

  (check	;correct input
      (doit (make-lexical-token 'N #f 1)
	    (make-lexical-token 'A #f +)
	    (make-lexical-token 'N #f 2)
	    (make-lexical-token 'T #f #\newline))
    => '(3))

  (check	;correct input
      (doit (make-lexical-token 'N #f 1)
	    (make-lexical-token 'A #f +)
	    (make-lexical-token 'N #f 2)
	    (make-lexical-token 'M #f *)
	    (make-lexical-token 'N #f 3)
	    (make-lexical-token 'T #f #\newline))
    => '(7))

  (check	;correct input
      (doit (make-lexical-token 'O #f #\()
	    (make-lexical-token 'N #f 1)
	    (make-lexical-token 'A #f +)
	    (make-lexical-token 'N #f 2)
	    (make-lexical-token 'C #f #\))
	    (make-lexical-token 'M #f *)
	    (make-lexical-token 'N #f 3)
	    (make-lexical-token 'T #f #\newline))
    => '(9))

  (check	;correct input
      (doit (make-lexical-token 'O #f #\()
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
	    (make-lexical-token 'T #f #\newline))
    => '(4/5 9))

  #t)


;;;; done

(check-report)

;;; end of file
