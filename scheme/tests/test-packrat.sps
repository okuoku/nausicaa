;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for packrat parser
;;;Date: Sun Sep  6, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (c) 2004, 2005 Tony Garnock-Jones <tonyg@kcbbs.gen.nz>
;;;Copyright (c) 2005 LShift Ltd. <query@lshift.net>
;;;
;;;Permission is hereby granted, free of charge, to any person obtaining
;;;a  copy of  this  software and  associated  documentation files  (the
;;;"Software"), to  deal in the Software  without restriction, including
;;;without limitation  the rights to use, copy,  modify, merge, publish,
;;;distribute, sublicense,  and/or sell copies  of the Software,  and to
;;;permit persons to whom the Software is furnished to do so, subject to
;;;the following conditions:
;;;
;;;The  above  copyright notice  and  this  permission  notice shall  be
;;;included in all copies or substantial portions of the Software.
;;;
;;;THE  SOFTWARE IS  PROVIDED "AS  IS",  WITHOUT WARRANTY  OF ANY  KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT. IN  NO EVENT SHALL THE AUTHORS  OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY,  WHETHER IN AN
;;;ACTION OF  CONTRACT, TORT  OR OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION  WITH THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.
;;;


#!r6rs
(import (nausicaa)
  (nausicaa checks)
  (prefix (nausicaa packrat) packrat:)
  (nausicaa parser-tools lexical-token))

(check-set-mode! 'report-failed)
(display "*** testing packrat\n")


;;;; helpers

(define (make-lexer-closure stream)
  ;;Return a lexer  closure drawing tokens from STREAM,  which must be a
  ;;list of terminal/value  pairs.  Return <lexical-token> instances and
  ;;automatically  generate  the end-of-input  token  when  the list  is
  ;;empty.
  ;;
  (lambda ()
    (if (null? stream)
	(make-<lexical-token> '*eoi* #f (eof-object) 0)
      (let ((token (car stream)))
	(set! stream (cdr stream))
	(make-<lexical-token> (car token) #f (cdr token) 0)))))


(parametrise ((check-test-name 'terminal))

  (define (doit start-combinator . tokens)
    (let* ((lexer	(make-lexer-closure tokens))
	   (state	(packrat:initialise-state lexer))
	   (result	(start-combinator state)))
      (if (packrat:<success>? result)
	  (packrat:<success>-value result)
	(packrat:<error>-message result))))

;;; --------------------------------------------------------------------

  (let ((comb (packrat:make-terminal-combinator
	       'A
	       (lambda (semantic-value)
		 (lambda (state)
		   (packrat:make-<success> state semantic-value))))))

    (check
	(doit comb '(A . 123))
      => 123)

    (check
	(doit comb '(ERR . #f))
      => "expected token with category A")

    #f)

;;; --------------------------------------------------------------------

  (letrec ((a (packrat:make-terminal-combinator
	       'A
	       (lambda (semantic-value)
		 b)))
	   (b (packrat:make-terminal-combinator
	       'B
	       (lambda (semantic-value)
		 (lambda (state)
		   (packrat:make-<success> state semantic-value))))))

    (check
	(doit a
	      '(A . discarded)
	      '(B . 123))
      => 123)

    (check
	(doit a '(ERR . #f))
      => "expected token with category A")

    (check
	(doit a '(A . #f) '(ERR . #f))
      => "expected token with category B")

    #f)

;;; --------------------------------------------------------------------

  (let ((comb (packrat:make-terminal-combinator
	       'NUMBER
	       (lambda (semantic-value-a)
		 (packrat:make-terminal-combinator
		  'NUMBER
		  (lambda (semantic-value-b)
		    (lambda (state)
		      (packrat:make-<success> state
					      (+ semantic-value-a
						 semantic-value-b)))))))))

    (check
	(doit comb
	      '(NUMBER . 10)
	      '(NUMBER . 5))
      => 15)

    #f)

  #t)


(parametrise ((check-test-name 'sequence))

  (define (doit start-combinator . tokens)
    (let* ((lexer	(make-lexer-closure tokens))
	   (state	(packrat:initialise-state lexer))
	   (result	(start-combinator state)))
      (if (packrat:<success>? result)
	  (packrat:<success>-value result)
	(packrat:<error>-message result))))

;;; --------------------------------------------------------------------

  (let ()

    (define comb-a
      (packrat:make-terminal-combinator
       'A
       (lambda (semantic-value)
	 (lambda (state)
	   (packrat:make-<success> state semantic-value)))))

    (define comb-b
      (packrat:make-terminal-combinator
       'B
       (lambda (semantic-value)
	 (lambda (state)
	   (packrat:make-<success> state semantic-value)))))

    (define comb
      (packrat:make-sequence-combinator
       comb-a (lambda (semantic-value) comb-b)))

    (check
	(doit comb
	      '(A . discarded)
	      '(B . 123))
      => 123)

    (check
	(doit comb '(ERR . #f))
      => "expected token with category A")

    (check
	(doit comb '(A . #f) '(ERR . #f))
      => "expected token with category B")

    #f)

;;; --------------------------------------------------------------------

  (let ()

    (define comb
      (packrat:make-sequence-combinator
       (packrat:make-terminal-combinator
	'NUMBER
	(lambda (semantic-value)
	  (lambda (state)
	    (packrat:make-<success> state semantic-value))))
       (lambda (a)
	 (packrat:make-terminal-combinator
	  'NUMBER
	  (lambda (b)
	    (lambda (state)
	      (packrat:make-<success> state (+ a b))))))))

    (check
	(doit comb
	      '(NUMBER . 10)
	      '(NUMBER . 5))
      => 15)

    (check
	(doit comb '(ERR . #f))
      => "expected token with category NUMBER")

    (check
	(doit comb '(NUMBER . #f) '(ERR . #f))
      => "expected token with category NUMBER")

    #f)

  #f)


(parametrise ((check-test-name 'alternative))

  (define (doit start-combinator . tokens)
    (let* ((lexer	(make-lexer-closure tokens))
	   (state	(packrat:initialise-state lexer))
	   (result	(start-combinator state)))
      (if (packrat:<success>? result)
	  (packrat:<success>-value result)
	(packrat:<error>-message result))))

;;; --------------------------------------------------------------------

  (let ()

    (define comb-a
      (packrat:make-terminal-combinator
       'A
       (lambda (semantic-value)
	 (lambda (state)
	   (packrat:make-<success> state semantic-value)))))

    (define comb-b
      (packrat:make-terminal-combinator
       'B
       (lambda (semantic-value)
	 (lambda (state)
	   (packrat:make-<success> state semantic-value)))))

    (define comb
      (packrat:make-or-combinator comb-a comb-b))

    (check
	(doit comb '(A . 1))
      => 1)

    (check
	(doit comb '(B . 1))
      => 1)

    (check
	(doit comb '(ERR . #f))
      => "expected token with category B")


    #f)

;;; --------------------------------------------------------------------

  (let ()

    (define comb
      (packrat:make-or-combinator
       (packrat:make-terminal-combinator
	'A
	(lambda (semantic-value)
	  (lambda (state)
	    (packrat:make-<success> state semantic-value))))
       (lambda (state)
	(packrat:make-<error> state "it is not A, dammit!"))))

    (check
	(doit comb '(A . 1))
      => 1)

    (check
	(doit comb '(ERR . #f))
      => "it is not A, dammit!")

    #f)

;;; --------------------------------------------------------------------

  #;(let ()

    (define comb (packrat:make-or-combinator))

    (check
	(guard (E (else (condition-message E)))
	  (doit comb '(N . 1)))
      => "OR combinator with no alternatives")

    #f)

  #t)


(parametrise ((check-test-name 'unless))

  (define (doit start-combinator . tokens)
    (let* ((lexer	(make-lexer-closure tokens))
	   (state	(packrat:initialise-state lexer))
	   (result	(start-combinator state)))
      (if (packrat:<success>? result)
	  (packrat:<success>-value result)
	(packrat:<error>-message result))))

;;; --------------------------------------------------------------------

  (let ()

    (define comb-a
      (packrat:make-terminal-combinator
       'A
       (lambda (semantic-value)
	 (lambda (state)
	   (packrat:make-<success> state semantic-value)))))

    (define comb-b
      (packrat:make-terminal-combinator
       'B
       (lambda (semantic-value)
	 (lambda (state)
	   (packrat:make-<success> state semantic-value)))))

    (define comb
      (packrat:make-unless-combinator comb-a comb-b))

    (check
	(doit comb '(A . 1))
      => "failed negation rule")

    (check
	(doit comb '(B . 1))
      => 1)

    #f)

  #t)


(parametrise ((check-test-name 'memoize))

  (define (doit start-combinator . tokens)
    (let* ((lexer	(make-lexer-closure tokens))
	   (state	(packrat:initialise-state lexer))
	   (result-1	(start-combinator state))
	   (result-2	(start-combinator state))
	   (report	(lambda (result)
			  (if (packrat:<success>? result)
			      (packrat:<success>-value result)
			    (packrat:<error>-message result)))))
      (cons (report result-1)
	    (report result-2))))

;;; --------------------------------------------------------------------

  (let ()	;supplied keyword

    (define comb-a
      (packrat:make-terminal-combinator
       'A
       (lambda (semantic-value)
	 (lambda (state)
	   (packrat:make-<success> state semantic-value)))))

    (define comb
      (packrat:make-memoize-combinator comb-a 'this))

    (check
	(doit comb '(A . 1))
      => '(1 . 1))

    (check
	(doit comb '(B . 1))
      => '("expected token with category A" . "expected token with category A"))

    #f)

;;; --------------------------------------------------------------------

  (let () ;automaticall generated keyword

    (define comb-a
      (packrat:make-terminal-combinator
       'A
       (lambda (semantic-value)
	 (lambda (state)
	   (packrat:make-<success> state semantic-value)))))

    (define comb
      (packrat:make-memoize-combinator comb-a))

    (check
	(doit comb '(A . 1))
      => '(1 . 1))

    (check
	(doit comb '(B . 1))
      => '("expected token with category A" . "expected token with category A"))

    #f)

  #t)


(parametrise ((check-test-name 'calc-1))

;;;Arithmetic expressions parser with pair tokens input stream.

  (define (doit . stream)
    (let* ((lexer	(make-lexer-closure stream))
	   (result	(calc-combinator (packrat:initialise-state lexer))))
      (if (packrat:<success>? result)
	  (packrat:<success>-value result)
	(packrat:<error>-message result))))

  (define calc-combinator
    (packrat:make-grammar-combinator

     expr

     (expr	((a <- mul-expr '+ b <- expr)
		 (+ a b))
		((a <- mul-expr '- b <- expr)
		 (- a b))
		((a <- mul-expr)
		 a))

     (mul-expr	((a <- simple '* b <- simple)
		 (* a b))
		((a <- simple '/ b <- simple)
		 (/ a b))
		((a <- simple)
		 a))

     (simple	((a <- 'NUM)
		 a)
		(('+ a <- 'NUM)
		 a)
		(('+ a <- simple)
		 a)
		(('- a <- 'NUM)
		 (- a))
		(('- a <- simple)
		 (- a))
		(('OPAREN a <- expr 'CPAREN)
		 a))))

;;; --------------------------------------------------------------------

  (check
      (doit '(NUM . 1))
    => 1)

  (check
      (doit '(+)
	    '(NUM . 1))
    => 1)

  (check
      (doit '(-)
	    '(NUM . 1))
    => -1)

;;; --------------------------------------------------------------------

  (check
      (doit '(+)
	    '(+)
	    '(+)
	    '(NUM . 1))
    => 1)

  (check
      (doit '(-)
	    '(-)
	    '(NUM . 1))
    => 1)

  (check
      (doit '(-)
	    '(-)
	    '(-)
	    '(NUM . 1))
    => -1)

  (check
      (doit '(-)
	    '(-)
	    '(+)
	    '(+)
	    '(-)
	    '(+)
	    '(NUM . 1))
    => -1)

;;; --------------------------------------------------------------------

  (check
      (doit '(NUM . 1)
	    '(+)
	    '(NUM . 2))
    => 3)

  (check
      (doit '(NUM . 1)
	    '(-)
	    '(NUM . 2))
    => -1)

  (check
      (doit '(NUM . 2)
	    '(*)
	    '(NUM . 3))
    => 6)

  (check
      (doit '(NUM . 2)
	    '(/)
	    '(NUM . 3))
    => 2/3)

;;; --------------------------------------------------------------------

  (check
      (doit '(NUM . 1)
	    '(+)
	    '(NUM . 2)
	    '(+)
	    '(NUM . 3)
	    '(+)
	    '(NUM . 4))
    => 10)

  (check
      (doit '(-)
	    '(-)
	    '(NUM . 2)
	    '(+)
	    '(+)
	    '(-)
	    '(+)
	    '(NUM . 3))
    => -1)

;;; --------------------------------------------------------------------

  (check
      (doit '(NUM . 1)
	    '(+)
	    '(NUM . 2)
	    '(*)
	    '(NUM . 3))
    => 7)

  (check
      (doit '(NUM . 1)
	    '(+)
	    '(NUM . 2)
	    '(/)
	    '(NUM . 3))
    => (+ 1 (/ 2 3)))

  (check
      (doit '(NUM . 1)
	    '(-)
	    '(NUM . 2)
	    '(*)
	    '(NUM . 3))
    => (- 1 (* 2 3)))

  (check
      (doit '(NUM . 1)
	    '(-)
	    '(NUM . 2)
	    '(/)
	    '(NUM . 3))
    => (- 1 (/ 2 3)))

  (check
      (doit '(NUM . 1)
	    '(*)
	    '(NUM . 2)
	    '(-)
	    '(NUM . 3))
    => (- (* 1 2) 3))

  (check
      (doit '(NUM . 1)
	    '(/)
	    '(NUM . 2)
	    '(-)
	    '(NUM . 3))
    => (- (/ 1 2) 3))

;;; --------------------------------------------------------------------

  (check
      (doit '(OPAREN)
	    '(NUM . 1)
	    '(+)
	    '(NUM . 2)
	    '(CPAREN)
	    '(*)
	    '(NUM . 3))
    => 9)

  (check
      (doit '(+ *))
    => "expected token with category OPAREN")

  #t)


(parametrise ((check-test-name 'calc-2))

;;;Arithmetic expressions parser  with S-expression input stream.  Makes
;;;use of ":error" to generate error messages.

  (define (doit sexp)
    (let* ((lexer	(make-lexer-closure (sexp->stream sexp)))
	   (result	(calc-combinator (packrat:initialise-state lexer))))
      (if (packrat:<success>? result)
	  (packrat:<success>-value result)
	(packrat:<error>-message result))))

  (define (sexp->stream sexp)
    ;;Convert  an S-expression  in SEXP  into a  list  of terminal/value
    ;;pairs.  The S-expression must represent an arithmetics expression.
    ;;The generated tokens have terminal symbols:
    ;;
    ;;	NUMBER OPEN-PAREN CLOSE-PAREN + - * /
    ;;
    (define (pair-lexer sexp)
      (let loop ((sexp   (if (pair? sexp)
			     sexp
			   (list sexp)))
		 (result '()))
	(if (null? sexp)
	    result
	  (let ((atom (car sexp)))
	    (loop (cdr sexp)
		  (cond ((number? atom)
			 `((NUMBER . ,atom) . ,result))
			((memq atom '(+ - * /))
			 `((,atom) . ,result))
			((pair? atom)
			 (append '((CLOSE-PAREN)) (pair-lexer atom) '((OPEN-PAREN)) result))
			(else
			 (error #f "invalid token" atom))))))))
    (reverse (pair-lexer sexp)))

  (define calc-combinator
    (packrat:make-grammar-combinator
     expr

     (expr	((a <- mul-expr '+ b <- expr)
		 (+ a b))
		((a <- mul-expr '- b <- expr)
		 (- a b))
		((a <- mul-expr)
		 a)
		((:error "syntax error while parsing expression")))

     (mul-expr	((a <- simple '* b <- simple)
		 (* a b))
		((a <- simple '/ b <- simple)
		 (/ a b))
		((a <- simple)
		 a)
		((:error "syntax error while parsing mul/div expression")))

     (simple	((a <- 'NUMBER)
		 a)
		(('+ a <- expr)
		 a)
		(('- a <- expr)
		 (- a))
		(('OPEN-PAREN a <- expr 'CLOSE-PAREN)
		 a)
		((:error "syntax error while parsing simple expression")))))

;;; --------------------------------------------------------------------

  (check
      (sexp->stream '(1 + 2 + 3))
    => '((NUMBER . 1)
	 (+)
	 (NUMBER . 2)
	 (+)
	 (NUMBER . 3)))

  (check
      (sexp->stream '((1 + 2) * 3))
    => '((OPEN-PAREN)
	 (NUMBER . 1)
	 (+)
	 (NUMBER . 2)
	 (CLOSE-PAREN)
	 (*)
	 (NUMBER . 3)))

;;; --------------------------------------------------------------------

  (check
      (doit 1)
    => 1)

  (check
      (doit '(+ 1))
    => 1)

  (check
      (doit '(- 1))
    => -1)

;;; --------------------------------------------------------------------

  (check
      (doit '(+ + + 1))
    => 1)

  (check
      (doit '(- - 1))
    => 1)

  (check
      (doit '(- - - 1))
    => -1)

  (check
      (doit '(- - + +  - + 1))
    => -1)

;;; --------------------------------------------------------------------

  (check
      (doit '(1 + 2))
    => 3)

  (check
      (doit '(1 - 2))
    => -1)

  (check
      (doit '(2 * 3))
    => 6)

  (check
      (doit '(2 / 3))
    => 2/3)

;;; --------------------------------------------------------------------

  (check
      (doit '(1 + 2 + 3 + 4))
    => 10)

  (check
      (doit '(- - 2 + + - + 3))
    => -1)

;;; --------------------------------------------------------------------

  (check
      (doit '(1 + 2 * 3))
    => 7)

  (check
      (doit '(1 + 2 / 3))
    => (+ 1 (/ 2 3)))

  (check
      (doit '(1 - 2 * 3))
    => (- 1 (* 2 3)))

  (check
      (doit '(1 - 2 / 3))
    => (- 1 (/ 2 3)))

  (check
      (doit '(1 * 2 - 3))
    => (- (* 1 2) 3))

  (check
      (doit '(1 / 2 - 3))
    => (- (/ 1 2) 3))

;;; --------------------------------------------------------------------

  (check
      (doit '((1 + 2) * 3))
    => 9)

;;; --------------------------------------------------------------------

  (check
      (doit '(+ *))
    => "syntax error while parsing expression")

  #t)


(parametrise ((check-test-name 'left-recursion))

  (define (doit start-combinator . stream)
    (let* ((lexer	(make-lexer-closure stream))
	   (result	(start-combinator (packrat:initialise-state lexer))))
      (if (packrat:<success>? result)
	  (packrat:<success>-value result)
	(packrat:<error>-message result))))

;;; --------------------------------------------------------------------

  (let ()

    (define digits-comb
      (packrat:make-grammar-combinator digits
				       (digits	((a <- digits b <- digit)
						 (string-append a b))
						((d <- digit)
						 d))
				       (digit	((d <- 'NUM)
						 d))))

    (check
	(guard (E (else (condition-message E)))
	  (doit digits-comb '(NUM . "1")))
      => "left recursive combinator")

    #f)

;;; --------------------------------------------------------------------

  (let ()	;test indirect recursion limitation

    (define indirect-digits-comb
      (packrat:make-grammar-combinator digits
				       (digits	((a <- X b <- 'NUM)
						 (string-append a b))
						((d <- 'NUM)
						 d))
				       (X	((d <- digits)
						 d))))

    (check
	(guard (E (else (condition-message E)))
	  (doit indirect-digits-comb '(NUM . "1")))
      => "left recursive combinator")

    #f)

  #t)


;;;; done

(check-report)

;;; end of file
