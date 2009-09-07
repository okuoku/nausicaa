;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for packrat parser
;;;Date: Sun Sep  6, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
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


(import (nausicaa)
  (checks)
  (packrat))

(check-set-mode! 'report-failed)
(display "*** testing packrat\n")


(parametrise ((check-test-name 'calc-1))

  (define (make-lexer-closure tokens)
    (let ((stream tokens))
      (lambda ()
	(if (null? stream)
	    (make-lexical-token '*eoi* #f (eof-object))
	  (let ((token (car stream)))
	    (set! stream (cdr stream))
	    (make-lexical-token (car token) #f (cdr token)))))))

  (define calc
    (packrat-parser expr

		    (expr   ((a <- mul-expr '+ b <- expr)
			     (+ a b))
			    ((a <- mul-expr '- b <- expr)
			     (- a b))
			    ((a <- mul-expr)
			     a))

		    (mul-expr ((a <- simple '* b <- simple)
			     (* a b))
			    ((a <- simple '/ b <- simple)
			     (/ a b))
			    ((a <- simple)
			     a))

		    (simple ((a <- 'NUM)
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

  (define (doit . tokens)
    (let* ((lexer	(make-lexer-closure tokens))
	   (result	(calc (base-generator->results lexer))))
      (if (parse-result-successful? result)
	  (parse-result-semantic-value result)
	(parse-result-error result))))

;;; --------------------------------------------------------------------

  (check 'this
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

  #t)


(parametrise ((check-test-name 'calc-2))

  (define (sexp->tokens sexp)
    (define (lexer sexp)
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
			 (append '((CLOSE-PAREN)) (lexer atom) '((OPEN-PAREN)) result))
			(else
			 (error #f "invalid token" atom))))))))
    (reverse (lexer sexp)))

  (define (make-lexer-closure sexp)
    (let ((stream (sexp->tokens sexp)))
      (lambda ()
	(if (null? stream)
	    (make-lexical-token '*eoi* #f (eof-object))
	  (let ((token (car stream)))
	    (set! stream (cdr stream))
	    (make-lexical-token (car token) #f (cdr token)))))))

  (define calc
    (packrat-parser expr

		    (expr   ((a <- mul-expr '+ b <- expr)
			     (+ a b))
			    ((a <- mul-expr '- b <- expr)
			     (- a b))
			    ((a <- mul-expr)
			     a))

		    (mul-expr ((a <- simple '* b <- simple)
			     (* a b))
			    ((a <- simple '/ b <- simple)
			     (/ a b))
			    ((a <- simple)
			     a))

		    (simple ((a <- 'NUMBER)
			     a)
			    (('+ a <- expr)
			     a)
			    (('- a <- expr)
			     (- a))
			    (('OPEN-PAREN a <- expr 'CLOSE-PAREN)
			     a))))

  (define (doit sexp)
    (let* ((lexer	(make-lexer-closure sexp))
	   (result	(calc (base-generator->results lexer))))
      (if (parse-result-successful? result)
	  (parse-result-semantic-value result)
	(parse-result-error result))))

;;; --------------------------------------------------------------------

  (check
      (sexp->tokens '(1 + 2 + 3))
    => '((NUMBER . 1)
	 (+)
	 (NUMBER . 2)
	 (+)
	 (NUMBER . 3)))

  (check
      (sexp->tokens '((1 + 2) * 3))
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
      (parse-error? (doit '(+ *)))
    => #t)

  #t)



;; (define (x)
;;   (sc-expand
;;    '(packrat-parser expr
;; 		    (expr ((a <- mul-expr '+ b <- mul-expr)
;; 			   (+ a b))
;; 			  ((a <- mul-expr) a))
;; 		    (mul-expr ((a <- simple '* b <- simple)
;; 			     (* a b))
;; 			    ((a <- simple) a))
;; 		    (simple ((a <- 'num) a)
;; 			    (('oparen a <- expr 'cparen) a)))))

;; (let ((p ((packrat-parse `((expr (/ (a <- mul-expr '+ b <- mul-expr ,(packrat-lambda (a b) (+ a b)))
;; 				    mul-expr))
;; 			   (mul-expr (/ (a <- simple '* b <- simple ,(packrat-lambda (a b) (* a b)))
;; 				      simple))
;; 			   (simple (/ 'num
;; 				      ('oparen a <- expr 'cparen ,(packrat-lambda (a) a))))))
;; 	  'expr)))
;;   (try-packrat-parse-pattern
;;    p '()
;;    (packrat-list-results '((oparen) (num . 1) (+) (num . 2) (cparen) (*) (num . 3)))
;;    (lambda (bindings result) (values bindings (parse-result-semantic-value result)))
;;    (lambda (err)
;;      (list 'parse-error
;; 	   (parse-position->string (parse-error-position err))
;; 	   (parse-error-expected err)
;; 	   (parse-error-messages err)))))

;; (define expr-parse
;;   (let ((p ((packrat-parse `((toplevel (e <- expr #f ,(packrat-lambda (e) e)))
;; 			     (expr (/ (a <- mul-expr "+"ws b <- expr
;; 					 ,(packrat-lambda (a b) (+ a b)))
;; 				      mul-expr))
;; 			     (mul-expr (/ (a <- simple "*"ws b <- mul-expr
;; 					   ,(packrat-lambda (a b) (* a b)))
;; 					simple))
;; 			     (simple (/ num
;; 					("("ws a <- expr ")"ws
;; 					 ,(packrat-lambda (a) a))))
;; 			     (num ((d <- digit)+ ws
;; 				   ,(packrat-lambda (d) (string->number (list->string d)))))
;; 			     (ws (#\ *))
;; 			     (digit (/: "0123456789"))))
;; 	    'toplevel)))
;;     (lambda (str)
;;       (try-packrat-parse-pattern
;;        p '()
;;        (packrat-string-results "<str>" str)
;;        (lambda (bindings result)
;; 	 (values bindings (parse-result-semantic-value result)))
;;        (lambda (err)
;; 	 (list 'parse-error
;; 	       (parse-position->string (parse-error-position err))
;; 	       (parse-error-expected err)
;; 	       (parse-error-messages err)))))))


;;;; done

(check-report)

;;; end of file
