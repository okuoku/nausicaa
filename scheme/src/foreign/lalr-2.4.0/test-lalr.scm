;;; proofs.scm --
;;;

(load "lalr.scm")

(define (display-result v)
  (if v
      (begin
        (display "==> ")
        (display v)
        (newline))))

(define eoi-token
  (make-lexical-token '*eoi* #f #f))


;;;; LR driver, associativity tests

(if #t
    (begin
      (define (doit . tokens)
	(define calc-parser
	  (lalr-parser
;;;	   (output: assoc "proof-associativity.yy.scm")
	   (expect: 0)

	   (N (left: A) (right: M) (nonassoc: U))

	   (E		(N)		: $1
			(E A E)         : (list $1 '+ $3)
			(E M E)         : (list $1 '* $3)
			(A E (prec: U))	: (list 'U $2))))

	(let* ((lexer		(lambda ()
				  (if (null? tokens)
				      eoi-token
				    (let ((t (car tokens)))
				      (set! tokens (cdr tokens))
				      t))))
	       (error-handler	(lambda args #f)))
	  (display-result (calc-parser lexer error-handler))))

;;       (doit (make-lexical-token 'N #f 1)
;; 	    (make-lexical-token 'A   #f 'A)
;; 	    (make-lexical-token 'N #f 2)
;; 	    eoi-token)

;;       ;;left-associativity
;;       (doit (make-lexical-token 'N #f 1)
;; 	    (make-lexical-token 'A   #f 'A)
;; 	    (make-lexical-token 'N #f 2)
;; 	    (make-lexical-token 'A   #f 'A)
;; 	    (make-lexical-token 'N #f 3)
;; 	    eoi-token)

      ;;right-associativity
      (doit (make-lexical-token 'N #f 1)
	    (make-lexical-token 'M #f '*)
	    (make-lexical-token 'N #f 2)
	    (make-lexical-token 'M #f '*)
	    (make-lexical-token 'N #f 3)
	    eoi-token)

      ;;precedence
      (doit (make-lexical-token 'N #f 1)
	    (make-lexical-token 'A #f '+)
	    (make-lexical-token 'N #f 2)
	    (make-lexical-token 'M #f '*)
	    (make-lexical-token 'N #f 3)
	    eoi-token)

      ))


;;;; GLR driver, associativity tests

(if #f
    (begin
      (define (doit . tokens)
	(define calc-parser
	  (lalr-parser
	   (driver: glr)
	   (expect: 0)

	   (NUM LPAREN RPAREN
		(left: + -)
		(right: * /)
		(nonassoc: uminus))

	   (output	(expr)			: (display-result $1))

	   (expr	(expr + expr)           : (list $1 '+ $3)
			(expr - expr)           : (list $1 '- $3)
			(expr * expr)           : (list $1 '* $3)
			(expr / expr)           : (list $1 '/ $3)
			(- expr (prec: uminus)) : (list '- $2)
			(NUM)                   : $1
			(LPAREN expr RPAREN)    : $2)))

	(let* ((lexer		(lambda ()
				  (if (null? tokens)
				      eoi-token
				    (let ((t (car tokens)))
				      (set! tokens (cdr tokens))
				      t))))
	       (error-handler	(lambda args #f)))
	  (calc-parser lexer error-handler)))

      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token '+   #f '+)
	    (make-lexical-token 'NUM #f 2)
	    eoi-token)

      ;;left-associativity
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token '+   #f '+)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token '+   #f '+)
	    (make-lexical-token 'NUM #f 3)
	    eoi-token)

      ;;right-associativity
      (doit (make-lexical-token 'NUM #f 1)
	    (make-lexical-token '*   #f '*)
	    (make-lexical-token 'NUM #f 2)
	    (make-lexical-token '*   #f '*)
	    (make-lexical-token 'NUM #f 3)
	    eoi-token)

      ;;precedence
      (doit (make-lexical-token 'N #f 1)
	    (make-lexical-token 'A #f 'A)
	    (make-lexical-token 'N #f 2)
	    (make-lexical-token 'M #f 'M)
	    (make-lexical-token 'N #f 3)
	    eoi-token)

      ))

;;; end of file
