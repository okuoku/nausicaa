;;; test-lr-associativity.scm --
;;;

(load "common-test.scm")

(define make-parser
  (lalr-parser
   (expect: 0)
   (N (left: A) (right: M) (nonassoc: U))
   (E	(N)		: $1
	(E A E)         : (list $1 '+ $3)
	(E M E)         : (list $1 '* $3)
	(A E (prec: U))	: (list 'U $2))))

(define (doit . tokens)
  (make-parser (make-lexer tokens) error-handler))

;;; --------------------------------------------------------------------

(check
    (doit (make-lexical-token 'N #f 1)
	  (make-lexical-token 'A #f '+)
	  (make-lexical-token 'N #f 2))
  => '(1 + 2))

(check
    (doit (make-lexical-token 'N #f 1)
	  (make-lexical-token 'M #f '*)
	  (make-lexical-token 'N #f 2))
  => '(1 * 2))

(check
    (doit (make-lexical-token 'A #f '-)
	  (make-lexical-token 'N #f 1))
  => '(- 1))

;;; --------------------------------------------------------------------

(check
    ;;left-associativity
    (doit (make-lexical-token 'N #f 1)
	  (make-lexical-token 'A #f 'A)
	  (make-lexical-token 'N #f 2)
	  (make-lexical-token 'A #f 'A)
	  (make-lexical-token 'N #f 3))
  => '((1 A 2) A 3))

(check
    ;;right-associativity
    (doit (make-lexical-token 'N #f 1)
	  (make-lexical-token 'M #f '*)
	  (make-lexical-token 'N #f 2)
	  (make-lexical-token 'M #f '*)
	  (make-lexical-token 'N #f 3))
  => '(1 * (2 * 3)))

(check
    ;;precedence
    (doit (make-lexical-token 'N #f 1)
	  (make-lexical-token 'A #f '+)
	  (make-lexical-token 'N #f 2)
	  (make-lexical-token 'M #f '*)
	  (make-lexical-token 'N #f 3))
  => '(1 + (2 * 3)))

(check
    ;;precedence
    (doit (make-lexical-token 'N #f 1)
	  (make-lexical-token 'M #f '*)
	  (make-lexical-token 'N #f 2)
	  (make-lexical-token 'A #f '+)
	  (make-lexical-token 'N #f 3))
  => '(1 * (2 + 3)))

;;; end of file
