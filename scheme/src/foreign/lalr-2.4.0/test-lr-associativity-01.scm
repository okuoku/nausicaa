;;; test-lr-associativity-01.scm --
;;;

(load "common-test.scm")

(define (doit . tokens)
  (let ((parser (lalr-parser
		 (expect: 0)
		 (N (left: A)
		    (right: M)
		    (nonassoc: U))
		 (E	(N)		: $1
			(E A E)         : (list $1 '+ $3)
			(E M E)         : (list $1 '* $3)
			(A E (prec: U))	: (list '- $2)))))
    (parser (make-lexer tokens) error-handler)))

;;; --------------------------------------------------------------------
;;; Single operator.

(check
    (doit (make-lexical-token 'N #f 1))
  => 1)

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
;;; Precedence.

(check
    (doit (make-lexical-token 'N #f 1)
	  (make-lexical-token 'A #f '+)
	  (make-lexical-token 'N #f 2)
	  (make-lexical-token 'M #f '*)
	  (make-lexical-token 'N #f 3))
  => '(1 + (2 * 3)))

(check
    (doit (make-lexical-token 'N #f 1)
	  (make-lexical-token 'M #f '*)
	  (make-lexical-token 'N #f 2)
	  (make-lexical-token 'A #f '+)
	  (make-lexical-token 'N #f 3))
  => '((1 * 2) + 3))

;;; --------------------------------------------------------------------
;;; Associativity.

(check
    (doit (make-lexical-token 'N #f 1)
	  (make-lexical-token 'A #f '+)
	  (make-lexical-token 'N #f 2)
	  (make-lexical-token 'A #f '+)
	  (make-lexical-token 'N #f 3))
  => '((1 + 2) + 3))

(check
    (doit (make-lexical-token 'N #f 1)
	  (make-lexical-token 'M #f '*)
	  (make-lexical-token 'N #f 2)
	  (make-lexical-token 'M #f '*)
	  (make-lexical-token 'N #f 3))
  => '(1 * (2 * 3)))

;;; end of file
