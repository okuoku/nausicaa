;;; test-lr-basics-02.scm --
;;
;;A grammar that only accept a single terminal or the EOI.

(load "common-test.scm")

(define (doit . tokens)
  (let ((parser (lalr-parser (expect: 0)
			     (A)
			     (e (A) : $1
				()  : 0))))
    (parser (make-lexer tokens) error-handler)))

(check
    (doit)
  => 0)

(check
    (doit (make-lexical-token 'A #f 1))
  => 1)

(check
    ;;Parse correctly the first A  and reduce it.  The second A triggers
    ;;an  error which  empties  the  stack and  consumes  all the  input
    ;;tokens.  Finally, an unexpected end-of-input error is returned.
    (doit (make-lexical-token 'A #f 1)
	  (make-lexical-token 'A #f 2)
	  (make-lexical-token 'A #f 3))
  => #f)

;;; end of file
