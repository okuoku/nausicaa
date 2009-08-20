;;; test-lr-basics-01.scm --
;;;

(load "common-test.scm")

(define (doit . tokens)
  ;;A grammar that only accept a single terminal as input.
  (let* ((lexer		(make-lexer tokens))
	 (parser	(lalr-parser (expect: 0)
				     (A)
				     (e (A) : $1))))
    (parser lexer error-handler)))

(check
    (doit (make-lexical-token 'A #f 1))
  => 1)

(check
    (doit)
  => #f)

(check
    ;;Parse correctly the first A  and reduce it.  The second A triggers
    ;;an  error which  empties  the  stack and  consumes  all the  input
    ;;tokens.  Finally, an unexpected end-of-input error is returned.
    (doit (make-lexical-token 'A #f 1)
	  (make-lexical-token 'A #f 2)
	  (make-lexical-token 'A #f 3))
  => #f)

;;; end of file
