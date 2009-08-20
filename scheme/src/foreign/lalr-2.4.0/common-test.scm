;;; common-test.scm --
;;;

(debug-enable 'backtrace)
(use-modules (ice-9 syncase))

(load "lalr.scm")

(define-syntax when
  (syntax-rules ()
    ((_ ?expr ?body ...)
     (if ?expr
	 (let () ?body ...)
       #f))))

(define-syntax check
  (syntax-rules (=>)
    ((_ ?expr => ?expected-result)
     (check ?expr (=> equal?) ?expected-result))

    ((_ ?expr (=> ?equal) ?expected-result)
     (let ((result	?expr)
	   (expected	?expected-result))
       (when (not (?equal result expected))
	 (display "Failed test: \n")
	 (write (quote ?expr))(newline)
	 (display "\tresult was: ")
	 (write result)(newline)
	 (display "\texpected: ")
	 (write expected)(newline))))))

;;; --------------------------------------------------------------------

(define (display-result v)
  (if v
      (begin
        (display "==> ")
        (display v)
        (newline))))

(define eoi-token
  (make-lexical-token '*eoi* #f #f))

(define (make-lexer tokens)
  (lambda ()
    (if (null? tokens)
	eoi-token
      (let ((t (car tokens)))
	(set! tokens (cdr tokens))
	t))))

(define (error-handler message . args)
  (display "error handler: ")
  (display message)
  (newline)
  (when (< 0 (length args))
    (pretty-print args)
    (newline))
  (cons message args))

;;; end of file
