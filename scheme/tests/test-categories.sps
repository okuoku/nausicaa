;;; test-categories.sps --
;;;

(import (rnrs)
  (char-sets))

(define (make-set ell)
  (map (lambda (range)
	 (write (cons (string-append "#\\x" (number->string (char->integer (car range)) 16))
		      (string-append "#\\x" (number->string (char->integer (cdr range)) 16))))
	 (newline))
    (char-set-domain-ref (apply char-set ell))))

(define ell '(

  ))

(make-set ell)


;;; end of file
