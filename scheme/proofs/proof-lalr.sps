;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: proofs for lalr
;;;Date: Wed Aug 12, 2009
;;;
;;;Abstract
;;;
;;;
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
  (checks))

(define (display-result v)
  (if v
      (begin
        (display "==> ")
        (display v)
        (newline))))

(define eoi-token
  (make-lexical-token '*eoi* #f #f))

(check-set-mode! 'report-failed)



;;;; LR driver, associativity

(when #t
  (let ()
    (define parser-terminals
      '(N (left: A) (right: M) (nonassoc: U)))

    (define parser-non-terminals
      '((E	(N)		: $1
		(E A E)		: (list $1 $2 $3)
		(E M E)		: (list $1 $2 $3)
		(A E (prec: U))	: (list $1 $2))))

    (define make-parser
      (parameterise ((debugging	#f))
	(lalr-parser :output-value #t :expect 0
		     :terminals parser-terminals
		     :rules parser-non-terminals)))

    (define (doit tokens)
      (let* ((lexer		(lambda ()
				  (let ((t (car tokens)))
				    (set! tokens (cdr tokens))
				    t)))
	     (error-handler	(lambda (message token)
				  ;;(display message)(newline)
                                  `(error-token . ,(lexical-token-value token))))
	     (parser		(make-parser)))
	(parameterise ((debugging	#f))
	  (parser lexer error-handler))))

    (when #f
      (lalr-parser :output-port (current-output-port)
		   :expect #f
;;;		 :dump-table "/tmp/marco/p"
		   :terminals parser-terminals
		   :rules parser-non-terminals)
      (newline)
      (newline))

    (check
	(doit (list (make-lexical-token 'N #f 1)
		    eoi-token))
      => 1)

    (check
	(doit (list (make-lexical-token 'A #f '-)
		    (make-lexical-token 'N #f 1)
		    eoi-token))
      => '(- 1))

    (check
	(doit (list (make-lexical-token 'A #f '+)
		    (make-lexical-token 'N #f 1)
		    eoi-token))
      => '(+ 1))

    (check
	(doit (list (make-lexical-token 'N #f 1)
		    (make-lexical-token 'A #f '+)
		    (make-lexical-token 'N #f 2)
		    eoi-token))
      => '(1 + 2))

    (check
	(doit (list (make-lexical-token 'N #f 1)
		    (make-lexical-token 'A #f '+)
		    (make-lexical-token 'N #f 2)
		    (make-lexical-token 'M #f '*)
		    (make-lexical-token 'N #f 3)
		    eoi-token))
      => '(1 + (2 * 3)))

    (check	;left associative
	(doit (list (make-lexical-token 'N #f 1)
		    (make-lexical-token 'A #f '+)
		    (make-lexical-token 'N #f 2)
		    (make-lexical-token 'A #f '+)
		    (make-lexical-token 'N #f 3)
		    eoi-token))
      => '((1 + 2) + 3))

    (check 'this 	;right associative
	(doit (list (make-lexical-token 'N #f 1)
		    (make-lexical-token 'M #f '*)
		    (make-lexical-token 'N #f 2)
		    (make-lexical-token 'M #f '*)
		    (make-lexical-token 'N #f 3)
		    eoi-token))
      => '(1 * (2 * 3)))

    ))



;;;; done

(check-report)

;;; end of file
