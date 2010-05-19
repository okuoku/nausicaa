;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: implementation of the INFIX syntax
;;;Date: Tue May 18, 2010
;;;
;;;Abstract
;;;
;;;	The only reason to have  the INFIX syntax in a library separated
;;;	from (infix)  is to  allow loading it  without loading  also the
;;;	infix lexer, which takes some memory.
;;;
;;;Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (infix syntax)
  (export infix)
  (import (rnrs)
    (prefix (only (nausicaa) + - * / < > <= >= =) nausicaa:)
    (for (parser-tools lexical-token)	expand)
    (for (infix syntax-parser)		expand)
    (for (infix helpers)		expand))


(define-syntax infix
  (lambda (stx)
    (define (%stx-list->tokens expr)
      (define-syntax set-cons!
	(syntax-rules ()
	  ((_ ?name ?form)
	   (set! ?name (cons ?form ?name)))))
      (define-syntax set-append!
	(syntax-rules ()
	  ((_ ?name ?form ...)
	   (set! ?name (append ?form ... ?name)))))
      (do ((result '())
	   (expr	(if (pair? expr) expr (list expr)) (cdr expr)))
	  ((null? expr)
	   result)
	(let ((atom (car expr)))
	  (cond ((number? atom)
		 (set-cons! result (make-<lexical-token> 'NUM #f atom 0)))
		((identifier? atom)
		 (set-cons! result
			    (cond
			     ((or (free-identifier=? atom #'+)
				  (free-identifier=? atom #'nausicaa:+))
			      (make-<lexical-token> 'ADD	#f atom 0))
			     ((or (free-identifier=? atom #'-)
				  (free-identifier=? atom #'nausicaa:-))
			      (make-<lexical-token> 'SUB	#f atom 0))
			     ((or (free-identifier=? atom #'*)
				  (free-identifier=? atom #'nausicaa:*))
			      (make-<lexical-token> 'MUL	#f atom 0))
			     ((or (free-identifier=? atom #'/)
				  (free-identifier=? atom #'nausicaa:/))
			      (make-<lexical-token> 'DIV	#f atom 0))
			     ((free-identifier=? atom #'%)
			      (make-<lexical-token> 'MOD	#f #'mod 0))
			     ((free-identifier=? atom #'^)
			      (make-<lexical-token> 'EXPT	#f #'expt 0))
			     ((free-identifier=? atom #'//)
			      (make-<lexical-token> 'DIV0	#f #'div 0))
			     ((or (free-identifier=? atom #'<)
				  (free-identifier=? atom #'nausicaa:<))
			      (make-<lexical-token> 'LT		#f #'< 0))
			     ((or (free-identifier=? atom #'>)
				  (free-identifier=? atom #'nausicaa:>))
			      (make-<lexical-token> 'GT		#f #'> 0))
			     ((or (free-identifier=? atom #'<=)
				  (free-identifier=? atom #'nausicaa:<=))
			      (make-<lexical-token> 'LE		#f #'<= 0))
			     ((or (free-identifier=? atom #'>=)
				  (free-identifier=? atom #'nausicaa:>=))
			      (make-<lexical-token> 'GE		#f #'>= 0))
			     ((or (free-identifier=? atom #'=)
				  (free-identifier=? atom #'nausicaa:=))
			      (make-<lexical-token> 'EQ		#f #'= 0))

			     ((free-identifier=? atom #'?)
			      ;;Here  using the  #'if  syntax object  as
			      ;;semantic  value  is  a  trick  to  avoid
			      ;;insertion  of a  raw  value: the  parser
			      ;;will take it and use it as the IF in the
			      ;;output form.
			      (make-<lexical-token> 'QUESTION-ID #f #'if 0))

			     ((free-identifier=? atom #':)
			      (make-<lexical-token> 'COLON-ID	#f #': 0))
			     (else
			      (make-<lexical-token> 'ID		#f atom 0)))))
		((pair? atom)
		 ;;Parentheses in reverse  order because the RESULT will
		 ;;be reversed!!!
		 (set-append! result ell-rparen-token (%stx-list->tokens atom) ell-lparen-token))
		(else
		 ;;Everything else  is just put there as  "NUM", that is
		 ;;as operand.
		 (make-<lexical-token> 'NUM #f atom 0))))))

    (syntax-case stx (? :)
      ((_ ?operand ...)
       (let ((tokens	(reverse (%stx-list->tokens (syntax->list #'(?operand ...))))))
	 (if (null? tokens)
	     #'(values)
	   ((make-infix-syntax-parser)		;parser function
	    (lambda ()				;lexer function
	      (if (null? tokens)
		  eoi-token
		(let ((t (car tokens)))
		  (set! tokens (cdr tokens))
		  t)))
	    error-handler #f)))))))


;;;; done

)

;;; end of file
