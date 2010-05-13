;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: infix notation
;;;Date: Sat Aug 15, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (infix)
  (export infix-string->sexp infix->prefix infix)
  (import (rnrs)
    (prefix (only (nausicaa) + - * / < > <= >= =) nausicaa:)
    (only (keywords) define-keywords)
    (silex lexer)
    (infix string-lexer)
    (infix string-parser)
    (parser-tools source-location)
    (for (infix sexp-parser)		expand run)
    (for (parser-tools lexical-token)	expand run)
    (for (infix helpers)		expand run))

(define-keywords :string :counters)


(define (infix-string->sexp string)
  (let* ((IS		(lexer-make-IS :string string :counters 'all))
	 (lexer		(lexer-make-lexer infix-string-lexer-table IS))
	 (parser	(make-infix-string-parser)))
    (parser lexer error-handler #f)))


(define (infix->prefix . sexp)
  (let* ((tokens	(reverse (%infix-sexp->tokens sexp)))
	 (lexer		(lambda ()
			  (if (null? tokens)
			      eoi-token
			    (let ((t (car tokens)))
			      (set! tokens (cdr tokens))
			      t))))
	 (parser	(make-infix-sexp-parser)))
    (parser lexer error-handler #f)))

(define (%infix-sexp->tokens expr)
  (do ((result '())
       (expr	(if (pair? expr) expr (list expr)) (cdr expr)))
      ((null? expr)
       result)
    (let ((atom (car expr)))
      (cond ((number? atom)
	     (set! result
		   (cons (make-<lexical-token> 'NUM #f atom 0)
			 result)))
	    ((symbol? atom)
	     (set! result
		   (cons (case atom
			   ((+)	(make-<lexical-token> 'ADD	#f '+ 0))
			   ((-)	(make-<lexical-token> 'SUB	#f '- 0))
			   ((*)	(make-<lexical-token> 'MUL	#f '* 0))
			   ((/)	(make-<lexical-token> 'DIV	#f '/ 0))
			   ((%)	(make-<lexical-token> 'MOD	#f 'mod 0))
			   ((^)	(make-<lexical-token> 'EXPT	#f 'expt 0))
			   ((//) (make-<lexical-token> 'DIV0	#f 'div 0))
			   ((<)	(make-<lexical-token> 'LT	#f '< 0))
			   ((>)	(make-<lexical-token> 'GT	#f '> 0))
			   ((<=) (make-<lexical-token> 'LE	#f '<= 0))
			   ((>=) (make-<lexical-token> 'GE	#f '>= 0))
			   ((=)	(make-<lexical-token> 'EQ	#f '= 0))
			   (else (make-<lexical-token> 'ID	#f atom 0)))
			 result)))
	    ((procedure? atom)
	     (set! result
		   (cons (make-<lexical-token> 'ID #f atom 0)
			 result)))
	    ((pair? atom)
	     (set! result
		   ;;Parentheses  in reverse  order  because the  RESULT
		   ;;will be reversed!!!
		   (append ell-rparen-token
			   (%infix-sexp->tokens atom)
			   ell-lparen-token
			   result)))
	    (else
	     (make-<lexical-token> 'NUM #f atom 0))))))


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

    (syntax-case stx ()
      ((_ ?operand ...)
       (let ((tokens	(reverse (%stx-list->tokens (syntax->list #'(?operand ...))))))
	 (if (null? tokens)
	     #'(values)
	   ((make-infix-sexp-parser)		;parser function
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
