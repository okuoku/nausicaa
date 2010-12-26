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
  (export infix-string->sexp infix->prefix)
  (import (rnrs)
    (silex lexer)
    (parser-tools source-location)
    (parser-tools lexical-token)
    (infix string-lexer)
    (infix string-parser)
    (infix sexp-parser)
    (infix helpers))


(define (infix-string->sexp string)
  (let* ((IS		(lexer-make-IS (string: string) (counters: 'all)))
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

(define %infix-sexp->tokens
  (let (($add		(make-<lexical-token> 'ADD		#f '+ 1))
	($sub		(make-<lexical-token> 'SUB		#f '- 1))
	($mul		(make-<lexical-token> 'MUL		#f '* 1))
	($div		(make-<lexical-token> 'DIV		#f '/ 1))
	($mod		(make-<lexical-token> 'MOD		#f 'mod 1))
	($expt		(make-<lexical-token> 'EXPT		#f 'expt 1))
	($div0		(make-<lexical-token> 'DIV0		#f 'div 2))
	($lt		(make-<lexical-token> 'LT		#f '< 1))
	($gt		(make-<lexical-token> 'GT		#f '> 1))
	($le		(make-<lexical-token> 'LE		#f '<= 2))
	($ge		(make-<lexical-token> 'GE		#f '>= 2))
	($eq		(make-<lexical-token> 'EQ		#f '= 1))
	($question	(make-<lexical-token> 'QUESTION-ID	#f 'if 1))
	($colon		(make-<lexical-token> 'COLON-ID		#f ': 1)))
    (lambda (expr)
      (do ((tokens '())
	   (expr	(if (pair? expr) expr (list expr)) (cdr expr)))
	  ((null? expr)
	   tokens)
	(let ((atom (car expr)))
	  (let-syntax ((set-cons!	(syntax-rules ()
					  ((_ ?name ?form)
					   (set! ?name (cons ?form ?name)))))
		       (set-append!	(syntax-rules ()
					  ((_ ?name ?form ...)
					   (set! ?name (append ?form ... ?name))))))
	    (cond ((number? atom)
		   (set-cons! tokens (make-<lexical-token> 'NUM #f atom 0)))
		  ((symbol? atom)
		   (set-cons! tokens (case atom
				       ((+)	$add)
				       ((-)	$sub)
				       ((*)	$mul)
				       ((/)	$div)
				       ((%)	$mod)
				       ((^)	$expt)
				       ((//)	$div0)
				       ((<)	$lt)
				       ((>)	$gt)
				       ((<=)	$le)
				       ((>=)	$ge)
				       ((=)	$eq)
				       ((?)	$question)
				       ((:)	$colon)
				       (else	(make-<lexical-token> 'ID #f atom 0)))))
		  ((procedure? atom)
		   (set-cons! tokens (make-<lexical-token> 'ID #f atom 0)))
		  ((pair? atom)
		   (set-append! tokens
				;;Parentheses  in reverse  order because
				;;the TOKENS will be reversed!!!
				ell-rparen-token
				(%infix-sexp->tokens atom)
				ell-lparen-token))
		  (else
		   (set-cons! tokens (make-<lexical-token> 'NUM #f atom 0))))))))))


;;;; done

)

;;; end of file
