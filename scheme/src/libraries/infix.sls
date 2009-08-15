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


(library (infix)
  (export
    infix-string->sexp
    infix->prefix)
  (import (rnrs)
    (silex lexer)
    (infix string-lexer)
    (infix sexp-parser)
    (infix string-parser))


;;;; helpers

(define unknown-location
  (make-source-location #f +nan.0 +nan.0 +nan.0 0))

(define eoi-token
  (make-lexical-token '*eoi* unknown-location (eof-object)))

(define ell-lparen-token
  (list (make-lexical-token 'LPAREN unknown-location #\()))

(define ell-rparen-token
  (list (make-lexical-token 'RPAREN unknown-location #\))))

(define (error-handler message token)
  (error #f
    (if (not (lexical-token? token))
	message
      (let* ((position	(lexical-token-source token))
	     (line	(source-location-line position))
	     (column	(source-location-column position)))
	(string-append
	 message
	 " line " (if line (number->string line) "unknown")
	 " column " (if column (number->string column) "unknown"))))
    token))


(define (infix-string->sexp string)
  (let* ((IS		(lexer-make-IS :string string :counters 'all))
	 (lexer		(lexer-make-lexer infix-string-lexer-table IS))
	 (parser	(make-infix-string-parser)))
    (parser lexer error-handler #f)))


(define (infix->prefix sexp)
  (let* ((tokens	(infix-sexp->tokens sexp))
	 (lexer		(lambda ()
			  (if (null? tokens)
			      eoi-token
			    (let ((t (car tokens)))
			      (set! tokens (cdr tokens))
			      t))))
	 (parser	(make-infix-sexp-parser)))
    (parser lexer error-handler #f)))

(define (infix-sexp->tokens expr)
  (reverse (%infix-sexp->tokens expr)))

(define (%infix-sexp->tokens expr)
  (do ((result '())
       (expr	(if (pair? expr) expr (list expr)) (cdr expr)))
      ((null? expr)
       result)
    (let ((atom (car expr)))
      (cond ((number? atom)
	     (set! result
		   (cons (make-lexical-token 'NUM unknown-location atom)
			 result)))
	    ((symbol? atom)
	     (set! result
		   (cons (case atom
			   ((+)	(make-lexical-token 'ADD	unknown-location '+))
			   ((-)	(make-lexical-token 'SUB	unknown-location '-))
			   ((*)	(make-lexical-token 'MUL	unknown-location '*))
			   ((/)	(make-lexical-token 'DIV	unknown-location '/))
			   ((%)	(make-lexical-token 'MOD	unknown-location 'mod))
			   ((^)	(make-lexical-token 'EXPT	unknown-location 'expt))
			   ((//) (make-lexical-token 'DIV0	unknown-location 'div))
			   ((<)	(make-lexical-token 'LT		unknown-location '<))
			   ((>)	(make-lexical-token 'GT		unknown-location '>))
			   ((<=) (make-lexical-token 'LE	unknown-location '<=))
			   ((>=) (make-lexical-token 'GE	unknown-location '>=))
			   ((=)	(make-lexical-token 'EQ		unknown-location '=))
			   (else (make-lexical-token 'ID	unknown-location atom)))
			 result)))
	    ((procedure? atom)
	     (set! result
		   (cons (make-lexical-token 'ID unknown-location atom)
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
	     (make-lexical-token 'NUM unknown-location atom))))))


;;;; done

)

;;; end of file
