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
  (import (for (rnrs) run expand (meta 2))
    (prefix (only (nausicaa) + - * / < > <= >= =) nausicaa:)
    (for (parser-tools lexical-token)	expand)
    (for (infix sexp-parser)		expand))


(define-syntax infix
  (let ( ;;Constant tokens representing the recognised operators.
	($add:rnrs	(make-<lexical-token> 'ADD #f #'+ 0))
	($add:nausicaa	(make-<lexical-token> 'ADD #f #'nausicaa:+ 0))
	($sub:rnrs	(make-<lexical-token> 'SUB #f #'- 0))
	($sub:nausicaa	(make-<lexical-token> 'SUB #f #'nausicaa:- 0))
	($mul:rnrs	(make-<lexical-token> 'MUL #f #'* 0))
	($mul:nausicaa	(make-<lexical-token> 'MUL #f #'nausicaa:* 0))
	($/:rnrs	(make-<lexical-token> 'DIV #f #'/ 0))
	($div:nausicaa	(make-<lexical-token> 'DIV #f #'nausicaa:/ 0))
	($mod		(make-<lexical-token> 'MOD #f #'mod 0))
	($mod0		(make-<lexical-token> 'MOD0 #f #'mod0 0))
	($expt		(make-<lexical-token> 'EXPT #f #'expt 0))
	($div		(make-<lexical-token> 'DIV  #f #'div 0))
	($div0		(make-<lexical-token> 'DIV0 #f #'div0 0))
	($lt:rnrs	(make-<lexical-token> 'LT #f #'< 0))
	($lt:nausicaa	(make-<lexical-token> 'LT #f #'nausicaa:< 0))
	($gt:rnrs	(make-<lexical-token> 'GT #f #'> 0))
	($gt:nausicaa	(make-<lexical-token> 'GT #f #'nausicaa:> 0))
	($le:rnrs	(make-<lexical-token> 'LE #f #'<= 0))
	($le:nausicaa	(make-<lexical-token> 'LE #f #'nausicaa:<= 0))
	($ge:rnrs	(make-<lexical-token> 'GE #f #'>= 0))
	($ge:nausicaa	(make-<lexical-token> 'GE #f #'nausicaa:>= 0))
	($eq:rnrs	(make-<lexical-token> 'EQ #f #'= 0))
	($eq:nausicaa	(make-<lexical-token> 'EQ #f #'nausicaa:= 0))
	;;Here we use the #'if syntax  object as semantic value: it is a
	;;trick to avoid insertion of  a raw value: the parser will take
	;;it and use it as the IF in the output form.
	($question	(make-<lexical-token> 'QUESTION-ID #f #'if 0))
	($colon		(make-<lexical-token> 'COLON-ID #f #': 0))

	;;Special  constant  tokens.  Notice  that  the  left and  right
	;;parens tokens  are wrapped  in a list  because below  they are
	;;used as arguments to APPEND.
	($eoi		(make-<lexical-token> '*eoi* #f (eof-object) 0))
	($left-paren	(make-<lexical-token> 'LPAREN #f #\( 0))
	($right-paren	(make-<lexical-token> 'RPAREN #f #\) 0)))

    (define-syntax memv
      (syntax-rules ()
	((_ ?atom ?stx)
	 (free-identifier=? ?atom ?stx))
	((_ ?atom ?stx ...)
	 (or (free-identifier=? ?atom ?stx) ...))))

    (define-syntax case
      (syntax-rules (else)
	((_ ?atom ((?s ...) ?e ...) ... (else ?b ...))
	 (cond ((memv ?atom #'?s ...) ?e ...) ... (else ?b ...)))))

    (define (syntax->list stx)
      (syntax-case stx ()
	((?begin . ?body)
	 (and (identifier? #'?begin) (free-identifier=? #'begin #'?begin))
	 (cons #'begin #'?body))
	(()		'())
	((?car . ?cdr)	(cons (syntax->list #'?car) (syntax->list #'?cdr)))
	(?atom		(identifier? #'?atom)	#'?atom)
	(?atom		(syntax->datum #'?atom))))

    (define (stx-list->reversed-tokens sexp reversed-tokens)
      ;;Convert a list of syntax objects to the reversed list of tokens.
      ;;This  is a  recursive function:  it recurses  to  process nested
      ;;lists in the  input SEXP; for this reason  we cannot reverse the
      ;;tokens here, we have to let the caller reverse it.
      ;;
      (if (null? sexp)
	  reversed-tokens
	(let ((atom (car sexp)))
	  (cond ((identifier? atom)
		 (stx-list->reversed-tokens
		  (cdr sexp)
		  (cons (case atom
			  ((+)			$add:rnrs)
			  ((nausicaa:+)		$add:nausicaa)
			  ((-)			$sub:rnrs)
			  ((nausicaa:-)		$sub:nausicaa)
			  ((*)			$mul:rnrs)
			  ((nausicaa:*)		$mul:nausicaa)
			  ((/)			$/:rnrs)
			  ((nausicaa:/)		$div:nausicaa)
			  ((% mod)		$mod)
			  ((mod0)		$mod0)
			  ((^ expt)		$expt)
			  ((// div)		$div)
			  ((div0)		$div0)
			  ((<)			$lt:rnrs)
			  ((nausicaa:<)		$lt:nausicaa)
			  ((>)			$gt:rnrs)
			  ((nausicaa:>)		$gt:nausicaa)
			  ((<=)			$le:rnrs)
			  ((nausicaa:<=)	$le:nausicaa)
			  ((>=)			$ge:rnrs)
			  ((nausicaa:>=)	$ge:nausicaa)
			  ((=)			$eq:rnrs)
			  ((nausicaa:=)		$eq:nausicaa)
			  ((?)			$question)
			  ((:)			$colon)
			  (else
			   (make-<lexical-token> 'ID #f atom 0)))
			reversed-tokens)))
		((pair? atom)
		 (if (and (identifier? (car atom))
			  (free-identifier=? #'begin (car atom)))
		     (stx-list->reversed-tokens
		      (cdr sexp)
		      (cons (make-<lexical-token> 'NUM #f (cons #'begin (cdr atom)) 0)
			    reversed-tokens))
		   ;;Parentheses in  reverse order because  the TOKENS
		   ;;will be reversed!!!
		   (stx-list->reversed-tokens
		    (cdr sexp)
		    (cons $right-paren
			  (stx-list->reversed-tokens atom (cons $left-paren reversed-tokens))))))
		(else
		 ;;Everything else is just put there as "NUM", that is
		 ;;as operand.
		 (stx-list->reversed-tokens
		  (cdr sexp)
		  (cons (make-<lexical-token> 'NUM #f atom 0)
			reversed-tokens)))))))

    (lambda (stx)
      (syntax-case stx ()
	((?infix ?operand ...)
	 (let ((stx-lst (syntax->list #'(?operand ...))))
	   (cond ((null? stx-lst)
		  #'(values))
		 ((not (pair? stx-lst))
		  stx-lst)
		 (else
		  (let ((tokens (reverse (stx-list->reversed-tokens stx-lst '()))))
		    ((make-infix-sexp-parser) ;parser function
		     (lambda ()		      ;lexer function
		       (if (null? tokens)
			   $eoi
			 (let ((t (car tokens)))
			   (set! tokens (cdr tokens))
			   t)))
		     (lambda (message token) ;error handler
		       (syntax-violation 'infix
			 (string-append "processing infix expression: " message)
			 (syntax->datum #'(?infix ?operand ...)) token))
		     #f))))))))))


;;;; done

)

;;; end of file
