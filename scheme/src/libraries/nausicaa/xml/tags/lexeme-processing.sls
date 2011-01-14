;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: lexeme processing parametrised functions
;;;Date: Fri Jan 14, 2011
;;;
;;;Abstract
;;;
;;;	Define a  set of parametrised functions used  to process lexemes
;;;	from XML documents.  This library is used by the XML lexer table
;;;	produced with  SILex; see the "make-tables.sps"  program in this
;;;	directory.
;;;
;;;Copyright (C) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!r6rs
(library (nausicaa xml tags lexeme-processing)
  (export
    current-input-source

    ;; token maker parameters		 default token makers
    lexical-error-token-maker		make-lexical-error-token
    eoi-token-maker			make-eoi-token

    open-paren-token-maker		make-open-paren-token

    )
  (import (nausicaa)
    (nausicaa parser-tools))


;;;; input source handling

(define current-input-source
  (make-parameter #f))

(define-syntax input-source
  (lambda (stx)
    (syntax-case stx ()
      ((?keyword)
       #`(make-<source-location> (current-input-source)
				 #,(datum->syntax #'?keyword 'yyline)
				 #,(datum->syntax #'?keyword 'yycolumn)
				 #,(datum->syntax #'?keyword 'yyoffset)))
      )))


;;;; built in token makers

(define (make-lexical-error-token yygetc yyungetc yytext yyline yycolumn yyoffset)
  (silex-default-error-handler yytext))

(define (make-eoi-token yygetc yyungetc yytext yyline yycolumn yyoffset)
  (silex-default-eof-handler))

;;; --------------------------------------------------------------------

(define (make-open-paren-token yygetc yyungetc yytext yyline yycolumn yyoffset)
  (make* <lexical-token> 'OPAREN (input-source) #\( 1))


;;;; token maker parameters

(define-syntax define-token-maker-parameter
  (syntax-rules ()
    ((_ ?param ?default)
     (define ?param
       (make-parameter ?default
	 (lambda (obj)
	   (if (procedure? obj)
	       obj
	     (assertion-violation '?param
	       "expected procedure as parameter value" obj))))))))

(define-token-maker-parameter lexical-error-token-maker		make-lexical-error-token)
(define-token-maker-parameter eoi-token-maker			make-eoi-token)

(define-token-maker-parameter open-paren-token-maker		make-open-paren-token)


;;;; done

)

;;; end of file
