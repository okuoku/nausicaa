;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: lexeme processing parameters and functions
;;;Date: Wed Jan  5, 2011
;;;
;;;Abstract
;;;
;;;
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
(library (nausicaa r6rs lexeme-processing)
  (export
    current-input-source

    ;; token maker parameters		 default token makers
    lexical-error-token-maker		make-lexical-error-token
    eof-token-maker			make-eof-token

    open-paren-token-maker		make-open-paren-token
    close-paren-token-maker		make-close-paren-token
    open-bracket-token-maker		make-open-bracket-token
    close-bracket-token-maker		make-close-bracket-token
    tick-token-maker			make-tick-token
    back-tick-token-maker		make-back-tick-token
    comma-at-token-maker		make-comma-at-token
    comma-token-maker			make-comma-token
    dot-token-maker			make-dot-token
    doublequote-token-maker		make-doublequote-token
    semicolon-token-maker		make-semicolon-token
    sharp-paren-token-maker		make-sharp-paren-token
    sharp-vu8paren-token-maker		make-sharp-vu8paren-token
    sharp-tick-token-maker		make-sharp-tick-token
    sharp-back-tick-token-maker		make-sharp-back-tick-token
    sharp-comma-at-token-maker		make-sharp-comma-at-token
    sharp-comma-token-maker		make-sharp-comma-token
    sharp-semicolon-token-maker		make-sharp-semicolon-token

    line-comment-token-maker		make-line-comment-token
    line-comment-noend-token-maker	make-line-comment-noend-token
    open-nested-comment-token-maker	make-open-nested-comment-token
    sharp-bang-r6rs-token-maker		make-sharp-bang-r6rs-token
    sharp-bang-token-maker		make-sharp-bang-token
    white-space-token-maker		make-white-space-token
    line-ending-token-maker		make-line-ending-token

    identifier-token-maker		make-identifier-token
    boolean-token-maker			make-boolean-token
    named-character-token-maker		make-named-character-token
    hex-character-token-maker		make-hex-character-token
    literal-character-token-maker	make-literal-character-token
    number-token-maker			make-number-token
    )
  (import (nausicaa)
    (nausicaa parser-tools lexical-token)
    (nausicaa parser-tools source-location)
    (nausicaa silex default-error-handler))


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

(define (make-eof-token yygetc yyungetc yytext yyline yycolumn yyoffset)
  (silex-default-eof-handler))

;;; --------------------------------------------------------------------

(define (make-open-paren-token yygetc yyungetc yytext yyline yycolumn yyoffset)
  (make* <lexical-token> 'OPAREN (input-source) #\( 1))

(define (make-close-paren-token yygetc yyungetc yytext yyline yycolumn yyoffset)
  (make* <lexical-token> 'CPAREN (input-source) #\) 1))

(define (make-open-bracket-token yygetc yyungetc yytext yyline yycolumn yyoffset)
  (make* <lexical-token> 'OBRACKET (input-source) #\[ 1))

(define (make-close-bracket-token yygetc yyungetc yytext yyline yycolumn yyoffset)
  (make* <lexical-token> 'CBRACKET (input-source) #\] 1))

(define (make-tick-token yygetc yyungetc yytext yyline yycolumn yyoffset)
  (make* <lexical-token> 'TICK (input-source) #\' 1))

(define (make-back-tick-token yygetc yyungetc yytext yyline yycolumn yyoffset)
  (make* <lexical-token> 'BACKTICK (input-source) #\` 1))

(define (make-comma-at-token yygetc yyungetc yytext yyline yycolumn yyoffset)
  (make* <lexical-token> 'COMMAAT (input-source) ",@" 2))

(define (make-comma-token yygetc yyungetc yytext yyline yycolumn yyoffset)
  (make* <lexical-token> 'COMMA (input-source) #\, 1))

(define (make-dot-token yygetc yyungetc yytext yyline yycolumn yyoffset)
  (make* <lexical-token> 'DOT (input-source) #\. 1))

(define (make-doublequote-token yygetc yyungetc yytext yyline yycolumn yyoffset)
  (make* <lexical-token> 'DOUBLEQUOTE (input-source) #\" 1))

(define (make-semicolon-token yygetc yyungetc yytext yyline yycolumn yyoffset)
  (make* <lexical-token> 'SEMICOLON (input-source) #\; 1))

(define (make-sharp-paren-token yygetc yyungetc yytext yyline yycolumn yyoffset)
  (make* <lexical-token> 'SHARPPAREN (input-source) "#(" 2))

(define (make-sharp-vu8paren-token yygetc yyungetc yytext yyline yycolumn yyoffset)
  (make* <lexical-token> 'SHARPVU8PAREN (input-source) "#vu8(" 4))

(define (make-sharp-tick-token yygetc yyungetc yytext yyline yycolumn yyoffset)
  (make* <lexical-token> 'SHARPTICK (input-source) "#'" 2))

(define (make-sharp-back-tick-token yygetc yyungetc yytext yyline yycolumn yyoffset)
  (make* <lexical-token> 'SHARPBACKTICK (input-source) "#`" 2))

(define (make-sharp-comma-at-token yygetc yyungetc yytext yyline yycolumn yyoffset)
  (make* <lexical-token> 'SHARPCOMMAAT (input-source) "#,@" 3))

(define (make-sharp-comma-token yygetc yyungetc yytext yyline yycolumn yyoffset)
  (make* <lexical-token> 'SHARPCOMMA (input-source) "#," 1))

(define (make-sharp-semicolon-token yygetc yyungetc yytext yyline yycolumn yyoffset)
  (make* <lexical-token> 'SHARPSEMICOLON (input-source) "#;" 2))

;;; --------------------------------------------------------------------

(define (make-line-comment-token yygetc yyungetc yytext yyline yycolumn yyoffset)
  (make* <lexical-token> 'LINECOMMENT (input-source) yytext (string-length yytext)))

(define (make-line-comment-noend-token yygetc yyungetc yytext yyline yycolumn yyoffset)
  (make* <lexical-token> 'LINECOMMENT-NOEND (input-source) yytext (string-length yytext)))

(define (make-open-nested-comment-token yygetc yyungetc yytext yyline yycolumn yyoffset)
  (make* <lexical-token> 'ONESTEDCOMMENT (input-source) "#|" 2))

(define (make-sharp-bang-r6rs-token yygetc yyungetc yytext yyline yycolumn yyoffset)
  (make* <lexical-token> 'SHARPBANGR6RS (input-source) "#!r6rs" 6))

(define (make-sharp-bang-token yygetc yyungetc yytext yyline yycolumn yyoffset)
  (make* <lexical-token> 'SHARPBANG (input-source) "#!" 2))

(define (make-white-space-token yygetc yyungetc yytext yyline yycolumn yyoffset)
  (make* <lexical-token> 'WHITESPACE (input-source) yytext (string-length yytext)))

(define (make-line-ending-token yygetc yyungetc yytext yyline yycolumn yyoffset)
  (make* <lexical-token> 'LINEENDING (input-source) yytext (string-length yytext)))

;;; --------------------------------------------------------------------

(define (make-identifier-token yygetc yyungetc yytext yyline yycolumn yyoffset)
  (make* <lexical-token> 'IDENTIFIER (input-source) (string->symbol yytext) (string-length yytext)))

(define (make-boolean-token yygetc yyungetc yytext yyline yycolumn yyoffset)
  (make* <lexical-token> 'BOOLEAN (input-source)
	 ;;Notice that  we cannot use a  CASE with strings;  refer to the
	 ;;definition of EQV? in the R6RS document.
	 (cond ((or (string=? yytext "#t")
		    (string=? yytext "#T")) #t)
	       ((or (string=? yytext "#f")
		    (string=? yytext "#F")) #f)
	       (else
		;;Notice that this should never happen.
		(assertion-violation 'make-boolean-token
		  "internal error, invalid boolean" yytext)))
	 2))

(define (make-named-character-token yygetc yyungetc yytext yyline yycolumn yyoffset)
  (make* <lexical-token> 'CHARACTER (input-source)
	 ;;Notice that  we cannot use a  CASE with strings;  refer to the
	 ;;definition of EQV? in the R6RS document.
	 (cond ((string=? yytext "#\\nul")	#\nul)
	       ((string=? yytext "#\\alarm")	#\alarm)
	       ((string=? yytext "#\\backspace")	#\backspace)
	       ((string=? yytext "#\\tab")	#\tab)
	       ((string=? yytext "#\\linefeed")	#\linefeed)
	       ((string=? yytext "#\\newline")	#\newline)
	       ((string=? yytext "#\\vtab")	#\vtab)
	       ((string=? yytext "#\\page")	#\page)
	       ((string=? yytext "#\\return")	#\return)
	       ((string=? yytext "#\\esc")	#\esc)
	       ((string=? yytext "#\\space")	#\space)
	       ((string=? yytext "#\\delete")	#\delete)
	       (else
		;;Notice that this should never happen.
		(assertion-violation 'make-named-character-token
		  "internal error, invalid named character" yytext)))
	 (string-length yytext)))

(define (make-hex-character-token yygetc yyungetc yytext yyline yycolumn yyoffset)
  (make* <lexical-token> 'CHARACTER (input-source)
	 (let* ((len (string-length yytext))
		(num (string->number (substring yytext 3 len) 16)))
	   (if (or (<= 0 num #xD7FF) (<= #xE000 num #x10FFFF))
	       (integer->char num)
	     ((lexical-error-token-maker) yygetc yyungetc yytext yyline yycolumn yyoffset)))
	 (string-length yytext)))

(define (make-literal-character-token yygetc yyungetc yytext yyline yycolumn yyoffset)
  (make* <lexical-token> 'CHARACTER (input-source) (string-ref yytext 2) (string-length yytext)))

(define (make-number-token yygetc yyungetc yytext yyline yycolumn yyoffset)
  (let ((n (string->number yytext)))
    (if n
	(make* <lexical-token> 'NUMBER (input-source) n (string-length yytext))
      ((lexical-error-token-maker) yygetc yyungetc yytext yyline yycolumn yyoffset))))


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
(define-token-maker-parameter eof-token-maker			make-eof-token)

(define-token-maker-parameter open-paren-token-maker		make-open-paren-token)
(define-token-maker-parameter close-paren-token-maker		make-close-paren-token)
(define-token-maker-parameter open-bracket-token-maker		make-open-bracket-token)
(define-token-maker-parameter close-bracket-token-maker		make-close-bracket-token)
(define-token-maker-parameter tick-token-maker			make-tick-token)
(define-token-maker-parameter back-tick-token-maker		make-back-tick-token)
(define-token-maker-parameter comma-at-token-maker		make-comma-at-token)
(define-token-maker-parameter comma-token-maker			make-comma-token)
(define-token-maker-parameter dot-token-maker			make-dot-token)
(define-token-maker-parameter doublequote-token-maker		make-doublequote-token)
(define-token-maker-parameter semicolon-token-maker		make-semicolon-token)
(define-token-maker-parameter sharp-paren-token-maker		make-sharp-paren-token)
(define-token-maker-parameter sharp-vu8paren-token-maker	make-sharp-vu8paren-token)
(define-token-maker-parameter sharp-tick-token-maker		make-sharp-tick-token)
(define-token-maker-parameter sharp-back-tick-token-maker	make-sharp-back-tick-token)
(define-token-maker-parameter sharp-comma-at-token-maker	make-sharp-comma-at-token)
(define-token-maker-parameter sharp-comma-token-maker		make-sharp-comma-token)
(define-token-maker-parameter sharp-semicolon-token-maker	make-sharp-semicolon-token)

(define-token-maker-parameter line-comment-token-maker		make-line-comment-token)
(define-token-maker-parameter line-comment-noend-token-maker	make-line-comment-noend-token)
(define-token-maker-parameter open-nested-comment-token-maker	make-open-nested-comment-token)
(define-token-maker-parameter sharp-bang-r6rs-token-maker	make-sharp-bang-r6rs-token)
(define-token-maker-parameter sharp-bang-token-maker		make-sharp-bang-token)
(define-token-maker-parameter white-space-token-maker		make-white-space-token)
(define-token-maker-parameter line-ending-token-maker		make-line-ending-token)

(define-token-maker-parameter identifier-token-maker		make-identifier-token)
(define-token-maker-parameter boolean-token-maker		make-boolean-token)
(define-token-maker-parameter named-character-token-maker	make-named-character-token)
(define-token-maker-parameter hex-character-token-maker		make-hex-character-token)
(define-token-maker-parameter literal-character-token-maker	make-literal-character-token)
(define-token-maker-parameter number-token-maker		make-number-token)


;;;; done

)

;;; end of file
