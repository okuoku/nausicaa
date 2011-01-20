;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: lexer for R6RS programs and libraries
;;;Date: Wed Dec 22, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa r6rs lexer)
  (export
    r6rs-lexer-table make-token-lexer table comments sharpbang blanks

    r6rs-nested-comment-lexer-table	r6rs-line-comment-lexer-table
    r6rs-string-lexer-table		r6rs-character-lexer-table
    r6rs-identifier-lexer-table		r6rs-number-lexer-table

    read-character			read-identifier
    read-number				read-line-comment
    read-string				read-string*
    read-nested-comment			read-nested-comment*

;;;all the following are reexported from (nausicaa r6rs lexeme-processing)
    current-input-source

    ;; token maker parameters		 default token makers
    lexical-error-token-maker		make-lexical-error-token
    eoi-token-maker			make-eoi-token

    open-paren-token-maker		make-open-paren-token
    close-paren-token-maker		make-close-paren-token
    open-bracket-token-maker		make-open-bracket-token
    close-bracket-token-maker		make-close-bracket-token
    tick-token-maker			make-tick-token
    back-tick-token-maker		make-back-tick-token
    comma-at-token-maker		make-comma-at-token
    comma-token-maker			make-comma-token
    dot-token-maker			make-dot-token
    double-quote-token-maker		make-double-quote-token
    sharp-paren-token-maker		make-sharp-paren-token
    sharp-vu8-paren-token-maker		make-sharp-vu8-paren-token
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
    string-token-maker			make-string-token
    nested-comment-token-maker		make-nested-comment-token

    )
  (import (nausicaa)
    (nausicaa r6rs lexer-table)
    (nausicaa r6rs nested-comment-lexer-table)
    (nausicaa r6rs line-comment-lexer-table)
    (nausicaa r6rs string-lexer-table)
    (nausicaa r6rs character-lexer-table)
    (nausicaa r6rs identifier-lexer-table)
    (nausicaa r6rs number-lexer-table)
    (nausicaa r6rs lexeme-processing)
    (nausicaa parser-tools)
    (prefix (nausicaa silex lexer) lex.))


;;;; lexer thunk makers

(define-auxiliary-syntaxes
  table
  comments
  sharpbang
  blanks)

(define-maker (make-token-lexer IS)
  %make-token-lexer
  ((table	r6rs-lexer-table)
   (comments	#f)
   (sharpbang	#f)
   (blanks	#f)))

(define (%make-token-lexer IS lexer-table comments? sharpbang? blanks?)
  ;;This is  a full R6RS lexer  function maker.  Given  an input system,
  ;;return  a lexer  function producing  tokens.  The  boolean arguments
  ;;allow the selection of tokens to discard.
  ;;
  ;;If COMMENTS?  is true: discard tokens with  category LINECOMMENT and
  ;;the   result   of   calling   the   function   referenced   by   the
  ;;NESTED-COMMENT-TOKEN-MAKER parameter  used to read  nested comments.
  ;;The default value is false.
  ;;
  ;;If SHARPBANG?  is false:  discard tokens with category SHARPBANGR6RS
  ;;and SHARPBANG.  The default value is false.
  ;;
  ;;If BLANKS?   is false: discard  tokens with category  WHITESPACE and
  ;;LINEENDING.  The default value is false.
  ;;
  ;;Notice that the sharp-semicolon token is still returned by the lexer
  ;;thunk: it is impossible to discard datums at the lexer level, it has
  ;;to be done by the parser.
  ;;
  (let ((lexer (lex.make-lexer lexer-table IS)))
    (lambda ()
      (let next (((T <lexical-token>) (lexer)))
	(define (%string-token)
	  (let ((S (read-string* IS)))
	    (if (string? S)
		((string-token-maker) (lex.lexer-get-func-getc IS) (lex.lexer-get-func-ungetc IS)
		 S T.location.line T.location.column T.location.offset)
	      S)))
	(define (%nested-comment-token)
	  (let ((S (read-nested-comment* IS)))
	    (if (string? S)
		((nested-comment-token-maker) (lex.lexer-get-func-getc IS) (lex.lexer-get-func-ungetc IS)
		 S T.location.line T.location.column T.location.offset)
	      S)))
	(cond (T.special? T)
	      ((eq? T.category 'DOUBLEQUOTE)
	       (%string-token))
	      ((memq T.category '(WHITESPACE LINEENDING))
	       (if blanks? T (next (lexer))))
	      ((memq T.category '(SHARPBANGR6RS SHARPBANG))
	       (if sharpbang? T (next (lexer))))
	      ((eq? T.category 'LINECOMMENT)
	       (if comments? T (next (lexer))))
	      ((eq? T.category 'ONESTEDCOMMENT)
	       (let (((R <lexical-token>) (%nested-comment-token)))
		 (if (or R.special? comments?) R (next (lexer)))))
	      (else T))))))


;;;; reading strings

(define (read-string IS)
  ;;Given  an input  system  from  which a  double  quote character  has
  ;;already  been consumed,  read  characters composing  an R6RS  string
  ;;stopping at the ending double quote.  Return the Scheme string.
  ;;
  ;;If an error occurs reading  the string: a condition object is raised
  ;;with  components &lexical,  &message, &who,  &irritants;  the single
  ;;value in the &irritants list is the string that caused the error.
  ;;
  ;;If end of  input is found reading the string:  a condition object is
  ;;raised  with components &lexical,  &message, &who,  &irritants.  The
  ;;single value in the irritants list is the EOF object.
  ;;
  (let-values (((port getter)	(open-string-output-port))
	       ((lexer)		(lex.make-lexer r6rs-string-lexer-table IS)))
    (let next (((T <lexical-token>) (lexer)))
      (define (%error message)
	(raise
	 (condition (make-lexical-violation)
		    (make-message-condition message)
		    (make-who-condition 'read-string)
		    (make-irritants-condition (list T.value)))))
      (cond (T.end-of-input?
	     (%error "end of input found while reading string"))
	    (T.lexer-error?
	     (%error "lexical violation while reading string"))
	    ((eq? T 'STRING)
	     (getter))
	    (else
	     (display T port)
	     (next (lexer)))))))

(define (read-string* IS)
  ;;Like READ-STRING but do not raise exceptions.  Given an input system
  ;;from which a double quote  character has already been consumed, read
  ;;characters composing  an R6RS string  stopping at the  ending double
  ;;quote.  Return the Scheme string.
  ;;
  ;;If an  error occurs reading the  string: return the  return value of
  ;;the function referenced  by the parameter LEXICAL-ERROR-TOKEN-MAKER,
  ;;which must be a <lexical-token> having *lexer-error* as category.
  ;;
  ;;If the end  of input is found reading the  string: return the return
  ;;value of  the function referenced by  the parameter EOF-TOKEN-MAKER,
  ;;which must be a <lexical-token> having *eoi* as category.
  ;;
  (let-values (((port getter)	(open-string-output-port))
	       ((lexer)		(lex.make-lexer r6rs-string-lexer-table IS)))
    (let next (((T <lexical-token>) (lexer)))
      (cond (T.end-of-input?	T)
	    (T.lexer-error?	T)
	    ((eq? T 'STRING)	(getter))
	    (else
	     (display T port)
	     (next (lexer)))))))


;;;; reading characters

(define (read-character IS)
  ;;Given an input system, read  a single character datum compliant with
  ;;R6RS.  Return the Scheme character.
  ;;
  ;;If  an error  occurs reading  the character:  a condition  object is
  ;;raised  with components  &lexical, &message,  &who,  &irritants; the
  ;;single value  in the &irritants list  is the string  that caused the
  ;;error.
  ;;
  ;;If end of  input is found reading the  character: a condition object
  ;;is raised with components &lexical, &message, &who, &irritants.  The
  ;;single value in the irritants list is the EOF object.
  ;;
  (let (((T <lexical-token>) ((lex.make-lexer r6rs-character-lexer-table IS))))
    (define (%error message)
      (raise
       (condition (make-lexical-violation)
		  (make-message-condition message)
		  (make-who-condition 'read-string)
		  (make-irritants-condition (list T.value)))))
    (cond (T.end-of-input?
	   (%error "end of input found while reading character"))
	  (T.lexer-error?
	   (%error "lexical violation while reading character"))
	  (else T))))


;;;; reading identifiers

(define (read-identifier IS)
  ;;Given an input system, read an identifier datum compliant with R6RS.
  ;;Return the Scheme symbol.
  ;;
  ;;If an  error occurs  reading the identifier:  a condition  object is
  ;;raised  with components  &lexical, &message,  &who,  &irritants; the
  ;;single value  in the &irritants list  is the string  that caused the
  ;;error.
  ;;
  ;;If end of input is  found reading the identifier: a condition object
  ;;is raised with components &lexical, &message, &who, &irritants.  The
  ;;single value in the irritants list is the EOF object.
  ;;
  (let (((T <lexical-token>) ((lex.make-lexer r6rs-identifier-lexer-table IS))))
    (define (%error message)
      (raise
       (condition (make-lexical-violation)
		  (make-message-condition message)
		  (make-who-condition 'read-identifier)
		  (make-irritants-condition (list T.value)))))
    (cond (T.end-of-input?
	   (%error "end of input found while reading identifier"))
	  (T.lexer-error?
	   (%error "lexical violation while reading identifier"))
	  (else T))))


;;;; reading numbers

(define (read-number IS)
  ;;Given  an input  system, read  a number  datum compliant  with R6RS.
  ;;Return the Scheme number.
  ;;
  ;;If an error occurs reading  the number: a condition object is raised
  ;;with  components &lexical,  &message, &who,  &irritants;  the single
  ;;value in the &irritants list is the string that caused the error.
  ;;
  ;;If end of  input is found reading the number:  a condition object is
  ;;raised  with components &lexical,  &message, &who,  &irritants.  The
  ;;single value in the irritants list is the EOF object.
  ;;
  (let (((T <lexical-token>) ((lex.make-lexer r6rs-number-lexer-table IS))))
    (define (%error message)
      (raise
       (condition (make-lexical-violation)
		  (make-message-condition message)
		  (make-who-condition 'read-number)
		  (make-irritants-condition (list T.value)))))
    (cond (T.end-of-input?
	   (%error "end of input found while reading number"))
	  (T.lexer-error?
	   (%error "lexical violation while reading number"))
	  (else T))))


;;;; reading nested comments

(define (read-nested-comment IS)
  ;;Given  an input  system from  which the  opening sequence  of nested
  ;;comments "#|"  has already been consumed,  read characters composing
  ;;an R6RS  nested comment matching sequence "|#".   Return the comment
  ;;as Scheme string enclosed in "#|" and "|#" sequences.
  ;;
  ;;If an error occurs reading the nested comment: a condition object is
  ;;raised  with components  &lexical, &message,  &who,  &irritants; the
  ;;single value  in the &irritants list  is the string  that caused the
  ;;error.  Notice that this should never happen.
  ;;
  ;;If end  of input  is found reading  the nested comment:  a condition
  ;;object   is  raised  with   components  &lexical,   &message,  &who,
  ;;&irritants.   The single  value in  the  irritants list  is the  EOF
  ;;object.
  ;;
  (let-values (((port getter)	(open-string-output-port))
	       ((lexer)		(lex.make-lexer r6rs-nested-comment-lexer-table IS))
	       ((count)		1))
    (put-char port #\#)
    (put-char port #\|)
    (let next (((T <lexical-token>) (lexer)))
      (define (%error message)
	(raise
	 (condition (make-lexical-violation)
		    (make-message-condition message)
		    (make-who-condition 'read-nested-comment)
		    (make-irritants-condition (list T.value)))))
      (cond (T.end-of-input?
	     (%error "end of input found while reading nested comment"))
	    (T.lexer-error? ;this should never happen
	     (%error "lexical violation while reading nested comment"))
	    ((eq? T 'CLOSE)
	     (decr! count)
	     (put-char port #\|)
	     (put-char port #\#)
	     (if (zero? count)
		 (getter)
	       (next (lexer))))
	    ((eq? T 'OPEN)
	     (incr! count)
	     (put-char port #\#)
	     (put-char port #\|)
	     (next (lexer)))
	    (else
	     (display T port)
	     (next (lexer)))))))

(define (read-nested-comment* IS)
  ;;Like  READ-NESTED-COMMENT but  do  not raise  exceptions.  Given  an
  ;;input system from which the opening sequence of nested comments "#|"
  ;;has already been consumed,  read characters composing an R6RS nested
  ;;comment matching sequence "|#".  Return the comment as Scheme string
  ;;enclosed in "#|" and "|#" sequences.
  ;;
  ;;If an error  occurs reading the comment: return  the return value of
  ;;the function referenced  by the parameter LEXICAL-ERROR-TOKEN-MAKER,
  ;;which must be a <lexical-token> having *lexer-error* as category.
  ;;
  ;;If the end of input is  found reading the comment: return the return
  ;;value of  the function referenced by  the parameter EOF-TOKEN-MAKER,
  ;;which must be a <lexical-token> having *eoi* as category.
  ;;
  (let-values (((port getter)	(open-string-output-port))
	       ((lexer)		(lex.make-lexer r6rs-nested-comment-lexer-table IS))
	       ((count)		1))
    (put-char port #\#)
    (put-char port #\|)
    (let next (((T <lexical-token>) (lexer)))
      (cond (T.end-of-input?	T)
	    (T.lexer-error?	T) ;this should never happen
	    ((eq? T 'CLOSE)
	     (decr! count)
	     (put-char port #\|)
	     (put-char port #\#)
	     (if (zero? count)
		 (getter)
	       (next (lexer))))
	    ((eq? T 'OPEN)
	     (incr! count)
	     (put-char port #\#)
	     (put-char port #\|)
	     (next (lexer)))
	    (else
	     (display T port)
	     (next (lexer)))))))


;;;; reading line comments

(define (read-line-comment IS)
  ;;Given  an  input system,  read  characters  composing  an R6RS  line
  ;;comment.  Return the string representing the comment.
  ;;
  ;;If an error  occurs reading the line comment:  a condition object is
  ;;raised  with components  &lexical, &message,  &who,  &irritants; the
  ;;single value  in the &irritants list  is the string  that caused the
  ;;error.
  ;;
  ;;If  end of  input is  found reading  the line  comment:  a condition
  ;;object   is  raised  with   components  &lexical,   &message,  &who,
  ;;&irritants.   The single  value in  the  irritants list  is the  EOF
  ;;object.
  ;;
  (let (((T <lexical-token>) ((lex.make-lexer r6rs-line-comment-lexer-table IS))))
    (define (%error message)
      (raise
       (condition (make-lexical-violation)
		  (make-message-condition message)
		  (make-who-condition 'read-line-comment)
		  (make-irritants-condition (list T.value)))))
    (cond (T.end-of-input?
	   (%error "end of input found while reading line comment"))
	  (T.lexer-error?
	   (%error "lexical violation while reading line comment"))
	  (else T))))


;;;; done

)

;;; end of file
