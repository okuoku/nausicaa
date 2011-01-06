;;; -*- coding: utf-8 -*-
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
    r6rs-lexer-table
    r6rs-nested-comment-lexer-table r6rs-line-comment-lexer-table
    r6rs-string-lexer-table r6rs-character-lexer-table
    r6rs-identifier-lexer-table r6rs-number-lexer-table

    read-string read-string* read-character read-identifier read-number
    read-nested-comment read-nested-comment*
    read-line-comment make-token-lexer)
  (import (nausicaa)
    (nausicaa r6rs lexer-table)
    (nausicaa r6rs nested-comment-lexer-table)
    (nausicaa r6rs line-comment-lexer-table)
    (nausicaa r6rs string-lexer-table)
    (nausicaa r6rs character-lexer-table)
    (nausicaa r6rs identifier-lexer-table)
    (nausicaa r6rs number-lexer-table)
    (nausicaa r6rs lexeme-processing)
    (nausicaa parser-tools lexical-token)
    (nausicaa silex lexer))


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
	       ((lexer)		(lexer-make-lexer r6rs-string-lexer-table IS)))
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
  (let (((T <lexical-token>) ((lexer-make-lexer r6rs-character-lexer-table IS))))
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
  (let (((T <lexical-token>) ((lexer-make-lexer r6rs-identifier-lexer-table IS))))
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
  (let (((T <lexical-token>) ((lexer-make-lexer r6rs-number-lexer-table IS))))
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
	       ((lexer)		(lexer-make-lexer r6rs-nested-comment-lexer-table IS))
	       ((count)		1))
    (display "#|" port)
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
	     (display "|#" port)
	     (if (zero? count)
		 (getter)
	       (next (lexer))))
	    ((eq? T 'OPEN)
	     (incr! count)
	     (display "#|" port)
	     (next (lexer)))
	    (else
	     (display T port)
	     (next (lexer)))))))

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
  (let (((T <lexical-token>) ((lexer-make-lexer r6rs-line-comment-lexer-table IS))))
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


(define make-token-lexer
  ;;This is the full R6RS  lexer function maker.  Given an input system,
  ;;return a lexer function producing tokens.
  ;;
  (case-lambda
   ((IS)
    (make-token-lexer IS r6rs-lexer-table))
   ((IS lexer-table)
    (let ((lexer (lexer-make-lexer lexer-table IS)))
      (lambda ()
	(let next (((T <lexical-token>) (lexer)))
	  (define (%string-token)
	    (let ((S (read-string* IS)))
	      (if (string? S)
		  ((string-token-maker) (lexer-get-func-getc IS) (lexer-get-func-ungetc IS)
		   S T.location.line T.location.column T.location.offset)
		S)))
	  (define (%nested-comment-token)
	    (let ((S (read-nested-comment* IS)))
	      (if (string? S)
		  ((nested-comment-token-maker) (lexer-get-func-getc IS) (lexer-get-func-ungetc IS)
		   S T.location.line T.location.column T.location.offset)
		S)))
	  (cond (T.special? T)
		((eq? T.category 'WHITESPACE)
		 (next (lexer)))
                ((eq? T.category 'LINEENDING)
                 (next (lexer)))
		((eq? T.category 'DOUBLEQUOTE)
		 (%string-token))
		((eq? T.category 'ONESTEDCOMMENT)
		 (%nested-comment-token))
		(else T))))))))

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
	       ((lexer)		(lexer-make-lexer r6rs-string-lexer-table IS)))
    (let next (((T <lexical-token>) (lexer)))
      (cond (T.end-of-input?	T)
	    (T.lexer-error?	T)
	    ((eq? T 'STRING)	(getter))
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
	       ((lexer)		(lexer-make-lexer r6rs-nested-comment-lexer-table IS))
	       ((count)		1))
    (display "#|" port)
    (let next (((T <lexical-token>) (lexer)))
      (cond (T.end-of-input?	T)
	    (T.lexer-error?	T) ;this should never happen
	    ((eq? T 'CLOSE)
	     (decr! count)
	     (display "|#" port)
	     (if (zero? count)
		 (getter)
	       (next (lexer))))
	    ((eq? T 'OPEN)
	     (incr! count)
	     (display "#|" port)
	     (next (lexer)))
	    (else
	     (display T port)
	     (next (lexer)))))))


;;;; done

)

;;; end of file
