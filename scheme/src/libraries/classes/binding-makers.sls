;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: let-syntax binding makers for dot notation
;;;Date: Tue Nov  2, 2010
;;;
;;;Abstract
;;;
;;;	This library defines functions that build LET-SYNTAX bindings to
;;;	be  used  in  defining  dot  notation.   This  stuff  is  in  an
;;;	independent library because it  is needed by the generated code,
;;;	not by the (classes) library itself.
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


#!r6rs
(library (classes binding-makers)
  (export
    make-field-bindings
    make-method-bindings
    make-setter-getter-bindings
    )
  (import (rnrs)
    (syntax-utilities)
    (classes helpers))


(define (make-field-bindings use-dot-notation? variable-stx clauses-stx synner)
  ;;Build and return a list of lists representing LET-SYNTAX bindings to
  ;;be used to access the fields of a class or label.
  ;;
  ;;USE-DOT-NOTATION? must be a boolean value: true if dot notation must
  ;;be used, false if the field name identifiers must be used directly.
  ;;
  ;;VARIABLE-STX  must be  the identifier  bound to  the class  or label
  ;;instance;  if  USE-DOT-NOTATION?   is  false,  VARIABLE-STX  is  the
  ;;identifier of the "this" method argument.
  ;;
  ;;CLAUSES-STX must  be the list  of clauses defining the  fields; each
  ;;clause must be in one of the forms:
  ;;
  ;;   (mutable   ?field ?accessor ?mutator)
  ;;   (immutable ?field ?accessor)
  ;;
  ;;SYNNER must be a function used to raise syntax violation errors with
  ;;the context of the caller.
  ;;
  (define (main)
    (assert (boolean? use-dot-notation?))
    (assert (identifier? variable-stx))
    (map (lambda (clause-stx)
    	   (make-single-field-binding clause-stx synner))
      (unwrap-syntax-object clauses-stx)))

  (define (make-single-field-binding clause-stx synner)
    (define (make-keyword field-stx)
      (if use-dot-notation?
	  (syntax-dot-notation-identifier variable-stx field-stx)
	;;If dot notation is off,  VARIABLE-STX is the identifier of the
	;;"this" method argument.
	;;
	;;Notice that  FIELD-STX was not introduced in  the same context
	;;of VARIABLE-STX,  so we have  to create a new  identifier with
	;;the  same   name  of  FIELD-STX   and  the  same   context  of
	;;VARIABLE-STX.
	(datum->syntax variable-stx (syntax->datum field-stx))))
    (syntax-case clause-stx (mutable immutable)
      ((mutable ?field ?accessor ?mutator)
       #`(#,(make-keyword #'?field)
	  (identifier-syntax
	   (_              (?accessor #,variable-stx))
	   ((set! _ ?expr) (?mutator  #,variable-stx ?expr)))))
      ((immutable ?field ?accessor)
       #`(#,(make-keyword #'?field)
	  (identifier-syntax
	   (?accessor #,variable-stx))))
      (_
       (synner "invalid syntax in field clause" clause-stx))))

  (main))


(define (make-method-bindings use-dot-notation variable-stx clauses-stx synner)
  ;;Build and return a list of lists representing LET-SYNTAX bindings to
  ;;be used to call the methods of a class or label.
  ;;
  ;;USE-DOT-NOTATION? must be a boolean value: true if dot notation must
  ;;be used, false if the method name identifiers must be used directly.
  ;;
  ;;VARIABLE-STX  must be  the identifier  bound to  the class  or label
  ;;instance;  if  USE-DOT-NOTATION?   is  false,  VARIABLE-STX  is  the
  ;;identifier of the "this" method argument.
  ;;
  ;;CLAUSE-STX must  be a syntax object  holding the list  of clauses in
  ;;the form:
  ;;
  ;;   ((?method ?function-name) ...)
  ;;
  ;;SYNNER must be a function used to raise syntax violation errors with
  ;;the context of the caller.
  ;;
  (define (main)
    (map (lambda (clause-stx)
	   (syntax-case clause-stx ()
	     ((?method ?function-name)
	      (make-single-method-binding use-dot-notation variable-stx
					  #'?method #'?function-name))
	     (_
	      (synner "invalid method specification clause" clause-stx))))
      (unwrap-syntax-object clauses-stx)))

  (define (make-single-method-binding use-dot-notation? variable-stx method-stx function-name-stx)
    #`(#,(if use-dot-notation?
	     (syntax-dot-notation-identifier variable-stx method-stx)
	   ;;If dot  notation is off, VARIABLE-STX is  the identifier of
	   ;;the "this" method argument.
	   ;;
	   ;;Notice  that  METHOD-STX was  not  introduced  in the  same
	   ;;context  of  VARIABLE-STX,  so  we  have to  create  a  new
	   ;;identifier with  the same name  of METHOD-STX and  the same
	   ;;context of VARIABLE-STX.
	   (datum->syntax variable-stx (syntax->datum method-stx)))
       (syntax-rules ()
	 ((_ ?arg (... ...))
	  (#,function-name-stx #,variable-stx ?arg (... ...))))))

  (main))


(define (make-setter-getter-bindings variable-stx Setter-stx Getter-stx)
  ;;Build and return a list of lists representing LET-SYNTAX bindings to
  ;;be used by the setter and getter macros.
  ;;
  ;;VARIABLE-STX  must be  the identifier  bound to  the class  or label
  ;;instance.
  ;;
  ;;SETTER-STX and  GETTER-STX must be  identifiers bound to  the setter
  ;;and getter function or macro.
  ;;
  ;;SYNNER must be a function used to raise syntax violation errors with
  ;;the context of the caller.
  ;;
  (define (main)
    (append (if (not (syntax->datum Setter-stx))
		'()
	      (list (make-Setter-binding variable-stx Setter-stx)))
	    (if (not (syntax->datum Getter-stx))
		'()
	      (list (make-Getter-binding variable-stx Getter-stx)))))

  (define (make-Setter-binding variable-stx setter-stx)
    #`(#,(%variable-name->Setter-name variable-stx)
       (syntax-rules ()
	 ((_ key0 key (... ...) value)
	  (#,setter-stx #,variable-stx key0 key (... ...) value)))))

  (define (make-Getter-binding variable-stx getter-stx)
    #`(#,(%variable-name->Getter-name variable-stx)
       (syntax-rules ()
	 ((_ key0 key (... ...))
	  (#,getter-stx #,variable-stx key0 key (... ...))))))

  (main))


;;;; done

)

;;; end of file
