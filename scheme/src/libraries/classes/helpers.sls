;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: miscellaneous helper functions
;;;Date: Tue Nov  2, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This  program  is free  software:  you  can redistribute  it
;;;and/or modify it  under the terms of the  GNU General Public
;;;License as published by the Free Software Foundation, either
;;;version  3 of  the License,  or (at  your option)  any later
;;;version.
;;;
;;;This  program is  distributed in  the hope  that it  will be
;;;useful, but  WITHOUT ANY WARRANTY; without  even the implied
;;;warranty  of  MERCHANTABILITY or  FITNESS  FOR A  PARTICULAR
;;;PURPOSE.   See  the  GNU  General Public  License  for  more
;;;details.
;;;
;;;You should  have received a  copy of the GNU  General Public
;;;License   along   with    this   program.    If   not,   see
;;;<http://www.gnu.org/licenses/>.
;;;


#!r6rs
(library (classes helpers)
  (export
    %variable-name->Setter-name
    %variable-name->Getter-name
    %make-fields-accessor-of-transformer
    %make-fields-mutator-of-transformer)
  (import (rnrs))


(define (%variable-name->Setter-name variable-name-stx)
  (datum->syntax variable-name-stx
		 (string->symbol
		  (string-append (symbol->string (syntax->datum variable-name-stx))
				 ".__nausicaa_private_Setter_identifier_syntax"))))

(define (%variable-name->Getter-name variable-name-stx)
  (datum->syntax variable-name-stx
		 (string->symbol
		  (string-append (symbol->string (syntax->datum variable-name-stx))
				 ".__nausicaa_private_Getter_identifier_syntax"))))


;;;; accessor-of and mutator-of macro transformers generation

(define (%make-fields-accessor-of-transformer class-identifier fields virtual-fields synner)
  ;;Build and return  a syntax object holding a  lambda function; this
  ;;lambda function is the transformer used to retrieve field accessor
  ;;functions from the class definition given the field name.
  ;;
  ;;FIELDS and VIRTUAL-FIELDS must be syntax objects holding a list of
  ;;field specifications in the following format:
  ;;
  ;;    (mutable   ?field ?accessor ?mutator)
  ;;    (immutable ?field ?accessor)
  ;;
  ;;the  order of  the field  specifications must  match the  order of
  ;;fields in the RTD definition.
  ;;
  ;;Example, for a class like:
  ;;
  ;;  (define-class <alpha> (fields (mutable a) (mutable b)))
  ;;
  ;;we want to generate this macro transformer:
  ;;
  ;;  (lambda (stx)
  ;;    (syntax-case stx ()
  ;;      ((_ ?slot-name)
  ;;       (case (syntax->datum #'?slot-name)
  ;;         ((a) #'<alpha>-a)
  ;;         ((b) #'<alpha>-b)
  ;;         (else
  ;;          (syntax-violation '<alpha> "unknown class field"
  ;;            (syntax->datum stx)
  ;;            (syntax->datum #'?slot-name)))))))
  ;;
  (let loop ((case-branches	'())
	     (field-index	0)
	     (fields		#`(#,@fields #,@virtual-fields)))
    (syntax-case fields (mutable immutable)
      (()
       (%make-field-accessor-or-mutator-transformer-function class-identifier case-branches))

      (((mutable ?field ?accessor ?mutator) . ?clauses)
       (loop (cons #'((?field) #'?accessor) case-branches)
	     (+ 1 field-index)
	     #'?clauses))

      (((immutable ?field ?accessor) . ?clauses)
       (loop (cons #'((?field) #'?accessor) case-branches)
	     (+ 1 field-index)
	     #'?clauses))

      ((?spec . ?clauses)
       (synner "invalid field specification while building \"field accessor of\" transformer"
		#'?spec)))))

(define (%make-fields-mutator-of-transformer class-identifier fields virtual-fields synner)
  ;;Build and return  a syntax object holding a  lambda function; this
  ;;lambda function is the  transformer used to retrieve field mutator
  ;;functions from the class definition given the field name.
  ;;
  ;;FIELDS and VIRTUAL-FIELDS must be syntax objects holding a list of
  ;;field specifications in the following format:
  ;;
  ;;    (mutable   ?field ?accessor ?mutator)
  ;;    (immutable ?field ?accessor)
  ;;
  ;;the  order of  the field  specifications must  match the  order of
  ;;fields in the RTD definition.
  ;;
  ;;Example, for a class like:
  ;;
  ;;  (define-class <alpha> (fields (mutable a) (mutable b) (immutable c)))
  ;;
  ;;we want to generate this macro transformer:
  ;;
  ;;  (lambda (stx)
  ;;    (syntax-case stx ()
  ;;      ((_ ?slot-name)
  ;;       (case (syntax->datum #'?slot-name)
  ;;         ((a) #'<alpha>-a-set!)
  ;;         ((b) #'<alpha>-b-set!)
  ;;         (else
  ;;          (syntax-violation '<alpha> "unknown class field"
  ;;            (syntax->datum stx)
  ;;            (syntax->datum #'?slot-name)))))))
  ;;
  (let loop ((case-branches	'())
	     (field-index	0)
	     (fields		#`(#,@fields #,@virtual-fields)))
    (syntax-case fields (mutable immutable)
      (()
       (%make-field-accessor-or-mutator-transformer-function class-identifier case-branches))

      (((mutable ?field ?accessor ?mutator) . ?clauses)
       (loop (cons #'((?field) #'?mutator) case-branches)
	     (+ 1 field-index)
	     #'?clauses))

      (((immutable ?field ?accessor) . ?clauses)
       (loop case-branches
	     (+ 1 field-index)
	     #'?clauses))

      ((?spec . ?clauses)
       (synner "invalid field specification while building \"field mutator of\" transformer"
		#'?spec)))))

(define (%make-field-accessor-or-mutator-transformer-function class-identifier case-branches)
  ;;Auxiliary  function  for  %MAKE-FIELDS-MUTATOR-OF-TRANSFORMER  and
  ;;%MAKE-FIELDS-ACCESSOR-OF-TRANSFORMER.  Build and return the actual
  ;;transformer syntax object.
  ;;
  (if (null? case-branches)
      #`(lambda (stx)
	  (syntax-violation (quote #,class-identifier) "class has no mutable fields" stx))
    #`(lambda (stx)
	(syntax-case stx ()
	  ((_ ?slot-name)
	   (case (syntax->datum #'?slot-name)
	     #,@case-branches
	     (else
	      (syntax-violation (quote #,class-identifier)
		"unknown class field"
		(syntax->datum stx)
		(syntax->datum #'?slot-name)))))))))


;;;; done

)

;;; end of file
