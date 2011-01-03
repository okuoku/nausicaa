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
(library (nausicaa language classes helpers)
  (export
    %variable-name->Setter-name
    %variable-name->Getter-name
    %make-fields-accessor-of-transformer
    %make-fields-mutator-of-transformer
    %make-with-field-class-bindings
    %list-of-unique-field-types
    %detect-recursive-type-in-fields
    %reverse-inheritance-hierarchy-identifiers)
  (import (rnrs)
    (nausicaa language classes internal-auxiliary-syntaxes)
    (nausicaa language identifier-properties)
    (nausicaa language syntax-utilities))


(define (%variable-name->Setter-name variable-name-stx)
  (identifier-suffix variable-name-stx ".__nausicaa_private_Setter_identifier_syntax"))

(define (%variable-name->Getter-name variable-name-stx)
  (identifier-suffix variable-name-stx ".__nausicaa_private_Getter_identifier_syntax"))


;;;; accessor-of and mutator-of macro transformers generation

(define (%make-fields-accessor-of-transformer class-identifier fields virtual-fields synner)
  ;;Build and return  a syntax object holding a  lambda function; this
  ;;lambda function is the transformer used to retrieve field accessor
  ;;functions from the class definition given the field name.
  ;;
  ;;FIELDS and VIRTUAL-FIELDS must be syntax objects holding a list of
  ;;field specifications in the following format:
  ;;
  ;;    (mutable   ?field ?accessor ?mutator ?field-class ...)
  ;;    (immutable ?field ?accessor ?field-class ...)
  ;;
  ;;the  order of  the field  specifications must  match the  order of
  ;;fields in the RTD definition.
  ;;
  ;;SYNNER must  be the closure  used to raise  a syntax violation  if a
  ;;parse  error happens;  it  must accept  two  arguments: the  message
  ;;string, the subform.
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

      (((mutable ?field ?accessor ?mutator ?field-class ...) . ?clauses)
       (loop (cons #'((?field) #'?accessor) case-branches)
	     (+ 1 field-index)
	     #'?clauses))

      (((immutable ?field ?accessor ?field-class ...) . ?clauses)
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
  ;;    (mutable   ?field ?accessor ?mutator ?field-class ...)
  ;;    (immutable ?field ?accessor ?field-class ...)
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

      (((mutable ?field ?accessor ?mutator ?field-class ...) . ?clauses)
       (loop (cons #'((?field) #'?mutator) case-branches)
	     (+ 1 field-index)
	     #'?clauses))

      (((immutable ?field ?accessor ?field-class ...) . ?clauses)
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


;;;; field class helpers

(define (%make-with-field-class-bindings fields virtual-fields synner)
  ;;Build and  return the list of  bindings for a  WITH-CLASS use, which
  ;;define the bindings of typed fields.
  ;;
  ;;FIELDS and VIRTUAL-FIELDS must be syntax objects holding a list of
  ;;field specifications in the following format:
  ;;
  ;;    (mutable   ?field ?accessor ?mutator ?field-class ...)
  ;;    (immutable ?field ?accessor ?field-class ...)
  ;;
  ;;SYNNER must  be the closure  used to raise  a syntax violation  if a
  ;;parse  error happens;  it  must accept  two  arguments: the  message
  ;;string, the subform.
  ;;
  (let loop ((fields	#`(#,@fields #,@virtual-fields))
	     (bindings	'()))
    (syntax-case fields (mutable immutable)
      (()
       bindings)

      (((mutable ?field ?accessor ?mutator ?field-class ...) . ?fields)
       (loop #'?fields (cons #'(?field ?field-class ...) bindings)))

      (((immutable ?field ?accessor ?field-class ...) . ?fields)
       (loop #'?fields (cons #'(?field ?field-class ...) bindings)))

      ((?spec . ?fields)
       (synner "invalid field specification while building typed fields bindings"
		#'?spec)))))

(define (%list-of-unique-field-types fields virtual-fields tail synner)
  ;;Build and return a list of identifiers representing all the types of
  ;;fields and virtual  fields in a class or  label defintion; this list
  ;;is used to detect recursive  types.  The returned list is guaranteed
  ;;not to have duplicates, and it enforces no particular order.
  ;;
  ;;FIELDS and VIRTUAL-FIELDS must be syntax objects holding a list of
  ;;field specifications in the following format:
  ;;
  ;;    (mutable   ?field ?accessor ?mutator ?field-class ...)
  ;;    (immutable ?field ?accessor ?field-class ...)
  ;;
  ;;TAIL must be  null or a proper list  of identifiers representing the
  ;;field types of the superclass or superlabel; it will become the tail
  ;;of the returned list.
  ;;
  ;;SYNNER must  be the closure  used to raise  a syntax violation  if a
  ;;parse  error happens;  it  must accept  two  arguments: the  message
  ;;string, the subform.
  ;;
  (let loop ((fields	#`(#,@fields #,@virtual-fields))
	     (types	tail))
    (syntax-case fields (mutable immutable)
      (()
       (begin
	 (delete-duplicated-identifiers types)))

      (((mutable ?field ?accessor ?mutator . ?field-classes) . ?fields)
       (loop #'?fields (syntax->list #'?field-classes types)))

      (((immutable ?field ?accessor . ?field-classes) . ?fields)
       (loop #'?fields (syntax->list #'?field-classes types)))

      ((?spec . ?fields)
       (synner "invalid field specification while verifying recursive types" #'?spec))
      )))

(define (%detect-recursive-type-in-fields thing field-classes synner)
  ;;Look  for   the  identifier  THING   in  the  list   of  identifiers
  ;;FIELD-CLASSES; if it is there raise a syntax violation.
  ;;
  ;;SYNNER must  be the closure  used to raise  a syntax violation  if a
  ;;parse  error happens;  it  must accept  two  arguments: the  message
  ;;string, the subform.
  ;;
  (let ((c (exists (lambda (c)
		     (and (free-identifier=? c thing) c))
	     field-classes)))
    (when c
      (synner "detected recursive type" c))))


;;;; miscellaneous helpers

(define (%reverse-inheritance-hierarchy-identifiers thing)
  ;;Given  the identifier  THING  representing a  class  or label  name,
  ;;return  the   list  of  identifiers   representing  its  inheritance
  ;;hierarchy;  the list  does include  the <TOP>  class/label  which is
  ;;always the root of classes and labels.
  ;;
  ;;The returned list is from the topmost superclass to the lowest class
  ;;and ends with THING itself.
  ;;
  (let loop ((parents `(,thing)))
    (let ((p (lookup-identifier-property (car parents) #':superclass-property)))
      (if p
	  (loop (cons p parents))
	parents))))


;;;; done

)

;;; end of file