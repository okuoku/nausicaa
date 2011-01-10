;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: miscellaneous helper functions
;;;Date: Tue Nov  2, 2010
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
(library (nausicaa language classes helpers)
  (export
    filter-and-compose-with-mixin-clauses
    validate-class-clauses
    validate-label-clauses
    (rename (validate-class-clauses validate-mixin-clauses))
    normalise-class-inheritance
    extract-super-properties-if-any
    variable-name->Setter-name
    variable-name->Getter-name
    make-fields-accessor-of-transformer
    make-fields-mutator-of-transformer
    make-with-field-class-bindings
    list-of-unique-field-types)
  (import (rnrs)
    (nausicaa language classes internal-auxiliary-syntaxes)
    (prefix (nausicaa language classes properties) prop.)
    (prefix (only (nausicaa language syntax-utilities)
		  delete-duplicate-identifiers
		  identifier-subst
		  identifier-suffix
		  syntax->list
		  unwrap-syntax-object
		  validate-definition-clauses)
	    synux.)
    (prefix (only (nausicaa language classes clause-parsers)
		  %collect-clause/mixins)
	    parser.)
    (for (nausicaa language auxiliary-syntaxes) (meta -1))
    (for (nausicaa language classes top) (meta -1)))


(define (filter-and-compose-with-mixin-clauses original-clauses identifier validate-clauses synner)
  ;;Compose the original  clauses from a class, label  or mixin with the
  ;;clauses from the requested mixins.
  ;;
  ;;In the  original clauses  from a class,  label or  mixin definition:
  ;;separate  the  MIXINS clauses  from  the  other  clauses.  For  each
  ;;required mixin: retrieve its  clasuses, specialise them and add them
  ;;to the non-MIXINS original clauses.
  ;;
  ;;Return list of definition clauses joined with the mixin clauses; the
  ;;list of mixin identifiers to be used for inspection purposes.
  ;;
  ;;ORIGINAL-CLAUSES must be an unwrapped syntax object representing the
  ;;original clauses in the definition of a class, label or mixin.
  ;;
  ;;IDENTIFIER must be  the identifier of the receiving  class, label or
  ;;mixin.
  ;;
  ;;VALIDATE-CLAUSES  must be a  function used  to validate  the clauses
  ;;after each mixin has been joined.
  ;;
  ;;SYNNER must  be the closure  used to raise  a syntax violation  if a
  ;;parse  error  occurs; it  must  accept  two  arguments: the  message
  ;;string, the subform.
  ;;
  (let ((mixin-identifiers (parser.%collect-clause/mixins original-clauses synner)))
    (let loop ((clauses original-clauses)
	       (mixins  mixin-identifiers))
      (if (null? mixins)
	  (values clauses mixin-identifiers)
	(let ((clauses (append clauses
			       (specialise-mixin-clauses (car mixins) identifier synner))))
	  ;;After each composition validate the clauses.
	  (validate-clauses clauses synner)
	  (loop clauses (cdr mixins)))))))


;;;; clause definition validation

(define (validate-class-clauses clauses synner)
  ;;Validate the definition clauses  for DEFINE-CLASS; CLAUSES must be a
  ;;syntax object holding the definition clauses.
  ;;
  ;;SYNNER must  be the closure  used to raise  a syntax violation  if a
  ;;parse  error  occurs; it  must  accept  two  arguments: the  message
  ;;string, the subform.
  ;;
  (synux.validate-definition-clauses
   ;; mandatory keywords
   '()
   ;; optional keywords
   (list #'parent #'sealed #'opaque #'parent-rtd #'nongenerative #'fields #'protocol
	 #'inherit #'predicate #'maker #'maker-transformer #'custom-maker
	 #'setter #'getter #'bindings
	 #'public-protocol #'maker-protocol #'superclass-protocol
	 #'virtual-fields #'methods #'method #'method-syntax
	 #'mixins #'satisfies)
   ;; at most once keywords
   (list #'parent #'sealed #'opaque #'parent-rtd #'nongenerative
	 #'inherit #'predicate #'maker #'maker-transformer #'custom-maker
	 #'setter #'getter #'bindings
	 #'protocol #'public-protocol #'maker-protocol #'superclass-protocol)
   ;; mutually exclusive keywords sets
   (list (list #'inherit #'parent #'parent-rtd)
	 (list #'maker #'custom-maker)
	 (list #'maker-transformer #'custom-maker))
   clauses synner))

(define (validate-label-clauses clauses synner)
  ;;Validate the definition clauses  for DEFINE-LABEL; CLAUSES must be a
  ;;syntax object holding the definition clauses.
  ;;
  ;;SYNNER must  be the closure  used to raise  a syntax violation  if a
  ;;parse  error  occurs; it  must  accept  two  arguments: the  message
  ;;string, the subform.
  ;;
  (synux.validate-definition-clauses
   ;; mandatory keywords
   '()
   ;; optional keywords
   (list #'inherit #'predicate #'setter #'getter #'bindings
	 #'virtual-fields #'methods #'method #'method-syntax
	 #'custom-maker #'mixins #'satisfies)
   ;; at most once keywords
   (list #'inherit #'predicate #'setter #'getter #'bindings
	 #'custom-maker)
   ;; mutually exclusive keywords sets
   '()
   clauses synner))


(define (specialise-mixin-clauses mixin-identifier destination-identifier synner)
  ;;Given   a   source  mixin   identifier   name   and  a   destination
  ;;class/label/mixin  identifier  name:  retrieve  the clauses  of  the
  ;;mixin, substitute the occurrences  of the source identifier with the
  ;;destination  identifier, return  the result  as an  unwrapped syntax
  ;;object.
  ;;
  ;;It is an error if the mixin identifier is undefined.
  ;;
  ;;SYNNER must  be the closure  used to raise  a syntax violation  if a
  ;;parse  error  occurs; it  must  accept  two  arguments: the  message
  ;;string, the subform.
  ;;
  (let ((mixin-clauses (prop.mixin-clauses-ref mixin-identifier)))
    (if mixin-clauses
	(synux.unwrap-syntax-object ;the receiving end expects an unwrapped object
	 (synux.identifier-subst (list mixin-identifier)
				 (list destination-identifier)
				 mixin-clauses))
      (synner "undefined mixin identifier" mixin-identifier))))


(define (normalise-class-inheritance superclass-identifier parent-name parent-rtd parent-cd synner)
  ;;Normalise the  inheritance for this  class.  We must end  with sound
  ;;values  for SUPERCLASS-IDENTIFIER,  PARENT-RTD  and PARENT-CD.   The
  ;;parse procedure  before the call of  this function has  left us with
  ;;the following situation:
  ;;
  ;;* If the INHERIT clause  is present: SUPERCLASS-IDENTIFIER is set to
  ;;the identifier  of a superclass macro; PARENT-RTD  and PARENT-CD set
  ;;to false.
  ;;
  ;;*  If  the PARENT  clause  is present:  PARENT-NAME  is  set to  the
  ;;identifier of the  parent record type; SUPERCLASS-IDENTIFIER, PARENT
  ;;and PARENT-CD are set to false.
  ;;
  ;;* If the PARENT-RTD clause is present: PARENT-RTD is set to a syntax
  ;;object evaluating  to the parent RTD;  PARENT-CD is set  to a syntax
  ;;object   evaluating   to    the   parent   constructor   descriptor;
  ;;SUPERCLASS-IDENTIFIER is set to false.
  ;;
  ;;PARENT-NAME must be  false or the identifier of  the parent *record*
  ;;type (not class type).
  ;;
  ;;Return the  3 new  values for SUPERCLASS-IDENTIFIER,  PARENT-RTD and
  ;;PARENT-CD.
  ;;
  (cond

   ;;The INHERIT clause is present with "<top>" as superclass.
   ((and superclass-identifier
	 (free-identifier=? superclass-identifier #'<top>-superclass)
	 (not parent-name)
	 (not parent-rtd)
	 (not parent-cd))
    (values superclass-identifier
	    #'(record-type-descriptor <top>)	      ;parent-rtd
	    #'(record-constructor-descriptor <top>))) ;parent-cd

   ;;The  INHERIT clause  is present  with a  superclass different
   ;;from "<top>".
   ((and superclass-identifier
	 (identifier? superclass-identifier)
	 (not parent-name)
	 (not parent-rtd)
	 (not parent-cd))
    (values superclass-identifier
	    #`(#,superclass-identifier :class-record-type-descriptor) ;parent-rtd
	    #`(#,superclass-identifier :superclass-constructor-descriptor))) ;parent-cd

   ;;The PARENT clause is present.
   ((and parent-name
	 (not superclass-identifier)
	 (not parent-rtd)
	 (not parent-cd))
    (values #'<top>-superclass ;superclass-identifier
	    #`(record-type-descriptor        #,parent-name) ;parent-rtd
	    #`(record-constructor-descriptor #,parent-name))) ;parent-cd

   ;;The PARENT-RTD clause is present.
   ((and (not superclass-identifier)
	 (not parent-name)
	 parent-rtd
	 parent-cd)
    (values #'<top>-superclass parent-rtd parent-cd))

   ;;No inheritance clauses are present.
   ((and (not superclass-identifier)
	 (not parent-name)
	 (not parent-rtd)
	 (not parent-cd))
    (values #'<top>-superclass		     ;superclass-identifier
	    #'(record-type-descriptor <top>) ;parent-rtd
	    #'(record-constructor-descriptor <top>))) ;parent-cd

   (else
    (synner "invalid selection of superclass" #f))))


(define (extract-super-properties-if-any identifier)
  ;;If the parent of  a class or label is a record,  rather than a class
  ;;or  label,  there  are  no superclass/superlabel  properties  to  be
  ;;inspected.
  ;;
  ;;Return two values: a boolean, true  if the parent is a superclass or
  ;;superlabel;  false or a  record representing  the properties  of the
  ;;superclass or superlabel.  The return  values are both false or both
  ;;true.
  ;;
  (let ((id? (identifier? identifier)))
    (if id?
	(let ((props (and id? (prop.struct-properties-ref identifier))))
	  (if props
	      (values #t props)
	    (values #f #f)))
      (values #f #f))))


(define (variable-name->Setter-name variable-name-stx)
  (synux.identifier-suffix variable-name-stx ".__nausicaa_private_Setter_identifier_syntax"))

(define (variable-name->Getter-name variable-name-stx)
  (synux.identifier-suffix variable-name-stx ".__nausicaa_private_Getter_identifier_syntax"))


;;;; accessor-of and mutator-of macro transformers generation

(define (make-fields-accessor-of-transformer class-identifier fields virtual-fields synner)
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
       (make-field-accessor-or-mutator-transformer-function class-identifier case-branches))

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

(define (make-fields-mutator-of-transformer class-identifier fields virtual-fields synner)
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
       (make-field-accessor-or-mutator-transformer-function class-identifier case-branches))

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

(define (make-field-accessor-or-mutator-transformer-function class-identifier case-branches)
  ;;Auxiliary   function   for  MAKE-FIELDS-MUTATOR-OF-TRANSFORMER   and
  ;;MAKE-FIELDS-ACCESSOR-OF-TRANSFORMER.   Build and  return  the actual
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

(define (make-with-field-class-bindings fields virtual-fields synner)
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

(define (list-of-unique-field-types fields virtual-fields tail synner)
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
  ;;TAIL must be null, or a wrapped or unwrapped syntax object holding a
  ;;proper  list of  identifiers  representing the  field  types of  the
  ;;superclass or  superlabel; it will  become the tail of  the returned
  ;;list.
  ;;
  ;;SYNNER must  be the closure  used to raise  a syntax violation  if a
  ;;parse  error happens;  it  must accept  two  arguments: the  message
  ;;string, the subform.
  ;;
  (let loop ((fields	#`(#,@fields #,@virtual-fields))
	     (types	(synux.syntax->list tail)))
    (syntax-case fields (mutable immutable)
      (()
       (synux.delete-duplicate-identifiers types))

      (((mutable ?field ?accessor ?mutator . ?field-classes) . ?fields)
       (loop #'?fields (synux.syntax->list #'?field-classes types)))

      (((immutable ?field ?accessor . ?field-classes) . ?fields)
       (loop #'?fields (synux.syntax->list #'?field-classes types)))

      ((?spec . ?fields)
       (synner "invalid field specification while verifying recursive types" #'?spec))
      )))


;;;; done

)

;;; end of file
