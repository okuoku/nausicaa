;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: helper definitions for classes library
;;;Date: Tue Apr 27, 2010
;;;
;;;Abstract
;;;
;;;
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
(library (classes clause-parsers)
  (export
    ;; label-specific clause collectors
    %collect-clause/label/inherit
    %collect-clause/label/predicate

    ;; class-specific clause collectors and helper functions
    %collect-clause/class/inherit
    %collect-clause/parent
    %collect-clause/parent-rtd
    %collect-clause/nongenerative
    %collect-clause/opaque
    %collect-clause/sealed
    %collect-clause/protocol
    %collect-clause/public-protocol
    %collect-clause/maker-protocol
    %collect-clause/superclass-protocol
    %collect-clause/maker
    %collect-clause/predicate
    %output-forms/concrete-fields

    ;; clause collectors
    %collect-clause/fields
    %collect-clause/virtual-fields
    %collect-clause/methods
    %collect-clause/method
    %collect-clause/method-syntax
    %collect-clause/setter
    %collect-clause/getter
    %collect-clause/bindings
    )
  (import (rnrs)
    (for (only (rnrs base) define-syntax) (meta -1))
    (gensym)
    (syntax-utilities)
    (classes helpers)
    (for (classes top) (meta -1))
    (for (auxiliary-syntaxes) (meta -1)))


;;;; class-specific definition clauses collectors

(define (%collect-clause/class/inherit clauses synner)
  ;;Given a  list of  class definition clauses  in CLAUSES,  extract the
  ;;INHERIT clause and  parse it; there must be  only one INHERIT clause
  ;;in CLAUSES.
  ;;
  ;;Return  Five values:  an identifier  representing the  superclass, 4
  ;;booleans representing the inherit  options.  If no INHERIT clause is
  ;;found: return false as superclass and all true for the options.
  ;;
  ;;SYNNER must  be the closure  used to raise  a syntax violation  if a
  ;;parse  error  occurs; it  must  accept  two  arguments: the  message
  ;;string, the subform.
  ;;
  (define (select-superclass superclass-name)
    (if (free-identifier=? #'<top> superclass-name)
	#'<top>-superclass
      superclass-name))
  (let ((clauses (filter-clauses #'inherit clauses)))
    (if (null? clauses)
	(values #f #t #t #t #t)
      (syntax-case (car clauses) (inherit)

	((inherit ?superclass-name)
	 (identifier? #'?superclass-name)
	 (values (select-superclass #'?superclass-name) #t #t #t #t))

	((inherit ?superclass-name (?inherit-option ...))
	 (all-identifiers? #'(?superclass-name ?inherit-option ...))
	 (let-values (((inherit-concrete-fields? inherit-virtual-fields? inherit-methods?
						 inherit-setter-and-getter?)
		       (%parse-class-inherit-options #'(?inherit-option ...) synner)))
	   (values (select-superclass #'?superclass-name)
		   inherit-concrete-fields? inherit-virtual-fields? inherit-methods?
		   inherit-setter-and-getter?)))

	(_
	 (synner "invalid inherit clause" (car clauses)))
	))))

(define (%collect-clause/parent clauses synner)
  ;;Given a  list of definition  clauses in CLAUSES, extract  the PARENT
  ;;clause  and  parse it;  there  must be  only  one  PARENT clause  in
  ;;CLAUSES.
  ;;
  ;;Return the identifier of the  parent record type; return false if no
  ;;PARENT clause is present.
  ;;
  ;;SYNNER must  be the closure  used to raise  a syntax violation  if a
  ;;parse  error  occurs; it  must  accept  two  arguments: the  message
  ;;string, the subform.
  ;;
  (let ((clauses (filter-clauses #'parent clauses)))
    (if (null? clauses)
	#f
      (syntax-case (car clauses) (parent)
	((parent ?record-name)
	 (identifier? #'?record-name)
	 #'?record-name)

	(_
	 (synner "invalid parent clause" (car clauses)))))))

(define (%collect-clause/parent-rtd clauses synner)
  ;;Given  a  list  of   definition  clauses  in  CLAUSES,  extract  the
  ;;PARENT-RTD clause and parse it; there must be only one PARENT-RTD in
  ;;CLAUSES.
  ;;
  ;;Return two values:  the expression evaluating to the  parent RTD and
  ;;the  expression  evaluating to  the  parent constructor  descriptor;
  ;;return two false if PARENT-RTD is not present.
  ;;
  ;;SYNNER must  be the closure  used to raise  a syntax violation  if a
  ;;parse  error  occurs; it  must  accept  two  arguments: the  message
  ;;string, the subform.
  ;;
  (let ((clauses (filter-clauses #'parent-rtd clauses)))
    (if (null? clauses)
	(values #f #f)
      (syntax-case (car clauses) (parent-rtd)
	((parent-rtd ?rtd ?cd)
	 (values #'?rtd #'?cd))
	(_
	 (synner "invalid parent-rtd clause" (car clauses)))))))

(define (%collect-clause/nongenerative thing-identifier clauses synner)
  ;;Given  a  list  of   definition  clauses  in  CLAUSES,  extract  the
  ;;NONGENERATIVE  clause   and  parse  it;  there  must   be  only  one
  ;;NONGENERATIVE clause in CLAUSES.
  ;;
  ;;Return  an identifier  representing the  UID of  the  defined thing.
  ;;THING-IDENTIFIER must be the identifier the UID belongs to.
  ;;
  ;;SYNNER must  be the closure  used to raise  a syntax violation  if a
  ;;parse  error  occurs; it  must  accept  two  arguments: the  message
  ;;string, the subform.
  ;;
  (let ((clauses (filter-clauses #'nongenerative clauses)))
    (if (null? clauses)
	(datum->syntax thing-identifier (gensym))
      (syntax-case (car clauses) (nongenerative)

	((nongenerative)
	 (datum->syntax thing-identifier (gensym)))

	((nongenerative ?uid)
	 (identifier? #'?uid)
	 #'?uid)

	(_
	 (synner "invalid nongenerative clause" (car clauses)))
	))))

(define (%collect-clause/sealed clauses synner)
  ;;Given a  list of definition  clauses in CLAUSES, extract  the SEALED
  ;;clause  and  parse it;  there  must be  only  one  SEALED clause  in
  ;;CLAUSES.
  ;;
  ;;Return a boolean  establishing if the defined thing  type is sealed;
  ;;return false if no SEALED clause was found.
  ;;
  ;;SYNNER must  be the closure  used to raise  a syntax violation  if a
  ;;parse  error  occurs; it  must  accept  two  arguments: the  message
  ;;string, the subform.
  ;;
  (let ((clauses (filter-clauses #'sealed clauses)))
    (if (null? clauses)
	#f
      (syntax-case (car clauses) (sealed)

	((sealed ?bool)
	 (boolean? (syntax->datum #'?bool))
	 (syntax->datum #'?bool))

	(_
	 (synner "invalid sealed clause" (car clauses)))
	))))

(define (%collect-clause/opaque clauses synner)
  ;;Given a  list of definition  clauses in CLAUSES, extract  the OPAQUE
  ;;clause  and  parse it;  there  must be  only  one  OPAQUE clause  in
  ;;CLAUSES.
  ;;
  ;;Return a boolean  establishing if the defined thing  type is opaque;
  ;;return false if no OPAQUE clause was found.
  ;;
  ;;SYNNER must  be the closure  used to raise  a syntax violation  if a
  ;;parse  error  occurs; it  must  accept  two  arguments: the  message
  ;;string, the subform.
  ;;
  (let ((clauses (filter-clauses #'opaque clauses)))
    (if (null? clauses)
	#f
      (syntax-case (car clauses) (opaque)

	((opaque ?bool)
	 (boolean? (syntax->datum #'?bool))
	 (syntax->datum #'?bool))

	(_
	 (synner "invalid opaque clause" (car clauses)))
	))))

(define (%collect-clause/protocol clauses synner)
  ;;Given a list of definition  clauses in CLAUSES, extract the PROTOCOL
  ;;clause  and parse  it; there  must be  only one  PROTOCOL  clause in
  ;;CLAUSES.
  ;;
  ;;Return  the clause  expression or  false if  no PROTOCOL  clause was
  ;;found; return false if no PROTOCOL clause is present.
  ;;
  ;;SYNNER must  be the closure  used to raise  a syntax violation  if a
  ;;parse  error  occurs; it  must  accept  two  arguments: the  message
  ;;string, the subform.
  ;;
  (let ((clauses (filter-clauses #'protocol clauses)))
    (if (null? clauses)
	#f
      (syntax-case (car clauses) (protocol)

	((protocol ?expression)
	 #'?expression)

	(_
	 (synner "invalid protocol clause" (car clauses)))
	))))

(define (%collect-clause/public-protocol clauses synner)
  ;;Given  a  list  of   definition  clauses  in  CLAUSES,  extract  the
  ;;PUBLIC-PROTOCOL  clause  and  parse  it;  there  must  be  only  one
  ;;PUBLIC-PROTOCOL clause in CLAUSES.
  ;;
  ;;Return the  clause expression or false if  no PUBLIC-PROTOCOL clause
  ;;was found.
  ;;
  ;;SYNNER must  be the closure  used to raise  a syntax violation  if a
  ;;parse  error  occurs; it  must  accept  two  arguments: the  message
  ;;string, the subform.
  ;;
  (let ((clauses (filter-clauses #'public-protocol clauses)))
    (if (null? clauses)
	#f
      (syntax-case (car clauses) (public-protocol)

	((public-protocol ?expression)
	 #'?expression)

	(_
	 (synner "invalid public-protocol clause" (car clauses)))
	))))

(define (%collect-clause/maker-protocol clauses synner)
  ;;Given  a  list  of   definition  clauses  in  CLAUSES,  extract  the
  ;;MAKER-PROTOCOL  clause  and  parse   it;  there  must  be  only  one
  ;;MAKER-PROTOCOL clause in CLAUSES.
  ;;
  ;;Return the  clause expression or  false if no  MAKER-PROTOCOL clause
  ;;was found.
  ;;
  ;;SYNNER must  be the closure  used to raise  a syntax violation  if a
  ;;parse  error  occurs; it  must  accept  two  arguments: the  message
  ;;string, the subform.
  ;;
  (let ((clauses (filter-clauses #'maker-protocol clauses)))
    (if (null? clauses)
	#f
      (syntax-case (car clauses) (maker-protocol)

	((maker-protocol ?expression)
	 #'?expression)

	(_
	 (synner "invalid maker-protocol clause" (car clauses)))
	))))

(define (%collect-clause/superclass-protocol clauses synner)
  ;;Given  a  list  of   definition  clauses  in  CLAUSES,  extract  the
  ;;SUPERCLASS-PROTOCOL  clause and  parse it;  there must  be  only one
  ;;SUPERCLASS-PROTOCOL clause in CLAUSES.
  ;;
  ;;Return  the clause  expression  or false  if no  SUPERCLASS-PROTOCOL
  ;;clause was found.
  ;;
  ;;SYNNER must  be the closure  used to raise  a syntax violation  if a
  ;;parse  error  occurs; it  must  accept  two  arguments: the  message
  ;;string, the subform.
  ;;
  (let ((clauses (filter-clauses #'superclass-protocol clauses)))
    (if (null? clauses)
	#f
      (syntax-case (car clauses) (superclass-protocol)

	((superclass-protocol ?expression)
	 #'?expression)

	(_
	 (synner "invalid superclass-protocol clause" (car clauses)))
	))))

(define (%collect-clause/predicate predicate-identifier clauses synner)
  ;;Given a  list of  class definition clauses  in CLAUSES,  extract the
  ;;PREDICATE  clause and  parse it;  there must  be only  one PREDICATE
  ;;clause in CLAUSES.
  ;;
  ;;Return  an  identifier representing  the  custom  predicate for  the
  ;;class; return PREDICATE-IDENTIFIER if no PREDICATE clause is found.
  ;;
  ;;SYNNER must  be the closure  used to raise  a syntax violation  if a
  ;;parse  error  occurs; it  must  accept  two  arguments: the  message
  ;;string, the subform.
  ;;
  (let ((clauses (filter-clauses #'predicate clauses)))
    (if (null? clauses)
	predicate-identifier
      (syntax-case (car clauses) (predicate)
	((predicate ?predicate)
	 (identifier? #'?predicate)
	 #'?predicate)
	(_
	 (synner "invalid predicate clause" (car clauses)))
	))))

(define (%collect-clause/maker clauses synner)
  ;;Given a  list of  class definition clauses  in CLAUSES,  extract the
  ;;MAKER clause  and parse it; there  must be only one  MAKER clause in
  ;;CLAUSES.
  ;;
  ;;Return two  values: a  syntax object holding  a list  of identifiers
  ;;representing the  mandatory arguments to the maker,  a syntax object
  ;;holding a list of maker clauses representing optional arguments.  If
  ;;no MAKER clause is present: return false and false.
  ;;
  ;;SYNNER must  be the closure  used to raise  a syntax violation  if a
  ;;parse  error  occurs; it  must  accept  two  arguments: the  message
  ;;string, the subform.
  ;;
  (let ((clauses (filter-clauses #'maker clauses)))
    (if (null? clauses)
	(values #f #f)
      (syntax-case (car clauses) (maker)

	((maker (?positional-arg ...) (?optional-keyword ?optional-default ?option ...) ...)
	 (all-identifiers? #'(?optional-keyword ...))
	 (values #'(?positional-arg ...) #'((?optional-keyword ?optional-default ?option ...) ...)))

	(_
	 (synner "invalid maker clause" (car clauses)))
	))))

(define-syntax %output-forms/concrete-fields
  (syntax-rules ()
    ((_ ?rtd ?specs ?synner)
     (output-forms/class/concrete-fields ?rtd ?specs ?synner 0 '()))))

(define (output-forms/class/concrete-fields rtd-identifier field-specs synner index field-definitions)
  ;;This is  a recursive funciton accumulating  in FIELD-DEFINITIONS the
  ;;definitions of  the concrete  fields accessor and  mutator bindings.
  ;;Given a  list of concrete  field specifications in  FIELD-SPECS with
  ;;the format:
  ;;
  ;;	(mutable <field name> <accessor identifier> <mutator identifier>)
  ;;	(immutable <field name> <accessor identifier>)
  ;;
  ;;extract the  accessor and  mutator identifiers and  build a  list of
  ;;syntax objects with the format:
  ;;
  ;;	(begin
  ;;	  (define <accessor identifier>
  ;;	    (record-accessor RTD-IDENTIFIER INDEX))
  ;;	  (define <mutator identifier>
  ;;	    (record-mutator RTD-IDENTIFIER INDEX)))
  ;;
  ;;    (define <accessor identifier>
  ;;	   (record-accessor RTD-IDENTIFIER INDEX))
  ;;
  ;;SYNNER must be  the closure used to raise a  syntax violation if a
  ;;parse  error occurs;  it must  accept two  arguments:  the message
  ;;string, the subform.
  ;;
  (define (recurse field-defs)
    (output-forms/class/concrete-fields rtd-identifier (cdr field-specs) synner (+ 1 index) field-defs))
  (if (null? field-specs)
      (reverse field-definitions)
    (syntax-case (car field-specs) (mutable immutable)

      ((mutable ?field ?accessor ?mutator)
       (all-identifiers? #'(?field ?accessor ?mutator))
       (recurse (cons #`(begin
			  (define ?accessor  (record-accessor #,rtd-identifier #,index))
			  (define ?mutator   (record-mutator  #,rtd-identifier #,index)))
		      field-definitions)))

      ((immutable ?field ?accessor)
       (all-identifiers? #'(?field ?accessor))
       (recurse (cons #`(define ?accessor  (record-accessor #,rtd-identifier #,index))
		      field-definitions)))

      (_
       (synner "invalid concrete field specification" (car field-specs)))

      )))

(define (%parse-class-inherit-options inherit-options/stx synner)
  ;;Here we already know that INHERIT-OPTIONS is a list of identifiers.
  ;;
  (let loop ((concrete-fields	#t)
	     (virtual-fields	#t)
	     (methods		#t)
	     (setter-and-getter	#t)
	     (options		(syntax->datum inherit-options/stx)))
    (if (null? options)
	(values concrete-fields virtual-fields methods setter-and-getter)
      (case (car options)

	((all everything)
	 (loop #t #t #t #t (cdr options)))

	((dry nothing)
	 (loop #f #f #f #f (cdr options)))

	((concrete-fields)
	 (loop #t virtual-fields methods setter-and-getter (cdr options)))
	((no-concrete-fields)
	 (loop #f virtual-fields methods setter-and-getter (cdr options)))

	((virtual-fields)
	 (loop concrete-fields #t methods setter-and-getter (cdr options)))
	((no-virtual-fields)
	 (loop concrete-fields #f methods setter-and-getter (cdr options)))

	((fields)
	 (loop #t #t methods setter-and-getter (cdr options)))
	((no-fields)
	 (loop #f #f methods setter-and-getter (cdr options)))

	((methods)
	 (loop concrete-fields virtual-fields #t setter-and-getter (cdr options)))
	((no-methods)
	 (loop concrete-fields virtual-fields #f setter-and-getter (cdr options)))

	((setter-and-getter)
	 (loop concrete-fields virtual-fields methods #t (cdr options)))
	((no-setter-and-getter)
	 (loop concrete-fields virtual-fields methods #f (cdr options)))

	(else
	 (synner "invalid inheritance option" (car options)))))))


;;;; label-specific definition clauses collectors

(define (%collect-clause/label/inherit clauses synner)
  ;;Given a  list of  label definition clauses  in CLAUSES,  extract the
  ;;INHERIT clause and  parse it; there must be  only one INHERIT clause
  ;;in CLAUSES.
  ;;
  ;;Return  four values:  an identifier  representing the  superlabel, 3
  ;;booleans representing the inherit  options.  If no INHERIT clause is
  ;;found: return "<top>-superlabel" and all true.
  ;;
  ;;SYNNER must  be the closure  used to raise  a syntax violation  if a
  ;;parse  error  occurs; it  must  accept  two  arguments: the  message
  ;;string, the subform.
  ;;
  (define (select-superlabel superlabel-name)
    (if (free-identifier=? #'<top> superlabel-name)
	#'<top>-superlabel
      superlabel-name))
  (let ((clauses (filter-clauses #'inherit clauses)))
    (if (null? clauses)
	(values #'<top>-superlabel #t #t #t)
      (syntax-case (car clauses) (inherit)

	((inherit ?superlabel-name)
	 (identifier? #'?superlabel-name)
	 (values (select-superlabel #'?superlabel-name) #t #t #t))

	((inherit ?superlabel-name (?inherit-option ...))
	 (all-identifiers? #'(?superlabel-name ?inherit-option ...))
	 (let-values (((inherit-virtual-fields? inherit-methods? inherit-setter-and-getter?)
		       (%parse-label-inherit-options #'(?inherit-option ...) synner)))
	   (values (select-superlabel #'?superlabel-name)
		   inherit-virtual-fields? inherit-methods? inherit-setter-and-getter?)))

	(_
	 (synner "invalid inherit clause" (car clauses)))
	))))

(define (%collect-clause/label/predicate clauses synner)
  ;;Given a  list of  label definition clauses  in CLAUSES,  extract the
  ;;PREDICATE  clause and  parse it;  there must  be only  one PREDICATE
  ;;clause in CLAUSES.
  ;;
  ;;Return  an  identifier representing  the  custom  predicate for  the
  ;;label; return false if no PREDICATE clause is found.
  ;;
  ;;SYNNER must  be the closure  used to raise  a syntax violation  if a
  ;;parse  error  occurs; it  must  accept  two  arguments: the  message
  ;;string, the subform.
  ;;
  (let ((clauses (filter-clauses #'predicate clauses)))
    (if (null? clauses)
	#f
      (syntax-case (car clauses) (predicate)
	((predicate ?predicate)
	 (identifier? #'?predicate)
	 #'?predicate)
	(_
	 (synner "invalid predicate clause" (car clauses)))
	))))

(define (%parse-label-inherit-options inherit-options/stx synner)
  ;;Here we already know that INHERIT-OPTIONS is a list of identifiers.
  ;;
  (let next-option ((virtual-fields	#t)
		    (methods		#t)
		    (setter-and-getter	#t)
		    (options		(syntax->datum inherit-options/stx)))
    (if (null? options)
	(values virtual-fields methods setter-and-getter)
      (case (car options)

	((all everything)
	 (next-option #t #t #t (cdr options)))

	((dry nothing)
	 (next-option #f #f #f (cdr options)))

	((virtual-fields)
	 (next-option #t methods setter-and-getter (cdr options)))
	((no-virtual-fields)
	 (next-option #f methods setter-and-getter (cdr options)))

	((fields)
	 (next-option #t methods setter-and-getter (cdr options)))
	((no-fields)
	 (next-option #f methods setter-and-getter (cdr options)))

	((methods)
	 (next-option virtual-fields #t setter-and-getter (cdr options)))
	((no-methods)
	 (next-option virtual-fields #f setter-and-getter (cdr options)))

	((setter-and-getter)
	 (next-option virtual-fields methods #t (cdr options)))
	((no-setter-and-getter)
	 (next-option virtual-fields methods #f (cdr options)))

	(else
	 (synner "invalid inheritance option" (car options)))))))


;;;; definition clauses collectors

(define (%collect-clause/fields thing-identifier clauses synner)
  ;;Given  a list  of definition  clauses  in CLAUSES,  extract all  the
  ;;FIELDS clauses and parse them;  there can be multiple FIELDS clauses
  ;;in CLAUSES.
  ;;
  ;;THING-IDENTIFIER must be an identifier representing the thing (class
  ;;or label) name  to which the fields belong: it is  used to build the
  ;;accessor and mutator names when not given in the input specs.
  ;;
  ;;Return null  or a validated  list of virtual fields  having elements
  ;;with format:
  ;;
  ;;    (immutable <field name> <field accessor>)
  ;;    (mutable   <field name> <field accessor> <field mutator>)
  ;;
  ;;where IMMUTABLE and  MUTABLE are symbols and the  other elements are
  ;;identifiers.
  ;;
  ;;SYNNER must  be the closure  used to raise  a syntax violation  if a
  ;;parse  error  occurs; it  must  accept  two  arguments: the  message
  ;;string, the subform.
  ;;
  (let next-clause ((clauses   (filter-clauses #'fields clauses))
		    (collected '()))
    (if (null? clauses)
	(reverse collected)
      (syntax-case (car clauses) (fields)
	((fields ?field-clause ...)
	 (next-clause (cdr clauses)
		      (%parse-clause/fields thing-identifier (cdar clauses) synner collected)))
	(_
	 (synner "invalid fields clause" (car clauses)))
	))))

(define (%collect-clause/virtual-fields thing-identifier clauses synner)
  ;;Given  a list  of definition  clauses  in CLAUSES,  extract all  the
  ;;VIRTUAL-FIELDS  clauses  and  parse  them;  there  can  be  multiple
  ;;VIRTUAL-FIELDS clauses in CLAUSES.
  ;;
  ;;THING-IDENTIFIER must be an identifier representing the thing (class
  ;;or label) name  to which the fields belong: it is  used to build the
  ;;accessor and mutator names when not given in the input specs.
  ;;
  ;;Return null  or a validated  list of virtual fields  having elements
  ;;with format:
  ;;
  ;;    (immutable <field name> <field accessor>)
  ;;    (mutable   <field name> <field accessor> <field mutator>)
  ;;
  ;;where  IMMUTABLE and  MUTABLE are  symbols and  the other
  ;;elements are identifiers.
  ;;
  ;;SYNNER must  be the closure  used to raise  a syntax violation  if a
  ;;parse  error  occurs; it  must  accept  two  arguments: the  message
  ;;string, the subform.
  ;;
  (let next-clause ((clauses   (filter-clauses #'virtual-fields clauses))
		    (collected '()))
    (if (null? clauses)
	(reverse collected)
      (syntax-case (car clauses) (virtual-fields)
	((virtual-fields ?field-clause ...)
	 (next-clause (cdr clauses)
		      (%parse-clause/virtual-fields thing-identifier (cdar clauses) synner collected)))
	(_
	 (synner "invalid virtual-fields clause" (car clauses)))
	))))

(define (%collect-clause/methods thing-identifier clauses synner)
  ;;Given  a list  of definition  clauses  in CLAUSES,  extract all  the
  ;;METHODS  clauses  and parse  them;  there  can  be multiple  METHODS
  ;;clauses in CLAUSES.
  ;;
  ;;THING-IDENTIFIER must be an identifier representing the thing (class
  ;;or label) name to which the  methods belong: it is used to build the
  ;;method function or macro name when not given in the input specs.
  ;;
  ;;Return  null or  a validated  list of  method  specifications having
  ;;elements with format:
  ;;
  ;;	(<method identifier> <function or macro identifier>)
  ;;
  ;;SYNNER must  be the closure  used to raise  a syntax violation  if a
  ;;parse  error  occurs; it  must  accept  two  arguments: the  message
  ;;string, the subform.
  ;;
  (let next-clause ((clauses   (filter-clauses #'methods clauses))
		    (collected '()))
    (if (null? clauses)
	(reverse collected)
      (syntax-case (car clauses) (methods)
	((methods ?method-clause ...)
	 (next-clause (cdr clauses)
		      (%parse-clause/methods thing-identifier
					     (unwrap-syntax-object #'(?method-clause ...))
					     synner collected)))
	(_
	 (synner "invalid methods clause" (car clauses)))
	))))

(define (%collect-clause/method thing-name clauses synner define/with-class)
  ;;Given  a list  of definition  clauses  in CLAUSES,  extract all  the
  ;;METHOD clauses and parse them;  there can be multiple METHOD clauses
  ;;in CLAUSES.
  ;;
  ;;Return  two  values:  null/null   or  a  validated  list  of  method
  ;;specifications having elements with format:
  ;;
  ;;    (<method name> <function name>)
  ;;
  ;;and a list of definitions with the format:
  ;;
  ;;    (<definition> ...)
  ;;
  ;;in which each definition has one of the formats:
  ;;
  ;;    (define (<function name> . <args>) . <body>)
  ;;    (define <function name> <expression>)
  ;;
  ;;SYNNER must  be the closure  used to raise  a syntax violation  if a
  ;;parse  error  occurs; it  must  accept  two  arguments: the  message
  ;;string, the subform.
  ;;
  (let next-clause ((clauses		(filter-clauses #'method clauses))
		    (methods		'())
		    (definitions	'()))
    (if (null? clauses)
	(values (reverse methods) (reverse definitions))
      (syntax-case (car clauses) (method)
	((method . ?args)
	 (let-values (((m d) (%parse-clause/method thing-name (car clauses) synner define/with-class)))
	   (next-clause (cdr clauses) (cons m methods) (cons d definitions))))
	(_
	 (synner "invalid method clause" (car clauses)))
	))))

(define (%collect-clause/method-syntax thing-name clauses synner)
  ;;Given  a list  of definition  clauses  in CLAUSES,  extract all  the
  ;;METHOD-SYNTAX  clauses  and  parse   them;  there  can  be  multiple
  ;;METHOD-SYNTAX clauses in CLAUSES.
  ;;
  ;;Return two  values: null/null or  a validated list of  method syntax
  ;;specifications having elements with format:
  ;;
  ;;    (<method name> <macro name>)
  ;;
  ;;and a list of definitions with the format:
  ;;
  ;;    (<definition> ...)
  ;;
  ;;in which each definition has the format:
  ;;
  ;;    (define-syntax <macro name> <expression>)
  ;;
  ;;SYNNER must  be the closure  used to raise  a syntax violation  if a
  ;;parse  error  occurs; it  must  accept  two  arguments: the  message
  ;;string, the subform.
  ;;
  (let next-clause ((clauses		(filter-clauses #'method-syntax clauses))
		    (methods		'())
		    (definitions	'()))
    (if (null? clauses)
	(values (reverse methods) (reverse definitions))
      (syntax-case (car clauses) (method-syntax)
	((method-syntax . ?args)
	 (let-values (((m d) (%parse-clause/method-syntax thing-name (car clauses) synner)))
	   (next-clause (cdr clauses) (cons m methods) (cons d definitions))))
	(_
	 (synner "invalid method-syntax clause" (car clauses)))
	))))

(define (%collect-clause/setter clauses synner)
  ;;Given a  list of definition  clauses in CLAUSES, extract  the SETTER
  ;;clause  and  parse it;  there  must be  only  one  SETTER clause  in
  ;;CLAUSES.
  ;;
  ;;Return an identifier representing the name of the setter function or
  ;;macro; return false if no SETTER clause is found.
  ;;
  ;;SYNNER must  be the closure  used to raise  a syntax violation  if a
  ;;parse  error  occurs; it  must  accept  two  arguments: the  message
  ;;string, the subform.
  ;;
  (let ((clauses (filter-clauses #'setter clauses)))
    (if (null? clauses)
	#f
      (syntax-case (car clauses) (setter)
	((setter ?setter)
	 #'?setter)
	(_
	 (synner "invalid setter clause" (car clauses)))
	))))

(define (%collect-clause/getter clauses synner)
  ;;Given a  list of definition  clauses in CLAUSES, extract  the GETTER
  ;;clause  and  parse it;  there  must be  only  one  GETTER clause  in
  ;;CLAUSES.
  ;;
  ;;Return an identifier representing the name of the getter function or
  ;;macro; return false if no GETTER clause is found.
  ;;
  ;;SYNNER must  be the closure  used to raise  a syntax violation  if a
  ;;parse  error  occurs; it  must  accept  two  arguments: the  message
  ;;string, the subform.
  ;;
  (let ((clauses (filter-clauses #'getter clauses)))
    (if (null? clauses)
	#f
      (syntax-case (car clauses) (getter)
	((getter ?getter)
	 #'?getter)
	(_
	 (synner "invalid getter clause" (car clauses)))
	))))

(define (%collect-clause/bindings clauses synner)
  ;;Given a list of definition  clauses in CLAUSES, extract the BINDINGS
  ;;clause  and parse  it; there  must be  only one  BINDINGS  clause in
  ;;CLAUSES.
  ;;
  ;;Return an  identifier representing the  name of the  bindings macro;
  ;;return "<top>-bindings" if no BINDINGS clause is found.
  ;;
  ;;SYNNER must  be the closure  used to raise  a syntax violation  if a
  ;;parse  error  occurs; it  must  accept  two  arguments: the  message
  ;;string, the subform.
  ;;
  (let ((clauses (filter-clauses #'bindings clauses)))
    (if (null? clauses)
	#'<top>-bindings
      (syntax-case (car clauses) (bindings)
	((bindings ?macro-name)
	 (identifier? #'?macro-name)
	 #'?macro-name)
	(_
	 (synner "invalid bindings clause" (car clauses)))
	))))


;;;; definition claues parsers

(define (%parse-clause/fields thing-name field-clauses synner collected-fields)
  ;;This   is   a    recursive   function   accumulating   elements   in
  ;;COLLECTED-FIELDS, which  must be  null at the  entry call.   Given a
  ;;list  of  concrete   fields  specifications  in  FIELD-CLAUSES,  for
  ;;example:
  ;;
  ;;    ((mutable a) (immutable b) (mutable c cacc cmut))
  ;;
  ;;parse  it and accumulate  a list  of normalised  specifications each
  ;;with one of the formats:
  ;;
  ;;    (immutable <field name> <accessor name>)
  ;;    (mutable   <field name> <accessor name> <mutator name>)
  ;;
  ;;THING-NAME must  be an identifier  representing the thing  (class or
  ;;label) name  to which the  fields belong: it  is used to  build the
  ;;accessor and mutator names when not given in the input specs.
  ;;
  ;;SYNNER must  be the closure  used to raise  a syntax violation  if a
  ;;parse  error happens;  it  must accept  two  arguments: the  message
  ;;string, the subform.
  ;;
  (define (recurse field-spec)
    (%parse-clause/fields thing-name (cdr field-clauses) synner
			  (cons field-spec collected-fields)))
  (if (null? field-clauses)
      collected-fields ;it  is important to  keep the order,  we reverse
		       ;the list in the calling function
    (syntax-case (car field-clauses) (mutable immutable)

      ((mutable ?field ?accessor ?mutator)
       (all-identifiers? #'(?field ?accessor ?mutator))
       (recurse #'(mutable ?field ?accessor ?mutator)))

      ((mutable ?field)
       (identifier? #'?field)
       (recurse #`(mutable ?field
			   #,(syntax-accessor-identifier thing-name #'?field)
			   #,(syntax-mutator-identifier  thing-name #'?field))))

      ((immutable ?field ?accessor)
       (all-identifiers? #'(?field ?accessor ?mutator))
       (recurse #'(immutable ?field ?accessor)))

      ((immutable ?field)
       (identifier? #'?field)
       (recurse #`(immutable ?field #,(syntax-accessor-identifier thing-name #'?field))))

      (?field
       (identifier? #'?field)
       (recurse #`(#,(syntax immutable) ?field #,(syntax-accessor-identifier thing-name #'?field))))

      (_
       (synner "invalid fields clause" (car field-clauses)))
      )))

(define (%parse-clause/virtual-fields thing-name field-clauses synner collected-fields)
  ;;This   is   a    recursive   function   accumulating   elements   in
  ;;COLLECTED-FIELDS, which  must be  null at the  entry call.   Given a
  ;;list of virtual fields specifications in FIELD-CLAUSES, for example:
  ;;
  ;;    ((mutable a) (immutable b) (mutable c cacc cmut))
  ;;
  ;;parse  it and accumulate  a list  of normalised  specifications each
  ;;with one of the formats:
  ;;
  ;;    (immutable <field name> <accessor name>)
  ;;    (mutable   <field name> <accessor name> <mutator name>)
  ;;
  ;;THING-NAME must  be an identifier  representing the thing  (class or
  ;;label) name  to which  the fields  belong: it is  used to  build the
  ;;accessor and mutator names when not given in the input specs.
  ;;
  ;;SYNNER must  be the closure  used to raise  a syntax violation  if a
  ;;parse  error happens;  it  must accept  two  arguments: the  message
  ;;string, the subform.
  ;;
  (define (recurse field-spec)
    (%parse-clause/virtual-fields thing-name (cdr field-clauses) synner
				  (cons field-spec collected-fields)))
  (if (null? field-clauses)
      (reverse collected-fields) ;it is important to keep the order
    (syntax-case (car field-clauses) (mutable immutable)

      ((mutable ?field ?accessor ?mutator)
       (all-identifiers? #'(?field ?accessor ?mutator))
       (recurse #'(mutable ?field ?accessor ?mutator)))

      ((mutable ?field)
       (identifier? #'?field)
       (recurse #`(mutable ?field
			   #,(syntax-accessor-identifier thing-name #'?field)
			   #,(syntax-mutator-identifier  thing-name #'?field))))

      ((immutable ?field ?accessor)
       (all-identifiers? #'(?field ?accessor ?mutator))
       (recurse #'(immutable ?field ?accessor)))

      ((immutable ?field)
       (identifier? #'?field)
       (recurse #`(immutable ?field #,(syntax-accessor-identifier thing-name #'?field))))

      (?field
       (identifier? #'?field)
       (recurse #`(immutable ?field #,(syntax-accessor-identifier thing-name #'?field))))

      (_
       (synner "invalid virtual-fields clause" (car field-clauses)))
      )))

(define (%parse-clause/methods thing-name methods-clauses synner collected-methods)
  ;;This   is   a    recursive   function   accumulating   elements   in
  ;;COLLECTED-METHODS, which  must be null  at the entry call.   Given a
  ;;list of methods specifications in METHODS-CLAUSES, for example:
  ;;
  ;;    ((a alpha) (b) (gamma))
  ;;
  ;;parse  it and accumulate  a list  of normalised  specifications each
  ;;with one of the formats:
  ;;
  ;;    (<method name> <function/macro name>)
  ;;
  ;;THING-NAME must  be an identifier  representing the thing  (class or
  ;;label) name  to which the  methods belong: it  is used to  build the
  ;;accessor and mutator names when not given in the input specs.
  ;;
  ;;SYNNER must  be the closure  used to raise  a syntax violation  if a
  ;;parse  error happens;  it  must accept  two  arguments: the  message
  ;;string, the subform.
  ;;
  (define (recurse method-spec)
    (%parse-clause/methods thing-name (cdr methods-clauses) synner
			   (cons method-spec collected-methods)))
  (if (null? methods-clauses)
      (reverse collected-methods)
    (syntax-case (car methods-clauses) (method)

      ((?field-name ?method-function/macro)
       (and (identifier? #'?field-name) (identifier? #'?method-function/macro))
       (recurse (list #'?field-name #'?method-function/macro)))

      ((?field-name)
       (identifier? #'?field-name)
       (recurse (list #'?field-name (syntax-method-identifier thing-name #'?field-name))))

      (?field-name
       (identifier? #'?field-name)
       (recurse (list #'?field-name (syntax-method-identifier thing-name #'?field-name))))

      (_
       (synner "invalid methods clause" (car methods-clauses)))
      )))

(define (%parse-clause/method thing-name clause synner define/with-class)
  ;;Given a METHOD clause in CLAUSE, parse it and return two values: the
  ;;method specification as a list with format:
  ;;
  ;;    (<field name> <function name>)
  ;;
  ;;and the associated definition as a list with format:
  ;;
  ;;    (define/with-class (<function name> . <args>) . <body>)
  ;;    (define <function name> <expression>)
  ;;
  ;;SYNNER must  be the closure  used to raise  a syntax violation  if a
  ;;parse  error happens;  it  must accept  two  arguments: the  message
  ;;string, the subform.
  ;;
  (syntax-case clause (method)

    ((method (?method . ?args) . ?body)
     (identifier? #'?method)
     (with-syntax ((FUNCTION-NAME (syntax-method-identifier thing-name #'?method)))
       (values #'(?method FUNCTION-NAME)
	       #`(#,define/with-class (FUNCTION-NAME . ?args) . ?body))))

    ((method ?method ?expression)
     (identifier? #'?method)
     (with-syntax ((FUNCTION-NAME (syntax-method-identifier thing-name #'?method)))
       (values #'(?method FUNCTION-NAME)
	       #'(define FUNCTION-NAME ?expression))))

    (_
     (synner "invalid method clause" clause))
    ))

(define (%parse-clause/method-syntax thing-name clause synner)
  ;;Given  a METHOD-SYNTAX  clause in  CLAUSE, parse  it and  return two
  ;;values: the method specification as a list with format:
  ;;
  ;;    (<field name> <syntax name>)
  ;;
  ;;and the associated definition as a list with format:
  ;;
  ;;    (define-syntax <syntax name> <transformer>)
  ;;
  ;;SYNNER must  be the closure  used to raise  a syntax violation  if a
  ;;parse  error happens;  it  must accept  two  arguments: the  message
  ;;string, the subform.
  ;;
  (syntax-case clause ()

    ((method-syntax ?method ?transformer)
     (identifier? #'?method)
     (with-syntax ((MACRO-NAME (syntax-method-identifier thing-name #'?method)))
       (values #'(?method MACRO-NAME)
	       #'(define-syntax MACRO-NAME ?transformer))))

    (_
     (synner "invalid method-syntax clause" clause))
    ))


;;;; done

)

;;; end of file
