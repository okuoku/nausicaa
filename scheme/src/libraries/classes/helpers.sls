;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: helper definitions for classes library
;;;Date: Tue Apr 27, 2010
;;;
;;;Abstract
;;;
;;;	Aaron Hsu  contributed the SYNTAX->LIST function  through a post
;;;	on comp.lang.scheme.
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


(library (classes helpers)
  (export
    %variable-name->Setter-name		%variable-name->Getter-name
    syntax->list
    syntax-prefix			syntax-suffix
    syntax-accessor-name		syntax-mutator-name
    syntax-method-name
    syntax-dot-notation-name
    duplicated-identifiers?		all-identifiers?
    %parse-inherit-options

    %check-clauses			%clause-ref

    ;; label-specific clause collectors
    %collect-clause/label/inherit
    %collect-clause/label/predicate

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
    (gensym)
    (classes top))


(define (%variable-name->Setter-name variable-name/stx)
  (datum->syntax variable-name/stx
		 (string->symbol
		  (string-append (symbol->string (syntax->datum variable-name/stx))
				 ".__nausicaa_private_Setter_identifier_syntax"))))

(define (%variable-name->Getter-name variable-name/stx)
  (datum->syntax variable-name/stx
		 (string->symbol
		  (string-append (symbol->string (syntax->datum variable-name/stx))
				 ".__nausicaa_private_Getter_identifier_syntax"))))

(define (syntax-accessor-name class-name/stx field-name/stx)
  (datum->syntax class-name/stx
		 (string->symbol
		  (string-append (symbol->string (syntax->datum class-name/stx))
				 "-"
				 (symbol->string (syntax->datum field-name/stx))))))

(define syntax-method-name syntax-accessor-name)

(define (syntax-mutator-name class-name/stx field-name/stx)
  (datum->syntax class-name/stx
		 (string->symbol
		  (string-append (symbol->string (syntax->datum class-name/stx))
				 "-"
				 (symbol->string (syntax->datum field-name/stx))
				 "-set!"))))

(define (syntax-dot-notation-name variable-name/stx field-name/stx)
  (datum->syntax variable-name/stx
		 (string->symbol
		  (string-append (symbol->string (syntax->datum variable-name/stx))
				 "."
				 (symbol->string (syntax->datum field-name/stx))))))


(define (syntax->list stx)
  ;;Given a syntax object STX holding  a list, decompose it and return a
  ;;list of syntax  objects.  Take care of returning  a proper list when
  ;;the input is a syntax object holding a proper list.
  ;;
  ;;This functions  provides a workaround  for bugs in Ikarus  and Mosh,
  ;;which expand syntax objects holding a list into IMproper lists.
  ;;
  (syntax-case stx ()
    (()			'())
    ((?car . ?cdr)	(cons (syntax->list #'?car) (syntax->list #'?cdr)))
    (?atom		#'?atom)))

(define (syntax-prefix prefix-string symbol/stx)
  (datum->syntax symbol/stx
		 (string->symbol (string-append prefix-string
						(symbol->string (syntax->datum symbol/stx))))))

(define (syntax-suffix symbol/stx suffix-string)
  (datum->syntax symbol/stx
		 (string->symbol (string-append (symbol->string (syntax->datum symbol/stx))
						suffix-string))))


(define (all-identifiers? ell/stx)
  (for-all identifier? (syntax->list ell/stx)))

(define (duplicated-identifiers? ell/stx)
  ;;Search the list of  identifier syntax objects ELL/STX for duplicated
  ;;identifiers; return  false of a  syntax object holding  a duplicated
  ;;identifier.
  ;;
  (if (null? ell/stx)
      #f
    (let loop ((x  (car ell/stx))
	       (ls (cdr ell/stx)))
      (if (null? ls)
	  (duplicated-identifiers? (cdr ell/stx))
	(if (bound-identifier=? x (car ls))
	    x
	  (loop x (cdr ls)))))))


(define (%parse-inherit-options inherit-options/stx input-form/stx)
  ;;Here we already know that INHERIT-OPTIONS is a list of identifiers.
  ;;
  (let loop ((concrete-fields	#t)
	     (virtual-fields	#t)
	     (methods		#t)
	     (setter-and-getter	#t)
	     (options		(syntax->datum inherit-options/stx)))
    (if (null? options)
	(list concrete-fields virtual-fields methods setter-and-getter)
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
	 (syntax-violation 'define-class
	   "invalid inheritance option"
	   (syntax->datum input-form/stx) (car options)))))))


(define (%check-clauses only-once-keywords multiple-times-keywords clauses synner)
  ;;Make  sure that  CLAUSES is  an  unwrapped syntax  object list  only
  ;;holding subforms  starting with  a keyword in  ONLY-ONCE-KEYWORDS or
  ;;MULTIPLE-TIMES-KEYWORDS; any order is allowed.
  ;;
  ;;SYNNER must  be the closure  used to raise  a syntax violation  if a
  ;;parse  error  occurs; it  must  accept  two  arguments: the  message
  ;;string, the subform.
  ;;
  (unless (or (null? clauses) (list? clauses))
    (synner "expected possibly empty list of clauses" clauses))

  (for-each
      (lambda (clause)
	(unless (list? clause)
	  (synner "expected list as clause" clause))
	(let ((key (car clause)))
	  (unless (identifier? key)
	    (synner "expected identifier as first clause element" clause))
	  (let ((key (syntax->datum key)))
	    (unless (or (memq key only-once-keywords) (memq key multiple-times-keywords))
	      (synner (string-append
			"unrecognised clause keyword, expected one among: "
			(%keywords-join-for-message (append only-once-keywords multiple-times-keywords)))
		      key))
	    (let ((count (length (filter (lambda (once-key) (eq? key once-key)) only-once-keywords))))
	      (unless (or (zero? count) (= 1 count))
		(synner (string-append "clause " (symbol->string key) " given multiple times")
			clause))))))
    clauses))

(define (%keywords-join-for-message keywords)
  ;;Given  a  list of  symbols,  join  them a  string  with  a comma  as
  ;;separator; return  the string.  To  be used to build  error messages
  ;;involving the list of keywords.
  ;;
  (let ((keys (map symbol->string keywords)))
    (if (null? keys)
	""
      (call-with-values
	  (lambda ()
	    (open-string-output-port))
	(lambda (port getter)
	  (display (car keys) port)
	  (let loop ((keys (cdr keys)))
	    (if (null? keys)
		(getter)
	      (begin
		(display ", " port)
		(display (car keys) port)
		(loop (cdr keys))))))))))

(define (%clause-ref key clauses)
  ;;Given a  list of definition clauses (unwrapped  syntax object), look
  ;;for  the  ones having  KEY  (a symbol)  as  keyword  and return  the
  ;;selected  clauses in  a single  list; return  the empty  list  if no
  ;;matching clause is found.
  ;;
  ;;Examples:
  ;;
  ;;    (%clause-ref 'methods '((<fields> <a> <b>)
  ;;				(<methods> <c> <d>)))
  ;;    => ((methods <c> <d>))
  ;;
  ;;    (%clause-ref 'method  '((<fields> <a> <b>)
  ;;				(<method> <c>)
  ;;				(<method> (<d>) ---)))
  ;;    => ((method <c>) (method (<d>) ---))
  ;;
  (let next-clause ((clauses  clauses)
		    (selected '()))
    (if (null? clauses)
	(reverse selected) ;it is important to keep the order
      (next-clause (cdr clauses)
		   (if (eq? key (syntax->datum (caar clauses)))
		       (cons (car clauses) selected)
		     selected)))))


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
  (let ((clauses (%clause-ref 'inherit clauses)))
    (if (null? clauses)
	(values #'<top>-superlabel #t #t #t)
      (syntax-case (car clauses) (inherit)

	((inherit ?superlabel-name)
	 (identifier? #'?superlabel-name)
	 (values (if (free-identifier=? #'<top> #'?superlabel-name)
		     #'<top>-superlabel
		   #'?superlabel-name)
		 #t #t #t))

	((inherit ?superlabel-name (?inherit-option ...))
	 (all-identifiers? #'(?superlabel-name ?inherit-option ...))
	 (call-with-values
	     (lambda ()
	       (%parse-label-inherit-options #'?inherit-clauses synner))
	   (lambda (virtual-fields methods setter-and-getter)
	     (values (if (free-identifier=? #'<top> #'?superlabel-name)
			 #'<top>-superlabel
		       #'?superlabel-name)
		     virtual-fields methods setter-and-getter))))

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
  (let ((clauses (%clause-ref 'predicate clauses)))
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
  ;;or label) name tho which the  fields belong: it is used to build the
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
  (let next-clause ((clauses   (%clause-ref 'fields clauses))
		    (collected '()))
    (if (null? clauses)
	(reverse collected)
      (syntax-case (car clauses) (fields)
	((fields ?field-clause ...)
	 (next-clause (cdr clauses)
		      (%parse-clause/fields thing-identifier (syntax->list #'(?field-clause ...))
					    synner collected)))
	(_
	 (synner "invalid fields clause" (car clauses)))
	))))

(define (%collect-clause/virtual-fields thing-identifier clauses synner)
  ;;Given  a list  of definition  clauses  in CLAUSES,  extract all  the
  ;;VIRTUAL-FIELDS  clauses  and  parse  them;  there  can  be  multiple
  ;;VIRTUAL-FIELDS clauses in CLAUSES.
  ;;
  ;;THING-IDENTIFIER must be an identifier representing the thing (class
  ;;or label) name tho which the  fields belong: it is used to build the
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
  (let next-clause ((clauses   (%clause-ref 'virtual-fields clauses))
		    (collected '()))
    (if (null? clauses)
	(reverse collected)
      (syntax-case (car clauses) (virtual-fields)
	((virtual-fields ?field-clause ...)
	 (next-clause (cdr clauses)
		      (%parse-clause/virtual-fields thing-identifier (syntax->list #'(?field-clause ...))
						    synner collected)))
	(_
	 (synner "invalid virtual-fields clause" (car clauses)))
	))))

(define (%collect-clause/methods thing-identifier clauses synner)
  ;;Given  a list  of definition  clauses  in CLAUSES,  extract all  the
  ;;METHODS  clauses  and parse  them;  there  can  be multiple  METHODS
  ;;clauses in CLAUSES.
  ;;
  ;;THING-IDENTIFIER must be an identifier representing the thing (class
  ;;or label) name tho which the methods belong: it is used to build the
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
  (let next-clause ((clauses   (%clause-ref 'methods clauses))
		    (collected '()))
    (if (null? clauses)
	(reverse collected)
      (syntax-case (car clauses) (methods)
	((methods ?method-clause ...)
	 (next-clause (cdr clauses)
		      (%parse-clause/methods thing-identifier (syntax->list #'(?method-clause ...))
					     synner collected)))
	(_
	 (synner "invalid methods clause" (car clauses)))
	))))

(define (%collect-clause/method clauses synner define/with-class)
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
  (let next-clause ((clauses		(%clause-ref 'method clauses))
		    (methods		'())
		    (definitions	'()))
    (if (null? clauses)
	(values (reverse methods) (reverse definitions))
      (syntax-case (car clauses) (method)
	((method . ?args)
	 (call-with-values
	     (lambda ()
	       (%parse-clause/method (car clauses) synner  define/with-class))
	   (lambda (m d)
	     (next-clause (cdr clauses) (cons m methods) (cons d definitions)))))
	(_
	 (synner "invalid method clause" (car clauses)))
	))))

(define (%collect-clause/method-syntax clauses synner)
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
  (let next-clause ((clauses		(%clause-ref 'method-syntax clauses))
		    (methods		'())
		    (definitions	'()))
    (if (null? clauses)
	(values (reverse methods) (reverse definitions))
      (syntax-case (car clauses) (method-syntax)
	((method-syntax . ?args)
	 (call-with-values
	     (lambda ()
	       (%parse-clause/method-syntax (car clauses) synner))
	   (lambda (m d)
	     (next-clause (cdr clauses) (cons m methods) (cons d definitions)))))
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
  (let ((clauses (%clause-ref 'setter clauses)))
    (if (null? clauses)
	#f
      (syntax-case (car clauses) (setter)
	((setter ?setter)
	 (identifier? #'?setter)
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
  (let ((clauses (%clause-ref 'getter clauses)))
    (if (null? clauses)
	#f
      (syntax-case (car clauses) (getter)
	((getter ?getter)
	 (identifier? #'?getter)
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
  (let ((clauses (%clause-ref 'bindings clauses)))
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
  ;;label) name  tho which the  fields belong: it  is used to  build the
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
      (reverse collected-fields) ;it is important to keep the order
    (syntax-case (car field-clauses) (mutable immutable)

      ((mutable ?field ?accessor ?mutator)
       (all-identifiers? #'(?field ?accessor ?mutator))
       (recurse (list (datum->syntax #'?field 'mutable) #'?field #'?accessor #'?mutator)))

      ((mutable ?field)
       (identifier? #'?field)
       (recurse (list (datum->syntax #'?field 'mutable)
		      #'?field
		      (syntax-accessor-name thing-name #'?field)
		      (syntax-mutator-name  thing-name #'?field))))

      ((immutable ?field ?accessor)
       (all-identifiers? #'(?field ?accessor ?mutator))
       (recurse (list (datum->syntax #'?field 'immutable)
		      #'?field
		      #'?accessor)))

      ((immutable ?field)
       (identifier? #'?field)
       (recurse (list (datum->syntax #'?field 'immutable)
		      #'?field
		      (syntax-accessor-name thing-name #'?field))))

      (?field
       (identifier? #'?field)
       (recurse (list (datum->syntax #'?field 'immutable)
		      #'?field
		      (syntax-accessor-name thing-name #'?field))))

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
  ;;label) name  tho which the  fields belong: it  is used to  build the
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
       (recurse (list (datum->syntax #'?field 'mutable)
		      #'?field #'?accessor #'?mutator)))

      ((mutable ?field)
       (identifier? #'?field)
       (recurse (list (datum->syntax #'?field 'mutable)
		      #'?field
		      (syntax-accessor-name thing-name #'?field)
		      (syntax-mutator-name  thing-name #'?field))))

      ((immutable ?field ?accessor)
       (all-identifiers? #'(?field ?accessor ?mutator))
       (recurse (list (datum->syntax #'?field 'immutable)
		      #'?field
		      #'?accessor)))

      ((immutable ?field)
       (identifier? #'?field)
       (recurse (list (datum->syntax #'?field 'immutable)
		      #'?field
		      (syntax-accessor-name thing-name #'?field))))

      (?field
       (identifier? #'?field)
       (recurse (list (datum->syntax #'?field 'immutable)
		      #'?field
		      (syntax-accessor-name thing-name #'?field))))

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
    (syntax-case (car methods-clauses) ()

      ((?method ?method-name)
       (and (identifier? #'?method) (identifier? #'?method-name))
       (recurse (list #'?method #'?method-name)))

      ((?method)
       (identifier? #'?method)
       (recurse (list #'?method (syntax-method-name thing-name #'?method))))

      (?method
       (identifier? #'?method)
       (recurse (list #'?method (syntax-method-name thing-name #'?method))))

      (_
       (synner "invalid methods clause" (car methods-clauses)))
      )))

(define (%parse-clause/method clause synner define/with-class)
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
     (with-syntax ((FUNCTION-NAME (datum->syntax #'?method (gensym))))
       (values #'(?method FUNCTION-NAME)
	       #`(#,define/with-class (FUNCTION-NAME . ?args) . ?body))))

    ((method ?method ?expression)
     (identifier? #'?method)
     (with-syntax ((FUNCTION-NAME (datum->syntax #'?method (gensym))))
       (values #'(?method FUNCTION-NAME)
	       #'((define FUNCTION-NAME ?expression)))))

    (_
     (synner "invalid method clause" clause))
    ))

(define (%parse-clause/method-syntax clause synner)
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
  (syntax-case clause (method-syntax)

    ((method-syntax ?method ?transformer)
     (identifier? #'?method)
     (with-syntax ((MACRO-NAME (datum->syntax #'?method (gensym))))
       (values #'(?method MACRO-NAME)
	       #'(define-syntax MACRO-NAME ?transformer))))

    (_
     (synner "invalid method-syntax clause" clause))
    ))


;;;; done

)

;;; end of file
