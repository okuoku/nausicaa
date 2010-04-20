;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: handling import sets
;;;Date: Fri Apr 16, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Jargon
;;;------
;;;
;;;List of renamings
;;;
;;;	A "list of  renamings" is a list of lists;  each sublist has two
;;;	symbols as elements:
;;;
;;;		((internal-symbol0	external-symbol0)
;;;		 (internal-symbol	external-symbol)
;;;		 ...)
;;;
;;;	a  list of renamings  represents the  identifiers exported  by a
;;;	library or the identifiers imported by a library.
;;;
;;;	In the  first case:  the external symbol  is the one  visible by
;;;	code which imports this library;  the internal symbol is the one
;;;	bound with DEFINE or DEFINE-SYNTAX in the body of this library.
;;;
;;;	In the  second case: the external  symbol is the  one visible in
;;;	the  body  of this  library;  the  internal  symbol is  the  one
;;;	exported by the imported library.
;;;
;;;	If we know  the library name and the renamings,  we can build an
;;;	import set with:
;;;
;;;		(only (rename <library-name> . <renamings>))
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


(library (libraries import-specs)
  (export

    <import-spec>
    make-<import-spec>			<import-spec>?
    <import-spec>-import-set
    <import-spec>-import-levels
    <import-spec>-original
    <import-spec>-library-reference

    <import-set>
    make-<import-set>			<import-set>?
    <import-set>-subset
    <import-set>-bindings-transformer
    <import-set>-original
    <import-set>-library-reference

    apply-import-spec/only
    apply-import-spec/except
    apply-import-spec/prefix
    apply-import-spec/rename
    )
  (import (nausicaa)
    (matches)
    (libraries helpers)
    (libraries references))


(define-class <import-spec>
  (fields (immutable import-set)
		;The  import set  of this  specification; it  can  be an
		;<import-set> object or <library-reference> object.
	  (immutable import-levels)
		;A list of exact integers representing the import levels
		;for which  this import set was  requested.  It contains
		;at least one element.
	  (immutable original))
		;The original import specification.
  (virtual-fields (immutable library-reference))
  (protocol (lambda (make-parent)
	      (lambda (sexp)
		(receive (import-set import-levels)
		    (%import-spec-parse sexp)
		  ((make-parent) import-set import-levels sexp)))))

  (method (apply (o <import-spec>) renamings)
    (assert (%list-of-renamings? renamings))
    (if (is-a? o.import-set <library-reference>)
	renamings
      (with-fields ((o.import-set <import-set>))
	(o.import-set.apply renamings))))

  (nongenerative nausicaa:libraries:<import-spec>))

(define (%import-spec-parse sexp)
  ;;Parse SEXP  as a  library import specification  as defined  by R6RS;
  ;;return  two values: the  import set,  the list  of import  levels as
  ;;exact integers.
  ;;
  (match sexp

    (('for ?import-set)
     (values (make-<import-set> ?import-set) '(0)))

    (('for ?import-set ?import-levels ...)
     (values (make-<import-set> ?import-set)
	     (map (lambda (level)
		    (match level
		      ('run	'0)
		      ('expand	'1)
		      (('meta (:predicate integer? (:predicate exact? ?level)))
		       ?level)
		      (_
		       (assertion-violation '%import-levels-parse "invalid import level" level))))
	       ?import-levels)))

    (?import-set
     (values (make-<import-set> ?import-set) '(0)))))

(define (<import-spec>-library-reference (o <import-spec>))
  (if (is-a? o.import-set <library-reference>)
      o.import-set
    (with-fields ((o.import-set <import-set>))
      o.import-set.library-reference)))


(define-class <import-set>
  (fields (immutable subset)
		;The  core  of  this  import  set;  it  can  be  another
		;<import-set> or a <library-reference>.
	  (immutable bindings-transformer)
		;A  function  to apply  to  a  renamings  to obtain  the
		;imported bindings.
	  (immutable original))
		;The original symbolic expression of this import set.
  (virtual-fields (immutable library-reference))
  (protocol (lambda (make-parent)
	      (lambda (sexp)
		(receive (subset bindings-transformer)
		    (%import-set-parse sexp)
		  ((make-parent) subset bindings-transformer sexp)))))

  (method (apply (o <import-set>) renamings)
    (o.bindings-transformer renamings))

  (nongenerative nausicaa:libraries:<import-set>))

(define (%import-set-parse sexp)
  ;;Parse SEXP as  an import set as defined by  R6RS; return two values:
  ;;the subset which can be  an <import-set> or a <library-reference>, a
  ;;function which can be used  to apply the specified transformation to
  ;;a list of renamings.
  ;;
  (match sexp

    ;;The RENAME clause can  appear with and without renamings; recurse,
    ;;avoiding the creation of an empty import set.
    (('rename ?import-set)
     (%import-set-parse ?import-set))

    ;;Match the RENAME clause.
    (('rename ?import-set . (:predicate %list-of-renamings? ?renamings-spec))
     (receive (subset transformer)
	 (%import-set-parse ?import-set)
       (values subset
	       (lambda (renamings)
		 (apply-import-spec/rename (transformer renamings)
					   ?renamings-spec)))))

    ;;The  ONLY clause  can  appear with  and  without symbols;  without
    ;;symbols means exclude all the bindings.
    (('only ?import-set)
     (receive (subset transformer)
	 (%import-set-parse ?import-set)
       (values subset
	       (lambda (renamings) '()))))

    (('only ?import-set . (:predicate %list-of-symbols? ?list-of-identifiers))
     (receive (subset transformer)
	 (%import-set-parse ?import-set)
       (values subset
	       (lambda (renamings)
		 (apply-import-spec/only (transformer renamings)
					 ?list-of-identifiers)))))

    ;;The  EXCEPT clause can  appear with  and without  symbols; without
    ;;symbols  means  take  all  the  bindings.  Recurse  to  avoid  the
    ;;creation of an empty import set.
    (('except ?import-set)
     (%import-set-parse ?import-set))

    (('except ?import-set . (:predicate %list-of-symbols? ?list-of-identifiers))
     (receive (subset transformer)
	 (%import-set-parse ?import-set)
       (values subset
	       (lambda (renamings)
		 (apply-import-spec/except (transformer renamings)
					   ?list-of-identifiers)))))

    ;;The PREFIX clause must appear with a symbolic prefix.
    (('prefix ?import-set (:predicate symbol? ?prefix))
     (receive (subset transformer)
	 (%import-set-parse ?import-set)
       (values subset
	       (lambda (renamings)
		 (apply-import-spec/prefix (transformer renamings)
					   ?prefix)))))

    ;;The LIBRARY  clause allows library  names starting
    ;;with FOR, ONLY, etc.
    (('library (:predicate library-reference? ?library-reference))
     (values (make <library-reference> ?library-reference)
	     (lambda (renamings)
	       renamings)))

    ;;A plain library name.
    ((:predicate library-reference? ?library-reference)
     (values (make <library-reference> ?library-reference)
	     (lambda (renamings)
	       renamings)))

    ;;Everything else is an error.
    (_
     (assertion-violation '%import-set-parse "invalid import set" sexp))))

(define (<import-set>-library-reference (o <import-set>))
  (if (is-a? o.subset <library-reference>)
      o.subset
    (with-fields ((o.subset <import-set>))
      o.subset.library-reference)))


(define (apply-import-spec/only renamings list-of-identifiers)
  ;;Apply the  ONLY import specification.   RENAMINGS must be a  list of
  ;;renamings.   LIST-OF-IDENTIFIERS  must be  the  list  of symbols  to
  ;;accept in the RENAMINGS, all the others are discarded.
  ;;
  (filter (lambda (renaming)
	    (memq (cadr renaming) list-of-identifiers))
    renamings))

(define (apply-import-spec/except renamings list-of-identifiers)
  ;;Apply the EXCEPT import specification.   RENAMINGS must be a list of
  ;;renamings.   LIST-OF-IDENTIFIERS  must be  the  list  of symbols  to
  ;;discard in the RENAMINGS, all the others are accepted.
  ;;
  (filter (lambda (renaming)
	    (not (memq (cadr renaming) list-of-identifiers)))
    renamings))

(define (apply-import-spec/prefix renamings prefix-symbol)
  ;;Apply the PREFIX import specification.   RENAMINGS must be a list of
  ;;renamings.  PREFIX-SYMBOL must be the symbol to prefix to the second
  ;;symbol in the RENAMINGS.
  ;;
  (let ((prefix-string (symbol->string prefix-symbol)))
    (map (lambda (renaming)
	   (list (car renaming)
		 (string->symbol (string-append prefix-string
						(symbol->string (cadr renaming))))))
      renamings)))

(define (apply-import-spec/rename renamings renamings-spec)
  ;;Apply the RENAME import specification.   RENAMINGS must be a list of
  ;;renamings.  RENAMINGS-SPEC must be  the list of renamings specifying
  ;;the rename operation for RENAMINGS.
  ;;
  (reverse (fold-left (lambda (knil single-renaming)
			(let ((res (exists (lambda (spec)
					     (if (eq? (cadr single-renaming) (car spec))
						 (list (car single-renaming) (cadr spec))
					       #f))
					   renamings-spec)))
			  (cons (if res res single-renaming) knil)))
		      '()
		      renamings)))


;;;; done

)

;;; end of file
