;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: R6RS library form inspection
;;;Date: Tue Apr  6, 2010
;;;
;;;Abstract
;;;--------
;;;
;;;	This library implements functions to inspect the S-expression of
;;;	LIBRARY forms as defined by R6RS.
;;;
;;;Jargon
;;;------
;;;
;;;RENAMINGS
;;;
;;;	A "renamings" is  a list of lists; each  sublist has two symbols
;;;	as elements:
;;;
;;;		((internal-symbol0	external-symbol0)
;;;		 (internal-symbol	external-symbol)
;;;		 ...)
;;;
;;;	a renamings represents the  identifiers exported by a library or
;;;	the identifiers imported by a library.
;;;
;;;	In the  first case:  the external symbol  is the one  visible by
;;;	code which imports this library;  the internal symbol is the one
;;;	bound with DEFINE or DEFINE-SYNTAX in the body of this library.
;;;
;;;	In the  second case: the external  symbol is the  one visible in
;;;	the  body  of this  library;  the  internal  symbol is  the  one
;;;	exported by the imported library.
;;;
;;;	If we know  the library specification and the  renamings, we can
;;;	build an import set with:
;;;
;;;		(only (rename <library-name> . <renamings>))
;;;
;;;SINGLE-RENAMING
;;;
;;;	It is a list of two symbols:
;;;
;;;		(internal-symbol	external-symbol)
;;;
;;;	every element in a "renamings" is a "single-renaming".
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


(library (libraries)
  (export

    <library>					<library>-with-record-fields-of
    make-<library>				<library>?
    <library>-exports
    <library>-imported-libraries
    <library>-requested-library-name
    <library>-library-name
    <library>-library-version
    <library>-imported-bindings

    ;; conditions
    &library-name
    make-library-name-condition
    library-name-condition?
    condition-library-name

    &library-invalid-sexp
    make-library-invalid-sexp-condition
    library-invalid-sexp-condition?
    condition-library-invalid-sexp
    error-library-invalid-sexp

    &library-not-found
    make-library-not-found-condition
    library-not-found-condition?
    error-library-not-found

    &library-invalid-exports
    make-library-invalid-exports-condition
    library-invalid-exports-condition?
    error-library-invalid-exports

    &library-invalid-import-set
    make-library-invalid-import-set-condition
    library-invalid-import-set-condition?
    condition-library-invalid-import-set
    error-library-invalid-import-set

    &library-invalid-phase-spec
    make-library-invalid-phase-spec-condition
    library-invalid-phase-spec-condition?
    condition-library-invalid-phase-spec
    error-library-invalid-phase-spec

    ;;search path
    library-search-path-environment-variable
    get-search-path
    get-search-path-function

    ;;loading
    load-library				load-library-function
    load-library-from-file			ignore-library-version
    library-reference-resolve
    )
  (import (nausicaa)
    (compensations)
    (lists)
    (strings)
    (char-sets)
    (matches)
    (libraries rnrs-bindings)
    (libraries low))


;;;; condition objects

(define-condition-type &library-name
  &condition
  make-library-name-condition
  library-name-condition?
  (spec	condition-library-name))

;;; --------------------------------------------------------------------

(define-condition-type &library-invalid-sexp
  &error
  make-library-invalid-sexp-condition
  library-invalid-sexp-condition?
  (sexp		condition-library-invalid-sexp))

(define-inline (error-library-invalid-sexp ?who ?library-name ?library-sexp)
  (raise (condition
	  (make-who-condition ?who)
	  (make-message-condition "invalid LIBRARY form S-expression")
	  (make-library-name-condition ?library-name)
	  (make-library-invalid-sexp-condition ?library-sexp))))

;;; --------------------------------------------------------------------

(define-condition-type &library-not-found
  &error
  make-library-not-found-condition
  library-not-found-condition?)

(define-inline (error-library-not-found ?who ?library-name)
  (raise (condition
	  (make-who-condition ?who)
	  (make-message-condition "library not found")
	  (make-library-name-condition ?library-name)
	  (make-library-not-found-condition))))

;;; --------------------------------------------------------------------

(define-condition-type &library-invalid-exports
  &error
  make-library-invalid-exports-condition
  library-invalid-exports-condition?
  (export-spec	condition-library-invalid-exports))

(define-inline (error-library-invalid-exports ?who ?library-name ?export-spec)
  (raise (condition
	  (make-who-condition ?who)
	  (make-message-condition "invalid library exports specification")
	  (make-library-name-condition ?library-name)
	  (make-library-invalid-exports-condition ?export-spec))))

;;; --------------------------------------------------------------------

(define-condition-type &library-invalid-import-set
  &error
  make-library-invalid-import-set-condition
  library-invalid-import-set-condition?
  (import-set	condition-library-invalid-import-set))

(define-inline (error-library-invalid-import-set ?who ?library-name ?import-set)
  (raise (condition
	  (make-who-condition ?who)
	  (make-message-condition "invalid library import set specification")
	  (make-library-name-condition ?library-name)
	  (make-library-invalid-import-set-condition ?import-set))))

;;; --------------------------------------------------------------------

(define-condition-type &library-invalid-phase-spec
  &error
  make-library-invalid-phase-spec-condition
  library-invalid-phase-spec-condition?
  (phase-spec	condition-library-invalid-phase-spec))

(define-inline (error-library-invalid-phase-spec ?who ?library-name ?library-phase-spec)
  (raise (condition
	  (make-who-condition ?who)
	  (make-message-condition "invalid library import phase specification")
	  (make-library-name-condition ?library-name)
	  (make-library-invalid-phase-spec-condition ?library-phase-spec))))


(define-class <raw-library>
  ;;This class is for raw informations extracted from the library.
  ;;
  (fields (immutable requested-library-name)
		;The  library specification, as  defined by  R6RS, which
		;was used to find this library; it can be different from
		;the one in the  library form.
	  (immutable library-sexp)
		;The  full  S-expression of  the  library:  this is  the
		;LIBRARY form.
	  (immutable library-name)
		;The  library specification, as  defined by  R6RS, which
		;was found in the LIBRARY form.
	  (immutable library-version)
		;The version specification  in the library specification
		;from the LIBRARY form; it can be null if no version was
		;specified.
	  (mutable exports)
		;The raw  content of the  EXPORT clause.  It is  null if
		;the library exports nothing.
	  (mutable imports)
		;The  raw  content of  the  IMPORT  clause.  The  IMPORT
		;clause is not allowed to  be null, so this field cannot
		;be null either.
	  (mutable body)
		;The body of the library;  it is null if the library has
		;empty body.
	  )
  (protocol
   (lambda (make-<top>)
     (lambda/with* ((requested-library-name <list>) (library-sexp <list>))
       (match library-sexp
	 (('library (:predicate %library-name? ?library-name)
	    ('export . ?exports) ;?exports can be null
	    ('import . ?imports) ;?imports can be null
	    . ?body) ;?body can be null
	  ((make-<top>) requested-library-name library-sexp
	   ?library-name
	   (%library-version-ref ?library-name)
	   ?exports ?imports ?body))
	 (_
	  (error-library-invalid-sexp 'make-<raw-library> requested-library-name library-sexp))))))
  (nongenerative nausicaa:libraries:<raw-library>))


(define-class <import-spec>
  (fields (immutable library-name)
		;The library name, as  defined by R6RS, specified in the
		;import set.
	  (immutable library-reference)
		;The library reference in the import set.
	  (immutable phases)
		;A list  of exact  integers representing the  phases for
		;which this  import set  was requested.  It  contains at
		;least one element.
	  (immutable import-set)
		;The original import set.
	  )
  (protocol (lambda (make-<top>)
	      (lambda (import-set library-name)

		(define (main)
		  (receive (library-reference phases)
		      (match import-set
			;;The FOR  clause is parsed here  because it can
			;;appear only once.
			(('for ?import-set)
			 (values (%take-import-spec ?import-set) 0))
			(('for ?import-set . ?phases)
			 (values (%take-import-spec ?import-set)
				 (%process-phases ?phases)))
			;;Everything  else is  handed  to the  recursive
			;;function.
			(?import-set
			 (values (%take-import-spec ?import-set) 0)))
		    ((make-<top>) (library-reference-resolve library-reference)
		     library-reference phases import-set)))

		(define (%take-import-spec spec)
		  (match spec
		    ;;The  RENAME  clause can  appear  with and  without
		    ;;renamings.
		    (('rename ?import-set)
		     (%take-import-spec ?import-set))
		    (('rename ?import-set . _)
		     (%take-import-spec ?import-set))

		    ;;The  ONLY  clause  can  appear  with  and  without
		    ;;symbols.
		    (('only ?import-set)
		     (%take-import-spec ?import-set))
		    (('only ?import-set . _)
		     (%take-import-spec ?import-set))

		    ;;The  EXCEPT  clause can  appear  with and  without
		    ;;symbols.
		    (('except ?import-set)
		     (%take-import-spec ?import-set))
		    (('except ?import-set . _)
		     (%take-import-spec ?import-set))

		    ;;The  PREFIX  clause must  appear  with a  symbolic
		    ;;prefix.
		    (('prefix ?import-set (:predicate symbol? ?prefix))
		     (%take-import-spec ?import-set))

		    ;;The LIBRARY  clause allows library  names starting
		    ;;with FOR, ONLY, etc.
		    (('library (:predicate %library-reference? ?library-reference))
		     ?library-reference)

		    ;;A plain library name.
		    ((:predicate %library-reference? ?library-reference)
		     ?library-reference)

		    ;;Everything else is an error.
		    (?import-set
		     (error-library-invalid-import-set 'make-<import-spec> library-name ?import-set))))

	      (define (%process-phases phases-list)
		(map (lambda (phase)
		       (match phase
			 (('meta (:predicate %phase-number? ?phase))
			  ?phase)
			 ((and (? symbol?) ?phase)
			  (case ?phase
			    ((run)    0)
			    ((expand) 0)
			    (else
			     (error-library-invalid-phase-spec 'make-<import-spec>
							       library-name ?phase))))
			 (?phase-spec
			  (error-library-invalid-phase-spec 'make-<import-spec>
							    library-name ?phase-spec))))
		  phases-list))

	      (main))))
  (nongenerative nausicaa:libraries:<import-spec>))


(define-class <library>
  (parent <raw-library>)
  (fields (mutable exports)
		;Renamings representing the export list.
	  (mutable import-specs)
		;List of <import-spec> records; it represents the import
		;sets of this library.   Notice that an imported library
		;can appear in more import sets.
	  )
  (virtual-fields (immutable requested-library-name	<raw-library>-requested-library-name)
		  (immutable library-name		<raw-library>-library-name)
		  (immutable library-version		<raw-library>-library-version)
		  (immutable imported-libraries		<library>-imported-libraries))
  (methods imported-bindings)

  (protocol (lambda (make-<raw-library>)
	      (lambda/with* ((requested-library-name <list>) (library-sexp <list>))

		(define (main)
		  (begin0-let ((lib ((make-<raw-library> requested-library-name library-sexp)
				     #f #f))) ;exports import-specs
		    (%build-exports! lib)
		    (%build-import-specs! lib)))

		(define (%build-exports! (lib <library>))
		  (let-fields (((raw <raw-library>) lib))
		    (set! lib.exports
			  (reverse
			   (fold-left
			    (lambda (knil spec)
			      (define (%error)
				(error-library-invalid-exports 'make-<library> lib.library-name spec))
			      (match spec
				((:predicate symbol? ?identifier)
				 `((,?identifier ,?identifier) . ,knil))
				(('rename . ?renamings)
				 (if (%list-of-renamings? ?renamings)
				     (append (reverse ?renamings) knil)
				   (%error)))
				(_
				 (%error))))
			    '()
			    raw.exports)))))

		(define (%build-import-specs! (lib <library>))
		  (let-fields (((raw <raw-library>) lib))
		    (set! lib.import-specs
			  (if (null? raw.imports)
			      '()
			    (map (lambda (import-set)
				   (make-<import-spec> import-set lib.library-name))
			      raw.imports)))))
		(main))))
  (nongenerative nausicaa:libraries:<library>))

(define (<library>-library-version (o <library>))
  o.library-version)

(define (<library>-requested-library-name (o <library>))
  o.requested-library-name)

(define (<library>-library-name (o <library>))
  o.library-name)

(define (<library>-imported-libraries (lib <library>))
  (delete-duplicates (map (lambda/with ((spec <import-spec>))
			    spec.library-name)
		       lib.import-specs)))


;;;; libraries search path

(define library-search-path-environment-variable
  (make-parameter "SCHEME_LIBPATH"
    (lambda (name)
      (assert (string? name))
      name)))

(define (get-search-path)
  (string-tokenize (get-environment-variable (library-search-path-environment-variable))
		   (char-set-complement (char-set #\:) char-set:full)))

(define get-search-path-function
  (make-parameter get-search-path
    (lambda (f)
      (assert (procedure? f))
      f)))

(define (get-library-pathname spec)
  ;;Given a library specification, find  the pathname of its file on the
  ;;system.  Return the pathname as a string or #f if not found.
  ;;
  ;;SPEC  must  be  a  library  specification as  defined  in  the  R6RS
  ;;document.   The version  specification,  if present,  is taken  into
  ;;account  depending  on   the  value  of  the  IGNORE-LIBRARY-VERSION
  ;;parameter: if #f it is removed before building the pathname.
  ;;

  (let* ((spec (if (pair? (last spec))
		   (if (ignore-library-version)
		       (drop-right spec 1)
		     (append (drop-right spec 1)
			     (list (string-join (map number->string (take-right spec 1)) "."))))
		 spec))
	 (name (string-join (map symbol->string/maybe spec) "/"))
	 (dirs ((get-search-path-function))))
    (exists (lambda (dir)
	      (exists (lambda (ext)
			(let ((pathname (string-append dir "/" name ext)))
			  (if (file-exists? pathname)
			      pathname
			    #f)))
		      $extensions))
	    dirs)))


;;;; library loading from file

(define ignore-library-version
  (make-parameter #t))

(define-constant $extensions
  (list (cond-expand
	 (ikarus	".ikarus.sls")
	 (larceny	".larceny.sls")
	 (mosh		".mosh.sls")
	 (petite	".petite.sls")
	 (ypsilon	".ypsilon.sls"))
	".sls" ".scm" ".ss"))

(define get-library-pathname-function
  (make-parameter get-library-pathname
    (lambda (f)
      (assert (procedure? f))
      f)))

(define (load-library-from-file spec)
  ;;Search a library  on the system and load it.  Return  the sexp or #f
  ;;if not found.
  ;;
  (let ((pathname ((get-library-pathname-function) spec)))
    (if pathname
	(with-compensations
	  (letrec ((port (compensate
			     (open-input-file pathname)
			   (with
			    (close-port port)))))
	    (read port)))
      #f)))


;;;; resolving library references

(define (library-reference-resolve library-reference)
  library-reference)


;;;; library registry

(define-constant $library-registry
  (make-hashtable equal-hash equal?))

(define load-library-function
  (make-parameter load-library-from-file
    (lambda (f)
      (assert (procedure? f))
      f)))

(define (load-library spec)
  (or (hashtable-ref $library-registry spec #f)
      (let ((sexp ((load-library-function) spec)))
	(if sexp
	    (begin0-let ((lib (make-<library> spec sexp)))
	      (hashtable-set! $library-registry spec lib))
	  (error-library-not-found 'load-library spec)))))


;;;; libraries import bindings

(define (<library>-imported-bindings (lib <library>))
  (define (main)
    (let-fields (((raw <raw-library>) lib))
      (concatenate
       (map (lambda (spec)
	      (match spec
		(('for ?import-set)
		 (%extract-bindings-from-import-spec ?import-set))
		(('for ?import-set _)
		 (%extract-bindings-from-import-spec ?import-set))
		(?import-set
		 (%extract-bindings-from-import-spec ?import-set))))
	 raw.imports))))

  (define (%extract-bindings-from-import-spec spec)
    (match spec
      (('rename ?import-set)
       (%extract-bindings-from-import-spec ?import-set))
      (('rename ?import-set . list-of-renamings)
       (%extract-bindings-from-import-spec ?import-set))

      (('only ?import-set) ;exclude all the bindings
       '())
      (('only ?import-set . ?list-of-ids)
       (%apply-import-spec/only (%extract-bindings-from-import-spec ?import-set)
				?list-of-ids))

      (('except ?import-set)
       (%extract-bindings-from-import-spec ?import-set))
      (('except ?import-set . ?list-of-ids)
       (%apply-import-spec/except (%extract-bindings-from-import-spec ?import-set) ?list-of-ids))

      (('prefix ?import-set (:predicate symbol? ?prefix))
       (%apply-import-spec/prefix (%extract-bindings-from-import-spec ?import-set)))

      (('library (:predicate %library-reference? ?library-reference))
       (<library>-exports (load-library ?library-reference)))

      ((:predicate %library-reference? ?library-reference)
       (<library>-exports (load-library ?library-reference)))

      (?import-set
       (error-library-invalid-import-set '<library>-imported-bindings lib.library-name ?import-set))))

  (main))


;;;; rnrs libraries

(define (%symbols->renamings list-of-symbols)
  (map (lambda (sym)
	 (list sym sym))
    list-of-symbols))

(define (%register-rnrs-lib library-name bindings)
  (let*-fields (((spec <top>)		library-name)
		((lib  <library>)	(make-<library> spec `(library ,library-name
								(export ,@bindings)
								(import))))
		((raw  <raw-library>)	lib))
    (hashtable-set! $library-registry spec lib)
    (hashtable-set! $library-registry (drop-right spec 1) lib)
		;remove the version spec
    ))

(%register-rnrs-lib '(rnrs base (6))			rnrs-base-6)
(%register-rnrs-lib '(rnrs unicode (6))			rnrs-unicode-6)
(%register-rnrs-lib '(rnrs bytevectors (6))		rnrs-bytevectors-6)
(%register-rnrs-lib '(rnrs lists (6))			rnrs-lists-6)
(%register-rnrs-lib '(rnrs sorting (6))			rnrs-sorting-6)
(%register-rnrs-lib '(rnrs control (6))			rnrs-control-6)
(%register-rnrs-lib '(rnrs records syntactic (6))	rnrs-records-syntactic-6)
(%register-rnrs-lib '(rnrs records procedural (6))	rnrs-records-procedural-6)
(%register-rnrs-lib '(rnrs records inspection (6))	rnrs-records-inspection-6)
(%register-rnrs-lib '(rnrs exceptions (6))		rnrs-exceptions-6)
(%register-rnrs-lib '(rnrs conditions (6))		rnrs-conditions-6)
(%register-rnrs-lib '(rnrs io ports (6))		rnrs-io-ports-6)
(%register-rnrs-lib '(rnrs io simple (6))		rnrs-io-simple-6)
(%register-rnrs-lib '(rnrs files (6))			rnrs-files-6)
(%register-rnrs-lib '(rnrs enums (6))			rnrs-enums-6)
(%register-rnrs-lib '(rnrs programs (6))		rnrs-programs-6)
(%register-rnrs-lib '(rnrs arithmetic fixnums (6))	rnrs-arithmetic-fixnums-6)
(%register-rnrs-lib '(rnrs arithmetic flonums (6))	rnrs-arithmetic-flonums-6)
(%register-rnrs-lib '(rnrs arithmetic bitwise (6))	rnrs-arithmetic-bitwise-6)
(%register-rnrs-lib '(rnrs syntax-case (6))		rnrs-syntax-case-6)
(%register-rnrs-lib '(rnrs hashtables (6))		rnrs-hashtables-6)

(parametrise ((load-library-function (lambda (spec)
				       `(library (rnrs (6))
					  (export ,@rnrs-6)
					  (import (rnrs base (6))
					    (rnrs unicode (6))
					    (rnrs bytevectors (6))
					    (rnrs lists (6))
					    (rnrs sorting (6))
					    (rnrs control (6))
					    (rnrs records syntactic (6))
					    (rnrs records procedural (6))
					    (rnrs records inspection (6))
					    (rnrs exceptions (6))
					    (rnrs conditions (6))
					    (rnrs io ports (6))
					    (rnrs io simple (6))
					    (rnrs files (6))
					    (rnrs enums (6))
					    (rnrs programs (6))
					    (rnrs arithmetic fixnums (6))
					    (rnrs arithmetic flonums (6))
					    (rnrs arithmetic bitwise (6))
					    (rnrs syntax-case (6))
					    (rnrs hashtables (6)))))))
  (load-library '(rnrs (6))))


;;;; done

)

;;; end of file
