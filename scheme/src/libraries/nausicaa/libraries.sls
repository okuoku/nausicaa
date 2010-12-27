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

    <library>
    make-<library>				<library>?
    <library>-sexp
    <library>-name-sexp
    <library>-exports-sexp
    <library>-imports-sexp
    <library>-body
    <library>-name
    <library>-exports
    <library>-import-specs
    <library>-references

    ;; utilities
    library-search-path-environment-variable
    library-search-path
    library-search-path-function
    library-file-extensions
    scan-library-search-path
    scan-library-search-path-function
    load-libraries-from-files
    load-library-function
    load-library
    )
  (import (nausicaa)
    (compensations)
    (lists)
    (strings)
    (char-sets)
    (matches)
    (libraries rnrs-bindings)
    (libraries names)
    (libraries references)
    (libraries import-specs)
    (libraries helpers)
    (libraries conditions))


(define-class <library>
  (fields (immutable sexp)
		;The  full  S-expression of  the  library:  this is  the
		;LIBRARY form.
	  (immutable name-sexp)
		;The library name  S-expression object being the library
		;name from the LIBRARY form.
	  (immutable exports-sexp)
		;The raw  content of the  EXPORT clause.  It is  null if
		;the library exports nothing.
	  (immutable imports-sexp)
		;The  raw  content of  the  IMPORT  clause.  The  IMPORT
		;clause is not allowed to  be null, so this field cannot
		;be null either.
	  (immutable body)
		;The body of the library;  it is null if the library has
		;empty body.

	  (immutable name)
		;A  <library-name>  record  representing this  library's
		;name.
	  (immutable exports)
		;List of renamings representing the export list.
	  (immutable import-specs)
		;List of <import-spec> records; it represents the import
		;sets of this library.   Notice that an imported library
		;can appear in more import sets.
	  )
  (virtual-fields (immutable references <library>-references))
		;List of  <library-references> representing the  list of
		;imported libraries.
  (methods imported-bindings)

  (protocol (lambda (make-top)
	      (lambda (sexp)
		(match sexp
		  (('library (:predicate library-name? ?name-sexp)
		     ('export . ?exports-sexp) ;?exports can be null
		     ('import . ?imports-sexp) ;?imports can be null
		     . ?body)		       ;?body can be null
		   ((make-top) sexp ?name-sexp ?exports-sexp ?imports-sexp ?body
		    (make <library-name> ?name-sexp)
		    (%list-of-renamings-from-exports-sexp 'make-<library> ?exports-sexp)
		    (map make-<import-spec> ?imports-sexp)))
		  (_
		   (error 'make-<raw-library>
		     "invalid library symbolic expression" sexp))))))

  (nongenerative nausicaa:libraries:<library>))

(define (%list-of-renamings-from-exports-sexp who exports-sexp)
  (reverse (fold-left (lambda (knil spec)
			(match spec
			  ((:predicate symbol? ?identifier)
			   `((,?identifier ,?identifier) . ,knil))
			  (('rename . (:predicate %list-of-renamings? ?renamings))
			   (append (reverse ?renamings) knil))
			  (_
			   (error who "invalid exports list in library form" exports-sexp))))
		      '()
		      exports-sexp)))

(define (<library>-references (lib <library>))
  (delete-duplicates (map (lambda ((spec <import-spec>))
			    spec.library-reference)
		       lib.import-specs)))


;;;; libraries search path

(define library-search-path-environment-variable
  (make-parameter "SCHEME_LIBPATH"
    (lambda (name)
      (assert (string? name))
      name)))

(define (library-search-path)
  (string-tokenize (get-environment-variable (library-search-path-environment-variable))
		   (char-set-complement (char-set #\:) char-set:full)))

(define library-search-path-function
  (make-parameter library-search-path
    (lambda (f)
      (assert (procedure? f))
      f)))


;;;; finding libraries on file systems

(define-constant library-file-extensions
  (list (cond-expand
	 (ikarus	".ikarus.sls")
	 (larceny	".larceny.sls")
	 (mosh		".mosh.sls")
	 (petite	".petite.sls")
	 (vicare	".vicare.sls")
	 (ypsilon	".ypsilon.sls"))
	".sls" ".scm" ".ss"))

(define (scan-library-search-path (ref <library-reference>))
  ;;Scan the  search path for  library files whose pathname  matches the
  ;;list of  identifiers from the  library reference.  Return a  list of
  ;;strings, possibly null, representing the library pathnames.
  ;;
  ;;Make use of LIBRARY-SEARCH-PATH-FUNCTION.  Currently ignore version.
  ;;
  (let ((filename (string-join (map symbol->string ref.identifiers) "/")))
    (fold-left (lambda (knil dirname)
		 (fold-left (lambda (knil fileext)
			      (let ((pathname (string-append dirname "/" filename fileext)))
				(if (file-exists? pathname)
				    (cons pathname knil)
				  knil)))
			    '()
			    library-file-extensions))
	       '()
	       ((library-search-path-function)))))

(define scan-library-search-path-function
  (make-parameter scan-library-search-path
    (lambda (f)
      (assert (procedure? f))
      f)))


;;;; loading libraries

(define (load-libraries-from-files (ref <library-reference>))
  ;;Search  the  system  for   all  the  libraries  matching  the  given
  ;;reference.  Return a list of <library> records, possibly null.
  ;;
  ;;If loading a  library fails: the error is  ignored and that pathname
  ;;skipped.
  ;;
  (fold-left (lambda (knil pathname)
	       (with-compensations
		 (letrec ((port (compensate
				    (open-input-file pathname)
				  (with
				   (close-port port)))))
		   (guard (E (else knil))
		     (cons (make-<library> (read port))
			   knil)))))
	     '()
	     ((scan-library-search-path-function) ref)))

(define (sort-libraries libs)
  (list-sort (lambda ((a <library>) (b <library>))
	       (library-name<? a.name b.name))
	     libs))


;;;; library registry

(define-constant $library-registry
  (make-eq-hashtable))

(define load-library-function
  (make-parameter load-libraries-from-files
    (lambda (f)
      (assert (procedure? f))
      f)))

(define (load-library ref)
  (let (((reference <library-reference>) (if (is-a? ref <library-reference>)
					     ref
					   (make <library-reference> ref))))
    (let ((libs ((load-library-function) reference)))
      (if (null? libs)
	  (let ((lib (hashtable-ref $library-registry reference.identifiers #f)))
	    (if lib
		(list lib)
	      (raise-library-not-found 'load-library reference)))
	(begin0-let ((lib (car libs)))
	  (hashtable-set! $library-registry reference lib))))))


;;;; rnrs libraries

(define (%symbols->renamings list-of-symbols)
  (map (lambda (sym)
	 (list sym sym))
    list-of-symbols))

(define (%register-rnrs-lib library-name bindings)
  (let ((lib (make <library>
	       `(library ,library-name
		  (export ,@bindings)
		  (import)))))
    (hashtable-set! $library-registry library-name lib)
    (hashtable-set! $library-registry (drop-right library-name 1) lib)
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

(let ((lib (lambda (spec)
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
  (hashtable-set! $library-registry '(rnrs (6)) lib)
  (hashtable-set! $library-registry '(rnrs) lib))


;;;; done

)

;;; end of file
