;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: R6RS library form inspection
;;;Date: Tue Apr  6, 2010
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


(library (libraries)
  (export

    <library>					<library>-with-record-fields-of
    make-<library>				<library>?
    <library>-spec
    <library>-sexp

    ;; conditions
    &library-not-found
    make-library-not-found-condition
    library-not-found-condition?
    condition-library-not-found-spec
    error-library-not-found

    &library-invalid-exports
    make-library-invalid-exports-condition
    library-invalid-exports-condition?
    condition-library-invalid-exports-spec
    error-invalid-exports

    ;;search path
    library-search-path-environment-variable
    get-search-path
    get-search-path-function

    ;;loading
    load-library				load-library-function
    load-library-from-file
    ignore-library-version

    ;;matching

    )
  (import (nausicaa)
    (sentinel)
    (compensations)
    (lists)
    (strings)
    (char-sets)
    (sexps))


(define-class <library>
  (nongenerative nausicaa:libraries:<library>)
  (fields (immutable spec)
		;the library specification as defined by R6RS
	  (immutable sexp)
		;the full S-expression of the library

	  (mutable raw-parsed?)
		;true if %LIBRARY-RAW-PARSE  has already been applied to
		;this record
	  (mutable _raw-exports)
		;the   content  of  the   EXPORT  clause,   produced  by
		;%LIBRARY-RAW-PARSE
	  (mutable _raw-imports)
		;the   content  of  the   IMPORT  clause,   produced  by
		;%LIBRARY-RAW-PARSE
	  (mutable _raw-body)
		;the body of the library, produced by %LIBRARY-RAW-PARSE
	  (mutable _exports)
		;list pairs representing the  export list, each pair as:
		;(defined-name . exported-name)
	  )
  (virtual-fields (immutable raw-exports)
		  (immutable raw-imports)
		  (immutable raw-body)
		  (immutable exports))

  (protocol (lambda (make-<top>)
	      (lambda/with* ((spec <list>) (sexp <list>))
		((make-<top>) spec sexp
		 #f	  ;raw-parsed?
		 #f #f #f ;_raw-exports _raw-imports _raw-body
		 #f	  ;_exports
		 )))))


(define-condition-type &library-not-found
  &error
  make-library-not-found-condition
  library-not-found-condition?
  (spec		condition-library-not-found-spec))

(define-inline (error-library-not-found ?who ?library-spec)
  (raise (condition
	  (make-who-condition ?who)
	  (make-message-condition "library not found")
	  (make-library-not-found-condition ?library-spec))))

;;; --------------------------------------------------------------------

(define-condition-type &library-invalid-exports
  &error
  make-library-invalid-exports-condition
  library-invalid-exports-condition?
  (export-spec	condition-library-invalid-exports-spec))

(define-inline (error-invalid-exports ?who ?export-spec)
  (raise (condition
	  (make-who-condition ?who)
	  (make-message-condition "invalid library exports specification")
	  (make-library-invalid-exports-condition ?export-spec))))


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


;;;; library loading

(define ignore-library-version
  (make-parameter #t))

(define $extensions
  (list (cond-expand
	 (ikarus	".ikarus.sls")
	 (larceny	".larceny.sls")
	 (mosh		".mosh.sls")
	 (petite	".petite.sls")
	 (ypsilon	".ypsilon.sls"))
	".sls" ".scm" ".ss"))

(define (load-library-from-file spec)
  ;;Search a library  on the system and load it.  Return  the sexp or #f
  ;;if not found.
  ;;
  (let ((pathname (get-library-pathname spec)))
    (if pathname
	(with-compensations
	  (letrec ((port (compensate
			     (open-input-file pathname)
			   (with
			    (close-port port)))))
	    (read port)))
      #f)))

(define (get-library-pathname spec)
  ;;Given a library specification, find  the pathname of its file on the
  ;;system.  Return the pathname as a string or #f if not found.
  ;;
  ;;SPEC  must  be  a  library  specification as  defined  in  the  R6RS
  ;;document.   The version  specification,  if present,  is taken  into
  ;;account  depending  on   the  value  of  the  IGNORE-LIBRARY-VERSION
  ;;parameter: if #f it is removed before building the pathname.
  ;;
  (let* ((spec	(if (ignore-library-version)
		    (if (pair? (last spec))
			(drop-right spec 1)
		      spec)
		  spec))
	 (name	(string-join (map symbol->string spec) "/"))
	 (dirs	((get-search-path-function))))
    (exists (lambda (dir)
	      (exists (lambda (ext)
			(let ((pathname (string-append dir "/" name ext)))
			  (if (file-exists? pathname)
			      pathname
			    #f)))
		      $extensions))
	    dirs)))


;;;; basic matching

(define (<library>-raw-exports (lib <library>))
  (%library-raw-parse lib)
  (<library>-_raw-exports lib))

(define (<library>-raw-imports (lib <library>))
  (%library-raw-parse lib)
  (<library>-_raw-imports lib))

(define (<library>-raw-body (lib <library>))
  (%library-raw-parse lib)
  (<library>-_raw-body lib))

(define (%library-raw-parse (lib <library>))
  (unless lib.raw-parsed?
    (let-sexp-variables ((?spec		sentinel)
			 (?exports	sentinel)
			 (?imports	sentinel)
			 (?body		sentinel))
      (let* ((match (sexp-match `(library ,(sexp-var ?spec)
				   (export ,(sexp-var-rest ?exports))
				   (import ,(sexp-var-rest ?imports))
				   ,(sexp-var-rest ?body))
				lib.sexp))
	     (exports (assq ?exports match))
	     (imports (assq ?imports match))
	     (body    (assq ?body    match)))
	(when exports (set! lib._raw-exports (cdr exports)))
	(when imports (set! lib._raw-imports (cdr imports)))
	(when body    (set! lib._raw-body    (cdr body)))
	(set! lib.raw-parsed? #t)))))


;;;; export lists

(define (<library>-exports (lib <library>))
  (reverse
   (fold-left (lambda (knil spec)
		(cond ((symbol? spec)
		       `((,spec ,spec) . ,knil))
		      ((and (pair? spec)
			    (sexp-match? `(rename ,(sexp-one `(,(sexp-pred symbol?)
							       ,(sexp-pred symbol?))))
					 spec))
		       (append (reverse (cdr spec)) knil))
		      (else
		       (error-invalid-exports '<library>-exports spec))))
	      '()
	      lib.raw-exports)))



;;;; done

)

;;; end of file
