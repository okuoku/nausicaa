;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa
;;;Contents: foreign library inspection generation
;;;Date: Wed Nov 25, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009-2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (inspector-maker)
  (export

    ;; Autoconf library
    autoconf-lib		autoconf-lib/no-newline
    autoconf-lib-write

    ;; sizeof library
    sizeof-lib			sizeof-lib/quote
    sizeof-lib-exports		sizeof-lib-exports/quote
    sizeof-lib-drop-exports	sizeof-lib-drop-exports/quote
    sizeof-renamed-exports
    sizeof-lib-write

    ;; main control
    define-shared-object
    clang-type-translation-lib-write

    ;; auxiliary functions
    class-uid-prefix

    ;; inspection
    define-c-defines		define-c-defines/public
    define-c-string-defines
    define-c-enumeration	define-c-type-alias
    define-c-type		define-c-struct

    ;; auxiliary syntaxes
    options
    label			no-label
    wrapper			no-wrapper
    mirror			no-mirror)
  (import (nausicaa)
    (nausicaa formations)
    (nausicaa times-and-dates)
    (nausicaa strings))


;;;; helpers

(define-auxiliary-syntaxes
  options
  label		no-label
  wrapper	no-wrapper
  mirror	no-mirror)

(define-inline (append! ?var ?ell)
  (set! ?var (append ?var ?ell)))

(define (hat->at str)
  ;;GNU  Autoconf wants  symbols enclosed  in "@...@"  for  its variable
  ;;substitutions, but  we cannot  use #\@ in  R6RS symbols.  So,  to be
  ;;able  to  use  S-expressions  when  composing  the  body  of  Scheme
  ;;libraries: we build  Scheme symbols with #\^ characters  in place of
  ;;#\@; we convert the S-expression  library body into a string; we use
  ;;this function to map #\^ to #\@; finally write the body to a file.
  ;;
  (string-map (lambda (idx ch)
		(if (char=? #\^ ch)
		    #\@
		  ch))
	      str))

(define (dot->underscore str)
  (string-map (lambda (idx ch)
		(if (char=? #\. ch)
		    #\_
		  ch))
	      str))

(define (dash->underscore str)
  (string-map (lambda (idx ch)
		(if (char=? #\- ch)
		    #\_
		  ch))
	      str))

(define (format-symbol . args)
  (string->symbol (apply format args)))

(define class-uid-prefix
  (make-parameter "nausicaa:default:"
    (lambda (uid)
      (cond ((string? uid)
	     uid)
	    ((symbol? uid)
	     (symbol->string uid))
	    (else
	     (assertion-violation 'class-uid-prefix
	       "expected string or symbol as class UID prefix" uid))))))


;;;; Autoconf library
;;
;;The definitions  in this code page  compose a library  of GNU Autoconf
;;macros used to inspect the data  types defined by a foreign C library;
;;the result  is a file that  should be included by  "aclocal.m4" in the
;;Autoconf infrastructure.
;;
;;The generated macro  file holds a single macro  whose name is selected
;;by the call to the AUTOCONF-LIB-WRITE function.
;;

(define-values ($autoconf-port $autoconf-getter)
  (open-string-output-port))

(define (autoconf-lib text-line)
  ;;Append TEXT-LINE to the Autoconf  library text; add a newline at the
  ;;end.
  ;;
  (autoconf-lib/no-newline text-line)
  (newline $autoconf-port))

(define (autoconf-lib/no-newline text-line)
  ;;Append  TEXT-LINE  to  the  Autoconf  library text  (do  NOT  add  a
  ;;newline).
  ;;
  (display text-line $autoconf-port))

(define (autoconf-lib-write filename libname macro-name)
  ;;Write  to  the  specified  FILENAME  the contents  of  the  Autoconf
  ;;library.  LIBNAME must  be the library specification and  it is used
  ;;only as a comment.
  ;;
  (let ((port (transcoded-port (open-file-output-port filename (file-options no-fail))
			       (make-transcoder (utf-8-codec)))))
    (format port "dnl ~a --\n" libname)
    (display $autoconf-license port)
    (display (string-append "\nAC_DEFUN([" (symbol->string/maybe macro-name) "],[\n\n") port)
    (display ($autoconf-getter) port)
    (display "\n\n])\n\n\
              dnl end of file\n\
              dnl Local Variables:\n\
              dnl mode: autoconf\n\
              dnl End:\n" port)
    (close-port port)))


;;;; sizeof library
;;
;;The definitions  in this code page  compose an R6RS  library with type
;;and constant definitions resulting from  the inspection of a foreign C
;;language  library;   the  library  is  built  upon   the  features  of
;;Nausicaa/Scheme, making use of its FFI (foreign functions interface).
;;
;;
;;

;;The S-expression representing the  generated library body, without the
;;struct section.
;;
(define $sizeof-library-body		'())

;;The  S-expression  representing  the  generated library  body,  struct
;;definitions section.
(define $structs-library-body		'())

;;A  list of  symbols  representing  bindings to  be  exported from  the
;;generated library, without the struct section.
;;
(define $sizeof-lib-exports		'())

;;A  list of  symbols  representing  bindings to  be  exported from  the
;;generated library for the struct section.
;;
(define $structs-lib-exports	'())

;;A list of symbols representing bindings that must NOT be exported from
;;the   generated  library.    It  is   used  to   exclude   symbols  in
;;$SIZEOF-LIB-EXPORTS.  It makes sense because some symbols are appended
;;to be  exported by  automatic procedures, and  we may want  to exclude
;;some of them by hand picking.
;;
(define $sizeof-lib-drop-exports	'())

;;A list  of lists, each of which  holding two symbols.  It  is added to
;;the  export  list  of  the   generated  library  as  a  RENAME  export
;;specification.
;;
(define $sizeof-lib-renamed-exports	'())

(define-syntax sizeof-lib/quote
  ;;Append S-expressions to the end of the current sizeof library.
  ;;
  (syntax-rules ()
    ((_ ?sexp0 ?sexp ...)
     (sizeof-lib (quote (?sexp0 ?sexp ...))))))

(define (sizeof-lib sexp)
  ;;Append an S-expression to the end of the current sizeof library.
  ;;
  (append! $sizeof-library-body sexp))

(define (%structs-lib sexp)
  ;;Append  an S-expression  to the  end  of the  library body,  structs
  ;;section.
  ;;
  (append! $structs-library-body sexp))

(define (%structs-lib-exports . ell)
  ;;Add  a list  of  symbols to  the  list of  exports  for the  structs
  ;;section.
  ;;
  (append! $structs-lib-exports ell))

(define-syntax sizeof-lib-exports/quote
  ;;Add a list of symbols to the list of exports in the sizeof library.
  ;;
  (syntax-rules ()
    ((_ ?symbol0 ?symbol ...)
     (%sizeof-lib-exports (quote ?symbol0) (quote ?symbol) ...))))

(define (sizeof-lib-exports . ell)
  ;;Add a  list of export specifications  to the list of  exports in the
  ;;sizeof library.
  ;;
  (append! $sizeof-lib-exports ell))

(define-syntax sizeof-lib-drop-exports/quote
  ;;Add a list of  symbols to the list of exports to  drop in the sizeof
  ;;library.
  ;;
  (syntax-rules ()
    ((_ ?symbol0 ?symbol ...)
     (%sizeof-lib-drop-exports (quote ?symbol0) (quote ?symbol) ...))))

(define (sizeof-lib-drop-exports . ell)
  ;;Add a list of  symbols to the list of exports to  drop in the sizeof
  ;;library.
  ;;
  (append! $sizeof-lib-drop-exports ell))

(define (sizeof-renamed-exports list-of-renamings)
  ;;Register a list of renamings for the export list.
  ;;
  (append! $sizeof-lib-renamed-exports list-of-renamings))

(define (sizeof-lib-write filename libname libname-type-translation)
  ;;Write to  the specified FILENAME  the contents of the  sizeof Scheme
  ;;library.
  ;;
  ;;LIBNAME must be the library specification.  LIBNAME-TYPE-TRANSLATION
  ;;must be the library specification for the type translation library.
  ;;
  (define (%filter-exports symbols-list)
    (filter (lambda (S)
	      (not (memq S $sizeof-lib-drop-exports)))
      symbols-list))
  (define lib-exports
    (begin0-let ((ell `( ;;
			c-sizeof c-strideof c-alignof c-valueof c-inspect
			pointer-c-ref pointer-c-set!
			pointer-c-accessor pointer-c-mutator
			array-c-ref array-c-set! array-c-pointer-to
			pointer: wrapper: mirror: malloc:
			,@(%filter-exports $sizeof-lib-exports)
			,@(%filter-exports $structs-lib-exports))))
      (unless (null? $sizeof-lib-renamed-exports)
	(append! ell `((rename ,@$sizeof-lib-renamed-exports))))))
  (define lib-imports
    `((nausicaa)
      (nausicaa language makers)
      (for (prefix (rename (only ,libname-type-translation
				 clang-maybe-foreign-type->clang-external-type
				 clang-maybe-foreign-type->clang-external-type*)
			   ;; return false when type not found
			   (clang-maybe-foreign-type->clang-external-type  translate-type)
			   ;; assertion violation when type not found
			   (clang-maybe-foreign-type->clang-external-type* translate-type*))
		   ffi.)
	   expand)
      (nausicaa posix clang embedded-accessors-and-mutators)
      (for (prefix (only (nausicaa ffi syntax-helpers) %prepend) ffi.) expand)
      (prefix (only (nausicaa ffi memory) memcpy) mem.)
      (prefix (nausicaa ffi pointers) ffi.)
      (prefix (nausicaa ffi sizeof) ffi.)
      (prefix (nausicaa ffi peekers-and-pokers) ffi.)))
  (define libout
    `(library ,libname
       (export ,@lib-exports)
       (import ,@lib-imports)

       (define-auxiliary-syntaxes
	 pointer: wrapper: mirror: malloc:)

       (define-syntax* (c-sizeof stx)
	 (syntax-case stx ()
	   ((_ ?type ?number-of-elements)
	    (identifier? #'?type)
	    #`(* ?number-of-elements
		 #,(ffi.%prepend #'?c-sizeof "strideof-" (ffi.translate-type* #'?type))))
	   ((_ ?type)
	    (identifier? #'?type)
	    (ffi.%prepend #'?c-sizeof "sizeof-" (ffi.translate-type* #'?type)))
	   (_
	    (synner "invalid C language sizeof specification"))))

       (define-syntax* (c-strideof stx)
	 (syntax-case stx ()
	   ((_ ?type)
	    (identifier? #'?type)
	    (ffi.%prepend #'c-strideof "strideof-" (ffi.translate-type* #'?type)))
	   (_
	    (synner "invalid C language strideof specification"))))

       (define-syntax* (c-alignof stx)
	 (syntax-case stx ()
	   ((_ ?type)
	    (identifier? #'?type)
	    (ffi.%prepend #'c-alignof "alignof-" (ffi.translate-type* #'?type)))
	   (_
	    (synner "invalid C language alignof specification"))))

       (define-syntax* (c-valueof stx)
	 (syntax-case stx ()
	   ((_ ?thing)
	    (identifier? #'?thing)
	    (ffi.%prepend #'c-valueof "valueof-" #'?thing))
	   (_
	    (synner "invalid C language valueof specification"))))

       (define-syntax* (c-inspect stx)
	 (syntax-case stx ()
	   ((_ ?thing)
	    (identifier? #'?thing)
	    (ffi.%prepend #'c-inspect "inspect-" #'?thing))
	   (_
	    (synner "invalid C language valueof specification"))))

       (define-syntax* (pointer-c-ref stx)
	 (syntax-case stx ()
	   ((_ ?type ?pointer ?offset)
	    #`(ffi.pointer-c-ref #,(let ((type (ffi.translate-type #'?type)))
				     (if type (datum->syntax #'?type type) #'?type))
				 ?pointer ?offset))
	   (_
	    (synner "invalid syntax for C language raw memory getter"))))

       (define-syntax* (pointer-c-set! stx)
	 (syntax-case stx ()
	   ((_ ?type ?pointer ?offset ?value)
	    #`(ffi.pointer-c-set! #,(let ((type (ffi.translate-type #'?type)))
				      (if type (datum->syntax #'?type type) #'?type))
				  ?pointer ?offset ?value))
	   (_
	    (synner "invalid syntax for C language raw memory setter"))))

       (define-syntax* (pointer-c-accessor stx)
	 (syntax-case stx ()
	   ((_ ?type)
	    #`(ffi.pointer-c-accessor #,(let ((type (ffi.translate-type #'?type)))
					  (if type (datum->syntax #'?type type) #'?type))))
	   (_
	    (synner "invalid syntax for C language raw memory getter"))))

       (define-syntax* (pointer-c-mutator stx)
	 (syntax-case stx ()
	   ((_ ?type)
	    #`(ffi.pointer-c-mutator #,(let ((type (ffi.translate-type #'?type)))
					 (if type (datum->syntax #'?type type) #'?type))))
	   (_
	    (synner "invalid syntax for C language raw memory setter"))))

       (define-inline (array-c-ref ?type ?pointer ?index)
	 (pointer-c-ref ?type ?pointer (* ?index (c-strideof ?type))))

       (define-inline (array-c-set! ?type ?pointer ?index ?value)
	 (pointer-c-set! ?type ?pointer (* ?index (c-strideof ?type)) ?value))

       (define-inline (array-c-pointer-to ?type ?pointer ?index)
	 (ffi.pointer-add ?pointer (* ?index (c-strideof ?type))))

       ,@$sizeof-library-body
       ,@$structs-library-body))

  (define strout
    (hat->at (call-with-string-output-port
		 (lambda (port)
		   (pretty-print libout port)))))
  (define port
    (transcoded-port (open-file-output-port filename (file-options no-fail))
		     (make-transcoder (utf-8-codec))))
  (format port ";;; ~s --\n" libname)
  (display $sizeof-license port)
  (display "\n\n" port)
  (display strout port)
  (display "\n\n;;; end of file\n" port)
  (close-port port))


;;;; C language type translation library

#| The definitions in this page are meant to produce the C language data
types  translation  library:  a  library exporting  bindings  needed  to
convert  foreign  library  specific   type  symbols  into  Nausicaa  FFI
normalised type symbols.

Usually for  extension XXX such  library is called: (nausicaa  XXX clang
type-translation).   It is  common to  let GNU  Autoconf  preprocess the
library   to  inser   Nausicaa   FFI  symbols   determined  at   package
configuration time.

As an example, it should be something like:

  (library (nausicaa XXX clang type-translation)
    (export clang-foreign-type->clang-external-type
	    clang-maybe-foreign-type->clang-external-type
	    clang-maybe-foreign-type->clang-external-type*
	    enum-clang-foreign-types clang-foreign-types)
    (import (rnrs)
      (prefix (nausicaa ffi clang type-translation) ffi.))
    (define-enumeration enum-clang-foreign-types
      (alpha_t beta_t gamma_t)
      clang-foreign-types)
    (define (clang-foreign-type->clang-external-type type)
      (case type
	((alpha_t)	'@TYPEOF_ALPHA_T@)
	((beta_t)	'@TYPEOF_BETA_T@)
	((gamma_t)	'pointer)
	(else #f)))
    (define (clang-maybe-foreign-type->clang-external-type type)
      (unless (symbol? type)
	(set! type (syntax->datum type)))
      (or (clang-foreign-type->clang-external-type type)
          (ffi.clang-foreign-type->clang-external-type type)))
    (define (clang-maybe-foreign-type->clang-external-type* type)
      (or (clang-maybe-foreign-type->clang-external-type type)
          (assertion-violation 'clang-maybe-foreign-type->clang-external-type*
	    "unknown type specifier" type))))

|#

(define $clang-type-translation-map	'())
(define $clang-type-translation-enum	'())

(define (%clang-type-translation-register-type-alias external-type internal-type)
  (set-cons! $clang-type-translation-map `((,external-type) (quote ,internal-type)))
  (set-cons! $clang-type-translation-enum external-type))

(define (clang-type-translation-lib-write filename clang-libname)
  ;;Write to  the specified  FILENAME the contents  of the  clang Scheme
  ;;library.  CLANG-LIBNAME must  be the library specification.
  ;;
  (define libout
    `(library ,clang-libname
       (export clang-foreign-type->clang-external-type
	       clang-maybe-foreign-type->clang-external-type
	       clang-maybe-foreign-type->clang-external-type*
	       enum-clang-foreign-types clang-foreign-types)
       (import (rnrs)
	 (prefix (nausicaa ffi clang type-translation) ffi.))
       (define-enumeration enum-clang-foreign-types
	 ,(reverse $clang-type-translation-enum)
	 clang-foreign-types)
       (define (clang-foreign-type->clang-external-type type)
	 (case type
	   ,@(reverse $clang-type-translation-map)
	   (else #f)))
       (define (clang-maybe-foreign-type->clang-external-type type)
	 (unless (symbol? type)
	   (set! type (syntax->datum type)))
	 (or (clang-foreign-type->clang-external-type type)
	     (ffi.clang-foreign-type->clang-external-type type)))
       (define (clang-maybe-foreign-type->clang-external-type* type)
	 (or (clang-maybe-foreign-type->clang-external-type type)
	     (assertion-violation 'clang-maybe-foreign-type->clang-external-type*
	       "unknown type specifier" type)))
       ))
  (define strout
    (hat->at (call-with-string-output-port
		 (lambda (port)
		   (pretty-print libout port)))))
  (define port
    (transcoded-port (open-file-output-port filename (file-options no-fail))
		     (make-transcoder (utf-8-codec))))
  (format port ";;; ~s --\n" clang-libname)
  (display $clang-license port)
  (display "\n#!r6rs\n" port)
  (display strout port)
  (display "\n\n;;; end of file\n" port)
  (close-port port))


;;;; shared library

(define-syntax define-shared-object
  (syntax-rules ()
    ((_ ?name ?default-library-name)
     (%define-shared-object (quote ?name) (quote ?default-library-name)))))

(define (%define-shared-object name default-library-name)
  (let* ((name		(symbol->string name))
	 (upname	(dash->underscore (string-upcase name)))
	 (dnname	(string-downcase name))
	 (tiname	(string-titlecase name))
	 (varname	(format "~a_SHARED_OBJECT" upname))
	 (varname-sym	(string->symbol varname)))
    (autoconf-lib (format "NAU_DS_WITH_OPTION([~a],[~a-shared-object],[~a],
  [~a shared library file],[select ~a shared library file])"
		    varname dnname default-library-name tiname tiname))
    (let ((at-symbol (format "^~a^" varname)))
      (sizeof-lib `((define ,varname-sym ,at-symbol)))
      (sizeof-lib-exports varname-sym))))


;;;; enumerations

(define-syntax define-c-enumeration
  ;;Register in  the output libraries  a set of enumerated  C constants.
  ;;Example:
  ;;
  ;;  (define-c-enumeration lib_type
  ;;    LIB_VALUE_ZERO LIB_VALUE_ONE LIB_VALUE_TWO)
  ;;
  (syntax-rules ()
    ((_ ?enum-name ?string-typedef ?symbol-name0 ?symbol-name ...)
     (%register-enumeration (quote ?enum-name) ?string-typedef
			    (quote (?symbol-name0 ?symbol-name ...))))))

(define (%register-enumeration enum-name string-typedef symbol-names)
  (%register-type enum-name 'signed-int string-typedef)
  (autoconf-lib (format "\ndnl enum ~a" enum-name))
  (for-each (lambda (symbol)
	      (let ((valueof-symbol (string->symbol (string-append "valueof-" (symbol->string symbol)))))
		(autoconf-lib (format "NAUSICAA_ENUM_VALUE([~a])" symbol))
		(let ((at-symbol (format-symbol "^VALUEOF_~a^" symbol)))
		  (sizeof-lib `((define ,valueof-symbol ,at-symbol))))))
    symbol-names))


;;;; C preprocessor symbols

(define-syntax define-c-defines
  ;;Register in the output libraries  a set of C preprocessor constants.
  ;;Example:
  ;;
  ;;  (define-c-defines "seek whence values"
  ;;    SEEK_SET SEEK_CUR SEEK_END)
  ;;
  (syntax-rules ()
    ((_ ?description ?symbol-name0 ?symbol-name ...)
     (%register-preprocessor-symbols ?description (quote (?symbol-name0 ?symbol-name ...))))))

(define (%register-preprocessor-symbols description symbol-names)
  (autoconf-lib (format "\ndnl Preprocessor symbols: ~a" description))
  (for-each (lambda (symbol)
	      (let ((valueof-symbol (string->symbol (string-append "valueof-" (symbol->string symbol)))))
		(autoconf-lib (format "NAUSICAA_DEFINE_VALUE([~a])" symbol))
		(let ((at-symbol (format-symbol "^VALUEOF_~a^" symbol)))
		  (sizeof-lib `((define ,valueof-symbol ,at-symbol))))))
    symbol-names))

(define-syntax define-c-defines/public
  ;;Register in the output libraries  a set of C preprocessor constants.
  ;;Example:
  ;;
  ;;  (define-c-defines/public "seek whence values"
  ;;    SEEK_SET SEEK_CUR SEEK_END)
  ;;
  (syntax-rules ()
    ((_ ?description ?symbol-name0 ?symbol-name ...)
     (%register-preprocessor-symbols/public ?description (quote (?symbol-name0 ?symbol-name ...))))))

(define (%register-preprocessor-symbols/public description symbol-names)
  (sizeof-renamed-exports (map (lambda (symbol)
				  (list (format-symbol "valueof-~a" symbol) symbol))
			     symbol-names))
  (%register-preprocessor-symbols description symbol-names))

;;; --------------------------------------------------------------------

(define-syntax define-c-string-defines
  ;;Register  in the  output libraries  a set  of C  preprocessor string
  ;;literals.
  ;;
  (syntax-rules ()
    ((_ ?description ?symbol-name0 ?symbol-name ...)
     (%register-string-preprocessor-symbols ?description (quote (?symbol-name0 ?symbol-name ...))))))

(define (%register-string-preprocessor-symbols description symbol-names)
  (autoconf-lib (format "\ndnl String preprocessor symbols: ~a" description))
  (for-each (lambda (symbol)
	      (let ((valueof-symbol (string->symbol (string-append "valueof-" (symbol->string symbol)))))
		(autoconf-lib (format "NAUSICAA_STRING_TEST([~a],[~a])" symbol symbol))
		(let ((at-symbol (format "^STRINGOF_~a^" symbol)))
		  (sizeof-lib `((define ,valueof-symbol ,at-symbol))))))
    symbol-names))


;;;; C type aliases

(define-syntax define-c-type-alias
  (syntax-rules ()
    ((_ ?alias ?type)
     (%register-type-alias (quote ?alias) (quote ?type)))))

(define (%register-type-alias alias type)
  (%clang-type-translation-register-type-alias alias type))


;;;; C type inspection

(define-syntax define-c-type
  (syntax-rules ()
    ((_ ?name ?type-category)
     (%register-type (quote ?name) (quote ?type-category) (symbol->string (quote ?name))))
    ((_ ?name ?type-category ?type-string)
     (%register-type (quote ?name) (quote ?type-category) ?type-string))))

(define (%register-type name type-category string-typedef)
  (let* ((keyword		(string-upcase (symbol->string name)))
	 (ac-symbol-typeof	(format-symbol "^TYPEOF_~a^"	keyword))
	 (string-typedef	(cond ((string=? "#t" string-typedef)	"\"#t\"")
				      ((string=? "#f" string-typedef)	"\"#f\"")
				      (else				string-typedef))))
    (autoconf-lib (format "NAUSICAA_SIZEOF_TEST([~a],[~a])" keyword string-typedef))
    (autoconf-lib (format "NAUSICAA_BASE_TYPE_TEST([~a],[~a],[~a])" keyword string-typedef type-category))
    (%clang-type-translation-register-type-alias name ac-symbol-typeof)))


;;;; C struct type inspection

(define-syntax* (define-c-struct stx)
  ;;Usage example:
  ;;
  ;;   (define-c-struct timeval
  ;;     "struct timeval"
  ;;     (signed-int    tv_sec	(name: sec))
  ;;     (signed-int    tv_usec	(name: sec)))
  ;;
  (define (main stx)
    (syntax-case stx (options)
      ((_ ?name ?type-string (options ?option ...) . ?field-specs)
       (let-values
	   (((label? wrapper? mirror?)
	     (%parse-struct-options #'(?option ...)))
	    ((field-type-categories C-field-names Scheme-field-names)
	     (%parse-struct-fields #'?field-specs)))
	 #`(%generate-struct-type #,label? #,wrapper? #,mirror? '?name ?type-string
				  '#,field-type-categories '#,C-field-names
				  '#,Scheme-field-names)))

      ((_ ?name ?type-string . ?field-specs)
       #'(define-c-struct ?name ?type-string (options label wrapper mirror) . ?field-specs))

      (_
       (synner "invalid struct definition"))))

  (define (%parse-struct-options stx)
    (let next-option ((stx	stx)
		      (label?	#t)
		      (wrapper?	#t)
		      (mirror?	#t))
      (syntax-case stx (struct-label label no-label wrapper no-wrapper mirror no-mirror)
	(()				(values label? wrapper? mirror?))

	((label ?option ...)		(next-option #'(?option ...) #t wrapper? mirror?))
	((no-label ?option ...)		(next-option #'(?option ...) #f wrapper? mirror?))

	((wrapper ?option ...)		(next-option #'(?option ...) label? #t mirror?))
	((no-wrapper ?option ...)	(next-option #'(?option ...) label? #f mirror?))

	((mirror ?option ...)		(next-option #'(?option ...) label? wrapper? #t))
	((no-mirror ?option ...)	(next-option #'(?option ...) label? wrapper? #f))

	(?subform
	 (synner "invalid options in struct definition" #'?subform)))))

  (define (%parse-struct-fields stx)
    (let next-field ((stx		stx)
		     (categories	'())
		     (C-fields		'())
		     (Scheme-fields	'()))
      (syntax-case stx ()
	(()
	 (values (reverse categories) (reverse C-fields) (reverse Scheme-fields)))
	(((?type-category ?C-field-name . ?field-options) ?spec ...)
	 (let-values (((Scheme-field-name) (%parse-field-options #'?C-field-name #'?field-options)))
	   (next-field #'(?spec ...)
		       (cons #'?type-category	categories)
		       (cons #'?C-field-name	C-fields)
		       (cons Scheme-field-name	Scheme-fields))))
	(?subform
	 (synner "invalid field specification in struct definition" #'?subform)))))

  (define (%parse-field-options C-field-name stx)
    (let ((Scheme-field-name C-field-name))
      (let next-option ((stx stx))
	(syntax-case stx (name:)
	  (()
	   (values Scheme-field-name))
	  (((name: ?Scheme-field-name) . ?options)
	   (begin
	     (set! Scheme-field-name #'?Scheme-field-name)
	     (next-option #'?options)))
	  ((?subform . ?options)
	   (synner "invalid field option in struct definition" #'?subform))
	  ))))

  (main stx))

(define (%generate-struct-type label? wrapper? mirror?
			       struct-name struct-string-typedef
			       field-type-categories C-field-names Scheme-field-names)
  ;;Build what is needed to handle a C struct type.
  ;;
  ;;LABEL?,  WRAPPER?  and  MIRROR?  are  boolean values:  true  when the
  ;;corresponding  interface to the  structure must  be included  in the
  ;;output.
  ;;
  ;;STRUCT-NAME is the bare name of the C structure as Scheme string.
  ;;
  ;;STRUCT-STRING-TYPEDEF  is the  Scheme string  to use  to define  a C
  ;;variable of the type.
  ;;
  ;;FIELD-TYPE-CATEGORIES is  a list of Scheme  symbols representing the
  ;;guessed  type of the  fields; to  be used  by GNU  Autoconf Nausicaa
  ;;inspection macros in the "configure.ac" template.
  ;;
  ;;C-FIELD-NAMES  is a  list of  Scheme symbols  representing  the bare
  ;;names  of the struct  fields: they  are used  by Autoconf  macros to
  ;;inspect the C struct.
  ;;
  ;;SCHEME-FIELD-NAMES is a list of Scheme symbols representing the bare
  ;;names of the struct fields: they are used by labels and classes.
  ;;
  ;;*NOTE*  There  is  a  lot  of code  duplication  in  this  function,
  ;;especially  the building  of symbols  to  output to  files; this  is
  ;;because  keeping the  code  readable was  a  priority.  Beware  when
  ;;changing things!!!
  ;;
  (define (main)
    (%generate-type-translation)
    (%generate-autoconf-macros)
    (%generate-constants)
    (when label?
      (%generate-label-interface-to-struct))
    (when wrapper?
      (%generate-class-wrapper-for-pointer))
    (when mirror?
      (%generate-mirror-class)))

  ;;Scheme  symbol to  be  used  as argument  for  C-SIZEOF and  similar
  ;;macros.
  (define struct-name-for-inspection
    (format-symbol "struct-~a" struct-name))

  (define struct-name-upcase
    (string-upcase (symbol->string struct-name)))

  ;;Scheme symbols representing Autoconf substitution symbols; they have
  ;;the "@" replaced by "^" because R6RS does not allow "@" in symbols.
  (define ac-symbol-sizeof
    (format-symbol "^SIZEOF_~a^" struct-name-upcase))
  (define ac-symbol-alignof
    (format-symbol "^ALIGNOF_~a^" struct-name-upcase))
  (define ac-symbol-strideof
    (format-symbol "^STRIDEOF_~a^" struct-name-upcase))

  ;;Scheme  symbols  used  as  identifier names  for  struct  inspection
  ;;bindings;  these are the  bindings used  internally by  the C-SIZEOF
  ;;macro and similar.
  (define sizeof-struct
    (format-symbol "sizeof-~a"	struct-name-for-inspection))
  (define alignof-struct
    (format-symbol "alignof-~a"	struct-name-for-inspection))
  (define strideof-struct
    (format-symbol "strideof-~a" struct-name-for-inspection))

  ;;Scheme  symbol  used as  identifier  name  of  the generated  struct
  ;;pointer interface label.
  (define label-name
    (format-symbol "<pointer-to-~a>" struct-name))

  ;;Scheme  symbol  used as  identifier  name  of  the generated  struct
  ;;pointer interface class wrapper.
  (define wrapper-name
    (format-symbol "<struct-~a>" struct-name))

  ;;Scheme symbol used as UID in the NONGENERATIVE clause of the wrapper
  ;;class definition.
  (define wrapper-uid
    (format-symbol "~a:~a" (class-uid-prefix) wrapper-name))

  ;;Scheme symbol used as identifier name of the generated struct mirror
  ;;class.
  (define mirror-name
    (format-symbol "<~a>" struct-name))

  ;;Scheme symbol used as UID  in the NONGENERATIVE clause of the mirror
  ;;class definition.
  (define mirror-uid
    (format-symbol "~a:~a" (class-uid-prefix) mirror-name))

  ;;Scheme  symbols used  as identifier  names for  conversion functions
  ;;between labels, wrapper classes and mirror classes.
  (define pointer->pointer	(format-symbol "~a-pointer->~a-pointer" struct-name struct-name))
  (define wrapper->pointer	(format-symbol "~a-wrapper->~a-pointer" struct-name struct-name))
  (define mirror->pointer	(format-symbol "~a-mirror->~a-pointer"  struct-name struct-name))
  (define pointer->wrapper	(format-symbol "~a-pointer->~a-wrapper" struct-name struct-name))
  (define wrapper->wrapper	(format-symbol "~a-wrapper->~a-wrapper" struct-name struct-name))
  (define mirror->wrapper	(format-symbol "~a-mirror->~a-wrapper"  struct-name struct-name))
  (define pointer->mirror	(format-symbol "~a-pointer->~a-mirror"  struct-name struct-name))
  (define wrapper->mirror	(format-symbol "~a-wrapper->~a-mirror"  struct-name struct-name))
  (define mirror->mirror	(format-symbol "~a-mirror->~a-mirror"   struct-name struct-name))

  ;;Lists of  symbols representing source and destination  fields in dot
  ;;notation.  They are used in the convertion functions between labels,
  ;;wrapper classes and mirror classes.
  (define src-fields
    (map (lambda (field-name)
	   (format-symbol "src.~a" field-name))
      Scheme-field-names))
  (define dst-fields
    (map (lambda (field-name)
	   (format-symbol "dst.~a" field-name))
      Scheme-field-names))
  (define src-fields->dst-fields
    (map (lambda (src dst)
	   `(set! ,dst ,src))
      src-fields
      dst-fields))

  (define (%generate-type-translation)
    ;;Every struct type  has a type alias for its name.   It is NOT used
    ;;for  callout  function arguments;  rather,  it  is  used for  type
    ;;inspection by C-SIZEOF etc.
    ;;
    (%register-type-alias struct-name-for-inspection struct-name-for-inspection)
    ;;Every struct  type has a  pointer type alias; for  example "struct
    ;;flock" is  associated to  "struct-flock*" as alias  for "pointer".
    ;;Such alias is meant to be used in callout functions definition.
    ;;
    (%register-type-alias (format-symbol "~a*"  struct-name-for-inspection) 'pointer))

  (define (%generate-autoconf-macros)
    (autoconf-lib (format "\ndnl Struct inspection: ~a" struct-name))
    (autoconf-lib (format "NAUSICAA_INSPECT_STRUCT_TYPE([~a],[~a],[\"#f\"])"
		    struct-name-upcase struct-string-typedef))
    (for-each
	(lambda (C-field-name field-type-category)
	  (define field-keyword
	    (dot->underscore (string-upcase (symbol->string C-field-name))))
	  (if (eq? 'embedded field-type-category)
	      (autoconf-lib (format "NAUSICAA_INSPECT_FIELD_TYPE_POINTER([~a_~a],[~a],[~a])"
			      struct-name-upcase field-keyword struct-string-typedef C-field-name))
	    (autoconf-lib (format "NAUSICAA_INSPECT_FIELD_TYPE([~a_~a],[~a],[~a],[~a])"
			    struct-name-upcase field-keyword
			    struct-string-typedef C-field-name field-type-category))))
      C-field-names field-type-categories))

  (define (%generate-constants)
    (sizeof-lib `((define ,sizeof-struct	,ac-symbol-sizeof)
		  (define ,alignof-struct	,ac-symbol-alignof)
		  (define ,strideof-struct	,ac-symbol-strideof))))

  (define (%generate-label-interface-to-struct)
    ;;A structure definition like:
    ;;
    ;; (define-c-struct timeval
    ;;   "struct timeval"
    ;;   (signed-int    tv_sec	sec)
    ;;   (signed-int    tv_usec	usec))
    ;;
    ;;should become:
    ;;
    ;; (define-label <pointer-to-timeval>
    ;;   (custom-maker <pointer-to-timeval>-maker)
    ;;   (virtual-fields (mutable sec)
    ;;                   (mutable usec)))
    ;;
    ;; (define-inline (<pointer-to-timeval>-sec-ref ?pointer)
    ;;   (pointer-c-ref @TYPEOF_TIMEVAL_TV_SEC@ ?pointer @OFFSETOF_TIMEVAL_TV_USEC@))
    ;;
    ;; (define-inline (<pointer-to-timeval>-sec-set! ?pointer ?value)
    ;;   (pointer-c-ref @TYPEOF_TIMEVAL_TV_SEC@ ?pointer @OFFSETOF_TIMEVAL_TV_SEC@ ?value))
    ;;
    ;; (define-inline (<pointer-to-timeval>-usec-ref ?pointer)
    ;;   (pointer-c-ref @TYPEOF_TIMEVAL_TV_USEC@ ?pointer @OFFSETOF_TIMEVAL_TV_USEC@))
    ;;
    ;; (define-inline (<pointer-to-timeval>-usec-set! ?pointer ?value)
    ;;   (pointer-c-ref @TYPEOF_TIMEVAL_TV_USEC@ ?pointer @OFFSETOF_TIMEVAL_TV_USEC@ ?value))
    ;;
    ;; (define-maker <pointer-to-timeval>-maker %<pointer-to-timeval>-maker
    ;;   ((pointer:     sentinel        (without wrapper: mirror:))
    ;;    (wrapper:     sentinel        (without pointer: mirror:))
    ;;    (mirror:      sentinel        (without pointer: wrapper:))
    ;;    (malloc:      sentinel        (mandatory))))
    ;;
    ;; (define-syntax %<pointer-to-timeval>-maker
    ;;   (syntax-rules (sentinel)
    ;;     ((_ sentinel sentinel sentinel ?malloc) (?malloc (c-sizeof ,struct-id)))
    ;;     ((_ ?pointer sentinel sentinel ?malloc) (timeval-pointer->timeval-pointer ?pointer ?malloc))
    ;;     ((_ sentinel ?wrapper sentinel ?malloc) (timeval-wrapper->timeval-pointer ?wrapper ?malloc))
    ;;     ((_ sentinel sentinel ?mirror  ?malloc) (timeval-mirror->timeval-pointer  ?mirror  ?malloc))))
    ;;
    ;; (define (timeval-pointer->timeval-pointer (src <pointer-to-timeval>) malloc)
    ;;   (let (((dst <pointer-to-timeval>) (malloc (c-sizeof struct-timeval))))
    ;;     (set! dst.sec  src.sec)
    ;;     (set! dst.usec src.usec)
    ;;     dst))
    ;;
    ;; (define (timeval-wrapper->timeval-pointer (src <struct-timeval>) malloc)
    ;;   (let (((dst <pointer-to-timeval>) (malloc (c-sizeof struct-timeval))))
    ;;     (set! dst.sec  src.sec)
    ;;     (set! dst.usec src.usec)
    ;;     dst))
    ;;
    ;; (define (timeval-mirror->timeval-pointer (src <timeval>) malloc)
    ;;   (let (((dst <pointer-to-timeval>) (malloc (c-sizeof struct-timeval))))
    ;;     (set! dst.sec  src.sec)
    ;;     (set! dst.usec src.usec)
    ;;     dst))
    ;;
    (let ((fields                       '())
          (accessors-and-mutators       '())
          (struct-keyword               (string-upcase (symbol->string struct-name))))
      (for-each
	  (lambda (C-field-name Scheme-field-name field-type-category)
	    (let* ((accessor	(format-symbol "~a-~a"      label-name Scheme-field-name))
		   (mutator	(format-symbol "~a-~a-set!" label-name Scheme-field-name))
		   (keyword	(dot->underscore (string-upcase (symbol->string C-field-name))))
		   (typeof	(format-symbol "^TYPEOF_~a_~a^"   struct-keyword keyword))
		   (offset	(format-symbol "^OFFSETOF_~a_~a^" struct-keyword keyword)))
	      (if (eq? 'embedded field-type-category)
		  (begin
		    (set-cons! fields `(mutable ,Scheme-field-name))
		    ;; (set-cons! accessors-and-mutators
		    ;; 	       `(define-inline (,accessor ?pointer)
		    ;; 		  (ffi:pointer-add ?pointer ,offset)))
		    (set-cons! accessors-and-mutators
		    	       `(define-inline (,accessor ?pointer)
		    		  (null-accessor ?pointer ,offset)))
		    (set-cons! accessors-and-mutators
		    	       `(define-inline (,mutator ?pointer ?value)
		    		  (null-mutator ?pointer ,offset ?value))))
		(begin
		  (set-cons! fields `(mutable ,Scheme-field-name))
		  (set-cons! accessors-and-mutators
			     `(define-inline (,accessor ?pointer)
				(pointer-c-ref ,typeof ?pointer ,offset)))
		  (set-cons! accessors-and-mutators
			     `(define-inline (,mutator ?pointer ?value)
				(pointer-c-set! ,typeof ?pointer ,offset ?value)))
		  ))))
	C-field-names Scheme-field-names field-type-categories)
      ;; register sexps to the sizeof library, structs section
      (%structs-lib-exports label-name)
      (let* ((maker-name	(format-symbol "~a-maker" label-name))
	     (constructor-name	(format-symbol "%~a"      maker-name))
	     (pointer->pointer	(format-symbol "~a-pointer->~a-pointer" struct-name struct-name))
	     (wrapper->pointer	(format-symbol "~a-wrapper->~a-pointer" struct-name struct-name))
	     (mirror->pointer	(format-symbol "~a-mirror->~a-pointer"  struct-name struct-name)))
	(%structs-lib
	 `((define-label ,label-name (custom-maker ,maker-name) (virtual-fields ,@fields))
	   ,@accessors-and-mutators
	   (define-maker ,maker-name ,constructor-name
	     ((pointer:	sentinel	(without wrapper: mirror:))
	      (wrapper:	sentinel	(without pointer: mirror:))
	      (mirror:	sentinel	(without pointer: wrapper:))
	      (malloc:	sentinel	(mandatory))))
	   (define-syntax ,constructor-name
	     (syntax-rules (sentinel)
	       ((_ sentinel sentinel sentinel ?malloc) (?malloc (c-sizeof ,struct-name-for-inspection)))
	       ((_ ?pointer sentinel sentinel ?malloc) (,pointer->pointer ?pointer ?malloc))
	       ((_ sentinel ?wrapper sentinel ?malloc) (,wrapper->pointer ?wrapper ?malloc))
	       ((_ sentinel sentinel ?mirror  ?malloc) (,mirror->pointer  ?mirror  ?malloc))))
	   (define (,pointer->pointer (src ,label-name) malloc)
	     (let (((dst ,label-name) (malloc (c-sizeof ,struct-name-for-inspection))))
	       (mem.memcpy dst src (c-sizeof ,struct-name-for-inspection))
	       dst))
	   (define (,wrapper->pointer (src ,wrapper-name) malloc)
	     (let (((dst ,label-name) (malloc (c-sizeof ,struct-name-for-inspection))))
	       (mem.memcpy dst src (c-sizeof ,struct-name-for-inspection))
	       dst))
	   (define (,mirror->pointer (src ,mirror-name) malloc)
	     (let (((dst ,label-name) (malloc (c-sizeof ,struct-name-for-inspection))))
	       ,@src-fields->dst-fields
	       dst))
	   )))
      ))

  (define (%generate-class-wrapper-for-pointer)
    ;;A structure definition like:
    ;;
    ;; (define-c-struct timeval
    ;;   "struct timeval"
    ;;   (signed-int	tv_sec)
    ;;   (signed-int	tv_usec))
    ;;
    ;;should become:
    ;;
    ;; (define-class <struct-timeval>
    ;;   (nongenerative nausicaa:posix:<struct-timeval>)
    ;;   (fields (immutable pointer))
    ;;   (virtual-fields (mutable tv_sec)
    ;;                   (mutable tv_usec)))
    ;;
    ;; (define-inline (<struct-timeval>-tv_sec-ref ?wrapper)
    ;;   (<pointer-to-timeval>-tv_sec (<struct-timeval>-pointer ?wrapper)))
    ;;
    ;; (define-inline (<struct-timeval>-tv_sec-set! ?wrapper ?value)
    ;;   (<pointer-to-timeval>-tv-sec-set! (<struct-timeval>-pointer ?wrapper) ?value))
    ;;
    ;; (define-inline (<struct-timeval>-tv_usec-ref ?wrapper)
    ;;   (<pointer-to-timeval>-tv_usec (<struct-timeval>-pointer ?wrapper)))
    ;;
    ;; (define-inline (<struct-timeval>-tv_usec-set! ?wrapper ?value)
    ;;   (<pointer-to-timeval>-tv_usec-set! (<struct-timeval>-pointer ?wrapper) ?value))
    ;;
    (let* ((fields			'())
	   (accessors-and-mutators	'())
	   (struct-keyword		(string-upcase (symbol->string struct-name))))
      (for-each
	  (lambda (C-field-name Scheme-field-name field-type-category)
	    (let* ((accessor	(format-symbol "~a-~a"      wrapper-name Scheme-field-name))
		   (mutator	(format-symbol "~a-~a-set!" wrapper-name Scheme-field-name))
		   (L-accessor	(format-symbol "~a-~a"      label-name Scheme-field-name))
		   (L-mutator	(format-symbol "~a-~a-set!" label-name Scheme-field-name))
		   (pointer	(format-symbol "~a-pointer" wrapper-name))
		   (keyword	(dot->underscore (string-upcase (symbol->string C-field-name))))
		   (typeof	(format-symbol "^TYPEOF_~a_~a^"   struct-keyword keyword))
		   (offset	(format-symbol "^OFFSETOF_~a_~a^" struct-keyword keyword)))
	      (if (eq? 'embedded field-type-category)
		  (begin
		    (set-cons! fields `(immutable ,Scheme-field-name))
		    (set-cons! accessors-and-mutators
			       `(define-inline (,accessor ?wrapper)
				  (,L-accessor (,pointer ?wrapper)))))
		(begin
		  (set-cons! fields `(mutable ,Scheme-field-name))
		  (set-cons! accessors-and-mutators
			     `(define-inline (,accessor ?wrapper)
				(,L-accessor (,pointer ?wrapper))))
		  (set-cons! accessors-and-mutators
			     `(define-inline (,mutator ?wrapper ?value)
				(,L-mutator (,pointer ?wrapper) ?value)))
		  ))))
	C-field-names Scheme-field-names field-type-categories)
      ;;register sexps to the sizeof library, structs section
      (%structs-lib-exports wrapper-name)
      (let ((maker-name (format-symbol "~a-maker-transformer" wrapper-name)))
	(%structs-lib
	 `((define-class ,wrapper-name
	     (nongenerative ,wrapper-uid)
	     (maker ()
		    (pointer:	sentinel	(without wrapper: mirror:))
		    (wrapper:	sentinel	(without pointer: mirror:))
		    (mirror:	sentinel	(without pointer: wrapper:))
		    (malloc:	sentinel	(mandatory)))
	     (maker-transformer ,maker-name)
	     (fields (immutable pointer))
	     (virtual-fields ,@fields))
	   ,@accessors-and-mutators
	   (define-syntax ,maker-name
	     (syntax-rules (sentinel)
	       ((_ ?constructor sentinel sentinel sentinel ?malloc)
		(?constructor (?malloc (c-sizeof ,struct-name-for-inspection))))
	       ((_ ?constructor ?pointer sentinel sentinel ?malloc)
		(,pointer->wrapper ?constructor ?pointer ?malloc))
	       ((_ ?constructor sentinel ?wrapper sentinel ?malloc)
		(,wrapper->wrapper ?constructor ?wrapper ?malloc))
	       ((_ ?constructor sentinel sentinel ?mirror  ?malloc)
		(,mirror->wrapper  ?constructor ?mirror  ?malloc))))
	   (define (,pointer->wrapper constructor pointer malloc)
	     (constructor (,pointer->pointer pointer malloc)))
 	   (define (,wrapper->wrapper constructor wrapper malloc)
	     (constructor (,wrapper->pointer wrapper malloc)))
	   (define (,mirror->wrapper  constructor mirror  malloc)
	     (constructor (,mirror->pointer  mirror  malloc)))
	   )))
      ))

  (define (%generate-mirror-class)
    ;;A structure definition like:
    ;;
    ;; (define-c-struct timeval
    ;;   "struct timeval"
    ;;   (signed-int	tv_sec)
    ;;   (signed-int	tv_usec))
    ;;
    ;;should become:
    ;;
    ;; (define-class <timeval>
    ;;   (nongenerative nausicaa:posix:<timeval>)
    ;;   (fields (immutable tv_sec)
    ;;           (immutable tv_usec)))
    ;;
    (%structs-lib-exports mirror-name)
    (let ((maker-name (format-symbol "~a-maker-transformer" mirror-name)))
      (%structs-lib
       `((define-class ,mirror-name
	   (nongenerative ,mirror-uid)
	   (maker ()
		  (pointer:	sentinel	(without wrapper: mirror:))
		  (wrapper:	sentinel	(without pointer: mirror:))
		  (mirror:	sentinel	(without pointer: wrapper:)))
	   (maker-transformer ,maker-name)
	   (fields ,@(map (lambda (name)
			    `(mutable ,name))
		       Scheme-field-names)))
	 (define-syntax ,maker-name
	   (syntax-rules (sentinel)
	     ((_ ?constructor ?pointer sentinel sentinel)
	      (,pointer->mirror ?constructor ?pointer))
	     ((_ ?constructor sentinel ?wrapper sentinel)
	      (,wrapper->mirror ?constructor ?wrapper))
	     ((_ ?constructor sentinel sentinel ?mirror)
	      (,mirror->mirror  ?constructor ?mirror))))
	 (define (,pointer->mirror constructor (src ,label-name))
	   (constructor ,@src-fields))
	 (define (,wrapper->mirror constructor (src ,wrapper-name))
	   (constructor ,@src-fields))
	 (define (,mirror->mirror constructor (src ,mirror-name))
	   (constructor ,@src-fields))
	 ))
      ))

  (main))


;;;; license notices

;; (define $date
;;   (current-date))

(define $sizeof-license
  (string-append ";;;
;;;Part of: Nausicaa
;;;Contents: foreign library inspection generation
;;;Date: " #;(date->string $date "~a ~b ~e, ~Y") "
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) " #;(date->string $date "~Y") " Marco Maggi <marco.maggi-ipsu@poste.it>
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
"))

(define $clang-license
  (string-append ";;;
;;;Part of: Nausicaa
;;;Contents: foreign library C language type mapping
;;;Date: " #;(date->string $date "~a ~b ~e, ~Y") "
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) " #;(date->string $date "~Y") " Marco Maggi <marco.maggi-ipsu@poste.it>
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
"))

(define $autoconf-license
  (string-append "dnl
dnl Part of: Nausicaa
dnl Contents: foreign library inspection Autoconf macros
dnl Date: " #;(date->string $date "~a ~b ~e, ~Y") "
dnl
dnl Abstract
dnl
dnl	This file has been automatically generated by the inspector
dnl	maker Nausicaa library; it is meant to be included by the
dnl	\"aclocal.m4\" file in the Autoconf infrastructure.
dnl
dnl	  This file defines an Autoconf macro which expands in the set
dnl	of Autoconf tests needed to inspect a foreign C language library.
dnl
dnl Copyright (c) " #;(date->string $date "~Y") " Marco Maggi <marco.maggi-ipsu@poste.it>
dnl
dnl This program is free software:  you can redistribute it and/or modify
dnl it under the terms of the  GNU General Public License as published by
dnl the Free Software Foundation, either version 3 of the License, or (at
dnl your option) any later version.
dnl
dnl This program is  distributed in the hope that it  will be useful, but
dnl WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
dnl MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
dnl General Public License for more details.
dnl
dnl You should  have received  a copy of  the GNU General  Public License
dnl along with this program.  If not, see <http://www.gnu.org/licenses/>.
dnl
"))


;;;; done

)

;;; end of file
