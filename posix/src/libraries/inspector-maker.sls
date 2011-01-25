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

    ;; main control
    sizeof-lib			sizeof-lib-write
    sizeof-lib-exports		sizeof-lib-drop-exports
    autoconf-lib		autoconf-lib-write
    define-shared-object
    clang-type-translation-lib-write

    class-uid

    ;; inspection
    define-c-defines		define-c-string-defines
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

(define (hat->at str)
  ;;GNU Autoconf  wants symbols enclosed  in "@...@", but we  cannot use
  ;;#\@  in R6RS symbols.   So, to  be able  to use  quasiquotation when
  ;;composing the body of Scheme libraries: we build Scheme symbols with
  ;;#\^  characters in place  of #\@;  convert the  S-expression library
  ;;body into  a string; use  this function to  map #\^ to  #\@; finally
  ;;write the body to a file.
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

(define class-uid
  (make-parameter "nausicaa:default:"
    (lambda (uid)
      (cond ((string? uid)
	     uid)
	    ((symbol? uid)
	     (symbol->string uid))
	    (else
	     (assertion-violation 'class-uid "expected string or symbol as class UID prefix" uid))))))


;;;; Autoconf library

(define $autoconf-library	"")

(define (autoconf-lib str)
  ;;Append STR  to the current Autoconf  library; add a new  line at the
  ;;end.
  ;;
  (set! $autoconf-library (string-append $autoconf-library str "\n")))

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
    (display $autoconf-library port)
    (display "\n\n])\n\n\
              dnl end of file\n\
              dnl Local Variables:\n\
              dnl mode: autoconf\n\
              dnl End:\n" port)
    (close-port port)))


;;;; sizeof library

(define $sizeof-library			'())
(define $sizeof-lib-exports		'())
(define $sizeof-lib-drop-exports	'())

(define-syntax sizeof-lib
  (syntax-rules ()
    ((_ ?sexp0 ?sexp ...)
     (%sizeof-lib (quote (?sexp0 ?sexp ...))))))

(define (%sizeof-lib sexp)
  ;;Append an S-expression to the end of the current sizeof library.
  ;;
  (set! $sizeof-library (append $sizeof-library sexp)))

(define-syntax sizeof-lib-exports
  (syntax-rules ()
    ((_ ?symbol0 ?symbol ...)
     (%sizeof-lib-exports (quote ?symbol0) (quote ?symbol) ...))))

(define (%sizeof-lib-exports . ell)
  ;;Add a list of symbols to the list of exports in the sizeof library.
  ;;
  (set! $sizeof-lib-exports (append $sizeof-lib-exports ell)))

(define-syntax sizeof-lib-drop-exports
  (syntax-rules ()
    ((_ ?symbol0 ?symbol ...)
     (%sizeof-lib-drop-exports (quote ?symbol0) (quote ?symbol) ...))))

(define (%sizeof-lib-drop-exports . ell)
  ;;Add a list of  symbols to the list of exports to  drop in the sizeof
  ;;library.
  ;;
  (set! $sizeof-lib-drop-exports (append $sizeof-lib-drop-exports ell)))

(define (sizeof-lib-write filename libname libname-clang-types)
  ;;Write to  the specified FILENAME  the contents of the  sizeof Scheme
  ;;library.     LIBNAME   must    be    the   library    specification.
  ;;LIBNAME-CLANG-TYPES must be the clang library specification.
  ;;
  (let ((libout `(library ,libname
		   (export
		     c-sizeof c-strideof c-alignof c-valueof c-inspect
		     pointer-c-ref pointer-c-set! pointer-c-accessor pointer-c-mutator
		     array-c-ref array-c-set! array-c-pointer-to
		     ,@(filter (lambda (S)
				 (not (memq S $sizeof-lib-drop-exports)))
			 $sizeof-lib-exports)
		     ,@(filter (lambda (S)
				 (not (memq S $sizeof-lib-drop-exports)))
			 $structs-lib-exports))
		   (import (nausicaa)
		     (for ,libname-clang-types expand)
		     (nausicaa ffi syntax-helpers)
		     ;;Notice that the "ffi:" prefix is hardcoded in the
		     ;;library (nausicaa ffi extension-utilities).
		     (prefix (nausicaa ffi pointers)		ffi:)
		     (prefix (nausicaa ffi sizeof)		ffi:)
		     (prefix (nausicaa ffi peekers-and-pokers)	ffi:)
		     (nausicaa ffi extension-utilities))
		   (define-sizeof-macros)
		   ,@$sizeof-library
		   ,@$structs-library)))
    (let ((strout (call-with-string-output-port
		      (lambda (port)
			(pretty-print libout port)))))
      (set! strout (hat->at strout))
      (let ((port (transcoded-port (open-file-output-port filename (file-options no-fail))
       				   (make-transcoder (utf-8-codec)))))
	(format port ";;; ~s --\n" libname)
	(display $sizeof-license port)
	(display "\n\n" port)
	(display strout port)
	(display "\n\n;;; end of file\n" port)
	(close-port port)))))


;;;; sizeof library, structs section

(define $structs-library	'())
(define $structs-lib-exports	'())

(define (%structs-lib sexp)
  ;;Append an S-expression to the end of the current structs library.
  ;;
  (set! $structs-library (append $structs-library sexp)))

(define (%structs-lib-exports . ell)
  ;;Add a list of symbols to the list of exports in the structs library.
  ;;
  (set! $structs-lib-exports (append $structs-lib-exports ell)))


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
    (define (clang-maybe-foreign-type->clang-external-type
	     type)
      (or (clang-foreign-type->clang-external-type type)
          (ffi.clang-foreign-type->clang-external-type type))))

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
  (let* ((libout `(library ,clang-libname
		    (export clang-foreign-type->clang-external-type
			    clang-maybe-foreign-type->clang-external-type
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
		      (or (clang-foreign-type->clang-external-type type)
			  (ffi.clang-foreign-type->clang-external-type type)))
		    ))
	 (strout (hat->at (call-with-string-output-port
			      (lambda (port)
				(pretty-print libout port)))))
	 (port (transcoded-port (open-file-output-port filename (file-options no-fail))
				(make-transcoder (utf-8-codec)))))
    (format port ";;; ~s --\n" clang-libname)
    (display $clang-license port)
    (display "\n#!r6rs\n" port)
    (display strout port)
    (display "\n\n;;; end of file\n" port)
    (close-port port)))


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
    ;;FIXME This will fail with Ikarus up to revision 1870.  Use Mosh!.
    (let ((at-symbol (format "^~a^" varname)))
      (%sizeof-lib `((define ,varname-sym ,at-symbol)))
      (%sizeof-lib-exports varname-sym))))


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
		  (%sizeof-lib `((define ,valueof-symbol ,at-symbol)))
		  ;;(%sizeof-lib-exports symbol)
		  )))
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
		  (%sizeof-lib `((define ,valueof-symbol ,at-symbol)))
		  ;;(%sizeof-lib-exports symbol)
		  )))
    symbol-names))

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
		  (%sizeof-lib `((define ,valueof-symbol ,at-symbol)))
		  ;;(%sizeof-lib-exports symbol)
		  )))
    symbol-names))


;;;; C type aliases

(define-syntax define-c-type-alias
  (syntax-rules ()
    ((_ ?alias ?type)
     (%register-type-alias (quote ?alias) (quote ?type)))))

(define (%register-type-alias alias type)
  (%clang-type-translation-register-type-alias alias type)
;;;  (%sizeof-lib `((define ,alias (quote ,type))))
;;;  (%sizeof-lib-exports alias)
  )


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
;;;	 (ac-symbol-sizeof	(format-symbol "^SIZEOF_~a^"	keyword))
;;;	 (ac-symbol-alignof	(format-symbol "^ALIGNOF_~a^"	keyword))
;;;	 (ac-symbol-strideof	(format-symbol "^STRIDEOF_~a^"	keyword))
;;;	 (name-typeof		name)
;;;	 (name-sizeof		(format-symbol "sizeof-~s"	name))
;;;	 (name-alignof		(format-symbol "alignof-~s"	name))
;;;	 (name-strideof		(format-symbol "strideof-~s"	name))
	 (string-typedef	(cond ((string=? "#t" string-typedef)
				       "\"#t\"")
				      ((string=? "#f" string-typedef)
				       "\"#f\"")
				      (else
				       string-typedef))))
    (autoconf-lib (format "NAUSICAA_SIZEOF_TEST([~a],[~a])"
		    keyword string-typedef))
    (autoconf-lib (format "NAUSICAA_BASE_TYPE_TEST([~a],[~a],[~a])"
		    keyword string-typedef type-category))
;;; (autoconf-lib (format "NAUSICAA_INSPECT_TYPE([~a],[~a],[~a],[#f])"
;;;                       keyword string-typedef type-category))
;;; (%sizeof-lib `((define ,name-typeof		(quote ,ac-symbol-typeof))
;;; 		   (define ,name-sizeof		,ac-symbol-sizeof)
;;; 		   (define ,name-alignof	,ac-symbol-alignof)
;;; 		   (define ,name-strideof	,ac-symbol-strideof)))
;;; (%sizeof-lib-exports name-typeof name-sizeof name-alignof name-strideof)
    (%clang-type-translation-register-type-alias name ac-symbol-typeof)))


;;;; C struct type inspection

(define-syntax* (define-c-struct stx)
  ;;Usage example:
  ;;
  ;;   (define-c-struct timeval
  ;;     "struct timeval"
  ;;     (signed-int    tv_sec)
  ;;     (signed-int    tv_usec))
  ;;
  (define (main stx)
    (syntax-case stx (options)
      ((_ ?name ?type-string (options ?option ...) . ?field-specs)
       (let-values (((label? wrapper? mirror?)		(%parse-struct-options #'(?option ...)))
		    ((field-type-categories
		      field-names mirror-field-names)	(%parse-struct-fields #'?field-specs)))
	 #`(%generate-struct-type #,label? #,wrapper? #,mirror? '?name ?type-string
				  '#,field-type-categories '#,field-names '#,mirror-field-names)))

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
		     (fields		'())
		     (mirror-fields	'()))
      (syntax-case stx ()
	(()
	 (values (reverse categories) (reverse fields) (reverse mirror-fields)))
	(((?type-category ?field-name ?mirror-field-name) ?spec ...)
	 (next-field #'(?spec ...)
		     (cons #'?type-category	categories)
		     (cons #'?field-name	fields)
		     (cons #'?mirror-field-name	mirror-fields)))
	(((?type-category ?field-name) ?spec ...)
	 (next-field #'(?spec ...)
		     (cons #'?type-category	categories)
		     (cons #'?field-name	fields)
		     (cons #'?field-name	mirror-fields)))
	(?subform
	 (synner "invalid field specification in struct definition" #'?subform)))))

  (main stx))

(define (%generate-struct-type label? wrapper? mirror?
			       struct-name struct-string-typedef
			       field-type-categories field-names mirror-field-names)
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
  ;;FIELD-NAMES is a list of  Scheme symbols representing the bare names
  ;;of the  struct fields: they  are used by  the label and  the pointer
  ;;wrapper class.
  ;;
  ;;MIRROR-FIELD-NAMES is a list of Scheme symbols representing the bare
  ;;names of the struct fields: they are used by the mirror class.
  ;;
  ;;*NOTE*  There  is  a  lot  of code  duplication  in  this  function,
  ;;especially the symbols  to output to files; this  is because keeping
  ;;the code readable was a priority.  Beware when changing things!!!
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

  (define (%generate-type-translation)
    (let* ((str	(format "struct-~a" struct-name))
	   (sym	(string->symbol str)))
      ;;Every struct type has a type alias for its name.  It is NOT used
      ;;for  callout function  arguments; rather,  it is  used  for type
      ;;inspection by C-SIZEOF etc.
      ;;
      (%register-type-alias sym sym)
      ;;Every struct type has a  pointer type alias; for example "struct
      ;;flock" is associated to  "struct-flock*" as alias for "pointer".
      ;;Such alias is meant to be used in callout functions definition.
      ;;
      (%register-type-alias (format-symbol "~a*" str) 'pointer)))

  (define (%generate-autoconf-macros)
    (autoconf-lib (format "\ndnl Struct inspection: ~a" struct-name))
    (let* ((struct-keyword	(string-upcase (symbol->string struct-name)))
	   (ac-symbol-sizeof	(format-symbol "^SIZEOF_~a^"	struct-keyword))
	   (ac-symbol-alignof	(format-symbol "^ALIGNOF_~a^"	struct-keyword))
	   (ac-symbol-strideof	(format-symbol "^STRIDEOF_~a^"	struct-keyword)))
      (autoconf-lib (format "NAUSICAA_INSPECT_STRUCT_TYPE([~a],[~a],[\"#f\"])"
		      struct-keyword struct-string-typedef))
      (for-each
	  (lambda (field-name field-type-category)
	    (let ((field-keyword (dot->underscore (string-upcase (symbol->string field-name)))))
	      (if (eq? 'embedded field-type-category)
		  (autoconf-lib (format "NAUSICAA_INSPECT_FIELD_TYPE_POINTER([~a_~a],[~a],[~a])"
				  struct-keyword field-keyword struct-string-typedef field-name))
		(autoconf-lib (format "NAUSICAA_INSPECT_FIELD_TYPE([~a_~a],[~a],[~a],[~a])"
				struct-keyword field-keyword
				struct-string-typedef field-name field-type-category)))))
	field-names field-type-categories)))

  (define (%generate-constants)
    (let* ((name	(format "struct-~a" struct-name))
	   (sizeof	(format-symbol "sizeof-~a"	name))
	   (alignof	(format-symbol "alignof-~a"	name))
	   (strideof	(format-symbol "strideof-~a"	name))
	   (keyword	(string-upcase (symbol->string struct-name)))
	   (ac-sizeof	(format-symbol "^SIZEOF_~a^"	keyword))
	   (ac-alignof	(format-symbol "^ALIGNOF_~a^"	keyword))
	   (ac-strideof	(format-symbol "^STRIDEOF_~a^"	keyword)))
      (%sizeof-lib `((define ,sizeof	,ac-sizeof)
		     (define ,alignof	,ac-alignof)
		     (define ,strideof	,ac-strideof)))))

  (define (%generate-label-interface-to-struct)
    ;;A structure definition like:
    ;;
    ;; (define-c-struct timeval
    ;;   "struct timeval"
    ;;   (signed-int	tv_sec)
    ;;   (signed-int	tv_usec))
    ;;
    ;;should become:
    ;;
    ;; (define-label pointer-to-timeval
    ;;   (virtual-fields (mutable tv_sec)
    ;;                   (mutable tv_usec)))
    ;;
    ;; (define-syntax pointer-to-timeval-tv_sec-ref
    ;;   (syntax-rules ()
    ;;     ((_ ?pointer)
    ;;      (pointer-c-ref @TYPEOF_TIMEVAL_TV_SEC@ ?pointer @OFFSETOF_TIMEVAL_TV_USEC@))))
    ;;
    ;; (define-syntax pointer-to-timeval-tv_sec-set!
    ;;   (syntax-rules ()
    ;;     ((_ ?pointer ?value)
    ;;      (pointer-c-ref @TYPEOF_TIMEVAL_TV_SEC@ ?pointer @OFFSETOF_TIMEVAL_TV_SEC@ ?value))))
    ;;
    ;; (define-syntax pointer-to-timeval-tv_usec-ref
    ;;   (syntax-rules ()
    ;;     ((_ ?pointer)
    ;;      (pointer-c-ref @TYPEOF_TIMEVAL_TV_USEC@ ?pointer @OFFSETOF_TIMEVAL_TV_USEC@))))
    ;;
    ;; (define-syntax pointer-to-timeval-tv_usec-set!
    ;;   (syntax-rules ()
    ;;     ((_ ?pointer ?value)
    ;;      (pointer-c-ref @TYPEOF_TIMEVAL_TV_USEC@ ?pointer @OFFSETOF_TIMEVAL_TV_USEC@ ?value))))
    ;;
    (let ((label-name			(format-symbol "pointer-to-~a" struct-name))
	  (fields			'())
	  (accessors-and-mutators	'())
	  (struct-keyword		(string-upcase (symbol->string struct-name))))
      (for-each
	  (lambda (field-name field-type-category)
	    (let* ((accessor	(format-symbol "~a-~a"      label-name field-name))
		   (mutator	(format-symbol "~a-~a-set!" label-name field-name))
		   (keyword	(dot->underscore (string-upcase (symbol->string field-name))))
		   (typeof	(format-symbol "^TYPEOF_~a_~a^"   struct-keyword keyword))
		   (offset	(format-symbol "^OFFSETOF_~a_~a^" struct-keyword keyword)))
	      (if (eq? 'embedded field-type-category)
		  (begin
		    (set-cons! fields `(immutable ,field-name))
		    (set-cons! accessors-and-mutators
			       `(define-syntax ,accessor
				  (syntax-rules ()
				    ((_ ?pointer)
				     (ffi:pointer-add ?pointer ,offset))))))
		(begin
		  (set-cons! fields `(mutable ,field-name))
		  (set-cons! accessors-and-mutators
			     `(define-syntax ,accessor
				(syntax-rules ()
				  ((_ ?pointer)
				   (pointer-c-ref ,typeof ?pointer ,offset)))))
		  (set-cons! accessors-and-mutators
			     `(define-syntax ,mutator
				(syntax-rules ()
				  ((_ ?pointer ?value)
				   (pointer-c-set! ,typeof ?pointer ,offset ?value)))))
		  ))))
	field-names field-type-categories)
      ;; register sexps to the sizeof library, structs section
      (%structs-lib-exports label-name)
      (%structs-lib `((define-label ,label-name
			(virtual-fields ,@fields))
		      ,@accessors-and-mutators))))

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
    ;; (define-syntax <struct-timeval>-tv_sec-ref
    ;;   (syntax-rules ()
    ;;     ((_ ?pointer)
    ;;      (pointer-to-timeval-tv_sec (<struct-timeval>-pointer ?pointer)))))
    ;;
    ;; (define-syntax <struct-timeval>-tv_sec-set!
    ;;   (syntax-rules ()
    ;;     ((_ ?pointer ?value)
    ;;      (pointer-to-timeval-tv-sec-set! (<struct-timeval>-pointer ?pointer) ?value))))
    ;;
    ;; (define-syntax <struct-timeval>-tv_usec-ref
    ;;   (syntax-rules ()
    ;;     ((_ ?pointer)
    ;;      (pointer-to-timeval-tv_usec (<struct-timeval>-pointer ?pointer)))))
    ;;
    ;; (define-syntax <struct-timeval>-tv_usec-set!
    ;;   (syntax-rules ()
    ;;     ((_ ?pointer ?value)
    ;;      (pointer-to-timeval-tv_usec-set! (<struct-timeval>-pointer ?pointer) ?value))))
    ;;
    (let* ((class-name			(format-symbol "<struct-~a>" struct-name))
	   (label-name			(format-symbol "pointer-to-~a" struct-name))
	   (uid				(format-symbol "~a:~a" (class-uid) class-name))
	   (fields			'())
	   (accessors-and-mutators	'())
	   (struct-keyword		(string-upcase (symbol->string struct-name))))
      (for-each
	  (lambda (field-name field-type-category)
	    (let* ((accessor	(format-symbol "~a-~a"      class-name field-name))
		   (mutator	(format-symbol "~a-~a-set!" class-name field-name))
		   (L-accessor	(format-symbol "~a-~a"      label-name field-name))
		   (L-mutator	(format-symbol "~a-~a-set!" label-name field-name))
		   (pointer	(format-symbol "~a-pointer" class-name))
		   (keyword	(dot->underscore (string-upcase (symbol->string field-name))))
		   (typeof	(format-symbol "^TYPEOF_~a_~a^"   struct-keyword keyword))
		   (offset	(format-symbol "^OFFSETOF_~a_~a^" struct-keyword keyword)))
	      (if (eq? 'embedded field-type-category)
		  (begin
		    (set-cons! fields `(immutable ,field-name))
		    (set-cons! accessors-and-mutators
			       `(define-syntax ,accessor
				  (syntax-rules ()
				    ((_ ?pointer)
				     (,L-accessor (,pointer ?pointer)))))))
		(begin
		  (set-cons! fields `(mutable ,field-name))
		  (set-cons! accessors-and-mutators
			     `(define-syntax ,accessor
				(syntax-rules ()
				  ((_ ?pointer)
				   (,L-accessor (,pointer ?pointer))))))
		  (set-cons! accessors-and-mutators
			     `(define-syntax ,mutator
				(syntax-rules ()
				  ((_ ?pointer ?value)
				   (,L-mutator (,pointer ?pointer) ?value)))))
		  ))))
	field-names field-type-categories)
      ;;register sexps to the sizeof library, structs section
      (%structs-lib-exports class-name)
      (let ((sizeof (format-symbol "struct-~a" struct-name)))
	(%structs-lib `((define-class ,class-name
			  (nongenerative ,uid)
			  (protocol (lambda (make-top)
				      (lambda (malloc)
					((make-top) (malloc (c-sizeof ,sizeof))))))
			  (fields (immutable pointer))
			  (virtual-fields ,@fields))
			,@accessors-and-mutators)))))

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
    ;;   (nongenerative nausicaa:<timeval>)
    ;;   (fields (immutable tv_sec)
    ;;           (immutable tv_usec)))
    ;;
    (let* ((class-name	(format-symbol "<~a>" struct-name))
	   (uid		(format-symbol "~a:~a" (class-uid) class-name))
	   (fields	'()))
      (for-each
	  (lambda (field-name field-type-category)
	    (set-cons! fields `(mutable ,field-name)))
	field-names field-type-categories)
      ;;register sexps to the sizeof library, structs section
      (%structs-lib-exports class-name)
      (%structs-lib `((define-class ,class-name
			(nongenerative ,uid)
			(fields ,@fields))))))

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
dnl Contents: foreign library inspection generation
dnl Date: " #;(date->string $date "~a ~b ~e, ~Y") "
dnl
dnl Abstract
dnl
dnl
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
