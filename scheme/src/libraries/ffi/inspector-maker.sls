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
;;;Copyright (c) 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (ffi inspector-maker)
  (export

    ;; main control
    sizeof-lib			sizeof-lib-write
    sizeof-lib-exports
    autoconf-lib		autoconf-lib-write
    define-shared-object
    clang-lib-write

    class-uid

    ;; inspection
    define-c-defines		define-c-string-defines
    define-c-enumeration	define-c-type-alias
    define-c-type		define-c-struct)
  (import (nausicaa)
    (formations)
    (times-and-dates)
    (strings))


;;;; helpers

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
    (display "\n\n])\n\n\ndnl end of file\n" port)
    (close-port port)))



;;;; Sizeof library

(define $sizeof-library		'())
(define $sizeof-lib-exports	'())

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

(define (sizeof-lib-write filename libname libname-clang-types)
  ;;Write to  the specified FILENAME  the contents of the  sizeof Scheme
  ;;library.  LIBNAME must be the library specification.
  ;;
  (let ((libout `(library ,libname
		   (export
		     c-sizeof c-strideof c-alignof c-valueof c-inspect
		     pointer-c-ref pointer-c-set! pointer-c-accessor pointer-c-mutator
		     array-c-ref array-c-set! array-c-pointer-to
		     ,@$sizeof-lib-exports
		     ,@$structs-lib-exports)
		   (import (rnrs)
		     (only (classes) define-label)
		     (for ,libname-clang-types expand)
		     (ffi syntax-helpers)
		     (prefix (ffi pointers)		ffi:)
		     (prefix (ffi sizeof)		ffi:)
		     (prefix (ffi peekers-and-pokers)	ffi:)
		     (ffi extension-utilities))
		   (define-sizeof-macros)
		   ,@$sizeof-library
		   ,@$structs-library)))
    (let ((strout (call-with-string-output-port
		      (lambda (port)
			(display libout port)))))
      (set! strout (hat->at strout))
      (let ((port (transcoded-port (open-file-output-port filename (file-options no-fail))
       				   (make-transcoder (utf-8-codec)))))
	(format port ";;; ~s --\n" libname)
	(display $sizeof-license port)
	(display "\n\n" port)
	(display strout port)
	(display "\n\n;;; end of file\n" port)
	(close-port port)))))


;;;; Structs library

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


;;;; Data types library

(define $clang-library	'())
(define $clang-type-map	'())
(define $clang-type-enum '())

(define (%clang-lib sexp)
  ;;Append an S-expression to the end of the current clang library.
  ;;
  (set! $clang-library (append $clang-library sexp)))

(define (%clang-register-type-alias external-type internal-type)
  (set-cons! $clang-type-map `((,external-type) (quote ,internal-type)))
  (set-cons! $clang-type-enum external-type))

(define (clang-lib-write filename clang-libname package)
  ;;Write to  the specified  FILENAME the contents  of the  clang Scheme
  ;;library.  CLANG-LIBNAME must  be the library specification.  PACKAGE
  ;;must be the name of the package as a string.
  ;;
  (let ((libout `(library ,clang-libname
		   (export clang-foreign-type->clang-external-type
			   clang-maybe-foreign-type->clang-external-type
			   enum-clang-foreign-types clang-external-types)
		   (import (rnrs))
		   (define-enumeration enum-clang-foreign-types
		     ,(reverse $clang-type-enum)
		     clang-external-types)
		   (define (clang-foreign-type->clang-external-type type)
		     (case type
		       ,@(reverse $clang-type-map)
		       (else #f)))
		   (define (clang-maybe-foreign-type->clang-external-type type)
		     (or (clang-foreign-type->clang-external-type type)
			 type))
		   )))
    (let ((strout (call-with-string-output-port
		      (lambda (port)
			(pretty-print libout port)))))
      (set! strout (hat->at strout))
      (let ((port (transcoded-port (open-file-output-port filename (file-options no-fail))
				   (make-transcoder (utf-8-codec)))))
	(format port ";;; ~s --\n" clang-libname)
	(display $clang-license port)
	(display "\n\n" port)
	(display strout port)
	(display "\n\n;;; end of file\n" port)
	(close-port port)))))


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
    (let ((at-symbol (format-symbol "\"^~a^\"" varname)))
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
		(let ((at-symbol (format "\"^STRINGOF_~a^\"" symbol)))
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
  (%clang-register-type-alias alias type)
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
	 )
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
    (%clang-register-type-alias name ac-symbol-typeof)))


;;;; C struct type inspection

(define-syntax define-c-struct
  (syntax-rules ()
    ((_ ?name ?type-string
	(?field-type-category ?field-name)
	...)
     (%register-struct-type (quote ?name) (quote ?type-string)
			    (quote (?field-name ...))
			    (quote (?field-type-category ...))))))

(define (%register-struct-type struct-name struct-string-typedef field-names field-type-categories)
  (autoconf-lib (format "\ndnl Struct inspection: ~a" struct-name))
  (let* ((struct-keyword (string-upcase (symbol->string struct-name)))
	 (label-name	(format "<struct-~a>" struct-name))
	 (label-type	(string->symbol label-name)))

    (%register-type-alias struct-name struct-name)

    ;;Output the data structure inspection stuff.
    ;;
    (let ((ac-symbol-sizeof	(format-symbol "^SIZEOF_~a^"	struct-keyword))
	  (ac-symbol-alignof	(format-symbol "^ALIGNOF_~a^"	struct-keyword))
	  (ac-symbol-strideof	(format-symbol "^STRIDEOF_~a^"	struct-keyword))
	  (name-typeof		struct-name)
	  (name-sizeof		(format-symbol "sizeof-~s"	struct-name))
	  (name-alignof		(format-symbol "alignof-~s"	struct-name))
	  (name-strideof	(format-symbol "strideof-~s"	struct-name)))
      (autoconf-lib (format "NAUSICAA_INSPECT_STRUCT_TYPE([~a],[~a],[#f])"
		      struct-keyword struct-string-typedef))
      (%sizeof-lib `((define ,name-sizeof	,ac-symbol-sizeof)
		     (define ,name-alignof	,ac-symbol-alignof)
		     (define ,name-strideof	,ac-symbol-strideof)))
;;;   (%sizeof-lib-exports name-sizeof name-alignof name-strideof)
      (%structs-lib-exports label-type))

    ;;Output the field inspection stuff.
    ;;
    (let ((label-fields  '())
	  (label-methods '()))
      (for-each
	  (lambda (field-name field-type-category)
	    (let* ((field-keyword	(dot->underscore (string-upcase (symbol->string field-name))))
		   (name-field-accessor
		    (format-symbol "struct-~a-~a-ref"  struct-name field-name))
		   (name-field-mutator
		    (format-symbol "struct-~a-~a-set!" struct-name field-name))
		   (label-field-accessor
		    (format-symbol "~a-~a"      label-type field-name))
		   (label-field-mutator
		    (format-symbol "~a-~a-set!" label-type field-name))
		   (ac-symbol-field-typeof
		    (format-symbol "^TYPEOF_~a_~a^"   struct-keyword field-keyword))
		   (ac-symbol-field-offset
		    (format-symbol "^OFFSETOF_~a_~a^" struct-keyword field-keyword)))
	      (if (eq? 'embedded field-type-category)
		  (begin
		    (autoconf-lib (format "NAUSICAA_INSPECT_FIELD_TYPE_POINTER([~a],[~a],[~a])"
				    (format "~a_~a" struct-keyword field-keyword)
				    struct-string-typedef field-name))
		    (set-cons! label-fields `(immutable ,field-name))
		    (set-cons! label-methods `(define-syntax ,label-field-accessor
						(syntax-rules ()
						  ((_ ?pointer)
						   (pointer-c-ref ,ac-symbol-field-typeof
								  ?pointer ,ac-symbol-field-offset)))))
		    )
		(begin
		  (autoconf-lib (format "NAUSICAA_INSPECT_FIELD_TYPE([~a],[~a],[~a],[~a])"
				  (format "~a_~a" struct-keyword field-keyword)
				  struct-string-typedef field-name field-type-category))
		  (set-cons! label-fields `(mutable ,field-name))
		  (set-cons! label-methods `(define-syntax ,label-field-accessor
					      (syntax-rules ()
						((_ ?pointer)
						 (pointer-c-ref ,ac-symbol-field-typeof
								?pointer ,ac-symbol-field-offset)))))
		  (set-cons! label-methods `(define-syntax ,label-field-mutator
					      (syntax-rules ()
						((_ ?pointer ?value)
						 (pointer-c-set! ,ac-symbol-field-typeof
								 ?pointer ,ac-symbol-field-offset
								 ?value)))))
		  ))))
	field-names field-type-categories)
      (%structs-lib `((define-label ,label-type
			(virtual-fields ,@label-fields))
		      ,@label-methods)))))


;;;; license notices

(define $date
  (current-date))

(define $sizeof-license
  (string-append ";;;
;;;Part of: Nausicaa
;;;Contents: foreign library inspection generation
;;;Date: " (date->string $date "~a ~b ~e, ~Y") "
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) " (date->string $date "~Y") " Marco Maggi <marco.maggi-ipsu@poste.it>
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
;;;Date: " (date->string $date "~a ~b ~e, ~Y") "
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) " (date->string $date "~Y") " Marco Maggi <marco.maggi-ipsu@poste.it>
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
dnl Date: " (date->string $date "~a ~b ~e, ~Y") "
dnl
dnl Abstract
dnl
dnl
dnl
dnl Copyright (c) " (date->string $date "~Y") " Marco Maggi <marco.maggi-ipsu@poste.it>
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
