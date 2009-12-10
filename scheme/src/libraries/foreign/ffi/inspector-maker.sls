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
;;;Copyright (c) 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (foreign ffi inspector-maker)
  (export

    ;; main control
    sizeof-lib			sizeof-lib-write
    sizeof-lib-exports
    autoconf-lib		autoconf-lib-write
    define-shared-object

    ;; inspection
    define-c-defines		define-c-string-defines
    define-c-enumeration	define-c-type-alias
    define-c-type		define-c-struct)
  (import (nausicaa)
    (formations)
    (times-and-dates)
    (strings))


;;;; helpers

(define (hat->dot str)
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


;;;; Autoconf and Scheme libraries

(define $autoconf-library	"")
(define $sizeof-library		'())
(define $sizeof-lib-exports	'())

(define (autoconf-lib str)
  ;;Append STR  to the current Autoconf  library; add a new  line at the
  ;;end.
  ;;
  (set! $autoconf-library (string-append $autoconf-library str "\n")))

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

(define (autoconf-lib-write filename libname)
  ;;Write  to  the  specified  FILENAME  the contents  of  the  Autoconf
  ;;library.  LIBNAME must  be the library specification and  it is used
  ;;only as a comment.
  ;;
  (autoconf-lib "\ndnl end of file")
  (let ((port (transcoded-port (open-file-output-port filename (file-options no-fail))
			       (make-transcoder (utf-8-codec)))))
    (format port "dnl ~a --\n" libname)
    (display $autoconf-license port)
    (display "\n\n" port)
    (display $autoconf-library port)
    (close-port port)))

(define (sizeof-lib-write filename libname)
  ;;Write to  the specified FILENAME  the contents of the  sizeof Scheme
  ;;library.  LIBNAME must be the library specification.
  ;;
  (let ((libout `(library ,libname
		   (export ,@$sizeof-lib-exports)
		   (import (rnrs)
		     (foreign ffi)
		     (foreign ffi sizeof))
		   ,@$sizeof-library)))
    (let ((strout (call-with-string-output-port
		      (lambda (port)
			(pretty-print libout port)))))
      (set! strout (hat->dot strout))
      (let ((port (transcoded-port (open-file-output-port filename (file-options no-fail))
				   (make-transcoder (utf-8-codec)))))

	(format port ";;; ~s --\n" libname)
	(display $sizeof-license port)
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
	 (upname	(string-upcase name))
	 (dnname	(string-downcase name))
	 (tiname	(string-titlecase name))
	 (varname	(format "~a_SHARED_OBJECT" upname))
	 (varname-sym	(string->symbol varname)))
    (autoconf-lib (format "NAU_DS_WITH_OPTION([~a],[~a-shared-object],[~a],
  [~a shared library file],[select ~a shared library file])"
		    varname dnname default-library-name tiname tiname))
    (let ((at-symbol (string->symbol (format "\"^~a^\"" varname))))
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
    ((_ ?enum-name ?symbol-name0 ?symbol-name ...)
     (%register-enumeration (quote ?enum-name) (quote (?symbol-name0 ?symbol-name ...))))))

(define (%register-enumeration enum-name symbol-names)
  (autoconf-lib (format "\ndnl enum ~a" enum-name))
  (for-each (lambda (symbol)
	      (autoconf-lib (format "NAUSICAA_ENUM_VALUE([~a])" symbol))
	      (let ((at-symbol (string->symbol (format "^VALUEOF_~a^"
						 (symbol->string symbol)))))
		(%sizeof-lib `((define ,symbol ,at-symbol)))
		(%sizeof-lib-exports symbol)))
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
	      (autoconf-lib (format "NAUSICAA_DEFINE_VALUE([~a])" symbol))
	      (let ((at-symbol (string->symbol (format "^VALUEOF_~a^" (symbol->string symbol)))))
		(%sizeof-lib `((define ,symbol ,at-symbol)))
		(%sizeof-lib-exports symbol)))
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
	      (autoconf-lib (format "NAUSICAA_STRING_TEST([~a],[~a])" symbol symbol))
	      (let ((at-symbol (string->symbol (format "\"^STRINGOF_~a^\""
						 (symbol->string symbol)))))
		(%sizeof-lib `((define ,symbol ,at-symbol)))
		(%sizeof-lib-exports symbol)))
    symbol-names))


;;;; C type aliases

(define-syntax define-c-type-alias
  (syntax-rules ()
    ((_ ?alias ?type)
     (%register-type-alias (quote ?alias) (quote ?type)))))

(define (%register-type-alias alias type)
  (%sizeof-lib `((define ,alias (quote ,type))))
  (%sizeof-lib-exports alias))


;;;; C type inspection

(define-syntax define-c-type
  (syntax-rules ()
    ((_ ?name ?type-category)
     (%register-type (quote ?name) (quote ?type-category) (symbol->string (quote ?name))))
    ((_ ?name ?type-category ?type-string)
     (%register-type (quote ?name) (quote ?type-category) ?type-string))))

(define (%register-type name type-category string-typedef)
  (let* ((keyword		(string-upcase (symbol->string name)))
	 (symbol-typeof		(string->symbol (format "^TYPEOF_~a^"		keyword)))
	 (symbol-sizeof		(string->symbol (format "^SIZEOF_~a^"		keyword)))
	 (symbol-alignof	(string->symbol (format "^ALIGNOF_~a^"		keyword)))
	 (symbol-strideof	(string->symbol (format "^STRIDEOF_~a^"		keyword)))
	 (symbol-accessor	(string->symbol (format "^GETTEROF_~a^"		keyword)))
	 (symbol-mutator	(string->symbol (format "^SETTEROF_~a^"		keyword)))
	 (name-typeof		name)
	 (name-sizeof		(string->symbol (format "sizeof-~s"		name)))
	 (name-alignof		(string->symbol (format "alignof-~s"		name)))
	 (name-strideof		(string->symbol (format "strideof-~s"		name)))
	 (name-accessor		(string->symbol (format "pointer-ref-c-~s"	name)))
	 (name-mutator		(string->symbol (format "pointer-set-c-~s!"	name))))
    (autoconf-lib (format "NAUSICAA_INSPECT_TYPE([~a],[~a],[~a],[#f])"
		    keyword string-typedef type-category))
    (%sizeof-lib `((define ,name-typeof	(quote ,symbol-typeof))
		   (define ,name-sizeof	,symbol-sizeof)
		   (define ,name-alignof	,symbol-alignof)
		   (define ,name-strideof	,symbol-strideof)
		   (define ,name-accessor	,symbol-accessor)
		   (define ,name-mutator	,symbol-mutator)))
    (%sizeof-lib-exports name-typeof
			 name-sizeof name-alignof name-strideof
			 name-accessor name-mutator)))


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
  (let ((struct-keyword (string-upcase (symbol->string struct-name))))

    ;;Output the data structure inspection stuff.
    ;;
    (let ((symbol-sizeof	(string->symbol (format "^SIZEOF_~a^"	struct-keyword)))
	  (symbol-alignof	(string->symbol (format "^ALIGNOF_~a^"	struct-keyword)))
	  (symbol-strideof	(string->symbol (format "^STRIDEOF_~a^"	struct-keyword)))
	  (name-typeof		struct-name)
	  (name-sizeof		(string->symbol (format "sizeof-~s"	struct-name)))
	  (name-alignof		(string->symbol (format "alignof-~s"	struct-name)))
	  (name-strideof	(string->symbol (format "strideof-~s"	struct-name))))
      (autoconf-lib (format "NAUSICAA_INSPECT_STRUCT_TYPE([~a],[~a],[#f])"
		      struct-keyword struct-string-typedef))
      (%sizeof-lib `((define ,name-sizeof	,symbol-sizeof)
		     (define ,name-alignof	,symbol-alignof)
		     (define ,name-strideof	,symbol-strideof)))
      (%sizeof-lib-exports name-sizeof name-alignof name-strideof))

    ;;Output the field inspection stuff.
    ;;
    (for-each
	(lambda (field-name field-type-category)
	  (let* ((field-keyword		(dot->underscore (string-upcase (symbol->string field-name))))
		 (name-field-accessor	(string->symbol
					 (format "struct-~a-~a-ref"  struct-name field-name)))
		 (name-field-mutator	(string->symbol
					 (format "struct-~a-~a-set!" struct-name field-name)))
		 (symbol-field-offset	(string->symbol
					 (format "^OFFSETOF_~a_~a^" struct-keyword field-keyword)))
		 (symbol-field-accessor	(string->symbol
					 (format "^GETTEROF_~a_~a^" struct-keyword field-keyword)))
		 (symbol-field-mutator	(string->symbol
					 (format "^SETTEROF_~a_~a^" struct-keyword field-keyword))))
	    (if (eq? 'embedded field-type-category)
		(begin
		  (autoconf-lib (format "NAUSICAA_INSPECT_FIELD_TYPE_POINTER([~a],[~a],[~a])"
				  (format "~a_~a" struct-keyword field-keyword)
				  struct-string-typedef field-name))
		  (%sizeof-lib `((define-c-struct-field-pointer-accessor
				   ,name-field-accessor ,symbol-field-offset)))
		  (%sizeof-lib-exports name-field-accessor))
	      (begin
		(autoconf-lib (format "NAUSICAA_INSPECT_FIELD_TYPE([~a],[~a],[~a],[~a])"
				(format "~a_~a" struct-keyword field-keyword)
				struct-string-typedef field-name field-type-category))
		(%sizeof-lib `((define-c-struct-accessor-and-mutator
				 ,name-field-mutator ,name-field-accessor
				 ,symbol-field-offset ,symbol-field-mutator ,symbol-field-accessor)))
		(%sizeof-lib-exports name-field-mutator name-field-accessor)))))
      field-names field-type-categories)))


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
