;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: foreign function interface extensions
;;;Date: Tue Nov 18, 2008
;;;
;;;Abstract
;;;
;;;	This is the core of the foreign functions interface.
;;;
;;;Copyright (c) 2008-2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa ffi)
  (export

;;; bindings from (ffi conditions)
    &library-name
    make-library-name-condition
    library-name-condition?
    condition-library-name

    &shared-object
    make-shared-object-condition
    shared-object-condition?
    condition-shared-object

    &foreign-symbol
    make-foreign-symbol-condition
    foreign-symbol-condition?
    condition-foreign-symbol

    &shared-object-opening-error
    make-shared-object-opening-error-condition
    shared-object-opening-error-condition?

    &shared-object-lookup-error
    make-shared-object-lookup-error-condition
    shared-object-lookup-error-condition?

    raise-shared-object-opening-error
    raise-shared-object-lookup-error

;;; bindings from (ffi pointers)
    <pointer>
    pointer?
    pointer-null			pointer-null?
    integer->pointer			pointer->integer
    pointer-diff			pointer-add
    pointer-incr!
    pointer=?				pointer<>?
    pointer<?				pointer>?
    pointer<=?				pointer>=?

;;; --------------------------------------------------------------------

    shared-object?			libc-shared-object
    open-shared-object			lookup-shared-object
    make-c-function			make-c-function/with-errno
    make-c-callout			make-c-callout/with-errno
    make-c-callback			(rename (platform:free-c-callback free-c-callback))

    define-shared-object
    define-c-functions			define-c-functions/with-errno
    define-c-callouts			define-c-callouts/with-errno
    make-c-function*			make-c-function/with-errno*
    make-c-callout*			make-c-callout/with-errno*
    make-c-callback*)
  (import (nausicaa)
    (for (nausicaa ffi clang-data-types) expand run)
    (nausicaa ffi conditions)
;;;    (nausicaa ffi peekers-and-pokers)
    (nausicaa ffi pointers)
    (nausicaa ffi sizeof)
    (for (nausicaa ffi syntax-helpers) expand)
    (prefix (nausicaa ffi compat) platform:)
    (only (nausicaa language unimplemented) raise-unimplemented-error))


;;;; helpers

(define (%symbol/string->string thing who msg)
  (cond ((symbol? thing)
	 (symbol->string thing))
	((string? thing)
	 thing)
	(else
	 (assertion-violation who msg thing))))


(define-class (<shared-object> open-shared-object shared-object?)
  (nongenerative nausicaa:ffi:primitives:<shared-object>)
  (fields (immutable library-name)
	  (immutable reference))
  (protocol (lambda (make-top)
	      (lambda (library-name)
		(let ((library-name (%symbol/string->string
				     library-name 'open-shared-object
				     "expected Scheme string or symbol as shared library name")))
		  ((make-top) library-name (platform:open-shared-object library-name)))))))

(define libc-shared-object
  (open-shared-object LIBC_SHARED_OBJECT_SPEC))

(define (lookup-shared-object (shared-object <shared-object>) foreign-symbol)
  ;;Catch and reraise &shared-object-lookup-error  so that we can put in
  ;;the <shared-object> record.
  ;;
  (guard (E ((shared-object-lookup-error-condition? E)
	     (raise-shared-object-lookup-error (condition-who E)
					       (condition-message E)
					       shared-object
					       (condition-foreign-symbol E)))
	    (else
	     (raise-continuable E)))
    (platform:lookup-shared-object
     shared-object.reference
     (%symbol/string->string foreign-symbol 'lookup-shared-object
			     "expected string or symbol as symbol in foreign shared library"))))


(define (make-c-function shared-object ret-type funcname arg-types)
  (make-c-callout (clang-external-type->clang-type ret-type)
		  (lookup-shared-object shared-object funcname)
		  (map clang-external-type->clang-type arg-types)))

(define (make-c-function/with-errno shared-object ret-type funcname arg-types)
  (make-c-callout/with-errno (clang-external-type->clang-type ret-type)
			     (lookup-shared-object shared-object funcname)
			     (map clang-external-type->clang-type arg-types)))

(define (make-c-callout ret-type address arg-types)
  (platform:make-c-callout (clang-external-type->clang-type ret-type)
			   address
			   (map clang-external-type->clang-type arg-types)))

(define (make-c-callout/with-errno ret-type address arg-types)
  (platform:make-c-callout/with-errno (clang-external-type->clang-type ret-type)
				      address
				      (map clang-external-type->clang-type arg-types)))

(define (make-c-callback ret-type proc arg-types)
  (platform:make-c-callback (clang-external-type->clang-type ret-type)
			    proc
			    (map clang-external-type->clang-type arg-types)))


;; (define-syntax define-c-struct-accessor-and-mutator
;;   (syntax-rules ()
;;     ((_ ?mutator-name ?accessor-name ?field-offset ?foreign-type-mutator ?foreign-type-accessor)
;;      (begin
;;        (define-c-struct-accessor ?accessor-name ?field-offset ?foreign-type-accessor)
;;        (define-c-struct-mutator  ?mutator-name  ?field-offset ?foreign-type-mutator)))))

;; (define-syntax define-c-struct-accessor
;;   (lambda (use-stx)
;;     (syntax-case use-stx ()
;;       ((_ ?accessor-name ?field-offset ?foreign-type-accessor)
;;        (if (syntax->datum (syntax ?field-offset))
;; 	   #'(define-syntax ?accessor-name
;; 	       (syntax-rules ()
;; 		 ((_ struct-pointer)
;; 		  (?foreign-type-accessor struct-pointer ?field-offset))))
;; 	 #'(define-syntax ?accessor-name
;; 	     (syntax-rules ()
;; 	       ((_ struct-pointer)
;; 		(raise-unimplemented-error (quote ?accessor-name))))))))))

;; (define-syntax define-c-struct-mutator
;;   (lambda (use-stx)
;;     (syntax-case use-stx ()
;;       ((_ ?mutator-name ?field-offset ?foreign-type-mutator)
;;        (if (syntax->datum (syntax ?field-offset))
;; 	   #'(define-syntax ?mutator-name
;; 	       (syntax-rules ()
;; 		 ((_ struct-pointer value)
;; 		  (?foreign-type-mutator struct-pointer
;; 					?field-offset
;; 					value))))
;; 	 #'(define-syntax ?mutator-name
;; 	     (syntax-rules ()
;; 	       ((_ struct-pointer value)
;; 		(raise-unimplemented-error (quote ?mutator-name))))))))))

;; (define-syntax define-c-struct-field-pointer-accessor
;;   (lambda (use-stx)
;;     (syntax-case use-stx ()
;;       ((_ ?accessor-name ?field-offset)
;;        (if (syntax->datum (syntax ?field-offset))
;; 	   #'(define-syntax ?accessor-name
;; 	       (syntax-rules ()
;; 		 ((_ struct-pointer)
;; 		  (pointer-add struct-pointer ?field-offset))))
;; 	 #'(define-syntax ?accessor-name
;; 	     (syntax-rules ()
;; 	       ((_ struct-pointer)
;; 		(raise-unimplemented-error (quote ?accessor-name))))))))))


;; (define-syntax define-c-struct-accessor-and-mutator/from-type
;;   (syntax-rules ()
;;     ((_ ?mutator-name ?accessor-name ?field-offset ?type)
;;      (begin
;;        (define-c-struct-accessor/from-type ?accessor-name ?field-offset ?type)
;;        (define-c-struct-mutator/from-type  ?mutator-name  ?field-offset ?type)))))

;; (define-syntax define-c-struct-accessor/from-type
;;   (lambda (stx)
;;     (syntax-case stx ()
;;       ((_ ?accessor-name ?field-offset ?type)
;;        #`(define-syntax ?accessor-name
;; 	   (syntax-rules ()
;; 	     ((_ struct-pointer)
;; 	      #,(if (syntax->datum (syntax ?field-offset))
;; 		    #'(pointer-c-ref ?type struct-pointer ?field-offset)
;; 		  #'(raise-unimplemented-error (quote ?accessor-name))))))))))

;; (define-syntax define-c-struct-mutator/from-type
;;   (lambda (use-stx)
;;     (syntax-case use-stx ()
;;       ((_ ?mutator-name ?field-offset ?foreign-type-mutator)
;;        #`(define-syntax ?mutator-name
;; 	   (syntax-rules ()
;; 	     ((_ struct-pointer value)
;; 	      #,(if (syntax->datum (syntax ?field-offset))
;; 		    #'(pointer-c-set! ?type struct-pointer ?field-offset value)
;; 		  #'(raise-unimplemented-error (quote ?mutator-name))))))))))


(define-syntax make-c-function*
  (lambda (stx)
    (define type-mapper 'clang-maybe-foreign-type->clang-external-type)
    (syntax-case stx ()
      ((k ?shared-object ?ret-type ?funcname ((?arg-type0 ?arg-name0) (?arg-type ?arg-name) ...))
       #'(k ?shared-object ?ret-type ?funcname (?arg-type0 ?arg-type ...)))

      ((k ?shared-object ?ret-type ?funcname (?arg-type0 ?arg-type ...))
       (with-syntax ((MAPPER		(datum->syntax #'?ret-type  type-mapper))
		     (MAPPER0		(datum->syntax #'?arg-type0 type-mapper))
		     ((MAPPER1 ...)	(map (lambda (type/stx)
					       (datum->syntax type/stx type-mapper))
					  (syntax->list #'(?arg-type ...)))))
	 #'(make-c-function ?shared-object
			    (MAPPER (quote ?ret-type))
			    '?funcname
			    `(,(MAPPER0 (quote ?arg-type0))
			      ,(MAPPER1 (quote ?arg-type))
			      ...))))
      )))

(define-syntax make-c-function/with-errno*
  (lambda (stx)
    (define type-mapper 'clang-maybe-foreign-type->clang-external-type)
    (syntax-case stx ()
      ((k ?shared-object ?ret-type ?funcname ((?arg-type0 ?arg-name0) (?arg-type ?arg-name) ...))
       #'(k ?shared-object ?ret-type ?funcname (?arg-type0 ?arg-type ...)))

      ((k ?shared-object ?ret-type ?funcname (?arg-type0 ?arg-type ...))
       (with-syntax ((MAPPER		(datum->syntax #'?ret-type  type-mapper))
		     (MAPPER0		(datum->syntax #'?arg-type0 type-mapper))
		     ((MAPPER1 ...)	(map (lambda (type/stx)
					       (datum->syntax type/stx type-mapper))
					  (syntax->list #'(?arg-type ...)))))
	 #'(make-c-function/with-errno ?shared-object
				       (MAPPER (quote ?ret-type))
				       '?funcname
				       `(,(MAPPER0 (quote ?arg-type0))
					 ,(MAPPER1 (quote ?arg-type))
					 ...))))
      )))

(define-syntax make-c-callout*
  (lambda (stx)
    (define type-mapper 'clang-maybe-foreign-type->clang-external-type)
    (syntax-case stx ()
      ((k ?ret-type ?address ((?arg-type0 ?arg-name0) (?arg-type ?arg-name) ...))
       #'(k ?ret-type ?address (?arg-type0 ?arg-type ...)))

      ((_ ?ret-type ?address (?arg-type0 ?arg-type ...))
       (with-syntax ((MAPPER		(datum->syntax #'?ret-type  type-mapper))
		     (MAPPER0		(datum->syntax #'?arg-type0 type-mapper))
		     ((MAPPER1 ...)	(map (lambda (type/stx)
					       (datum->syntax type/stx type-mapper))
					  (syntax->list #'(?arg-type ...)))))
	 #'(make-c-callout (MAPPER (quote ?ret-type))
			   ?address
			   `(,(MAPPER0 (quote ?arg-type0))
			     ,(MAPPER1 (quote ?arg-type))
			     ...))))
      )))

(define-syntax make-c-callout/with-errno*
  (lambda (stx)
    (define type-mapper 'clang-maybe-foreign-type->clang-external-type)
    (syntax-case stx ()
      ((k ?ret-type ?address ((?arg-type0 ?arg-name0) (?arg-type ?arg-name) ...))
       #'(k ?ret-type ?address (?arg-type0 ?arg-type ...)))

      ((_ ?ret-type ?address (?arg-type0 ?arg-type ...))
       (with-syntax ((MAPPER		(datum->syntax #'?ret-type  type-mapper))
		     (MAPPER0		(datum->syntax #'?arg-type0 type-mapper))
		     ((MAPPER1 ...)	(map (lambda (type/stx)
					       (datum->syntax type/stx type-mapper))
					  (syntax->list #'(?arg-type ...)))))
	 #'(make-c-callout/with-errno (MAPPER (quote ?ret-type))
				      ?address
				      `(,(MAPPER0 (quote ?arg-type0))
					,(MAPPER1 (quote ?arg-type))
					...))))
      )))

(define-syntax make-c-callback*
  (lambda (stx)
    (define type-mapper 'clang-maybe-foreign-type->clang-external-type)
    (syntax-case stx ()
      ((k ?ret-type ?scheme-function ((?arg-type0 ?arg-name0) (?arg-type ?arg-name) ...))
       #'(k ?ret-type ?scheme-function (?arg-type0 ?arg-type ...)))

      ((_ ?ret-type ?scheme-function (?arg-type0 ?arg-type ...))
       (with-syntax ((MAPPER		(datum->syntax #'?ret-type  type-mapper))
		     (MAPPER0		(datum->syntax #'?arg-type0 type-mapper))
		     ((MAPPER1 ...)	(map (lambda (type/stx)
					       (datum->syntax type/stx type-mapper))
					  (syntax->list #'(?arg-type ...)))))
	 #'(make-c-callback (MAPPER (quote ?ret-type))
			    ?scheme-function
			    `(,(MAPPER0 (quote ?arg-type0))
			      ,(MAPPER1 (quote ?arg-type))
			      ...))))
      )))


(define-syntax define-shared-object
  (syntax-rules ()
    ((_ ?name ?library-name)
     (define ?name
       (open-shared-object ?library-name)))))

(define-syntax define-c-functions
  (syntax-rules ()
    ((_ ?shared-object (?name (?retval ?funcname (?arg0 ?arg ...))) ...)
     (begin
       (define ?name
	 (make-c-function* ?shared-object ?retval ?funcname (?arg0 ?arg ...)))
       ...))))

(define-syntax define-c-functions/with-errno
  (syntax-rules ()
    ((_ ?shared-object (?name (?retval ?funcname (?arg0 ?arg ...))) ...)
     (begin
       (define ?name
	 (make-c-function/with-errno* ?shared-object ?retval ?funcname (?arg0 ?arg ...)))
       ...))))

(define-syntax define-c-callouts
  (syntax-rules ()
    ((_ (?name (?retval ?funcname (?arg0 ?arg ...))) ...)
     (begin
       (define ?name
	 (make-c-callout* ?retval ?funcname (?arg0 ?arg ...)))
       ...))))

(define-syntax define-c-callouts/with-errno
  (syntax-rules ()
    ((_ (?name (?retval ?funcname (?arg0 ?arg ...))) ...)
     (begin
       (define ?name
	 (make-c-callout/with-errno* ?retval ?funcname (?arg0 ?arg ...)))
       ...))))


;;;; done

)

;;; end of file
