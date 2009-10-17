;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: foreign function interface extensions
;;;Date: Tue Nov 18, 2008
;;;
;;;Abstract
;;;
;;;	This is the core of the foreign functions interface.
;;;
;;;Copyright (c) 2008, 2009 Marco Maggi <marcomaggi@gna.org>
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


(library (foreign ffi)
  (export

    ;;shared object access
    open-shared-object
    shared-object			self-shared-object

    ;;interface functions
    primitive-make-c-function		primitive-make-c-function/with-errno
    make-c-function			make-c-function/with-errno
    define-c-function			define-c-function/with-errno
    primitive-make-c-callback		primitive-free-c-callback
    make-c-callback
    errno

    ;;foreign struct accessors
    define-c-struct-accessor-and-mutator
    define-c-struct-accessor		define-c-struct-mutator
    define-c-struct-field-pointer-accessor

    ;; bindings from (foreign ffi pointers)
    pointer?
    pointer-null		pointer-null?
    integer->pointer		pointer->integer
    pointer-diff		pointer-add
    pointer=?			pointer<>?
    pointer<?			pointer>?
    pointer<=?			pointer>=?

;;; bindings from (foreign ffi peekers-and-pokers)

    ;;peekers
    pointer-ref-c-int8			pointer-ref-c-uint8
    pointer-ref-c-int16			pointer-ref-c-uint16
    pointer-ref-c-int32			pointer-ref-c-uint32
    pointer-ref-c-int64			pointer-ref-c-uint64
    pointer-ref-c-float			pointer-ref-c-double
    pointer-ref-c-signed-char		pointer-ref-c-unsigned-char
    pointer-ref-c-signed-short		pointer-ref-c-unsigned-short
    pointer-ref-c-signed-int		pointer-ref-c-unsigned-int
    pointer-ref-c-signed-long		pointer-ref-c-unsigned-long
    pointer-ref-c-signed-long-long	pointer-ref-c-unsigned-long-long
    pointer-ref-c-pointer		pointer-ref-c-void*

    ;;pokers
    pointer-set-c-int8!			pointer-set-c-uint8!
    pointer-set-c-int16!		pointer-set-c-uint16!
    pointer-set-c-int32!		pointer-set-c-uint32!
    pointer-set-c-int64!		pointer-set-c-uint64!
    pointer-set-c-float!		pointer-set-c-double!
    pointer-set-c-signed-char!		pointer-set-c-unsigned-char!
    pointer-set-c-signed-short!		pointer-set-c-unsigned-short!
    pointer-set-c-signed-int!		pointer-set-c-unsigned-int!
    pointer-set-c-signed-long!		pointer-set-c-unsigned-long!
    pointer-set-c-signed-long-long!	pointer-set-c-unsigned-long-long!
    pointer-set-c-pointer!		pointer-set-c-void*!

    ;;array peekers
    array-ref-c-int8			array-ref-c-uint8
    array-ref-c-int16			array-ref-c-uint16
    array-ref-c-int32			array-ref-c-uint32
    array-ref-c-int64			array-ref-c-uint64
    array-ref-c-float			array-ref-c-double
    array-ref-c-signed-char		array-ref-c-unsigned-char
    array-ref-c-signed-short		array-ref-c-unsigned-short
    array-ref-c-signed-int		array-ref-c-unsigned-int
    array-ref-c-signed-long		array-ref-c-unsigned-long
    array-ref-c-signed-long-long	array-ref-c-unsigned-long-long
    array-ref-c-void*			array-ref-c-pointer

    ;;array pokers
    array-set-c-int8!			array-set-c-uint8!
    array-set-c-int16!			array-set-c-uint16!
    array-set-c-int32!			array-set-c-uint32!
    array-set-c-int64!			array-set-c-uint64!
    array-set-c-float!			array-set-c-double!
    array-set-c-signed-char!		array-set-c-unsigned-char!
    array-set-c-signed-short!		array-set-c-unsigned-short!
    array-set-c-signed-int!		array-set-c-unsigned-int!
    array-set-c-signed-long!		array-set-c-unsigned-long!
    array-set-c-signed-long-long!	array-set-c-unsigned-long-long!
    array-set-c-void*!			array-set-c-pointer!

    peek-signed-char			peek-unsigned-char
    peek-signed-short			peek-unsigned-short
    peek-signed-int			peek-unsigned-int
    peek-signed-long			peek-unsigned-long
    peek-signed-long-long		peek-unsigned-long-long
    peek-float				peek-double
    peek-pointer			peek-void*

    peek-int8				peek-uint8
    peek-int16				peek-uint16
    peek-int32				peek-uint32
    peek-int64				peek-uint64

    poke-signed-char!			poke-unsigned-char!
    poke-signed-short!			poke-unsigned-short!
    poke-signed-int!			poke-unsigned-int!
    poke-signed-long!			poke-unsigned-long!
    poke-signed-long-long!		poke-unsigned-long-long!
    poke-float!				poke-double!
    poke-pointer!			poke-void*!

    poke-int8!				poke-uint8!
    poke-int16!				poke-uint16!
    poke-int32!				poke-uint32!
    poke-int64!				poke-uint64!

    peek-array-signed-char		peek-array-unsigned-char
    peek-array-signed-short		peek-array-unsigned-short
    peek-array-signed-int		peek-array-unsigned-int
    peek-array-signed-long		peek-array-unsigned-long
    peek-array-signed-long-long		peek-array-unsigned-long-long
    peek-array-float			peek-array-double
    peek-array-pointer			peek-array-void*

    peek-array-int8			peek-array-uint8
    peek-array-int16			peek-array-uint16
    peek-array-int32			peek-array-uint32
    peek-array-int64			peek-array-uint64

    poke-array-signed-char!		poke-array-unsigned-char!
    poke-array-signed-short!		poke-array-unsigned-short!
    poke-array-signed-int!		poke-array-unsigned-int!
    poke-array-signed-long!		poke-array-unsigned-long!
    poke-array-signed-long-long!	poke-array-unsigned-long-long!
    poke-array-float!			poke-array-double!
    poke-array-pointer!			poke-array-void*!

    poke-array-int8!			poke-array-uint8!
    poke-array-int16!			poke-array-uint16!
    poke-array-int32!			poke-array-uint32!
    poke-array-int64!			poke-array-uint64!)
  (import (rnrs)
    (foreign ffi pointers)
    (foreign ffi peekers-and-pokers)
    (only (foreign ffi compat)
	  shared-object primitive-open-shared-object self-shared-object
	  primitive-make-c-function primitive-make-c-function/with-errno
	  primitive-make-c-callback primitive-free-c-callback
	  errno)
    (only (unimplemented)
	  raise-unimplemented-error))


(define (open-shared-object library-name)
  (primitive-open-shared-object (if (symbol? library-name)
				    (symbol->string library-name)
				  library-name)))

(define-syntax make-c-function
  (lambda (stx)
    (define (%quote-if-predefined-type arg-stx)
      (if (memq (syntax->datum arg-stx)
		'(void
		  char schar signed-char uchar unsigned-char
		  int signed-int ssize_t uint unsigned unsigned-int size_t
		  long signed-long ulong unsigned-long float double
		  pointer void* char* FILE* callback))
	  (list (syntax quote) arg-stx)
	arg-stx))
    (syntax-case stx ()
      ((_ ?ret-type ?funcname (?arg-type0 ?arg-type ...))
       (with-syntax ((ret	(%quote-if-predefined-type #'?ret-type))
		     ((arg ...)	(map %quote-if-predefined-type #'(?arg-type0 ?arg-type ...))))
	 #'(primitive-make-c-function ret '?funcname (list arg ...)))))))

(define-syntax make-c-function/with-errno
  (lambda (stx)
    (define (%quote-if-predefined-type arg-stx)
      (if (memq (syntax->datum arg-stx)
		'(void
		  char schar signed-char uchar unsigned-char
		  int signed-int ssize_t uint unsigned unsigned-int size_t
		  long signed-long ulong unsigned-long float double
		  pointer void* char* FILE* callback))
	  (list (syntax quote) arg-stx)
	arg-stx))
    (syntax-case stx ()
      ((_ ?ret-type ?funcname (?arg-type0 ?arg-type ...))
       (with-syntax ((ret	(%quote-if-predefined-type #'?ret-type))
		     ((arg ...)	(map %quote-if-predefined-type #'(?arg-type0 ?arg-type ...))))
	 #'(primitive-make-c-function/with-errno ret '?funcname (list arg ...)))))))

(define-syntax define-c-function
  (syntax-rules ()
    ((_ ?name (?ret-type ?funcname (?arg-type0 ?arg-type ...)))
     (define ?name
       (make-c-function ?ret-type ?funcname (?arg-type0 ?arg-type ...))))))

(define-syntax define-c-function/with-errno
  (syntax-rules ()
    ((_ ?name (?ret-type ?funcname (?arg-type0 ?arg-type ...)))
     (define ?name
       (make-c-function/with-errno ?ret-type ?funcname (?arg-type0 ?arg-type ...))))))


(define-syntax make-c-callback
  (lambda (stx)
    (define (%quote-if-predefined-type arg-stx)
      (if (memq (syntax->datum arg-stx)
		'(void
		  char schar signed-char uchar unsigned-char
		  int signed-int ssize_t uint unsigned unsigned-int size_t
		  long signed-long ulong unsigned-long float double
		  pointer void* char* FILE* callback))
	  (list (syntax quote) arg-stx)
	arg-stx))
    (syntax-case stx ()
      ((_ ?ret-type ?scheme-function (?arg-type0 ?arg-type ...))
       (with-syntax ((ret	(%quote-if-predefined-type #'?ret-type))
		     ((arg ...)	(map %quote-if-predefined-type #'(?arg-type0 ?arg-type ...))))
	 #'(primitive-make-c-callback ret ?scheme-function (list arg ...)))))))


;;;; foreign structures accessors

(define-syntax define-c-struct-accessor-and-mutator
  (syntax-rules ()
    ((_ ?mutator-name ?accessor-name ?field-offset ?foreign-type-mutator ?foreign-type-accessor)
     (begin
       (define-c-struct-accessor ?accessor-name ?field-offset ?foreign-type-accessor)
       (define-c-struct-mutator  ?mutator-name ?field-offset ?foreign-type-mutator)))))

(define-syntax define-c-struct-accessor
  (lambda (use-stx)
    (syntax-case use-stx ()
      ((_ ?accessor-name ?field-offset ?foreign-type-accessor)
       (if (syntax->datum (syntax ?field-offset))
	   #'(define-syntax ?accessor-name
	       (syntax-rules ()
		 ((_ struct-pointer)
		  (?foreign-type-accessor struct-pointer ?field-offset))))
	 #'(define-syntax ?accessor-name
	     (syntax-rules ()
	       ((_ struct-pointer)
		(raise-unimplemented-error (quote ?accessor-name))))))))))

(define-syntax define-c-struct-mutator
  (lambda (use-stx)
    (syntax-case use-stx ()
      ((_ ?mutator-name ?field-offset ?foreign-type-mutator)
       (if (syntax->datum (syntax ?field-offset))
	   #'(define-syntax ?mutator-name
	       (syntax-rules ()
		 ((_ struct-pointer value)
		  (?foreign-type-mutator struct-pointer
					?field-offset
					value))))
	 #'(define-syntax ?mutator-name
	     (syntax-rules ()
	       ((_ struct-pointer value)
		(raise-unimplemented-error (quote ?mutator-name))))))))))

(define-syntax define-c-struct-field-pointer-accessor
  (lambda (use-stx)
    (syntax-case use-stx ()
      ((_ ?accessor-name ?field-offset)
       (if (syntax->datum (syntax ?field-offset))
	   #'(define-syntax ?accessor-name
	       (syntax-rules ()
		 ((_ struct-pointer)
		  (pointer-add struct-pointer ?field-offset))))
	 #'(define-syntax ?accessor-name
	     (syntax-rules ()
	       ((_ struct-pointer)
		(raise-unimplemented-error (quote ?accessor-name))))))))))


;;;; done

)

;;; end of file
