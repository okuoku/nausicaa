;;;
;;;Part of: Uriel libraries for R6RS Scheme
;;;Contents: foreign function interface extensions
;;;Date: Tue Nov 18, 2008
;;;Time-stamp: <2008-12-11 18:24:01 marco>
;;;
;;;Abstract
;;;
;;;	This is the core of the "(uriel ffi)" library.
;;;
;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
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


;;;; Setup.

(library (uriel ffi)
  (export

    ;;interface functions
    shared-object open-shared-object self-shared-object
    primitive-make-c-function primitive-make-c-function/with-errno
    make-c-function make-c-function/with-errno
    define-c-function define-c-function/with-errno

    ;;memory functions
    primitive-free
    primitive-malloc malloc
    primitive-calloc calloc
    make-block-cache make-caching-object-factory
    small-blocks-cache page-blocks-cache
    compensate-malloc compensate-calloc
    compensate-malloc/small compensate-malloc/page
    memset memmove memcpy

    ;;memory blocks
    make-memory-block-record memory-block-record?
    memory-block-pointer memory-block-size
    memory-block compensate-malloc/block

    ;;conditions: out of memory
    &out-of-memory make-out-of-memory-condition out-of-memory-condition?
    out-of-memory-requested-number-of-bytes
    raise-out-of-memory

    ;;conditions: errno error
    &errno make-errno-condition errno-condition?
    errno-numeric-value errno-symbolic-value
    raise-errno-error

    ;;string functions
    strlen cstring->string cstring->string/len strerror
    string->cstring string->cstring/compensated
    string-or-symbol->cstring
    string-or-symbol->cstring/compensated

    ;;peekers
    pointer-ref-c-signed-char		pointer-ref-c-unsigned-char
    pointer-ref-c-signed-short		pointer-ref-c-unsigned-short
    pointer-ref-c-signed-int		pointer-ref-c-unsigned-int
    pointer-ref-c-signed-long		pointer-ref-c-unsigned-long
    pointer-ref-c-signed-long-long	pointer-ref-c-unsigned-long-long
    pointer-ref-c-float			pointer-ref-c-double
    pointer-ref-c-pointer

    ;;pokers
    pointer-set-c-char!			pointer-set-c-short!
    pointer-set-c-int!			pointer-set-c-long!
    pointer-set-c-long-long!		pointer-set-c-float!
    pointer-set-c-double!		pointer-set-c-pointer!

    ;;pointers
    pointer? integer->pointer pointer->integer pointer-null?
    pointer-diff pointer-add pointer-null

    ;;bytevector functions
    bytevector->pointer bytevector->memory-block
    pointer->bytevector memory-block->bytevector

    ;;foreign struct accessors
    define-c-struct-accessors)

  (import (rnrs)
    (uriel lang)
    (uriel printing)
    (uriel ffi compat)
    (uriel ffi conditions))



;;;; dynamic loading

(define (open-shared-object library-name)
  (primitive-open-shared-object (if (symbol? library-name)
				    (symbol->string library-name)
				  library-name)))



;;;; function interface

(define-syntax make-c-function
  (lambda (use-stx)
    (define list-of-types
      '(void
	char schar signed-char uchar unsigned-char
	int signed-int ssize_t uint unsigned unsigned-int size_t
	long signed-long ulong unsigned-long float double
	pointer void* char* FILE* callback))

    (define (quote-if-predefined-type arg-stx)
      (if (memq (syntax->datum arg-stx) list-of-types)
	  (list (syntax quote) arg-stx)
	arg-stx))

    (syntax-case use-stx ()
      ((_ ?ret-type ?funcname (?arg-type0 ?arg-type ...))
       (with-syntax
	   ((ret	(quote-if-predefined-type (syntax ?ret-type)))
	    (args	(cons (syntax list)
			      (map quote-if-predefined-type
				(syntax (?arg-type0 ?arg-type ...))))))
	 (syntax
	  (primitive-make-c-function ret '?funcname args)))))))

(define-syntax make-c-function/with-errno
  (lambda (use-stx)
    (define list-of-types
      '(void
	char schar signed-char uchar unsigned-char
	int signed-int ssize_t uint unsigned unsigned-int size_t
	long signed-long ulong unsigned-long float double
	pointer void* char* FILE* callback))

    (define (quote-if-predefined-type arg-stx)
      (if (memq (syntax->datum arg-stx) list-of-types)
	  (list (syntax quote) arg-stx)
	arg-stx))

    (syntax-case use-stx ()
      ((_ ?ret-type ?funcname (?arg-type0 ?arg-type ...))
       (with-syntax
	   ((ret	(quote-if-predefined-type (syntax ?ret-type)))
	    (args	(cons (syntax list)
			      (map quote-if-predefined-type
				(syntax (?arg-type0 ?arg-type ...))))))
	 (syntax
	  (primitive-make-c-function/with-errno ret '?funcname args)))))))

(define-syntax define-c-function
  (syntax-rules ()
    ((_ ?name (?ret-type ?funcname (?arg-type0 ?arg-type ...)))
     (define ?name
       (make-c-function
	?ret-type ?funcname (?arg-type0 ?arg-type ...))))))

(define-syntax define-c-function/with-errno
  (syntax-rules ()
    ((_ ?name (?ret-type ?funcname (?arg-type0 ?arg-type ...)))
     (define ?name
       (make-c-function/with-errno
	?ret-type ?funcname (?arg-type0 ?arg-type ...))))))



;;;; memory functions

(define (pointer-diff pointer-1 pointer-2)
  (- (pointer->integer pointer-1)
     (pointer->integer pointer-2)))

(define (pointer-add pointer offset)
  (integer->pointer (+ (pointer->integer pointer)
		       offset)))

(define pointer-null
  (integer->pointer 0))

(define-c-function memset
  (void* memset (void* int size_t)))

(define-c-function memmove
  (void* memmove (void* void* size_t)))

(define-c-function memcpy
  (void* memcpy (void* void* size_t)))



;;;; caching

(define (make-block-cache block-size max-cached-block-number)
  (unless (and (fixnum? block-size)
	       (< 0 block-size))
    (assertion-violation 'make-block-cache
      "expected strictly positive fixnum as size of cached memory blocks"
      block-size))
  (unless (and (fixnum? max-cached-block-number)
	       (< 0 max-cached-block-number))
    (assertion-violation 'make-block-cache
      "expected strictly positive fixnum as max number of cached memory blocks"
      max-cached-block-number))
  (let ((list-of-cached-blocks '())
	(number-of-cached-blocks 0))
    (case-lambda
     (()
      (if (null? list-of-cached-blocks)
	  (calloc 1 block-size)
	(let ((pointer (car list-of-cached-blocks)))
	  (set! list-of-cached-blocks (cdr list-of-cached-blocks))
	  (set! number-of-cached-blocks (- number-of-cached-blocks 1))
	  pointer)))
     ((pointer)
      (case pointer
	((purge)
	 (map primitive-free list-of-cached-blocks)
	 (set! list-of-cached-blocks '())
	 (set! number-of-cached-blocks 0))
	((list)
	 list-of-cached-blocks)
	(else
	 (if (< number-of-cached-blocks max-cached-block-number)
	     (begin
	       (set! list-of-cached-blocks (cons pointer list-of-cached-blocks))
	       (set! number-of-cached-blocks (+ 1 number-of-cached-blocks)))
	   (primitive-free pointer))))))))

(define (make-caching-object-factory init-func final-func
				     block-size max-cached-block-number)
  (let ((block-cache (make-block-cache block-size max-cached-block-number)))
    (case-lambda
     (()
      (let ((pointer (block-cache)))
	(init-func pointer)
	pointer))
     ((pointer)
      (case pointer
	((purge)
	 ;;Notice that  here we have ALREADY applied  the final function
	 ;;to all the stuff in the cache.
	 (block-cache 'purge))
	(else
	 (final-func pointer)
	 (block-cache pointer)))))))

(define small-blocks-size 32)
(define small-blocks-cache (make-caching-object-factory
			    (lambda (x) x)
			    (lambda (p) (memset p 0 small-blocks-size))
			    small-blocks-size 10))

(define page-blocks-size 4096)
(define page-blocks-cache (make-block-cache page-blocks-size 10))



;;;; special allocators

(define-record-type memory-block-record
  (fields (immutable pointer memory-block-pointer)
	  (immutable size memory-block-size)))


(define (memory-block block-or-size)
  (if (memory-block-record? block-or-size)
      (let ((size	(memory-block-size block-or-size))
	    (pointer	(memory-block-pointer block-or-size)))
	(cond
	 ((<= block-or-size small-blocks-size)
	  (small-blocks-cache pointer))
	 ((<= block-or-size page-blocks-size)
	  (page-blocks-cache pointer))
	 (else
	  (primitive-free pointer))))
    (cond
     ((<= block-or-size small-blocks-size)
      (make-memory-block-record (small-blocks-cache) small-blocks-size))
     ((<= block-or-size page-blocks-size)
      (make-memory-block-record (page-blocks-cache) page-blocks-size))
     (else
      (make-memory-block-record (malloc block-or-size) block-or-size)))))



;;;; compensations

(define (compensate-malloc number-of-bytes)
  (letrec
      ((p (compensate
	      (malloc number-of-bytes)
	    (with
	     (primitive-free p)))))
    p))

(define (compensate-calloc count element-size)
  (letrec
      ((p (compensate
	      (calloc count element-size)
	    (with
	     (primitive-free p)))))
    p))

(define (compensate-malloc/small)
  (letrec
      ((p (compensate
	      (small-blocks-cache)
	    (with
	     (small-blocks-cache p)))))
    p))

(define (compensate-malloc/page)
  (letrec
      ((p (compensate
	      (page-blocks-cache)
	    (with
	     (page-blocks-cache p)))))
    p))

(define (compensate-malloc/block number-of-bytes)
  (letrec
      ((b (compensate
	      (memory-block number-of-bytes)
	    (with
	     (memory-block b)))))
    (memory-block-pointer b)))



;;;; string functions

(define (string->cstring/compensated s)
  (let* ((len		(string-length s))
	 (alloc-len	(+ 1 len))
	 (pointer	(compensate-malloc/block alloc-len))
	 (bv		(string->utf8 s)))
    (pointer-set-c-char! pointer len 0)
    (do ((i 0 (+ 1 i)))
	((= i len)
	 pointer)
      (pointer-set-c-char! pointer i (bytevector-s8-ref bv i)))))

(define (string-or-symbol->cstring/compensated s)
  (string->cstring/compensated (symbol->string/maybe s)))

(define (string-or-symbol->cstring s)
  (string->cstring (symbol->string/maybe s)))



;;;; bytevector functions

(define bytevector->pointer
  (case-lambda
   ((bv malloc)
    (bytevector->pointer bv malloc (bytevector-length bv) 0))
   ((bv malloc number-of-bytes)
    (bytevector->pointer bv malloc number-of-bytes 0))
   ((bv malloc number-of-bytes offset)
    (let* ((p	(malloc number-of-bytes)))
      (do ((i 0 (+ 1 i)))
	  ((= i number-of-bytes)
	   p)
	(pointer-set-c-char! p i (bytevector-u8-ref bv (+ i offset))))))))

(define bytevector->memory-block
  (case-lambda
   ((bv malloc)
    (bytevector->memory-block bv malloc (bytevector-length bv) 0))
   ((bv malloc number-of-bytes)
    (bytevector->memory-block bv malloc number-of-bytes 0))
   ((bv malloc number-of-bytes offset)
    (make-memory-block-record
     (bytevector->pointer bv malloc number-of-bytes offset)
     number-of-bytes))))

;;; --------------------------------------------------------------------

(define pointer->bytevector
  (case-lambda
   ((pointer number-of-bytes)
    (pointer->bytevector pointer number-of-bytes 0))
   ((pointer number-of-bytes offset)
    (let* ((bv (make-bytevector number-of-bytes)))
      (do ((i 0 (+ 1 i)))
	  ((= i number-of-bytes)
	   bv)
	(bytevector-u8-set! bv i (pointer-ref-c-unsigned-char pointer (+ i offset))))))))

(define memory-block->bytevector
  (case-lambda
   ((mb)
    (memory-block->bytevector mb (memory-block-size mb) 0))
   ((mb number-of-bytes)
    (memory-block->bytevector mb number-of-bytes 0))
   ((mb number-of-bytes offset)
    (pointer->bytevector (memory-block-pointer mb) number-of-bytes offset))))



;;;; foreign structures accessors

(define-syntax define-c-struct-accessors
  (syntax-rules ()
    ((_ ?setter-name ?getter-name ?field-offset ?foreign-type-setter ?foreign-type-getter)
     (begin
       (define-syntax ?setter-name
	 (syntax-rules ()
	   ((_ struct-pointer value)
	    (?foreign-type-setter struct-pointer ?field-offset value))))
       (define-syntax ?getter-name
	 (syntax-rules ()
	   ((_ struct-pointer)
	    (?foreign-type-getter struct-pointer ?field-offset))))))))




;;;; done

)

;;; end of file
