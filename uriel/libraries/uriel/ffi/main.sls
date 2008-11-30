;;;
;;;Part of: Uriel libraries for R6RS Scheme
;;;Contents: foreign function interface extensions
;;;Date: Tue Nov 18, 2008
;;;Time-stamp: <2008-11-30 17:49:12 marco>
;;;
;;;Abstract
;;;
;;;
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
    malloc primitive-malloc primitive-free
    make-block-cache small-blocks-cache make-caching-object-factory
    compensate-malloc compensate-malloc/small

    ;;out of memory condition
    &out-of-memory make-out-of-memory-condition out-of-memory-condition?
    out-of-memory-requested-number-of-bytes

    ;;string functions
    strlen string->cstring cstring->string

    ;;peekers
    pointer-ref-c-signed-char		pointer-ref-c-unsigned-char
    pointer-ref-c-signed-short		pointer-ref-c-unsigned-short
    pointer-ref-c-signed-int		pointer-ref-c-unsigned-int
    pointer-ref-c-signed-long		pointer-ref-c-unsigned-long
    pointer-ref-c-float			pointer-ref-c-double
    pointer-ref-c-pointer

    ;;pokers
    pointer-set-c-char!			pointer-set-c-short!
    pointer-set-c-int!			pointer-set-c-long!
    pointer-set-c-float!		pointer-set-c-double!
    pointer-set-c-pointer!

    ;;pointers
    integer->pointer pointer->integer pointer?)

  (import (rnrs)
    (uriel lang)
    (uriel define-macro)
    (uriel printing)
    (uriel ffi compat)
    (uriel define-macro))



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
	long ulong unsigned-long float double
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
	long ulong unsigned-long float double
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



;;;; basic memory functions

(define-condition-type &out-of-memory &error
  make-out-of-memory-condition out-of-memory-condition?
  (number-of-bytes out-of-memory-requested-number-of-bytes))

(define (malloc size)
  (let ((p (primitive-malloc size)))
    (unless p
      (raise (make-who-condition 'malloc)
	     (make-message-condition "out of memory")
	     (make-out-of-memory-condition size)))
    p))



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
	  (malloc block-size)
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

(define small-blocks-cache (make-block-cache 32 10))

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



;;;; compensations

(define (compensate-malloc number-of-bytes)
  (letrec
      ((p (compensate
	      (malloc number-of-bytes)
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



;;;; done

)

;;; end of file
