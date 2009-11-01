;;;Copyright (c) 2008, 2009 Marco Maggi <marcomaggi@gna.org>
;;;
;;;Redistribution and  use in source  and binary forms, with  or without
;;;modification,  are permitted provided  that the  following conditions
;;;are met:
;;;
;;;1. Redistributions  of source  code must  retain the  above copyright
;;;   notice, this list of conditions and the following disclaimer.
;;;
;;;2. Redistributions in binary form  must reproduce the above copyright
;;;   notice, this  list of conditions  and the following  disclaimer in
;;;   the  documentation  and/or   other  materials  provided  with  the
;;;   distribution.
;;;
;;;3. Neither the name of the  authors nor the names of its contributors
;;;   may  be used  to endorse  or  promote products  derived from  this
;;;   software without specific prior written permission.
;;;
;;;THIS SOFTWARE  IS PROVIDED BY THE COPYRIGHT  HOLDERS AND CONTRIBUTORS
;;;"AS IS"  AND ANY  EXPRESS OR IMPLIED  WARRANTIES, INCLUDING,  BUT NOT
;;;LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;;;OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;SPECIAL,  EXEMPLARY,  OR CONSEQUENTIAL  DAMAGES  (INCLUDING, BUT  NOT
;;;LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;;DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;;THEORY OF  LIABILITY, WHETHER IN CONTRACT, STRICT  LIABILITY, OR TORT
;;;(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;;OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


(library (foreign ffi platform)
  (export
    self-shared-object
    open-shared-object		open-shared-object*
    lookup-shared-object	lookup-shared-object*
    make-c-function		make-c-function/with-errno
    pointer->c-function		pointer->c-function/with-errno
    make-c-callback		free-c-callback
    internal-type->implementation-type
    (rename (internal-type->implementation-type internal-type->implementation-type/callout))
    implementation-data-types)
  (import (rnrs)
    (foreign ffi conditions)
    (prefix (primitives ffi/dlopen ffi/dlsym
			foreign-file foreign-procedure
			get-errno set-errno!)
	    larceny:)
    (only (foreign ffi pointers)
	  pointer-null integer->pointer)
    (only (unimplemented)
	  raise-unimplemented-error))


;;;; helpers

(define (%normalise-arg-types arg-types)
  (if (equal? '(void) arg-types)
      '()
    arg-types))

(define (%normalise-foreign-symbol foreign-symbol)
  (if (symbol? foreign-symbol)
      (symbol->string foreign-symbol)
    foreign-symbol))

(define errno
  (case-lambda
   ((value)
    (larceny:set-errno! value))
   (()
    (larceny:get-errno))))


;;;; dynamic loading

;;In case of error this raises an exception automatically.
(define (open-shared-object library-name)
  (let ((library-name (%normalise-foreign-symbol library-name)))
    (larceny:foreign-file library-name)
    (larceny:ffi/dlopen library-name)))

(define (open-shared-object* library-name)
  (let* ((library-name	(%normalise-foreign-symbol library-name))
	 (lib-ref	(open-shared-object library-name)))
    (or lib-ref
	(raise-unknown-shared-object library-name 'open-shared-object*
				     "unable to open shared object"))))

(define self-shared-object
  (larceny:ffi/dlopen ""))

;;; --------------------------------------------------------------------

(define (lookup-shared-object lib-spec foreign-symbol)
  ;;This already returns #f when the symbol is not found.
  (let ((address (larceny:ffi/dlsym lib-spec (%normalise-foreign-symbol foreign-symbol))))
    (and address (integer->pointer address))))

(define (lookup-shared-object* lib-spec foreign-symbol)
  (let* ((foreign-symbol	(%normalise-foreign-symbol foreign-symbol))
	 (ptr			(lookup-shared-object lib-spec foreign-symbol)))
    (or ptr
	(raise-unknown-foreign-symbol lib-spec foreign-symbol
				      'lookup-shared-object*
				      "could not find foreign symbol in foreign library"))))


;;;; values normalisation
;;
;;Larceny revision  6404 supports the following data  types for function
;;arguments and return values:
;;
;;  void
;;  int		unsigned
;;  long	unsigned-long
;;  float	double
;;  void*	(maybe void*)
;;
;;The ugly "(maybe void*)" represents a pointer which can be NULL.  When
;;not NULL, it is a record of type void*-rt; when NULL it is #f.
;;

(define implementation-data-types
  (make-enumeration '(void
		      int	unsigned
		      long	unsigned-long
		      float	double
		      void*)))

(define (internal-type->implementation-type type)
  (case type
    ((signed-int)			'int)
    ((unsigned-int)			'unsigned)
    ((signed-long)			'long)
    ((unsigned-long)			'ulong)
    ((float)				'float)
    ((double)				'double)
    ((pointer)				'(maybe void*))
    ((callback)				'(maybe void*))
    ((void)				'void)
    (else
     (assertion-violation #f
       "C language type identifier is unknown by Larceny" type))))


(define (make-c-function lib-spec ret-type funcname arg-types)
  (let ((callout-closure (larceny:foreign-procedure funcname
						    (%normalise-arg-types arg-types)
						    ret-type)))
    ;;When the  return value is a  pointer: if the pointer  is NULL, the
    ;;return value  is #f.   So we have  to convert it  to POINTER-NULL.
    ;;Ugly but what can I do?
    (if (equal? ret-type '(maybe void*))
	(lambda args
	  (or (apply callout-closure args) pointer-null))
      callout-closure)))

(define (make-c-function/with-errno lib-spec ret-type funcname arg-types)
  (let ((f (make-c-function lib-spec ret-type funcname arg-types)))
    (lambda args
      ;;We have to use LET* here  to enforce the order of evaluation: we
      ;;want  to gather  the "errno"  value AFTER  the  foreign function
      ;;call.
      (let* ((retval	(begin
			  (errno 0)
			  (apply f args)))
	     (errval	(errno)))
	(values retval errval)))))

(define (pointer->c-function ret-type address arg-types)
  (raise-unimplemented-error 'pointer->c-function))

(define (pointer->c-function/with-errno ret-type address arg-types)
  (let ((closure (pointer->c-function ret-type address arg-types)))
    (lambda args
      ;;We have to use LET* here  to enforce the order of evaluation: we
      ;;want  to gather  the "errno"  value AFTER  the  foreign function
      ;;call.
      (let* ((retval	(begin
			  (errno 0)
			  (apply closure args)))
	     (errval	(errno)))
	(values retval errval)))))


(define (make-c-callback ret-type scheme-function arg-types)
  (raise-unimplemented-error 'make-c-callback
			     "callbacks are not implemented for Larceny"))
  ;; (ffi/make-callback 'i386 scheme-function
  ;; 		     (map nausicaa-type->larceny-type arg-types)
  ;; 		     (nausicaa-type->larceny-type ret-type))

(define (free-c-callback cb)
  #f)


;;;; done

)

;;; end of file
