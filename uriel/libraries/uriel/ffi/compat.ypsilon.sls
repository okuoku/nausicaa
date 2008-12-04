;;;Copyright (c) 2004-2008 Yoshikatsu Fujita. All rights reserved.
;;;Copyright (c) 2004-2008 LittleWing Company Limited. All rights reserved.
;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
;;;
;;;Time-stamp: <2008-12-04 17:12:35 marco>
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


;;;; setup

(library (uriel ffi compat)
  (export

    ;;shared object loading
    shared-object primitive-open-shared-object self-shared-object

    ;;interface functions
    primitive-make-c-function primitive-make-c-function/with-errno

    ;;basic memory allocation
    primitive-free
    malloc primitive-malloc
    calloc primitive-calloc

    ;;pointers
    pointer? integer->pointer pointer->integer pointer-null?

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

    ;;basic string conversion
    strlen strerror
    string->cstring
    cstring->string cstring->string/len

    ;;conditions
    raise-errno-error)
  (import (core)
    (srfi receive)
    (srfi parameters)
    (uriel ffi sizeof)
    (uriel ffi errno)
    (uriel ffi conditions))



;;;; dynamic loading

(define self-shared-object (load-shared-object ""))

(define shared-object
  (make-parameter self-shared-object))

;;In case of error this raises an exception automatically.
(define primitive-open-shared-object load-shared-object)



;;;; value normalisation: scheme -> c language

;;;The following mapping functions  are normalisators from Scheme values
;;;to values usable by the C language interface functions.

(define (assert-int value)
  (if (and (integer? value)
	   (exact? value))
      value
    (assertion-violation 'assert-int
      "expected exact integer as function argument" value)))

(define (assert-float value)
  (if (flonum? value)
      (flonum->float value)
    (assertion-violation 'assert-float
      "expected flonum as function argument" value)))

(define (assert-double value)
  (if (flonum? value)
      value
    (assertion-violation 'assert-double
      "expected flonum as function argument" value)))

(define (assert-string value)
  (if (string? value)
      value
    (assertion-violation 'assert-string
      "expected string as function argument" value)))

(define (assert-bytevector value)
  (if (bytevector? value)
      value
    (assertion-violation 'assert-bytevector
      "expected bytevector as function argument" value)))

(define (assert-pointer p)
  (if (pointer? p)
      (pointer-value p)
    (assertion-violation 'assert-pointer
      "expected pointer as function argument" p)))

(define (assert-closure value)
  (if (procedure? value)
      value
    (assertion-violation 'assert-closure
      "expected procedure as function argument" value)))


;;;; values normalisation: Uriel -> Ypsilon

;;;This mapping function normalises  the C type identifiers supported by
;;;Uriel to the identifiers supported by Ypsilon.  Notice that currently
;;;(Ypsilon checkout  281) there is  neither support for "char"  nor for
;;;"long" in Ypsilon.
;;;
;;;Care  must  be  taken  in  selecting  types,  because:
;;;
;;;* selecting "void*" as Ypsilon  type will cause Ypsilon to allocate a
;;;  bytevector and use as value;
;;;
;;;* selecting "char*" as Ypsilon  type will cause Ypsilon to allocate a
;;;  string and use it as value.
;;;
(define (external->internal type)
  (case type
    ((void)
     'void)
    ((int
      signed-int ssize_t uint unsigned unsigned-int size_t
      long signed-long ulong unsigned-long)
     'int)
    ((double)
     'double)
    ((float)
     'float)
    ((pointer void* char* FILE*)
     'void*)
    ((callback)
     'void*)
    (else
     (assertion-violation 'make-c-function
       "unknown C language type identifier" type))))



;;;; pointers

(define-record-type pointer
  (fields (immutable value)))

(define (integer->pointer value)
  (unless (integer? value)
    (assertion-violation 'integer->pointer
      "expected integer value" value))
  (make-pointer value))

(define (pointer->integer pointer)
  (unless (pointer? pointer)
    (assertion-violation 'pointer->integer
      "expected pointer value" pointer))
  (pointer-value pointer))

(define (pointer-null? pointer)
  (= 0 (pointer-value pointer)))



;;;; interface functions

(define (select-cast-and-stub ret-type)
  (let ((identity (lambda (x) x)))
    (case ret-type
      ((void)
       (values identity stdcall-shared-object->void))
      ((int)
       (values identity stdcall-shared-object->int))
      ((double) ; float is not supported as return type
       (values identity stdcall-shared-object->double))
      ((void*)
       (values integer->pointer stdcall-shared-object->intptr))
      (else
       (assertion-violation 'make-c-callout
	 "unknown C language type identifier used for return value" ret-type)))))

(define (select-argument-mapper arg-type)
  (case (external->internal arg-type)
    ((void)
     (lambda (x) x))
    ((int)
     assert-int)
    ((float)
     assert-float)
    ((double)
     assert-double)
    ((callback)
     assert-closure)
    ((void*)
     assert-pointer)
    (else
     (assertion-violation 'make-c-callout
       "unknown C language type identifier used for function argument" arg-type))))

(define (primitive-make-c-function ret-type funcname arg-types)
  (receive (cast-func stub-func)
      (select-cast-and-stub (external->internal ret-type))
    (let ((f (lookup-shared-object (shared-object) funcname)))
      (unless f
	(error 'make-c-callout
	  "function not available in shared object" funcname))
      (when (equal? '(void) arg-types)
	(set! arg-types '()))
      (let* ((mappers (map select-argument-mapper arg-types)))
	(case (length mappers)
	  ((0)
	   (lambda ()
	     (cast-func (stub-func f 0))))
	  ((1)
	   (let ((mapper (car mappers)))
	     (lambda (arg)
	       (cast-func (stub-func f (mapper arg))))))
	  ((2)
	   (let ((mapper1 (car mappers))
		 (mapper2 (cadr mappers)))
	     (lambda (arg1 arg2)
	       (cast-func (stub-func f
				     (mapper1 arg1)
				     (mapper2 arg2))))))
	  ((3)
	   (let ((mapper1 (car mappers))
		 (mapper2 (cadr mappers))
		 (mapper3 (caddr mappers)))
	     (lambda (arg1 arg2 arg3)
	       (cast-func (stub-func f
				     (mapper1 arg1)
				     (mapper2 arg2)
				     (mapper3 arg3))))))
	  ((4)
	   (let ((mapper1 (car mappers))
		 (mapper2 (cadr mappers))
		 (mapper3 (caddr mappers))
		 (mapper4 (cadddr mappers)))
	     (lambda (arg1 arg2 arg3 arg4)
	       (cast-func (stub-func f
				     (mapper1 arg1)
				     (mapper2 arg2)
				     (mapper3 arg3)
				     (mapper4 arg4))))))
	  (else
	   (lambda args
	     (unless (= (length args) (length mappers))
	       (assertion-violation funcname
		 (format "wrong number of arguments, expected ~a" (length mappers))
		 args))
	     (cast-func (apply stub-func f
			       (map (lambda (m a)
				      (m a)) mappers args))))))))))

(define (primitive-make-c-function/with-errno ret-type funcname arg-types)
  (receive (cast-func stub-func)
      (select-cast-and-stub (external->internal ret-type))
    (let ((f (lookup-shared-object (shared-object) funcname)))
      (unless f
	(error 'make-c-callout
	  "function not available in shared object" funcname))
      (when (equal? '(void) arg-types)
	(set! arg-types '()))
      (let* ((mappers (map select-argument-mapper arg-types)))
	(case (length mappers)
	  ((0)
	   (lambda ()
	     (values (cast-func (stub-func f 0))
		     (shared-object-c-errno))))
	  ((1)
	   (let ((mapper (car mappers)))
	     (lambda (arg)
	       (values (cast-func (stub-func f (mapper arg)))
		       (shared-object-c-errno)))))
	  ((2)
	   (let ((mapper1 (car mappers))
		 (mapper2 (cadr mappers)))
	     (lambda (arg1 arg2)
	       (values (cast-func (stub-func f
					     (mapper1 arg1)
					     (mapper2 arg2)))
		       (shared-object-c-errno)))))
	  ((3)
	   (let ((mapper1 (car mappers))
		 (mapper2 (cadr mappers))
		 (mapper3 (caddr mappers)))
	     (lambda (arg1 arg2 arg3)
	       (values (cast-func (stub-func f
					     (mapper1 arg1)
					     (mapper2 arg2)
					     (mapper3 arg3)))
		       (shared-object-c-errno)))))
	  ((4)
	   (let ((mapper1 (car mappers))
		 (mapper2 (cadr mappers))
		 (mapper3 (caddr mappers))
		 (mapper4 (cadddr mappers)))
	     (lambda (arg1 arg2 arg3 arg4)
	       (values (cast-func (stub-func f
					     (mapper1 arg1)
					     (mapper2 arg2)
					     (mapper3 arg3)
					     (mapper4 arg4)))
		       (shared-object-c-errno)))))
	  (else
	   (lambda args
	     (unless (= (length args) (length mappers))
	       (assertion-violation funcname
		 (format "wrong number of arguments, expected ~a" (length mappers))
		 args))
	     (values (cast-func (apply stub-func f
				       (map (lambda (m a)
					      (m a)) mappers args)))
		     (shared-object-c-errno)))))))))



;;;; memory allocation

(define primitive-free
  (primitive-make-c-function 'void 'free '(void*)))

(define primitive-malloc
  (let ((malloc (primitive-make-c-function 'void* 'malloc '(size_t))))
    (lambda (number-of-bytes)
      (let ((p (malloc number-of-bytes)))
	(if (pointer-null? p) #f p)))))

(define (malloc number-of-bytes)
  (or (primitive-malloc number-of-bytes)
      (raise-out-of-memory 'malloc number-of-bytes)))

(define primitive-calloc
  (let ((calloc (primitive-make-c-function 'void* 'calloc '(size_t))))
    (lambda (count element-size)
      (let ((p (calloc count element-size)))
	(if (pointer-null? p)
	    #f
	  p)))))

(define (calloc count element-size)
  (or (primitive-calloc count element-size)
      (raise-out-of-memory 'calloc (list count element-size))))



;;;; pokers and peekers

(define (pointer-ref-c-signed-char pointer position)
  (bytevector-s8-ref
   (make-bytevector-mapping (pointer-value pointer)
			    (+ 1 position))
   position))

(define (pointer-ref-c-unsigned-char pointer position)
  (bytevector-u8-ref
   (make-bytevector-mapping (pointer-value pointer)
			    (+ 1 position))
   position))

;;; --------------------------------------------------------------------

(define (pointer-ref-c-signed-short pointer position)
  (bytevector-s16-ref (make-bytevector-mapping (pointer-value pointer)
					       (+ 2 position))
		      position (native-endianness)))

(define (pointer-ref-c-unsigned-short pointer position)
  (bytevector-u16-ref (make-bytevector-mapping (pointer-value pointer)
					       (+ 2 position))
		      position (native-endianness)))

;;; --------------------------------------------------------------------

(define (pointer-ref-c-signed-int pointer position)
  (bytevector-s32-ref (make-bytevector-mapping (pointer-value pointer)
					       (+ 4 position))
		      position (native-endianness)))

(define (pointer-ref-c-unsigned-int pointer position)
  (bytevector-u32-ref (make-bytevector-mapping (pointer-value pointer)
					       (+ 4 position))
		      position (native-endianness)))

;;; --------------------------------------------------------------------

(define (pointer-ref-c-signed-long pointer position)
  (if on-32-bits-system
      (bytevector-s32-ref (make-bytevector-mapping (pointer-value pointer)
						   (+ 4 position))
			  position (native-endianness))
    (bytevector-s64-ref (make-bytevector-mapping (pointer-value pointer)
						 (+ 8 position))
			position (native-endianness))))

(define (pointer-ref-c-unsigned-long pointer position)
  (if on-32-bits-system
      (bytevector-u32-ref (make-bytevector-mapping (pointer-value pointer)
						   (+ 4 position))
			  position (native-endianness))
    (bytevector-u64-ref (make-bytevector-mapping (pointer-value pointer)
						 (+ 8 position))
			position (native-endianness))))

;;; --------------------------------------------------------------------

(define (pointer-ref-c-signed-long-long pointer position)
  (bytevector-s64-ref (make-bytevector-mapping (pointer-value pointer)
					       (+ 8 position))
		      position (native-endianness)))

(define (pointer-ref-c-unsigned-long-long pointer position)
  (bytevector-u64-ref (make-bytevector-mapping (pointer-value pointer)
					       (+ 8 position))
		      position (native-endianness)))

;;; --------------------------------------------------------------------

(define (pointer-ref-c-float pointer position)
  (bytevector-ieee-single-ref (make-bytevector-mapping (pointer-value pointer)
						       (+ 4 position))
			      position (native-endianness)))

(define (pointer-ref-c-double pointer position)
  (bytevector-ieee-double-ref (make-bytevector-mapping (pointer-value pointer)
						       (+ 8 position))
			      position (native-endianness)))

;;; --------------------------------------------------------------------

(define (pointer-ref-c-pointer pointer position)
  (cond ((= 4 sizeof-pointer)
	 (make-pointer (pointer-ref-c-unsigned-int pointer position)))
	((= 8 sizeof-pointer)
	 (make-pointer (pointer-ref-c-unsigned-long-long pointer position)))
	(else
	 (assertion-violation 'pointer-ref-c-pointer
	   "cannot determine size of pointers for peeker function"))))

;;; --------------------------------------------------------------------

(define (pointer-set-c-char! pointer position value)
  (bytevector-u8-set! (make-bytevector-mapping (pointer-value pointer)
					       (+ 1 position))
		      position value))

(define (pointer-set-c-short! pointer position value)
  (bytevector-u16-set! (make-bytevector-mapping (pointer-value pointer)
						(+ 4 position))
		       position value (native-endianness)))

(define (pointer-set-c-int! pointer position value)
  (bytevector-s32-set! (make-bytevector-mapping (pointer-value pointer)
						(+ 4 position))
		       position value (native-endianness)))

(define (pointer-set-c-long! pointer position value)
  (if on-32-bits-system
      (bytevector-s32-set! (make-bytevector-mapping (pointer-value pointer)
						    (+ 4 position))
			   position value (native-endianness))
    (bytevector-s64-set! (make-bytevector-mapping (pointer-value pointer)
						  (+ 8 position))
			 position value (native-endianness))))

(define (pointer-set-c-long-long! pointer position value)
  (bytevector-s64-set! (make-bytevector-mapping (pointer-value pointer)
						(+ 8 position))
		       position value (native-endianness)))

;;; --------------------------------------------------------------------

(define (pointer-set-c-float! pointer position value)
  (bytevector-ieee-single-set!
   (make-bytevector-mapping (pointer-value pointer)
			    (+ 4 position))
   position value (native-endianness)))

(define (pointer-set-c-double! pointer position value)
  (bytevector-ieee-double-set!
   (make-bytevector-mapping (pointer-value pointer)
			    (+ 8 position))
   position value (native-endianness)))

;;; --------------------------------------------------------------------

(define (pointer-set-c-pointer! pointer position the-pointer)
  (cond ((= 4 sizeof-pointer)
	 (bytevector-u32-set! (make-bytevector-mapping (pointer-value pointer)
						       (+ 4 position))
			      position (pointer-value the-pointer)
			      (native-endianness)))
	((= 8 sizeof-pointer)
	 (bytevector-u64-set! (make-bytevector-mapping (pointer-value pointer)
						       (+ 8 position))
			      position (pointer-value the-pointer)
			      (native-endianness)))
	(else
	 (assertion-violation 'pointer-set-c-pointer
	   "cannot determine size of pointers for peeker function"))))



;;;; string functions

(define strlen
  (primitive-make-c-function 'size_t 'strlen '(char*)))

(define primitive-strerror
  (primitive-make-c-function 'char* 'strerror '(int)))

(define (cstring->string pointer)
  (let* ((len	(strlen pointer))
	 (bv	(make-bytevector len)))
    (do ((i 0 (+ 1 i)))
	((= i len)
	 (utf8->string bv))
      (bytevector-s8-set! bv i (pointer-ref-c-signed-char pointer i)))))

(define (cstring->string/len pointer len)
  (let* ((bv	(make-bytevector len)))
    (do ((i 0 (+ 1 i)))
	((= i len)
	 (utf8->string bv))
      (bytevector-s8-set! bv i (pointer-ref-c-signed-char pointer i)))))

(define (string->cstring s)
  (let* ((len		(string-length s))
	 (pointer	(malloc (+ 1 len)))
	 (bv		(string->utf8 s)))
    (do ((i 0 (+ 1 i)))
	((= i len)
	 (pointer-set-c-char! pointer i 0);set to zero the last byte
	 pointer)
      (pointer-set-c-char! pointer i (bytevector-s8-ref bv i)))))

(define (strerror code)
  (cstring->string (primitive-strerror code)))



;;;; conditions

;;This is not in "conditions.sls" because it requires STRERROR.
(define (raise-errno-error who errno irritants)
  (raise (condition (make-who-condition who)
		    (make-message-condition (strerror errno))
		    (make-errno-condition errno)
		    (make-irritants-condition irritants))))



;;;; done

)

;;; end of file
