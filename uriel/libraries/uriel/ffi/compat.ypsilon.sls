;;;Copyright (c) 2004-2008 Yoshikatsu Fujita. All rights reserved.
;;;Copyright (c) 2004-2008 LittleWing Company Limited. All rights reserved.
;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
;;;
;;;Time-stamp: <2008-12-17 18:38:55 marco>
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
    primitive-make-c-function primitive-make-c-function/with-errno)
  (import (core)
    (srfi receive)
    (uriel ffi sizeof)
    (uriel memory))


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
      (pointer->integer p)
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
		     (shared-object-errno))))
	  ((1)
	   (let ((mapper (car mappers)))
	     (lambda (arg)
	       (values (cast-func (stub-func f (mapper arg)))
		       (shared-object-errno)))))
	  ((2)
	   (let ((mapper1 (car mappers))
		 (mapper2 (cadr mappers)))
	     (lambda (arg1 arg2)
	       (values (cast-func (stub-func f
					     (mapper1 arg1)
					     (mapper2 arg2)))
		       (shared-object-errno)))))
	  ((3)
	   (let ((mapper1 (car mappers))
		 (mapper2 (cadr mappers))
		 (mapper3 (caddr mappers)))
	     (lambda (arg1 arg2 arg3)
	       (values (cast-func (stub-func f
					     (mapper1 arg1)
					     (mapper2 arg2)
					     (mapper3 arg3)))
		       (shared-object-errno)))))
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
		       (shared-object-errno)))))
	  (else
	   (lambda args
	     (unless (= (length args) (length mappers))
	       (assertion-violation funcname
		 (format "wrong number of arguments, expected ~a" (length mappers))
		 args))
	     (values (cast-func (apply stub-func f
				       (map (lambda (m a)
					      (m a)) mappers args)))
		     (shared-object-errno)))))))))



;;;; done

)

;;; end of file
