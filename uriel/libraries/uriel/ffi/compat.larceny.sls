;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
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
  (import (rnrs)
    (primitives foreign-file foreign-procedure)
    (uriel lang)
    (uriel ffi sizeof)
    (uriel memory))


;;;; dynamic loading

(define self-shared-object (foreign-file ""))

(define shared-object
  (make-parameter self-shared-object))

;;In case of error this raises an exception automatically.
(define primitive-open-shared-object foreign-file)



;;;; value normalisation: scheme -> c language

;;;The following mapping functions  are normalisators from Scheme values
;;;to values usable by the C language interface functions.

(define (assert-int value)
  (if (and (integer? value)
	   (exact? value))
      value
    (assertion-violation 'assert-int
      "expected exact integer as function argument" value)))

(define (assert-unsigned value)
  (if (and (integer? value)
	   (exact? value)
	   (<= 0 value))
      value
    (assertion-violation 'assert-unsigned
      "expected non-negative exact integer as function argument" value)))

(define (assert-float value)
  (if (flonum? value)
      value
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


;;;; values normalisation: Uriel -> Larceny

(define larceny-pointer-integer
  (case pointer-integer
    ((unsigned-long)	'ulong)
    ((unsigned-int)	'unsigned)))

(define (external->internal type)
  (case type
    ((void)
     'void)
    ((int signed-int ssize_t)
     'int)
    ((uint unsigned unsigned-int size_t)
     'unsigned)
    ((long signed-long)
     'long)
    ((ulong unsigned-long)
     'ulong)
    ((float)
     'float)
    ((double)
     'double)
    ((pointer void* char* FILE*)
     larceny-pointer-integer)
    ((callback)
     larceny-pointer-integer)
    (else
     (assertion-violation 'make-c-function
       "unknown C language type identifier" type))))


;;;; interface functions

(define (select-cast-for-return-value return-value-type
				      external-return-value-type)
  (if (memq external-return-value-type '(pointer void* char* FILE*))
      integer->pointer
    (let ((identity (lambda (x) x)))
      (case return-value-type
	((void int unsigned long ulong float double)
	 identity)
	(else
	 (assertion-violation 'make-c-callout
	   "unknown C language type identifier used for return value"
	   return-value-type))))))

(define (select-cast-for-argument argument-type
				  external-argument-type)
  (cond
   ((memq external-argument-type '(pointer void* char* FILE*))
    assert-pointer)
   ((eq? external-argument-type 'callback)
    assert-closure)
   (else
    (let ((identity (lambda (x) x)))
      (case (external->internal argument-type)
	((void)
	 identity)
	((int long)
	 assert-int)
	((unsigned ulong)
	 assert-unsigned)
	((float)
	 assert-float)
	((double)
	 assert-double)
	(else
	 (assertion-violation 'make-c-callout
	   "unknown C language type identifier used for function argument"
	   argument-type)))))))

(define (primitive-make-c-function ext-return-value-type funcname ext-argument-types)
  (let* ((argument-types	(if (equal? '(void) ext-argument-types)
				    '()
				  (map external->internal ext-argument-types)))
	 (return-value-type	(external->internal ext-return-value-type))
	 (return-value-cast	(select-cast-for-return-value return-value-type
							      ext-return-value-type))
	 (f			(foreign-procedure (symbol->string/maybe funcname)
						   argument-types
						   return-value-type))
	 (argument-casts	(map select-cast-for-argument
				  argument-types
				  ext-argument-types)))
    (case (length argument-casts)
      ((0)
       (lambda ()
	 (return-value-cast (f))))
      ((1)
       (let ((cast (car argument-casts)))
	 (lambda (arg)
	   (return-value-cast (f (cast arg))))))
      ((2)
       (let ((cast1 (car argument-casts))
	     (cast2 (cadr argument-casts)))
	 (lambda (arg1 arg2)
	   (return-value-cast (f (cast1 arg1)
				 (cast2 arg2))))))
      ((3)
       (let ((cast1 (car argument-casts))
	     (cast2 (cadr argument-casts))
	     (cast3 (caddr argument-casts)))
	 (lambda (arg1 arg2 arg3)
	   (return-value-cast (f (cast1 arg1)
				 (cast2 arg2)
				 (cast3 arg3))))))
      ((4)
       (let ((cast1 (car argument-casts))
	     (cast2 (cadr argument-casts))
	     (cast3 (caddr argument-casts))
	     (cast4 (cadddr argument-casts)))
	 (lambda (arg1 arg2 arg3 arg4)
	   (return-value-cast (f (cast1 arg1)
				 (cast2 arg2)
				 (cast3 arg3)
				 (cast4 arg4))))))
      (else
       (lambda args
	 (unless (= (length args) (length argument-casts))
	   (assertion-violation funcname
	     (format "wrong number of arguments, expected ~a" (length argument-casts))
	     args))
	 (return-value-cast (apply f (map
					 (lambda (m a) (m a))
				       argument-casts args))))))))


(define (primitive-make-c-function/with-errno return-value-type funcname argument-types)
  #f)



;;;; done

)

;;; end of file
