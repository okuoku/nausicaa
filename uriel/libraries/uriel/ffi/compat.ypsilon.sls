;;;Copyright (c) 2004-2008 Yoshikatsu Fujita. All rights reserved.
;;;Copyright (c) 2004-2008 LittleWing Company Limited. All rights reserved.
;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
;;;
;;;Time-stamp: <2008-11-26 10:26:51 marco>
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
    primitive-make-c-function

    ;;basic memory allocation
    primitive-malloc primitive-free

    ;;basic string conversion
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
  (import (core)
;;;    (uriel printing)
    (srfi receive)
    (srfi parameters)
    (uriel ffi sizeof))


;;;; dynamic loading

(define self-shared-object (load-shared-object ""))

(define shared-object
  (make-parameter self-shared-object))

(define (primitive-open-shared-object library-name)
  ;;This raises an exception automatically.
  (load-shared-object library-name))



;;;; value normalisation: scheme -> c language

;;;The following mapping functions  are normalisators from Scheme values
;;;to values usable by the C language interface functions.

(define (assert-bool value)
  (if (boolean? value)
      (if value 1 0)
    (assertion-violation 'assert-bool
      "expected #t or #f as function argument" value)))

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

(define (assert-closure value)
  (if (procedure? value)
      value
    (assertion-violation 'assert-closure
      "expected procedure as function argument" value)))

(define (assert-char* value)
  (string->utf8-n-nul (assert-string value)))



;;;; value normalisation: c language -> scheme

;;;The  following  mapping   functions  are  normalisators  from  values
;;;returned by the C language interface functions to Scheme values.

(define int->bool
  (lambda (val)
    (not (= val 0))))

(define char*->string
  (lambda (val)
    (and val (bytevector->string val (make-transcoder (utf-8-codec))))))

(define string->utf8-n-nul
  (lambda (s)
    (string->utf8 (string-append s "\x0;"))))



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
    ((int signed-int ssize_t uint unsigned unsigned-int size_t)
     'int)
    ((double)
     'double)
    ((float)
     'float)
    ((pointer void* char*)
     'void*)
    ((callback)
     'void*)
    ((void)
     'void)
    (else (error 'make-c-function
	    "unknown C language type identifier" type))))



;;;; interface functions

(define (make-c-function ret-type funcname args)
  (define (select-cast-and-stub ret-type)
    (let ((identity (lambda (x) x)))
      (case ret-type
	((void)
	 (values identity stdcall-shared-object->void))
	((int)
	 (values identity stdcall-shared-object->int))
	((double)
	 (values identity stdcall-shared-object->double))
	((void*)
	 (values identity stdcall-shared-object->intptr))
	((bool)
	 (values int->bool stdcall-shared-object->int))
	((char*)
	 (values char*->string stdcall-shared-object->char*))
	(else
	 (assertion-violation 'make-c-callout
	   "unknown C language type identifier used for return value" ret-type)))))

  (define (select-argument-mapper arg-type)
    (case (external->internal arg-type)
      ((int)
       assert-int)
      ((bool)
       assert-bool)
      ((void*)
       assert-bytevector)
      ((float)
       assert-float)
      ((double)
       assert-double)
      ((byte*)
       assert-bytevector)
      ((char*)
       assert-char*)
      ((callback)
       assert-closure)
      (else
       (assertion-violation 'make-c-callout
	 "unknown C language type identifier used for function argument" arg-type))))

  (receive (cast-func stub-func)
      (select-cast-and-stub (external->internal ret-type))
    (let ((f (lookup-shared-object (shared-object) funcname)))
      (unless f
	(error 'make-c-callout
	  "function not available in shared object" funcname))
      (let* ((mappers (map select-argument-mapper args)))
	(lambda args
	  (unless (= (length args) (length mappers))
	    (assertion-violation funcname
	      (format "wrong number of arguments, expected ~a" (length mappers))
	      args))
	  (cast-func (apply stub-func f
			    (map (lambda (m a)
				   (m a)) mappers args))))))))


(define-syntax primitive-make-c-function
  (syntax-rules ()
    ((_ ?ret-type ?funcname (?arg-type0 ?arg-type ...))
     (make-c-function '?ret-type '?funcname '(?arg-type0 ?arg-type ...)))))



;;;; memory allocation

;;Whenever an  interface function returns  a pointer: Ypsilon  wraps the
;;block of memory into a bytevector.   So when we feed a block of memory
;;to an interface function: we have to build a bytevector.

(define (primitive-malloc number-of-bytes)
  (make-bytevector number-of-bytes))

(define (primitive-free p)
  #f)


;;;; pokers and peekers

(define pointer-ref-c-signed-char	bytevector-s8-ref)
(define pointer-ref-c-unsigned-char	bytevector-u8-ref)
(define pointer-ref-c-signed-short	bytevector-s16-native-ref)
(define pointer-ref-c-unsigned-short	bytevector-u16-native-ref)
(define pointer-ref-c-signed-int	bytevector-s32-native-ref)
(define pointer-ref-c-unsigned-int	bytevector-u32-native-ref)
(define pointer-ref-c-signed-long	(when on-32-bits-system
					  bytevector-s32-native-ref
					  bytevector-s64-native-ref))
(define pointer-ref-c-unsigned-long	(when on-32-bits-system
					  bytevector-u32-native-ref
					  bytevector-u64-native-ref))
(define pointer-ref-c-float		bytevector-ieee-single-native-ref)
(define pointer-ref-c-double		bytevector-ieee-double-native-ref)
(define pointer-ref-c-pointer
  (cond ((= 4 sizeof-pointer)		bytevector-u32-native-ref)
	((= 8 sizeof-pointer)		bytevector-u64-native-ref)
	(else
	 (assertion-violation 'pointer-ref-c-pointer
	   "cannot determine size of pointers for peeker function"))))

(define pointer-set-c-char!		bytevector-u8-set!)
(define pointer-set-c-short!		bytevector-u16-native-set!)
(define pointer-set-c-int!		bytevector-u32-native-set!)
(define pointer-set-c-long!		(when on-32-bits-system
					  bytevector-u32-native-set!
					  bytevector-u64-native-set!))
(define pointer-set-c-float!		bytevector-ieee-single-native-set!)
(define pointer-set-c-double!		bytevector-ieee-double-native-set!)
(define pointer-set-c-pointer!
  (cond ((= 4 sizeof-pointer)		bytevector-u32-native-set!)
	((= 8 sizeof-pointer)		bytevector-u64-native-set!)
	(else
	 (assertion-violation 'pointer-set-c-pointer
	   "cannot determine size of pointers for peeker function"))))



;;;; pointers

(define (integer->pointer x)
  x)

(define (pointer->integer x)
  x)

(define (pointer? x)
  (fixnum? x))



;;;; string functions

(define strlen
  (primitive-make-c-function int strlen (pointer)))

(define (cstring->string cstr)
  (let ((str (bytevector->string cstr (make-transcoder (utf-8-codec)))))
    (substring str 0 (- (string-length str) 1))))

(define (string->cstring str)
  (string->utf8 (string-append str "\x0;")))



;;;; done

)

;;; end of file
