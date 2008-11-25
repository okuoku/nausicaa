;;;Copyright (c) 2004-2008 Yoshikatsu Fujita. All rights reserved.
;;;Copyright (c) 2004-2008 LittleWing Company Limited. All rights reserved.
;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
;;;
;;;Time-stamp: <2008-11-25 11:22:00 marco>
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
    shared-object primitive-open-shared-object primitive-make-c-function
    primitive-malloc primitive-free
    strlen string->cstring cstring->string)
  (import (core)
    (srfi receive)
    (srfi parameters)
    (uriel ffi sizeof))

  (define on-windows
    (and (string-contains (architecture-feature 'operating-system) "windows")
	 #t))
  (define on-darwin
    (and (string-contains (architecture-feature 'operating-system) "darwin")
	 #t))
  (define on-linux
    (and (string-contains (architecture-feature 'operating-system) "linux")
	 #t))
  (define on-freebsd
    (and (string-contains (architecture-feature 'operating-system) "freebsd")
	 #t))
  (define on-posix
    (not on-windows))

  (define on-x64
    (and (string-contains (architecture-feature 'machine-hardware) "x86_64")
	 #t))
  (define on-ia32
    (not on-x64))



;;;; dynamic loading

(define shared-object
  (make-parameter (load-shared-object "")))

(define (primitive-open-shared-object library-name)
  ;;This raises an exception automatically.
  (load-shared-object library-name))



;;;; value normalisation: scheme -> c language

;;;The following mapping functions  are normalisators from Scheme values
;;;to values usable by the C language interface functions.

(define (assert-bool value)
  (if (boolean? value)
      (if value 1 0)
    (assertion-violation name (format "expected #t or #f, but got ~r, as argument ~s" i n))))

(define assert-int
  (lambda (name n i)
    (cond ((and (integer? i) (exact? i)) i)
	  (else
	   (assertion-violation name (format "expected exact integer, but got ~r, as argument ~s" i n))))))

(define assert-float
  (lambda (name n f)
    (cond ((flonum? f) (flonum->float f))
	  (else
	   (assertion-violation name (format "expected flonum, but got ~r, as argument ~s" f n))))))

(define assert-double
  (lambda (name n f)
    (cond ((flonum? f) f)
	  (else
	   (assertion-violation name (format "expected flonum, but got ~r, as argument ~s" f n))))))

(define assert-string
  (lambda (name n s)
    (cond ((string? s) s)
	  (else
	   (assertion-violation name (format "expected string, but got ~r, as argument ~s" s n))))))

(define assert-bytevector
  (lambda (name n b)
    (cond ((bytevector? b) b)
	  (else
	   (assertion-violation name (format "expected bytevector, but got ~r, as argument ~s" b n))))))

(define assert-closure
  (lambda (name n p)
    (cond ((procedure? p) p)
	  (else
	   (assertion-violation name (format "expected procedure, but got ~r, as argument ~s" p n))))))

(define assert-int-vector
  (lambda (name n vect)
    (or (vector? vect)
	(assertion-violation name (format "expected vector, but got ~r, as argument ~s" vect n)))
    (let ((lst (vector->list vect)))
      (for-each (lambda (i)
		  (or (and (integer? i) (exact? i))
		      (assertion-violation name (format "expected list of exact integer, but got ~r, as argument ~s" vect n))))
	lst)
      lst)))

(define assert-string-vector
  (lambda (name n vect)
    (or (vector? vect)
	(assertion-violation name (format "expected vector, but got ~r, as argument ~s" vect n)))
    (let ((lst (vector->list vect)))
      (for-each (lambda (s)
		  (or (string? s)
		      (assertion-violation name (format "expected vector of string, but got ~r, as argument ~s" vect n))))
	lst)
      lst)))

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

(define make-binary-array-of-int
  (lambda argv
    (let ((step (architecture-feature 'alignof:int))
	  (proc (case (architecture-feature 'sizeof:int)
		  ((4) bytevector-s32-native-set!)
		  ((8) bytevector-s64-native-set!)
		  (else
		   (syntax-violation 'make-binary-array-of-int "byte size of int not defined")))))
      (let ((bv (make-bytevector (* step (length argv)))))
	(let loop ((offset 0) (arg argv))
	  (cond ((null? arg) bv)
		(else
		 (let ((value (car arg)))
		   (proc bv offset value)
		   (loop (+ offset step) (cdr arg))))))))))

(define make-binary-array-of-char*
  (lambda (ref . argv)
    (apply vector
	   ref
	   (map (lambda (value) (string->utf8-n-nul value)) argv))))



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
    ((int signed-int uint unsigned unsigned-int size_t ssize_t)
     'int)
    ((double)
     'double)
    ((pointer void* char*)
     'void*)
    ((void)
     'void)
    (else (error 'make-c-function
	    "unknown C language type identifier" type))))


;;;; interface functions

(define (make-c-callout ret-type funcname args)
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
    (case arg-type
      ((int)
       assert-int)
      ((bool)
       assert-bool)
      ((void*)
       assert-int)
      ((float)
       assert-float)
      ((double)
       assert-double)
      ((byte*)
       assert-bytevector)
      ((char*)
       assert-char*)
      (else
       (assertion-violation 'make-c-callout
	 "unknown C language type identifier used for function argument" arg-type))))

;;       ((_ name n [c-callback void (args ...)] var)
;;        (make-callback 0 (c-callback-arguments args ...) (assert-closure 'name n var)))
;;       ((_ name n [c-callback int (args ...)] var)
;;        (make-callback 0 (c-callback-arguments args ...) (assert-closure 'name n var)))
;;       ((_ name n [c-callback void __stdcall (args ...)] var)
;;        (make-callback 1 (c-callback-arguments args ...) (assert-closure 'name n var)))
;;       ((_ name n [c-callback int __stdcall (args ...)] var)

  (receive (cast-func stub-func)
      (select-cast-and-stub ret-value)
    (let ((f (lookup-shared-object (shared-object) funcname)))
      (unless f
	(error 'make-c-callout
	  "function not available in shared object" funcname))
      (lambda (args ...)
	(cast-func (apply stub-func f (map select-argument-mapper args))))))))



;;;; memory allocation

(define platform-malloc
  (primitive-make-c-function pointer malloc (size_t)))

(define (primitive-malloc number-of-bytes)
  (let ((p (platform-malloc number-of-bytes)))
    (if (= 0 p)
	#f
      p)))

(define primitive-free
  (primitive-make-c-function void free (pointer)))

;;  (c-function (shared-object) libc-name void free (char*)))


;;;; string functions

(define strlen
  (primitive-make-c-function int strlen (pointer)))

;;  (c-function (shared-object) libc-name int __stdcall strlen (char*)))

(define (cstring->string cstr)
  (let ((str (bytevector->string cstr (make-transcoder (utf-8-codec)))))
    (substring str 0 (- (string-length str) 1))))

(define (string->cstring str)
  (string->utf8 (string-append str "\x0;")))



;;;; done

)

;;; end of file
