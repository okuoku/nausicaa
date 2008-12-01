;;;Copyright (c) 2004-2008 Yoshikatsu Fujita. All rights reserved.
;;;Copyright (c) 2004-2008 LittleWing Company Limited. All rights reserved.
;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
;;;
;;;Time-stamp: <2008-12-01 11:47:10 marco>
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
    (srfi receive)
    (srfi parameters)
    (uriel ffi sizeof)
    (uriel ffi out-of-memory))



;;;; documentation

;;Disclaimer
;;----------
;;
;;What follows  is an unofficial documentation of  the foreign functions
;;interface implemented by Ypsilon Scheme.  Currently (Sun Nov 30, 2008)
;;the interface  is fully  undocumented, so this  documentation reflects
;;the current understanding of it by Uriel's maintainer.
;;
;;The low  level procedures described  below are available  in Ypsilon's
;;core library; the high level procedures are in the "(ffi)" library.
;;
;;
;;Loading shared objects
;;----------------------
;;
;;It is done with the LOAD-SHARED-OBJECT procedure: it accepts as single
;;argument a string selecting the shared object file; it returns a value
;;referencing the loaded shared object.
;;
;;The string can  be the name of  a library's file or a  pathname of the
;;same,  absolute  or relative.   On  Unix  like  systems the  following
;;examples will work:
;;
;;  (import (rnrs)
;;    (ffi))
;;  (define zlib   (load-shared-object "libz.so")
;;  (define bzlib2 (load-shared-object "/usr/lib/libbz2.so")
;;
;;and assuming the current working directory is "/usr":
;;
;;  (import (rnrs)
;;    (ffi))
;;  (define gtk (load-shared-object "./lib/libgtk.so")
;;
;;If LOAD-SHARED-OBJECT is called with  an empty string as argument: the
;;return value references  the current process.  With this  value we can
;;access symbols exported by the  current program and the libraries that
;;have been already loaded (this may be platform specific, check out the
;;documentation  of your  operative system).   For example:  the symbols
;;from the standard C libray, like "fwrite", can be accessed this way.
;;
;;
;;Retrieving pointers to exported symbols
;;---------------------------------------
;;
;;It  is done  with the  LOOKUP-SHARED-OBJECT procedure:  it  accepts as
;;first argument the value  referencing an already loaded shared object,
;;and as second  argument a symbol or string representing  the name of a
;;function exported by the shared object; it returns a value referencing
;;the exported function.
;;
;;On Unix like systems the following example will work:
;;
;;  (import (rnrs)
;;    (ffi))
;;  (define zlib (load-shared-object "libz.so")
;;  (define deflateInit_f (lookup-shared-object zlib 'deflateInit_))
;;  (define deflate_f     (lookup-shared-object zlib 'deflate))
;;  (define deflateEnd_f  (lookup-shared-object zlib 'deflateEnd))
;;
;;as we see: we have to care about the true name of the exported symbols
;;(when  coding  in  the  C  language:  we call  the  init  function  as
;;"deflateInit", but the actual exported symbol is "deflateInit_").
;;
;;Another  example for  Unix  like  systems: to  access  the C  standard
;;"fwrite" we do:
;;
;;  (import (rnrs)
;;    (ffi))
;;  (define self (load-shared-object "")
;;  (define fwrite_f (lookup-shared-object self "fwrite"))
;;
;;
;;Calling foreign functions
;;-------------------------
;;
;;It is done by calling a  stub procedure, provided by Ypsilon, which is
;;specialised  to return  a type  of value.   All the  functions  in the
;;example above return  a C language "int" value, so,  for them, we have
;;to use the stub procedure STDCALL-SHARED-OBJECT->INT.
;;
;;Currently (Ypsilon checkout 285) the available stub procedures are:
;;
;;stdcall-shared-object->void
;;stdcall-shared-object->int
;;stdcall-shared-object->double
;;stdcall-shared-object->intptr
;;stdcall-shared-object->char*
;;
;;the "intptr"  one is to  be used for  foreign functions that  return a
;;pointer different from "char *".
;;
;;Knowing that the C language prototype of "deflateInit()" is:
;;
;;  int deflateInit (z_streamp strm, int level);
;;
;;in which "z_streamp"  is a pointer to structure,  to call the function
;;we can define a wrapper like this:
;;
;;  (import (rnrs)
;;    (ffi))
;;  (define zlib (load-shared-object "libz.so")
;;  (define deflateInit_f (lookup-shared-object zlib 'deflateInit_))
;;
;;  (define (deflateInit zstream level)
;;    (stdcall-shared-object->int deflateInit_f zstream level))
;;
;;Ypsilon automatically  figures out  how to push  the arguments  on the
;;stack.
;;
;;* The "zstream" argument must be a Scheme bytevector: Ypsilon extracts
;;  a pointer to  the first byte of  its data area and pushes  it on the
;;  stack.
;;
;;* The  "level" argument must be  an exact integer (in  the "int" range
;;  allowed by  the underlying platform): Ypsilon takes  the integer and
;;  pushes it on the stack.
;;
;;* The return  value of  the  foreign  function is  an "int":  the stub
;;  function  STDCALL-SHARED-OBJECT->INT builds  a Scheme  integer value
;;  out of it, and returns it.
;;
;;
;;Handling values conversion
;;--------------------------
;;
;;Arguments and  return values  are all proper  Scheme built  in values.
;;Ypsilon  does the  conversion  between Scheme  values  and C  language
;;values  automatically.   We  have  to  understand  the  rules  of  the
;;conversion.
;;
;;Values of types "int" and "double" are converted to exact integers and
;;double precision flonums.
;;
;;Pointers of  all types but "char  *" and the  referenced memory blocks
;;are  converted to  bytevectors.  We  do  not need  to allocate  memory
;;blocks by  accessing the standard  "malloc()" function: we  just build
;;byte vector using MAKE-BYTEVECTOR, and use it as argument to functions
;;requiring a "void *" pointer, or other pointers but "char *".
;;
;;Pointers  of  type "char  *"  and  the  referenced memory  blocks  are
;;converted  to  UTF8 coded  strings,  whose  last  byte is  zero.   For
;;compatibility purposes,  the Uriel libraries avoid this  and treat all
;;the C  strings as  memory blocks: "char  *" pointers are  treated like
;;"void *" pointers.
;;
;;
;;Accessing "errno"
;;-----------------
;;
;;It is done with  SHARED-OBJECT-C-ERRNO.  Calling this function returns
;;the  value of  the "errno"  variable  just after  the last  call to  a
;;foreign interface.
;;


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
    ((long signed-long ulong unsigned-long)
     'int)
    ((double)
     'double)
    ((float)
     'float)
    ((pointer void* char* FILE*)
     'void*)
    ((callback)
     'void*)
    ((void)
     'void)
    (else (assertion-violation 'make-c-function
	    "unknown C language type identifier" type))))



;;;; interface functions

(define (select-cast-and-stub ret-type)
  (let ((identity (lambda (x) x)))
    (case ret-type
      ((void)
       (values identity stdcall-shared-object->void))
      ((int)
       (values identity stdcall-shared-object->int))
      ((double)
       (values identity stdcall-shared-object->double))
      ((char* void*)
       (values identity stdcall-shared-object->intptr))
      ((bool)
       (values int->bool stdcall-shared-object->int))
;;;For compatibility purposes we do not use this stub, but rather we use
;;;the   one    for   "void   *".
;;;
;;;     ((char*)
;;;      (values char*->string stdcall-shared-object->char*))
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
    ((void)
     (lambda (x) x))
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
		 (mapper4 (caddr mappers)))
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
		 (mapper4 (caddr mappers)))
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

(define primitive-malloc
  (primitive-make-c-function 'void* 'malloc '(size_t)))

(define primitive-free
  (primitive-make-c-function 'void 'free '(void*)))

;; (define (primitive-malloc number-of-bytes)
;;   (make-bytevector number-of-bytes 0))

;; (define (primitive-free p)
;;   #f)

;;This  is required  by the  string functions  below.  This  function is
;;duplicated in "(uriel ffi)".
(define (malloc size)
  (let ((p (primitive-malloc size)))
    (unless p
      (raise (make-who-condition 'malloc)
	     (make-message-condition "out of memory")
	     (make-out-of-memory-condition size)))
    p))




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

(define primitive-strlen
  (primitive-make-c-function 'size_t 'strlen '(char*)))

(define (strlen cstr)
  (if (bytevector? cstr)
      (do ((i 0 (+ 1 i)))
	  ((= 0 (bytevector-u8-ref cstr i))
	   i)
	#f)
    (primitive-strlen cstr)))

(define (cstring->string cstr)
  (let ((len (strlen cstr)))
    (if (bytevector? cstr)
	;;We  have  to use  STRLEN  and  SUBSTRING  here because  it  is
	;;possible  for  foreign functions  to  place a  zero-terminated
	;;string in a larger buffer.  We have to cut the correct slice.
	(substring
	 (bytevector->string cstr (make-transcoder (utf-8-codec)))
	 0 len)
      (let* ((bv (make-bytevector len)))
	(do ((i 0 (+ 1 i)))
	    ((= i len)
	     (utf8->string bv))
	  (bytevector-s8-set! bv i (pointer-ref-c-signed-char cstr i)))))))

(define (string->cstring s)
  (let* ((len	(string-length s))
	 (p	(malloc (+ 1 len)))
	 (bv	(string->utf8 s)))
    (pointer-set-c-char! p len 0)
    (do ((i 0 (+ 1 i)))
	((= i len)
	 p)
      (pointer-set-c-char! p i (bytevector-s8-ref bv i)))))



;;;; done

)

;;; end of file
