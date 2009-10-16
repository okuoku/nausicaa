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


(library (foreign ffi compat)
  (export
    shared-object primitive-open-shared-object self-shared-object
    primitive-make-c-function primitive-make-c-function/with-errno
    primitive-make-c-callback primitive-free-c-callback
    errno)
  (import (rnrs)
    (primitives make-parameter
		foreign-file foreign-procedure
		get-errno set-errno!)
    (only (foreign ffi pointers)
	  pointer-null)
    (only (unimplemented)
	  raise-unimplemented-error))


;;;; dynamic loading

(define self-shared-object (foreign-file ""))

(define shared-object
  (make-parameter self-shared-object))

;;In case of error this raises an exception automatically.
(define primitive-open-shared-object foreign-file)



;;;; values normalisation

(define (nausicaa-type->larceny-type type)
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
     '(maybe void*))
    ((callback)
     ;;This ugly thing "(maybe void*)" represents a pointer which can be
     ;;NULL.  When not NULL, it is  a record of type void*-rt; when NULL
     ;;it is #f.
     '(maybe void*))
    (else
     (assertion-violation 'primitive-make-c-function
       "unknown C language type identifier" type))))


;;;; callout closures

(define errno
  (case-lambda
   ((value)
    (set-errno! value))
   (()
    (get-errno))))

(define (primitive-make-c-function ret-type funcname arg-types)
  (let ((callout-closure (foreign-procedure (if (string? funcname)
						funcname
					      (symbol->string funcname))
					    (if (equal? '(void) arg-types)
						'()
					      (map nausicaa-type->larceny-type arg-types))
					    (nausicaa-type->larceny-type ret-type))))
    ;;When the  return value is a  pointer: if the pointer  is NULL, the
    ;;return value  is #f.   So we have  to convert it  to POINTER-NULL.
    ;;Ugly but what can I do?
    (if (memq ret-type '(pointer void* char* FILE*))
	(lambda args
	  (or (apply callout-closure args) pointer-null))
      callout-closure)))

(define (primitive-make-c-function/with-errno ret-type funcname arg-types)
  (let ((f (primitive-make-c-function ret-type funcname arg-types)))
    (lambda args
      ;;We have to use LET* here  to enforce the order of evaluation: we
      ;;want  to gather  the "errno"  value AFTER  the  foreign function
      ;;call.
      (let* ((retval	(begin
			  (errno 0)
			  (apply f args)))
	     (errval	(errno)))
	(values retval errval)))))


(define (primitive-make-c-callback ret-type scheme-function arg-types)
  (raise-unimplemented-error 'primitive-make-c-callback
			     "callbacks are not implemented for Larceny"))
  ;; (ffi/make-callback 'i386 scheme-function
  ;; 		     (map nausicaa-type->larceny-type arg-types)
  ;; 		     (nausicaa-type->larceny-type ret-type))

(define (primitive-free-c-callback cb)
  #f)


;;;; done

)

;;; end of file
