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


;;;; setup

(library (uriel ffi compat)
  (export

    ;;shared object loading
    shared-object primitive-open-shared-object self-shared-object

    ;;interface functions
    primitive-make-c-function primitive-make-c-function/with-errno

    errno)
  (import (rnrs)
    (primitives
     foreign-file foreign-procedure get-errno set-errno!
     syscall)
    (uriel lang)
    (uriel ffi sizeof)
    (uriel memory))


;;;; dynamic loading

(define self-shared-object (foreign-file ""))

(define shared-object
  (make-parameter self-shared-object))

;;In case of error this raises an exception automatically.
(define primitive-open-shared-object foreign-file)



;;;; values normalisation: Uriel -> Larceny

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
     '(maybe void*))
    ((callback)
     '(maybe void*))
    (else
     (assertion-violation 'make-c-function
       "unknown C language type identifier" type))))


;;;; interface functions

(define errno
  (case-lambda
   ((value)
    (set-errno! value))
   (()
    (get-errno))))

(define (primitive-make-c-function ret-type funcname arg-types)
  (foreign-procedure (symbol->string/maybe funcname)
		     (if (equal? '(void) arg-types)
			 '()
		       (map external->internal arg-types))
		     (external->internal ret-type)))

(define (primitive-make-c-function/with-errno ret-type funcname arg-types)
  (let ((f (primitive-make-c-function ret-type funcname arg-types)))
    (lambda args
      ;;We have to use LET* here  to enforce the order of evaluation: we
      ;;want  to gather  the "errno"  value AFTER  the  foreign function
      ;;call.
      (let* ((retval	(begin
			  (set-errno! 0)
			  (apply f args)))
	     (errval	(get-errno)))
	(values retval errval)))))



;;;; done

)

;;; end of file
