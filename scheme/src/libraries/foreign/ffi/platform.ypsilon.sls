;;;Copyright (c) 2008, 2009 Marco Maggi <marcomaggi@gna.org>
;;;Copyright (c) 2004-2008 Yoshikatsu Fujita. All rights reserved.
;;;Copyright (c) 2004-2008 LittleWing Company Limited. All rights reserved.
;;;
;;;Abstract
;;;--------
;;;
;;;	For informations  on Ypsilon's FFI,  read the code  in Ypsilon's
;;;	source tree, file "sitelib/ypsilon/ffi.scm".
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
    open-shared-object		lookup-shared-object
    make-c-callout		make-c-callout/with-errno
    make-c-callback		free-c-callback)
  (import (rnrs)
    (conditions)
    (foreign ffi conditions)
    (prefix (only (ypsilon ffi)
		  load-shared-object lookup-shared-object
		  make-cdecl-callout make-cdecl-callback
		  shared-object-errno)
	    ypsilon:)
    (only (foreign ffi pointers)
	  pointer? integer->pointer pointer->integer))


;;;; helpers

(define (%normalise-foreign-symbol foreign-symbol)
  (if (symbol? foreign-symbol)
      (symbol->string foreign-symbol)
    foreign-symbol))

(define (identity x)
  x)

(define (%normalise-arg-types arg-types)
  (if (equal? '(void) arg-types)
      '()
    arg-types))

(define (%make-mappers-list pointer-mapper arg-types)
  (let loop ((arg-types	arg-types)
	     (mappers	'()))
    (if (null? arg-types)
	(reverse mappers)
      (loop (cdr arg-types)
	    (cons (if (eq? 'void* (car arg-types))
		      pointer-mapper identity)
		  mappers)))))


;;;; dynamic loading

(define (open-shared-object library-name)
  ;;Open  a shared  object selected  with the  string  LIBRARY-NAME.  If
  ;;successful: Return  a shared object reference, which  for Ypsilon is
  ;;an exact integer representing the return value of "dlopen()".  If an
  ;;error occurs return #f.
  ;;
  ;;In case of error YPSILON:LOAD-SHARED-OBJECT raises an exception.
  ;;
  (guard (E (else #f))
    (ypsilon:load-shared-object (%normalise-foreign-symbol library-name))))

(define (lookup-shared-object library-reference foreign-symbol)
  ;;YPSILON:LOOKUP-SHARED-OBJECT already  returns #f when  the symbol is
  ;;not found.
  ;;
  (let* ((foreign-symbol	(%normalise-foreign-symbol foreign-symbol))
	 (address		(ypsilon:lookup-shared-object library-reference foreign-symbol)))
    (and address (integer->pointer address))))


(define make-c-callout
  (case-lambda
   ((ret-type address arg-types)
    (make-c-callout ret-type address arg-types "<anonymous function>"))
   ((ret-type address arg-types funcname)
    (let* ((closure	(ypsilon:make-cdecl-callout ret-type arg-types (pointer->integer address)))
	   (mappers	(%make-mappers-list pointer->integer arg-types))
	   (expected	(length mappers))
	   (ret-ptr?	(eq? ret-type 'void*)))
      (lambda args
	(let ((given	(length args)))
	  (unless (= expected given)
	    (raise-wrong-num-args funcname
				  (string-append "wrong number of arguments to callout, expected "
						 (number->string expected)
						 " given "
						 (number->string given))
				  funcname expected given))
	  (let ((ret-val (apply closure (map (lambda (m a) (m a)) mappers args))))
	    (if ret-ptr?
		(integer->pointer ret-val)
	      ret-val))))))))

(define make-c-callout/with-errno
  (case-lambda
   ((ret-type address arg-types)
    (make-c-callout/with-errno ret-type address arg-types "<anonymous function>"))
   ((ret-type address arg-types funcname)
    (let* ((closure	(ypsilon:make-cdecl-callout ret-type arg-types (pointer->integer address)))
	   (mappers	(%make-mappers-list pointer->integer arg-types))
	   (expected	(length mappers))
	   (ret-ptr?	(eq? ret-type 'void*)))
      (lambda args
	(let ((given	(length args)))
	  (unless (= expected given)
	    (raise-wrong-num-args funcname
				  (string-append "wrong number of arguments to callout, expected "
						 (number->string expected)
						 " given "
						 (number->string given))
				  funcname expected given))
	  (let ((ret-val	(begin
				  (ypsilon:shared-object-errno 0)
				  (apply closure (map (lambda (m a) (m a)) mappers args))))
		(errval	(ypsilon:shared-object-errno)))
	    (values (if ret-ptr? (integer->pointer ret-val) ret-val)
		    errval))))))))


;;;; callback functions

(define (make-c-callback ret-type closure arg-types)
  (let ((mappers (%make-mappers-list integer->pointer arg-types)))
    (integer->pointer
     (ypsilon:make-cdecl-callback ret-type arg-types
				  (if (eq? ret-type 'void*)
				      (lambda args
					(pointer->integer (apply closure (map (lambda (m a) (m a))
									   mappers args))))
				    (lambda args
				      (apply closure (map (lambda (m a) (m a))
						       mappers args))))))))

(define (free-c-callback cb)
  #f)


;;;; done

)

;;; end of file
