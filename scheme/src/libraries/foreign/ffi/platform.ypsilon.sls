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
    open-shared-object		self-shared-object
    lookup-shared-object	lookup-shared-object*
    make-c-function		make-c-function/with-errno
    pointer->c-function		pointer->c-function/with-errno
    make-c-callback		free-c-callback
    internal-type->implementation-type
    implementation-data-types)
  (import (rnrs)
    (prefix (only (ypsilon ffi)
		  load-shared-object lookup-shared-object
		  make-cdecl-callout make-cdecl-callback
		  shared-object-errno)
	    ypsilon:)
;;    (foreign ffi sizeof)
    (only (foreign ffi pointers)
	  pointer? integer->pointer pointer->integer))


;;;; helpers

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

;;In case of error this raises an exception automatically.
(define open-shared-object ypsilon:load-shared-object)

(define self-shared-object
  (ypsilon:load-shared-object ""))

(define (lookup-shared-object lib-spec foreign-symbol)
  ;;This already returns #f when the symbol is not found.
  (let ((address (ypsilon:lookup-shared-object lib-spec foreign-symbol)))
    (and address (integer->pointer address))))

(define (lookup-shared-object* lib-spec foreign-symbol)
  (let ((address (ypsilon:lookup-shared-object lib-spec foreign-symbol)))
    (integer->pointer (or address
			  (error #f "could not find foreign symbol in foreign library"
				 lib-spec foreign-symbol)))))


;;;; types normalisation
;;
;;In Ypsilon revision 503, it appears that an argument to a function can
;;be one among:
;;
;;  bool		char		size_t
;;  short		int		long		long-long
;;  unsigned-short	unsigned-int	unsigned-long	unsigned-long-long
;;  float		double
;;  void*
;;  int8_t		int16_t		int32_t		int64_t
;;  uint8_t		uint16_t	uint32_t	uint64_t
;;
;;the return type of a callout function can be one among:
;;
;;  void
;;  bool		char		size_t
;;  short		int		long		long-long
;;  unsigned-short	unsigned-int	unsigned-long	unsigned-long-long
;;  float		double
;;  void*		char*
;;  int8_t		int16_t		int32_t		int64_t
;;  uint8_t		uint16_t	uint32_t	uint64_t
;;
;;the return type of a callback function can be one among:
;;
;;  void
;;  bool		char		size_t
;;  short		int		long		long-long
;;  unsigned-short	unsigned-int	unsigned-long	unsigned-long-long
;;  float		double
;;  void*
;;  int8_t		int16_t		int32_t		int64_t
;;  uint8_t		uint16_t	uint32_t	uint64_t
;;
;;Care  must  be  taken  in  selecting  types,  because:
;;
;;* Selecting "void*"  as Ypsilon type will cause  Ypsilon to allocate a
;;  bytevector and use it as value.
;;
;;* Selecting "char*"  as Ypsilon type will cause  Ypsilon to allocate a
;;  string and use it as value.
;;

(define implementation-data-types
  (make-enumeration '(bool		char		size_t
		      short		int		long		long-long
		      unsigned-short	unsigned-int	unsigned-long	unsigned-long-long
		      float		double
		      void*
		      int8_t		int16_t		int32_t		int64_t
		      uint8_t		uint16_t	uint32_t	uint64_t)))

(define (internal-type->implementation-type type)
  (case type
    ((int8_t)				'int8_t)
    ((int16_t)				'int16_t)
    ((int32_t)				'int32_t)
    ((int64_t)				'int64_t)
    ((uint8_t)				'uint8_t)
    ((uint16_t)				'uint16_t)
    ((uint32_t)				'uint32_t)
    ((uint64_t)				'uint32_t)
    ((signed-char)			'char)
    ((unsigned-char)			'char)
    ((signed-short)			'short)
    ((unsigned-short)			'unsigned-short)
    ((signed-int)			'int)
    ((unsigned-int)			'unsigned-int)
    ((signed-long)			'long)
    ((unsigned-long)			'unsigned-long)
    ((signed-long-long)			'long-long)
    ((unsigned-long-long)		'unsigned-long-long)
    ((float)				'float)
    ((double)				'double)
    ((pointer)				'void*)
    ((callback)				'void*)
    ((void)				'void)
    (else
     (assertion-violation #f
       "C language data type unknown to Ypsilon" type))))


(define (%closure-and-mappers ret-type address arg-types)
  (values (ypsilon:make-cdecl-callout ret-type arg-types (pointer->integer address))
	  (%make-mappers-list pointer->integer arg-types)))

(define (%make-c-function maker lib-spec ret-type funcname arg-types)
  (maker ret-type
	 (lookup-shared-object* lib-spec funcname)
	 (%normalise-arg-types arg-types)))

(define (make-c-function lib-spec ret-type funcname arg-types)
  (%make-c-function pointer->c-function lib-spec ret-type funcname arg-types))

(define (make-c-function/with-errno lib-spec ret-type funcname arg-types)
  (%make-c-function pointer->c-function/with-errno lib-spec ret-type funcname arg-types))

(define (pointer->c-function ret-type address arg-types)
  (let-values (((closure mappers) (%closure-and-mappers ret-type address arg-types)))
    (if (eq? ret-type 'void*)
	(lambda args
	  (integer->pointer (apply closure (map (lambda (m a) (m a))
					     mappers args))))
      (lambda args
	(apply closure (map (lambda (m a) (m a))
			 mappers args))))))

(define (pointer->c-function/with-errno ret-type address arg-types)
  (let-values (((closure mappers) (%closure-and-mappers ret-type address arg-types)))
    (if (eq? ret-type 'void*)
	(lambda args
	  (let* ((retval (begin
			   (ypsilon:shared-object-errno 0)
			   (integer->pointer (apply closure (map (lambda (m a) (m a))
							      mappers args)))))
		 (errval (ypsilon:shared-object-errno)))
	    (values retval errval)))
      (lambda args
	(let* ((retval (begin
			 (ypsilon:shared-object-errno 0)
			 (apply closure (map (lambda (m a) (m a))
					  mappers args))))
	       (errval (ypsilon:shared-object-errno)))
	  (values retval errval))))))


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
