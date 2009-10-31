;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: compatibility layer for Mosh FFI
;;;Date: Thu Jun 25, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


(library (foreign ffi platform)
  (export
    open-shared-object		self-shared-object
    lookup-shared-object	lookup-shared-object*
    make-c-function		make-c-function/with-errno
    pointer->c-function		pointer->c-function/with-errno
    make-c-callback
    (rename (mosh:free-c-callback	free-c-callback))
    internal-type->implementation-type/callout
    internal-type->implementation-type
    implementation-data-types)
  (import (rnrs)
    (unimplemented)
    (prefix (only (mosh ffi)
		  open-shared-library make-c-function
		  make-c-callback free-c-callback
		  shared-errno)
	    mosh:))


;;;; helpers

(define (%normalise-arg-types arg-types)
  (if (equal? '(void) arg-types)
      '()
    arg-types))


;;;; dynamic loading

;;In case of error this raises an exception automatically.
(define open-shared-object mosh:open-shared-library)

(define self-shared-object
  (mosh:open-shared-library ""))

(define (lookup-shared-object lib-spec foreign-symbol)
  (raise-unimplemented-error 'lookup-shared-library)
  ;;This already returns #f when the symbol is not found.
  ;;(mosh:lookup-shared-library lib-spec foreign-symbol)
  )

(define (lookup-shared-object* lib-spec foreign-symbol)
  (raise-unimplemented-error 'lookup-shared-library)
  ;; (let ((ptr (mosh:lookup-shared-library lib-spec foreign-symbol)))
  ;;   (or ptr (error #f "could not find foreign symbol in foreign library"
  ;; 		   lib-spec foreign-symbol)))
  )



;;;; values normalisation
;;
;;According to "lib/mosh/ffi.ss" (revision 2140):
;;
;;* The accepted return values for callouts are:
;;
;;  void
;;  void*	char*
;;  int
;;  double
;;
;;* The accepted arguments for callouts are:
;;
;;  int
;;  double
;;  void*	char*
;;
;;  an empty list represents no arguments.
;;
;;* The accepted return values for callbacks are:
;;
;;  void
;;  bool		char		size_t
;;  short		int		long		long-long
;;  unsigned-short	unsigned-int	unsigned-long	unsigned-long-long
;;  int8_t		int16_t		int32_t		int64_t
;;  uint8_t		uint16_t	uint32_t	uint64_t
;;  float		double
;;  void*
;;
;;* The accepted arguments for callbacks are:
;;
;;  void
;;  bool		char		size_t
;;  short		int		long		long-long
;;  unsigned-short	unsigned-int	unsigned-long	unsigned-long-long
;;  int8_t		int16_t		int32_t		int64_t
;;  uint8_t		uint16_t	uint32_t	uint64_t
;;  float		double
;;  void*
;;
;;  an empty list represents no arguments.
;;

(define implementation-data-types
  (make-enumeration '(int8_t int16_t int32_t int64_t
 		      uint8_t uint16_t uint32_t uint64_t
		      char short unsigned-short
		      int unsigned-int long unsigned-long
		      long-long unsigned-long-long
		      float double pointer void bool)))

(define (internal-type->implementation-type/callout type)
  (case type
    ((int8_t)				'int)
    ((int16_t)				'int)
    ((int32_t)				'int)
    ((int64_t)				'int)
    ((uint8_t)				'int)
    ((uint16_t)				'int)
    ((uint32_t)				'int)
    ((uint64_t)				'int)
    ((signed-char)			'int)
    ((unsigned-char)			'int)
    ((signed-short)			'int)
    ((unsigned-short)			'int)
    ((signed-int)			'int)
    ((unsigned-int)			'int)
    ((signed-long)			'int)
    ((unsigned-long)			'int)
    ((signed-long-long)			'int)
    ((unsigned-long-long)		'int)
    ((float)				'double)
    ((double)				'double)
    ((pointer)				'void*)
    ((callback)				'void*)
    ((void)				'void)
    (else
     (assertion-violation #f
       "C language type identifier unknown by Mosh" type))))

(define (internal-type->implementation-type type)
  (case type
    ((int8_t)				'int8_t)
    ((int16_t)				'int16_t)
    ((int32_t)				'int32_t)
    ((int64_t)				'int64_t)
    ((uint8_t)				'uint8_t)
    ((uint16_t)				'uint16_t)
    ((uint32_t)				'uint32_t)
    ((uint64_t)				'uint64_t)
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
       "C language type identifier unknown by Mosh" type))))

;;;; interface functions

(define (make-c-function lib-spec ret-type funcname arg-types)
  (mosh:make-c-function lib-spec ret-type (string->symbol funcname)
			(%normalise-arg-types arg-types)))

(define (make-c-function/with-errno lib-spec ret-type funcname arg-types)
  (let ((closure (make-c-function lib-spec ret-type funcname arg-types)))
    (lambda args
      ;;We have to use LET* here  to enforce the order of evaluation: we
      ;;want  to gather  the "errno"  value AFTER  the  foreign function
      ;;call.
      (let* ((retval	(begin
			  (mosh:shared-errno 0)
			  (apply closure args)))
	     (errval	(mosh:shared-errno)))
	(values retval errval)))))

(define (pointer->c-function ret-type address arg-types)
  (raise-unimplemented-error 'pointer->c-function)
;;;  (mosh:pointer->c-function ret-type (%normalise-arg-types arg-types) address))
  )

(define (pointer->c-function/with-errno ret-type address arg-types)
  (let ((closure (pointer->c-function ret-type address arg-types)))
    (lambda args
      ;;We have to use LET* here  to enforce the order of evaluation: we
      ;;want  to gather  the "errno"  value AFTER  the  foreign function
      ;;call.
      (let* ((retval	(begin
			  (mosh:shared-errno 0)
			  (apply closure args)))
	     (errval	(mosh:shared-errno)))
	(values retval errval)))))

(define (make-c-callback ret-type scheme-proc arg-types)
  (mosh:make-c-callback ret-type (%normalise-arg-types arg-types) scheme-proc))


;;;; done

)

;;; end of file
