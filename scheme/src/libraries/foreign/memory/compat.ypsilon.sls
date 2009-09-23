;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: compatibility memory functions for Ypsilon
;;;Date: Tue Dec 16, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008, 2009 Marco Maggi <marcomaggi@gna.org>
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


#!r6rs
(library (foreign memory compat)
  (export

    system-free				system-malloc
    system-calloc			system-realloc

    platform-free			platform-malloc
    platform-calloc			platform-realloc

    memset				memmove
    memcpy				memcmp

    ;;pointers
    pointer?
    integer->pointer			pointer->integer
    pointer-null			pointer-null?
    pointer-diff			pointer-add
    pointer=?				pointer<>?
    pointer<?				pointer>?
    pointer<=?				pointer>=?

    ;;peekers
    pointer-ref-c-int8			pointer-ref-c-uint8
    pointer-ref-c-int16			pointer-ref-c-uint16
    pointer-ref-c-int32			pointer-ref-c-uint32
    pointer-ref-c-int64			pointer-ref-c-uint64
    pointer-ref-c-float			pointer-ref-c-double
    pointer-ref-c-void*

    pointer-ref-c-signed-char		pointer-ref-c-unsigned-char
    pointer-ref-c-signed-short		pointer-ref-c-unsigned-short
    pointer-ref-c-signed-int		pointer-ref-c-unsigned-int
    pointer-ref-c-signed-long		pointer-ref-c-unsigned-long
    pointer-ref-c-signed-long-long	pointer-ref-c-unsigned-long-long
    pointer-ref-c-pointer

    ;;pokers
    pointer-set-c-int8!			pointer-set-c-uint8!
    pointer-set-c-int16!		pointer-set-c-uint16!
    pointer-set-c-int32!		pointer-set-c-uint32!
    pointer-set-c-int64!		pointer-set-c-uint64!
    pointer-set-c-float!		pointer-set-c-double!
    pointer-set-c-void*!

    pointer-set-c-signed-char!		pointer-set-c-unsigned-char!
    pointer-set-c-signed-short!		pointer-set-c-unsigned-short!
    pointer-set-c-signed-int!		pointer-set-c-unsigned-int!
    pointer-set-c-signed-long!		pointer-set-c-unsigned-long!
    pointer-set-c-signed-long-long!	pointer-set-c-unsigned-long-long!
    pointer-set-c-pointer!)
  (import (core)
    (ypsilon ffi)
    (foreign ffi sizeof))


;;;; pointers

(define-record-type pointer
  (fields (immutable value)))

(define (integer->pointer value)
  (if (integer? value)
      (make-pointer value)
    (assertion-violation 'integer->pointer
      "expected integer value" value)))

(define (pointer->integer pointer)
  (unless (pointer? pointer)
    (assertion-violation 'pointer->integer
      "expected pointer value" pointer))
  (pointer-value pointer))

(define pointer-null
  (integer->pointer 0))

(define (pointer-null? pointer)
  (= 0 (pointer->integer pointer)))

(define (pointer-diff pointer-1 pointer-2)
  (- (pointer->integer pointer-1)
     (pointer->integer pointer-2)))

(define (pointer-add pointer offset)
  (integer->pointer (+ (pointer->integer pointer)
		       offset)))

(let-syntax ((define-pointer-comparison (syntax-rules ()
					  ((_ ?name ?func)
					   (define ?name
					     (case-lambda
					      (()
					       #f)
					      ((pointer)
					       #t)
					      ((pointer-a pointer-b)
					       (?func (pointer->integer pointer-a)
						      (pointer->integer pointer-b)))
					      ((pointer-a pointer-b . pointers)
					       (apply ?func (map pointer->integer
							      (cons* pointer-a
								     pointer-b pointers))))))))))
  (define-pointer-comparison pointer=? =)
  (define-pointer-comparison pointer<? <)
  (define-pointer-comparison pointer>? >)
  (define-pointer-comparison pointer<=? <=)
  (define-pointer-comparison pointer>=? >=))

(define pointer<>?
  (case-lambda
   (()
    #f)
   ((pointer . pointers)
    (not (apply pointer=? pointer pointers)))))


;;;; low level allocation functions
;;
;;For the "system-" functions we want pointers to be exact integers; for
;;the  "platform-" functions  we want  pointers  to be  records of  type
;;"pointer".
;;
;;Ypsilon  itself deals  with foreign  values of  type "void*"  as exact
;;integers.
;;

(define self (load-shared-object ""))

(define system-free
  (make-cdecl-callout 'void '(void*) (lookup-shared-object self 'free)))

(define system-malloc
  (make-cdecl-callout 'void* '(size_t) (lookup-shared-object self 'malloc)))

(define system-realloc
  (make-cdecl-callout 'void* '(void* size_t) (lookup-shared-object self 'realloc)))

(define system-calloc
  (make-cdecl-callout 'void* '(size_t size_t) (lookup-shared-object self 'calloc)))

;;; --------------------------------------------------------------------

(define (platform-free pointer)
  (system-free (pointer->integer pointer)))

(define (platform-malloc number-of-bytes)
  (integer->pointer (system-malloc number-of-bytes)))

(define (platform-realloc pointer number-of-bytes)
  (integer->pointer (system-realloc (pointer->integer pointer)
				    number-of-bytes)))

(define (platform-calloc count element-size)
  (integer->pointer (system-calloc count element-size)))


;;;; low level memory operations

(define memset
  (let* ((address (lookup-shared-object self 'memset))
	 (closure (make-cdecl-callout 'void* '(void* int void*) address)))
    (lambda (pointer value size)
      (integer->pointer (closure (pointer->integer pointer) value size)))))

(define memmove
  (let* ((address (lookup-shared-object self 'memmove))
	 (closure (make-cdecl-callout 'void* '(void* void* size_t) address)))
    (lambda (dst src size)
      (integer->pointer (closure (pointer->integer dst) (pointer->integer src) size)))))

(define memcpy
  (let* ((address (lookup-shared-object self 'memcpy))
	 (closure (make-cdecl-callout 'void* '(void* void* size_t) address)))
    (lambda (dst src size)
      (integer->pointer (closure (pointer->integer dst) (pointer->integer src) size)))))

(define memcmp
  (let* ((address (lookup-shared-object self 'memcmp))
	 (closure (make-cdecl-callout 'void* '(void* void* size_t) address)))
    (lambda (ptr-a ptr-b size)
      (closure (pointer->integer ptr-a) (pointer->integer ptr-b) size))))


;;;; peekers

;;We have to make the peekers work even when the position is negative.
;;
;;The  procedure MAKE-BYTEVECTOR-MAPPING accepts  2 arguments:  an exact
;;integer  representing  the memory  address,  the  number  of bytes  to
;;consider part of the bytevector.  We precompute the address+offset and
;;use the  resulting address as first  byte of the  bytevector; then the
;;size of the bytevector is just the size of the data we have to access.

(let-syntax ((define-peeker (syntax-rules ()
			      ((_ ?name ?getter ?sizeof-data)
			       (define (?name pointer position)
				 (?getter
				  (make-bytevector-mapping (+ position (pointer-value pointer))
							   ?sizeof-data)
				  0)))
			      ((_ ?name ?getter ?sizeof-data ?mapper)
			       (define (?name pointer position)
				 (?mapper
				  (?getter
				   (make-bytevector-mapping (+ position (pointer-value pointer))
							    ?sizeof-data)
				   0)))))))

  (define-peeker pointer-ref-c-int8	bytevector-s8-ref         1)
  (define-peeker pointer-ref-c-int16	bytevector-s16-native-ref 2)
  (define-peeker pointer-ref-c-int32	bytevector-s32-native-ref 4)
  (define-peeker pointer-ref-c-int64	bytevector-s64-native-ref 8)

  (define-peeker pointer-ref-c-uint8	bytevector-u8-ref         1)
  (define-peeker pointer-ref-c-uint16	bytevector-u16-native-ref 2)
  (define-peeker pointer-ref-c-uint32	bytevector-u32-native-ref 4)
  (define-peeker pointer-ref-c-uint64	bytevector-u64-native-ref 8)

  (define-peeker pointer-ref-c-float	bytevector-ieee-single-native-ref sizeof-float)
  (define-peeker pointer-ref-c-double	bytevector-ieee-double-native-ref sizeof-double)
  (define-peeker pointer-ref-c-void*	bytevector-c-void*-ref sizeof-pointer make-pointer))

(let-syntax ((define-signed-peeker (syntax-rules ()
				     ((_ ?name ?sizeof-data)
				      (define ?name (case ?sizeof-data
						      ((1) pointer-ref-c-int8)
						      ((2) pointer-ref-c-int16)
						      ((4) pointer-ref-c-int32)
						      ((8) pointer-ref-c-int64)))))))

  (define-signed-peeker pointer-ref-c-signed-char	sizeof-char)
  (define-signed-peeker pointer-ref-c-signed-short	sizeof-short)
  (define-signed-peeker pointer-ref-c-signed-int	sizeof-int)
  (define-signed-peeker pointer-ref-c-signed-long	sizeof-long)
  (define-signed-peeker pointer-ref-c-signed-long-long	sizeof-long-long))

(let-syntax ((define-unsigned-peeker (syntax-rules ()
				       ((_ ?name ?sizeof-data)
					(define ?name (case ?sizeof-data
							((1) pointer-ref-c-uint8)
							((2) pointer-ref-c-uint16)
							((4) pointer-ref-c-uint32)
							((8) pointer-ref-c-uint64)))))))
  (define-unsigned-peeker pointer-ref-c-unsigned-char	sizeof-char)
  (define-unsigned-peeker pointer-ref-c-unsigned-short	sizeof-short)
  (define-unsigned-peeker pointer-ref-c-unsigned-int	sizeof-int)
  (define-unsigned-peeker pointer-ref-c-unsigned-long	sizeof-long)
  (define-unsigned-peeker pointer-ref-c-unsigned-long-long sizeof-long-long))

(define pointer-ref-c-pointer pointer-ref-c-void*)


;;;; pokers

;;We have  to make the pokers  work even when the  position is negative.
;;The  procedure MAKE-BYTEVECTOR-MAPPING accepts  2 arguments:  an exact
;;integer  representing  the memory  address,  the  number  of bytes  to
;;consider part of the bytevector.  We precompute the address+offset and
;;use the  resulting address as first  byte of the  bytevector; then the
;;size of the bytevector is just the size of the data we have to access.

(let-syntax ((define-poker (syntax-rules ()
			     ((_ ?name ?setter ?sizeof-data)
			      (define (?name pointer position value)
				(?setter
				 (make-bytevector-mapping (+ position (pointer-value pointer))
							  ?sizeof-data)
				 0 value)))
			     ((_ ?name ?setter ?sizeof-data ?mapper)
			      (define (?name pointer position value)
				(?setter
				 (make-bytevector-mapping (+ position (pointer-value pointer))
							  ?sizeof-data)
				 0 (?mapper value)))))))

  (define-poker pointer-set-c-int8!	bytevector-s8-set!         1)
  (define-poker pointer-set-c-int16!	bytevector-s16-native-set! 2)
  (define-poker pointer-set-c-int32!	bytevector-s32-native-set! 4)
  (define-poker pointer-set-c-int64!	bytevector-s64-native-set! 8)

  (define-poker pointer-set-c-uint8!	bytevector-u8-set!         1)
  (define-poker pointer-set-c-uint16!	bytevector-u16-native-set! 2)
  (define-poker pointer-set-c-uint32!	bytevector-u32-native-set! 4)
  (define-poker pointer-set-c-uint64!	bytevector-u64-native-set! 8)

  (define-poker pointer-set-c-float!	bytevector-ieee-single-native-set! sizeof-float)
  (define-poker pointer-set-c-double!	bytevector-ieee-double-native-set! sizeof-double)
  (define-poker pointer-set-c-void*!	bytevector-c-void*-set! sizeof-pointer pointer-value))

(let-syntax ((define-signed-poker (syntax-rules ()
				    ((_ ?name ?sizeof-data)
				     (define ?name (case ?sizeof-data
						     ((1) pointer-set-c-int8!)
						     ((2) pointer-set-c-int16!)
						     ((4) pointer-set-c-int32!)
						     ((8) pointer-set-c-int64!)))))))
  (define-signed-poker pointer-set-c-signed-char!		sizeof-char)
  (define-signed-poker pointer-set-c-signed-short!		sizeof-short)
  (define-signed-poker pointer-set-c-signed-int!		sizeof-int)
  (define-signed-poker pointer-set-c-signed-long!		sizeof-long)
  (define-signed-poker pointer-set-c-signed-long-long!		sizeof-long-long))

(let-syntax ((define-unsigned-poker (syntax-rules ()
				      ((_ ?name ?sizeof-data)
				       (define ?name (case ?sizeof-data
						       ((1) pointer-set-c-uint8!)
						       ((2) pointer-set-c-uint16!)
						       ((4) pointer-set-c-uint32!)
						       ((8) pointer-set-c-uint64!)))))))
  (define-unsigned-poker pointer-set-c-unsigned-char!		sizeof-char)
  (define-unsigned-poker pointer-set-c-unsigned-short!		sizeof-short)
  (define-unsigned-poker pointer-set-c-unsigned-int!		sizeof-int)
  (define-unsigned-poker pointer-set-c-unsigned-long!		sizeof-long)
  (define-unsigned-poker pointer-set-c-unsigned-long-long!	sizeof-long-long))

(define pointer-set-c-pointer! pointer-set-c-void*!)


;;;; done

)

;;; end of file
