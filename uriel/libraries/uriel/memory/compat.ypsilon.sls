;;;
;;;Part of: Nausicaa/Uriel
;;;Contents: compatibility memory functions for Ypsilon
;;;Date: Tue Dec 16, 2008
;;;Time-stamp: <2008-12-17 21:05:46 marco>
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
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



;;;; setup

(library (uriel memory compat)
  (export

    platform-free			platform-malloc
    platform-calloc			platform-realloc

    memset				memmove
    memcpy				memcmp

    ;;pointers
    pointer?
    integer->pointer			pointer->integer

    ;;peekers
    pointer-ref-c-signed-char		pointer-ref-c-unsigned-char
    pointer-ref-c-signed-short		pointer-ref-c-unsigned-short
    pointer-ref-c-signed-int		pointer-ref-c-unsigned-int
    pointer-ref-c-signed-long		pointer-ref-c-unsigned-long
    pointer-ref-c-signed-long-long	pointer-ref-c-unsigned-long-long
    pointer-ref-c-float			pointer-ref-c-double
    pointer-ref-c-pointer

    ;;pokers
    pointer-set-c-char!			pointer-set-c-short!
    pointer-set-c-int!			pointer-set-c-long!
    pointer-set-c-long-long!		pointer-set-c-float!
    pointer-set-c-double!		pointer-set-c-pointer!)
  (import (core)
    (uriel ffi sizeof))


;;;; pointers

(define-record-type pointer
  (fields (immutable value)))

(define (integer->pointer value)
  (unless (integer? value)
    (assertion-violation 'integer->pointer
      "expected integer value" value))
  (make-pointer value))

(define (pointer->integer pointer)
  (unless (pointer? pointer)
    (assertion-violation 'pointer->integer
      "expected pointer value" pointer))
  (pointer-value pointer))


;;;; foreign functions

(define self (load-shared-object ""))

(define platform-free
  (let ((f (lookup-shared-object self 'free)))
    (lambda (pointer)
      (stdcall-shared-object->void f (pointer->integer pointer)))))

(define platform-malloc
  (let ((f (lookup-shared-object self 'malloc)))
    (lambda (number-of-bytes)
      (integer->pointer
       (stdcall-shared-object->intptr f number-of-bytes)))))

(define platform-realloc
  (let ((f (lookup-shared-object self 'realloc)))
    (lambda (pointer new-size)
      (integer->pointer
       (stdcall-shared-object->intptr f
				      (pointer->integer pointer)
				      new-size)))))

(define platform-calloc
  (let ((f (lookup-shared-object self 'calloc)))
    (lambda (count element-size)
      (integer->pointer
       (stdcall-shared-object->intptr f count element-size)))))

(define memset
  (let ((f (lookup-shared-object self 'memset)))
    (lambda (pointer byte-value number-of-bytes)
      (integer->pointer
       (stdcall-shared-object->intptr f
				      (pointer->integer pointer)
				      byte-value
				      number-of-bytes)))))

(define memmove
  (let ((f (lookup-shared-object self 'memmove)))
    (lambda (dst src number-of-bytes)
      (integer->pointer
       (stdcall-shared-object->intptr f
				      (pointer->integer dst)
				      (pointer->integer src)
				      number-of-bytes)))))

(define memcpy
  (let ((f (lookup-shared-object self 'memcpy)))
    (lambda (dst src number-of-bytes)
      (integer->pointer
       (stdcall-shared-object->intptr f
				      (pointer->integer dst)
				      (pointer->integer src)
				      number-of-bytes)))))

(define memcmp
  (let ((f (lookup-shared-object self 'memcmp)))
    (lambda (a b number-of-bytes)
      (integer->pointer
       (stdcall-shared-object->int f
				   (pointer->integer a)
				   (pointer->integer b)
				   number-of-bytes)))))



;;;; pokers and peekers

(define (pointer-ref-c-signed-char pointer position)
  (bytevector-s8-ref
   (make-bytevector-mapping (pointer-value pointer)
			    (+ 1 position))
   position))

(define (pointer-ref-c-unsigned-char pointer position)
  (bytevector-u8-ref
   (make-bytevector-mapping (pointer-value pointer)
			    (+ 1 position))
   position))

;;; --------------------------------------------------------------------

(define (pointer-ref-c-signed-short pointer position)
  (bytevector-s16-ref (make-bytevector-mapping (pointer-value pointer)
					       (+ 2 position))
		      position (native-endianness)))

(define (pointer-ref-c-unsigned-short pointer position)
  (bytevector-u16-ref (make-bytevector-mapping (pointer-value pointer)
					       (+ 2 position))
		      position (native-endianness)))

;;; --------------------------------------------------------------------

(define (pointer-ref-c-signed-int pointer position)
  (bytevector-s32-ref (make-bytevector-mapping (pointer-value pointer)
					       (+ 4 position))
		      position (native-endianness)))

(define (pointer-ref-c-unsigned-int pointer position)
  (bytevector-u32-ref (make-bytevector-mapping (pointer-value pointer)
					       (+ 4 position))
		      position (native-endianness)))

;;; --------------------------------------------------------------------

(define (pointer-ref-c-signed-long pointer position)
  (if on-32-bits-system
      (bytevector-s32-ref (make-bytevector-mapping (pointer-value pointer)
						   (+ 4 position))
			  position (native-endianness))
    (bytevector-s64-ref (make-bytevector-mapping (pointer-value pointer)
						 (+ 8 position))
			position (native-endianness))))

(define (pointer-ref-c-unsigned-long pointer position)
  (if on-32-bits-system
      (bytevector-u32-ref (make-bytevector-mapping (pointer-value pointer)
						   (+ 4 position))
			  position (native-endianness))
    (bytevector-u64-ref (make-bytevector-mapping (pointer-value pointer)
						 (+ 8 position))
			position (native-endianness))))

;;; --------------------------------------------------------------------

(define (pointer-ref-c-signed-long-long pointer position)
  (bytevector-s64-ref (make-bytevector-mapping (pointer-value pointer)
					       (+ 8 position))
		      position (native-endianness)))

(define (pointer-ref-c-unsigned-long-long pointer position)
  (bytevector-u64-ref (make-bytevector-mapping (pointer-value pointer)
					       (+ 8 position))
		      position (native-endianness)))

;;; --------------------------------------------------------------------

(define (pointer-ref-c-float pointer position)
  (bytevector-ieee-single-ref (make-bytevector-mapping (pointer-value pointer)
						       (+ 4 position))
			      position (native-endianness)))

(define (pointer-ref-c-double pointer position)
  (bytevector-ieee-double-ref (make-bytevector-mapping (pointer-value pointer)
						       (+ 8 position))
			      position (native-endianness)))

;;; --------------------------------------------------------------------

(define (pointer-ref-c-pointer pointer position)
  (cond ((= 4 sizeof-pointer)
	 (make-pointer (pointer-ref-c-unsigned-int pointer position)))
	((= 8 sizeof-pointer)
	 (make-pointer (pointer-ref-c-unsigned-long-long pointer position)))
	(else
	 (assertion-violation 'pointer-ref-c-pointer
	   "cannot determine size of pointers for peeker function"))))

;;; --------------------------------------------------------------------

(define (pointer-set-c-char! pointer position value)
  (bytevector-u8-set! (make-bytevector-mapping (pointer-value pointer)
					       (+ 1 position))
		      position value))

(define (pointer-set-c-short! pointer position value)
  (bytevector-u16-set! (make-bytevector-mapping (pointer-value pointer)
						(+ 4 position))
		       position value (native-endianness)))

(define (pointer-set-c-int! pointer position value)
  (bytevector-s32-set! (make-bytevector-mapping (pointer-value pointer)
						(+ 4 position))
		       position value (native-endianness)))

(define (pointer-set-c-long! pointer position value)
  (if on-32-bits-system
      (bytevector-s32-set! (make-bytevector-mapping (pointer-value pointer)
						    (+ 4 position))
			   position value (native-endianness))
    (bytevector-s64-set! (make-bytevector-mapping (pointer-value pointer)
						  (+ 8 position))
			 position value (native-endianness))))

(define (pointer-set-c-long-long! pointer position value)
  (bytevector-s64-set! (make-bytevector-mapping (pointer-value pointer)
						(+ 8 position))
		       position value (native-endianness)))

;;; --------------------------------------------------------------------

(define (pointer-set-c-float! pointer position value)
  (bytevector-ieee-single-set!
   (make-bytevector-mapping (pointer-value pointer)
			    (+ 4 position))
   position value (native-endianness)))

(define (pointer-set-c-double! pointer position value)
  (bytevector-ieee-double-set!
   (make-bytevector-mapping (pointer-value pointer)
			    (+ 8 position))
   position value (native-endianness)))

;;; --------------------------------------------------------------------

(define (pointer-set-c-pointer! pointer position the-pointer)
  (cond ((= 4 sizeof-pointer)
	 (bytevector-u32-set! (make-bytevector-mapping (pointer-value pointer)
						       (+ 4 position))
			      position (pointer-value the-pointer)
			      (native-endianness)))
	((= 8 sizeof-pointer)
	 (bytevector-u64-set! (make-bytevector-mapping (pointer-value pointer)
						       (+ 8 position))
			      position (pointer-value the-pointer)
			      (native-endianness)))
	(else
	 (assertion-violation 'pointer-set-c-pointer
	   "cannot determine size of pointers for peeker function"))))



;;;; done

)

;;; end of file
