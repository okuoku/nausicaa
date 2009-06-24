;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: compatibility memory functions for Larceny
;;;Date: Fri Dec 26, 2008
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



;;;; setup

(library (foreign memory compat)
  (export

    platform-free			platform-malloc
    platform-calloc			platform-realloc

    memset				memmove
    memcpy				memcmp

    ;;pointers
    pointer?
    integer->pointer			pointer->integer
    pointer-null			pointer-null?

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
  (import (rnrs)
    (primitives
     foreign-procedure

     %peek8 %peek8u %peek-short %peek-ushort %peek-int %peek-unsigned
     %peek-long %peek-ulong %peek-pointer

     %poke8 %poke8u %poke-short %poke-ushort %poke-int %poke-unsigned
     %poke-long %poke-ulong %poke-pointer

     void*-double-set! void*-double-ref
     void*-float-set! void*-float-ref

     void*? void*-rt record-constructor void*->address
     )
    (foreign ffi sizeof))


;;;; pointers

;;NULL pointers are #f for Larceny.   But then VOID*? does not accept #f
;;as valid.

(define (integer->pointer value)
  (if (= 0 value)
      #f
    (begin
      (unless (integer? value)
	(assertion-violation 'integer->pointer
	  "expected integer value" value))
      ((record-constructor void*-rt) value))))

(define (pointer->integer pointer)
  (if pointer
      (begin
	(unless (pointer? pointer)
	  (assertion-violation 'pointer->integer
	    "expected pointer value" pointer))
	(void*->address pointer))
    0))

(define (pointer? obj)
  (if obj (void*? obj) #t))

(define pointer-null
  #f)

(define (pointer-null? pointer)
  (if pointer #f #t))



;;;; foreign functions

(define platform-free
  (foreign-procedure "free" '(void*) 'void))

(define platform-malloc
  (foreign-procedure "malloc" '(unsigned) 'void*))

(define platform-realloc
  (foreign-procedure "realloc" '(void* unsigned) 'void*))

(define platform-calloc
  (foreign-procedure "calloc" '(unsigned unsigned) 'void*))

(define memset
  (foreign-procedure "memset" '(void* int unsigned) 'void*))

(define memmove
  (foreign-procedure "memmove" '(void* void* unsigned) 'void*))

(define memcpy
  (foreign-procedure "memcpy" '(void* void* unsigned) 'void*))

(define memcmp
  (foreign-procedure "memcmp" '(void* void* unsigned) 'int))



;;;; peekers

(define (pointer-ref-c-signed-char pointer position)
  (%peek8 (+ position (pointer->integer pointer))))

(define (pointer-ref-c-unsigned-char pointer position)
  (%peek8u (+ position (pointer->integer pointer))))

;;; --------------------------------------------------------------------

(define (pointer-ref-c-signed-short pointer position)
  (%peek-short (+ position (pointer->integer pointer))))

(define (pointer-ref-c-unsigned-short pointer position)
  (%peek-ushort (+ position (pointer->integer pointer))))

;;; --------------------------------------------------------------------

(define (pointer-ref-c-signed-int pointer position)
  (%peek-int (+ position (pointer->integer pointer))))

(define (pointer-ref-c-unsigned-int pointer position)
  (%peek-unsigned (+ position (pointer->integer pointer))))

;;; --------------------------------------------------------------------

(define (pointer-ref-c-signed-long pointer position)
  (%peek-long (+ position (pointer->integer pointer))))

(define (pointer-ref-c-unsigned-long pointer position)
  (%peek-ulong (+ position (pointer->integer pointer))))

;;; --------------------------------------------------------------------

(define (pointer-ref-c-signed-long-long pointer position)
  (error 'pointer-ref-c-signed-long-long
    "this primitive is not implemented"))

(define (pointer-ref-c-unsigned-long-long pointer position)
  (error 'pointer-ref-c-unsigned-long-long
    "this primitive is not implemented"))

;;; --------------------------------------------------------------------

(define pointer-ref-c-float void*-float-ref)

(define pointer-ref-c-double void*-double-ref)

;;; --------------------------------------------------------------------

(define (pointer-ref-c-pointer pointer position)
  (integer->pointer (%peek-pointer (+ position (pointer->integer pointer)))))



;;;; pokers

(define (pointer-set-c-char! pointer position value)
  ((if (< value 0) %poke8 %poke8u)
   (+ position (pointer->integer pointer)) value))

(define (pointer-set-c-short! pointer position value)
  ((if (< value 0) %poke-short %poke-ushort)
   (+ position (pointer->integer pointer)) value))

(define (pointer-set-c-int! pointer position value)
  ((if (< value 0) %poke-int %poke-unsigned)
   (+ position (pointer->integer pointer)) value))

(define (pointer-set-c-long! pointer position value)
  ((if (< value 0) %poke-long %poke-ulong)
   (+ position (pointer->integer pointer)) value))

(define (pointer-set-c-long-long! pointer position value)
  (error 'pointer-set-c-long-long!
    "this primitive is not implemented"))

(define pointer-set-c-float! void*-float-set!)

(define pointer-set-c-double! void*-double-set!)

(define (pointer-set-c-pointer! pointer position value)
  (%poke-pointer (+ (void*->address pointer) position)
		 (pointer->integer value)))



;;;; done

)

;;; end of file
