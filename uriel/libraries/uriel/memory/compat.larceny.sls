;;;
;;;Part of: Nausicaa/Uriel
;;;Contents: compatibility memory functions for Larceny
;;;Date: Fri Dec 26, 2008
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
  (import (rnrs)
    (primitives
     foreign-procedure

     %peek8 %peek8u %peek-short %peek-ushort %peek-int %peek-unsigned
     %peek-long %peek-ulong %peek-pointer

     %poke8 %poke8u %poke-short %poke-ushort %poke-int %poke-unsigned
     %poke-long %poke-ulong %poke-pointer

     void*-double-set! void*-double-ref
     )
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

(define larceny-pointer-integer
  (case pointer-integer
    ((unsigned-long)	'ulong)
    ((unsigned-int)	'unsigned)))

(define platform-free
  (let ((f (foreign-procedure "free" (list larceny-pointer-integer) 'void)))
    (lambda (pointer)
      (f (pointer-value pointer)))))

(define platform-malloc
  (let ((f (foreign-procedure "malloc" '(unsigned) larceny-pointer-integer)))
    (lambda (size)
      (make-pointer (f size)))))

(define platform-realloc
  (let ((f (foreign-procedure "realloc"
			      (list larceny-pointer-integer 'unsigned)
			      larceny-pointer-integer)))
    (lambda (pointer size)
      (make-pointer (f (pointer-value pointer) size)))))

(define platform-calloc
  (let ((f (foreign-procedure "calloc" '(unsigned unsigned) larceny-pointer-integer)))
    (lambda (count element-size)
      (make-pointer (f count element-size)))))

(define memset
  (let ((f (foreign-procedure "memset"
			      (list larceny-pointer-integer 'int 'unsigned)
			      larceny-pointer-integer)))
    (lambda (pointer value number-of-bytes)
      (make-pointer (f (pointer-value pointer) value number-of-bytes)))))

(define memmove
  (let ((f (foreign-procedure "memmove"
			      (list larceny-pointer-integer
				    larceny-pointer-integer
				    'unsigned)
			      larceny-pointer-integer)))
    (lambda (dst src number-of-bytes)
      (make-pointer (f (pointer-value dst)
		       (pointer-value src)
		       number-of-bytes)))))

(define memcpy
  (let ((f (foreign-procedure "memcpy"
			      (list larceny-pointer-integer
				    larceny-pointer-integer
				    'unsigned)
			      larceny-pointer-integer)))
    (lambda (dst src number-of-bytes)
      (make-pointer (f (pointer-value dst)
		       (pointer-value src)
		       number-of-bytes)))))

(define memcmp
  (let ((f (foreign-procedure "memcmp"
			      (list larceny-pointer-integer
				    larceny-pointer-integer
				    'unsigned)
			      'int)))
    (lambda (dst src number-of-bytes)
      (f (pointer-value dst)
	 (pointer-value src)
	 number-of-bytes))))



;;;; peekers

(define (pointer-ref-c-signed-char pointer position)
  (%peek8 (+ position (pointer-value pointer))))

(define (pointer-ref-c-unsigned-char pointer position)
  (%peek8u (+ position (pointer-value pointer))))

;;; --------------------------------------------------------------------

(define (pointer-ref-c-signed-short pointer position)
  (%peek-short (+ position (pointer-value pointer))))

(define (pointer-ref-c-unsigned-short pointer position)
  (%peek-ushort (+ position (pointer-value pointer))))

;;; --------------------------------------------------------------------

(define (pointer-ref-c-signed-int pointer position)
  (%peek-int (+ position (pointer-value pointer))))

(define (pointer-ref-c-unsigned-int pointer position)
  (%peek-unsigned (+ position (pointer-value pointer))))

;;; --------------------------------------------------------------------

(define (pointer-ref-c-signed-long pointer position)
  (%peek-long (+ position (pointer-value pointer))))

(define (pointer-ref-c-unsigned-long pointer position)
  (%peek-ulong (+ position (pointer-value pointer))))

;;; --------------------------------------------------------------------

(define (pointer-ref-c-signed-long-long pointer position)
  (error 'pointer-ref-c-signed-long-long
    "this primitive is not implemented"))

(define (pointer-ref-c-unsigned-long-long pointer position)
  (error 'pointer-ref-c-unsigned-long-long
    "this primitive is not implemented"))

;;; --------------------------------------------------------------------

(define (pointer-ref-c-float pointer position)
  (error 'pointer-ref-c-float
    "this primitive is not implemented"))

(define (pointer-ref-c-double pointer position)
  (void*-double-ref (pointer-value pointer) position))

;;; --------------------------------------------------------------------

(define (pointer-ref-c-pointer pointer position)
  (make-pointer (%peek-pointer (+ position (pointer-value pointer)))))



;;;; pokers

(define (pointer-set-c-char! pointer position value)
  ((if (< value 0) %poke8 %poke8u)
   (+ position (pointer-value pointer)) value))

(define (pointer-set-c-short! pointer position value)
  ((if (< value 0) %poke-short %poke-ushort)
   (+ position (pointer-value pointer)) value))

(define (pointer-set-c-int! pointer position value)
  ((if (< value 0) %poke-int %poke-unsigned)
   (+ position (pointer-value pointer)) value))

(define (pointer-set-c-long! pointer position value)
  ((if (< value 0) %poke-long %poke-ulong)
   (+ position (pointer-value pointer)) value))

(define (pointer-set-c-long-long! pointer position value)
  (error 'pointer-set-c-long-long!
    "this primitive is not implemented"))

(define (pointer-set-c-float! pointer position value)
  (error 'pointer-set-c-float!
    "this primitive is not implemented"))

(define (pointer-set-c-double! pointer position value)
  (void*-double-set! (pointer-value pointer) position value))

(define (pointer-set-c-pointer! pointer position value)
  (%poke-pointer (+ position (pointer-value pointer))
		 (pointer-value value)))



;;;; done

)

;;; end of file
