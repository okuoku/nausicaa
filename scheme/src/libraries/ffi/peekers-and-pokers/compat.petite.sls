;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: compatibility library for Petite Chez
;;;Date: Sat Mar 20, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (ffi peekers-and-pokers compat)
  (export
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
  (import (chezscheme)
    (only (ffi pointers) integer->pointer pointer->integer))


;;;; peekers

(define (pointer-ref-c-int8 pointer offset)
  (foreign-ref 'integer-8 (pointer->integer pointer) offset))

(define (pointer-ref-c-int16 pointer offset)
  (foreign-ref 'integer-16 (pointer->integer pointer) offset))

(define (pointer-ref-c-int32 pointer offset)
  (foreign-ref 'integer-32 (pointer->integer pointer) offset))

(define (pointer-ref-c-int64 pointer offset)
  (foreign-ref 'integer-64 (pointer->integer pointer) offset))

;;; --------------------------------------------------------------------

(define (pointer-ref-c-uint8 pointer offset)
  (foreign-ref 'unsigned-8 (pointer->integer pointer) offset))

(define (pointer-ref-c-uint16 pointer offset)
  (foreign-ref 'unsigned-16 (pointer->integer pointer) offset))

(define (pointer-ref-c-uint32 pointer offset)
  (foreign-ref 'unsigned-32 (pointer->integer pointer) offset))

(define (pointer-ref-c-uint64 pointer offset)
  (foreign-ref 'unsigned-64 (pointer->integer pointer) offset))

;;; --------------------------------------------------------------------

(define (pointer-ref-c-float pointer offset)
  (foreign-ref 'single-float (pointer->integer pointer) offset))

(define (pointer-ref-c-double pointer offset)
  (foreign-ref 'single-double (pointer->integer pointer) offset))

(define (pointer-ref-c-pointer pointer offset)
  (integer->pointer (foreign-ref 'void* (pointer->integer pointer) offset)))

(define pointer-ref-c-void*	pointer-ref-c-pointer)

;;; --------------------------------------------------------------------

(define (pointer-ref-c-signed-char pointer offset)
  (foreign-ref 'integer-8 (pointer->integer pointer) offset))

(define (pointer-ref-c-unsigned-char pointer offset)
  (foreign-ref 'unsigned-8 (pointer->integer pointer) offset))

(define (pointer-ref-c-signed-short pointer offset)
  (foreign-ref 'short (pointer->integer pointer) offset))

(define (pointer-ref-c-unsigned-short pointer offset)
  (foreign-ref 'unsigned-short (pointer->integer pointer) offset))

(define (pointer-ref-c-signed-int pointer offset)
  (foreign-ref 'int (pointer->integer pointer) offset))

(define (pointer-ref-c-unsigned-int pointer offset)
  (foreign-ref 'unsigned-int (pointer->integer pointer) offset))

(define (pointer-ref-c-signed-long pointer offset)
  (foreign-ref 'long (pointer->integer pointer) offset))

(define (pointer-ref-c-unsigned-long pointer offset)
  (foreign-ref 'unsigned-long (pointer->integer pointer) offset))

(define (pointer-ref-c-signed-long-long pointer offset)
  (foreign-ref 'long-long (pointer->integer pointer) offset))

(define (pointer-ref-c-unsigned-long-long pointer offset)
  (foreign-ref 'unsigned-long-long (pointer->integer pointer) offset))


;;;; pokers

(define (pointer-set-c-int8! pointer offset value)
  (foreign-set! 'integer-8 (pointer->integer pointer) offset value))

(define (pointer-set-c-int16! pointer offset value)
  (foreign-set! 'integer-16 (pointer->integer pointer) offset value))

(define (pointer-set-c-int32! pointer offset value)
  (foreign-set! 'integer-32 (pointer->integer pointer) offset value))

(define (pointer-set-c-int64! pointer offset value)
  (foreign-set! 'integer-64 (pointer->integer pointer) offset value))

;;; --------------------------------------------------------------------

(define (pointer-set-c-uint8! pointer offset value)
  (foreign-set! 'unsigned-8 (pointer->integer pointer) offset value))

(define (pointer-set-c-uint16! pointer offset value)
  (foreign-set! 'unsigned-16 (pointer->integer pointer) offset value))

(define (pointer-set-c-uint32! pointer offset value)
  (foreign-set! 'unsigned-32 (pointer->integer pointer) offset value))

(define (pointer-set-c-uint64! pointer offset value)
  (foreign-set! 'unsigned-64 (pointer->integer pointer) offset value))

;;; --------------------------------------------------------------------

(define (pointer-set-c-float! pointer offset value)
  (foreign-set! 'single-float (pointer->integer pointer) offset value))

(define (pointer-set-c-double! pointer offset value)
  (foreign-set! 'single-double (pointer->integer pointer) offset value))

(define (pointer-set-c-pointer! pointer offset value)
  (foreign-set! 'void* (pointer->integer pointer) offset (pointer->integer value)))

(define pointer-set-c-void*!	pointer-set-c-pointer!)

;;; --------------------------------------------------------------------

(define (pointer-set-c-signed-char! pointer offset value)
  (foreign-set! 'integer-8 (pointer->integer pointer) offset value))

(define (pointer-set-c-unsigned-char! pointer offset value)
  (foreign-set! 'unsigned-8 (pointer->integer pointer) offset value))

(define (pointer-set-c-signed-short! pointer offset value)
  (foreign-set! 'short (pointer->integer pointer) offset value))

(define (pointer-set-c-unsigned-short! pointer offset value)
  (foreign-set! 'unsigned-short (pointer->integer pointer) offset value))

(define (pointer-set-c-signed-int! pointer offset value)
  (foreign-set! 'int (pointer->integer pointer) offset value))

(define (pointer-set-c-unsigned-int! pointer offset value)
  (foreign-set! 'unsigned-int (pointer->integer pointer) offset value))

(define (pointer-set-c-signed-long! pointer offset value)
  (foreign-set! 'long (pointer->integer pointer) offset value))

(define (pointer-set-c-unsigned-long! pointer offset value)
  (foreign-set! 'unsigned-long (pointer->integer pointer) offset value))

(define (pointer-set-c-signed-long-long! pointer offset value)
  (foreign-set! 'long-long (pointer->integer pointer) offset value))

(define (pointer-set-c-unsigned-long-long! pointer offset value)
  (foreign-set! 'unsigned-long-long (pointer->integer pointer) offset value))


;;;; done

)

;;; end of file
