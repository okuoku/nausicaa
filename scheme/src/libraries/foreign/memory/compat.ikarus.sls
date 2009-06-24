;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: compatibility memory functions for Ikarus
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
    (except (ikarus foreign) memcpy))


;;;; pointers

(define pointer-null
  (integer->pointer 0))

(define (pointer-null? pointer)
  (= 0 (pointer->integer pointer)))


;;;; foreign functions

(define self (dlopen ""))

(define platform-free
  ((make-c-callout 'void '(pointer))
   (dlsym self "free")))

(define platform-malloc
  ((make-c-callout 'pointer '(signed-int))
   (dlsym self "malloc")))

(define platform-calloc
  ((make-c-callout 'pointer '(signed-int signed-int))
   (dlsym self "calloc")))

(define platform-realloc
  ((make-c-callout 'pointer '(pointer signed-int))
   (dlsym self "realloc")))

(define memset
  ((make-c-callout 'pointer '(pointer signed-int signed-int))
   (dlsym self "memset")))

(define memmove
  ((make-c-callout 'pointer '(pointer pointer signed-int))
   (dlsym self "memmove")))

(define memcpy
  ((make-c-callout 'pointer '(pointer pointer signed-int))
   (dlsym self "memcpy")))

(define memcmp
  ((make-c-callout 'signed-int '(pointer pointer signed-int))
   (dlsym self "memcmp")))


;;;; done

)

;;; end of file
