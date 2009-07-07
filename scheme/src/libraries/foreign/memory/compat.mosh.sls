;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: compatibility memory functions for Mosh
;;;Date: Thu Jun 25, 2009
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
  (import (rnrs)
    (mosh ffi)
    (foreign ffi sizeof))


;;;; foreign functions

(define self (open-shared-library ""))

(define platform-free
  (make-c-function self 'void 'free '(void*)))

(define platform-malloc
  (make-c-function self 'void* 'malloc '(int)))

(define platform-realloc
  (make-c-function self 'void* 'realloc '(void* int)))

(define platform-calloc
  (make-c-function self 'void* 'calloc '(int int)))

(define memset
  (make-c-function self 'void* 'memset '(void* int int)))

(define memmove
  (make-c-function self 'void* 'memmove '(void* void* int)))

(define memcpy
  (make-c-function self 'void* 'memcpy '(void* void* int)))

(define memcmp
  (make-c-function self 'int 'memcpy '(void* void* int)))


;;;; peekers

(define pointer-ref-c-void* pointer-ref-c-pointer)


;;;; pokers

;;;these are temporary!!!!!!!!!!!!!!!!!!!!!!!!
;; (define min-s64 (- (expt 2 (- 64 1))))
;; (define max-s64 (- (expt 2 (- 64 1)) 1))
;; (define max-u64	(- (expt 2 64) 1))

;; (define (pointer-set-c-int64! pointer index value)
;;   (if (and (<= min-s64 value) (<= value max-s64))
;;       (%pointer-set-c-int64! pointer index value)
;;     (assertion-violation 'pointer-set-c-int64!
;;       "value out of range for s64 memory poker" value)))

;; (define (pointer-set-c-uint64! pointer index value)
;;   (if (and (<= 0 value) (<= value max-u64))
;;       (%pointer-set-c-uint64! pointer index value)
;;     (assertion-violation 'pointer-set-c-uint64!
;;       "value out of range for u64 memory poker" value)))

(define-syntax define-signed-poker
  (syntax-rules ()
    ((_ ?name ?sizeof-data)
     (define ?name (case ?sizeof-data
		     ((1) pointer-set-c-int8!)
		     ((2) pointer-set-c-int16!)
		     ((4) pointer-set-c-int32!)
		     ((8) pointer-set-c-int64!))))))

(define-syntax define-unsigned-poker
  (syntax-rules ()
    ((_ ?name ?sizeof-data)
     (define ?name (case ?sizeof-data
		     ((1) pointer-set-c-uint8!)
		     ((2) pointer-set-c-uint16!)
		     ((4) pointer-set-c-uint32!)
		     ((8) pointer-set-c-uint64!))))))

(define-signed-poker pointer-set-c-signed-char!		sizeof-char)
(define-signed-poker pointer-set-c-signed-short!	sizeof-short)
(define-signed-poker pointer-set-c-signed-int!		sizeof-int)
(define-signed-poker pointer-set-c-signed-long!		sizeof-long)
(define-signed-poker pointer-set-c-signed-long-long!	sizeof-long-long)

(define-unsigned-poker pointer-set-c-unsigned-char!	sizeof-char)
(define-unsigned-poker pointer-set-c-unsigned-short!	sizeof-short)
(define-unsigned-poker pointer-set-c-unsigned-int!	sizeof-int)
(define-unsigned-poker pointer-set-c-unsigned-long!	sizeof-long)
(define-unsigned-poker pointer-set-c-unsigned-long-long! sizeof-long-long)

(define pointer-set-c-void*! pointer-set-c-pointer!)


;;;; done

)

;;; end of file
