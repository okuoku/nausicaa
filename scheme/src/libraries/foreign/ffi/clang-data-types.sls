;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: C language data types translation
;;;Date: Fri Oct 30, 2009
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


(library (foreign ffi clang-data-types)
  (export
    enum-clang-external-types		clang-external-types
    enum-clang-internal-types		clang-internal-types
    enum-clang-types			clang-types

    clang-external-type->clang-internal-type
    clang-internal-type->clang-type
    clang-external-type->clang-type

    clang-quote-type-stx-if-external)
  (import (rnrs)
    (foreign ffi clang-data-types compat)
    (foreign ffi sizeof))


(define-enumeration enum-clang-external-types
  ( ;;
   int8_t int16_t int32_t int64_t
   uint8_t uint16_t uint32_t uint64_t
   char schar signed-char
   uchar unsigned-char
   short signed-short
   ushort unsigned-short
   int signed-int
   ssize_t
   uint unsigned unsigned-int
   size_t
   long signed-long
   ulong unsigned-long
   long-long signed-long-long	llong
   ullong unsigned-long-long	ulong-long
   float double
   pointer void* char* FILE*
   callback void)
  clang-external-types)

(define-enumeration enum-clang-internal-types
  ( ;;
   int8_t int16_t int32_t int64_t
   uint8_t uint16_t uint32_t uint64_t
   signed-char unsigned-char
   short signed-short
   ushort unsigned-short
   signed-int unsigned-int
   signed-long unsigned-long
   signed-long-long unsigned-long-long
   float double
   callback pointer
   void)
  clang-internal-types)


(define (clang-external-type->clang-internal-type type)
  (case type
    ((int8_t)					'int8_t)
    ((int16_t)					'int16_t)
    ((int32_t)					'int32_t)
    ((int64_t)					'int64_t)
    ((uint8_t)					'uint8_t)
    ((uint16_t)					'uint16_t)
    ((uint32_t)					'uint32_t)
    ((uint64_t)					'uint64_t)
    ((char schar signed-char)			'signed-char)
    ((uchar unsigned-char)			'unsigned-char)
    ((short signed-short)			'signed-short)
    ((ushort unsigned-short)			'unsigned-short)
    ((int signed-int)				'signed-int)
    ((ssize_t)					ssize_t-integer)
    ((uint unsigned unsigned-int)		'unsigned-int)
    ((size_t)					size_t-integer)
    ((signed-long long)				'signed-long)
    ((ulong unsigned-long)			'unsigned-long)
    ((signed-long-long long-long llong)		'signed-long-long)
    ((ulong-long unsigned-long-long ullong)	'unsigned-long-long)
    ((float)					'float)
    ((double)					'double)
    ((pointer void* char* FILE*)		'pointer)
    ((callback)					'callback)
    ((void)					'void)
    (else
     (assertion-violation #f
       "C language data type unknown to Nausicaa" type))))

(define (clang-external-type->clang-type type)
  (clang-internal-type->clang-type (clang-external-type->clang-internal-type type)))


(define clang-external-types-universe
  (enum-set-universe (clang-external-types)))

(define (clang-quote-type-stx-if-external type-stx)
  (let ((type (syntax->datum type-stx)))
    (if (enum-set-member? type clang-external-types-universe)
	(quasisyntax (quote (unsyntax (datum->syntax type-stx type))))
      type-stx)))


;;;; done

)

;;; end of file
