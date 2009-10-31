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
    nausicaa-external-types
    nausicaa-internal-types
    external-type->internal-type
    %normalise-and-maybe-quote-type)
  (import (rnrs)
    (for (only (rnrs) quote) (meta -1))
    (foreign ffi sizeof))


(define (external-type->internal-type type)
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

(define (%normalise-and-maybe-quote-type type-stx)
  (let ((type (external-type->internal-type (syntax->datum type-stx))))
    (with-syntax ((TYPE (datum->syntax type-stx type)))
      (if (enum-set-member? type nausicaa-internal-types)
	  (syntax (quote TYPE))
	(syntax TYPE)))))


(define nausicaa-external-types
  (make-enumeration '(int8_t int16_t int32_t int64_t
		      uint8_t uint16_t uint32_t uint64_t
		      char schar signed-char
		      uchar unsigned-char
		      short signed-short
		      ushort unsigned-short
		      int signed-int
		      ssize_t
		      uint unsigned unsigned-int
		      size_t
		      signed-long long
		      ulong unsigned-long llong
		      signed-long-long long-long ullong
		      ulong-long unsigned-long-long
		      float double
		      pointer void* char* FILE*
		      callback void)))

(define nausicaa-internal-types
  (make-enumeration '(int8_t int16_t int32_t int64_t
		      uint8_t uint16_t uint32_t uint64_t
		      signed-char	unsigned-char
		      short		signed-short
		      ushort		unsigned-short
		      signed-int	unsigned-int
		      signed-long	unsigned-long
		      signed-long-long	unsigned-long-long
		      float		double
		      callback		pointer
		      void)))


;;;; done

)

;;; end of file
