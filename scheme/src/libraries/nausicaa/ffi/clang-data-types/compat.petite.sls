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
(library (nausicaa ffi clang-data-types compat)
  (export enum-clang-types clang-types clang-internal-type->clang-type)
  (import (rnrs))


(define-enumeration enum-clang-types
  ( ;;
   char		unsigned-char
   short	unsigned-short
   int		unsigned-int
   long		unsigned-long
   long-long	unsigned-long-long
   fixed-float	fixed-double
   void*
   integer-8	unsigned-8
   integer-16	unsigned-16
   integer-32	unsigned-32
   integer-64	unsigned-64
   single-float	double-float)
  clang-types)

(define (clang-internal-type->clang-type type)
  (case type
    ((int8_t)				'integer-8)
    ((int16_t)				'integer-16)
    ((int32_t)				'integer-32)
    ((int64_t)				'integer-64)
    ((uint8_t)				'unsigned-8)
    ((uint16_t)				'unsigned-16)
    ((uint32_t)				'unsigned-32)
    ((uint64_t)				'unsigned-64)
    ((signed-char)			'integer-8)
    ((unsigned-char)			'unsigned-8)
    ((signed-short)			'short)
    ((unsigned-short)			'unsigned-short)
    ((signed-int)			'int)
    ((unsigned-int)			'unsigned-int)
    ((signed-long)			'long)
    ((unsigned-long)			'unsigned-long)
    ((signed-long-long)			'long-long)
    ((unsigned-long-long)		'unsigned-long-long)
    ((float)				'fixed-float)
    ((double)				'fixed-double)
    ((pointer)				'void*)
    ((callback)				'void*)
    ((void)				'void)
    (else
     (assertion-violation #f
       "C language type identifier is unknown by Petite Chez Scheme" type))))


;;;; done

)

;;; end of file
