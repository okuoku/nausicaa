;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: FFI types for Larceny Scheme
;;;Date: Mon Nov 30, 2009
;;;
;;;Abstract
;;;
;;;Larceny revision 6404 supports  the following data types for function
;;;arguments and return values:
;;;
;;;  void
;;;  int	unsigned
;;;  long	unsigned-long
;;;  float	double
;;;  void*	(maybe void*)
;;;
;;;The  ugly "(maybe  void*)" represents  a pointer  which can  be NULL.
;;;When not NULL, it is a record of type void*-rt; when NULL it is #f.
;;;
;;;Copyright (c) 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (foreign ffi clang-data-types compat)
  (export enum-clang-types clang-types clang-internal-type->clang-type)
  (import (rnrs))


(define-enumeration enum-implementation-data-types
  ( ;;
   void
   int	unsigned
   long	unsigned-long
   float	double
   void*)
  implementation-data-types)

(define (internal-type->implementation-type type)
  (case type
    ((int8_t)				'int)
    ((int16_t)				'int)
    ((int32_t)				'int)
    ((int64_t)				'long)
    ((uint8_t)				'unsigned)
    ((uint16_t)				'unsigned)
    ((uint32_t)				'unsigned)
    ((uint64_t)				'ulong)
    ((signed-char)			'int)
    ((unsigned-char)			'int)
    ((signed-short)			'int)
    ((unsigned-short)			'unsigned)
    ((signed-int)			'int)
    ((unsigned-int)			'unsigned)
    ((signed-long)			'long)
    ((unsigned-long)			'ulong)
    ((signed-long-long)			'long)
    ((unsigned-long-long)		'long-long)
    ((float)				'float)
    ((double)				'double)
    ((pointer)				'(maybe void*))
    ((callback)				'(maybe void*))
    ((void)				'void)
    (else
     (assertion-violation #f
       "C language type identifier is unknown by Larceny Scheme" type))))


;;;; done

)

;;; end of file
