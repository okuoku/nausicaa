;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: FFI types for Ypsilon
;;;Date: Mon Nov 30, 2009
;;;
;;;Abstract
;;;
;;;In Ypsilon  revision 503, it appears  that an argument  to a function
;;;can be one among:
;;;
;;;  bool		char		size_t
;;;  short		int		long		long-long
;;;  unsigned-short	unsigned-int	unsigned-long	unsigned-long-long
;;;  float		double
;;;  void*
;;;  int8_t		int16_t		int32_t		int64_t
;;;  uint8_t		uint16_t	uint32_t	uint64_t
;;;
;;;the return type of a callout function can be one among:
;;;
;;;  void
;;;  bool		char		size_t
;;;  short		int		long		long-long
;;;  unsigned-short	unsigned-int	unsigned-long	unsigned-long-long
;;;  float		double
;;;  void*		char*
;;;  int8_t		int16_t		int32_t		int64_t
;;;  uint8_t		uint16_t	uint32_t	uint64_t
;;;
;;;the return type of a callback function can be one among:
;;;
;;;  void
;;;  bool		char		size_t
;;;  short		int		long		long-long
;;;  unsigned-short	unsigned-int	unsigned-long	unsigned-long-long
;;;  float		double
;;;  void*
;;;  int8_t		int16_t		int32_t		int64_t
;;;  uint8_t		uint16_t	uint32_t	uint64_t
;;;
;;;Care  must  be  taken  in  selecting  types,  because:
;;;
;;;* Selecting "void*" as Ypsilon  type will cause Ypsilon to allocate a
;;;  bytevector and use it as value.
;;;
;;;* Selecting "char*" as Ypsilon  type will cause Ypsilon to allocate a
;;;  string and use it as value.
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


(define-enumeration enum-clang-types
  ( ;;
   bool			char		size_t
   short		int		long		long-long
   unsigned-short	unsigned-int	unsigned-long	unsigned-long-long
   float		double
   void*
   int8_t		int16_t		int32_t		int64_t
   uint8_t		uint16_t	uint32_t	uint64_t)
  clang-types)

(define (clang-internal-type->clang-type type)
  (case type
    ((int8_t)				'int8_t)
    ((int16_t)				'int16_t)
    ((int32_t)				'int32_t)
    ((int64_t)				'int64_t)
    ((uint8_t)				'uint8_t)
    ((uint16_t)				'uint16_t)
    ((uint32_t)				'uint32_t)
    ((uint64_t)				'uint32_t)
    ((signed-char)			'char)
    ((unsigned-char)			'char)
    ((signed-short)			'short)
    ((unsigned-short)			'unsigned-short)
    ((signed-int)			'int)
    ((unsigned-int)			'unsigned-int)
    ((signed-long)			'long)
    ((unsigned-long)			'unsigned-long)
    ((signed-long-long)			'long-long)
    ((unsigned-long-long)		'unsigned-long-long)
    ((float)				'float)
    ((double)				'double)
    ((pointer)				'void*)
    ((callback)				'void*)
    ((void)				'void)
    (else
     (assertion-violation #f
       "C language data type unknown to Ypsilon Scheme" type))))


;;;; done

)

;;; end of file
