;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: FFI types for Mosh
;;;Date: Mon Nov 30, 2009
;;;
;;;Abstract
;;;
;;;	According to "lib/mosh/ffi.ss" (revision 2185):
;;;
;;;* The accepted return values for callouts are:
;;;
;;;  void
;;;  bool		char		size_t
;;;  short		int		long		long-long
;;;  unsigned-short	unsigned-int	unsigned-long	unsigned-long-long
;;;  int8_t		int16_t		int32_t		int64_t
;;;  uint8_t		uint16_t	uint32_t	uint64_t
;;;  float		double
;;;  void*
;;;
;;;* The accepted arguments for callouts are:
;;;
;;;  void
;;;  bool		char		size_t
;;;  short		int		long		long-long
;;;  unsigned-short	unsigned-int	unsigned-long	unsigned-long-long
;;;  int8_t		int16_t		int32_t		int64_t
;;;  uint8_t		uint16_t	uint32_t	uint64_t
;;;  float		double
;;;  char*		void*
;;;
;;;  an empty list represents no arguments.
;;;
;;;* The accepted return values for callbacks are:
;;;
;;;  void
;;;  bool		char		size_t
;;;  short		int		long		long-long
;;;  unsigned-short	unsigned-int	unsigned-long	unsigned-long-long
;;;  int8_t		int16_t		int32_t		int64_t
;;;  uint8_t		uint16_t	uint32_t	uint64_t
;;;  float		double
;;;  void*
;;;
;;;* The accepted arguments for callbacks are:
;;;
;;;  void
;;;  bool		char		size_t
;;;  short		int		long		long-long
;;;  unsigned-short	unsigned-int	unsigned-long	unsigned-long-long
;;;  int8_t		int16_t		int32_t		int64_t
;;;  uint8_t		uint16_t	uint32_t	uint64_t
;;;  float		double
;;;  void*
;;;
;;;  an empty list represents no arguments.
;;;
;;;Copyright (c) 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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
   int8_t int16_t int32_t int64_t
   uint8_t uint16_t uint32_t uint64_t
   char short unsigned-short
   int unsigned-int long unsigned-long
   long-long unsigned-long-long
   float double pointer void bool)
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
    ((uint64_t)				'uint64_t)
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
       "C language type identifier unknown by Mosh Scheme" type))))


;;;; done

)

;;; end of file
