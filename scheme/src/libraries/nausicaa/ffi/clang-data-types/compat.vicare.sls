;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: FFI types for Vicare Scheme
;;;Date: Mon Nov 30, 2009
;;;
;;;Abstract
;;;
;;;
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
   signed-char unsigned-char signed-short unsigned-short
   signed-int unsigned-int signed-long unsigned-long
   signed-long-long unsigned-long-long
   float double pointer void)
  clang-types)

(define (clang-internal-type->clang-type type)
  (case type
    ((int8_t)				'signed-char)
    ((int16_t)				'signed-short)
    ((int32_t)				'signed-int)
    ((int64_t)				'signed-long-long)
    ((uint8_t)				'unsigned-char)
    ((uint16_t)				'unsigned-short)
    ((uint32_t)				'unsigned-int)
    ((uint64_t)				'unsigned-long-long)
    ((signed-char)			'signed-char)
    ((unsigned-char)			'unsigned-char)
    ((signed-short)			'signed-short)
    ((unsigned-short)			'unsigned-short)
    ((signed-int)			'signed-int)
    ((unsigned-int)			'unsigned-int)
    ((signed-long)			'signed-long)
    ((unsigned-long)			'unsigned-long)
    ((signed-long-long)			'signed-long-long)
    ((unsigned-long-long)		'unsigned-long-long)
    ((float)				'float)
    ((double)				'double)
    ((pointer)				'pointer)
    ((callback)				'pointer)
    ((void)				'void)
    (else
     (assertion-violation #f
       "C language type identifier is unknown by Vicare Scheme" type))))


;;;; done

)

;;; end of file
