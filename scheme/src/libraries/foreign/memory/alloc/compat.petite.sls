;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausica/Scheme
;;;Contents: compatibility library for Petite Chez
;;;Date: Sat Mar 20, 2010
;;;
;;;Abstract
;;;
;;;	Petite Chez up to version 7.9.4 does NOT support the full FFI of
;;;	Chez, so here we just define placeholders.
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
(library (foreign memory alloc compat)
  (export
    system-free				system-malloc
    system-calloc			system-realloc
    platform-free			platform-malloc
    platform-calloc			platform-realloc)
  (import (except (chezscheme)
		  foreign-procedure)
    (unimplemented)
    (only (foreign ffi pointers)
	  integer->pointer
	  pointer->integer)
    (only (foreign ffi sizeof)
	  LIBC_SHARED_OBJECT_SPEC))


;;;; helpers

(define-syntax foreign-procedure
  (syntax-rules ()
    ((_ ?func-name (?arg-type ...) ?ret-type)
     (lambda args
       (raise-unimplemented-error 'foreign-procedure
				  "Petite Chez Scheme does not support full FFI"
				  ?func-name)))))


;;;; low level allocation functions
;;
;;For the "system-" functions we want pointers to be exact integers; for
;;the  "platform-" functions  we want  pointers  to be  records of  type
;;"pointer".
;;
;;Ikarus deals with foreign values of type "pointer" as "pointer" values.
;;

(define self (load-shared-object LIBC_SHARED_OBJECT_SPEC))

(define platform-free
  (foreign-procedure "free" (void*) void))

(define platform-malloc
  (foreign-procedure "malloc" (unsigned) void*))

(define platform-calloc
  (foreign-procedure "calloc" (unsigned-int unsigned-int) void*))

(define platform-realloc
  (foreign-procedure "realloc" (void* unsigned-int) void*))

;;; --------------------------------------------------------------------

(define (system-free pointer-integer)
  (platform-free (integer->pointer pointer-integer)))

(define (system-malloc number-of-bytes)
  (pointer->integer (platform-malloc number-of-bytes)))

(define (system-calloc count element-size)
  (pointer->integer (platform-calloc count element-size)))

(define (system-realloc pointer-integer number-of-bytes)
  (pointer->integer (platform-realloc (integer->pointer pointer-integer)
				      number-of-bytes)))


;;;; done

)

;;; end of file
