;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: compatibility low level memory functions for Ypsilon
;;;Date: Tue Oct 13, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008-2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa ffi memory alloc compat)
  (export
    system-free				system-malloc
    system-calloc			system-realloc
    platform-free			platform-malloc
    platform-calloc			platform-realloc)
  (import (core)
    (ypsilon ffi)
    (prefix (nausicaa ffi sizeof) ffi:)
    (nausicaa ffi pointers))


;;;; low level allocation functions
;;
;;For the "system-" functions we want pointers to be exact integers; for
;;the  "platform-" functions  we want  pointers  to be  records of  type
;;"pointer".
;;
;;Ypsilon  itself deals  with foreign  values of  type "void*"  as exact
;;integers.
;;

(define self (load-shared-object ffi:LIBC_SHARED_OBJECT_SPEC))

(define system-free
  (make-cdecl-callout 'void '(void*) (lookup-shared-object self 'free)))

(define system-malloc
  (make-cdecl-callout 'void* '(size_t) (lookup-shared-object self 'malloc)))

(define system-realloc
  (make-cdecl-callout 'void* '(void* size_t) (lookup-shared-object self 'realloc)))

(define system-calloc
  (make-cdecl-callout 'void* '(size_t size_t) (lookup-shared-object self 'calloc)))

;;; --------------------------------------------------------------------

(define (platform-free pointer)
  (system-free (pointer->integer pointer)))

(define (platform-malloc number-of-bytes)
  (integer->pointer (system-malloc number-of-bytes)))

(define (platform-realloc pointer number-of-bytes)
  (integer->pointer (system-realloc (pointer->integer pointer)
				    number-of-bytes)))

(define (platform-calloc count element-size)
  (integer->pointer (system-calloc count element-size)))


;;;; done

)

;;; end of file
