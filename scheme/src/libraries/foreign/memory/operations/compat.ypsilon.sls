;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: compatibility raw memory operations functions for Ypsilon
;;;Date: Tue Oct 13, 2009
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


(library (foreign memory operations compat)
  (export memset memmove memcpy memcmp)
  (import (core)
    (ypsilon ffi)
    (foreign memory pointers))

  (define self (load-shared-object ""))

  (define memset
    (let* ((address (lookup-shared-object self 'memset))
	   (closure (make-cdecl-callout 'void* '(void* int void*) address)))
      (lambda (pointer value size)
	(integer->pointer (closure (pointer->integer pointer) value size)))))

  (define memmove
    (let* ((address (lookup-shared-object self 'memmove))
	   (closure (make-cdecl-callout 'void* '(void* void* size_t) address)))
      (lambda (dst src size)
	(integer->pointer (closure (pointer->integer dst) (pointer->integer src) size)))))

  (define memcpy
    (let* ((address (lookup-shared-object self 'memcpy))
	   (closure (make-cdecl-callout 'void* '(void* void* size_t) address)))
      (lambda (dst src size)
	(integer->pointer (closure (pointer->integer dst) (pointer->integer src) size)))))

  (define memcmp
    (let* ((address (lookup-shared-object self 'memcmp))
	   (closure (make-cdecl-callout 'void* '(void* void* size_t) address)))
      (lambda (ptr-a ptr-b size)
	(closure (pointer->integer ptr-a) (pointer->integer ptr-b) size)))))

;;; end of file
