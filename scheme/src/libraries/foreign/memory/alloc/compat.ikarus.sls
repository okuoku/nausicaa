;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: compatibility low level memory functions for Ikarus
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
(library (foreign memory alloc compat)
  (export
    system-free				system-malloc
    system-calloc			system-realloc
    platform-free			platform-malloc
    platform-calloc			platform-realloc)
  (import (rnrs)
    (foreign ffi sizeof)
    (except (ikarus foreign) memcpy))


;;;; low level allocation functions
;;
;;For the "system-" functions we want pointers to be exact integers; for
;;the  "platform-" functions  we want  pointers  to be  records of  type
;;"pointer".
;;
;;Ikarus deals with foreign values of type "pointer" as "pointer" values.
;;

(define self (dlopen LIBC_SHARED_OBJECT_SPEC))

(define platform-free
  ((make-c-callout 'void '(pointer))
   (dlsym self "free")))

(define platform-malloc
  ((make-c-callout 'pointer '(signed-int))
   (dlsym self "malloc")))

(define platform-calloc
  ((make-c-callout 'pointer '(signed-int signed-int))
   (dlsym self "calloc")))

(define platform-realloc
  ((make-c-callout 'pointer '(pointer signed-int))
   (dlsym self "realloc")))

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
