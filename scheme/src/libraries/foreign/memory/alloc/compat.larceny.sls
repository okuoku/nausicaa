;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: compatibility memory low level functions for Larceny
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


(library (foreign memory alloc compat)
  (export
    system-free		system-malloc
    system-calloc	system-realloc
    platform-free	platform-malloc
    platform-calloc	platform-realloc)
  (import (rnrs)
    (primitives foreign-procedure)
    (foreign memory pointers))


;;;; low level allocation functions
;;
;;For the "system-" functions we want pointers to be exact integers; for
;;the  "platform-" functions  we want  pointers  to be  records of  type
;;"pointer".
;;
;;See (foreign  memory pointers) for  a discussion of pointer  values in
;;Larceny.
;;

(define platform-free
  (foreign-procedure "free" '(void*) 'void))

(define platform-malloc
  (let ((f (foreign-procedure "malloc" '(unsigned) 'void*)))
    (lambda (number-of-bytes)
      (retval->pointer (f number-of-bytes)))))

(define platform-realloc
  (let ((f (foreign-procedure "realloc" '(void* unsigned) 'void*)))
    (lambda (pointer number-of-bytes)
      (retval->pointer (f pointer number-of-bytes)))))

(define platform-calloc
  (let ((f (foreign-procedure "calloc" '(unsigned unsigned) 'void*)))
    (lambda (count element-size)
      (retval->pointer (f count element-size)))))

;;; --------------------------------------------------------------------

(define (system-free pointer-integer)
  (platform-free (integer->pointer pointer-integer)))

(define (system-malloc number-of-bytes)
  (pointer->integer (platform-malloc number-of-bytes)))

(define (system-realloc pointer-integer number-of-bytes)
  (pointer->integer (platform-realloc (integer->pointer pointer-integer)
				      number-of-bytes)))

(define (system-calloc count element-size)
  (pointer->integer (platform-calloc count element-size)))


;;;; done

)

;;; end of file
