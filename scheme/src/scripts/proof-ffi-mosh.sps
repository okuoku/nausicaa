;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: test script for Mosh's FFI
;;;Date: Sat Jul  4, 2009
;;;
;;;Abstract
;;;
;;;
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

(import (rnrs) (mosh ffi))

(define libc (open-shared-library ""))
(define platform-malloc (c-function libc void* malloc int))
(define p (platform-malloc 4096))

;; (define value (- (expt 2 64) 1))
;; (pointer-set-c-uint64! p 0 value)
;; (write (list value (pointer-ref-c-uint64 p 0)))
;; (newline)

(define value (+ 200 (- (expt 2 64) 1)))
(pointer-set-c-uint64! p 0 value)
(write (list value (pointer-ref-c-uint64 p 0)))
(newline)

;;; end of file
