;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: test script for Mosh's FFI
;;;Date: Sat Jul  4, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
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
;; (pointer-set-c-int8! p 0 1)
;; (pointer-set-c-int16! p 0 1)
;; (pointer-set-c-int32! p 0 1)
;; (pointer-set-c-int64! p 0 1)
;; (pointer-set-c-uint8! p 0 1)
;; (pointer-set-c-uint16! p 0 1)
;; (pointer-set-c-uint32! p 0 1)
(pointer-set-c-uint64! p 0 (- (expt 2 6) 1 ))

;;; end of file
