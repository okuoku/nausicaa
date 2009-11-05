;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: direct interface for time functions
;;;Date: Wed Nov  4, 2009
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


(library (foreign posix time platform)
  (export clock times)
  (import (rnrs)
    (foreign ffi)
    (foreign posix sizeof))


(define dummy
  (shared-object self-shared-object))

;; (define stub-lib
;;   (let ((o (open-shared-object 'libnausicaa-posix1.so)))
;;     (shared-object o)
;;     o))


;;;; CPU ticks and process ticks

(define-c-function/with-errno clock
  (double nausicaa_posix_clock (void)))

(define-c-function/with-errno times
  (double nausicaa_posix_times (pointer)))


;;;; done

)

;;; end of file
