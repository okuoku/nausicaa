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
  (export

    ;; clock ticks and CPU times
    clock		times

    ;; calendar time
    time

    ;; "struct tms" accessors
    struct-tms-tms_utime-ref
    struct-tms-tms_stime-ref
    struct-tms-tms_cutime-ref
    struct-tms-tms_cstime-ref

    )
  (import (rnrs)
    (foreign posix shared-object)
    (foreign ffi)
    (foreign posix sizeof))


(define dummy
  (shared-object standard-c-library))


;;; --------------------------------------------------------------------

(define dummy2
  (shared-object libnausicaa-posix))

;;; CPU ticks and process ticks

(define-c-function/with-errno clock
  (double nausicaa_posix_clock (void)))

(define-c-function times
  (double nausicaa_posix_times (pointer)))

;;;; calendar time

(define-c-function/with-errno time
  (double nausicaa_posix_time (pointer)))

;;;; "struct tms" accessors

(define-c-function struct-tms-tms_utime-ref
  (double nausicaa_posix_tms_utime_ref (void*)))

(define-c-function struct-tms-tms_stime-ref
  (double nausicaa_posix_tms_stime_ref (void*)))

(define-c-function struct-tms-tms_cutime-ref
  (double nausicaa_posix_tms_cutime_ref (void*)))

(define-c-function struct-tms-tms_cstime-ref
  (double nausicaa_posix_tms_cstime_ref (void*)))


;;;; done

)

;;; end of file
