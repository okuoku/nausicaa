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


(library (posix time platform)
  (export

    ;; clock ticks and CPU times
    clock		times

    ;; calendar time
    time

    ;; "struct tms" accessors
    struct-tms-tms_utime-ref		struct-tms-tms_utime-set!
    struct-tms-tms_stime-ref		struct-tms-tms_stime-set!
    struct-tms-tms_cutime-ref		struct-tms-tms_cutime-set!
    struct-tms-tms_cstime-ref		struct-tms-tms_cstime-set!

    )
  (import (rnrs)
    (foreign ffi)
    (posix shared-object)
    (posix sizeof))

  (define struct-tms*	'pointer)

  (define-c-functions/with-errno libnausicaa-posix
    (clock		(double nausicaa_posix_clock (void)))
    (time		(double nausicaa_posix_time (void))))

  (define-c-functions libnausicaa-posix
    (times		(double nausicaa_posix_times (struct-tms*))))

  (define-c-functions libnausicaa-posix
    (struct-tms-tms_utime-ref	(double nausicaa_posix_tms_utime_ref (void*)))
    (struct-tms-tms_stime-ref	(double nausicaa_posix_tms_stime_ref (void*)))
    (struct-tms-tms_cutime-ref	(double nausicaa_posix_tms_cutime_ref (void*)))
    (struct-tms-tms_cstime-ref	(double nausicaa_posix_tms_cstime_ref (void*)))
    (struct-tms-tms_utime-set!	(void nausicaa_posix_tms_utime_set (void* double)))
    (struct-tms-tms_stime-set!	(void nausicaa_posix_tms_stime_set (void* double)))
    (struct-tms-tms_cutime-set!	(void nausicaa_posix_tms_cutime_set (void* double)))
    (struct-tms-tms_cstime-set!	(void nausicaa_posix_tms_cstime_set (void* double)))))

;;; end of file
