;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: direct interface for glibc time functions
;;;Date: Fri Nov  6, 2009
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


(library (foreign glibc time platform)
  (export
    ;; simple calendar time
    stime

    ;; high resolution calendar time
    gettimeofday	settimeofday
    adjtime

    ;; broken down time
    localtime_r		gmtime_r
    timelocal		timegm

    ;; high accuracy time
    ntp_gettime		ntp_adjtime)
  (import (rnrs)
    (foreign posix shared-object)
    (foreign posix sizeof)
    (foreign ffi)
    (foreign ffi sizeof))


;;;; type aliases

(define struct-tm*		'pointer)
(define struct-ntptimeval*	'pointer)
(define struct-timex*		'pointer)


(define dummy
  (shared-object standard-c-library))

;;;; high-resolution calendar

(define-c-function/with-errno gettimeofday
  (int gettimeofday (pointer pointer)))

(define-c-function/with-errno settimeofday
  (int settimeofday (pointer pointer)))

(define-c-function/with-errno adjtime
  (int adjtime (pointer pointer)))

;;;; high-accuracy clock

(define-c-function/with-errno ntp_gettime
  (int ntp_gettime (struct-timex*)))

(define-c-function/with-errno ntp_adjtime
  (int ntp_adjtime (struct-timex*)))


(define dummy2
  (shared-object libnausicaa-posix))

;;;; simple calendar time

(define-c-function/with-errno stime
  (int nausicaa_posix_stime (double)))

;;;; broken-down time

(define-c-function/with-errno localtime_r
  (struct-tm* nausicaa_posix_localtime_r (double struct-tm*)))

(define-c-function/with-errno gmtime_r
  (struct-tm* nausicaa_posix_gmtime_r (double struct-tm*)))

(define-c-function/with-errno timelocal
  (double nausicaa_posix_timelocal (struct-tm*)))

(define-c-function/with-errno timegm
  (double nausicaa_posix_timegm (struct-tm*)))


;;;; done

)

;;; end of file
