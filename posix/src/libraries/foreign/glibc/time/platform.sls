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
    time		stime

    ;; high resolution calendar time
    gettimeofday	settimeofday
    adjtime

    ;; broken down time
    localtime		gmtime
    timelocal		timegm
    (rename (timelocal mktime))

    ;; high accuracy time
    ntp_gettime)
  (import (rnrs)
    (foreign ffi)
    (foreign ffi sizeof))

  (define dummy
    (shared-object self-shared-object))


(define-c-function/with-errno time
  (time_t time (pointer)))

(define-c-function/with-errno stime
  (time_t stime (pointer)))

;;; --------------------------------------------------------------------

(define-c-function/with-errno gettimeofday
  (int gettimeofday (pointer pointer)))

(define-c-function/with-errno settimeofday
  (int settimeofday (pointer pointer)))

(define-c-function/with-errno adjtime
  (int adjtime (pointer pointer)))

;;; --------------------------------------------------------------------

(define-c-function/with-errno localtime
  (pointer localtime_r (pointer pointer)))

(define-c-function/with-errno gmtime
  (pointer gmtime_r (pointer pointer)))

(define-c-function/with-errno timelocal
  (time_t timelocal (pointer)))

(define-c-function/with-errno timegm
  (time_t timegm (pointer)))

;;; --------------------------------------------------------------------

(define-c-function/with-errno ntp_gettime
  (int ntp_gettime (pointer)))


;;;; done

)

;;; end of file
