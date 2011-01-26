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
;;;Copyright (c) 2009, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa glibc time platform)
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
    ntp_gettime		ntp_adjtime

    ;; formatting broken-down time
    asctime_r		ctime_r
    strftime

    ;; parsing time strings
    strptime

    ;; setting alarms
    setitimer		getitimer
    alarm

    ;; sleeping
    sleep		nanosleep)
  (import (rnrs)
    (nausicaa ffi)
    (nausicaa posix sizeof)
    (nausicaa posix shared-object)
    (nausicaa posix clang type-translation))


(define-c-functions/with-errno libc-shared-object
  (gettimeofday		(int gettimeofday (pointer pointer)))
  (settimeofday		(int settimeofday (pointer pointer)))
  (adjtime		(int adjtime (pointer pointer)))
  (ntp_gettime		(int ntp_gettime (struct-timex*)))
  (ntp_adjtime		(int ntp_adjtime (struct-timex*)))
  (asctime_r		(char* asctime_r (struct-tm* char*)))
  (strftime		(size_t strftime (char* size_t char* struct-tm*)))
  (setitimer		(int setitimer (int struct-itimerval* struct-itimerval*)))
  (getitimer		(int getitimer (int struct-itimerval*)))
  (alarm		(unsigned alarm (unsigned)))
  (nanosleep		(int nanosleep (struct-timespec* struct-timespec*))))

(define-c-functions libc-shared-object
  (strptime		(char* strptime (char* char* struct-tm*)))
  (sleep		(unsigned sleep (unsigned))))

(define-c-functions/with-errno libnausicaa-posix
  (%stime		(int nausicaa_posix_stime (double)))
  (%localtime_r		(struct-tm* nausicaa_posix_localtime_r (double struct-tm*)))
  (%gmtime_r		(struct-tm* nausicaa_posix_gmtime_r (double struct-tm*)))
  (%timelocal		(double nausicaa_posix_timelocal (struct-tm*)))
  (%timegm		(double nausicaa_posix_timegm (struct-tm*)))
  (%ctime_r		(char* nausicaa_posix_ctime_r (double char*))))

(define (stime x)
  (let-values (((retval errno) (%stime x)))
    (values (exact retval) errno)))

(define (localtime_r x p)
  (%localtime_r (inexact x) p))

(define (gmtime_r x p)
  (%gmtime_r (inexact x) p))

(define (timelocal p)
  (let-values (((retval errno) (%timelocal p)))
    (values (exact retval) errno)))

(define (timegm p)
  (let-values (((retval errno) (%timegm p)))
    (values (exact retval) errno)))

(define (ctime_r x p)
  (let-values (((retval errno) (%ctime_r (inexact x) p)))
    (values retval errno)))


;;;; done

)

;;; end of file
