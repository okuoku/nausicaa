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
    sleep		nanosleep
    )
  (import (rnrs)
    (nausicaa ffi)
    (nausicaa ffi sizeof)
    (nausicaa posix sizeof)
    (nausicaa posix shared-object))


;;;; type aliases

(define struct-tm*		'pointer)
(define struct-ntptimeval*	'pointer)
(define struct-timex*		'pointer)
(define struct-itimerval*	'pointer)
(define struct-timespec*	'pointer)


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
  (stime		(int nausicaa_posix_stime (double)))
  (localtime_r		(struct-tm* nausicaa_posix_localtime_r (double struct-tm*)))
  (gmtime_r		(struct-tm* nausicaa_posix_gmtime_r (double struct-tm*)))
  (timelocal		(double nausicaa_posix_timelocal (struct-tm*)))
  (timegm		(double nausicaa_posix_timegm (struct-tm*)))
  (ctime_r		(char* nausicaa_posix_ctime_r (double char*))))


;;;; done

)

;;; end of file
