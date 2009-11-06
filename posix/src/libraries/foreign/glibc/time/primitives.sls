;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: marshaling interface for glibc time functions
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


(library (foreign glibc time primitives)
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
    (receive)
    (compensations)
    (only (foreign errno)
	  raise-errno-error)
    (prefix (foreign glibc time platform) platform:))


;;;; simple calendar time

(define (time)
  (receive (result errno)
      (platform:time pointer-null)
    (if (= -1 result)
	(raise-errno-error 'time errno)
      result)))

(define (stime)
  (receive (result errno)
      (platform:stime pointer-null)
    (if (= -1 result)
	(raise-errno-error 'stime errno)
      result)))


;;;; high resolution calendar

(define (gettimeofday timeval timezone)
  (receive (result errno)
      (platform:gettimeofday timeval timezone)
    (if (= -1 result)
	(raise-errno-error 'gettimeofday errno)
      result)))

(define (settimeofday timeval timezone)
  (receive (result errno)
      (platform:settimeofday timeval timezone)
    (if (= -1 result)
	(raise-errno-error 'settimeofday errno)
      result)))

(define (adjtime timeval-delta timeval-old-delta)
  (receive (result errno)
      (platform:adjtime timeval-delta timeval-old-delta)
    (if (= -1 result)
	(raise-errno-error 'adjtime errno)
      result)))


;;;; broken-down time

(define (localtime time *tm)
  (receive (result errno)
      (with-compensations
	(let ((*time (malloc-small/c)))
	  (poke-time_t! *time 0 time)
	  (platform:localtime *time *tm)))
    (if (pointer=? *tm result)
	*tm
      (raise-errno-error 'localtime errno))))

(define (gmtime time *tm)
  (receive (result errno)
      (with-compensations
	(let ((*time (malloc-small/c)))
	  (poke-time_t! *time 0 time)
	  (platform:gmtime *time *tm)))
    (if (pointer=? *tm result)
	*tm
      (raise-errno-error 'gmtime errno))))

(define (timelocal *tm)
  (receive (result errno)
      (platform:timelocal *tm)
    (if (= -1 result)
	(raise-errno-error 'timelocal errno)
      result)))

(define (timegm *tm)
  (receive (result errno)
      (platform:timegm *tm)
    (if (= -1 result)
	(raise-errno-error 'timegm errno)
      result)))


;;;; high accuracy time

(define (ntp_gettime *ntptimeval)
  (format #t "p ~s~%" *ntptimeval)
  (receive (result errno)
      (platform:ntp_gettime *ntptimeval)
    (format #t "res  ~s~%" result)
    (if (= 0 result)
	result
      (raise-errno-error 'ntp_gettime errno))))


;;;; done

)

;;; end of file
