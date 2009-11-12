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
    stime

    ;; high resolution calendar time
    gettimeofday	settimeofday
    adjtime

    ;; broken down time
    localtime		localtime*
    gmtime		gmtime*
    timelocal		timelocal*
    timegm		timegm*

    ;; high accuracy time
    ntp_gettime		ntp_gettime*
    ntp_adjtime		ntp_adjtime*

    ;; formatting broken-down time
    asctime		asctime*
    ctime
    strftime		strftime*

    ;; parsing time strings
    strptime		strptime*

    ;; setting alarms
    setitimer		setitimer*
    getitimer		getitimer*
    alarm
    )
  (import (rnrs)
    (receive)
    (compensations)
    (only (foreign ffi sizeof)
	  valueof-int-max)
    (only (foreign ffi pointers)
	  pointer-null
	  pointer-null?
	  pointer=?)
    (only (foreign ffi peekers-and-pokers)
	  pointer-ref-c-uint8)
    (only (foreign memory)
	  malloc-small/c
	  malloc-block/c)
    (only (foreign cstrings)
	  cstring->string
	  string->cstring/c)
    (only (foreign errno)
	  raise-errno-error)
    (foreign posix sizeof)
    (foreign posix time record-types)
    (prefix (foreign glibc time platform) platform:))


;;;; simple calendar time

(define (stime calendar-time)
  (receive (result errno)
      (platform:stime calendar-time)
    (if (= -1 result)
	(raise-errno-error 'stime errno calendar-time)
      result)))


;;;; high resolution calendar

(define (gettimeofday)
  (with-compensations
    (let ((timeval*	(malloc-block/c sizeof-struct-timeval))
	  (timezone*	(malloc-block/c sizeof-struct-timezone)))
      (receive (result errno)
	  (platform:gettimeofday timeval* timezone*)
	(if (= -1 result)
	    (raise-errno-error 'gettimeofday errno)
	  (values (struct-timeval->record  timeval*)
		  (struct-timezone->record timezone*)))))))

(define (settimeofday timeval timezone)
  (with-compensations
    (let ((timeval*	(record->struct-timeval  timeval  malloc-block/c))
	  (timezone*	(record->struct-timezone timezone malloc-block/c)))
      (receive (result errno)
	  (platform:settimeofday timeval* timezone*)
	(if (= -1 result)
	    (raise-errno-error 'settimeofday errno (list timeval timezone))
	  result)))))

(define (adjtime timeval-delta)
  (with-compensations
    (let ((timeval-delta*	(record->struct-timeval timeval-delta malloc-block/c))
	  (timeval-old-delta*	(malloc-block/c sizeof-struct-timeval)))
      (receive (result errno)
	  (platform:adjtime timeval-delta* timeval-old-delta*)
	(if (= -1 result)
	    (raise-errno-error 'adjtime errno timeval-delta)
	  (values result (struct-timeval->record timeval-old-delta*)))))))


;;;; broken-down time

(define (localtime time malloc)
  (let ((tm* (malloc sizeof-struct-tm)))
    (receive (result errno)
	(platform:localtime_r time tm*)
      (if (pointer=? tm* result)
	  tm*
	(raise-errno-error 'localtime errno time)))))

(define (localtime* time)
  (with-compensations
    (struct-tm->record (localtime time malloc-block/c))))

(define (gmtime time malloc)
  (let ((tm* (malloc sizeof-struct-tm)))
    (receive (result errno)
	(platform:gmtime_r time tm*)
      (if (pointer=? tm* result)
	  tm*
	(raise-errno-error 'gmtime errno time)))))

(define (gmtime* time)
  (with-compensations
    (struct-tm->record (gmtime time malloc-block/c))))

(define (timelocal tm*)
  (receive (result errno)
      (platform:timelocal tm*)
    (if (= -1 result)
	(raise-errno-error 'timelocal errno tm*)
      result)))

(define (timelocal* tm-record)
  (with-compensations
    (timelocal (record->struct-tm tm-record malloc-block/c))))

(define (timegm tm*)
  (receive (result errno)
      (platform:timegm tm*)
    (if (= -1 result)
	(raise-errno-error 'timegm errno tm*)
      result)))

(define (timegm* tm-record)
  (with-compensations
    (timegm (record->struct-tm tm-record malloc-block/c))))


;;;; high-accuracy time

(define (ntp_gettime ntptimeval*)
  (receive (result errno)
      (platform:ntp_gettime ntptimeval*)
    (if (= 0 result)
	result
      (raise-errno-error 'ntp_gettime errno ntptimeval*))))

(define (ntp_gettime*)
  (with-compensations
    (let ((ntptimeval* (malloc-block/c sizeof-struct-ntptimeval)))
      (platform:ntp_gettime ntptimeval*)
      (struct-ntptimeval->record ntptimeval*))))

(define (ntp_adjtime timex*)
  (receive (result errno)
      (platform:ntp_adjtime timex*)
    (if (= 0 result)
	result
      (raise-errno-error 'ntp_adjtime errno timex*))))

(define (ntp_adjtime* record)
  (with-compensations
    (let ((timex* (record->struct-timex record malloc-block/c)))
      (platform:ntp_gettime timex*)
      (struct-timex->record timex*))))


;;;; formatting broken-down time

(define (asctime struct-tm*)
  (with-compensations
    (let ((cstr (malloc-block/c 26)))
      (receive (result errno)
	  (platform:asctime_r struct-tm* cstr)
	(if (pointer-null? cstr)
	    (raise-errno-error 'asctime errno struct-tm*)
	  (cstring->string cstr))))))

(define (asctime* tm-record)
  (with-compensations
    (asctime (record->struct-tm tm-record malloc-block/c))))

(define (ctime calendar-time)
  (with-compensations
    (let ((cstr (malloc-block/c 26)))
      (receive (result errno)
	  (platform:ctime_r calendar-time cstr)
	(if (pointer-null? cstr)
	    (raise-errno-error 'ctime errno calendar-time)
	  (cstring->string cstr))))))

(define (strftime template struct-tm*)
  ;;*FIXME* Fixed  size for the output  buffer is UGLY!!!   But read the
  ;;documentation of "strftime()".
  (with-compensations
    (let* ((c-template		(string->cstring/c template))
	   (proposed-len	4096)
	   (output		(malloc-block/c proposed-len)))
      (receive (required-len errno)
	  (platform:strftime output proposed-len c-template struct-tm*)
	(if (= 0 required-len)
	    (if (= 0 errno)
		(error 'strftime "error formating output time/date string" (list template struct-tm*))
	      (raise-errno-error 'strftime errno (list template struct-tm*)))
	  (cstring->string output required-len))))))

(define (strftime* template tm-record)
  (with-compensations
    (strftime template (record->struct-tm tm-record malloc-block/c))))


;;;; parsing time strings

(define (strptime input-string template-string struct-tm*)
  (with-compensations
    (let* ((input*	(string->cstring/c input-string))
	   (template*	(string->cstring/c template-string))
	   (result	(platform:strptime input* template* struct-tm*)))
      (when (or (pointer-null? result)
		(not (= 0 (pointer-ref-c-uint8 result 0))))
	(error 'strptime
	  "unable to parse date/time string according to the template"
	  (list input-string template-string))))))

(define (strptime* input-string template-string)
  (with-compensations
    (let ((struct-tm* (malloc-block/c sizeof-struct-tm)))
      (struct-tm-tm_sec-set!    struct-tm* valueof-int-max)
      (struct-tm-tm_min-set!    struct-tm* valueof-int-max)
      (struct-tm-tm_hour-set!   struct-tm* valueof-int-max)
      (struct-tm-tm_mday-set!   struct-tm* valueof-int-max)
      (struct-tm-tm_mon-set!    struct-tm* valueof-int-max)
      (struct-tm-tm_year-set!   struct-tm* valueof-int-max)
      (struct-tm-tm_wday-set!   struct-tm* valueof-int-max)
      (struct-tm-tm_yday-set!   struct-tm* valueof-int-max)
      (struct-tm-tm_isdst-set!  struct-tm* valueof-int-max)
      (struct-tm-tm_gmtoff-set! struct-tm* valueof-int-max)
      (struct-tm-tm_zone-set!   struct-tm* pointer-null)
      (strptime input-string template-string struct-tm*)
      (struct-tm->record struct-tm*))))


;;;; setting alarms

(define (setitimer which itimerval-new* itimerval-old*)
  (receive (result errno)
      (platform:setitimer which itimerval-new* itimerval-old*)
    (if (= -1 result)
	(raise-errno-error 'setitimer errno (list which itimerval-new* itimerval-old*))
      result)))

(define (getitimer which itimerval-old*)
  (receive (result errno)
      (platform:getitimer which itimerval-old*)
    (if (= -1 result)
	(raise-errno-error 'getitimer errno (list which itimerval-old*))
      result)))

(define (alarm seconds)
  (receive (result errno)
      (platform:alarm seconds)
    (if (= -1 result)
	(raise-errno-error 'alarm errno seconds)
      result)))

;;; --------------------------------------------------------------------

(define (setitimer* which itimerval-record)
  (with-compensations
    (let ((itimerval-new*	(record->struct-itimerval itimerval-record malloc-block/c))
	  (itimerval-old*	(malloc-block/c sizeof-struct-itimerval)))
      (setitimer which itimerval-new* itimerval-old*)
      (struct-itimerval->record itimerval-old*))))

(define (getitimer* which)
  (with-compensations
    (let ((itimerval-old* (malloc-block/c sizeof-struct-itimerval)))
      (getitimer which itimerval-old*)
      (struct-itimerval->record itimerval-old*))))


;;;; done

)

;;; end of file
