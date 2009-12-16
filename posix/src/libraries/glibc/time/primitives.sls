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


(library (glibc time primitives)
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

    ;; sleeping
    (rename (platform:sleep	sleep))
    nanosleep		nanosleep*

    ;; structure/record conversion
    <struct-timespec>->pointer		pointer-><struct-timespec>
    <struct-timezone>->pointer		pointer-><struct-timezone>
    <struct-tm>->pointer		pointer-><struct-tm>
    <struct-ntptimeval>->pointer	pointer-><struct-ntptimeval>
    <struct-timex>->pointer		pointer-><struct-timex>
    <struct-itimerval>->pointer		pointer-><struct-itimerval>
    )
  (import (rnrs)
    (begin0)
    (receive)
    (compensations)
    (foreign ffi)
    (only (foreign ffi sizeof) valueof-int-max)
    (only (foreign memory) malloc-small/c malloc-block/c)
    (only (foreign cstrings) cstring->string string->cstring/c)
    (only (foreign errno) raise-errno-error)
    (posix sizeof)
    (posix typedefs)
    (prefix (glibc time platform) platform:)
    (only (posix time primitives)
	  pointer-><struct-timeval>
	  <struct-timeval>->pointer))


(define (pointer-><struct-timespec> pointer)
  (make-<struct-timespec> (struct-timespec-tv_sec-ref  pointer)
			  (struct-timespec-tv_nsec-ref pointer)))

(define (<struct-timespec>->pointer record malloc)
  (begin0-let ((pointer (malloc sizeof-timespec)))
    (struct-timespec-tv_sec-set!  pointer (<struct-timespec>-sec  record))
    (struct-timespec-tv_nsec-set! pointer (<struct-timespec>-nsec record))))

;;; --------------------------------------------------------------------

(define (pointer-><struct-timezone> pointer)
  (make-<struct-timezone> (struct-timezone-tz_minuteswest-ref  pointer)
			  (struct-timezone-tz_dsttime-ref      pointer)))

(define (<struct-timezone>->pointer record malloc)
  (begin0-let ((pointer (malloc sizeof-timezone)))
    (struct-timezone-tz_minuteswest-set! pointer (<struct-timezone>-minuteswest record))
    (struct-timezone-tz_dsttime-set!     pointer (<struct-timezone>-dsttime     record))))

;;; --------------------------------------------------------------------

(define (pointer-><struct-tm> pointer)
  (make-<struct-tm> (struct-tm-tm_sec-ref    pointer)
		    (struct-tm-tm_min-ref    pointer)
		    (struct-tm-tm_hour-ref   pointer)
		    (struct-tm-tm_mday-ref   pointer)
		    (struct-tm-tm_mon-ref    pointer)
		    (struct-tm-tm_year-ref   pointer)
		    (struct-tm-tm_wday-ref   pointer)
		    (struct-tm-tm_yday-ref   pointer)
		    (struct-tm-tm_isdst-ref  pointer)
		    (struct-tm-tm_gmtoff-ref pointer)
		    (struct-tm-tm_zone-ref pointer)))

(define (<struct-tm>->pointer record malloc)
  (begin0-let ((pointer (malloc sizeof-tm)))
    (struct-tm-tm_sec-set!    pointer (<struct-tm>-sec    record))
    (struct-tm-tm_min-set!    pointer (<struct-tm>-min    record))
    (struct-tm-tm_hour-set!   pointer (<struct-tm>-hour   record))
    (struct-tm-tm_mday-set!   pointer (<struct-tm>-mday   record))
    (struct-tm-tm_mon-set!    pointer (<struct-tm>-mon    record))
    (struct-tm-tm_year-set!   pointer (<struct-tm>-year   record))
    (struct-tm-tm_wday-set!   pointer (<struct-tm>-wday   record))
    (struct-tm-tm_yday-set!   pointer (<struct-tm>-yday   record))
    (struct-tm-tm_isdst-set!  pointer (<struct-tm>-isdst  record))
    (struct-tm-tm_gmtoff-set! pointer (<struct-tm>-gmtoff record))
    (struct-tm-tm_zone-set!   pointer (<struct-tm>-zone   record))))

;;; --------------------------------------------------------------------

(define (pointer-><struct-ntptimeval> pointer)
  (make-<struct-ntptimeval> (pointer-><struct-timeval> (struct-ntptimeval-time-ref pointer))
			    (struct-ntptimeval-maxerror-ref pointer)
			    (struct-ntptimeval-esterror-ref pointer)))

(define (<struct-ntptimeval>->pointer record malloc)
  (begin0-let ((pointer (malloc sizeof-ntptimeval)))
    (let ((time-pointer (struct-ntptimeval-time-ref pointer))
	  (time-record  (<struct-ntptimeval>-time record)))
      (struct-timeval-tv_sec-set!  time-pointer (<struct-timeval>-sec  time-record))
      (struct-timeval-tv_usec-set! time-pointer (<struct-timeval>-usec time-record)))
    (struct-ntptimeval-maxerror-set! pointer (<struct-ntptimeval>-maxerror record))
    (struct-ntptimeval-esterror-set! pointer (<struct-ntptimeval>-esterror record))))

;;; --------------------------------------------------------------------

(define (pointer-><struct-timex> pointer)
  (make-<struct-timex> (struct-timex-modes-ref pointer)
		       (struct-timex-offset-ref pointer)
		       (struct-timex-freq-ref pointer)
		       (struct-timex-maxerror-ref pointer)
		       (struct-timex-esterror-ref pointer)
		       (struct-timex-status-ref pointer)
		       (struct-timex-constant-ref pointer)
		       (struct-timex-precision-ref pointer)
		       (struct-timex-tolerance-ref pointer)
		       (pointer-><struct-timeval> (struct-timex-time-ref pointer))
		       (struct-timex-tick-ref pointer)
		       (struct-timex-ppsfreq-ref pointer)
		       (struct-timex-jitter-ref pointer)
		       (struct-timex-shift-ref pointer)
		       (struct-timex-stabil-ref pointer)
		       (struct-timex-jitcnt-ref pointer)
		       (struct-timex-calcnt-ref pointer)
		       (struct-timex-errcnt-ref pointer)
		       (struct-timex-stbcnt-ref pointer)))

(define (<struct-timex>->pointer record malloc)
  (begin0-let ((pointer (malloc sizeof-timex)))
    (let ((time-pointer (struct-timex-time-ref pointer))
	  (time-record  (<struct-timex>-time record)))
      (struct-timeval-tv_sec-set!  time-pointer (<struct-timeval>-sec  time-record))
      (struct-timeval-tv_usec-set! time-pointer (<struct-timeval>-usec time-record)))
    (struct-timex-modes-set!	 pointer (<struct-timex>-modes     record))
    (struct-timex-offset-set!    pointer (<struct-timex>-offset    record))
    (struct-timex-freq-set!      pointer (<struct-timex>-freq record))
    (struct-timex-maxerror-set!  pointer (<struct-timex>-maxerror  record))
    (struct-timex-esterror-set!  pointer (<struct-timex>-esterror  record))
    (struct-timex-status-set!    pointer (<struct-timex>-status    record))
    (struct-timex-constant-set!  pointer (<struct-timex>-constant  record))
    (struct-timex-precision-set! pointer (<struct-timex>-precision record))
    (struct-timex-tolerance-set! pointer (<struct-timex>-tolerance record))
    (struct-timex-tick-set!	 pointer (<struct-timex>-tick      record))
    (struct-timex-ppsfreq-set!   pointer (<struct-timex>-ppsfreq   record))
    (struct-timex-jitter-set!    pointer (<struct-timex>-jitter    record))
    (struct-timex-shift-set!	 pointer (<struct-timex>-shift     record))
    (struct-timex-stabil-set!	 pointer (<struct-timex>-stabil    record))
    (struct-timex-jitcnt-set!	 pointer (<struct-timex>-jitcnt    record))
    (struct-timex-calcnt-set!	 pointer (<struct-timex>-calcnt    record))
    (struct-timex-errcnt-set!	 pointer (<struct-timex>-errcnt    record))
    (struct-timex-stbcnt-set!	 pointer (<struct-timex>-stbcnt    record))))

;;; --------------------------------------------------------------------

(define (pointer-><struct-itimerval> pointer)
  (let ((interval-timeval*	(struct-itimerval-it_interval-ref pointer))
	(value-timeval*		(struct-itimerval-it_value-ref    pointer)))
    (make-<struct-itimerval> (make-<struct-timeval> (struct-timeval-tv_sec-ref  interval-timeval*)
						    (struct-timeval-tv_usec-ref interval-timeval*))
			     (make-<struct-timeval> (struct-timeval-tv_sec-ref  value-timeval*)
						    (struct-timeval-tv_usec-ref value-timeval*)))))

(define (<struct-itimerval>->pointer record malloc)
  (begin0-let ((pointer (malloc sizeof-itimerval)))
    (let ((interval-timeval*	(struct-itimerval-it_interval-ref pointer))
	  (value-timeval*	(struct-itimerval-it_value-ref    pointer))
	  (interval-record	(<struct-itimerval>-interval record))
	  (value-record		(<struct-itimerval>-value    record)))
    (struct-timeval-tv_sec-set!  interval-timeval* (<struct-timeval>-sec  interval-record))
    (struct-timeval-tv_usec-set! interval-timeval* (<struct-timeval>-usec interval-record))
    (struct-timeval-tv_sec-set!  value-timeval* (<struct-timeval>-sec  value-record))
    (struct-timeval-tv_usec-set! value-timeval* (<struct-timeval>-usec value-record))
    pointer)))


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
    (let ((timeval*	(malloc-block/c sizeof-timeval))
	  (timezone*	(malloc-block/c sizeof-timezone)))
      (receive (result errno)
	  (platform:gettimeofday timeval* timezone*)
	(if (= -1 result)
	    (raise-errno-error 'gettimeofday errno)
	  (values (pointer-><struct-timeval>  timeval*)
		  (pointer-><struct-timezone> timezone*)))))))

(define (settimeofday timeval timezone)
  (with-compensations
    (let ((timeval*	(<struct-timeval>->pointer   timeval  malloc-block/c))
	  (timezone*	(<struct-timezone>->pointer timezone malloc-block/c)))
      (receive (result errno)
	  (platform:settimeofday timeval* timezone*)
	(if (= -1 result)
	    (raise-errno-error 'settimeofday errno (list timeval timezone))
	  result)))))

(define (adjtime timeval-delta)
  (with-compensations
    (let ((timeval-delta*	(<struct-timeval>->pointer timeval-delta malloc-block/c))
	  (timeval-old-delta*	(malloc-block/c sizeof-timeval)))
      (receive (result errno)
	  (platform:adjtime timeval-delta* timeval-old-delta*)
	(if (= -1 result)
	    (raise-errno-error 'adjtime errno timeval-delta)
	  (values result (pointer-><struct-timeval> timeval-old-delta*)))))))


;;;; broken-down time

(define (localtime time malloc)
  (let ((tm* (malloc sizeof-tm)))
    (receive (result errno)
	(platform:localtime_r time tm*)
      (if (pointer=? tm* result)
	  tm*
	(raise-errno-error 'localtime errno time)))))

(define (localtime* time)
  (with-compensations
    (pointer-><struct-tm> (localtime time malloc-block/c))))

(define (gmtime time malloc)
  (let ((tm* (malloc sizeof-tm)))
    (receive (result errno)
	(platform:gmtime_r time tm*)
      (if (pointer=? tm* result)
	  tm*
	(raise-errno-error 'gmtime errno time)))))

(define (gmtime* time)
  (with-compensations
    (pointer-><struct-tm> (gmtime time malloc-block/c))))

(define (timelocal tm*)
  (receive (result errno)
      (platform:timelocal tm*)
    (if (= -1 result)
	(raise-errno-error 'timelocal errno tm*)
      result)))

(define (timelocal* tm-record)
  (with-compensations
    (timelocal (<struct-tm>->pointer tm-record malloc-block/c))))

(define (timegm tm*)
  (receive (result errno)
      (platform:timegm tm*)
    (if (= -1 result)
	(raise-errno-error 'timegm errno tm*)
      result)))

(define (timegm* tm-record)
  (with-compensations
    (timegm (<struct-tm>->pointer tm-record malloc-block/c))))


;;;; high-accuracy time

(define (ntp_gettime ntptimeval*)
  (receive (result errno)
      (platform:ntp_gettime ntptimeval*)
    (if (= 0 result)
	result
      (raise-errno-error 'ntp_gettime errno ntptimeval*))))

(define (ntp_gettime*)
  (with-compensations
    (let ((ntptimeval* (malloc-block/c sizeof-ntptimeval)))
      (platform:ntp_gettime ntptimeval*)
      (pointer-><struct-ntptimeval> ntptimeval*))))

(define (ntp_adjtime timex*)
  (receive (result errno)
      (platform:ntp_adjtime timex*)
    (if (= 0 result)
	result
      (raise-errno-error 'ntp_adjtime errno timex*))))

(define (ntp_adjtime* record)
  (with-compensations
    (let ((timex* (<struct-timex>->pointer record malloc-block/c)))
      (platform:ntp_gettime timex*)
      (pointer-><struct-timex> timex*))))


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
    (asctime (<struct-tm>->pointer tm-record malloc-block/c))))

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
    (strftime template (<struct-tm>->pointer tm-record malloc-block/c))))


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
    (let ((struct-tm* (malloc-block/c sizeof-tm)))
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
      (pointer-><struct-tm> struct-tm*))))


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
    (let ((itimerval-new*	(<struct-itimerval>->pointer itimerval-record malloc-block/c))
	  (itimerval-old*	(malloc-block/c sizeof-itimerval)))
      (setitimer which itimerval-new* itimerval-old*)
      (pointer-><struct-itimerval> itimerval-old*))))

(define (getitimer* which)
  (with-compensations
    (let ((itimerval-old* (malloc-block/c sizeof-itimerval)))
      (getitimer which itimerval-old*)
      (pointer-><struct-itimerval> itimerval-old*))))


;;;; sleeping

(define (nanosleep requested-time* remaining-time*)
  (receive (result errno)
      (platform:nanosleep requested-time* remaining-time*)
    (if (= -1 result)
	(raise-errno-error 'nanosleep errno (list requested-time* remaining-time*))
      result)))

(define (nanosleep* requested-time)
  (with-compensations
    (let ((requested-time*	(<struct-timespec>->pointer requested-time malloc-block/c))
	  (remaining-time*	(malloc-block/c sizeof-timespec)))
      (nanosleep requested-time* remaining-time*)
      (pointer-><struct-timespec> remaining-time*))))


;;;; done

)

;;; end of file
