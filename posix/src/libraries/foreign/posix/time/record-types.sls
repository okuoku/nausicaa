;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: record types for time functions
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


(library (foreign posix time record-types)
  (export

    <struct-tms>			<struct-tms-rtd>
    make-<struct-tms>			<struct-tms>?
    struct-tms->record			record->struct-tms
    <struct-tms>-utime			<struct-tms>-utime-set!
    <struct-tms>-stime			<struct-tms>-stime-set!
    <struct-tms>-cutime			<struct-tms>-cutime-set!
    <struct-tms>-cstime			<struct-tms>-cstime-set!

    <struct-timeval>			<struct-timeval-rtd>
    make-<struct-timeval>		<struct-timeval>?
    struct-timeval->record		record->struct-timeval
    <struct-timeval>-sec		<struct-timeval>-sec-set!
    <struct-timeval>-usec		<struct-timeval>-usec-set!

    <struct-timespec>			<struct-timespec-rtd>
    make-<struct-timespec>		<struct-timespec>?
    struct-timespec->record		record->struct-timespec
    <struct-timespec>-sec		<struct-timespec>-sec-set!
    <struct-timespec>-nsec		<struct-timespec>-nsec-set!

    <struct-timezone>			<struct-timezone-rtd>
    make-<struct-timezone>		<struct-timezone>?
    struct-timezone->record		record->struct-timezone
    <struct-timezone>-minuteswest	<struct-timezone>-minuteswest-set!
    <struct-timezone>-dsttime		<struct-timezone>-dsttime-set!

    <struct-tm>				<struct-tm-rtd>
    make-<struct-tm>			<struct-tm>?
    struct-tm->record			record->struct-tm
    <struct-tm>-sec			<struct-tm>-sec-set!
    <struct-tm>-min			<struct-tm>-min-set!
    <struct-tm>-hour			<struct-tm>-hour-set!
    <struct-tm>-mday			<struct-tm>-mday-set!
    <struct-tm>-mon			<struct-tm>-mon-set!
    <struct-tm>-year			<struct-tm>-year-set!
    <struct-tm>-wday			<struct-tm>-wday-set!
    <struct-tm>-yday			<struct-tm>-yday-set!
    <struct-tm>-isdst			<struct-tm>-isdst-set!
    <struct-tm>-gmtoff			<struct-tm>-gmtoff-set!
    <struct-tm>-zone			<struct-tm>-zone-set!

    <struct-ntptimeval>			<struct-ntptimeval-rtd>
    make-<struct-ntptimeval>		<struct-ntptimeval>?
    struct-ntptimeval->record		record->struct-ntptimeval
    <struct-ntptimeval>-time		<struct-ntptimeval>-time-set!
    <struct-ntptimeval>-maxerror	<struct-ntptimeval>-maxerror-set!
    <struct-ntptimeval>-esterror	<struct-ntptimeval>-esterror-set!

    <struct-timex>			<struct-timex-rtd>
    make-<struct-timex>			<struct-timex>?
    struct-timex->record		record->struct-timex
    <struct-timex>-modes		<struct-timex>-modes-set!
    <struct-timex>-offset		<struct-timex>-offset-set!
    <struct-timex>-frequency		<struct-timex>-frequency-set!
    <struct-timex>-maxerror		<struct-timex>-maxerror-set!
    <struct-timex>-esterror		<struct-timex>-esterror-set!
    <struct-timex>-status		<struct-timex>-status-set!
    <struct-timex>-constant		<struct-timex>-constant-set!
    <struct-timex>-precision		<struct-timex>-precision-set!
    <struct-timex>-tolerance		<struct-timex>-tolerance-set!
    <struct-timex>-time			<struct-timex>-time-set!
    <struct-timex>-tick			<struct-timex>-tick-set!
    <struct-timex>-ppsfreq		<struct-timex>-ppsfreq-set!
    <struct-timex>-jitter		<struct-timex>-jitter-set!
    <struct-timex>-shift		<struct-timex>-shift-set!
    <struct-timex>-stabil		<struct-timex>-stabil-set!
    <struct-timex>-jitcnt		<struct-timex>-jitcnt-set!
    <struct-timex>-calcnt		<struct-timex>-calcnt-set!
    <struct-timex>-errcnt		<struct-timex>-errcnt-set!
    <struct-timex>-stbcnt		<struct-timex>-stbcnt-set!

    <struct-itimerval>			<struct-itimerval-rtd>
    make-<struct-itimerval>		<struct-itimerval>?
    struct-itimerval->record		record->struct-itimerval
    <struct-itimerval>-interval		<struct-itimerval>-interval-set!
    <struct-itimerval>-value		<struct-itimerval>-value-set!
    )
  (import (rnrs)
    (begin0)
    (foreign posix time platform)
    (foreign posix sizeof))


(define-record-type <struct-tms>
  (fields (mutable utime)
	  (mutable stime)
	  (mutable cutime)
	  (mutable cstime)))

(define <struct-tms-rtd>
  (record-type-descriptor <struct-tms>))

(define (struct-tms->record pointer)
  (make-<struct-tms> (struct-tms-tms_utime-ref  pointer)
		     (struct-tms-tms_stime-ref  pointer)
		     (struct-tms-tms_cutime-ref pointer)
		     (struct-tms-tms_cutime-ref pointer)))

(define (record->struct-tms record malloc)
  (begin0-let ((pointer (malloc sizeof-struct-tms)))
    (struct-tms-tms_utime-set!  pointer (<struct-tms>-utime  record))
    (struct-tms-tms_stime-set!  pointer (<struct-tms>-stime  record))
    (struct-tms-tms_cutime-set! pointer (<struct-tms>-cutime record))
    (struct-tms-tms_cstime-set! pointer (<struct-tms>-cstime record))))


(define-record-type <struct-timeval>
  (fields (mutable sec)
	  (mutable usec)))

(define <struct-timeval-rtd>
  (record-type-descriptor <struct-timeval>))

(define (struct-timeval->record pointer)
  (make-<struct-timeval> (struct-timeval-tv_sec-ref  pointer)
			 (struct-timeval-tv_usec-ref pointer)))

(define (record->struct-timeval record malloc)
  (begin0-let ((pointer (malloc sizeof-struct-timeval)))
    (struct-timeval-tv_sec-set!  pointer (<struct-timeval>-sec  record))
    (struct-timeval-tv_usec-set! pointer (<struct-timeval>-usec record))))


(define-record-type <struct-timespec>
  (fields (mutable sec)
	  (mutable nsec)))

(define <struct-timespec-rtd>
  (record-type-descriptor <struct-timespec>))

(define (struct-timespec->record pointer)
  (make-<struct-timespec> (struct-timespec-tv_sec-ref  pointer)
			  (struct-timespec-tv_nsec-ref pointer)))

(define (record->struct-timespec record malloc)
  (begin0-let ((pointer (malloc sizeof-struct-timespec)))
    (struct-timespec-tv_sec-set!  pointer (<struct-timespec>-sec  record))
    (struct-timespec-tv_nsec-set! pointer (<struct-timespec>-nsec record))))


(define-record-type <struct-timezone>
  (fields (mutable minuteswest)
	  (mutable dsttime)))

(define <struct-timezone-rtd>
  (record-type-descriptor <struct-timezone>))

(define (struct-timezone->record pointer)
  (make-<struct-timezone> (struct-timezone-tz_minuteswest-ref  pointer)
			  (struct-timezone-tz_dsttime-ref      pointer)))

(define (record->struct-timezone record malloc)
  (begin0-let ((pointer (malloc sizeof-struct-timezone)))
    (struct-timezone-tz_minuteswest-set! pointer (<struct-timezone>-minuteswest record))
    (struct-timezone-tz_dsttime-set!     pointer (<struct-timezone>-dsttime     record))))


(define-record-type <struct-tm>
  (fields (mutable sec)
	  (mutable min)
	  (mutable hour)
	  (mutable mday)
	  (mutable mon)
	  (mutable year)
	  (mutable wday)
	  (mutable yday)
	  (mutable isdst)
	  (mutable gmtoff)
	  (mutable zone)))

(define <struct-tm-rtd>
  (record-type-descriptor <struct-tm>))

(define (struct-tm->record pointer)
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

(define (record->struct-tm record malloc)
  (begin0-let ((pointer (malloc sizeof-struct-tm)))
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


(define-record-type <struct-ntptimeval>
  (fields (mutable time)
	  (mutable maxerror)
	  (mutable esterror)))

(define <struct-ntptimeval-rtd>
  (record-type-descriptor <struct-ntptimeval>))

(define (struct-ntptimeval->record pointer)
  (make-<struct-ntptimeval> (struct-timeval->record (struct-ntptimeval-time-ref pointer))
			    (struct-ntptimeval-maxerror-ref pointer)
			    (struct-ntptimeval-esterror-ref pointer)))

(define (record->struct-ntptimeval record malloc)
  (begin0-let ((pointer (malloc sizeof-struct-ntptimeval)))
    (let ((time-pointer (struct-ntptimeval-time-ref pointer))
	  (time-record  (<struct-ntptimeval>-time record)))
      (struct-timeval-tv_sec-set!  time-pointer (<struct-timeval>-sec  time-record))
      (struct-timeval-tv_usec-set! time-pointer (<struct-timeval>-usec time-record)))
    (struct-ntptimeval-maxerror-set! pointer (<struct-ntptimeval>-maxerror record))
    (struct-ntptimeval-esterror-set! pointer (<struct-ntptimeval>-esterror record))))


(define-record-type <struct-timex>
  (fields (mutable modes)
	  (mutable offset)
	  (mutable frequency)
	  (mutable maxerror)
	  (mutable esterror)
	  (mutable status)
	  (mutable constant)
	  (mutable precision)
	  (mutable tolerance)
	  (mutable time)
	  (mutable tick)
	  (mutable ppsfreq)
	  (mutable jitter)
	  (mutable shift)
	  (mutable stabil)
	  (mutable jitcnt)
	  (mutable calcnt)
	  (mutable errcnt)
	  (mutable stbcnt)))

(define <struct-timex-rtd>
  (record-type-descriptor <struct-timex>))

(define (struct-timex->record pointer)
  (make-<struct-timex> (struct-timex-modes-ref pointer)
		       (struct-timex-offset-ref pointer)
		       (struct-timex-frequency-ref pointer)
		       (struct-timex-maxerror-ref pointer)
		       (struct-timex-esterror-ref pointer)
		       (struct-timex-status-ref pointer)
		       (struct-timex-constant-ref pointer)
		       (struct-timex-precision-ref pointer)
		       (struct-timex-tolerance-ref pointer)
		       (struct-timeval->record (struct-timex-time-ref pointer))
		       (struct-timex-tick-ref pointer)
		       (struct-timex-ppsfreq-ref pointer)
		       (struct-timex-jitter-ref pointer)
		       (struct-timex-shift-ref pointer)
		       (struct-timex-stabil-ref pointer)
		       (struct-timex-jitcnt-ref pointer)
		       (struct-timex-calcnt-ref pointer)
		       (struct-timex-errcnt-ref pointer)
		       (struct-timex-stbcnt-ref pointer)))

(define (record->struct-timex record malloc)
  (begin0-let ((pointer (malloc sizeof-struct-timex)))
    (let ((time-pointer (struct-timex-time-ref pointer))
	  (time-record  (<struct-timex>-time record)))
      (struct-timeval-tv_sec-set!  time-pointer (<struct-timeval>-sec  time-record))
      (struct-timeval-tv_usec-set! time-pointer (<struct-timeval>-usec time-record)))
    (struct-timex-modes-set!	 pointer (<struct-timex>-modes     record))
    (struct-timex-offset-set!    pointer (<struct-timex>-offset    record))
    (struct-timex-frequency-set! pointer (<struct-timex>-frequency record))
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


(define-record-type <struct-itimerval>
  (fields (mutable interval)
	  (mutable value)))

(define <struct-itimerval-rtd>
  (record-type-descriptor <struct-itimerval>))

(define (struct-itimerval->record pointer)
  (let ((interval-timeval*	(struct-itimerval-it_interval-ref pointer))
	(value-timeval*		(struct-itimerval-it_value-ref    pointer)))
    (make-<struct-itimerval> (make-<struct-timeval> (struct-timeval-tv_sec-ref  interval-timeval*)
						    (struct-timeval-tv_usec-ref interval-timeval*))
			     (make-<struct-timeval> (struct-timeval-tv_sec-ref  value-timeval*)
						    (struct-timeval-tv_usec-ref value-timeval*)))))

(define (record->struct-itimerval record malloc)
  (begin0-let ((pointer (malloc sizeof-struct-itimerval)))
    (let ((interval-timeval*	(struct-itimerval-it_interval-ref pointer))
	  (value-timeval*	(struct-itimerval-it_value-ref    pointer))
	  (interval-record	(<struct-itimerval>-interval record))
	  (value-record		(<struct-itimerval>-value    record)))
    (struct-timeval-tv_sec-set!  interval-timeval* (<struct-timeval>-sec  interval-record))
    (struct-timeval-tv_usec-set! interval-timeval* (<struct-timeval>-usec interval-record))
    (struct-timeval-tv_sec-set!  value-timeval* (<struct-timeval>-sec  value-record))
    (struct-timeval-tv_usec-set! value-timeval* (<struct-timeval>-usec value-record))
    pointer)))


;;;; done

)

;;; end of file
