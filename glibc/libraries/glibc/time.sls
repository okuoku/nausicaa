;;;
;;;Part of: Nausicaa/Glibc
;;;Contents: interface to the time and date functions
;;;Date: Mon Dec 22, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
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



;;;; setup

(library (glibc time)
  (export

    ;; simple calendar time
    time primitive-time primitive-time-function platform-time
    stime primitive-stime primitive-stime-function platform-stime

    ;; high resolution calendar time
    gettimeofday primitive-gettimeofday primitive-gettimeofday-function platform-gettimeofday
    settimeofday primitive-settimeofday primitive-settimeofday-function platform-settimeofday
    adjtime primitive-adjtime primitive-adjtime-function platform-adjtime

    ;; broken down time
    localtime primitive-localtime primitive-localtime-function platform-localtime
    gmtime primitive-gmtime primitive-gmtime-function platform-gmtime
    timelocal primitive-timelocal primitive-timelocal-function platform-timelocal
    timegm primitive-timegm primitive-timegm-function platform-timegm

    (rename (timelocal mktime))
    (rename (primitive-timelocal primitive-mktime))
    (rename (primitive-timelocal primitive-mktime-function))
    (rename (platform-timelocal platform-mktime))

    ;; high accuracy time
    ntp_gettime primitive-ntp_gettime primitive-ntp_gettime-function platform-ntp_gettime
    )
  (import (r6rs)
    (uriel lang)
    (uriel foreign)
    (glibc sizeof))

  (define libc
    (begin
      (shared-object self-shared-object)
      self-shared-object))



;;;; simple calendar time

(define-c-function/with-errno platform-time
  (time_t time (pointer)))

(define (primitive-time)
  (receive (result errno)
      (platform-time pointer-null)
    (if (= -1 result)
	(raise-errno-error 'primitive-time errno)
      result)))

(define primitive-time-function
  (make-parameter primitive-time
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-time-function
	  "expected procedure as value for the PRIMITIVE-TIME-FUNCTION parameter"
	  func))
      func)))

(define (time)
  ((primitive-time-function)))

;;; --------------------------------------------------------------------

(define-c-function/with-errno platform-stime
  (time_t stime (pointer)))

(define (primitive-stime)
  (receive (result errno)
      (platform-stime pointer-null)
    (if (= -1 result)
	(raise-errno-error 'primitive-stime errno)
      result)))

(define primitive-stime-function
  (make-parameter primitive-stime
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-stime-function
	  "expected procedure as value for the PRIMITIVE-STIME-FUNCTION parameter"
	  func))
      func)))

(define (stime)
  ((primitive-stime-function)))



;;;; high resolution calendar

(define-c-function/with-errno platform-gettimeofday
  (int gettimeofday (pointer pointer)))

(define (primitive-gettimeofday timeval timezone)
  (receive (result errno)
      (platform-gettimeofday timeval timezone)
    (if (= -1 result)
	(raise-errno-error 'primitive-gettimeofday errno)
      result)))

(define primitive-gettimeofday-function
  (make-parameter primitive-gettimeofday
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-gettimeofday-function
	  "expected procedure as value for PRIMITIVE-GETTIMEOFDAY-FUNCTION the parameter"
	  func))
      func)))

(define (gettimeofday timeval timezone)
  ((primitive-gettimeofday-function) timeval timezone))

;;; --------------------------------------------------------------------

(define-c-function/with-errno platform-settimeofday
  (int settimeofday (pointer pointer)))

(define (primitive-settimeofday timeval timezone)
  (receive (result errno)
      (platform-settimeofday timeval timezone)
    (if (= -1 result)
	(raise-errno-error 'primitive-settimeofday errno)
      result)))

(define primitive-settimeofday-function
  (make-parameter primitive-settimeofday
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-settimeofday-function
	  "expected procedure as value for PRIMITIVE-SETTIMEOFDAY-FUNCTION the parameter"
	  func))
      func)))

(define (settimeofday timeval timezone)
  ((primitive-settimeofday-function) timeval timezone))

;;; --------------------------------------------------------------------

(define-c-function/with-errno platform-adjtime
  (int adjtime (pointer pointer)))

(define (primitive-adjtime timeval-delta timeval-old-delta)
  (receive (result errno)
      (platform-adjtime timeval-delta timeval-old-delta)
    (if (= -1 result)
	(raise-errno-error 'primitive-adjtime errno)
      result)))

(define primitive-adjtime-function
  (make-parameter primitive-adjtime
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-adjtime-function
	  "expected procedure as value for PRIMITIVE-ADJTIME-FUNCTION the parameter"
	  func))
      func)))

(define (adjtime timeval-delta timeval-old-delta)
  ((primitive-adjtime-function) timeval-delta timeval-old-delta))



;;;; broken-down time

(define-c-function/with-errno platform-localtime
  (pointer localtime_r (pointer pointer)))


(define (primitive-localtime time *tm)
  (receive (result errno)
      (with-compensations
	(let ((*time (malloc-small/c)))
	  (poke-time_t! *time 0 time)
	  (platform-localtime *time *tm)))
    (if (pointer=? *tm result)
	*tm
      (raise-errno-error 'primitive-localtime errno))))

(define primitive-localtime-function
  (make-parameter primitive-localtime
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-localtime-function
	  "expected procedure as value for PRIMITIVE-LOCALTIME-FUNCTION the parameter"
	  func))
      func)))

(define (localtime time *tm)
  ((primitive-localtime-function) time *tm))

;;; --------------------------------------------------------------------

(define-c-function/with-errno platform-gmtime
  (pointer gmtime_r (pointer pointer)))

(define (primitive-gmtime time *tm)
  (receive (result errno)
      (with-compensations
	(let ((*time (malloc-small/c)))
	  (poke-time_t! *time 0 time)
	  (platform-gmtime *time *tm)))
    (if (pointer=? *tm result)
	*tm
      (raise-errno-error 'primitive-gmtime errno))))

(define primitive-gmtime-function
  (make-parameter primitive-gmtime
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-gmtime-function
	  "expected procedure as value for PRIMITIVE-GMTIME-FUNCTION the parameter"
	  func))
      func)))

(define (gmtime time *tm)
  ((primitive-gmtime-function) time *tm))

;;; --------------------------------------------------------------------

(define-c-function/with-errno platform-timelocal
  (time_t timelocal (pointer)))


(define (primitive-timelocal *tm)
  (receive (result errno)
      (platform-timelocal *tm)
    (if (= -1 result)
	(raise-errno-error 'primitive-timelocal errno)
      result)))

(define primitive-timelocal-function
  (make-parameter primitive-timelocal
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-timelocal-function
	  "expected procedure as value for PRIMITIVE-TIMELOCAL-FUNCTION the parameter"
	  func))
      func)))

(define (timelocal *tm)
  ((primitive-timelocal-function) *tm))

;;; --------------------------------------------------------------------

(define-c-function/with-errno platform-timegm
  (time_t timegm (pointer)))


(define (primitive-timegm *tm)
  (receive (result errno)
      (platform-timegm *tm)
    (if (= -1 result)
	(raise-errno-error 'primitive-timegm errno)
      result)))

(define primitive-timegm-function
  (make-parameter primitive-timegm
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-timegm-function
	  "expected procedure as value for PRIMITIVE-TIMEGM-FUNCTION the parameter"
	  func))
      func)))

(define (timegm *tm)
  ((primitive-timegm-function) *tm))



;;;; high accuracy time

(define-c-function/with-errno platform-ntp_gettime
  (int ntp_gettime (pointer)))

(define (primitive-ntp_gettime *ntptimeval)
  (format #t "p ~s~%" *ntptimeval)
  (receive (result errno)
      (platform-ntp_gettime *ntptimeval)
    (format #t "res  ~s~%" result)
    (if (= 0 result)
	result
      (raise-errno-error 'primitive-ntp_gettime errno))))

(define primitive-ntp_gettime-function
  (make-parameter primitive-ntp_gettime
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-ntp_gettime-function
	  "expected procedure as value for PRIMITIVE-NTP_GETTIME-FUNCTION the parameter"
	  func))
      func)))

(define (ntp_gettime *ntptimeval)
  ((primitive-ntp_gettime-function) *ntptimeval))




;;;; done

)

;;; end of file
