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





;;;; done

)

;;; end of file
