;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: access to POSIX stub library for time functions
;;;Date: Fri Dec 19, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008, 2009 Marco Maggi <marcomaggi@gna.org>
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

(library (posix time stub)
  (export
    clock primitive-clock primitive-clock-function platform-clock
    times primitive-times primitive-times-function platform-times

    make-struct-tms
    struct-tms-tms_utime-ref struct-tms-tms_stime-ref
    struct-tms-tms_cutime-ref struct-tms-tms_cstime-ref
    )
  (import (r6rs)
    (uriel lang)
    (uriel foreign)
    (posix sizeof))

  (define stub-lib
    (let ((o (open-shared-object 'libnausicaa-posix.so)))
      (shared-object o)
      o))


;;;; CPU ticks

(define-c-function/with-errno platform-clock
  (double nausicaa_posix_clock (void)))

(define (primitive-clock)
  (receive (result errno)
      (platform-clock)
    (if (= -1 result)
	(raise-errno-error 'primitive-clock errno)
      result)))

(define-primitive-parameter
  primitive-clock-function primitive-clock)

(define (clock)
  ((primitive-clock-function)))



;;;; process ticks

(define-c-function/with-errno platform-times
  (double nausicaa_posix_times (pointer)))

(define-record-type struct-tms
  (fields (immutable tms_utime struct-tms-tms_utime-ref)
	  (immutable tms_stime struct-tms-tms_stime-ref)
	  (immutable tms_cutime struct-tms-tms_cutime-ref)
	  (immutable tms_cstime struct-tms-tms_cstime-ref)))

(define (primitive-times)
  (with-compensations
    (let ((p (malloc-block/c (sizeof-double-array 4))))
      (receive (result errno)
	  (platform-times p)
	(if (= -1 result)
	    (raise-errno-error 'primitive-clock errno)
	  (values result
		  (make-struct-tms (peek-array-double p 0)
				   (peek-array-double p 1)
				   (peek-array-double p 2)
				   (peek-array-double p 3))))))))

(define-primitive-parameter
  primitive-times-function primitive-times)

(define (times)
  ((primitive-times-function)))



;;;; done

)

;;; end of file
