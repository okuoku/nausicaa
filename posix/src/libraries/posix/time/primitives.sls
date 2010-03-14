;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: marshaling interface to time functions
;;;Date: Wed Nov  4, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (posix time primitives)
  (export

    ;; clock ticks and CPU times
    clock		times

    ;; calendar time
    time

    ;; structure/record conversion
    <tms>->pointer		pointer-><tms>
    <timeval>->pointer		pointer-><timeval>
    )
  (import (rnrs)
    (language-extensions)
    (compensations)
    (only (foreign ffi sizeof) sizeof-double-array)
    (only (foreign ffi peekers-and-pokers) array-ref-c-double)
    (only (foreign ffi pointers) pointer-null)
    (only (foreign memory) malloc-block/c)
    (only (foreign errno) raise-errno-error)
    (posix typedefs)
    (posix sizeof)
    (prefix (posix time platform) platform:))


(define (pointer-><tms> pointer)
  (make-<tms> (platform:struct-tms-tms_utime-ref  pointer)
		     (platform:struct-tms-tms_stime-ref  pointer)
		     (platform:struct-tms-tms_cutime-ref pointer)
		     (platform:struct-tms-tms_cutime-ref pointer)))

(define (<tms>->pointer record malloc)
  (begin0-let ((pointer (malloc sizeof-tms)))
    (platform:struct-tms-tms_utime-set!  pointer (<tms>-utime  record))
    (platform:struct-tms-tms_stime-set!  pointer (<tms>-stime  record))
    (platform:struct-tms-tms_cutime-set! pointer (<tms>-cutime record))
    (platform:struct-tms-tms_cstime-set! pointer (<tms>-cstime record))))

;;; --------------------------------------------------------------------

(define (pointer-><timeval> pointer)
  (make-<timeval> (struct-timeval-tv_sec-ref  pointer)
			 (struct-timeval-tv_usec-ref pointer)))

(define (<timeval>->pointer record malloc)
  (begin0-let ((pointer (malloc sizeof-timeval)))
    (struct-timeval-tv_sec-set!  pointer (<timeval>-sec  record))
    (struct-timeval-tv_usec-set! pointer (<timeval>-usec record))))


;;;; clock ticks and CPU time

(define (clock)
  (receive (result errno)
      (platform:clock)
    (if (= -1 result)
	(raise-errno-error 'clock errno)
      result)))

(define (times)
  (with-compensations
    (let* ((struct-tms*	(malloc-block/c sizeof-tms))
	   (result	(platform:times struct-tms*)))
      (values result (pointer-><tms> struct-tms*)))))


;;;; calendar time

(define (time)
  (receive (result errno)
      (platform:time)
    (if (= -1 result)
	(raise-errno-error 'time errno)
      result)))


;;;; done

)

;;; end of file
