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
;;;Copyright (c) 2009-2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa posix time primitives)
  (export

    ;; clock ticks and CPU times
    clock		times

    ;; calendar time
    time

    ;; structure/record conversion
    ;; <tms>->pointer		pointer-><tms>
    ;; <timeval>->pointer		pointer-><timeval>
    )
  (import (nausicaa)
    (prefix (nausicaa ffi sizeof) ffi.)
    (only (nausicaa ffi memory) malloc-block/c)
    (only (nausicaa ffi errno) raise-errno-error)
;;    (nausicaa posix typedefs)
    (prefix (nausicaa posix sizeof) so.)
;;    (nausicaa posix time typedefs)
    (prefix (nausicaa posix time platform) platform.))


;;;; clock ticks and CPU time

(define (clock)
  (receive (result errno)
      (platform.clock)
    (if (= -1 result)
	(raise-errno-error 'clock errno)
      result)))

(define (times)
  (with-compensations
    (let* ((tms*	(malloc-block/c (so.c-sizeof struct-tms)))
	   (result	(platform.times tms*)))
      (values result (make so.<tms>
		       (so.pointer: tms*))))))


;;;; calendar time

(define (time)
  (receive (result errno)
      (platform.time)
    (if (= -1 result)
	(raise-errno-error 'time errno)
      result)))


;;;; done

)

;;; end of file
