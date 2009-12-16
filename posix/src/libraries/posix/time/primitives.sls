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


(library (posix time primitives)
  (export

    ;; clock ticks and CPU times
    clock		times

    ;; calendar time
    time

    )
  (import (rnrs)
    (receive)
    (compensations)
    (only (foreign ffi sizeof) sizeof-double-array)
    (only (foreign ffi peekers-and-pokers) array-ref-c-double)
    (only (foreign ffi pointers) pointer-null)
    (only (foreign memory) malloc-block/c)
    (only (foreign errno) raise-errno-error)
    (posix typedefs)
    (posix sizeof)
    (prefix (posix time platform) platform:))


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
      (values result (struct-tms->record struct-tms*)))))


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
