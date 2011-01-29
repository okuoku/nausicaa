;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: direct interface for time functions
;;;Date: Wed Nov  4, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa posix time platform)
  (export
    ;; clock ticks and CPU times
    clock		times
    ;; calendar time
    time)
  (import (nausicaa)
    (nausicaa ffi)
    (nausicaa posix clang type-translation)
    (nausicaa posix shared-object)
    (nausicaa posix sizeof))

  (define-c-functions/with-errno libnausicaa-posix
    (%clock		(double nausicaa_posix_clock (void)))
    (%time		(double nausicaa_posix_time (void))))

  (define-c-functions libnausicaa-posix
    (%times		(double nausicaa_posix_times (struct-tms*))))

  (define (clock)
    (receive (result errno)
	(%clock)
      (values (exact result) errno)))

  (define (time)
    (receive (result errno)
	(%time)
      (values (exact result) errno)))

  (define (times pointer)
    (exact (%times pointer)))

  )

;;; end of file
