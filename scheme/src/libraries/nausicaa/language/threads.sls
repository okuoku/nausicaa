;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: multithreading stuff
;;;Date: Wed Aug 18, 2010
;;;
;;;Abstract
;;;
;;;	This   library   is  meant   to   provide   place  holders   for
;;;	multithreading primitives.  At  present Nausicaa/Scheme does not
;;;	support any Scheme implementation providing threads.
;;;
;;;Copyright (c) 2010, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa language threads)
  (export
    define-thread-specific-location
    thread-specific-location-set!
    thread-specific-location-ref)
  (import (rnrs base))

  (define-syntax define-thread-specific-location
    (syntax-rules ()
      ((_ ?location ?init-value)
       (define ?location ?init-value))))

  (define-syntax thread-specific-location-set!
    (syntax-rules ()
      ((_ ?location ?obj)
       (set! ?location ?obj))))

  (define-syntax thread-specific-location-ref
    (syntax-rules ()
      ((_ ?location)
       ?location)))

  )

;;; end of file
