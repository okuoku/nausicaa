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

    <struct-tms>		<struct-tms-rtd>
    make-<struct-tms>		struct-tms->record
    <struct-tms>?

    <struct-tms>-utime		<struct-tms>-stime
    <struct-tms>-cutime		<struct-tms>-cstime)
  (import (rnrs)
    (foreign posix time platform)
    (foreign posix sizeof))


(define-record-type <struct-tms>
  (fields (immutable utime)
	  (immutable stime)
	  (immutable cutime)
	  (immutable cstime)))

(define <struct-tms-rtd>
  (record-type-descriptor <struct-tms>))

(define (struct-tms->record pointer)
  (make-<struct-tms> (struct-tms-tms_utime-ref pointer)
		     (struct-tms-tms_stime-ref pointer)
		     (struct-tms-tms_cutime-ref pointer)
		     (struct-tms-tms_cutime-ref pointer)))


;;;; done

)

;;; end of file
