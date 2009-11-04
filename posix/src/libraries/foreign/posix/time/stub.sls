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


#!r6rs
(library (foreign posix time stub)
  (export
    clock primitive-clock primitive-clock-function platform-clock
    times primitive-times primitive-times-function platform-times

    make-struct-tms
    struct-tms-tms_utime-ref struct-tms-tms_stime-ref
    struct-tms-tms_cutime-ref struct-tms-tms_cstime-ref
    )
  (import (nausicaa)
    (foreign ffi)
    (foreign ffi sizeof)
    (foreign memory)
    (foreign cstrings)
    (foreign errno)
    (foreign posix sizeof)
    (compensations))




;;;; done

)

;;; end of file
