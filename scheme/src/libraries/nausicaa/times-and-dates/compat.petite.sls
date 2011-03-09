;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: times and dates compat library for Petite
;;;Date: Fri Mar 12, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa times-and-dates compat)
  (export
    (rename (time-nanosecond	host:time-nanosecond)
	    (time-second	host:time-second)
	    (current-time	host:current-time)
	    (time-gmt-offset	host:time-gmt-offset))
    host:time-resolution)
  (import (rnrs)
    (only (chezscheme)
	  current-time time-second time-nanosecond date-zone-offset
	  time-utc->date))

  (define (time-gmt-offset time)
    ;;In (times-and-dates)  this function is called with  TIME being the
    ;;return value of HOST:CURRENT-TIME.
    ;;
    (date-zone-offset (time-utc->date time)))

  ;;I dunno  what is  the resolution for  Petite Chez Scheme,  I blindly
  ;;assume it is the same as the others (Marco Maggi, Fri Mar 12, 2010).
  (define host:time-resolution 1000))

;;; end of file
