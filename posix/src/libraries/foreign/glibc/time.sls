;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: interface to the time and date functions
;;;Date: Mon Dec 22, 2008
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


(library (foreign glibc time)
  (export

    ;; simple calendar time
    stime		stime-function

    ;; high resolution calendar time
    gettimeofday	gettimeofday-function
    settimeofday	settimeofday-function
    adjtime		adjtime-function

    ;; broken down time
    localtime		localtime-function
    localtime*		localtime*-function
    gmtime		gmtime-function
    gmtime*		gmtime*-function
    timelocal		timelocal-function
    timelocal*		timelocal*-function
    timegm		timegm-function
    timegm*		timegm*-function

    ;; high accuracy time
    ntp_gettime		ntp_gettime-function
    ntp_gettime*	ntp_gettime*-function
    ntp_adjtime		ntp_adjtime-function
    ntp_adjtime*	ntp_adjtime*-function
    )
  (import (rnrs)
    (foreign posix helpers)
    (prefix (foreign glibc time primitives) primitive:))


(define-parametrised stime calendar-time)

;;; --------------------------------------------------------------------

(define-parametrised gettimeofday)
(define-parametrised settimeofday timeval timezone)
(define-parametrised adjtime timeval-delta)

;;; --------------------------------------------------------------------

(define-parametrised localtime time malloc)
(define-parametrised localtime* time)
(define-parametrised gmtime time malloc)
(define-parametrised gmtime* time)
(define-parametrised timelocal tm-pointer)
(define-parametrised timelocal* tm-record)
(define-parametrised timegm tm-pointer)
(define-parametrised timegm* tm-record)

;;; --------------------------------------------------------------------

(define-parametrised ntp_gettime ntptimeval*)
(define-parametrised ntp_gettime*)
(define-parametrised ntp_adjtime timex*)
(define-parametrised ntp_adjtime* timex-record)


;;;; done

)

;;; end of file
