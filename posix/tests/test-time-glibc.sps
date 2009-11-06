;;;
;;;Part of: Nausicaa/Glibc
;;;Contents: tests for the time functions
;;;Date: Mon Dec 22, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
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

(import (r6rs)
  (uriel lang)
  (uriel foreign)
  (uriel test)
  (posix sizeof)
  (glibc sizeof)
  (glibc time))

(check-set-mode! 'report-failed)



(parameterize ((testname 'simple-calendar-time))

  (check
      (integer? (time))
    => #t)

  (check
      (guard (exc (else
		   (list (errno-condition? exc)
			 (condition-who exc))))
	(stime))
    => '(#t primitive-stime))

  )



(parameterize ((testname 'hires-calendar-time))

  (check
      (with-compensations
	(let ((timeval	(malloc-block/c sizeof-struct-timeval))
	      (timezone	(malloc-block/c sizeof-struct-timezone)))
	  (gettimeofday timeval timezone)
;;;	  (format #t "epoch ~s~%" (struct-timeval-tv_sec-ref timeval))
	  (integer? (struct-timeval-tv_sec-ref timeval))))
    => #t)

  )



(parameterize ((testname 'broken-down-time))

  (check
      (with-compensations
	(let ((*tm	(malloc-block/c sizeof-struct-tm)))
	  (localtime (time) *tm)
	  (struct-tm-tm_year-ref *tm)))
    => 108) ;; this is 1900 + 109 = 2008, it works only in 2008

  (check
      (with-compensations
	(let ((*tm	(malloc-block/c sizeof-struct-tm)))
	  (gmtime (time) *tm)
	  (struct-tm-tm_year-ref *tm)))
    => 108) ;; this is 1900 + 109 = 2008, it works only in 2008

  (let ((t (time)))
    (check
	(with-compensations
	  (let ((*tm	(malloc-block/c sizeof-struct-tm)))
	    (localtime t *tm)
	    (timelocal *tm)))
      => t))

  (let ((t (time)))
    (check
	(with-compensations
	  (let ((*tm	(malloc-block/c sizeof-struct-tm)))
	    (gmtime t *tm)
	    (timegm *tm)))
      => t))

  (let ((t (time)))
    (check
	(with-compensations
	  (let ((*tm	(malloc-block/c sizeof-struct-tm)))
	    (localtime t *tm)
	    (mktime *tm)))
      => t))

  )



(parameterize ((testname 'high-accuracy-time))

  (check
      (with-compensations
	(let ((p (malloc-block/c sizeof-struct-ntptimeval)))
	  (ntp_gettime p)
	  (format #t "secs ~s~%"
		  (struct-timeval-tv_sec (struct-ntptimeval-time-ref p)))
	  (integer? (struct-timeval-tv_sec
		     (struct-ntptimeval-time-ref p)))))
    => #t)
  )



;;;; done

(check-report)

;;; end of file
