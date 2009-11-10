;;;
;;;Part of: Nausicaa/Glibc
;;;Contents: tests for the time functions
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



(import (nausicaa)
  (checks)
  (deferred-exceptions)
  (compensations)
  (foreign errno)
  (foreign memory)
  (foreign posix sizeof)
  (prefix (foreign posix time) posix:)
  (prefix (foreign glibc time) glibc:)
  (foreign posix time record-types))

(check-set-mode! 'report-failed)
(display "*** testing Glibc time\n")


(parametrise ((check-test-name 'simple-calendar))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in simple calendar" E))
    (lambda ()

      (check
	  (guard (E (else (list (errno-condition? E)
				(condition-who E))))
	    (glibc:stime))
	=> '(#t stime))

      #t)))


(parametrise ((check-test-name 'hires-calendar))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in hires calendar" E))
    (lambda ()

      (check
	  (with-compensations
	    (let ((timeval	(malloc-block/c sizeof-struct-timeval))
		  (timezone	(malloc-block/c sizeof-struct-timezone)))
	      (glibc:gettimeofday timeval timezone)
;;;	  (format #t "epoch ~s~%" (struct-timeval-tv_sec-ref timeval))
	      (integer? (struct-timeval-tv_sec-ref timeval))))
	=> #t)

      #t)))


(parametrise ((check-test-name 'broken-down-time))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in broken down" E))
    (lambda ()

      (check
	  (with-compensations
	    (let ((*tm	(malloc-block/c sizeof-struct-tm)))
	      (glibc:localtime (posix:time) *tm)
	      (struct-tm-tm_year-ref *tm)))
	=> 108) ;; this is 1900 + 109 = 2008, it works only in 2008

      (check
	  (with-compensations
	    (let ((*tm	(malloc-block/c sizeof-struct-tm)))
	      (glibc:gmtime (posix:time) *tm)
	      (struct-tm-tm_year-ref *tm)))
	=> 108) ;; this is 1900 + 109 = 2008, it works only in 2008

      (let ((t (posix:time)))
	(check
	    (with-compensations
	      (let ((*tm	(malloc-block/c sizeof-struct-tm)))
		(glibc:localtime t *tm)
		(glibc:timelocal *tm)))
	  => t))

      (let ((t (posix:time)))
	(check
	    (with-compensations
	      (let ((*tm	(malloc-block/c sizeof-struct-tm)))
		(glibc:gmtime t *tm)
		(glibc:timegm *tm)))
	  => t))

      ;; (let ((t (posix:time)))
      ;; 	(check
      ;; 	    (with-compensations
      ;; 	      (let ((*tm	(malloc-block/c sizeof-struct-tm)))
      ;; 		(glibc:localtime t *tm)
      ;; 		(glibc:mktime *tm)))
      ;; 	  => t))

      #t)))


(parametrise ((check-test-name 'high-accuracy-time))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in high accuracy" E))
    (lambda ()

      ;; (check
      ;; 	  (with-compensations
      ;; 	    (let ((p (malloc-block/c sizeof-struct-ntptimeval)))
      ;; 	      (glibc:ntp_gettime p)
      ;; 	      (integer? (struct-timeval-tv_sec-ref (struct-ntptimeval-time-ref p)))))
      ;; 	=> #t)

      #t)))


;;;; done

(check-report)

;;; end of file
