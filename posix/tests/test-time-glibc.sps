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
	  ;;This should fail because root permissions are needed.
	  (guard (E ((errno-condition? E)
		     (errno-symbolic-value E))
		    (else
		     #f))
	    (glibc:stime (posix:time)))
	=> 'EPERM)

      #t)))


(parametrise ((check-test-name 'hires-calendar))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in hires calendar" E))
    (lambda ()

      (check
	  (let-values (((timeval timezone) (glibc:gettimeofday)))
	    #t)
	=> #t)

      (check
	  ;;This should fail because root permissions are needed.
	  (guard (E ((errno-condition? E)
		     (errno-symbolic-value E))
		    (else #f))
	    (let-values (((timeval timezone) (glibc:gettimeofday)))
	      (glibc:settimeofday timeval timezone)))
	=> 'EPERM)

      (check
	  ;;This should fail because root permissions are needed.
	  (guard (E ((errno-condition? E)
		     (errno-symbolic-value E))
		    (else #f))
	    (glibc:adjtime (make-<struct-timeval> 0 0)))
	=> 'EPERM)

      #t)))


(parametrise ((check-test-name 'broken-down-time))

  (define the-time (posix:time))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in broken down" E))
    (lambda ()

      (check
	  (with-compensations
	    (let ((tm* (glibc:localtime (posix:time) malloc-block/c)))
	      #t))
	=> #t)

      (check
	  (<struct-tm>? (glibc:localtime* (posix:time)))
	=> #t)

;;; --------------------------------------------------------------------

      (check
	  (with-compensations
	    (let ((tm* (glibc:localtime (posix:time) malloc-block/c)))
	      #t))
	=> #t)

      (check
	  (<struct-tm>? (glibc:localtime* (posix:time)))
	=> #t)

;;; --------------------------------------------------------------------

      (check
	  (with-compensations
	    (let ((tm* (glibc:localtime the-time malloc-block/c)))
	      (glibc:timelocal tm*)))
	=> the-time)

      (check
	  (with-compensations
	    (let ((tm-record (glibc:localtime* the-time)))
	      (glibc:timelocal* tm-record)))
	=> the-time)

;;; --------------------------------------------------------------------

      (check
	  (with-compensations
	    (let ((tm* (glibc:gmtime the-time malloc-block/c)))
	      (glibc:timegm tm*)))
	=> the-time)

      (check
	  (with-compensations
	    (let ((tm-record (glibc:gmtime* the-time)))
	      (glibc:timegm* tm-record)))
	=> the-time)

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
