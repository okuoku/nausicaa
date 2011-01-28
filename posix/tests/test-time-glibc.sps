;;;
;;;Part of: Nausicaa/Glibc
;;;Contents: tests for the time functions
;;;Date: Mon Dec 22, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008-2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (nausicaa checks)
  (prefix (nausicaa ffi sizeof) ffi.)
  (prefix (nausicaa ffi memory) ffi.)
  (nausicaa ffi cstrings)
  (nausicaa ffi errno)
  (nausicaa posix sizeof)
  (prefix (nausicaa posix time) px.)
  (prefix (nausicaa glibc time) glibc.))

(check-set-mode! 'report-failed)
(display "*** testing Glibc time\n")


;;;; helpers

(define (equal-<tm>? (a <tm>) (b <tm>))
  (and (= a.tm_sec b.tm_sec)
       (= a.tm_min b.tm_min)
       (= a.tm_hour b.tm_hour)
       (= a.tm_mday b.tm_mday)
       (= a.tm_mon b.tm_mon)
       (= a.tm_year b.tm_year)
       (= a.tm_wday b.tm_wday)
       (= a.tm_yday b.tm_yday)
       (= a.tm_isdst b.tm_isdst)
       (= a.tm_gmtoff b.tm_gmtoff)
       (ffi.pointer=? a.tm_zone b.tm_zone)))


(parametrise ((check-test-name 'simple-calendar))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in simple calendar" E))
    (lambda ()

      ;; (check
      ;; 	  ;;This should fail because root permissions are needed.
      ;; 	  (guard (E ((errno-condition? E)
      ;; 		     (errno-symbolic-value E))
      ;; 		    (else
      ;; 		     #f))
      ;; 	    (glibc.stime (px.time)))
      ;; 	=> 'EPERM)

      #t)))


(parametrise ((check-test-name 'hires-calendar))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in hires calendar" E))
    (lambda ()

      (check
	  (let-values (((timeval timezone) (glibc.gettimeofday)))
	    #t)
	=> #t)

      ;; (check
      ;; 	  ;;This should fail because root permissions are needed.
      ;; 	  (guard (E ((errno-condition? E)
      ;; 		     (errno-symbolic-value E))
      ;; 		    (else #f))
      ;; 	    (let-values (((timeval timezone) (glibc.gettimeofday)))
      ;; 	      (glibc.settimeofday timeval timezone)))
      ;; 	=> 'EPERM)

      ;; (check
      ;; 	  ;;This should fail because root permissions are needed.
      ;; 	  (guard (E ((errno-condition? E)
      ;; 		     (errno-symbolic-value E))
      ;; 		    (else #f))
      ;; 	    (glibc.adjtime (make-<timeval> 0 0)))
      ;; 	=> 'EPERM)

      #t)))


(parametrise ((check-test-name 'broken-down-time))

  (define the-time (px.time))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in broken down" E))
    (lambda ()

      (check
	  (with-compensations
	    (let ((tm* (glibc.localtime (px.time) ffi.malloc-block/c)))
	      #t))
	=> #t)

      (check
	  (is-a? (glibc.localtime* (px.time)) <tm>)
	=> #t)

;;; --------------------------------------------------------------------

      (check
	  (with-compensations
	    (let ((tm* (glibc.localtime (px.time) ffi.malloc-block/c)))
	      #t))
	=> #t)

      (check
	  (is-a? (glibc.localtime* (px.time)) <tm>)
	=> #t)

;;; --------------------------------------------------------------------

      (check
	  (with-compensations
	    (let ((tm* (glibc.localtime the-time ffi.malloc-block/c)))
	      (glibc.timelocal tm*)))
	=> the-time)

      (check
	  (with-compensations
	    (let ((tm-record (glibc.localtime* the-time)))
	      (glibc.timelocal* tm-record)))
	=> the-time)

;;; --------------------------------------------------------------------

      (check
	  (with-compensations
	    (let ((tm* (glibc.gmtime the-time ffi.malloc-block/c)))
	      (glibc.timegm tm*)))
	=> the-time)

      (check
	  (with-compensations
	    (let ((tm-record (glibc.gmtime* the-time)))
	      (glibc.timegm* tm-record)))
	=> the-time)

      #t)))


(parametrise ((check-test-name 'high-accuracy-time))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in high accuracy" E))
    (lambda ()

      (check
	  (is-a? (glibc.ntp_gettime*) <ntptimeval>)
      	=> #t)

      ;; (check
      ;; 	  ;;This should fail because root permissions are needed.
      ;; 	  (guard (E ((errno-condition? E)
      ;; 		     (errno-symbolic-value E))
      ;; 		    (else (write E) #f))
      ;; 	    (glibc.ntp_adjtime* (make-<timex>
      ;; 				 100 ;modes
      ;; 				 100 ;offset
      ;; 				 100 ;frequency
      ;; 				 100 ;maxerror
      ;; 				 100 ;esterror
      ;; 				 100 ;status
      ;; 				 100 ;constant
      ;; 				 100 ;precision
      ;; 				 100 ;tolerance
      ;; 				 (make-<timeval> 100 100) ;time
      ;; 				 100 ;tick
      ;; 				 100 ;ppsfreq
      ;; 				 100 ;jitter
      ;; 				 100 ;shift
      ;; 				 100 ;stabil
      ;; 				 100 ;jitcnt
      ;; 				 100 ;calcnt
      ;; 				 100 ;errcnt
      ;; 				 100 ;stbcnt
      ;; 				 )))
      ;; 	=> 'EPERM)

      #t)))


(parametrise ((check-test-name 'format-broken-down))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in format broken-down" E))
    (lambda ()
      (define broken
	(make* <tm>
	  0			      ;sec
	  1			      ;min
	  2			      ;hour
	  3			      ;mday
	  4			      ;mon
	  5			      ;year
	  3			      ;wday
	  122			      ;yday, this is wrong
	  0			      ;isdst
	  0			      ;gmtoff
	  (string->cstring/c "CET"))) ;zone
      (define the-time
	(glibc.timelocal* broken))

;;; --------------------------------------------------------------------

      (check
	  (glibc.asctime* broken)
	=> "Wed May  3 02:01:00 1905\n")

      (check
	  (glibc.ctime the-time)
	=> "Wed May  3 02:01:00 1905\n")

      (check
	  (glibc.strftime* "%a %h %d %H:%M:%S %Y" broken)
	=> "Wed May 03 02:01:00 1905")

      #t)))


(parametrise ((check-test-name 'parsing-strings))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in parsing strings" E))
    (lambda ()
      (define broken
	(make* <tm>
	  0			  ;sec
	  1			  ;min
	  2			  ;hour
	  3			  ;mday
	  4			  ;mon
	  5			  ;year
	  3			  ;wday
	  122			  ;yday, this is wrong
	  (ffi.c-valueof int-max) ;isdst
	  (ffi.c-valueof int-max) ;gmtoff
	  ffi.pointer-null))	  ;zone
      (define template "%a %h %d %H:%M:%S %Y")
      (define the-string "Wed May 03 02:01:00 1905")
      (check
	  (glibc.strptime* the-string template)
	(=> equal-<tm>?)
	broken)

      #t)))


(parametrise ((check-test-name 'alarms))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in alarms" E))
    (lambda ()

      (check
	  (is-a?
	   (glibc.setitimer* ITIMER_REAL (make* <itimerval>
					   (make* <timeval> 0 0)
					   (make* <timeval> 999999 1)))
	   <itimerval>)
	=> #t)

      ;;The record returned by GETITIMER* has unpredictable values.
      ;;
      ;; (check
      ;; 	  (begin
      ;; 	    (glibc.setitimer* ITIMER_REAL (make-<itimerval>
      ;; 					   (make-<timeval> 0 0)
      ;; 					   (make-<timeval> 999999 1)))
      ;; 	    (let* ((r (glibc.getitimer* ITIMER_REAL))
      ;; 		   (i (<itimerval>-interval r))
      ;; 		   (v (<itimerval>-value    r)))
      ;; 	      (list (<timeval>-sec  i)
      ;; 		    (<timeval>-usec i)
      ;; 		    (<timeval>-sec  v)
      ;; 		    (<timeval>-usec v))))
      ;; 	=> '(0 0 999999 1))

      (check
	  (is-a? (glibc.getitimer* (c-valueof ITIMER_REAL)) <itimerval>)
	=> #t)

      (check
	  (glibc.alarm 999999)
	=> 999999)

      #t)))


(parametrise ((check-test-name 'sleep))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in sleep" E))
    (lambda ()

      (check
	  (glibc.sleep 1)
	=> 0)

      (check
	  (let (((r <timespec>) (glibc.nanosleep* (make* <timespec> 1 0))))
	    (list r.tv_sec r.tv_nsec))
	=> '(0 0))

      #t)))


;;;; done

(check-report)

;;; end of file
