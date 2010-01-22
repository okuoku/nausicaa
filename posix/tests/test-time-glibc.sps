;;;
;;;Part of: Nausicaa/Glibc
;;;Contents: tests for the time functions
;;;Date: Mon Dec 22, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008, 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (only (foreign ffi sizeof) valueof-int-max)
  (foreign memory)
  (foreign cstrings)
  (foreign errno)
  (posix sizeof)
  (posix typedefs)
  (prefix (posix time) posix:)
  (prefix (glibc time) glibc:))

(check-set-mode! 'report-failed)
(display "*** testing Glibc time\n")


;;;; helpers

(define (equal-<tm>? a b)
  (and (equal? (<tm>-sec a)
	       (<tm>-sec b))
       (equal? (<tm>-min a)
	       (<tm>-min b))
       (equal? (<tm>-hour a)
	       (<tm>-hour b))
       (equal? (<tm>-mday a)
	       (<tm>-mday b))
       (equal? (<tm>-mon a)
	       (<tm>-mon b))
       (equal? (<tm>-year a)
	       (<tm>-year b))
       (equal? (<tm>-wday a)
	       (<tm>-wday b))
       (equal? (<tm>-yday a)
	       (<tm>-yday b))
       (equal? (<tm>-isdst a)
	       (<tm>-isdst b))
       (equal? (<tm>-gmtoff a)
	       (<tm>-gmtoff b))
       (pointer=? (<tm>-zone a)
		  (<tm>-zone b))))


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
      ;; 	    (glibc:stime (posix:time)))
      ;; 	=> 'EPERM)

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

      ;; (check
      ;; 	  ;;This should fail because root permissions are needed.
      ;; 	  (guard (E ((errno-condition? E)
      ;; 		     (errno-symbolic-value E))
      ;; 		    (else #f))
      ;; 	    (let-values (((timeval timezone) (glibc:gettimeofday)))
      ;; 	      (glibc:settimeofday timeval timezone)))
      ;; 	=> 'EPERM)

      ;; (check
      ;; 	  ;;This should fail because root permissions are needed.
      ;; 	  (guard (E ((errno-condition? E)
      ;; 		     (errno-symbolic-value E))
      ;; 		    (else #f))
      ;; 	    (glibc:adjtime (make-<timeval> 0 0)))
      ;; 	=> 'EPERM)

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
	  (<tm>? (glibc:localtime* (posix:time)))
	=> #t)

;;; --------------------------------------------------------------------

      (check
	  (with-compensations
	    (let ((tm* (glibc:localtime (posix:time) malloc-block/c)))
	      #t))
	=> #t)

      (check
	  (<tm>? (glibc:localtime* (posix:time)))
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

      (check
	  (<ntptimeval>? (glibc:ntp_gettime*))
      	=> #t)

      ;; (check
      ;; 	  ;;This should fail because root permissions are needed.
      ;; 	  (guard (E ((errno-condition? E)
      ;; 		     (errno-symbolic-value E))
      ;; 		    (else (write E) #f))
      ;; 	    (glibc:ntp_adjtime* (make-<timex>
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
	(make-<tm> 0			;sec
			  1			;min
			  2			;hour
			  3			;mday
			  4			;mon
			  5			;year
			  3			;wday
			  122			;yday, this is wrong
			  0			;isdst
			  0			;gmtoff
			  (string->cstring/c "CET") ;zone
			  ))

      (define the-time
	(glibc:timelocal* broken))

;;; --------------------------------------------------------------------

      (check
	  (glibc:asctime* broken)
	=> "Wed May  3 02:01:00 1905\n")

      (check
	  (glibc:ctime the-time)
	=> "Wed May  3 02:01:00 1905\n")

      (check
	  (glibc:strftime* "%a %h %d %H:%M:%S %Y" broken)
	=> "Wed May 03 02:01:00 1905")

      #t)))


(parametrise ((check-test-name 'parsing-strings))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in parsing strings" E))
    (lambda ()

      (define broken
	(make-<tm> 0			;sec
			  1			;min
			  2			;hour
			  3			;mday
			  4			;mon
			  5			;year
			  3			;wday
			  122			;yday, this is wrong
			  valueof-int-max	;isdst
			  valueof-int-max	;gmtoff
			  pointer-null		;zone
			  ))

      (define template "%a %h %d %H:%M:%S %Y")
      (define the-string "Wed May 03 02:01:00 1905")

      (check
	  (glibc:strptime* the-string template)
	(=> equal-<tm>?)
	broken)

      #t)))


(parametrise ((check-test-name 'alarms))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in alarms" E))
    (lambda ()

      (check
	  (<itimerval>?
	   (glibc:setitimer* ITIMER_REAL (make-<itimerval>
					  (make-<timeval> 0 0)
					  (make-<timeval> 999999 1))))
	=> #t)

      ;;The record returned by GETITIMER* has unpredictable values.
      ;;
      ;; (check
      ;; 	  (begin
      ;; 	    (glibc:setitimer* ITIMER_REAL (make-<itimerval>
      ;; 					   (make-<timeval> 0 0)
      ;; 					   (make-<timeval> 999999 1)))
      ;; 	    (let* ((r (glibc:getitimer* ITIMER_REAL))
      ;; 		   (i (<itimerval>-interval r))
      ;; 		   (v (<itimerval>-value    r)))
      ;; 	      (list (<timeval>-sec  i)
      ;; 		    (<timeval>-usec i)
      ;; 		    (<timeval>-sec  v)
      ;; 		    (<timeval>-usec v))))
      ;; 	=> '(0 0 999999 1))

      (check
	  (<itimerval>? (glibc:getitimer* ITIMER_REAL))
	=> #t)

      (check
	  (glibc:alarm 999999)
	=> 999999)

      #t)))


(parametrise ((check-test-name 'sleep))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in sleep" E))
    (lambda ()

      (check
	  (glibc:sleep 1)
	=> 0)

      (check
	  (let ((r (glibc:nanosleep* (make-<timespec> 1 0))))
	    (list (<timespec>-sec  r)
		  (<timespec>-nsec r)))
	=> '(0 0))

      #t)))


;;;; done

(check-report)

;;; end of file
