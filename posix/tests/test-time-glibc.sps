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
  (only (foreign ffi sizeof) valueof-int-max)
  (foreign memory)
  (foreign cstrings)
  (foreign errno)
  (foreign posix sizeof)
  (prefix (foreign posix time) posix:)
  (prefix (foreign glibc time) glibc:)
  (foreign posix time record-types))

(check-set-mode! 'report-failed)
(display "*** testing Glibc time\n")


;;;; helpers

(define (equal-<struct-tm>? a b)
  (and (equal? (<struct-tm>-sec a)
	       (<struct-tm>-sec b))
       (equal? (<struct-tm>-min a)
	       (<struct-tm>-min b))
       (equal? (<struct-tm>-hour a)
	       (<struct-tm>-hour b))
       (equal? (<struct-tm>-mday a)
	       (<struct-tm>-mday b))
       (equal? (<struct-tm>-mon a)
	       (<struct-tm>-mon b))
       (equal? (<struct-tm>-year a)
	       (<struct-tm>-year b))
       (equal? (<struct-tm>-wday a)
	       (<struct-tm>-wday b))
       (equal? (<struct-tm>-yday a)
	       (<struct-tm>-yday b))
       (equal? (<struct-tm>-isdst a)
	       (<struct-tm>-isdst b))
       (equal? (<struct-tm>-gmtoff a)
	       (<struct-tm>-gmtoff b))
       (pointer=? (<struct-tm>-zone a)
		  (<struct-tm>-zone b))))


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

      (check
	  (<struct-ntptimeval>? (glibc:ntp_gettime*))
      	=> #t)

      ;; (check
      ;; 	  ;;This should fail because root permissions are needed.
      ;; 	  (guard (E ((errno-condition? E)
      ;; 		     (errno-symbolic-value E))
      ;; 		    (else (write E) #f))
      ;; 	    (glibc:ntp_adjtime* (make-<struct-timex>
      ;; 				 100 ;modes
      ;; 				 100 ;offset
      ;; 				 100 ;frequency
      ;; 				 100 ;maxerror
      ;; 				 100 ;esterror
      ;; 				 100 ;status
      ;; 				 100 ;constant
      ;; 				 100 ;precision
      ;; 				 100 ;tolerance
      ;; 				 (make-<struct-timeval> 100 100) ;time
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
	(make-<struct-tm> 0			;sec
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
	(make-<struct-tm> 0			;sec
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
	(=> equal-<struct-tm>?)
	broken)

      #t)))


;;;; done

(check-report)

;;; end of file
