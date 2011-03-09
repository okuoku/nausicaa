;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: tests for the time and date struct interfaces
;;;Date: Fri Jan 28, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (nausicaa)
  (nausicaa checks)
  (nausicaa posix sizeof)
  (only (nausicaa ffi cstrings) string->cstring/c)
  (prefix (nausicaa ffi memory) mem.))

(check-set-mode! 'report-failed)
(display "*** testing time structs\n")


(parametrise ((check-test-name	'timeval))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in structs" E))
    (lambda ()

      (with-compensations ;from mirror to any
	(let (((M <timeval>) (make* <timeval>
			       1 2)))

	  (let (((P <pointer-to-timeval>) (make <pointer-to-timeval>
					    (mirror: M)
					    (malloc: mem.malloc-block/c))))
	    (check P.tv_sec	=> 1)
	    (check P.tv_usec	=> 2)
	    #f)

	  (let (((W <struct-timeval>) (make <struct-timeval>
					(mirror: M)
					(malloc: mem.malloc-block/c))))
	    (check W.tv_sec	=> 1)
	    (check W.tv_usec	=> 2)
	    #f)

	  (let (((D <timeval>) (make <timeval>
				 (mirror: M))))
	    (check D.tv_sec	=> 1)
	    (check D.tv_usec	=> 2)
	    #f)

	  #f))

      (with-compensations ;from wrapper to any
	(let (((W <struct-timeval>) (make <struct-timeval>
				      (malloc: mem.malloc-block/c))))
	  (set! W.tv_sec  1)
	  (set! W.tv_usec 2)

	  (let (((P <pointer-to-timeval>) (make <pointer-to-timeval>
					    (wrapper: W)
					    (malloc: mem.malloc-block/c))))
	    (check P.tv_sec	=> 1)
	    (check P.tv_usec	=> 2)
	    #f)

	  (let (((W <struct-timeval>) (make <struct-timeval>
					(wrapper: W)
					(malloc: mem.malloc-block/c))))
	    (check W.tv_sec	=> 1)
	    (check W.tv_usec	=> 2)
	    #f)

	  (let (((D <timeval>) (make <timeval>
				 (wrapper: W))))
	    (check D.tv_sec	=> 1)
	    (check D.tv_usec	=> 2)
	    #f)

	  #f))

	(with-compensations ;from pointer to any
	  (let (((P <pointer-to-timeval>) (make <pointer-to-timeval>
					    (malloc: mem.malloc-block/c))))
	    (set! P.tv_sec  1)
	    (set! P.tv_usec 2)

	    (let (((Q <pointer-to-timeval>) (make <pointer-to-timeval>
					      (pointer: P)
					      (malloc: mem.malloc-block/c))))
	      (check Q.tv_sec	=> 1)
	      (check Q.tv_usec	=> 2)
	      #f)

	    (let (((W <struct-timeval>) (make <struct-timeval>
					  (pointer: P)
					  (malloc: mem.malloc-block/c))))
	      (check W.tv_sec	=> 1)
	      (check W.tv_usec	=> 2)
	      #f)

	    (let (((M <timeval>) (make <timeval>
				   (pointer: P))))
	      (check M.tv_sec	=> 1)
	      (check M.tv_usec	=> 2)
	      #f)

	    #f))

	#t)))


(parametrise ((check-test-name	'timespec))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in structs" E))
    (lambda ()

      (with-compensations ;from mirror to any
	(let (((M <timespec>) (make* <timespec>
				1 2)))

	  (let (((P <pointer-to-timespec>) (make <pointer-to-timespec>
					     (mirror: M)
					     (malloc: mem.malloc-block/c))))
	    (check P.tv_sec	=> 1)
	    (check P.tv_nsec	=> 2)
	    #f)

	  (let (((W <struct-timespec>) (make <struct-timespec>
					 (mirror: M)
					 (malloc: mem.malloc-block/c))))
	    (check W.tv_sec	=> 1)
	    (check W.tv_nsec	=> 2)
	    #f)

	  (let (((D <timespec>) (make <timespec>
				  (mirror: M))))
	    (check D.tv_sec	=> 1)
	    (check D.tv_nsec	=> 2)
	    #f)

	  #f))

      (with-compensations ;from wrapper to any
	(let (((W <struct-timespec>) (make <struct-timespec>
				       (malloc: mem.malloc-block/c))))
	  (set! W.tv_sec  1)
	  (set! W.tv_nsec  2)

	  (let (((P <pointer-to-timespec>) (make <pointer-to-timespec>
					     (wrapper: W)
					     (malloc: mem.malloc-block/c))))
	    (check P.tv_sec	=> 1)
	    (check P.tv_nsec	=> 2)
	    #f)

	  (let (((W <struct-timespec>) (make <struct-timespec>
					 (wrapper: W)
					 (malloc: mem.malloc-block/c))))
	    (check W.tv_sec	=> 1)
	    (check W.tv_nsec	=> 2)
	    #f)

	  (let (((D <timespec>) (make <timespec>
				  (wrapper: W))))
	    (check D.tv_sec	=> 1)
	    (check D.tv_nsec	=> 2)
	    #f)

	  #f))

      (with-compensations ;from pointer to any
	(let (((P <pointer-to-timespec>) (make <pointer-to-timespec>
					   (malloc: mem.malloc-block/c))))
	  (set! P.tv_sec  1)
	  (set! P.tv_nsec 2)

	  (let (((Q <pointer-to-timespec>) (make <pointer-to-timespec>
					     (pointer: P)
					     (malloc: mem.malloc-block/c))))
	    (check Q.tv_sec	=> 1)
	    (check Q.tv_nsec	=> 2)
	    #f)

	  (let (((W <struct-timespec>) (make <struct-timespec>
					 (pointer: P)
					 (malloc: mem.malloc-block/c))))
	    (check W.tv_sec	=> 1)
	    (check W.tv_nsec	=> 2)
	    #f)

	  (let (((M <timespec>) (make <timespec>
				  (pointer: P))))
	    (check M.tv_sec	=> 1)
	    (check M.tv_nsec	=> 2)
	    #f)

	  #f))

      #t)))


(parametrise ((check-test-name	'tms))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in structs" E))
    (lambda ()

      (with-compensations ;from mirror to any
	(let (((M <tms>) (make* <tms> 1 2 3 4)))

	  (let (((P <pointer-to-tms>) (make <pointer-to-tms>
					(mirror: M)
					(malloc: mem.malloc-block/c))))
	    (check P.tms_utime	=> 1)
	    (check P.tms_stime	=> 2)
	    (check P.tms_cutime	=> 3)
	    (check P.tms_cstime	=> 4))

	  (let (((W <struct-tms>) (make <struct-tms>
				    (mirror: M)
				    (malloc: mem.malloc-block/c))))
	    (check W.tms_utime	=> 1)
	    (check W.tms_stime	=> 2)
	    (check W.tms_cutime	=> 3)
	    (check W.tms_cstime	=> 4))

	  (let (((D <tms>) (make <tms>
			     (mirror: M))))
	    (check D.tms_utime	=> 1)
	    (check D.tms_stime	=> 2)
	    (check D.tms_cutime	=> 3)
	    (check D.tms_cstime	=> 4))

	  #f))

      (with-compensations ;from wrapper to any
	(let (((W <struct-tms>) (make <struct-tms>
				  (malloc: mem.malloc-block/c))))
	  (set! W.tms_utime  1)
	  (set! W.tms_stime  2)
	  (set! W.tms_cutime 3)
	  (set! W.tms_cstime 4)

	  (let (((P <pointer-to-tms>) (make <pointer-to-tms>
					(wrapper: W)
					(malloc: mem.malloc-block/c))))
	    (check P.tms_utime	=> 1)
	    (check P.tms_stime	=> 2)
	    (check P.tms_cutime	=> 3)
	    (check P.tms_cstime	=> 4))

	  (let (((W <struct-tms>) (make <struct-tms>
				    (wrapper: W)
				    (malloc: mem.malloc-block/c))))
	    (check W.tms_utime	=> 1)
	    (check W.tms_stime	=> 2)
	    (check W.tms_cutime	=> 3)
	    (check W.tms_cstime	=> 4))

	  (let (((D <tms>) (make <tms>
			     (wrapper: W))))
	    (check D.tms_utime	=> 1)
	    (check D.tms_stime	=> 2)
	    (check D.tms_cutime	=> 3)
	    (check D.tms_cstime	=> 4))

	  #f))

      (with-compensations ;from pointer to any
	(let (((P <pointer-to-tms>) (make <pointer-to-tms>
				      (malloc: mem.malloc-block/c))))
	  (set! P.tms_utime  1)
	  (set! P.tms_stime  2)
	  (set! P.tms_cutime 3)
	  (set! P.tms_cstime 4)

	  (let (((Q <pointer-to-tms>) (make <pointer-to-tms>
					(pointer: P)
					(malloc: mem.malloc-block/c))))
	    (check Q.tms_utime	=> 1)
	    (check Q.tms_stime	=> 2)
	    (check Q.tms_cutime	=> 3)
	    (check Q.tms_cstime	=> 4))

	  (let (((W <struct-tms>) (make <struct-tms>
				    (pointer: P)
				    (malloc: mem.malloc-block/c))))
	    (check W.tms_utime	=> 1)
	    (check W.tms_stime	=> 2)
	    (check W.tms_cutime	=> 3)
	    (check W.tms_cstime	=> 4))

	  (let (((M <tms>) (make <tms>
			     (pointer: P))))
	    (check M.tms_utime	=> 1)
	    (check M.tms_stime	=> 2)
	    (check M.tms_cutime	=> 3)
	    (check M.tms_cstime	=> 4))

	  #f))

      #t)))


(parametrise ((check-test-name	'timezone))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in structs" E))
    (lambda ()

      (with-compensations ;from mirror to any
	(let (((M <timezone>) (make* <timezone>
				1 2)))

	  (let (((P <pointer-to-timezone>) (make <pointer-to-timezone>
					     (mirror: M)
					     (malloc: mem.malloc-block/c))))
	    (check P.tz_minuteswest	=> 1)
	    (check P.tz_dsttime		=> 2)
	    #f)

	  (let (((W <struct-timezone>) (make <struct-timezone>
					 (mirror: M)
					 (malloc: mem.malloc-block/c))))
	    (check W.tz_minuteswest	=> 1)
	    (check W.tz_dsttime		=> 2)
	    #f)

	  (let (((D <timezone>) (make <timezone>
				  (mirror: M))))
	    (check D.tz_minuteswest	=> 1)
	    (check D.tz_dsttime		=> 2)
	    #f)

	  #f))

      (with-compensations ;from wrapper to any
	(let (((W <struct-timezone>) (make <struct-timezone>
				       (malloc: mem.malloc-block/c))))
	  (set! W.tz_minuteswest  1)
	  (set! W.tz_dsttime	  2)

	  (let (((P <pointer-to-timezone>) (make <pointer-to-timezone>
					     (wrapper: W)
					     (malloc: mem.malloc-block/c))))
	    (check P.tz_minuteswest	=> 1)
	    (check P.tz_dsttime		=> 2)
	    #f)

	  (let (((W <struct-timezone>) (make <struct-timezone>
					 (wrapper: W)
					 (malloc: mem.malloc-block/c))))
	    (check W.tz_minuteswest	=> 1)
	    (check W.tz_dsttime		=> 2)
	    #f)

	  (let (((D <timezone>) (make <timezone>
				  (wrapper: W))))
	    (check D.tz_minuteswest	=> 1)
	    (check D.tz_dsttime		=> 2)
	    #f)

	  #f))

      (with-compensations ;from pointer to any
	(let (((P <pointer-to-timezone>) (make <pointer-to-timezone>
					   (malloc: mem.malloc-block/c))))
	  (set! P.tz_minuteswest 1)
	  (set! P.tz_dsttime	 2)

	  (let (((Q <pointer-to-timezone>) (make <pointer-to-timezone>
					     (pointer: P)
					     (malloc: mem.malloc-block/c))))
	    (check Q.tz_minuteswest	=> 1)
	    (check Q.tz_dsttime		=> 2)
	    #f)

	  (let (((W <struct-timezone>) (make <struct-timezone>
					 (pointer: P)
					 (malloc: mem.malloc-block/c))))
	    (check W.tz_minuteswest	=> 1)
	    (check W.tz_dsttime		=> 2)
	    #f)

	  (let (((M <timezone>) (make <timezone>
				  (pointer: P))))
	    (check M.tz_minuteswest	=> 1)
	    (check M.tz_dsttime		=> 2)
	    #f)

	  #f))

      #t)))


(parametrise ((check-test-name	'tm))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in structs" E))
    (lambda ()

      (with-compensations

	(define it
	  (string->cstring/c "it"))

;;; from mirror to any
	(let (((M <tm>) (make* <tm>
			  1 #|sec|# 2 #|min|# 3 #|hour|# 4 #|mday|# 5 #|mon|#
			  6 #|year|# 7 #|wday|# 8 #|yday|# 9 #|isdst|#
			  10 #|gmtoff|# it #|zone|#)))

	  (let (((P <pointer-to-tm>) (make <pointer-to-tm>
				       (mirror: M)
				       (malloc: mem.malloc-block/c))))
	    (check P.tm_sec	=> 1)
	    (check P.tm_min	=> 2)
	    (check P.tm_hour	=> 3)
	    (check P.tm_mday	=> 4)
	    (check P.tm_mon	=> 5)
	    (check P.tm_year	=> 6)
	    (check P.tm_wday	=> 7)
	    (check P.tm_yday	=> 8)
	    (check P.tm_isdst	=> 9)
	    (check P.tm_gmtoff	=> 10)
	    (check P.tm_zone	(=> mem.pointer=?) it)
	    #f)

	  (let (((W <struct-tm>) (make <struct-tm>
				   (mirror: M)
				   (malloc: mem.malloc-block/c))))
	    (check W.tm_sec	=> 1)
	    (check W.tm_min	=> 2)
	    (check W.tm_hour	=> 3)
	    (check W.tm_mday	=> 4)
	    (check W.tm_mon	=> 5)
	    (check W.tm_year	=> 6)
	    (check W.tm_wday	=> 7)
	    (check W.tm_yday	=> 8)
	    (check W.tm_isdst	=> 9)
	    (check W.tm_gmtoff	=> 10)
	    (check W.tm_zone	(=> mem.pointer=?) it)
	    #f)

	  (let (((D <tm>) (make <tm>
			    (mirror: M))))
	    (check D.tm_sec	=> 1)
	    (check D.tm_min	=> 2)
	    (check D.tm_hour	=> 3)
	    (check D.tm_mday	=> 4)
	    (check D.tm_mon	=> 5)
	    (check D.tm_year	=> 6)
	    (check D.tm_wday	=> 7)
	    (check D.tm_yday	=> 8)
	    (check D.tm_isdst	=> 9)
	    (check D.tm_gmtoff	=> 10)
	    (check D.tm_zone	(=> mem.pointer=?) it)
	    #f)

	  #f)

;;;from wrapper to any
	(let (((W <struct-tm>) (make <struct-tm>
				 (malloc: mem.malloc-block/c))))
	  (set! W.tm_sec	1)
	  (set! W.tm_min	2)
	  (set! W.tm_hour	3)
	  (set! W.tm_mday	4)
	  (set! W.tm_mon	5)
	  (set! W.tm_year	6)
	  (set! W.tm_wday	7)
	  (set! W.tm_yday	8)
	  (set! W.tm_isdst	9)
	  (set! W.tm_gmtoff	10)
	  (set! W.tm_zone	it)

	  (let (((P <pointer-to-tm>) (make <pointer-to-tm>
				       (wrapper: W)
				       (malloc: mem.malloc-block/c))))
	    (check P.tm_sec	=> 1)
	    (check P.tm_min	=> 2)
	    (check P.tm_hour	=> 3)
	    (check P.tm_mday	=> 4)
	    (check P.tm_mon	=> 5)
	    (check P.tm_year	=> 6)
	    (check P.tm_wday	=> 7)
	    (check P.tm_yday	=> 8)
	    (check P.tm_isdst	=> 9)
	    (check P.tm_gmtoff	=> 10)
	    (check P.tm_zone	(=> mem.pointer=?) it)
	    #f)

	  (let (((W <struct-tm>) (make <struct-tm>
				   (wrapper: W)
				   (malloc: mem.malloc-block/c))))
	    (check W.tm_sec	=> 1)
	    (check W.tm_min	=> 2)
	    (check W.tm_hour	=> 3)
	    (check W.tm_mday	=> 4)
	    (check W.tm_mon	=> 5)
	    (check W.tm_year	=> 6)
	    (check W.tm_wday	=> 7)
	    (check W.tm_yday	=> 8)
	    (check W.tm_isdst	=> 9)
	    (check W.tm_gmtoff	=> 10)
	    (check W.tm_zone	(=> mem.pointer=?) it)
	    #f)

	  (let (((D <tm>) (make <tm>
			    (wrapper: W))))
	    (check D.tm_sec	=> 1)
	    (check D.tm_min	=> 2)
	    (check D.tm_hour	=> 3)
	    (check D.tm_mday	=> 4)
	    (check D.tm_mon	=> 5)
	    (check D.tm_year	=> 6)
	    (check D.tm_wday	=> 7)
	    (check D.tm_yday	=> 8)
	    (check D.tm_isdst	=> 9)
	    (check D.tm_gmtoff	=> 10)
	    (check D.tm_zone	(=> mem.pointer=?) it)
	    #f)

	  #f)

;;;from pointer to any
	(let (((P <pointer-to-tm>) (make <pointer-to-tm>
				     (malloc: mem.malloc-block/c))))
	  (set! P.tm_sec	1)
	  (set! P.tm_min	2)
	  (set! P.tm_hour	3)
	  (set! P.tm_mday	4)
	  (set! P.tm_mon	5)
	  (set! P.tm_year	6)
	  (set! P.tm_wday	7)
	  (set! P.tm_yday	8)
	  (set! P.tm_isdst	9)
	  (set! P.tm_gmtoff	10)
	  (set! P.tm_zone	it)

	  (let (((Q <pointer-to-tm>) (make <pointer-to-tm>
				       (pointer: P)
				       (malloc: mem.malloc-block/c))))
	    (check Q.tm_sec	=> 1)
	    (check Q.tm_min	=> 2)
	    (check Q.tm_hour	=> 3)
	    (check Q.tm_mday	=> 4)
	    (check Q.tm_mon	=> 5)
	    (check Q.tm_year	=> 6)
	    (check Q.tm_wday	=> 7)
	    (check Q.tm_yday	=> 8)
	    (check Q.tm_isdst	=> 9)
	    (check Q.tm_gmtoff	=> 10)
	    (check Q.tm_zone	(=> mem.pointer=?) it)
	    #f)

	  (let (((W <struct-tm>) (make <struct-tm>
				   (pointer: P)
				   (malloc: mem.malloc-block/c))))
	    (check W.tm_sec	=> 1)
	    (check W.tm_min	=> 2)
	    (check W.tm_hour	=> 3)
	    (check W.tm_mday	=> 4)
	    (check W.tm_mon	=> 5)
	    (check W.tm_year	=> 6)
	    (check W.tm_wday	=> 7)
	    (check W.tm_yday	=> 8)
	    (check W.tm_isdst	=> 9)
	    (check W.tm_gmtoff	=> 10)
	    (check W.tm_zone	(=> mem.pointer=?) it)
	    #f)

	  (let (((M <tm>) (make <tm>
			    (pointer: P))))
	    (check M.tm_sec	=> 1)
	    (check M.tm_min	=> 2)
	    (check M.tm_hour	=> 3)
	    (check M.tm_mday	=> 4)
	    (check M.tm_mon	=> 5)
	    (check M.tm_year	=> 6)
	    (check M.tm_wday	=> 7)
	    (check M.tm_yday	=> 8)
	    (check M.tm_isdst	=> 9)
	    (check M.tm_gmtoff	=> 10)
	    (check M.tm_zone	(=> mem.pointer=?) it)
	    #f)

	  #f)

      #t))))


(parametrise ((check-test-name	'ntptimeval))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in structs" E))
    (lambda ()

      (with-compensations ;from mirror to any
	(let (((M <ntptimeval>) (make* <ntptimeval>
				  (make* <timeval> 1 2)
				  3 4)))

	  (let (((P <pointer-to-ntptimeval>) (make <pointer-to-ntptimeval>
					       (mirror: M)
					       (malloc: mem.malloc-block/c))))
	    (check P.time.tv_sec	=> 1)
	    (check P.time.tv_usec	=> 2)
	    (check P.maxerror		=> 3)
	    (check P.esterror		=> 4)
	    #f)

	  (let (((W <struct-ntptimeval>) (make <struct-ntptimeval>
					   (mirror: M)
					   (malloc: mem.malloc-block/c))))
	    (check W.time.tv_sec	=> 1)
	    (check W.time.tv_usec	=> 2)
	    (check W.maxerror		=> 3)
	    (check W.esterror		=> 4)
	    #f)

	  (let (((D <ntptimeval>) (make <ntptimeval>
				    (mirror: M))))
	    (check D.time.tv_sec	=> 1)
	    (check D.time.tv_usec	=> 2)
	    (check D.maxerror		=> 3)
	    (check D.esterror		=> 4)
	    #f)

	  #f))

      (with-compensations ;from wrapper to any
	(let (((W <struct-ntptimeval>) (make <struct-ntptimeval>
					 (malloc: mem.malloc-block/c))))
	  (set! W.time.tv_sec	1)
	  (set! W.time.tv_usec	2)
	  (set! W.maxerror	3)
	  (set! W.esterror	4)

	  (let (((P <pointer-to-ntptimeval>) (make <pointer-to-ntptimeval>
					       (wrapper: W)
					       (malloc: mem.malloc-block/c))))
	    (check P.time.tv_sec	=> 1)
	    (check P.time.tv_usec	=> 2)
	    (check P.maxerror		=> 3)
	    (check P.esterror		=> 4)
	    #f)

	  (let (((W <struct-ntptimeval>) (make <struct-ntptimeval>
					   (wrapper: W)
					   (malloc: mem.malloc-block/c))))
	    (check W.time.tv_sec	=> 1)
	    (check W.time.tv_usec	=> 2)
	    (check W.maxerror		=> 3)
	    (check W.esterror		=> 4)
	    #f)

	  (let (((D <ntptimeval>) (make <ntptimeval>
				    (wrapper: W))))
	    (check D.time.tv_sec	=> 1)
	    (check D.time.tv_usec	=> 2)
	    (check D.maxerror		=> 3)
	    (check D.esterror		=> 4)
	    #f)

	  #f))

      (with-compensations ;from pointer to any
	(let (((P <pointer-to-ntptimeval>) (make <pointer-to-ntptimeval>
					     (malloc: mem.malloc-block/c))))
	  (set! P.time.tv_sec	1)
	  (set! P.time.tv_usec	2)
	  (set! P.maxerror	3)
	  (set! P.esterror	4)

	  (let (((Q <pointer-to-ntptimeval>) (make <pointer-to-ntptimeval>
					       (pointer: P)
					       (malloc: mem.malloc-block/c))))
	    (check Q.time.tv_sec	=> 1)
	    (check Q.time.tv_usec	=> 2)
	    (check Q.maxerror		=> 3)
	    (check Q.esterror		=> 4)
	    #f)

	  (let (((W <struct-ntptimeval>) (make <struct-ntptimeval>
					   (pointer: P)
					   (malloc: mem.malloc-block/c))))
	    (check W.time.tv_sec	=> 1)
	    (check W.time.tv_usec	=> 2)
	    (check W.maxerror		=> 3)
	    (check W.esterror		=> 4)
	    #f)

	  (let (((M <ntptimeval>) (make <ntptimeval>
				    (pointer: P))))
	    (check M.time.tv_sec	=> 1)
	    (check M.time.tv_usec	=> 2)
	    (check M.maxerror		=> 3)
	    (check M.esterror		=> 4)
	    #f)

	  #f))

      #t)))




;;;; done

(check-report)

;;; end of file
