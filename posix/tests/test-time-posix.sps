;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: tests for the POSIX time and date functions
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



#!r6rs
(import (nausicaa)
  (nausicaa checks)
  (prefix (nausicaa ffi memory) mem.)
  (nausicaa posix sizeof)
  (prefix (nausicaa posix time) px.))

(check-set-mode! 'report-failed)
(display "*** testing POSIX time\n")


(parametrise ((check-test-name	'structs))

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


(parametrise ((check-test-name 'clock))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in clock" E))
    (lambda ()


      (check
	  (exact? (px.clock))
	=> #t)

      ;;This tests the type of values  in the fields of the <tms> mirror
      ;;class; the values are produced by functions in the stub library.
      (check
	  (receive (result (tms <tms>))
	      (px.times)
	    (list (exact? result)
		  (exact? tms.tms_utime)
		  (exact? tms.tms_stime)
		  (exact? tms.tms_cutime)
		  (exact? tms.tms_cstime)))
	=> '(#t #t #t #t #t))

      #t)))


(parametrise ((check-test-name 'simple-calendar))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in simple calendar" E))
    (lambda ()

      (check
	  (exact? (px.time))
	=> #t)

      #t)))


;;;; done

(check-report)

;;; end of file
