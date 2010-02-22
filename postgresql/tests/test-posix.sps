;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/PostgreSQL
;;;Contents: test for the polling connection functions
;;;Date: Mon Feb 15, 2010
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


(import (nausicaa)
  (compensations)
  (foreign memory)
  (records)
  (prefix (foreign databases postgresql) pg:)
  (prefix (posix sizeof) px:)
  (prefix (posix fd) px:)
  (for (prefix (posix typedefs) px:) expand run)
  (for (prefix (posix extensions) px:) expand)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing polling PostgreSQL functions\n")


(parametrise ((check-test-name	'opening))

  (define (%connect-asynchronously conn)
    (let ((fd		(pg:connection-socket conn))
	  (rd-fdset	(px:make-fdset malloc-block/c))
	  (wr-fdset	(px:make-fdset malloc-block/c))
	  (ex-fdset	(px:make-fdset malloc-block/c))
	  (timeout	(px:make-struct-timeval malloc-block/c)))

      (define (%poll set)
	(let loop ()
	  (px:FD_ZERO rd-fdset)
	  (px:FD_ZERO wr-fdset)
	  (px:FD_ZERO ex-fdset)
	  (px:FD_SET fd set)
	  (px:select px:FD_SETSIZE rd-fdset wr-fdset ex-fdset timeout)
	  (unless (px:FD_ISSET fd set)
	    (loop))))

      (with-fields* (((sec usec) px:struct-timeval* timeout))
	(set! timeout.sec 1)
	(set! timeout.usec 0))

      (let loop ((status (pg:polling-status writing)))
	(cond ((enum-set=? status (pg:polling-status ok))
	       #t)
	      ((enum-set=? status (pg:polling-status reading))
	       (%poll rd-fdset)
	       (loop (pg:connect-poll conn)))
	      ((enum-set=? status (pg:polling-status writing))
	       (%poll wr-fdset)
	       (loop (pg:connect-poll conn)))
	      (else
	       #f)))))

  (define (%reset-asynchronously conn)
    (let ((fd		(pg:connection-socket conn))
	  (rd-fdset	(px:make-fdset malloc-block/c))
	  (wr-fdset	(px:make-fdset malloc-block/c))
	  (ex-fdset	(px:make-fdset malloc-block/c))
	  (timeout	(px:make-struct-timeval malloc-block/c)))

      (define (%poll set)
	(let loop ()
	  (px:FD_ZERO rd-fdset)
	  (px:FD_ZERO wr-fdset)
	  (px:FD_ZERO ex-fdset)
	  (px:FD_SET fd set)
	  (px:select px:FD_SETSIZE rd-fdset wr-fdset ex-fdset timeout)
	  (unless (px:FD_ISSET fd set)
	    (loop))))

      (with-fields* (((sec usec) px:struct-timeval* timeout))
	(set! timeout.sec 1)
	(set! timeout.usec 0))

      (let loop ((status (pg:polling-status writing)))
	(cond ((enum-set=? status (pg:polling-status ok))
	       #t)
	      ((enum-set=? status (pg:polling-status reading))
	       (%poll rd-fdset)
	       (loop (pg:reset-poll conn)))
	      ((enum-set=? status (pg:polling-status writing))
	       (%poll wr-fdset)
	       (loop (pg:reset-poll conn)))
	      (else
	       #f)))))

;;; --------------------------------------------------------------------

  (check
      (with-compensations
	(letrec ((conn (compensate
			   (pg:connect-start "dbname=nausicaa-test")
			 (with
			  (pg:connect-finish conn)))))
	  (if (pg:status/bad? conn)
	      #f
	    (%connect-asynchronously conn))))
    => #t)

  (check
      (with-compensations
	(letrec ((conn (compensate
			   (pg:connect-start "dbname=nausicaa-test")
			 (with
			  (pg:connect-finish conn)))))
	  (if (pg:status/bad? conn)
	      #f
	    (begin
	      (%connect-asynchronously conn)
	      (if (not (pg:status/ok? conn))
		  #f
		(if (pg:reset-start conn)
		    (begin
		      (%reset-asynchronously conn)
		      (pg:status/ok? conn))
		  #f))))))
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file
