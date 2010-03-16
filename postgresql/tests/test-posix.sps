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
  (deferred-exceptions)
  (compensations)
  (foreign memory)
  (records)
  (prefix (foreign databases postgresql) pg:)
  (prefix (foreign databases postgresql compensated) pg:)
  (prefix (posix sizeof) px:)
  (prefix (posix fd) px:)
  (for (prefix (posix typedefs) px:) expand run)
  (for (prefix (posix extensions) px:) expand)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing polling PostgreSQL functions\n")


(parametrise ((check-test-name	'opening)
	      (debugging	#t))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred exception in opening" E))
    (lambda ()

      (define (%connect-asynchronously conn)
	(let ((fd	(pg:connection-socket conn))
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
	    (case (pg:polling-status->symbol status)
	      ((ok) #t)
	      ((reading)
	       (%poll rd-fdset)
	       (loop (pg:connect-poll conn)))
	      ((writing)
	       (%poll wr-fdset)
	       (loop (pg:connect-poll conn)))
	      (else #f)))))

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

      #t)))


(parametrise ((check-test-name	'parametrised-queries)
	      (debugging	#t))

  (define (%poll conn which)
    (with-compensations
      (let ((fd	(pg:connection-socket conn))
	    (rd-fdset	(px:make-fdset malloc-block/c))
	    (wr-fdset	(px:make-fdset malloc-block/c))
	    (ex-fdset	(px:make-fdset malloc-block/c))
	    (timeout	(px:make-struct-timeval malloc-block/c)))
	(with-fields* (((sec usec) px:struct-timeval* timeout))
	  (set! timeout.sec 10)
	  (set! timeout.usec 0))
	(px:FD_ZERO rd-fdset)
	(px:FD_ZERO wr-fdset)
	(px:FD_ZERO ex-fdset)
	(let ((set (if (eq? which 'read) rd-fdset wr-fdset)))
	  (px:FD_SET fd set)
	  (px:select px:FD_SETSIZE rd-fdset wr-fdset ex-fdset timeout)
	  (assert (px:FD_ISSET fd set))))))

  (define (%consume-events conn expected-status)
    (let next-result ((last-result #f))
      (when (pg:connection-is-busy? conn)
	(let consume-input ()
	  (%poll conn 'read)
	  (pg:connection-consume-input conn)
	  (when (pg:connection-is-busy? conn)
	    (consume-input))))
      (let ((result (pg:connection-get-result/c conn)))
	(if result
	    (if (enum-set=? expected-status (pg:result-status result))
		(next-result result)
	      (error #f "unexpected result status"))
	  last-result))))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred exception in parametrised-queries" E))
    (lambda ()

      (with-compensations
	(let ((conn (pg:connect-db/c "dbname=nausicaa-test")))

	  (check
	      (pg:status/ok? conn)
	    => #t)

	  (when (pg:status/ok? conn)

;;;	    (pg:exec-script/c conn "drop table accounts")

	    (push-compensation
	     (pg:exec-script/c conn "drop table accounts"))

	    (pg:connection-set-non-blocking conn)

;;; --------------------------------------------------------------------

	    (check
		(pg:exec-script/send conn "
create table accounts (nickname TEXT,
                       password TEXT);
insert into accounts (nickname, password)
   values ('ichigo', 'abcde');
insert into accounts (nickname, password)
   values ('rukia', '12345');
insert into accounts (nickname, password)
   values ('chad', 'fist');
")
	      => #t)

	    (let flush ()
	      (%poll conn 'write)
	      (when (pg:connection-flush conn)
		(flush)))

	    (%consume-events conn (pg:exec-status command-ok))

;;; --------------------------------------------------------------------

	    (check
		(pg:prepare-statement/send conn 'the-row
					   "select * from accounts where nickname = $1;" 1)
	      => #t)

	    (let flush ()
	      (%poll conn 'write)
	      (when (pg:connection-flush conn)
		(flush)))

	    (%consume-events conn (pg:exec-status command-ok))

;;; --------------------------------------------------------------------

	    (check
		(pg:describe-prepared-statement/send conn 'the-row)
	      => #t)

	    (let flush ()
	      (%poll conn 'write)
	      (when (pg:connection-flush conn)
		(flush)))

	    (%consume-events conn (pg:exec-status command-ok))

;;; --------------------------------------------------------------------

	    (check
		(pg:exec-prepared-statement/send conn 'the-row
						 (list (pg:parameter "rukia"))
						 #t)
	      => #t)

	    (let flush ()
	      (%poll conn 'write)
	      (when (pg:connection-flush conn)
		(flush)))

	    (let ((result (%consume-events conn (pg:exec-status tuples-ok))))

	      (check
		  (pg:result-number-of-tuples result)
		=> 1)

	      (check
		  (pg:result-number-of-fields result)
		=> 2)

	      (check
		  (pg:result-field-name result 0)
		=> "nickname")

	      (check
		  (pg:result-get-value/text result 0 0)
		=> "rukia")

	      (check
		  (pg:result-get-value/text result 0 1)
		=> "12345")

	      #f))

;;; --------------------------------------------------------------------

	  (check
	      (pg:exec-prepared-statement/send conn 'the-row
					       (list (pg:parameter "rukia"))
					       #t)
	    => #t)

	  (let ((cancel (pg:connection-get-cancel-handler/c conn)))

	    (check
		(pg:cancel-command cancel)
  	      => #t)

	    (let flush ()
	      (%poll conn 'write)
	      (when (pg:connection-flush conn)
		(flush)))

	    (let ((result (%consume-events conn (pg:exec-status tuples-ok))))

	      (check
		  (pg:result-number-of-tuples result)
		=> 1)

	      #f))

	  #t)))))


;;;; done

(check-report)

;;; end of file
