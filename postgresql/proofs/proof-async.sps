;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/PostgreSQL
;;;Contents: test for asynchronous query execution
;;;Date: Wed Feb 24, 2010
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
  (formations)
  (records)
  (compensations)
  (deferred-exceptions)
  (foreign memory)
  (prefix (foreign databases postgresql) pg:)
  (prefix (foreign databases postgresql compensated) pg:)
  (prefix (posix sizeof) px:)
  (prefix (posix fd) px:)
  (for (prefix (posix typedefs) px:) expand run)
  (for (prefix (posix extensions) px:) expand))


(define (main)
  (with-deferred-exceptions-handler
      (lambda (E)
	(exception-handler "deferred exception" E))
    (lambda ()
      (with-exception-handler
	  (lambda (E)
	    (exception-handler "error" E))
	(lambda ()
	  (with-compensations

	    (define $sql-create-table
	      "create table accounts (nickname TEXT,
                                      password TEXT);")

	    (define $sql-insert-data "
               insert into accounts (nickname, password)
                 values ('ichigo', 'abcde');
               insert into accounts (nickname, password)
                 values ('rukia', '12345');
               insert into accounts (nickname, password)
                 values ('chad', 'fist');")

	    (define $sql-query
	      "select * from accounts where nickname = $1;")

	    (let ((conn (database-connect/c "dbname=nausicaa-test")))

;;;	      (pg:exec-script/c conn "drop table accounts")

	      (database-send/c
	       conn (pg:exec-status command-ok)
	       (lambda (conn)
		 (pg:exec-script/send conn $sql-create-table)))

	      (database-send/c
	       conn (pg:exec-status command-ok)
	       (lambda (conn)
		 (pg:exec-script/send conn $sql-insert-data)))

	      (database-send/c
	       conn (pg:exec-status command-ok)
	       (lambda (conn)
		 (pg:prepare-statement/send conn 'the-row $sql-query 1)))

	      (let ((result (database-send/c
			     conn (pg:exec-status tuples-ok)
			     (lambda (conn)
			       (pg:exec-prepared-statement/send
				conn 'the-row (list (pg:parameter "rukia")) #t)))))

		(format #t
		  "number of tuples: ~a~%password: ~a~%"
		  (pg:result-number-of-tuples result)
		  (pg:result-get-value/text result 0 1)))

	      (pg:exec-script/c conn "drop table accounts")
	      ))
	  (exit))))))

(define (database-connect/c connect-info)
  (let ((conn (pg:connect-start/c connect-info)))
    (if (pg:status/bad? conn)
	(error 'database-conenct
	  "unable to start connection")
      (let loop ((status (pg:polling-status writing)))
	(case (enum->symbol status)
	  ((ok)
	   (pg:connection-set-non-blocking conn)
	   conn)
	  ((reading)
	   (connection-wait-for-read-ready conn)
	   (loop (pg:connect-poll conn)))
	  ((writing)
	   (connection-wait-for-write-ready conn)
	   (loop (pg:connect-poll conn)))
	  (else
	   (error 'database-connect/c
	     "unable to complete connection")))))))

(define (database-send/c conn expected-status action)
  (action conn)
  (do-while (pg:connection-flush conn)
    (connection-wait-for-write-ready conn))
  (let next-result ((last-result #f))
    (while (pg:connection-is-busy? conn)
      (connection-wait-for-read-ready conn)
      (pg:connection-consume-input conn))
    (let ((result (pg:connection-get-result/c conn)))
      (if result
	  (if (enum-set=? expected-status (pg:result-status result))
	      (next-result result)
	    (error 'database-exec-command/c
	      "unexpected result status"))
	last-result))))

(define-enumeration socket-event
  (read-ready write-ready exception)
  socket-events)

(define-syntax connection-wait-for-read-ready
  (syntax-rules ()
    ((_ ?conn)
     (connection-wait-for-event ?conn (socket-event read-ready)))))

(define-syntax connection-wait-for-write-ready
  (syntax-rules ()
    ((_ ?conn)
     (connection-wait-for-event ?conn (socket-event write-ready)))))

(define (connection-wait-for-event conn which)
  (with-compensations
    (let ((fd       (pg:connection-socket conn))
  	  (rd-fdset (px:make-fdset malloc-block/c))
  	  (wr-fdset (px:make-fdset malloc-block/c))
  	  (ex-fdset (px:make-fdset malloc-block/c))
  	  (timeout  (px:make-struct-timeval malloc-block/c)))
      (with-fields* (((sec usec) px:struct-timeval* timeout))
  	(set! timeout.sec  10)
  	(set! timeout.usec 0))
      (px:FD_ZERO rd-fdset)
      (px:FD_ZERO wr-fdset)
      (px:FD_ZERO ex-fdset)
      (let ((set (case which
		   ((read-ready)	rd-fdset)
		   ((write-ready)	wr-fdset)
		   ((exception)		ex-fdset))))
  	(px:FD_SET fd set)
  	(px:select px:FD_SETSIZE rd-fdset wr-fdset ex-fdset timeout)
  	(unless (px:FD_ISSET fd set)
	  (error 'connection-wait-for-event
	    "timeout expired while polling socket"))))))

(define (enum->symbol set)
  (car (enum-set->list set)))

(define (exception-handler prefix E)
  (cond ((message-condition? E)
	 (format (current-error-port)
	   "~a: ~a: ~a\n" prefix (condition-who E) (condition-message E))
	 (exit 1))
	(else (raise E))))

(main)

;;; end of file
