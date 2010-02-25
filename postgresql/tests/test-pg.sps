;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/PostgreSQL
;;;Contents: high-level API tests
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
  (deferred-exceptions)
  (prefix (foreign databases postgresql) pg:)
  (prefix (foreign databases postgresql compensated) pg:)
  (pretty-print)
  (debugging)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing PostgreSQL library\n")


(parametrise ((check-test-name	'connecting)
	      (debugging	#t))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred exception in connecting" E))
    (lambda ()

      (check
	  (with-compensations
	    (letrec ((conn (compensate
			       (pg:connect-db "dbname=nausicaa-test")
			     (with
			      (pg:connect-finish conn)))))
	      (if (not (enum-set=? (pg:connection-status ok) (pg:status conn)))
		  #f
		(begin
		  #t))))
	=> #t)

      (check	;compensated
	  (with-compensations
	    (let ((conn (pg:connect-db/c "dbname=nausicaa-test")))
	      (pg:status/ok? conn)))
	=> #t)

      (check	;reset
	  (with-compensations
	    (let ((conn (pg:connect-db/c "dbname=nausicaa-test")))
	      (if (not (pg:status/ok? conn))
		  #f
		(begin
		  (pg:reset conn)
		  (pg:status/ok? conn)))))
	=> #t)

;;; --------------------------------------------------------------------

      (check
	  (with-compensations
	    (letrec ((conn (compensate
			       (pg:set-db-login #f #f #f #f "nausicaa-test" #f #f)
			     (with
			      (pg:connect-finish conn)))))
	      (if (not (enum-set=? (pg:connection-status ok) (pg:status conn)))
		  #f
		(begin
		  #t))))
	=> #t)

      (check
	  (with-compensations
	    (letrec ((conn (compensate
			       (pg:set-db #f #f #f #f "nausicaa-test")
			     (with
			      (pg:connect-finish conn)))))
	      (if (not (enum-set=? (pg:connection-status ok) (pg:status conn)))
		  #f
		(begin
		  #t))))
	=> #t)

      #t)))


(parametrise ((check-test-name	'inspection)
	      (debugging	#t))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred exception in inspection" E))
    (lambda ()

      (check
	  (with-compensations
	    (let ((conn (pg:connect-db/c "dbname=nausicaa-test")))
	      (list (pg:connection-password conn)
		    (pg:connection-database conn)
		    )))
	=> '("" "nausicaa-test"))

      (check
	  (with-compensations
	    (let ((conn (pg:connect-db/c "dbname=nausicaa-test")))
	      (pg:connection-transaction-status conn)))
	(=> enum-set=?)
	(pg:transaction-status idle))

      (check
	  (with-compensations
	    (let ((conn (pg:connect-db/c "dbname=nausicaa-test")))
	      (pg:connection-parameter-status conn 'server_version)))
	=> "8.4.2")

      (check
	  (with-compensations
	    (let ((conn (pg:connect-db/c "dbname=nausicaa-test")))
	      (pg:connection-protocol-version conn)))
	=> 3)

      (check
	  (with-compensations
	    (let ((conn (pg:connect-db/c "dbname=nausicaa-test")))
	      (pg:connection-server-version conn)))
	=> 80402)

      (check
	  (with-compensations
	    (let ((conn (pg:connect-db/c "dbname=nausicaa-test")))
	      (pg:pid? (pg:connection-backend-pid conn))))
	=> #t)

      (check
	  (with-compensations
	    (let ((conn (pg:connect-db/c "dbname=nausicaa-test")))
	      (list (pg:connection-needs-password conn)
		    (pg:connection-used-password conn))))
	=> '(#f #f))

      (check
	  (with-compensations
	    (let ((conn (pg:connect-db/c "dbname=nausicaa-test")))
	      (pg:connection-get-ssl conn)))
	=> #f)

      #t)))


(parametrise ((check-test-name	'result-inspection)
	      (debugging	#t))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred exception in result-inspection" E))
    (lambda ()

      (with-compensations ;successful
	(let ((conn (pg:connect-db/c "dbname=nausicaa-test")))

	  (check
	      (pg:status/ok? conn)
	    => #t)

	  (when (pg:status/ok? conn)
	    (letrec ((result (compensate
				 (pg:exec-script conn "create table mine (enn int); drop table mine;")
			       (with
				(pg:clear-result result)))))

	      (check
		  (pg:result-status result)
		(=> enum-set=?)
		(pg:exec-status command-ok))

	      (check
		  (pg:status->string (pg:result-status result))
		=> "PGRES_COMMAND_OK")

	      (check
		  (pg:result-error-message result)
		=> #f)

	      (check
		  (pg:result-error-field result (pg:error-field severity))
		=> #f)

	      #f))))

      (with-compensations ;erroneous
	(let ((conn (pg:connect-db/c "dbname=nausicaa-test")))

	  (check
	      (pg:status/ok? conn)
	    => #t)

	  (when (pg:status/ok? conn)
	    (let ((result (pg:exec-script/c conn "create")))

	      (check
		  (pg:result-status result)
		(=> enum-set=?)
		(pg:exec-status fatal-error))

	      (check
		  (pg:status->string (pg:result-status result))
		=> "PGRES_FATAL_ERROR")

	      (check
		  (let ((s (pg:result-error-message result)))
;;;(display s)
		    (substring s 0 5))
		=> "ERROR")

	      (check
		  (pg:result-error-field result (pg:error-field severity))
		=> "ERROR")

	      #f))))

      #t)))


(parametrise ((check-test-name	'queries)
	      (debugging	#t))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred exception in queries" E))
    (lambda ()

      (with-compensations
	(let ((conn (pg:connect-db/c "dbname=nausicaa-test")))

	  (check
	      (pg:status/ok? conn)
	    => #t)

	  (when (pg:status/ok? conn)

;;;(pg:exec-script/c conn "drop table accounts")

	    (let ((result (pg:exec-script/c conn "
create table accounts (nickname TEXT, password TEXT);
insert into accounts (nickname, password) values ('ichigo', 'abcde');
insert into accounts (nickname, password) values ('rukia', '12345');
insert into accounts (nickname, password) values ('chad', 'fist');
select * from accounts;
")))
	      (push-compensation (pg:exec-script/c conn "drop table accounts"))

	      (check
		  (pg:result-status result)
		(=> enum-set=?)
		(pg:exec-status tuples-ok))

	      (check
		  (pg:status->string (pg:result-status result))
		=> "PGRES_TUPLES_OK")

	      (when (enum-set=? (pg:exec-status tuples-ok) (pg:result-status result))

		(check
		    (pg:result-number-of-tuples result)
		  => 3)

		(check
		    (pg:result-number-of-fields result)
		  => 2)

		(check
		    (pg:result-field-name result 0)
		  => "nickname")

		(check
		    (pg:result-field-name result 1)
		  => "password")

		(check
		    (pg:result-field-name? result 'password)
		  => #t)

		(check
		    (pg:result-field-name? result 'ciao)
		  => #f)

		(check
		    (guard (E (else
;;;			   (display (condition-message E))(newline)
			       (condition-who E)))
		      (pg:result-field-name result 2))
		  => 'result-field-name)

		(check
		    (pg:result-field-number result 'nickname)
		  => 0)

		(check
		    (pg:result-field-number result 'password)
		  => 1)

		(check
		    (guard (E (else
;;;			   (display (condition-message E))(newline)
			       (condition-who E)))
		      (pg:result-field-number result 'ciao))
		  => 'result-field-number)

		(check
		    (integer? (pg:result-column-table-oid result 0))
		  => #t)

		(check
		    (pg:result-table-column-number result 0)
		  => 1)

		(check
		    (pg:result-column-format-code result 0)
		  (=> enum-set=?)
		  (pg:format-code text))

		(check
		    (guard (E (else (condition-who E)))
		      (pg:result-column-format-code result 9))
		  => 'result-column-format-code)

		(check
		    (integer? (pg:result-column-type-oid result 1))
		  => #t)

		(check
		    (guard (E (else
;;;			   (display (condition-message E))(newline)
			       (condition-who E)))
		      (pg:result-column-type-oid result 9))
		  => 'result-column-type-oid)

		(check
		    (pg:result-type-modifier result 1)
		  => #f)

		(check
		    (pg:result-column-size result 1)
		  => #f)

		(check
		    (pg:result-null-value? result 0 1)
		  => #f)

		(check
		    (pg:result-get-value/text result 0 0)
		  => "ichigo")

		(check
		    (pg:result-get-value/text result 1 0)
		  => "rukia")

		(check
		    (pg:result-get-value/text result 2 1)
		  => "fist")

		(check
		    (pg:result-get-value/binary result 2 1)
		  => (string->utf8 "fist"))

		(check
		    (pg:result-command-status result)
		  => "SELECT")

		(check
		    (pg:result-affected-rows result)
		  => #f)

		(check
		    (pg:result-new-row-oid result)
		  => #f)

		#f)))))
      #t)))


(parametrise ((check-test-name	'parametrised-queries)
	      (debugging	#t))

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

;;;(pg:exec-script/c conn "drop table accounts")

	    (compensate
		(pg:exec-script/c conn "
create table accounts (nickname TEXT, password TEXT);
insert into accounts (nickname, password) values ('ichigo', 'abcde');
insert into accounts (nickname, password) values ('rukia', '12345');
insert into accounts (nickname, password) values ('chad', 'fist');
")
	      (with
	       (pg:exec-script/c conn "drop table accounts")))

	    (let ((result (pg:exec-parametrised-query/c
			   conn
			   "select * from accounts where nickname = $1;"
			   (list (pg:parameter "rukia"))
			   #t)))

	      (check
		  (pg:result-status result)
		(=> enum-set=?)
		(pg:exec-status tuples-ok))

	      (check
		  (pg:status->string (pg:result-status result))
		=> "PGRES_TUPLES_OK")

	      (when (enum-set=? (pg:exec-status tuples-ok) (pg:result-status result))

		(check
		    (pg:result-number-of-tuples result)
		  => 1)

		(check
		    (pg:result-number-of-fields result)
		  => 2)

		(check
		    (pg:result-field-name result 0)
		  => "nickname")


		#f)))))

      #t)))


(parametrise ((check-test-name	'prepared-statements)
	      (debugging	#t))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred exception in prepared-statement" E))
    (lambda ()

      (with-compensations
	(let ((conn (pg:connect-db/c "dbname=nausicaa-test")))

	  (check
	      (pg:status/ok? conn)
	    => #t)

	  (when (pg:status/ok? conn)

;;;(pg:exec-script/c conn "drop table accounts")

	    (compensate
		(pg:exec-script/c conn "
create table accounts (nickname TEXT, password TEXT);
insert into accounts (nickname, password) values ('ichigo', 'abcde');
insert into accounts (nickname, password) values ('rukia', '12345');
insert into accounts (nickname, password) values ('chad', 'fist');
")
	      (with
	       (pg:exec-script/c conn "drop table accounts")))

	    (let ((result (pg:prepare-statement/c conn 'the-row
						  "select * from accounts where nickname = $1;" 1)))

	      (check
		  (pg:result-status result)
		(=> enum-set=?)
		(pg:exec-status command-ok))

	      (check
		  (pg:status->string (pg:result-status result))
		=> "PGRES_COMMAND_OK")

	      #f)

	    (let ((result (pg:describe-prepared-statement/c conn 'the-row)))

	      (check
		  (pg:result-status result)
		(=> enum-set=?)
		(pg:exec-status command-ok))

	      (check
		  (pg:status->string (pg:result-status result))
		=> "PGRES_COMMAND_OK")

	      #f)

	    (let ((result (pg:exec-prepared-statement/c conn 'the-row
							(list (pg:parameter "rukia"))
							#t)))

	      (check
		  (pg:result-status result)
		(=> enum-set=?)
		(pg:exec-status tuples-ok))

	      (check
		  (pg:status->string (pg:result-status result))
		=> "PGRES_TUPLES_OK")

	      (when (enum-set=? (pg:exec-status tuples-ok) (pg:result-status result))

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

	    #f)))

      #t)))


(parametrise ((check-test-name	'escapes)
	      (debugging	#t))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred exception in escapes" E))
    (lambda ()

      (with-compensations
	(let ((conn (pg:connect-db/c "dbname=nausicaa-test")))

	  (check
	      (pg:escape-string-conn conn "select")
	    => "select")

	  (check
	      (pg:escape-string-conn conn "'")
	    => "''")

	  (check
	      (pg:escape-bytes-conn/bv conn '#vu8(0))
	    => "\\\\000")

	  (check
	      (pg:unescape-bytes/bv "ciao")
	    => '#vu8(99 105  97 111))

	  #f))

      #t)))


(parametrise ((check-test-name	'misc)
	      (debugging	#t))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred exception in misc" E))
    (lambda ()

      (check
	  (let ((ops (pg:connection-defaults)))
	    ;;(pretty-print ops)
	    (for-all pg:<connect-option>? ops))
	=> #t)

      (check
	  (let ((ops (pg:connection-info-parse "dbname=nausicaa-test")))
	    ;;(pretty-print ops)
	    (for-all pg:<connect-option>? ops))
	=> #t)

      (check
	  (guard (E (else
		     ;;(debug-print-condition "connect-parse" E)
		     #t))
	    (pg:connection-info-parse "ciao"))
	=> #t)

      #t)))


;;;; done

(check-report)

;;; end of file
