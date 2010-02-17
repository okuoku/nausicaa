;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/PostgreSQL
;;;Contents: primitive functions
;;;Date: Fri Feb 12, 2010
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


(library (foreign databases postgresql primitives)
  (export
    connect-start			;; PQconnectStart
    connect-poll			;; PQconnectPoll
    connect-db				;; PQconnectdb
    set-db-login			;; PQsetdbLogin
    set-db				;; PQsetdb
    finish				;; PQfinish
    reset-start				;; PQresetStart
    reset-poll				;; PQresetPoll
    reset				;; PQreset

    status				;; PQstatus
    status/ok?
    status/bad?
    status/started?
    status/made?
    status/awaiting-response?
    status/auth-ok?
    status/setenv?
    status/ssl-startup?
    status/needed?

    connection-defaults			;; PQconndefaults
    connection-info-parse		;; PQconninfoParse

    connection-database			;; PQdb
    connection-user			;; PQuser
    connection-password			;; PQpass
    connection-host			;; PQhost
    connection-port			;; PQport
    connection-tty			;; PQtty
    connection-options			;; PQoptions
    connection-socket			;; PQsocket
    connection-transaction-status	;; PQtransactionStatus
    connection-parameter-status		;; PQparameterStatus
    connection-protocol-version		;; PQprotocolVersion
    connection-server-version		;; PQserverVersion
    connection-error-message		;; PQerrorMessage
    connection-backend-pid		;; PQbackendPID
    connection-needs-password		;; PQconnectionNeedsPassword
    connection-used-password		;; PQconnectionUsedPassword
    connection-get-ssl			;; PQgetssl

    set-non-blocking			;; PQsetnonblocking

    (rename (PQconninfoFree			conninfo-free)
	    (PQgetCancel			get-cancel)
	    (PQfreeCancel			free-cancel)
	    (PQcancel				cancel)
	    (PQrequestCancel			request-cancel)
	    (PQclientEncoding			client-encoding)
	    (PQsetClientEncoding		set-client-encoding)
	    (PQinitSSL				init-ssl)
	    (PQinitOpenSSL			init-open-ssl)
	    (PQsetErrorVerbosity		set-error-verbosity)
	    (PQtrace				trace)
	    (PQuntrace				untrace)
	    (PQsetNoticeReceiver		set-notice-receiver)
	    (PQsetNoticeProcessor		set-notice-processor)
	    (PQregisterThreadLock		register-thread-lock)
	    (PQexec				exec)
	    (PQexecParams			exec-params)
	    (PQprepare				prepare)
	    (PQexecPrepared			exec-prepared)
	    (PQsendQuery			send-query)
	    (PQsendQueryParams			send-query-params)
	    (PQsendPrepare			send-prepare)
	    (PQsendQueryPrepared		send-query-prepared)
	    (PQgetResult			get-result)
	    (PQisBusy				is-busy)
	    (PQconsumeInput			consume-input)
	    (PQnotifies				notifies)
	    (PQputCopyData			put-copy-data)
	    (PQputCopyEnd			put-copy-end)
	    (PQgetCopyData			get-copy-data)
	    (PQgetline				getline)
	    (PQputline				putline)
	    (PQgetlineAsync			getline-async)
	    (PQputnbytes			putnbytes)
	    (PQendcopy				endcopy)
	    (PQisnonblocking			isnonblocking)
	    (PQisthreadsafe			isthreadsafe)
	    (PQflush				flush)
	    (PQfn				fn)
	    (PQresultStatus			result-status)
	    (PQresStatus			res-status)
	    (PQresultErrorMessage		result-error-message)
	    (PQresultErrorField			result-error-field)
	    (PQntuples				ntuples)
	    (PQnfields				nfields)
	    (PQbinaryTuples			binary-tuples)
	    (PQfname				fname)
	    (PQfnumber				fnumber)
	    (PQftable				ftable)
	    (PQftablecol			ftablecol)
	    (PQfformat				fformat)
	    (PQftype				ftype)
	    (PQfsize				fsize)
	    (PQfmod				fmod)
	    (PQcmdStatus			cmd-status)
	    (PQoidStatus			oid-status)
	    (PQoidValue				oid-value)
	    (PQcmdTuples			cmd-tuples)
	    (PQgetvalue				getvalue)
	    (PQgetlength			getlength)
	    (PQgetisnull			getisnull)
	    (PQnparams				nparams)
	    (PQparamtype			paramtype)
	    (PQdescribePrepared			describe-prepared)
	    (PQdescribePortal			describe-portal)
	    (PQsendDescribePrepared		send-describe-prepared)
	    (PQsendDescribePortal		send-describe-portal)
	    (PQclear				clear)
	    (PQfreemem				freemem)
	    (PQmakeEmptyPGresult		make-empty-pg-result)
	    (PQcopyResult			copy-result)
	    (PQsetResultAttrs			set-result-attrs)
	    (PQresultAlloc			result-alloc)
	    (PQsetvalue				setvalue)
	    (PQescapeStringConn			escape-string-conn)
	    (PQescapeByteaConn			escape-bytea-conn)
	    (PQunescapeBytea			unescape-bytea)
	    (PQescapeString			escape-string)
	    (PQescapeBytea			escape-bytea)
	    (PQprint				print)
	    (PQdisplayTuples			display-tuples)
	    (PQprintTuples			print-tuples)
	    (lo_open				lo-open)
	    (lo_close				lo-close)
	    (lo_read				lo-read)
	    (lo_write				lo-write)
	    (lo_lseek				lo-lseek)
	    (lo_creat				lo-creat)
	    (lo_create				lo-create)
	    (lo_tell				lo-tell)
	    (lo_truncate			lo-truncate)
	    (lo_unlink				lo-unlink)
	    (lo_import				lo-import)
	    (lo_import_with_oid			lo-import-with-oid)
	    (lo_export				lo-export)
	    (PQmblen				mblen)
	    (PQdsplen				dsplen)
	    (PQenv2encoding			env2encoding)
	    (PQencryptPassword			encrypt-password)
	    (pg_char_to_encoding		char-to-encoding)
	    (pg_encoding_to_char		encoding-to-char)
	    (pg_valid_server_encoding_id	valid-server-encoding-id)

	    (PQfreeNotify			free-notify)
	    (InvalidOid				invalid-oid)
	    (PQnoPasswordSupplied		no-password-supplied)))
  (import (rnrs)
    (begin0)
    (compensations)
    (foreign ffi)
    (foreign memory)
    (foreign cstrings)
    (foreign databases postgresql typedefs)
    (foreign databases postgresql enumerations)
    (foreign databases postgresql conditions)
    (foreign databases postgresql platform)
    (foreign databases postgresql sizeof))


;;;; connections management

(define (%do-connect who func info)
  (with-compensations
    (let ((p (func (string->cstring/c info))))
      (if (pointer-null? p)
	  (raise-out-of-memory who #f)
	(pointer-><connection> p)))))

(define connect-db
  (case-lambda
   (()
    (connect-db ""))
   ((info)
    (%do-connect 'connect-db PQconnectdb info))))

(define connect-start
  (case-lambda
   (()
    (connect-start ""))
   ((info)
    (%do-connect 'connect-start PQconnectStart info))))

(define (connect-poll conn)
  (let ((v (PQconnectPoll (<connection>->pointer conn))))
    (if (= v PGRES_POLLING_FAILED)
	(raise
	 (condition (make-postgresql-error-condition)
		    (make-connection-condition conn)
		    (make-who-condition 'connect-poll)
		    (make-message-condition (connection-error-message))))
      (value->polling-status v))))

(define (connect-poll* conn)
  (value->polling-status (PQconnectPoll (<connection>->pointer conn))))

(define (set-db-login pghost pgport pgoptions pgtty dbname login pwd)
  ;;Deprecated.
  ;;
  (define (%arg val)
    (if val
	(string->cstring/c val)
      pointer-null))
  (with-compensations
    (let ((p (PQsetdbLogin (%arg pghost) (%arg pgport) (%arg pgoptions) (%arg pgtty)
			   (%arg dbname) (%arg login) (%arg pwd))))
      (if (pointer-null? p)
	  (raise-out-of-memory 'set-db-login #f)
	(pointer-><connection> p)))))

(define (set-db pghost pgport pgoptions pgtty dbname)
  ;;Deprecated.
  ;;
  (set-db-login pghost pgport pgoptions pgtty dbname #f #f))

(define (finish conn)
  (PQfinish (<connection>->pointer conn)))

(define (reset conn)
  (PQreset (<connection>->pointer conn)))

(define (reset-start conn)
  (if (PQconnectStart (<connection>->pointer conn)) #t #f))

(define (reset-poll conn)
  (let ((v (PQresetPoll (<connection>->pointer conn))))
    (if (= v PGRES_POLLING_FAILED)
	(raise
	 (condition (make-postgresql-error-condition)
		    (make-connection-condition conn)
		    (make-who-condition 'connect-poll)
		    (make-message-condition (connection-error-message))))
      (value->polling-status v))))

(define (reset-poll* conn)
  (value->polling-status (PQresetPoll (<connection>->pointer conn))))

(define (%connect-options-array->list arry*)
  (let loop ((arry* arry*) (ell '()))
    (if (pointer-null? (struct-PQconninfoOption-keyword-ref arry*))
	ell
      (loop (pointer-add arry* strideof-PQconninfoOption)
	    (cons (pointer-><connect-option> arry*) ell)))))

(define (connection-defaults)
  (with-compensations
    (letrec ((arry* (compensate
			(PQconndefaults)
		      (with
		       (unless (pointer-null? arry*)
			 (PQconninfoFree arry*))))))
      (if (pointer-null? arry*)
	  (raise-out-of-memory 'connection-defaults #f)
	(%connect-options-array->list arry*)))))

(define (connection-info-parse info)
  (with-compensations
    (let ((errmsg* (malloc-small/c)))
      (letrec ((arry* (compensate
			  (PQconninfoParse (string->cstring/c info) errmsg*)
			(with
			 (unless (pointer-null? arry*)
			   (PQconninfoFree arry*))))))
	(if (pointer-null? arry*)
	    (let ((msg* (pointer-ref-c-pointer errmsg* 0)))
	      (if (pointer-null? msg*)
		  (raise-out-of-memory 'connection-info-parse #f)
		(begin
		  (push-compensation (primitive-free msg*))
		  (error 'connection-info-parse (cstring->string msg*) info))))
	  (%connect-options-array->list arry*))))))


;;;; inspection

(define (status conn)
  (value->connection-status (PQstatus (<connection>->pointer conn))))

(define (%status? status conn)
  (= status (PQstatus (<connection>->pointer conn))))

(define (status/ok? conn)
  (%status? CONNECTION_OK conn))

(define (status/bad? conn)
  (%status? CONNECTION_BAD conn))

(define (status/started? conn)
  (%status? CONNECTION_STARTED conn))

(define (status/made? conn)
  (%status? CONNECTION_MADE conn))

(define (status/awaiting-response? conn)
  (%status? CONNECTION_AWAITING_RESPONSE conn))

(define (status/auth-ok? conn)
  (%status? CONNECTION_AUTH_OK conn))

(define (status/setenv? conn)
  (%status? CONNECTION_SETENV conn))

(define (status/ssl-startup? conn)
  (%status? CONNECTION_SSL_STARTUP conn))

(define (status/needed? conn)
  (%status? CONNECTION_NEEDED conn))

;;; --------------------------------------------------------------------

;;;I have  verified by  inspecting the source  code of Libpq  in release
;;;8.4.2  that  the  pointers   returned  by  the  following  inspection
;;;functions, reference a string in the state of CONN; we do NOT have to
;;;release it (Marco Maggi.  Wed Feb 17, 2010).

(define (connection-database conn)
  (let ((p (PQdb (<connection>->pointer conn))))
    (if (pointer-null? p) #f  (cstring->string p))))

(define (connection-user conn)
  (let ((p (PQuser (<connection>->pointer conn))))
    (if (pointer-null? p) #f  (cstring->string p))))

(define (connection-password conn)
  (let ((p (PQpass (<connection>->pointer conn))))
    (if (pointer-null? p) #f  (cstring->string p))))

(define (connection-host conn)
  (let ((p (PQhost (<connection>->pointer conn))))
    (if (pointer-null? p) #f  (cstring->string p))))

(define (connection-port conn)
  (let ((p (PQport (<connection>->pointer conn))))
    (if (pointer-null? p) #f  (cstring->string p))))

(define (connection-tty conn)
  (let ((p (PQtty (<connection>->pointer conn))))
    (if (pointer-null? p) #f  (cstring->string p))))

(define (connection-options conn)
  (let ((p (PQoptions (<connection>->pointer conn))))
    (if (pointer-null? p) #f  (cstring->string p))))

(define (connection-socket conn)
  (integer-><fd> (PQsocket (<connection>->pointer conn))))

(define (connection-transaction-status conn)
  (value->transaction-status (PQtransactionStatus (<connection>->pointer conn))))

(define (connection-parameter-status conn parm-name)
  (with-compensations
    (let ((p (PQparameterStatus (<connection>->pointer conn) (string->cstring/c parm-name))))
      (if (pointer-null? p) #f (cstring->string p)))))

(define (connection-protocol-version conn)
  (PQprotocolVersion (<connection>->pointer conn)))

(define (connection-server-version conn)
  (PQserverVersion (<connection>->pointer conn)))

(define (connection-error-message conn)
  (let ((p (PQerrorMessage (<connection>->pointer conn))))
    (if (pointer-null? p) #f (cstring->string p))))

(define (connection-backend-pid conn)
  (integer->pid (PQbackendPID (<connection>->pointer conn))))

(define (connection-needs-password conn)
  (not (zero? (PQconnectionNeedsPassword (<connection>->pointer conn)))))

(define (connection-used-password conn)
  (not (zero? (PQconnectionUsedPassword (<connection>->pointer conn)))))

(define (connection-get-ssl conn)
  (let ((p (PQgetssl (<connection>->pointer conn))))
    (if (pointer-null? p) #f (pointer->ssl p))))




(define (set-non-blocking conn)
  (PQsetnonblocking (<connection>->pointer conn) 1))


;; typedef void (*PQnoticeReceiver) (void *arg, const PGresult *res);
;; typedef void (*PQnoticeProcessor) (void *arg, const char *message);
;;  typedef void  (*pgthreadlock_t)  (int acquire)))



;;;; done

)

;;; end of file
