;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/PostgreSQL
;;;Contents: compound library, high-level API
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


(library (foreign databases postgresql)
  (export

    ;; OID_MAX
    ;; PG_DIAG_SEVERITY
    ;; PG_DIAG_SQLSTATE
    ;; PG_DIAG_MESSAGE_PRIMARY
    ;; PG_DIAG_MESSAGE_DETAIL
    ;; PG_DIAG_MESSAGE_HINT
    ;; PG_DIAG_STATEMENT_POSITION
    ;; PG_DIAG_INTERNAL_POSITION
    ;; PG_DIAG_INTERNAL_QUERY
    ;; PG_DIAG_CONTEXT
    ;; PG_DIAG_SOURCE_FILE
    ;; PG_DIAG_SOURCE_LINE
    ;; PG_DIAG_SOURCE_FUNCTION
    ;; PG_COPYRES_ATTRS
    ;; PG_COPYRES_TUPLES
    ;; PG_COPYRES_EVENTS
    ;; PG_COPYRES_NOTICEHOOKS

    ;; CONNECTION_OK
    ;; CONNECTION_BAD
    ;; CONNECTION_STARTED
    ;; CONNECTION_MADE
    ;; CONNECTION_AWAITING_RESPONSE
    ;; CONNECTION_AUTH_OK
    ;; CONNECTION_SETENV
    ;; CONNECTION_SSL_STARTUP
    ;; CONNECTION_NEEDED

    ;; PGRES_POLLING_FAILED
    ;; PGRES_POLLING_READING
    ;; PGRES_POLLING_WRITING
    ;; PGRES_POLLING_OK
    ;; PGRES_POLLING_ACTIVE

    ;; PGRES_EMPTY_QUERY
    ;; PGRES_COMMAND_OK
    ;; PGRES_TUPLES_OK
    ;; PGRES_COPY_OUT
    ;; PGRES_COPY_IN
    ;; PGRES_BAD_RESPONSE
    ;; PGRES_NONFATAL_ERROR
    ;; PGRES_FATAL_ERROR

    ;; PQTRANS_IDLE
    ;; PQTRANS_ACTIVE
    ;; PQTRANS_INTRANS
    ;; PQTRANS_INERROR
    ;; PQTRANS_UNKNOWN

    ;; PQERRORS_TERSE
    ;; PQERRORS_DEFAULT
    ;; PQERRORS_VERBOSE

;;; --------------------------------------------------------------------

    connection			connection?
    pointer->connection		connection->pointer

    query-result		query-result?
    pointer->query-result	query-result->pointer

    fd				fd?
    integer->fd			fd->integer

    pid				pid?
    integer->pid		pid->integer

    <connect-option>		<connect-option-rtd>
    make-<connect-option>	<connect-option>?
    <connect-option>-keyword
    <connect-option>-envvar
    <connect-option>-compiled
    <connect-option>-val
    <connect-option>-label
    <connect-option>-dispchar
    <connect-option>-dispsize

    pointer-><connect-option>

    <parameter>				<parameter-rtd>
    make-<parameter>			<parameter>?
    <parameter>-value
    <parameter>-size
    <parameter>-text?
    <parameter>-oid

    parameter

    <notification>			<notification-rtd>
    make-<notification>			<notification>?
    <notification>-relname
    <notification>-pid
    <notification>-extra

;;; --------------------------------------------------------------------

    enum-polling-status			polling-status
    polling-status->value		value->polling-status
    polling-status->symbol

    enum-connection-status		connection-status
    connection-status->value		value->connection-status

    enum-transaction-status		transaction-status
    transaction-status->value		value->transaction-status

    enum-exec-status			exec-status
    exec-status->value			value->exec-status

    enum-error-field			error-field
    error-field->value			value->error-field

    enum-format-code			format-code
    format-code->value			value->format-code

    enum-error-verbosity		error-verbosity
    error-verbosity->value		value->error-verbosity

;;; --------------------------------------------------------------------

    &connection
    make-connection-condition
    connection-condition?
    connection-condition

    &connect-info
    make-connect-info-condition
    connect-info-condition?
    connect-info-condition

    &cancel-handler
    make-cancel-handler-condition
    cancel-handler-condition?
    cancel-handler-condition

    &query-string
    make-query-string-condition
    query-string-condition?
    query-string-condition

    &query-result
    make-query-result-condition
    query-result-condition?
    query-result-condition

    &parameters
    make-parameters-condition
    parameters-condition?
    parameters-condition

    &statement-name
    make-statement-name-condition
    statement-name-condition?
    statement-name-condition

    &portal-name
    make-portal-name-condition
    portal-name-condition?
    portal-name-condition

    &escape-string
    make-escape-string-condition
    escape-string-condition?
    escape-string-condition

;;; --------------------------------------------------------------------

    &postgresql-error
    make-postgresql-error-condition
    postgresql-error-condition?

    &postgresql-info-error
    make-postgresql-info-error-condition
    postgresql-info-error-condition?

    &postgresql-exec-error
    make-postgresql-exec-error-condition
    postgresql-exec-error-condition?

    &postgresql-prepare-error
    make-postgresql-prepare-error-condition
    postgresql-prepare-error-condition?

    &postgresql-poll-error
    make-postgresql-poll-error-condition
    postgresql-poll-error-condition?

    &postgresql-cancel-error
    make-postgresql-cancel-error-condition
    postgresql-cancel-error-condition?

    &postgresql-config-error
    make-postgresql-config-error-condition
    postgresql-config-error-condition?

    &postgresql-copy-error
    make-postgresql-copy-error-condition
    postgresql-copy-error-condition?

    &postgresql-copy-in-error
    make-postgresql-copy-in-error-condition
    postgresql-copy-in-error-condition?

    &postgresql-copy-out-error
    make-postgresql-copy-out-error-condition
    postgresql-copy-out-error-condition?

    &postgresql-copy-end-error
    make-postgresql-copy-end-error-condition
    postgresql-copy-end-error-condition?

    &postgresql-escape-encode-error
    make-postgresql-escape-encode-error-condition
    postgresql-escape-encode-error-condition?

    &postgresql-escape-decode-error
    make-postgresql-escape-decode-error-condition
    postgresql-escape-decode-error-condition?

;;; --------------------------------------------------------------------

    ;; connection handling
    connect-start			;; PQconnectStart
    connect-poll			;; PQconnectPoll
    connect-db				;; PQconnectdb
    set-db-login			;; PQsetdbLogin
    set-db				;; PQsetdb
    connect-finish			;; PQfinish

    reset-start				;; PQresetStart
    reset-poll				;; PQresetPoll
    reset				;; PQreset

    ;; connection inspection
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
    connection-needs-password?		;; PQconnectionNeedsPassword
    connection-used-password?		;; PQconnectionUsedPassword
    connection-get-ssl			;; PQgetssl

    clear-result			;; PQclear
    result-clear			;; PQclear

    ;; executing queries
    exec-script				;; PQexec
    exec-parametrised-query		;; PQexecParams
    prepare-statement			;; PQprepare
    describe-prepared-statement		;; PQdescribePrepared
    exec-prepared-statement		;; PQexecPrepared

    ;; executing asynchronous queries
    exec-script/send			;; PQsendQuery
    exec-parametrised-query/send	;; PQsendQueryParams
    prepare-statement/send		;; PQsendPrepare
    describe-prepared-statement/send	;; PQsendDescribePrepared
    exec-prepared-statement/send	;; PQsendQueryPrepared

    connection-get-result		;; PQgetResult
    connection-consume-input		;; PQconsumeInput
    connection-is-busy?			;; PQisBusy

    connection-set-blocking		;; PQsetnonblocking
    connection-set-non-blocking		;; PQsetnonblocking
    connection-is-blocking?		;; PQisnonblocking
    connection-is-non-blocking?		;; PQisnonblocking
    connection-flush			;; PQflush

    connection-notification		;; PQnotifies

    ;; cancelling a command
    connection-get-cancel-handler	;; PQgetCancel
    free-cancel-handler			;; PQfreeCancel
    cancel-command			;; PQcancel

    ;; COPY stuff
    result-binary-tuples?		;; PQbinaryTuples
    result-textual-tuples?		;; PQbinaryTuples
    connection-put-copy-data		;; PQputCopyData
    connection-put-copy-data/string
    connection-put-copy-data/bytevector
    connection-put-copy-end		;; PQputCopyEnd
    connection-put-copy-fail
    connection-get-copy-data		;; PQgetCopyData
    connection-get-copy-data/string
    connection-get-copy-data/bytevector

    ;; inspecting query results
    result-status			;; PQresultStatus
    status->string			;; PQresStatus
    result-error-message		;; PQresultErrorMessage
    result-error-field			;; PQresultErrorField

    result-status/empty-query?
    result-status/command-ok?
    result-status/tuples-ok?
    result-status/copy-out?
    result-status/copy-in?
    result-status/bad-response?
    result-status/nonfatal-error?
    result-status/fatal-error?
    result-status/bad?

    ;; extracting informations from query results
    result-number-of-tuples		;; PQntuples
    result-tuple-index?
    assert-result-tuple-index
    result-number-of-fields		;; PQnfields
    result-field-index?
    assert-result-field-index
    result-field-name			;; PQfname
    result-field-name?
    assert-result-field-name
    result-field-number			;; PQfnumber
    result-column-table-oid		;; PQftable
    result-table-column-number		;; PQftablecol
    result-column-format-code		;; PQfformat
    result-column-type-oid		;; PQftype
    result-type-modifier		;; PQfmod
    result-column-size			;; PQfsize
    result-null-value?			;; PQgetisnull
    result-value-length			;; PQgetlength
    result-get-value			;; PQgetvalue
    result-get-value/memblock		;; PQgetvalue
    result-get-value/text		;; PQgetvalue
    result-get-value/binary		;; PQgetvalue
    result-number-of-parameters		;; PQnparams
    result-parameter-index?
    assert-result-parameter-index
    result-parameter-type-oid		;; PQparamtype
    result-command-status		;; PQcmdStatus
    result-affected-rows		;; PQcmdTuples
    result-new-row-oid			;; PQoidValue

    ;; escaping
    escape-string-conn			;; PQescapeStringConn
    escape-bytes-conn			;; PQescapeByteaConn
    escape-bytes-conn/bv
    unescape-bytes			;; PQunescapeBytea
    unescape-bytes/bv

    ;; miscellaneous
    describe-portal			;; PQdescribePortal
    describe-portal/send		;; PQsendDescribePortal
    connection-client-encoding		;; PQclientEncoding
    connection-client-encoding-set!	;; PQsetClientEncoding
    connection-error-verbosity-set!	;; PQsetErrorVerbosity
    connection-set-notice-receiver	;; PQsetNoticeReceiver
    connection-set-notice-processor	;; PQsetNoticeProcessor
    make-notice-receiver-callback
    make-notice-processor-callback

    )
  (import (rnrs)
    (foreign databases postgresql typedefs)
    (foreign databases postgresql enumerations)
    (foreign databases postgresql conditions)
    (foreign databases postgresql primitives)
    (foreign databases postgresql sizeof)))

;;; end of file
