;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Postgresql
;;;Contents: enumeration type definitions
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


(library (foreign databases postgresql enumerations)
  (export
    enum-polling-status			polling-status
    polling-status->value		value->polling-status

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
    )
  (import (rnrs)
    (enumerations)
    (foreign databases postgresql sizeof))


(define-c-flags polling-status
  (PGRES_POLLING_OK
   PGRES_POLLING_FAILED
   PGRES_POLLING_READING
   PGRES_POLLING_WRITING
   PGRES_POLLING_ACTIVE)
  (ok failed
   reading writing active))

(define-c-flags connection-status
  (CONNECTION_OK
   CONNECTION_BAD
   CONNECTION_STARTED
   CONNECTION_MADE
   CONNECTION_AWAITING_RESPONSE
   CONNECTION_AUTH_OK
   CONNECTION_SETENV
   CONNECTION_SSL_STARTUP
   CONNECTION_NEEDED)
  (ok
   bad
   started
   made
   awaiting-response
   auth-ok
   setenv
   ssl-startup
   needed))

(define-c-flags transaction-status
  (PQTRANS_IDLE
   PQTRANS_ACTIVE
   PQTRANS_INTRANS
   PQTRANS_INERROR
   PQTRANS_UNKNOWN)
  (idle
   active
   intrans
   inerror
   unknown))

(define-c-flags exec-status
  (PGRES_EMPTY_QUERY
   PGRES_COMMAND_OK
   PGRES_TUPLES_OK
   PGRES_COPY_OUT
   PGRES_COPY_IN
   PGRES_BAD_RESPONSE
   PGRES_NONFATAL_ERROR
   PGRES_FATAL_ERROR)
  (empty-query
   command-ok
   tuples-ok
   copy-out
   copy-in
   bad-response
   nonfatal-error
   fatal-error))

(define-c-flags error-field
  (PG_DIAG_SEVERITY
   PG_DIAG_SQLSTATE
   PG_DIAG_MESSAGE_PRIMARY
   PG_DIAG_MESSAGE_DETAIL
   PG_DIAG_MESSAGE_HINT
   PG_DIAG_STATEMENT_POSITION
   PG_DIAG_INTERNAL_POSITION
   PG_DIAG_INTERNAL_QUERY
   PG_DIAG_CONTEXT
   PG_DIAG_SOURCE_FILE
   PG_DIAG_SOURCE_LINE
   PG_DIAG_SOURCE_FUNCTION)
  (severity
   sqlstate
   message-primary
   message-detail
   message-hint
   statement-position
   internal-position
   internal-query
   context
   source-file
   source-line
   source-function))

(define-c-flags format-code
  (0 1)
  (text binary))


;;;; done

)

;;; end of file
