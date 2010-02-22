;;;!mosh
;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/PostgreSQL
;;;Contents: foreign library inspection generator
;;;Date:
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
  (foreign ffi inspector-maker))


;;;; type definitions

(define-c-type Oid			unsigned-int)
(define-c-type-alias Oid*		pointer)

(sizeof-lib
 (define-syntax sizeof-Oid-array
   (syntax-rules ()
     ((_ ?number-of-elements)
      (* strideof-Oid ?number-of-elements))))
 (define-syntax array-set-c-Oid!
   (syntax-rules ()
     ((_ ?pointer ?index ?value)
      (pointer-set-c-Oid! ?pointer (* strideof-Oid ?index) ?value))))
 (define-syntax array-ref-c-Oid
   (syntax-rules ()
     ((_ ?pointer ?index)
      (pointer-ref-c-Oid ?pointer (* strideof-Oid ?index)))))
 )

(sizeof-lib-exports
 sizeof-Oid-array
 array-set-c-Oid!
 array-ref-c-Oid
 )

(define-c-type-alias pgthreadlock_t	callback)

(define-c-type-alias PGconn*		pointer)
(define-c-struct PGconn
  "struct pg_conn")

(define-c-type-alias PGresult*		pointer)
(define-c-struct PGresult
  "struct pg_result")

(define-c-type-alias PGcancel*		pointer)
(define-c-struct PGcancel
  "struct pg_cancel")

(define-c-type-alias PGnotify*		pointer)
(define-c-struct PGnotify
  "struct pgNotify"
  (pointer		relname)
  (signed-int		be_pid)
  (pointer		extra)
  (pointer		next))

(define-c-type-alias PQnoticeReceiver	callback)
(define-c-type-alias PQnoticeProcessor	callback)

(define-c-type-alias pqbool		signed-int)

(define-c-type-alias PQprintOpt*	pointer)
(define-c-struct PQprintOpt
  "struct _PQprintOpt"
  (signed-int		header)
  (signed-int		align)
  (signed-int		standard)
  (signed-int		html3)
  (signed-int		expanded)
  (signed-int		pager)
  (pointer		fieldSep)
  (pointer		tableOpt)
  (pointer		caption)
  (pointer		fieldName))

(define-c-type-alias PQconninfoOption*	pointer)
(define-c-struct PQconninfoOption
  "struct _PQconninfoOption"
  (pointer		keyword)
  (pointer		envvar)
  (pointer		compiled)
  (pointer		val)
  (pointer		label)
  (pointer		dispchar)
  (signed-int		dispsize))

(define-c-type-alias PQArgBlock*	pointer)
(define-c-struct PQArgBlock
  "PQArgBlock"
  (signed-int		len)
  (signed-int		isint)
  (pointer		ptr)
  (signed-int		integer))

(define-c-type-alias PGresAttDesc*	pointer)
(define-c-struct PGresAttDesc
  "struct pgresAttDesc"
  (pointer		name)
  (unsigned-int		tableid)
  (signed-int		columnid)
  (signed-int		format)
  (unsigned-int		typid)
  (signed-int		typlen)
  (signed-int		atttypmod))


;;;; constants

(define-c-defines "Miscellaneous"
  OID_MAX)

(define-c-defines "Identifiers of error message fields"
  PG_DIAG_SEVERITY
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

(define-c-defines "Option flags for PQcopyResult"
  PG_COPYRES_ATTRS
  PG_COPYRES_TUPLES
  PG_COPYRES_EVENTS
  PG_COPYRES_NOTICEHOOKS)

(define-c-enumeration ConnStatusType
  "ConnStatusType"
  CONNECTION_OK
  CONNECTION_BAD
  CONNECTION_STARTED
  CONNECTION_MADE
  CONNECTION_AWAITING_RESPONSE
  CONNECTION_AUTH_OK
  CONNECTION_SETENV
  CONNECTION_SSL_STARTUP
  CONNECTION_NEEDED)

(define-c-enumeration PostgresPollingStatusType
  "PostgresPollingStatusType"
  PGRES_POLLING_FAILED
  PGRES_POLLING_READING
  PGRES_POLLING_WRITING
  PGRES_POLLING_OK
  PGRES_POLLING_ACTIVE)

(define-c-enumeration ExecStatusType
  "ExecStatusType"
  PGRES_EMPTY_QUERY
  PGRES_COMMAND_OK
  PGRES_TUPLES_OK
  PGRES_COPY_OUT
  PGRES_COPY_IN
  PGRES_BAD_RESPONSE
  PGRES_NONFATAL_ERROR
  PGRES_FATAL_ERROR)

(define-c-enumeration PGTransactionStatusType
  "PGTransactionStatusType"
  PQTRANS_IDLE
  PQTRANS_ACTIVE
  PQTRANS_INTRANS
  PQTRANS_INERROR
  PQTRANS_UNKNOWN)

(define-c-enumeration PGVerbosity
  "PGVerbosity"
  PQERRORS_TERSE
  PQERRORS_DEFAULT
  PQERRORS_VERBOSE)


;;;; done

(define postgresql-library-spec
  '(foreign databases postgresql sizeof))

(define-shared-object postgresql libpq.so)

(autoconf-lib-write "configuration/postgresql-inspector.m4" postgresql-library-spec)
(sizeof-lib-write   "src/libraries/foreign/databases/postgresql/sizeof.sls.in" postgresql-library-spec)

;;; end of file
