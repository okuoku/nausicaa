;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/PostgreSQL
;;;Contents: bindings to foreign functions
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


(library (foreign databases postgresql platform)
  (export
    PQconnectStart
    PQconnectPoll
    PQconnectdb
    PQsetdbLogin
    PQfinish
    PQconndefaults
    PQconninfoParse
    PQconninfoFree
    PQresetStart
    PQresetPoll
    PQreset
    PQgetCancel
    PQfreeCancel
    PQcancel
    PQrequestCancel
    PQdb
    PQuser
    PQpass
    PQhost
    PQport
    PQtty
    PQoptions
    PQstatus
    PQtransactionStatus
    PQparameterStatus
    PQprotocolVersion
    PQserverVersion
    PQerrorMessage
    PQsocket
    PQbackendPID
    PQconnectionNeedsPassword
    PQconnectionUsedPassword
    PQclientEncoding
    PQsetClientEncoding
    PQgetssl
    PQinitSSL
    PQinitOpenSSL
    PQsetErrorVerbosity
    PQtrace
    PQuntrace
    PQsetNoticeReceiver
    PQsetNoticeProcessor
    PQregisterThreadLock
    PQexec
    PQexecParams
    PQprepare
    PQexecPrepared
    PQsendQuery
    PQsendQueryParams
    PQsendPrepare
    PQsendQueryPrepared
    PQgetResult
    PQisBusy
    PQconsumeInput
    PQnotifies
    PQputCopyData
    PQputCopyEnd
    PQgetCopyData
    PQgetline
    PQputline
    PQgetlineAsync
    PQputnbytes
    PQendcopy
    PQsetnonblocking
    PQisnonblocking
    PQisthreadsafe
    PQflush
    PQfn
    PQresultStatus
    PQresStatus
    PQresultErrorMessage
    PQresultErrorField
    PQntuples
    PQnfields
    PQbinaryTuples
    PQfname
    PQfnumber
    PQftable
    PQftablecol
    PQfformat
    PQftype
    PQfsize
    PQfmod
    PQcmdStatus
    PQoidStatus
    PQoidValue
    PQcmdTuples
    PQgetvalue
    PQgetlength
    PQgetisnull
    PQnparams
    PQparamtype
    PQdescribePrepared
    PQdescribePortal
    PQsendDescribePrepared
    PQsendDescribePortal
    PQclear
    PQfreemem
    PQmakeEmptyPGresult
    PQcopyResult
    PQsetResultAttrs
    PQresultAlloc
    PQsetvalue
    PQescapeStringConn
    PQescapeByteaConn
    PQunescapeBytea
    PQescapeString
    PQescapeBytea
    PQprint
    PQdisplayTuples
    PQprintTuples
    lo_open
    lo_close
    lo_read
    lo_write
    lo_lseek
    lo_creat
    lo_create
    lo_tell
    lo_truncate
    lo_unlink
    lo_import
    lo_import_with_oid
    lo_export
    PQmblen
    PQdsplen
    PQenv2encoding
    PQencryptPassword
    pg_char_to_encoding
    pg_encoding_to_char
    pg_valid_server_encoding_id

    PQfreeNotify
    PQsetdb
    InvalidOid
    PQnoPasswordSupplied)
  (import (rnrs)
    (foreign ffi)
    (foreign ffi sizeof)
    (foreign databases postgresql shared-object)
    (foreign databases postgresql sizeof))

(define InvalidOid 0)

(define (PQsetdb M_PGHOST M_PGPORT M_PGOPT M_PGTTY M_DBNAME)
  (PQsetdbLogin M_PGHOST M_PGPORT M_PGOPT M_PGTTY M_DBNAME pointer-null pointer-null))

(define PQnoPasswordSupplied
  "fe_sendauth: no password supplied\n")

(define int*		'pointer)
(define char**		'pointer)
(define size_t*		'pointer)


(define-c-functions postgresql-shared-object
  (PQconnectStart		(PGconn* PQconnectStart (char*)))
  (PQconnectPoll		(PostgresPollingStatusType PQconnectPoll (PGconn*)))
  (PQconnectdb			(PGconn* PQconnectdb (char*)))
  (PQsetdbLogin			(PGconn* PQsetdbLogin (char* char* char* char* char* char* char*)))
  (PQfinish			(void PQfinish (PGconn*)))
  (PQconndefaults		(PQconninfoOption* PQconndefaults (void)))
  (PQconninfoParse		(PQconninfoOption* PQconninfoParse (char* char**)))
  (PQconninfoFree		(void PQconninfoFree (PQconninfoOption*)))
  (PQresetStart			(int PQresetStart (PGconn*)))
  (PQresetPoll			(PostgresPollingStatusType PQresetPoll (PGconn*)))
  (PQreset			(void PQreset (PGconn*)))
  (PQgetCancel			(PGcancel* PQgetCancel (PGconn*)))
  (PQfreeCancel			(void PQfreeCancel (PGcancel*)))
  (PQcancel			(int PQcancel (PGcancel* char* int)))
  (PQrequestCancel		(int PQrequestCancel (PGconn*)))
  (PQdb				(char* PQdb (PGconn*)))
  (PQuser			(char* PQuser (PGconn*)))
  (PQpass			(char* PQpass (PGconn*)))
  (PQhost			(char* PQhost (PGconn*)))
  (PQport			(char* PQport (PGconn*)))
  (PQtty			(char* PQtty (PGconn*)))
  (PQoptions			(char* PQoptions (PGconn*)))
  (PQstatus			(ConnStatusType PQstatus (PGconn*)))
  (PQtransactionStatus		(PGTransactionStatusType PQtransactionStatus (PGconn*)))
  (PQparameterStatus		(char* PQparameterStatus (PGconn* char*)))
  (PQprotocolVersion		(int PQprotocolVersion (PGconn*)))
  (PQserverVersion		(int PQserverVersion (PGconn*)))
  (PQerrorMessage		(char* PQerrorMessage (PGconn*)))
  (PQsocket			(int PQsocket (PGconn*)))
  (PQbackendPID			(int PQbackendPID (PGconn*)))
  (PQconnectionNeedsPassword	(int PQconnectionNeedsPassword (PGconn*)))
  (PQconnectionUsedPassword	(int PQconnectionUsedPassword (PGconn*)))
  (PQclientEncoding		(int PQclientEncoding (PGconn*)))
  (PQsetClientEncoding		(int PQsetClientEncoding (PGconn* char*)))
  (PQgetssl			(void* PQgetssl (PGconn*)))
  (PQinitSSL			(void PQinitSSL (int)))
  (PQinitOpenSSL		(void PQinitOpenSSL (int int)))
  (PQsetErrorVerbosity		(PGVerbosity PQsetErrorVerbosity (PGconn* PGVerbosity)))
  (PQtrace			(void PQtrace (PGconn* FILE*)))
  (PQuntrace			(void PQuntrace (PGconn*)))
  (PQsetNoticeReceiver		(PQnoticeReceiver PQsetNoticeReceiver (PGconn* PQnoticeReceiver void*)))
  (PQsetNoticeProcessor		(PQnoticeProcessor PQsetNoticeProcessor (PGconn* PQnoticeProcessor void*)))
  (PQregisterThreadLock		(pgthreadlock_t PQregisterThreadLock (pgthreadlock_t)))
  (PQexec			(PGresult* PQexec (PGconn* char*)))
  (PQexecParams			(PGresult* PQexecParams (PGconn* char* int Oid* char** int* int* int)))
  (PQprepare			(PGresult* PQprepare (PGconn* char* char* int Oid*)))
  (PQexecPrepared		(PGresult* PQexecPrepared (PGconn* char* int char** int* int* int)))
  (PQsendQuery			(int PQsendQuery (PGconn* char*)))
  (PQsendQueryParams		(int PQsendQueryParams (PGconn* char* int Oid* char** int* int* int)))
  (PQsendPrepare		(int PQsendPrepare (PGconn* char* char* int Oid*)))
  (PQsendQueryPrepared		(int PQsendQueryPrepared (PGconn* char* int char** int* int* int)))
  (PQgetResult			(PGresult* PQgetResult (PGconn*)))
  (PQisBusy			(int PQisBusy (PGconn*)))
  (PQconsumeInput		(int PQconsumeInput (PGconn*)))
  (PQnotifies			(PGnotify* PQnotifies (PGconn*)))
  (PQputCopyData		(int PQputCopyData (PGconn* char* int)))
  (PQputCopyEnd			(int PQputCopyEnd (PGconn* char*)))
  (PQgetCopyData		(int PQgetCopyData (PGconn* char** int)))
  (PQgetline			(int PQgetline (PGconn* char* int)))
  (PQputline			(int PQputline (PGconn* char*)))
  (PQgetlineAsync		(int PQgetlineAsync (PGconn* char* int)))
  (PQputnbytes			(int PQputnbytes (PGconn* char* int)))
  (PQendcopy			(int PQendcopy (PGconn*)))
  (PQsetnonblocking		(int PQsetnonblocking (PGconn* int)))
  (PQisnonblocking		(int PQisnonblocking (PGconn*)))
  (PQisthreadsafe		(int PQisthreadsafe (void)))
  (PQflush			(int PQflush (PGconn*)))
  (PQfn				(PGresult* PQfn (PGconn* int int* int* int PQArgBlock* int)))
  (PQresultStatus		(ExecStatusType PQresultStatus (PGresult*)))
  (PQresStatus			(char* PQresStatus (ExecStatusType)))
  (PQresultErrorMessage		(char* PQresultErrorMessage (PGresult*)))
  (PQresultErrorField		(char* PQresultErrorField (PGresult* int)))
  (PQntuples			(int PQntuples (PGresult*)))
  (PQnfields			(int PQnfields (PGresult*)))
  (PQbinaryTuples		(int PQbinaryTuples (PGresult*)))
  (PQfname			(char* PQfname (PGresult* int)))
  (PQfnumber			(int PQfnumber (PGresult* char*)))
  (PQftable			(Oid PQftable (PGresult* int)))
  (PQftablecol			(int PQftablecol (PGresult* int)))
  (PQfformat			(int PQfformat (PGresult* int)))
  (PQftype			(Oid PQftype (PGresult* int)))
  (PQfsize			(int PQfsize (PGresult* int)))
  (PQfmod			(int PQfmod (PGresult* int)))
  (PQcmdStatus			(char* PQcmdStatus (PGresult*)))
  (PQoidStatus			(char* PQoidStatus (PGresult*)))
  (PQoidValue			(Oid PQoidValue (PGresult*)))
  (PQcmdTuples			(char* PQcmdTuples (PGresult*)))
  (PQgetvalue			(char* PQgetvalue (PGresult* int int)))
  (PQgetlength			(int PQgetlength (PGresult* int int)))
  (PQgetisnull			(int PQgetisnull (PGresult* int int)))
  (PQnparams			(int PQnparams (PGresult*)))
  (PQparamtype			(Oid PQparamtype (PGresult* int)))
  (PQdescribePrepared		(PGresult* PQdescribePrepared (PGconn* char*)))
  (PQdescribePortal		(PGresult* PQdescribePortal (PGconn* char*)))
  (PQsendDescribePrepared	(int PQsendDescribePrepared (PGconn* char*)))
  (PQsendDescribePortal		(int PQsendDescribePortal (PGconn* char*)))
  (PQclear			(void PQclear (PGresult*)))
  (PQfreemem			(void PQfreemem (void*)))
  (PQmakeEmptyPGresult		(PGresult* PQmakeEmptyPGresult (PGconn* ExecStatusType)))
  (PQcopyResult			(PGresult* PQcopyResult (PGresult* int)))
  (PQsetResultAttrs		(int PQsetResultAttrs (PGresult* int PGresAttDesc*)))
  (PQresultAlloc		(void* PQresultAlloc (PGresult* size_t)))
  (PQsetvalue			(int PQsetvalue (PGresult* int int  char* int)))
  (PQescapeStringConn		(size_t PQescapeStringConn (PGconn* char* char* size_t int*)))
  (PQescapeByteaConn		(void* PQescapeByteaConn (PGconn* void* size_t size_t*)))
  (PQunescapeBytea		(void* PQunescapeBytea (void* size_t*)))
  (PQescapeString		(size_t PQescapeString (char* char* size_t)))
  (PQescapeBytea		(void* PQescapeBytea (void* size_t size_t*)))
  (PQprint			(void PQprint (FILE* PGresult* PQprintOpt*)))
  (PQdisplayTuples		(void PQdisplayTuples (PGresult* FILE* int char* int int)))
  (PQprintTuples		(void PQprintTuples (PGresult* FILE* int int int)))
  (lo_open			(int lo_open (PGconn* Oid int)))
  (lo_close			(int lo_close (PGconn* int)))
  (lo_read			(int lo_read (PGconn* int char* size_t)))
  (lo_write			(int lo_write (PGconn* int char* size_t)))
  (lo_lseek			(int lo_lseek (PGconn* int int int)))
  (lo_creat			(Oid lo_creat (PGconn* int)))
  (lo_create			(Oid lo_create (PGconn* Oid)))
  (lo_tell			(int lo_tell (PGconn* int)))
  (lo_truncate			(int lo_truncate (PGconn* int size_t)))
  (lo_unlink			(int lo_unlink (PGconn* Oid)))
  (lo_import			(Oid lo_import (PGconn* char*)))
  (lo_import_with_oid		(Oid lo_import_with_oid (PGconn* char* Oid)))
  (lo_export			(int lo_export (PGconn* Oid char*)))
  (PQmblen			(int PQmblen (char* int)))
  (PQdsplen			(int PQdsplen (char* int)))
  (PQenv2encoding		(int PQenv2encoding (void)))
  (PQencryptPassword		(char* PQencryptPassword (char* char*)))
  (pg_char_to_encoding		(int pg_char_to_encoding (char*)))
  (pg_encoding_to_char		(char* pg_encoding_to_char (int)))
  (pg_valid_server_encoding_id	(int pg_valid_server_encoding_id (int))))

(define PQfreeNotify PQfreemem)


;;;; done

)

;;; end of file
