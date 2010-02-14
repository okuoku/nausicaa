dnl (foreign databases postgresql sizeof) --
dnl
dnl Part of: Nausicaa
dnl Contents: foreign library inspection generation
dnl Date: Sun Feb 14, 2010
dnl
dnl Abstract
dnl
dnl
dnl
dnl Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
dnl
dnl This program is free software:  you can redistribute it and/or modify
dnl it under the terms of the  GNU General Public License as published by
dnl the Free Software Foundation, either version 3 of the License, or (at
dnl your option) any later version.
dnl
dnl This program is  distributed in the hope that it  will be useful, but
dnl WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
dnl MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
dnl General Public License for more details.
dnl
dnl You should  have received  a copy of  the GNU General  Public License
dnl along with this program.  If not, see <http://www.gnu.org/licenses/>.
dnl


NAUSICAA_INSPECT_TYPE([OID],[Oid],[unsigned-int],[#f])

dnl Struct inspection: PGconn
NAUSICAA_INSPECT_STRUCT_TYPE([PGCONN],[struct pg_conn],[#f])

dnl Struct inspection: PGresult
NAUSICAA_INSPECT_STRUCT_TYPE([PGRESULT],[struct pg_result],[#f])

dnl Struct inspection: PGcancel
NAUSICAA_INSPECT_STRUCT_TYPE([PGCANCEL],[struct pg_cancel],[#f])

dnl Struct inspection: PGnotify
NAUSICAA_INSPECT_STRUCT_TYPE([PGNOTIFY],[struct pgNotify],[#f])
NAUSICAA_INSPECT_FIELD_TYPE([PGNOTIFY_RELNAME],[struct pgNotify],[relname],[pointer])
NAUSICAA_INSPECT_FIELD_TYPE([PGNOTIFY_BE_PID],[struct pgNotify],[be_pid],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([PGNOTIFY_EXTRA],[struct pgNotify],[extra],[pointer])
NAUSICAA_INSPECT_FIELD_TYPE([PGNOTIFY_NEXT],[struct pgNotify],[next],[pointer])

dnl Struct inspection: PQprintOpt
NAUSICAA_INSPECT_STRUCT_TYPE([PQPRINTOPT],[struct _PQprintOpt],[#f])
NAUSICAA_INSPECT_FIELD_TYPE([PQPRINTOPT_HEADER],[struct _PQprintOpt],[header],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([PQPRINTOPT_ALIGN],[struct _PQprintOpt],[align],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([PQPRINTOPT_STANDARD],[struct _PQprintOpt],[standard],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([PQPRINTOPT_HTML3],[struct _PQprintOpt],[html3],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([PQPRINTOPT_EXPANDED],[struct _PQprintOpt],[expanded],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([PQPRINTOPT_PAGER],[struct _PQprintOpt],[pager],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([PQPRINTOPT_FIELDSEP],[struct _PQprintOpt],[fieldSep],[pointer])
NAUSICAA_INSPECT_FIELD_TYPE([PQPRINTOPT_TABLEOPT],[struct _PQprintOpt],[tableOpt],[pointer])
NAUSICAA_INSPECT_FIELD_TYPE([PQPRINTOPT_CAPTION],[struct _PQprintOpt],[caption],[pointer])
NAUSICAA_INSPECT_FIELD_TYPE([PQPRINTOPT_FIELDNAME],[struct _PQprintOpt],[fieldName],[pointer])

dnl Struct inspection: PQconninfoOption
NAUSICAA_INSPECT_STRUCT_TYPE([PQCONNINFOOPTION],[struct _PQconninfoOption],[#f])
NAUSICAA_INSPECT_FIELD_TYPE([PQCONNINFOOPTION_KEYWORD],[struct _PQconninfoOption],[keyword],[pointer])
NAUSICAA_INSPECT_FIELD_TYPE([PQCONNINFOOPTION_ENVVAR],[struct _PQconninfoOption],[envvar],[pointer])
NAUSICAA_INSPECT_FIELD_TYPE([PQCONNINFOOPTION_COMPILED],[struct _PQconninfoOption],[compiled],[pointer])
NAUSICAA_INSPECT_FIELD_TYPE([PQCONNINFOOPTION_VAL],[struct _PQconninfoOption],[val],[pointer])
NAUSICAA_INSPECT_FIELD_TYPE([PQCONNINFOOPTION_LABEL],[struct _PQconninfoOption],[label],[pointer])
NAUSICAA_INSPECT_FIELD_TYPE([PQCONNINFOOPTION_DISPCHAR],[struct _PQconninfoOption],[dispchar],[pointer])
NAUSICAA_INSPECT_FIELD_TYPE([PQCONNINFOOPTION_DISPSIZE],[struct _PQconninfoOption],[dispsize],[signed-int])

dnl Struct inspection: PQArgBlock
NAUSICAA_INSPECT_STRUCT_TYPE([PQARGBLOCK],[PQArgBlock],[#f])
NAUSICAA_INSPECT_FIELD_TYPE([PQARGBLOCK_LEN],[PQArgBlock],[len],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([PQARGBLOCK_ISINT],[PQArgBlock],[isint],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([PQARGBLOCK_PTR],[PQArgBlock],[ptr],[pointer])
NAUSICAA_INSPECT_FIELD_TYPE([PQARGBLOCK_INTEGER],[PQArgBlock],[integer],[signed-int])

dnl Struct inspection: PGresAttDesc
NAUSICAA_INSPECT_STRUCT_TYPE([PGRESATTDESC],[struct pgresAttDesc],[#f])
NAUSICAA_INSPECT_FIELD_TYPE([PGRESATTDESC_NAME],[struct pgresAttDesc],[name],[pointer])
NAUSICAA_INSPECT_FIELD_TYPE([PGRESATTDESC_TABLEID],[struct pgresAttDesc],[tableid],[unsigned-int])
NAUSICAA_INSPECT_FIELD_TYPE([PGRESATTDESC_COLUMNID],[struct pgresAttDesc],[columnid],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([PGRESATTDESC_FORMAT],[struct pgresAttDesc],[format],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([PGRESATTDESC_TYPID],[struct pgresAttDesc],[typid],[unsigned-int])
NAUSICAA_INSPECT_FIELD_TYPE([PGRESATTDESC_TYPLEN],[struct pgresAttDesc],[typlen],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([PGRESATTDESC_ATTTYPMOD],[struct pgresAttDesc],[atttypmod],[signed-int])

dnl Preprocessor symbols: Miscellaneous
NAUSICAA_DEFINE_VALUE([OID_MAX])

dnl Preprocessor symbols: Identifiers of error message fields
NAUSICAA_DEFINE_VALUE([PG_DIAG_SEVERITY])
NAUSICAA_DEFINE_VALUE([PG_DIAG_SQLSTATE])
NAUSICAA_DEFINE_VALUE([PG_DIAG_MESSAGE_PRIMARY])
NAUSICAA_DEFINE_VALUE([PG_DIAG_MESSAGE_DETAIL])
NAUSICAA_DEFINE_VALUE([PG_DIAG_MESSAGE_HINT])
NAUSICAA_DEFINE_VALUE([PG_DIAG_STATEMENT_POSITION])
NAUSICAA_DEFINE_VALUE([PG_DIAG_INTERNAL_POSITION])
NAUSICAA_DEFINE_VALUE([PG_DIAG_INTERNAL_QUERY])
NAUSICAA_DEFINE_VALUE([PG_DIAG_CONTEXT])
NAUSICAA_DEFINE_VALUE([PG_DIAG_SOURCE_FILE])
NAUSICAA_DEFINE_VALUE([PG_DIAG_SOURCE_LINE])
NAUSICAA_DEFINE_VALUE([PG_DIAG_SOURCE_FUNCTION])

dnl Preprocessor symbols: Option flags for PQcopyResult
NAUSICAA_DEFINE_VALUE([PG_COPYRES_ATTRS])
NAUSICAA_DEFINE_VALUE([PG_COPYRES_TUPLES])
NAUSICAA_DEFINE_VALUE([PG_COPYRES_EVENTS])
NAUSICAA_DEFINE_VALUE([PG_COPYRES_NOTICEHOOKS])
NAUSICAA_INSPECT_TYPE([CONNSTATUSTYPE],[ConnStatusType],[signed-int],[#f])

dnl enum ConnStatusType
NAUSICAA_ENUM_VALUE([CONNECTION_OK])
NAUSICAA_ENUM_VALUE([CONNECTION_BAD])
NAUSICAA_ENUM_VALUE([CONNECTION_STARTED])
NAUSICAA_ENUM_VALUE([CONNECTION_MADE])
NAUSICAA_ENUM_VALUE([CONNECTION_AWAITING_RESPONSE])
NAUSICAA_ENUM_VALUE([CONNECTION_AUTH_OK])
NAUSICAA_ENUM_VALUE([CONNECTION_SETENV])
NAUSICAA_ENUM_VALUE([CONNECTION_SSL_STARTUP])
NAUSICAA_ENUM_VALUE([CONNECTION_NEEDED])
NAUSICAA_INSPECT_TYPE([POSTGRESPOLLINGSTATUSTYPE],[PostgresPollingStatusType],[signed-int],[#f])

dnl enum PostgresPollingStatusType
NAUSICAA_ENUM_VALUE([PGRES_POLLING_FAILED])
NAUSICAA_ENUM_VALUE([PGRES_POLLING_READING])
NAUSICAA_ENUM_VALUE([PGRES_POLLING_WRITING])
NAUSICAA_ENUM_VALUE([PGRES_POLLING_OK])
NAUSICAA_ENUM_VALUE([PGRES_POLLING_ACTIVE])
NAUSICAA_INSPECT_TYPE([EXECSTATUSTYPE],[ExecStatusType],[signed-int],[#f])

dnl enum ExecStatusType
NAUSICAA_ENUM_VALUE([PGRES_EMPTY_QUERY])
NAUSICAA_ENUM_VALUE([PGRES_COMMAND_OK])
NAUSICAA_ENUM_VALUE([PGRES_TUPLES_OK])
NAUSICAA_ENUM_VALUE([PGRES_COPY_OUT])
NAUSICAA_ENUM_VALUE([PGRES_COPY_IN])
NAUSICAA_ENUM_VALUE([PGRES_BAD_RESPONSE])
NAUSICAA_ENUM_VALUE([PGRES_NONFATAL_ERROR])
NAUSICAA_ENUM_VALUE([PGRES_FATAL_ERROR])
NAUSICAA_INSPECT_TYPE([PGTRANSACTIONSTATUSTYPE],[PGTransactionStatusType],[signed-int],[#f])

dnl enum PGTransactionStatusType
NAUSICAA_ENUM_VALUE([PQTRANS_IDLE])
NAUSICAA_ENUM_VALUE([PQTRANS_ACTIVE])
NAUSICAA_ENUM_VALUE([PQTRANS_INTRANS])
NAUSICAA_ENUM_VALUE([PQTRANS_INERROR])
NAUSICAA_ENUM_VALUE([PQTRANS_UNKNOWN])
NAUSICAA_INSPECT_TYPE([PGVERBOSITY],[PGVerbosity],[signed-int],[#f])

dnl enum PGVerbosity
NAUSICAA_ENUM_VALUE([PQERRORS_TERSE])
NAUSICAA_ENUM_VALUE([PQERRORS_DEFAULT])
NAUSICAA_ENUM_VALUE([PQERRORS_VERBOSE])
NAU_DS_WITH_OPTION([POSTGRESQL_SHARED_OBJECT],[postgresql-shared-object],[libpq.so],
  [Postgresql shared library file],[select Postgresql shared library file])

dnl end of file
