;;;!mosh
;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Sqlite
;;;Contents: foreign library inspection generator
;;;Date: Fri Nov 27, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
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


;;;; data types inspection

(define-c-type sqlite3_int64	signed-int)
(define-c-type sqlite3_uint64	unsigned-int)



;;;; data structures inspection

(define-c-struct sqlite3_file
  "sqlite3_file"
  (pointer		pMethods))

(define-c-struct sqlite3_io_methods
  "sqlite3_io_methods"
  (signed-int		iVersion)
  (pointer		xClose)
  (pointer		xRead)
  (pointer		xWrite)
  (pointer		xTruncate)
  (pointer		xSync)
  (pointer		xFileSize)
  (pointer		xLock)
  (pointer		xUnlock)
  (pointer		xCheckReservedLock)
  (pointer		xFileControl)
  (pointer		xSectorSize)
  (pointer		xDeviceCharacteristics))

(define-c-struct sqlite3_vfs
  "sqlite3_vfs"
  (signed-int		iVersion)
  (signed-int		szOsFile)
  (signed-int		mxPathname)
  (pointer		pNext)
  (pointer		zName)
  (pointer		pAppData)
  (pointer		xOpen)
  (pointer		xDelete)
  (pointer		xAccess)
  (pointer		xFullPathname)
  (pointer		xDlOpen)
  (pointer		xDlError)
  (pointer		xDlSym)
  (pointer		xDlClose)
  (pointer		xRandomness)
  (pointer		xSleep)
  (pointer		xCurrentTime)
  (pointer		xGetLastError))

(define-c-struct sqlite3_mem_methods
  "sqlite3_mem_methods"
  (pointer		xMalloc)
  (pointer		xFree)
  (pointer		xRealloc)
  (pointer		xSize)
  (pointer		xRoundup)
  (pointer		xInit)
  (pointer		xShutdown)
  (pointer		pAppData))

(define-c-struct sqlite3_mutex_methods
  "sqlite3_mutex_methods"
  (pointer		xMutexInit)
  (pointer		xMutexEnd)
  (pointer		xMutexAlloc)
  (pointer		xMutexFree)
  (pointer		xMutexEnter)
  (pointer		xMutexTry)
  (pointer		xMutexLeave)
  (pointer		xMutexHeld)
  (pointer		xMutexNotheld))

(define-c-struct sqlite3_pcache_methods
  "sqlite3_pcache_methods"
  (pointer		pArg)
  (pointer		xInit)
  (pointer		xShutdown)
  (pointer		xCreate)
  (pointer		xCachesize)
  (pointer		xPagecount)
  (pointer		xFetch)
  (pointer		xUnpin)
  (pointer		xRekey)
  (pointer		xTruncate)
  (pointer		xDestroy))


;;;; constants

(sizeof-lib
 (define SQLITE_ENABLE_COLUMN_METADATA	^SQLITE_ENABLE_COLUMN_METADATA^)
 (define SQLITE_DEBUG			^SQLITE_DEBUG^)
 (define SQLITE_ENABLE_UNLOCK_NOTIFY	^SQLITE_ENABLE_UNLOCK_NOTIFY^))

(sizeof-lib-exports
 SQLITE_ENABLE_COLUMN_METADATA
 SQLITE_DEBUG
 SQLITE_ENABLE_UNLOCK_NOTIFY)

(define-c-string-defines "version strings"
  SQLITE_VERSION
  SQLITE_SOURCE_ID)

(define-c-defines "version numbers"
  SQLITE_VERSION_NUMBER)

(define-c-defines "error codes"
  SQLITE_OK
  SQLITE_ERROR
  SQLITE_INTERNAL
  SQLITE_PERM
  SQLITE_ABORT
  SQLITE_BUSY
  SQLITE_LOCKED
  SQLITE_NOMEM
  SQLITE_READONLY
  SQLITE_INTERRUPT
  SQLITE_IOERR
  SQLITE_CORRUPT
  SQLITE_NOTFOUND
  SQLITE_FULL
  SQLITE_CANTOPEN
  SQLITE_PROTOCOL
  SQLITE_EMPTY
  SQLITE_SCHEMA
  SQLITE_TOOBIG
  SQLITE_CONSTRAINT
  SQLITE_MISMATCH
  SQLITE_MISUSE
  SQLITE_NOLFS
  SQLITE_AUTH
  SQLITE_FORMAT
  SQLITE_RANGE
  SQLITE_NOTADB
  SQLITE_ROW
  SQLITE_DONE)

(define-c-defines "extended error codes"
  SQLITE_IOERR_READ
  SQLITE_IOERR_SHORT_READ
  SQLITE_IOERR_WRITE
  SQLITE_IOERR_FSYNC
  SQLITE_IOERR_DIR_FSYNC
  SQLITE_IOERR_TRUNCATE
  SQLITE_IOERR_FSTAT
  SQLITE_IOERR_UNLOCK
  SQLITE_IOERR_RDLOCK
  SQLITE_IOERR_DELETE
  SQLITE_IOERR_BLOCKED
  SQLITE_IOERR_NOMEM
  SQLITE_IOERR_ACCESS
  SQLITE_IOERR_CHECKRESERVEDLOCK
  SQLITE_IOERR_LOCK
  SQLITE_IOERR_CLOSE
  SQLITE_IOERR_DIR_CLOSE
  SQLITE_LOCKED_SHAREDCACHE)

(define-c-defines "file open flags"
  SQLITE_OPEN_READONLY
  SQLITE_OPEN_READWRITE
  SQLITE_OPEN_CREATE
  SQLITE_OPEN_DELETEONCLOSE
  SQLITE_OPEN_EXCLUSIVE
  SQLITE_OPEN_MAIN_DB
  SQLITE_OPEN_TEMP_DB
  SQLITE_OPEN_TRANSIENT_DB
  SQLITE_OPEN_MAIN_JOURNAL
  SQLITE_OPEN_TEMP_JOURNAL
  SQLITE_OPEN_SUBJOURNAL
  SQLITE_OPEN_MASTER_JOURNAL
  SQLITE_OPEN_NOMUTEX
  SQLITE_OPEN_FULLMUTEX
  SQLITE_OPEN_SHAREDCACHE
  SQLITE_OPEN_PRIVATECACHE)

(define-c-defines "device characteristics"
  SQLITE_IOCAP_ATOMIC
  SQLITE_IOCAP_ATOMIC512
  SQLITE_IOCAP_ATOMIC1K
  SQLITE_IOCAP_ATOMIC2K
  SQLITE_IOCAP_ATOMIC4K
  SQLITE_IOCAP_ATOMIC8K
  SQLITE_IOCAP_ATOMIC16K
  SQLITE_IOCAP_ATOMIC32K
  SQLITE_IOCAP_ATOMIC64K
  SQLITE_IOCAP_SAFE_APPEND
  SQLITE_IOCAP_SEQUENTIAL)

(define-c-defines "file locking levels"
  SQLITE_LOCK_NONE
  SQLITE_LOCK_SHARED
  SQLITE_LOCK_RESERVED
  SQLITE_LOCK_PENDING
  SQLITE_LOCK_EXCLUSIVE)

(define-c-defines "synchronisation type flags"
  SQLITE_SYNC_NORMAL
  SQLITE_SYNC_FULL
  SQLITE_SYNC_DATAONLY)

(define-c-defines "file control opcodes"
  SQLITE_FCNTL_LOCKSTATE
  SQLITE_GET_LOCKPROXYFILE
  SQLITE_SET_LOCKPROXYFILE
  SQLITE_LAST_ERRNO)

(define-c-defines "flags for the xAccess method"
  SQLITE_ACCESS_EXISTS
  SQLITE_ACCESS_READWRITE
  SQLITE_ACCESS_READ)

(define-c-defines "configuration options"
  SQLITE_CONFIG_SINGLETHREAD
  SQLITE_CONFIG_MULTITHREAD
  SQLITE_CONFIG_SERIALIZED
  SQLITE_CONFIG_MALLOC
  SQLITE_CONFIG_GETMALLOC
  SQLITE_CONFIG_SCRATCH
  SQLITE_CONFIG_PAGECACHE
  SQLITE_CONFIG_HEAP
  SQLITE_CONFIG_MEMSTATUS
  SQLITE_CONFIG_MUTEX
  SQLITE_CONFIG_GETMUTEX
  SQLITE_CONFIG_CHUNKALLOC
  SQLITE_CONFIG_LOOKASIDE
  SQLITE_CONFIG_PCACHE
  SQLITE_CONFIG_GETPCACHE)

(define-c-defines "other configuration options"
  SQLITE_DBCONFIG_LOOKASIDE)

(define-c-defines "authoriser return codes"
  SQLITE_DENY
  SQLITE_IGNORE)

(define-c-defines "authoriser action codes"
  SQLITE_CREATE_INDEX
  SQLITE_CREATE_TABLE
  SQLITE_CREATE_TEMP_INDEX
  SQLITE_CREATE_TEMP_TABLE
  SQLITE_CREATE_TEMP_TRIGGER
  SQLITE_CREATE_TEMP_VIEW
  SQLITE_CREATE_TRIGGER
  SQLITE_CREATE_VIEW
  SQLITE_DELETE
  SQLITE_DROP_INDEX
  SQLITE_DROP_TABLE
  SQLITE_DROP_TEMP_INDEX
  SQLITE_DROP_TEMP_TABLE
  SQLITE_DROP_TEMP_TRIGGER
  SQLITE_DROP_TEMP_VIEW
  SQLITE_DROP_TRIGGER
  SQLITE_DROP_VIEW
  SQLITE_INSERT
  SQLITE_PRAGMA
  SQLITE_READ
  SQLITE_SELECT
  SQLITE_TRANSACTION
  SQLITE_UPDATE
  SQLITE_ATTACH
  SQLITE_DETACH
  SQLITE_ALTER_TABLE
  SQLITE_REINDEX
  SQLITE_ANALYZE
  SQLITE_CREATE_VTABLE
  SQLITE_DROP_VTABLE
  SQLITE_FUNCTION
  SQLITE_SAVEPOINT
  SQLITE_COPY)

(define-c-defines "run-time limit categories"
  SQLITE_LIMIT_LENGTH
  SQLITE_LIMIT_SQL_LENGTH
  SQLITE_LIMIT_COLUMN
  SQLITE_LIMIT_EXPR_DEPTH
  SQLITE_LIMIT_COMPOUND_SELECT
  SQLITE_LIMIT_VDBE_OP
  SQLITE_LIMIT_FUNCTION_ARG
  SQLITE_LIMIT_ATTACHED
  SQLITE_LIMIT_LIKE_PATTERN_LENGTH
  SQLITE_LIMIT_VARIABLE_NUMBER
  SQLITE_LIMIT_TRIGGER_DEPTH)

(define-c-defines "fundamental data types"
  SQLITE_INTEGER
  SQLITE_FLOAT
  SQLITE_BLOB
  SQLITE_NULL
  SQLITE_TEXT
  SQLITE3_TEXT)

(define-c-defines "text encodings"
  SQLITE_UTF8
  SQLITE_UTF16LE
  SQLITE_UTF16BE
  SQLITE_UTF16
  SQLITE_ANY
  SQLITE_UTF16_ALIGNED)

(define-c-defines "special constructor behaviour"
  SQLITE_STATIC
  SQLITE_TRANSIENT)

(define-c-defines "mutex types"
  SQLITE_MUTEX_FAST
  SQLITE_MUTEX_RECURSIVE
  SQLITE_MUTEX_STATIC_MASTER
  SQLITE_MUTEX_STATIC_MEM
  SQLITE_MUTEX_STATIC_MEM2
  SQLITE_MUTEX_STATIC_OPEN
  SQLITE_MUTEX_STATIC_PRNG
  SQLITE_MUTEX_STATIC_LRU
  SQLITE_MUTEX_STATIC_LRU2)

(define-c-defines "test operation codes"
  SQLITE_TESTCTRL_PRNG_SAVE
  SQLITE_TESTCTRL_PRNG_RESTORE
  SQLITE_TESTCTRL_PRNG_RESET
  SQLITE_TESTCTRL_BITVEC_TEST
  SQLITE_TESTCTRL_FAULT_INSTALL
  SQLITE_TESTCTRL_BENIGN_MALLOC_HOOKS
  SQLITE_TESTCTRL_PENDING_BYTE
  SQLITE_TESTCTRL_ASSERT
  SQLITE_TESTCTRL_ALWAYS
  SQLITE_TESTCTRL_RESERVE)

(define-c-defines "status parameters"
  SQLITE_STATUS_MEMORY_USED
  SQLITE_STATUS_PAGECACHE_USED
  SQLITE_STATUS_PAGECACHE_OVERFLOW
  SQLITE_STATUS_SCRATCH_USED
  SQLITE_STATUS_SCRATCH_OVERFLOW
  SQLITE_STATUS_MALLOC_SIZE
  SQLITE_STATUS_PARSER_STACK
  SQLITE_STATUS_PAGECACHE_SIZE
  SQLITE_STATUS_SCRATCH_SIZE

  SQLITE_DBSTATUS_LOOKASIDE_USED

  SQLITE_STMTSTATUS_FULLSCAN_STEP
  SQLITE_STMTSTATUS_SORT)


;;;; done

(define-shared-object sqlite libsqlite3.so)

(define sqlite-library-spec
  '(foreign databases sqlite sizeof))

(autoconf-lib-write "configuration/sqlite-inspector.m4" sqlite-library-spec)
(sizeof-lib-write   "src/libraries/foreign/databases/sqlite/sizeof.sls.in" sqlite-library-spec)

;;; end of file
