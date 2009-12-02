;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/SQLite
;;;Contents: high level interface
;;;Date: Thu Oct 29, 2009
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


(library (foreign databases sqlite)
  (export

    ;; version functions
    sqlite-libversion
    sqlite-sourceid
    sqlite-libversion-number
    sqlite-threadsafe

    ;; initialisation and configuration

    sqlite-initialize
    sqlite-shutdown
    sqlite-os-init
    sqlite-os-end
    sqlite-extended-result-codes

    ;; database connection handle

    sqlite-close
    sqlite-exec

    ;; database interaction
    sqlite-open
    sqlite-open16
    sqlite-open-v2
    sqlite-last-insert-rowid
    sqlite-changes
    sqlite-total-changes
    sqlite-interrupt
    sqlite-complete
    sqlite-complete16
    sqlite-busy-handler
    sqlite-busy-timeout
    sqlite-get-table
    sqlite-free-table
    sqlite-progress-handler

    ;; SQL stuff
    sqlite-prepare
    sqlite-prepare-v2
    sqlite-prepare16
    sqlite-prepare16-v2
    sqlite-sql
    sqlite-finalize
    sqlite-reset
    sqlite-db-handle
    sqlite-next-stmt
    sqlite-table-column-metadata

    sqlite-bind-parameter-count
    sqlite-bind-parameter-name
    sqlite-bind-parameter-index
    sqlite-clear-bindings
    sqlite-column-count
    sqlite-column-name
    sqlite-column-name16

    sqlite-column-database-name
    sqlite-column-database-name16
    sqlite-column-table-name
    sqlite-column-table-name16
    sqlite-column-origin-name
    sqlite-column-origin-name16

    sqlite-column-decltype
    sqlite-column-decltype16
    sqlite-step
    sqlite-data-count

    sqlite-column-blob
    sqlite-column-bytes
    sqlite-column-bytes16
    sqlite-column-double
    sqlite-column-int
    sqlite-column-int64
    sqlite-column-text
    sqlite-column-text16
    sqlite-column-type
    sqlite-column-value

    sqlite-create-function
    sqlite-create-function16

    sqlite-value-blob
    sqlite-value-bytes
    sqlite-value-bytes16
    sqlite-value-double
    sqlite-value-int
    sqlite-value-int64
    sqlite-value-text
    sqlite-value-text16
    sqlite-value-text16le
    sqlite-value-text16be
    sqlite-value-type
    sqlite-value-numeric-type

    ;; stuff for SQL functions
    sqlite-aggregate-context
    sqlite-user-data
    sqlite-context-db-handle

    sqlite-get-auxdata
    sqlite-set-auxdata

    sqlite-result-blob
    sqlite-result-double
    sqlite-result-error
    sqlite-result-error16
    sqlite-result-error-toobig
    sqlite-result-error-nomem
    sqlite-result-error-code
    sqlite-result-int
    sqlite-result-int64
    sqlite-result-null
    sqlite-result-text
    sqlite-result-text16
    sqlite-result-text16le
    sqlite-result-text16be
    sqlite-result-value
    sqlite-result-zeroblob

    ;; collations
    sqlite-create-collation
    sqlite-create-collation-v2
    sqlite-create-collation16

    sqlite-collation-needed
    sqlite-collation-needed16

    ;; memory allocation
    sqlite-malloc
    sqlite-realloc
    sqlite-free
    sqlite-memory-used
    sqlite-memory-highwater
    sqlite-release-memory

    ;; commit
    sqlite-get-autocommit
    sqlite-commit-hook
    sqlite-rollback-hook

    ;; extensions
    sqlite-load-extension
    sqlite-enable-load-extension
    sqlite-auto-extension
    sqlite-reset-auto-extension

    ;; blobs
    sqlite-blob-open
    sqlite-blob-close
    sqlite-blob-bytes
    sqlite-blob-read
    sqlite-blob-write

    sqlite-bind-blob
    sqlite-bind-double
    sqlite-bind-int
    sqlite-bind-int64
    sqlite-bind-null
    sqlite-bind-text
    sqlite-bind-text16
    sqlite-bind-value
    sqlite-bind-zeroblob

    ;; mutexes
    sqlite-mutex-alloc
    sqlite-mutex-free
    sqlite-mutex-enter
    sqlite-mutex-try
    sqlite-mutex-leave
    sqlite-mutex-held
    sqlite-mutex-notheld
    sqlite-db-mutex

    ;; online backup
    sqlite-backup-init
    sqlite-backup-step
    sqlite-backup-finish
    sqlite-backup-remaining
    sqlite-backup-pagecount

    ;; miscellaneous
    sqlite-randomness
    sqlite-set-authorizer
    sqlite-trace
    sqlite-profile
    sqlite-limit
    sqlite-sleep
    sqlite-update_hook
    sqlite-enable-shared-cache
    sqlite-soft-heap-limit
    sqlite-file-control
    sqlite-status
    sqlite-db-status
    sqlite-stmt-status
    sqlite-unlock-notify
    sqlite-strnicmp

    sqlite-errcode
    sqlite-extended-errcode
    sqlite-errmsg
    sqlite-errmsg16

    sqlite-vfs-find
    sqlite-vfs-register
    sqlite-vfs-unregister

;;; --------------------------------------------------------------------

    (rename (sqlite-finalize		sqlite-finalise))


;;; data types inspection

    sqlite3_int64
    sizeof-sqlite3_int64
    strideof-sqlite3_int64
    alignof-sqlite3_int64

    sqlite3_uint64
    sizeof-sqlite3_uint64
    strideof-sqlite3_uint64
    alignof-sqlite3_uint64


;;; data structures inspection

    sizeof-sqlite3_file
    strideof-sqlite3_file
    alignof-sqlite3_file

    sizeof-sqlite3_io_methods
    strideof-sqlite3_io_methods
    alignof-sqlite3_io_methods

    sizeof-sqlite3_vfs
    strideof-sqlite3_vfs
    alignof-sqlite3_vfs

    sizeof-sqlite3_mem_methods
    strideof-sqlite3_mem_methods
    alignof-sqlite3_mem_methods

    sizeof-sqlite3_mutex_methods
    strideof-sqlite3_mutex_methods
    alignof-sqlite3_mutex_methods

    sizeof-sqlite3_pcache_methods
    strideof-sqlite3_pcache_methods
    alignof-sqlite3_pcache_methods


;;; constants

    SQLITE_DEBUG
    SQLITE_ENABLE_COLUMN_METADATA
    SQLITE_ENABLE_UNLOCK_NOTIFY

    ;; version constants
    SQLITE_VERSION
    SQLITE_SOURCE_ID
    SQLITE_VERSION_NUMBER

    ;; error codes
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
    SQLITE_DONE

    ;; extended error codes
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
    SQLITE_LOCKED_SHAREDCACHE

    ;; file open flags
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
    SQLITE_OPEN_PRIVATECACHE

    ;; device characteristics
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
    SQLITE_IOCAP_SEQUENTIAL

    ;; file locking levels
    SQLITE_LOCK_NONE
    SQLITE_LOCK_SHARED
    SQLITE_LOCK_RESERVED
    SQLITE_LOCK_PENDING
    SQLITE_LOCK_EXCLUSIVE

    ;; synchronisation type flags
    SQLITE_SYNC_NORMAL
    SQLITE_SYNC_FULL
    SQLITE_SYNC_DATAONLY

    ;; file control opcodes
    SQLITE_FCNTL_LOCKSTATE
    SQLITE_GET_LOCKPROXYFILE
    SQLITE_SET_LOCKPROXYFILE
    SQLITE_LAST_ERRNO

    ;; flags for the xAccess method
    SQLITE_ACCESS_EXISTS
    SQLITE_ACCESS_READWRITE
    SQLITE_ACCESS_READ

    ;; configuration options
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
    SQLITE_CONFIG_GETPCACHE

    ;; other configuration options
    SQLITE_DBCONFIG_LOOKASIDE

    ;; authoriser return codes
    SQLITE_DENY
    SQLITE_IGNORE

    ;; authoriser action codes
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
    SQLITE_COPY

    ;; run-time limit categories
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
    SQLITE_LIMIT_TRIGGER_DEPTH

    ;; fundamental data types
    SQLITE_INTEGER
    SQLITE_FLOAT
    SQLITE_BLOB
    SQLITE_NULL
    SQLITE_TEXT
    SQLITE3_TEXT

    ;; text encodings
    SQLITE_UTF8
    SQLITE_UTF16LE
    SQLITE_UTF16BE
    SQLITE_UTF16
    SQLITE_ANY
    SQLITE_UTF16_ALIGNED

    ;; mutex types
    SQLITE_MUTEX_FAST
    SQLITE_MUTEX_RECURSIVE
    SQLITE_MUTEX_STATIC_MASTER
    SQLITE_MUTEX_STATIC_MEM
    SQLITE_MUTEX_STATIC_MEM2
    SQLITE_MUTEX_STATIC_OPEN
    SQLITE_MUTEX_STATIC_PRNG
    SQLITE_MUTEX_STATIC_LRU
    SQLITE_MUTEX_STATIC_LRU2

    ;; test operation codes
    SQLITE_TESTCTRL_PRNG_SAVE
    SQLITE_TESTCTRL_PRNG_RESTORE
    SQLITE_TESTCTRL_PRNG_RESET
    SQLITE_TESTCTRL_BITVEC_TEST
    SQLITE_TESTCTRL_FAULT_INSTALL
    SQLITE_TESTCTRL_BENIGN_MALLOC_HOOKS
    SQLITE_TESTCTRL_PENDING_BYTE
    SQLITE_TESTCTRL_ASSERT
    SQLITE_TESTCTRL_ALWAYS
    SQLITE_TESTCTRL_RESERVE

    ;; status parameters
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
    SQLITE_STMTSTATUS_SORT


;;; data structures accessors and mutators

struct-sqlite3_file-pMethods-set!			struct-sqlite3_file-pMethods-ref

struct-sqlite3_io_methods-iVersion-set!			struct-sqlite3_io_methods-iVersion-ref
struct-sqlite3_io_methods-xClose-set!			struct-sqlite3_io_methods-xClose-ref
struct-sqlite3_io_methods-xRead-set!			struct-sqlite3_io_methods-xRead-ref
struct-sqlite3_io_methods-xWrite-set!			struct-sqlite3_io_methods-xWrite-ref
struct-sqlite3_io_methods-xTruncate-set!		struct-sqlite3_io_methods-xTruncate-ref
struct-sqlite3_io_methods-xSync-set!			struct-sqlite3_io_methods-xSync-ref
struct-sqlite3_io_methods-xFileSize-set!		struct-sqlite3_io_methods-xFileSize-ref
struct-sqlite3_io_methods-xLock-set!			struct-sqlite3_io_methods-xLock-ref
struct-sqlite3_io_methods-xUnlock-set!			struct-sqlite3_io_methods-xUnlock-ref
struct-sqlite3_io_methods-xCheckReservedLock-set!	struct-sqlite3_io_methods-xCheckReservedLock-ref
struct-sqlite3_io_methods-xFileControl-set!		struct-sqlite3_io_methods-xFileControl-ref
struct-sqlite3_io_methods-xSectorSize-set!		struct-sqlite3_io_methods-xSectorSize-ref
struct-sqlite3_io_methods-xDeviceCharacteristics-set!
struct-sqlite3_io_methods-xDeviceCharacteristics-ref

struct-sqlite3_vfs-iVersion-set!		struct-sqlite3_vfs-iVersion-ref
struct-sqlite3_vfs-szOsFile-set!		struct-sqlite3_vfs-szOsFile-ref
struct-sqlite3_vfs-mxPathname-set!		struct-sqlite3_vfs-mxPathname-ref
struct-sqlite3_vfs-pNext-set!			struct-sqlite3_vfs-pNext-ref
struct-sqlite3_vfs-zName-set!			struct-sqlite3_vfs-zName-ref
struct-sqlite3_vfs-pAppData-set!		struct-sqlite3_vfs-pAppData-ref
struct-sqlite3_vfs-xOpen-set!			struct-sqlite3_vfs-xOpen-ref
struct-sqlite3_vfs-xDelete-set!			struct-sqlite3_vfs-xDelete-ref
struct-sqlite3_vfs-xAccess-set!			struct-sqlite3_vfs-xAccess-ref
struct-sqlite3_vfs-xFullPathname-set!		struct-sqlite3_vfs-xFullPathname-ref
struct-sqlite3_vfs-xDlOpen-set!			struct-sqlite3_vfs-xDlOpen-ref
struct-sqlite3_vfs-xDlError-set!		struct-sqlite3_vfs-xDlError-ref
struct-sqlite3_vfs-xDlSym-set!			struct-sqlite3_vfs-xDlSym-ref
struct-sqlite3_vfs-xDlClose-set!		struct-sqlite3_vfs-xDlClose-ref
struct-sqlite3_vfs-xRandomness-set!		struct-sqlite3_vfs-xRandomness-ref
struct-sqlite3_vfs-xSleep-set!			struct-sqlite3_vfs-xSleep-ref
struct-sqlite3_vfs-xCurrentTime-set!		struct-sqlite3_vfs-xCurrentTime-ref
struct-sqlite3_vfs-xGetLastError-set!		struct-sqlite3_vfs-xGetLastError-ref

struct-sqlite3_mem_methods-xMalloc-set!		struct-sqlite3_mem_methods-xMalloc-ref
struct-sqlite3_mem_methods-xFree-set!		struct-sqlite3_mem_methods-xFree-ref
struct-sqlite3_mem_methods-xRealloc-set!	struct-sqlite3_mem_methods-xRealloc-ref
struct-sqlite3_mem_methods-xSize-set!		struct-sqlite3_mem_methods-xSize-ref
struct-sqlite3_mem_methods-xRoundup-set!	struct-sqlite3_mem_methods-xRoundup-ref
struct-sqlite3_mem_methods-xInit-set!		struct-sqlite3_mem_methods-xInit-ref
struct-sqlite3_mem_methods-xShutdown-set!	struct-sqlite3_mem_methods-xShutdown-ref
struct-sqlite3_mem_methods-pAppData-set!	struct-sqlite3_mem_methods-pAppData-ref


;;;; enumerations

    sqlite-open-flag-symbol		sqlite-open-flags


;;;; conditions

    &sqlite-session
    make-sqlite-session-condition
    sqlite-session-condition?
    sqlite-session-condition

    &sqlite-database
    make-sqlite-database-condition
    sqlite-database-condition?
    sqlite-database-condition

    &sqlite-query
    make-sqlite-query-condition
    sqlite-query-condition?
    sqlite-query-condition

    &sqlite-statement
    make-sqlite-statement-condition
    sqlite-statement-condition?
    sqlite-statement-condition

;;; --------------------------------------------------------------------

    &sqlite-error
    make-sqlite-error-condition
    sqlite-error-condition?

    &sqlite-opening-error
    make-sqlite-opening-error-condition
    sqlite-opening-error-condition?
    raise-sqlite-opening-error

    &sqlite-querying-error
    make-sqlite-querying-error-condition
    sqlite-querying-error-condition?
    raise-sqlite-querying-error

    &sqlite-finalizing-error
    make-sqlite-finalizing-error-condition
    sqlite-finalizing-error-condition?
    raise-sqlite-finalizing-error

    &sqlite-stepping-error
    make-sqlite-stepping-error-condition
    sqlite-stepping-error-condition?
    raise-sqlite-stepping-error

    (rename (&sqlite-finalizing-error			&sqlite-finalising-error)
	    (make-sqlite-finalizing-error-condition	make-sqlite-finalising-error-condition)
	    (sqlite-finalizing-error-condition?		sqlite-finalising-error-condition?)
	    (raise-sqlite-finalizing-error		raise-sqlite-finalising-error))


    )
  (import (rnrs)
    (foreign databases sqlite primitives)
    (foreign databases sqlite enumerations)
    (foreign databases sqlite conditions)
    (foreign databases sqlite sizeof)))

;;; end of file
