;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/SQLite
;;;Contents: one-to-one-mapping interface
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


(library (foreign databases sqlite platform)
  (export

;;;Some of  the functions are  commented out because: they  are variadic
;;;functions.

    ;; version functions
    sqlite3_libversion
    sqlite3_sourceid
    sqlite3_libversion_number
    sqlite3_threadsafe

    ;; initialisation and configuration

    sqlite3_initialize
    sqlite3_shutdown
    sqlite3_os_init
    sqlite3_os_end
;;; sqlite3_config
    sqlite3_extended_result_codes

    ;; database connection handle

    sqlite3_close
    sqlite3_exec
;;; sqlite3_db_config

    ;; database interaction
    sqlite3_open
    sqlite3_open16
    sqlite3_open_v2
    sqlite3_last_insert_rowid
    sqlite3_changes
    sqlite3_total_changes
    sqlite3_interrupt
    sqlite3_complete
    sqlite3_complete16
    sqlite3_busy_handler
    sqlite3_busy_timeout
    sqlite3_get_table
    sqlite3_free_table
    sqlite3_progress_handler

    ;; SQL stuff
    sqlite3_prepare
    sqlite3_prepare_v2
    sqlite3_prepare16
    sqlite3_prepare16_v2
    sqlite3_sql
    sqlite3_finalize
    sqlite3_reset
    sqlite3_db_handle
    sqlite3_next_stmt
    sqlite3_table_column_metadata

    sqlite3_bind_parameter_count
    sqlite3_bind_parameter_name
    sqlite3_bind_parameter_index
    sqlite3_clear_bindings
    sqlite3_column_count
    sqlite3_column_name
    sqlite3_column_name16

    sqlite3_column_database_name
    sqlite3_column_database_name16
    sqlite3_column_table_name
    sqlite3_column_table_name16
    sqlite3_column_origin_name
    sqlite3_column_origin_name16

    sqlite3_column_decltype
    sqlite3_column_decltype16
    sqlite3_step
    sqlite3_data_count

    sqlite3_column_blob
    sqlite3_column_bytes
    sqlite3_column_bytes16
    sqlite3_column_double
    sqlite3_column_int
    sqlite3_column_int64
    sqlite3_column_text
    sqlite3_column_text16
    sqlite3_column_type
    sqlite3_column_value

    sqlite3_create_function
    sqlite3_create_function16

    sqlite3_value_blob
    sqlite3_value_bytes
    sqlite3_value_bytes16
    sqlite3_value_double
    sqlite3_value_int
    sqlite3_value_int64
    sqlite3_value_text
    sqlite3_value_text16
    sqlite3_value_text16le
    sqlite3_value_text16be
    sqlite3_value_type
    sqlite3_value_numeric_type

    ;; stuff for SQL functions
    sqlite3_aggregate_context
    sqlite3_user_data
    sqlite3_context_db_handle

    sqlite3_get_auxdata
    sqlite3_set_auxdata

    sqlite3_result_blob
    sqlite3_result_double
    sqlite3_result_error
    sqlite3_result_error16
    sqlite3_result_error_toobig
    sqlite3_result_error_nomem
    sqlite3_result_error_code
    sqlite3_result_int
    sqlite3_result_int64
    sqlite3_result_null
    sqlite3_result_text
    sqlite3_result_text16
    sqlite3_result_text16le
    sqlite3_result_text16be
    sqlite3_result_value
    sqlite3_result_zeroblob

    ;; collations
    sqlite3_create_collation
    sqlite3_create_collation_v2
    sqlite3_create_collation16

    sqlite3_collation_needed
    sqlite3_collation_needed16

    ;; printing
;;; sqlite3_mprintf
;;; sqlite3_vmprintf
;;; sqlite3_snprintf

    ;; memory allocation
    sqlite3_malloc
    sqlite3_realloc
    sqlite3_free
    sqlite3_memory_used
    sqlite3_memory_highwater
    sqlite3_release_memory

    ;; commit
    sqlite3_get_autocommit
    sqlite3_commit_hook
    sqlite3_rollback_hook

    ;; extensions
    sqlite3_load_extension
    sqlite3_enable_load_extension
    sqlite3_auto_extension
    sqlite3_reset_auto_extension

    ;; blobs
    sqlite3_blob_open
    sqlite3_blob_close
    sqlite3_blob_bytes
    sqlite3_blob_read
    sqlite3_blob_write

    sqlite3_bind_blob
    sqlite3_bind_double
    sqlite3_bind_int
    sqlite3_bind_int64
    sqlite3_bind_null
    sqlite3_bind_text
    sqlite3_bind_text16
    sqlite3_bind_value
    sqlite3_bind_zeroblob

    ;; mutexes
    sqlite3_mutex_alloc
    sqlite3_mutex_free
    sqlite3_mutex_enter
    sqlite3_mutex_try
    sqlite3_mutex_leave
    sqlite3_mutex_held
    sqlite3_mutex_notheld
    sqlite3_db_mutex

    ;; online backup
    sqlite3_backup_init
    sqlite3_backup_step
    sqlite3_backup_finish
    sqlite3_backup_remaining
    sqlite3_backup_pagecount

    ;; miscellaneous
    sqlite3_randomness
    sqlite3_set_authorizer
    sqlite3_trace
    sqlite3_profile
    sqlite3_limit
    sqlite3_sleep
    sqlite3_update_hook
    sqlite3_enable_shared_cache
    sqlite3_soft_heap_limit
    sqlite3_file_control
;;; sqlite3_test_control
    sqlite3_status
    sqlite3_db_status
    sqlite3_stmt_status
    sqlite3_unlock_notify
    sqlite3_strnicmp

    sqlite3_errcode
    sqlite3_extended_errcode
    sqlite3_errmsg
    sqlite3_errmsg16

    sqlite3_vfs_find
    sqlite3_vfs_register
    sqlite3_vfs_unregister)
  (import (rnrs)
    (unimplemented)
    (foreign ffi)
    (foreign ffi sizeof)
    (foreign databases sqlite sizeof)
    (foreign databases sqlite shared-object))


(define-syntax define-c-function/feature
  (syntax-rules ()
    ((_ ?name ?feature (?ret-type ?foreign-name ?arg-types))
     (define ?name
       (if ?feature
	   (make-c-function* sqlite-shared-object ?ret-type ?foreign-name ?arg-types)
	 (lambda args
	   (raise-unimplemented-error (quote ?foreign-name)
				      "this feature is not available in the SQLite library")))))))


;;;; data type aliases

(define char**			'pointer)
(define char***			'pointer)
(define int*			'pointer)
(define void**			'pointer)

(define sqlite3*		'pointer)
(define sqlite3**		'pointer)
(define sqlite3_backup		'pointer)
(define sqlite3_backup*		'pointer)
(define sqlite3_blob*		'pointer)
(define sqlite3_blob**		'pointer)
(define sqlite3_context*	'pointer)
(define sqlite3_file*		'pointer)
(define sqlite3_mutex*		'pointer)
(define sqlite3_pcache*		'pointer)
(define sqlite3_stmt*		'pointer)
(define sqlite3_stmt**		'pointer)
(define sqlite3_value*		'pointer)
(define sqlite3_vfs*		'pointer)


;;;; version functions

(define-c-functions sqlite-shared-object
  (sqlite3_libversion
   (char* sqlite3_libversion (void)))

  (sqlite3_sourceid
   (char* sqlite3_sourceid (void)))

  (sqlite3_libversion_number
   (int sqlite3_libversion_number (void)))

  (sqlite3_threadsafe
   (int sqlite3_threadsafe (void))))


;;;; initialisation and configuration

(define-c-functions sqlite-shared-object
  (sqlite3_initialize
   (int sqlite3_initialize (void)))

  (sqlite3_shutdown
   (int sqlite3_shutdown (void)))

  (sqlite3_os_init
   (int sqlite3_os_init (void)))

  (sqlite3_os_end
   (int sqlite3_os_end (void)))

  ;; This is a variadic function.
  ;;
  ;;   (sqlite3_config
  ;;   (int sqlite3_config (int ...)))

  (sqlite3_extended_result_codes
   (int sqlite3_extended_result_codes (sqlite3* int))))


;;;; database connection handle

(define-c-functions sqlite-shared-object
  (sqlite3_close
   (int sqlite3_close (sqlite3*)))

  (sqlite3_exec
   (int sqlite3_exec (sqlite3* char* callback void* char**)))

  ;; This is a variadic function.
  ;;
  ;;   (sqlite3_db_config
  ;;   (int sqlite3_db_config (sqlite3* int ...)))
  )


;;;; database interaction

(define-c-functions sqlite-shared-object
  (sqlite3_open
   (int sqlite3_open (char* sqlite3**)))

  (sqlite3_open16
   (int sqlite3_open16 (void* sqlite3**)))

  (sqlite3_open_v2
   (int sqlite3_open_v2 (char* sqlite3** int char*)))

  (sqlite3_last_insert_rowid
   (sqlite3_int64 sqlite3_last_insert_rowid (sqlite3*)))

  (sqlite3_changes
   (int sqlite3_changes (sqlite3*)))

  (sqlite3_total_changes
   (int sqlite3_total_changes (sqlite3*)))

  (sqlite3_interrupt
   (void sqlite3_interrupt (sqlite3*)))

  (sqlite3_complete
   (int sqlite3_complete (char*)))

  (sqlite3_complete16
   (int sqlite3_complete16 (char*)))

  (sqlite3_busy_handler
   (int sqlite3_busy_handler (sqlite3* callback void*)))

  (sqlite3_busy_timeout
   (int sqlite3_busy_timeout (sqlite3* int)))

  (sqlite3_get_table
   (int sqlite3_get_table (sqlite3* char* char*** int* int* char**)))

  (sqlite3_free_table
   (void sqlite3_free_table (char**)))

  (sqlite3_progress_handler
   (void sqlite3_progress_handler (sqlite3* int callback void*))))


;;;; SQL stuff

(define-c-functions sqlite-shared-object
  (sqlite3_prepare
   (int sqlite3_prepare (sqlite3* char* int sqlite3_stmt** char**)))

  (sqlite3_prepare_v2
   (int sqlite3_prepare_v2 (sqlite3* char* int sqlite3_stmt** char**)))

  (sqlite3_prepare16
   (int sqlite3_prepare16 (sqlite3* void* int sqlite3_stmt** void**)))

  (sqlite3_prepare16_v2
   (int sqlite3_prepare16_v2 (sqlite3* void* int sqlite3_stmt** void**)))

  (sqlite3_sql
   (char* sqlite3_sql (sqlite3_stmt*)))

  (sqlite3_finalize
   (int sqlite3_finalize (sqlite3_stmt*)))

  (sqlite3_reset
   (int sqlite3_reset (sqlite3_stmt*)))

  (sqlite3_db_handle
   (sqlite3* sqlite3_db_handle (sqlite3_stmt*)))

  (sqlite3_next_stmt
   (sqlite3_stmt* sqlite3_next_stmt (sqlite3* sqlite3_stmt*)))

;;; --------------------------------------------------------------------

  (sqlite3_bind_parameter_count
   (int sqlite3_bind_parameter_count (sqlite3_stmt*)))

  (sqlite3_bind_parameter_name
   (char* sqlite3_bind_parameter_name (sqlite3_stmt* int)))

  (sqlite3_bind_parameter_index
   (int sqlite3_bind_parameter_index (sqlite3_stmt* char*)))

  (sqlite3_clear_bindings
   (int sqlite3_clear_bindings(sqlite3_stmt*)))

  (sqlite3_column_count
   (int sqlite3_column_count (sqlite3_stmt*)))

  (sqlite3_column_name
   (char* sqlite3_column_name (sqlite3_stmt* int)))

  (sqlite3_column_name16
   (void* sqlite3_column_name16 (sqlite3_stmt* int)))

;;; --------------------------------------------------------------------

  (sqlite3_column_decltype
   (char* sqlite3_column_decltype (sqlite3_stmt* int)))

  (sqlite3_column_decltype16
   (void* sqlite3_column_decltype16 (sqlite3_stmt* int)))

  (sqlite3_step
   (int sqlite3_step (sqlite3_stmt*)))

  (sqlite3_data_count
   (int sqlite3_data_count (sqlite3_stmt*)))

;;; --------------------------------------------------------------------

  (sqlite3_column_blob
   (void* sqlite3_column_blob (sqlite3_stmt* int)))

  (sqlite3_column_bytes
   (int sqlite3_column_bytes (sqlite3_stmt* int)))

  (sqlite3_column_bytes16
   (int sqlite3_column_bytes16 (sqlite3_stmt* int)))

  (sqlite3_column_double
   (double sqlite3_column_double (sqlite3_stmt* int)))

  (sqlite3_column_int
   (int sqlite3_column_int (sqlite3_stmt* int)))

  (sqlite3_column_int64
   (sqlite3_int64 sqlite3_column_int64 (sqlite3_stmt* int)))

  (sqlite3_column_text
   (void* sqlite3_column_text (sqlite3_stmt* int)))

  (sqlite3_column_text16
   (void* sqlite3_column_text16 (sqlite3_stmt* int)))

  (sqlite3_column_type
   (int sqlite3_column_type (sqlite3_stmt* int)))

  (sqlite3_column_value
   (sqlite3_value* sqlite3_column_value (sqlite3_stmt* int)))

;;; --------------------------------------------------------------------

  (sqlite3_create_function
   (int sqlite3_create_function (sqlite3* char* int int void* callback callback callback)))

  (sqlite3_create_function16
   (int sqlite3_create_function16 (sqlite3* void* int int void* callback callback callback)))

;;; --------------------------------------------------------------------

  (sqlite3_value_blob
   (void* sqlite3_value_blob (sqlite3_value*)))

  (sqlite3_value_bytes
   (int sqlite3_value_bytes (sqlite3_value*)))

  (sqlite3_value_bytes16
   (int sqlite3_value_bytes16 (sqlite3_value*)))

  (sqlite3_value_double
   (double sqlite3_value_double (sqlite3_value*)))

  (sqlite3_value_int
   (int sqlite3_value_int (sqlite3_value*)))

  (sqlite3_value_int64
   (sqlite3_int64 sqlite3_value_int64 (sqlite3_value*)))

  (sqlite3_value_text
   (void* sqlite3_value_text (sqlite3_value*)))

  (sqlite3_value_text16
   (void* sqlite3_value_text16 (sqlite3_value*)))

  (sqlite3_value_text16le
   (void* sqlite3_value_text16le (sqlite3_value*)))

  (sqlite3_value_text16be
   (void* sqlite3_value_text16be (sqlite3_value*)))

  (sqlite3_value_type
   (int sqlite3_value_type (sqlite3_value*)))

  (sqlite3_value_numeric_type
   (int sqlite3_value_numeric_type (sqlite3_value*))))

;;; --------------------------------------------------------------------

(define-c-function/feature sqlite3_table_column_metadata SQLITE_ENABLE_COLUMN_METADATA
  (int sqlite3_table_column_metadata (sqlite3* char* char* char* char** char** int* int* int*)))

(define-c-function/feature sqlite3_column_database_name SQLITE_ENABLE_COLUMN_METADATA
  (char* sqlite3_column_database_name (sqlite3_stmt* int)))

(define-c-function/feature sqlite3_column_database_name16 SQLITE_ENABLE_COLUMN_METADATA
  (void* sqlite3_column_database_name16 (sqlite3_stmt* int)))

(define-c-function/feature sqlite3_column_table_name SQLITE_ENABLE_COLUMN_METADATA
  (char* sqlite3_column_table_name (sqlite3_stmt* int)))

(define-c-function/feature sqlite3_column_table_name16 SQLITE_ENABLE_COLUMN_METADATA
  (void* sqlite3_column_table_name16 (sqlite3_stmt* int)))

(define-c-function/feature sqlite3_column_origin_name SQLITE_ENABLE_COLUMN_METADATA
  (char* sqlite3_column_origin_name (sqlite3_stmt* int)))

(define-c-function/feature sqlite3_column_origin_name16 SQLITE_ENABLE_COLUMN_METADATA
  (void* sqlite3_column_origin_name16 (sqlite3_stmt* int)))


;;;; stuff for SQL functions

(define-c-functions sqlite-shared-object

  (sqlite3_aggregate_context
   (void* sqlite3_aggregate_context (sqlite3_context* int)))

  (sqlite3_user_data
   (void* sqlite3_user_data (sqlite3_context*)))

  (sqlite3_context_db_handle
   (sqlite3* sqlite3_context_db_handle (sqlite3_context*)))


  (sqlite3_get_auxdata
   (void* sqlite3_get_auxdata (sqlite3_context* int)))

  (sqlite3_set_auxdata
   (void sqlite3_set_auxdata (sqlite3_context* int void* callback)))

;;; --------------------------------------------------------------------

  (sqlite3_result_blob
   (void sqlite3_result_blob (sqlite3_context* void* int callback)))

  (sqlite3_result_double
   (void sqlite3_result_double (sqlite3_context* double)))

  (sqlite3_result_error
   (void sqlite3_result_error (sqlite3_context* char* int)))

  (sqlite3_result_error16
   (void sqlite3_result_error16 (sqlite3_context* void* int)))

  (sqlite3_result_error_toobig
   (void sqlite3_result_error_toobig (sqlite3_context*)))

  (sqlite3_result_error_nomem
   (void sqlite3_result_error_nomem (sqlite3_context*)))

  (sqlite3_result_error_code
   (void sqlite3_result_error_code (sqlite3_context* int)))

  (sqlite3_result_int
   (void sqlite3_result_int (sqlite3_context* int)))

  (sqlite3_result_int64
   (void sqlite3_result_int64 (sqlite3_context* sqlite3_int64)))

  (sqlite3_result_null
   (void sqlite3_result_null (sqlite3_context*)))

  (sqlite3_result_text
   (void sqlite3_result_text (sqlite3_context* char* int callback)))

  (sqlite3_result_text16
   (void sqlite3_result_text16 (sqlite3_context* void* int callback)))

  (sqlite3_result_text16le
   (void sqlite3_result_text16le (sqlite3_context* void* int callback)))

  (sqlite3_result_text16be
   (void sqlite3_result_text16be (sqlite3_context* void* int callback)))

  (sqlite3_result_value
   (void sqlite3_result_value (sqlite3_context* sqlite3_value*)))

  (sqlite3_result_zeroblob
   (void sqlite3_result_zeroblob (sqlite3_context* int))))


;;;; collations

(define-c-functions sqlite-shared-object

  (sqlite3_create_collation
   (int sqlite3_create_collation (sqlite3* char* int void* callback)))

  (sqlite3_create_collation_v2
   (int sqlite3_create_collation_v2 (sqlite3* char* int void* callback callback)))

  (sqlite3_create_collation16
   (int sqlite3_create_collation16 (sqlite3* void* int void* callback)))


  (sqlite3_collation_needed
   (int sqlite3_collation_needed (sqlite3* void* callback)))

  (sqlite3_collation_needed16
   (int sqlite3_collation_needed16 (sqlite3* void* callback))))


;;;; printing

;;;These are variadic functions.
;;
;; (define-c-function sqlite3_mprintf
;;   (char* sqlite3_mprintf (char* ...)))
;;
;; (define-c-function sqlite3_vmprintf
;;   (char* sqlite3_vmprintf (char* va_list)))
;;
;; (define-c-function sqlite3_snprintf
;;   (char* sqlite3_snprintf (int char* char* ...)))


;;;; memory allocation

(define-c-functions sqlite-shared-object

  (sqlite3_malloc
   (void* sqlite3_malloc (int)))

  (sqlite3_realloc
   (void* sqlite3_realloc (void* int)))

  (sqlite3_free
   (void sqlite3_free (void*)))

  (sqlite3_memory_used
   (sqlite3_int64 sqlite3_memory_used (void)))

  (sqlite3_memory_highwater
   (sqlite3_int64 sqlite3_memory_highwater (int)))

  (sqlite3_release_memory
   (int sqlite3_release_memory (int))))


;;;; commit

(define-c-functions sqlite-shared-object

  (sqlite3_get_autocommit
   (int sqlite3_get_autocommit (sqlite3*)))

  (sqlite3_commit_hook
   (void* sqlite3_commit_hook (sqlite3* callback)))

  (sqlite3_rollback_hook
   (void* sqlite3_rollback_hook (sqlite3* callback))))


;;;; extensions

(define-c-functions sqlite-shared-object

  (sqlite3_load_extension
   (int sqlite3_load_extension (sqlite3* char* char* char**)))

  (sqlite3_enable_load_extension
   (int sqlite3_enable_load_extension (sqlite3* int)))

  (sqlite3_auto_extension
   (int sqlite3_auto_extension (callback)))

  (sqlite3_reset_auto_extension
   (void sqlite3_reset_auto_extension (void))))


;;;; blobs

(define-c-functions sqlite-shared-object

  (sqlite3_blob_open
   (int sqlite3_blob_open (sqlite3* char* char* char* sqlite3_int64 int sqlite3_blob**)))

  (sqlite3_blob_close
   (int sqlite3_blob_close (sqlite3_blob*)))

  (sqlite3_blob_bytes
   (int sqlite3_blob_bytes (sqlite3_blob*)))

  (sqlite3_blob_read
   (int sqlite3_blob_read (sqlite3_blob* void* int int)))

  (sqlite3_blob_write
   (int sqlite3_blob_write (sqlite3_blob* void* int int)))

;;; --------------------------------------------------------------------

  (sqlite3_bind_blob
   (int sqlite3_bind_blob (sqlite3_stmt* int void* int callback)))

  (sqlite3_bind_double
   (int sqlite3_bind_double (sqlite3_stmt* int double)))

  (sqlite3_bind_int
   (int sqlite3_bind_int (sqlite3_stmt* int int)))

  (sqlite3_bind_int64
   (int sqlite3_bind_int64 (sqlite3_stmt* int sqlite3_int64)))

  (sqlite3_bind_null
   (int sqlite3_bind_null (sqlite3_stmt* int)))

  (sqlite3_bind_text
   (int sqlite3_bind_text (sqlite3_stmt* int char* int callback)))

  (sqlite3_bind_text16
   (int sqlite3_bind_text16 (sqlite3_stmt* int void* int callback)))

  (sqlite3_bind_value
   (int sqlite3_bind_value (sqlite3_stmt* int sqlite3_value*)))

  (sqlite3_bind_zeroblob
   (int sqlite3_bind_zeroblob (sqlite3_stmt* int int))))


;;;; mutexes

(define-c-functions sqlite-shared-object

  (sqlite3_mutex_alloc
   (sqlite3_mutex* sqlite3_mutex_alloc (int)))

  (sqlite3_mutex_free
   (void sqlite3_mutex_free (sqlite3_mutex*)))

  (sqlite3_mutex_enter
   (void sqlite3_mutex_enter (sqlite3_mutex*)))

  (sqlite3_mutex_try
   (int sqlite3_mutex_try (sqlite3_mutex*)))

  (sqlite3_mutex_leave
   (void sqlite3_mutex_leave (sqlite3_mutex*)))

  (sqlite3_db_mutex
   (sqlite3_mutex* sqlite3_db_mutex (sqlite3*))))

;;; --------------------------------------------------------------------

(define-c-function/feature sqlite3_mutex_held SQLITE_DEBUG
  (int sqlite3_mutex_held (sqlite3_mutex*)))

(define-c-function/feature sqlite3_mutex_notheld SQLITE_DEBUG
  (int sqlite3_mutex_notheld (sqlite3_mutex*)))


;;;; online backup

(define-c-functions sqlite-shared-object

  (sqlite3_backup_init
   (sqlite3_backup* sqlite3_backup_init (sqlite3* char* sqlite3* char*)))

  (sqlite3_backup_step
   (int sqlite3_backup_step (sqlite3_backup* int)))

  (sqlite3_backup_finish
   (int sqlite3_backup_finish (sqlite3_backup*)))

  (sqlite3_backup_remaining
   (int sqlite3_backup_remaining (sqlite3_backup*)))

  (sqlite3_backup_pagecount
   (int sqlite3_backup_pagecount (sqlite3_backup*))))


;;;; miscellaneous

(define-c-functions sqlite-shared-object

  (sqlite3_randomness
   (void sqlite3_randomness (int void*)))

  (sqlite3_set_authorizer
   (int sqlite3_set_authorizer (sqlite3* callback void*)))

  (sqlite3_trace
   (void* sqlite3_trace (sqlite3* callback void*)))

  (sqlite3_profile
   (void* sqlite3_profile (sqlite3* callback void*)))

  (sqlite3_limit
   (int sqlite3_limit (sqlite3* int int)))

  (sqlite3_sleep
   (int sqlite3_sleep (int)))

  (sqlite3_update_hook
   (void* sqlite3_update_hook (sqlite3* callback void*)))

  (sqlite3_enable_shared_cache
   (int sqlite3_enable_shared_cache (int)))

  (sqlite3_soft_heap_limit
   (void sqlite3_soft_heap_limit (int)))

  (sqlite3_file_control
   (int sqlite3_file_control (sqlite3* char* int void*)))

;;;This is a variadic function and not for public use.
  ;;
  ;;   (sqlite3_test_control
  ;;   (int sqlite3_test_control (int ...)))

  (sqlite3_status
   (int sqlite3_status (int int* int* int)))

  (sqlite3_db_status
   (int sqlite3_db_status (sqlite3* int int* int* int)))

  (sqlite3_stmt_status
   (int sqlite3_stmt_status (sqlite3_stmt* int int)))

  (sqlite3_strnicmp
   (int sqlite3_strnicmp (char* char* int)))

;;; --------------------------------------------------------------------

  (sqlite3_errcode
   (int sqlite3_errcode (sqlite3*)))

  (sqlite3_extended_errcode
   (int sqlite3_extended_errcode (sqlite3*)))

  (sqlite3_errmsg
   (char* sqlite3_errmsg (sqlite3*)))

  (sqlite3_errmsg16
   (void* sqlite3_errmsg16 (sqlite3*)))

;;; --------------------------------------------------------------------

  (sqlite3_vfs_find
   (sqlite3_vfs* sqlite3_vfs_find (char*)))

  (sqlite3_vfs_register
   (int sqlite3_vfs_register (sqlite3_vfs* int)))

  (sqlite3_vfs_unregister
   (int sqlite3_vfs_unregister (sqlite3_vfs*))))

;;; --------------------------------------------------------------------

(define-c-function/feature sqlite3_unlock_notify SQLITE_ENABLE_UNLOCK_NOTIFY
  (int sqlite3_unlock_notify (sqlite3* callback void*)))



;;;; done

)

;;; end of file
