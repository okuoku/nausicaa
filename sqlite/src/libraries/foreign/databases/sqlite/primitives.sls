;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/SQLite
;;;Contents: values marshaling interface
;;;Date: Thu Oct 29, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (foreign databases sqlite primitives)
  (export

;;;The  renamed  bindings  commented  out  are the  ones  wrapped  by  a
;;;marshaling function in this library.

    (rename
     ;; version functions
     (sqlite3_libversion		sqlite-libversion)
     (sqlite3_sourceid			sqlite-sourceid)
     (sqlite3_libversion_number		sqlite-libversion-number)
     (sqlite3_threadsafe		sqlite-threadsafe)

     ;; initialisation and configuration

     (sqlite3_initialize		sqlite-initialize)
     (sqlite3_shutdown			sqlite-shutdown)
     (sqlite3_os_init			sqlite-os-init)
     (sqlite3_os_end			sqlite-os-end)
     (sqlite3_extended_result_codes	sqlite-extended-result-codes)

     ;; database connection handle

     (sqlite3_close			sqlite-close)
;;;     (sqlite3_exec			sqlite-exec)

     ;; database interaction
;;;     (sqlite3_open			sqlite-open)
     (sqlite3_open16			sqlite-open16)
;;;     (sqlite3_open_v2		sqlite-open-v2)
     (sqlite3_last_insert_rowid		sqlite-last-insert-rowid)
     (sqlite3_changes			sqlite-changes)
     (sqlite3_total_changes		sqlite-total-changes)
     (sqlite3_interrupt			sqlite-interrupt)
     (sqlite3_complete			sqlite-complete)
     (sqlite3_complete16		sqlite-complete16)
     (sqlite3_busy_handler		sqlite-busy-handler)
     (sqlite3_busy_timeout		sqlite-busy-timeout)
;;;     (sqlite3_get_table		sqlite-get-table)
     (sqlite3_free_table		sqlite-free-table)
     (sqlite3_progress_handler		sqlite-progress-handler)

     ;; SQL stuff
     (sqlite3_prepare			sqlite-prepare)
;;;     (sqlite3_prepare_v2		sqlite-prepare-v2)
     (sqlite3_prepare16			sqlite-prepare16)
     (sqlite3_prepare16_v2		sqlite-prepare16-v2)
     (sqlite3_sql			sqlite-sql)
;;;     (sqlite3_finalize		sqlite-finalize)
;;;     (sqlite3_reset			sqlite-reset)
     (sqlite3_db_handle			sqlite-db-handle)
     (sqlite3_next_stmt			sqlite-next-stmt)
     (sqlite3_table_column_metadata	sqlite-table-column-metadata)

     (sqlite3_bind_parameter_count	sqlite-bind-parameter-count)
     (sqlite3_bind_parameter_name	sqlite-bind-parameter-name)
     (sqlite3_bind_parameter_index	sqlite-bind-parameter-index)
     (sqlite3_clear_bindings		sqlite-clear-bindings)
     (sqlite3_column_count		sqlite-column-count)
;;;     (sqlite3_column_name		sqlite-column-name)
     (sqlite3_column_name16		sqlite-column-name16)

     (sqlite3_column_database_name	sqlite-column-database-name)
     (sqlite3_column_database_name16	sqlite-column-database-name16)
     (sqlite3_column_table_name		sqlite-column-table-name)
     (sqlite3_column_table_name16	sqlite-column-table-name16)
     (sqlite3_column_origin_name	sqlite-column-origin-name)
     (sqlite3_column_origin_name16	sqlite-column-origin-name16)

     (sqlite3_column_decltype		sqlite-column-decltype)
     (sqlite3_column_decltype16		sqlite-column-decltype16)
;;;     (sqlite3_step			sqlite-step)
     (sqlite3_data_count		sqlite-data-count)

     (sqlite3_column_blob		sqlite-column-blob)
     (sqlite3_column_bytes		sqlite-column-bytes)
     (sqlite3_column_bytes16		sqlite-column-bytes16)
     (sqlite3_column_double		sqlite-column-double)
     (sqlite3_column_int		sqlite-column-int)
     (sqlite3_column_int64		sqlite-column-int64)
;;;     (sqlite3_column_text		sqlite-column-text)
     (sqlite3_column_text16		sqlite-column-text16)
     (sqlite3_column_type		sqlite-column-type)
     (sqlite3_column_value		sqlite-column-value)

     (sqlite3_create_function		sqlite-create-function)
     (sqlite3_create_function16		sqlite-create-function16)

     (sqlite3_value_blob		sqlite-value-blob)
     (sqlite3_value_bytes		sqlite-value-bytes)
     (sqlite3_value_bytes16		sqlite-value-bytes16)
     (sqlite3_value_double		sqlite-value-double)
     (sqlite3_value_int			sqlite-value-int)
     (sqlite3_value_int64		sqlite-value-int64)
     (sqlite3_value_text		sqlite-value-text)
     (sqlite3_value_text16		sqlite-value-text16)
     (sqlite3_value_text16le		sqlite-value-text16le)
     (sqlite3_value_text16be		sqlite-value-text16be)
     (sqlite3_value_type		sqlite-value-type)
     (sqlite3_value_numeric_type	sqlite-value-numeric-type)

     ;; stuff for SQL functions
     (sqlite3_aggregate_context		sqlite-aggregate-context)
     (sqlite3_user_data			sqlite-user-data)
     (sqlite3_context_db_handle		sqlite-context-db-handle)

     (sqlite3_get_auxdata		sqlite-get-auxdata)
     (sqlite3_set_auxdata		sqlite-set-auxdata)

     (sqlite3_result_blob		sqlite-result-blob)
     (sqlite3_result_double		sqlite-result-double)
     (sqlite3_result_error		sqlite-result-error)
     (sqlite3_result_error16		sqlite-result-error16)
     (sqlite3_result_error_toobig	sqlite-result-error-toobig)
     (sqlite3_result_error_nomem	sqlite-result-error-nomem)
     (sqlite3_result_error_code		sqlite-result-error-code)
     (sqlite3_result_int		sqlite-result-int)
     (sqlite3_result_int64		sqlite-result-int64)
     (sqlite3_result_null		sqlite-result-null)
     (sqlite3_result_text		sqlite-result-text)
     (sqlite3_result_text16		sqlite-result-text16)
     (sqlite3_result_text16le		sqlite-result-text16le)
     (sqlite3_result_text16be		sqlite-result-text16be)
     (sqlite3_result_value		sqlite-result-value)
     (sqlite3_result_zeroblob		sqlite-result-zeroblob)

     ;; collations
     (sqlite3_create_collation		sqlite-create-collation)
     (sqlite3_create_collation_v2	sqlite-create-collation-v2)
     (sqlite3_create_collation16	sqlite-create-collation16)

     (sqlite3_collation_needed		sqlite-collation-needed)
     (sqlite3_collation_needed16	sqlite-collation-needed16)

     ;; memory allocation
     (sqlite3_malloc			sqlite-malloc)
     (sqlite3_realloc			sqlite-realloc)
     (sqlite3_free			sqlite-free)
     (sqlite3_memory_used		sqlite-memory-used)
     (sqlite3_memory_highwater		sqlite-memory-highwater)
     (sqlite3_release_memory		sqlite-release-memory)

     ;; commit
     (sqlite3_get_autocommit		sqlite-get-autocommit)
     (sqlite3_commit_hook		sqlite-commit-hook)
     (sqlite3_rollback_hook		sqlite-rollback-hook)

     ;; extensions
     (sqlite3_load_extension		sqlite-load-extension)
     (sqlite3_enable_load_extension	sqlite-enable-load-extension)
     (sqlite3_auto_extension		sqlite-auto-extension)
     (sqlite3_reset_auto_extension	sqlite-reset-auto-extension)

     ;; blobs
     (sqlite3_blob_open			sqlite-blob-open)
     (sqlite3_blob_close		sqlite-blob-close)
     (sqlite3_blob_bytes		sqlite-blob-bytes)
     (sqlite3_blob_read			sqlite-blob-read)
     (sqlite3_blob_write		sqlite-blob-write)

     (sqlite3_bind_blob			sqlite-bind-blob)
     (sqlite3_bind_double		sqlite-bind-double)
     (sqlite3_bind_int			sqlite-bind-int)
     (sqlite3_bind_int64		sqlite-bind-int64)
     (sqlite3_bind_null			sqlite-bind-null)
     (sqlite3_bind_text			sqlite-bind-text)
     (sqlite3_bind_text16		sqlite-bind-text16)
     (sqlite3_bind_value		sqlite-bind-value)
     (sqlite3_bind_zeroblob		sqlite-bind-zeroblob)

     ;; mutexes
     (sqlite3_mutex_alloc		sqlite-mutex-alloc)
     (sqlite3_mutex_free		sqlite-mutex-free)
     (sqlite3_mutex_enter		sqlite-mutex-enter)
     (sqlite3_mutex_try			sqlite-mutex-try)
     (sqlite3_mutex_leave		sqlite-mutex-leave)
     (sqlite3_mutex_held		sqlite-mutex-held)
     (sqlite3_mutex_notheld		sqlite-mutex-notheld)
     (sqlite3_db_mutex			sqlite-db-mutex)

     ;; online backup
     (sqlite3_backup_init		sqlite-backup-init)
     (sqlite3_backup_step		sqlite-backup-step)
     (sqlite3_backup_finish		sqlite-backup-finish)
     (sqlite3_backup_remaining		sqlite-backup-remaining)
     (sqlite3_backup_pagecount		sqlite-backup-pagecount)

     ;; miscellaneous
     (sqlite3_randomness		sqlite-randomness)
     (sqlite3_set_authorizer		sqlite-set-authorizer)
     (sqlite3_trace			sqlite-trace)
     (sqlite3_profile			sqlite-profile)
     (sqlite3_limit			sqlite-limit)
     (sqlite3_sleep			sqlite-sleep)
     (sqlite3_update_hook		sqlite-update_hook)
     (sqlite3_enable_shared_cache	sqlite-enable-shared-cache)
     (sqlite3_soft_heap_limit		sqlite-soft-heap-limit)
     (sqlite3_file_control		sqlite-file-control)
     (sqlite3_status			sqlite-status)
     (sqlite3_db_status			sqlite-db-status)
     (sqlite3_stmt_status		sqlite-stmt-status)
     (sqlite3_unlock_notify		sqlite-unlock-notify)
     (sqlite3_strnicmp			sqlite-strnicmp)

     (sqlite3_errcode			sqlite-errcode)
     (sqlite3_extended_errcode		sqlite-extended-errcode)
     (sqlite3_errmsg			sqlite-errmsg)
     (sqlite3_errmsg16			sqlite-errmsg16)

     (sqlite3_vfs_find			sqlite-vfs-find)
     (sqlite3_vfs_register		sqlite-vfs-register)
     (sqlite3_vfs_unregister		sqlite-vfs-unregister))

;;; --------------------------------------------------------------------
;;; marshaling functions and helpers

    sqlite-open				sqlite-open-v2
    sqlite-exec				sqlite-get-table
    sqlite-prepare-v2			sqlite-finalize
    sqlite-step				sqlite-reset

    sqlite-column-name			sqlite-column-text

    (rename (sqlite-finalize		sqlite-finalise)))
  (import (rnrs)
    (language-extensions)
    (compensations)
    (foreign ffi)
    (foreign memory)
    (foreign cstrings)
    (foreign databases sqlite conditions)
    (foreign databases sqlite enumerations)
    (foreign databases sqlite platform)
    (foreign databases sqlite sizeof))


;;;; helpers

(define char**		'pointer)


(define-syntax with-sqlite-locking
  (syntax-rules ()
    ((_ ?session ?form0 ?form ...)
     (let ((mux (sqlite3_db_mutex ?session)))
       (dynamic-wind
	   (lambda ()
	     (sqlite3_mutex_enter mux))
	   (lambda ()
	     ?form0 ?form ...)
	   (lambda ()
	     (sqlite3_mutex_leave mux)))))))


(define (sqlite-open database)
  (with-compensations
    (let* ((session*	(malloc-small/c))
	   (code	(sqlite3_open (string->cstring/c database) session*))
	   (session	(pointer-ref-c-pointer session* 0)))
      (cond ((pointer-null? session)
	     (raise-continuable
	      (condition (make-sqlite-opening-error-condition code)
			 (make-out-of-memory-condition #f)
			 (make-sqlite-database-condition database)
			 (make-who-condition 'sqlite-open)
			 (make-message-condition "not enough memory to open SQLite database"))))
	    ((= 0 code)
	     session)
	    (else
	     ;;According  to SQLite  documentation,  memory returned  by
	     ;;"sqlite_errmsg()" is  managed internally, we  do not need
	     ;;to release it.
	     (let ((errmsg (cstring->string (sqlite3_errmsg session))))
	       ;;Yes,   we   have  to   close   the   session  even   if
	       ;;opening/creating the database failed.
	       (sqlite3_close session)
	       (raise-sqlite-opening-error 'sqlite-open errmsg code database)))))))

(define sqlite-open-v2
  (case-lambda
   ((database flags)
    (sqlite-open-v2 database flags #f))
   ((database flags name-of-vfs-module)
    (with-compensations
      (let* ((session*	(malloc-small/c))
	     (code	(sqlite3_open_v2 (string->cstring/c database)
					 session*
					 (%sqlite-open-enum->flags flags)
					 (if name-of-vfs-module
					     (string->cstring/c name-of-vfs-module)
					   pointer-null)))
	     (session	(pointer-ref-c-pointer session* 0)))
	(cond ((pointer-null? session)
	     (raise-continuable
	      (condition (make-sqlite-opening-error-condition code)
			 (make-out-of-memory-condition #f)
			 (make-sqlite-database-condition database)
			 (make-who-condition 'sqlite-open-v2)
			 (make-message-condition "not enough memory to open SQLite database"))))
	      ((= 0 code)
	       session)
	      (else
	       ;;According  to SQLite  documentation,  memory returned  by
	       ;;"sqlite_errmsg()" is  managed internally, we  do not need
	       ;;to release it.
	       (let ((errmsg (cstring->string (sqlite3_errmsg session))))
		 ;;Yes,   we   have  to   close   the   session  even   if
		 ;;opening/creating the database failed.
		 (sqlite3_close session)
		 (raise-sqlite-opening-error 'sqlite-open-v2 errmsg code database)))))))))


(define sqlite-exec
  (case-lambda
   ((session query)
    (sqlite-exec session query #f))
   ((session query scheme-callback)
    (with-compensations
      (let ((column-names	'())
	    (condition-object	#f))

	(define (%cstring/null obj)
	  (if (pointer-null? obj)
	      #f
	    (cstring->string obj)))

	(define (%closure unused-custom-data column-number values** names**)
	  (guard (E (else (set! condition-object E)
			  SQLITE_ABORT))
	    (when (null? column-names)
	      (do ((i 0 (+ 1 i)))
		  ((= i column-number)
		   (set! column-names (reverse column-names)))
		(set-cons! column-names (cstring->string (array-ref-c-pointer names** i)))))
	    (let ((column-values '()))
	      (do ((i 0 (+ 1 i)))
		  ((= i column-number))
		(let ((val* (array-ref-c-pointer values** i)))
		  (set-cons! column-values (%cstring/null val*))))
	      (scheme-callback column-names (reverse column-values)))))

	(define callback
	  (if scheme-callback
	      (make-c-callback* int %closure (void* int char** char**))
	    pointer-null))

	(let* ((errmsg**	(malloc-small/c))
	       (code		(sqlite3_exec session (string->cstring/c query)
					      callback pointer-null errmsg**)))
	  (cond (condition-object
		 (raise condition-object))
		((= code SQLITE_OK)
		 code)
		(else
		 (let* ((errmsg* (pointer-ref-c-pointer errmsg** 0))
			(errmsg  (cstring->string       errmsg*)))
		   ;;According to  SQLite documentation, memory  for the
		   ;;error message of "sqlite3_exec()" is allocated with
		   ;;"sqlite3_malloc(), so we have to release it.
		   (sqlite3_free errmsg*)
		   (raise-sqlite-querying-error 'sqlite-exec errmsg code session query))))))))))


(define (sqlite-get-table session query)
  (with-compensations
    (let* ((table**	(malloc-small/c))
	   (rownum*	(malloc-small/c))
	   (colnum*	(malloc-small/c))
	   (errmsg**	(malloc-small/c))
	   (code	(sqlite3_get_table session (string->cstring/c query)
					   table** rownum* colnum* errmsg**)))
      (if (= code SQLITE_OK)
	  (let ((table*	(pointer-ref-c-pointer table** 0))
		;;The reported rownum does NOT include the row of column
		;;names, so we add one here.
		(rownum	(+ 1 (pointer-ref-c-signed-int rownum* 0)))
		(colnum	(pointer-ref-c-signed-int colnum* 0)))
	    (do ((i 0 (+ 1 i))
		 (table	'()))
		((= i rownum)
		 (sqlite3_free_table table*)
		 (reverse table))
	      (set-cons! table
			 (do ((j 0 (+ 1 j))
			      (row '()))
			     ((= j colnum)
			      (reverse row))
			   (set-cons! row (cstring->string
					   (array-ref-c-pointer table* (+ j (* i colnum)))))))))
	(let* ((errmsg* (pointer-ref-c-pointer errmsg** 0))
	       (errmsg  (cstring->string       errmsg*)))
	  ;;According to  SQLite documentation, memory  for the error
	  ;;message    of   "sqlite3_exec()"   is    allocated   with
	  ;;"sqlite3_malloc(), so we have to release it.
	  (sqlite3_free errmsg*)
	  (raise-sqlite-querying-error 'sqlite-exec errmsg code session query))))))


(define (sqlite-prepare-v2 session query)
  (with-compensations
    (let* ((statement**	(malloc-small/c))
	   (next**	(malloc-small/c))
	   (sql*	(string->cstring/c query))
	   (end*	(pointer-add sql* (strlen sql*)))
	   (statements	'()))
      (pointer-set-c-pointer! next** 0 pointer-null)
      (let loop ()
	(let* ((code	(sqlite3_prepare_v2 session sql* -1 statement** next**))
	       (next*	(pointer-ref-c-pointer next** 0)))
	  (if (= code SQLITE_OK)
	      (let ((statement* (pointer-ref-c-pointer statement** 0)))
		(unless (pointer-null? statement*)
		  (set-cons! statements statement*))
		(if (pointer=? next* end*)
		    (reverse statements)
		  (begin
		    (set! sql* next*)
		    (loop))))
	    ;;According  to  SQLite  documentation, memory  returned  by
	    ;;"sqlite_errmsg()" is managed internally, we do not need to
	    ;;release it.
	    (let ((errmsg (cstring->string (sqlite3_errmsg session))))
	      (for-each sqlite3_finalize statements)
	      (raise-sqlite-preparing-error 'sqlite-prepare-v2 errmsg code session
					    (cstring->string sql* (pointer-diff next* sql*))))))))))

(define (sqlite-step session statement)
  (let ((code (sqlite3_step statement)))
    (if (or (= code SQLITE_ROW)
	    (= code SQLITE_DONE))
	code
      (let ((errmsg (cstring->string (sqlite3_errmsg session))))
	(raise-sqlite-stepping-error 'sqlite-step errmsg code session statement)))))

(define (sqlite-finalize session statement)
  (let ((code (sqlite3_finalize statement)))
    (if (= code SQLITE_OK)
	code
      (let ((errmsg (cstring->string (sqlite3_errmsg session))))
	(raise-sqlite-finalizing-error 'sqlite-finalize errmsg code session statement)))))

(define (sqlite-reset session statement)
  (let ((code (sqlite3_reset statement)))
    (if (= code SQLITE_OK)
	code
      (let ((errmsg (cstring->string (sqlite3_errmsg session))))
	(raise-sqlite-finalizing-error 'sqlite-reset errmsg code session statement)))))


(define (sqlite-column-name statement column-index)
  (cstring->string (sqlite3_column_name statement column-index)))

(define (sqlite-column-text statement column-index)
  (cstring->string (sqlite3_column_text statement column-index)))


;;;; done

)

;;; end of file
