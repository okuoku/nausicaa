;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/SQLite
;;;Contents: enumeration definitions
;;;Date: Sat Nov 14, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
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


(library (foreign databases sqlite enumerations)
  (export
    sqlite-open-flag-symbol	sqlite-open-flags
    %sqlite-open-enum->flags)
  (import (rnrs)
    (foreign databases sqlite sizeof))


(define-enumeration sqlite-open-flag-symbol
  (CREATE
   DELETEONCLOSE
   EXCLUSIVE
   FULLMUTEX
   MAIN_DB
   MAIN_JOURNAL
   MASTER_JOURNAL
   NOMUTEX
   PRIVATECACHE
   READONLY
   READWRITE
   SHAREDCACHE
   SUBJOURNAL
   TEMP_DB
   TEMP_JOURNAL
   TRANSIENT_DB)
  sqlite-open-flags)

(define (%sqlite-open-enum->flags enum-set)
  (fold-left
   (lambda (knil item)
     (bitwise-ior knil
		  (case item
		    ((READONLY)		SQLITE_OPEN_READONLY)
		    ((READWRITE)	SQLITE_OPEN_READWRITE)
		    ((CREATE)		SQLITE_OPEN_CREATE)
		    ((DELETEONCLOSE)	SQLITE_OPEN_DELETEONCLOSE)
		    ((EXCLUSIVE)	SQLITE_OPEN_EXCLUSIVE)
		    ((MAIN_DB)		SQLITE_OPEN_MAIN_DB)
		    ((TEMP_DB)		SQLITE_OPEN_TEMP_DB)
		    ((TRANSIENT_DB)	SQLITE_OPEN_TRANSIENT_DB)
		    ((MAIN_JOURNAL)	SQLITE_OPEN_MAIN_JOURNAL)
		    ((TEMP_JOURNAL)	SQLITE_OPEN_TEMP_JOURNAL)
		    ((SUBJOURNAL)	SQLITE_OPEN_SUBJOURNAL)
		    ((MASTER_JOURNAL)	SQLITE_OPEN_MASTER_JOURNAL)
		    ((NOMUTEX)		SQLITE_OPEN_NOMUTEX)
		    ((FULLMUTEX)	SQLITE_OPEN_FULLMUTEX)
		    ((SHAREDCACHE)	SQLITE_OPEN_SHAREDCACHE)
		    ((PRIVATECACHE)	SQLITE_OPEN_PRIVATECACHE))))
   0 (enum-set->list enum-set)))


;;;; done

)

;;; end of file
