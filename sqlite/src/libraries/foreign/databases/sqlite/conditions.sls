;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/SQLite
;;;Contents: condition type definitions
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


(library (foreign databases sqlite conditions)
  (export

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
    )
  (import (rnrs))


(define-condition-type &sqlite-session
  &condition
  make-sqlite-session-condition
  sqlite-session-condition?
  (session	sqlite-session-condition))

(define-condition-type &sqlite-database
  &condition
  make-sqlite-database-condition
  sqlite-database-condition?
  (database	sqlite-database-condition))

(define-condition-type &sqlite-query
  &condition
  make-sqlite-query-condition
  sqlite-query-condition?
  (query	sqlite-query-condition))


(define-condition-type &sqlite-error
  &error
  make-sqlite-error-condition
  sqlite-error-condition?)

;;; --------------------------------------------------------------------

(define-condition-type &sqlite-opening-error
  &sqlite-error
  make-sqlite-opening-error-condition
  sqlite-opening-error-condition?)

(define-syntax raise-sqlite-opening-error
  (syntax-rules ()
    ((_ ?who ?message ?database)
     (raise-continuable
      (condition (make-sqlite-opening-error-condition)
		 (make-sqlite-database-condition ?database)
		 (make-who-condition ?who)
		 (make-message-condition ?message))))))

;;; --------------------------------------------------------------------

(define-condition-type &sqlite-querying-error
  &sqlite-error
  make-sqlite-querying-error-condition
  sqlite-querying-error-condition?)

(define-syntax raise-sqlite-querying-error
  (syntax-rules ()
    ((_ ?who ?message ?session ?query)
     (raise-continuable
      (condition (make-sqlite-querying-error-condition)
		 (make-sqlite-session-condition ?session)
		 (make-sqlite-query-condition ?query)
		 (make-who-condition ?who)
		 (make-message-condition ?message))))))


;;;; done

)

;;; end of file
