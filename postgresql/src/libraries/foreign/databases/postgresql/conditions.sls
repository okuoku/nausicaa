;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/PostgreSQL
;;;Contents: condition object type definitions
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


(library (foreign databases postgresql conditions)
  (export

    &connection
    make-connection-condition
    connection-condition?
    connection-condition

    &connect-info
    make-connect-info-condition
    connect-info-condition?
    connect-info-condition

    &cancel-handler
    make-cancel-handler-condition
    cancel-handler-condition?
    cancel-handler-condition

    &query-string
    make-query-string-condition
    query-string-condition?
    query-string-condition

    &query-result
    make-query-result-condition
    query-result-condition?
    query-result-condition

    &parameters
    make-parameters-condition
    parameters-condition?
    parameters-condition

    &statement-name
    make-statement-name-condition
    statement-name-condition?
    statement-name-condition

    &portal-name
    make-portal-name-condition
    portal-name-condition?
    portal-name-condition

    &escape-string
    make-escape-string-condition
    escape-string-condition?
    escape-string-condition

;;; --------------------------------------------------------------------

    &postgresql-error
    make-postgresql-error-condition
    postgresql-error-condition?

    &postgresql-poll-error
    make-postgresql-poll-error-condition
    postgresql-poll-error-condition?

    &postgresql-info-error
    make-postgresql-info-error-condition
    postgresql-info-error-condition?

    &postgresql-exec-error
    make-postgresql-exec-error-condition
    postgresql-exec-error-condition?

    &postgresql-prepare-error
    make-postgresql-prepare-error-condition
    postgresql-prepare-error-condition?

    &postgresql-cancel-error
    make-postgresql-cancel-error-condition
    postgresql-cancel-error-condition?

    &postgresql-config-error
    make-postgresql-config-error-condition
    postgresql-config-error-condition?

    &postgresql-copy-error
    make-postgresql-copy-error-condition
    postgresql-copy-error-condition?

    &postgresql-copy-in-error
    make-postgresql-copy-in-error-condition
    postgresql-copy-in-error-condition?

    &postgresql-copy-out-error
    make-postgresql-copy-out-error-condition
    postgresql-copy-out-error-condition?

    &postgresql-copy-end-error
    make-postgresql-copy-end-error-condition
    postgresql-copy-end-error-condition?

    &postgresql-escape-encode-error
    make-postgresql-escape-encode-error-condition
    postgresql-escape-encode-error-condition?

    &postgresql-escape-decode-error
    make-postgresql-escape-decode-error-condition
    postgresql-escape-decode-error-condition?
    )
  (import (rnrs))


(define-condition-type &connection
  &condition
  make-connection-condition
  connection-condition?
  (connection		connection-condition))

(define-condition-type &connect-info
  &condition
  make-connect-info-condition
  connect-info-condition?
  (connect-info		connect-info-condition))

(define-condition-type &cancel-handler
  &condition
  make-cancel-handler-condition
  cancel-handler-condition?
  (cancel-handler	cancel-handler-condition))

(define-condition-type &query-string
  &condition
  make-query-string-condition
  query-string-condition?
  (string		query-string-condition))

(define-condition-type &query-result
  &condition
  make-query-result-condition
  query-result-condition?
  (result		query-result-condition))

(define-condition-type &parameters
  &condition
  make-parameters-condition
  parameters-condition?
  (parameters		parameters-condition))

(define-condition-type &statement-name
  &condition
  make-statement-name-condition
  statement-name-condition?
  (statement-name	statement-name-condition))

(define-condition-type &portal-name
  &condition
  make-portal-name-condition
  portal-name-condition?
  (portal-name		portal-name-condition))

(define-condition-type &escape-string
  &condition
  make-escape-string-condition
  escape-string-condition?
  (escape-string	escape-string-condition))


(define-condition-type &postgresql-error
  &error
  make-postgresql-error-condition
  postgresql-error-condition?)

(define-condition-type &postgresql-info-error
  &postgresql-error
  make-postgresql-info-error-condition
  postgresql-info-error-condition?)

(define-condition-type &postgresql-exec-error
  &postgresql-error
  make-postgresql-exec-error-condition
  postgresql-exec-error-condition?)

(define-condition-type &postgresql-prepare-error
  &postgresql-error
  make-postgresql-prepare-error-condition
  postgresql-prepare-error-condition?)

(define-condition-type &postgresql-poll-error
  &postgresql-error
  make-postgresql-poll-error-condition
  postgresql-poll-error-condition?)

(define-condition-type &postgresql-cancel-error
  &postgresql-error
  make-postgresql-cancel-error-condition
  postgresql-cancel-error-condition?)

(define-condition-type &postgresql-config-error
  &postgresql-error
  make-postgresql-config-error-condition
  postgresql-config-error-condition?)

(define-condition-type &postgresql-copy-error
  &postgresql-error
  make-postgresql-copy-error-condition
  postgresql-copy-error-condition?)

(define-condition-type &postgresql-copy-in-error
  &postgresql-copy-error
  make-postgresql-copy-in-error-condition
  postgresql-copy-in-error-condition?)

(define-condition-type &postgresql-copy-out-error
  &postgresql-copy-error
  make-postgresql-copy-out-error-condition
  postgresql-copy-out-error-condition?)

(define-condition-type &postgresql-copy-end-error
  &postgresql-copy-error
  make-postgresql-copy-end-error-condition
  postgresql-copy-end-error-condition?)

(define-condition-type &postgresql-escape-encode-error
  &postgresql-copy-error
  make-postgresql-escape-encode-error-condition
  postgresql-escape-encode-error-condition?)

(define-condition-type &postgresql-escape-decode-error
  &postgresql-copy-error
  make-postgresql-escape-decode-error-condition
  postgresql-escape-decode-error-condition?)


;;;; done

)

;;; end of file
