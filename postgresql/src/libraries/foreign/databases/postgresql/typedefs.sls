;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/PostgreSQL
;;;Contents: type definitions
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


(library (foreign databases postgresql typedefs)
  (export

    connection			connection?
    pointer->connection		connection->pointer

    cancel-handler		cancel-handler?
    pointer->cancel-handler	cancel-handler->pointer

    query-result		query-result?
    pointer->query-result	query-result->pointer

;;; --------------------------------------------------------------------

    fd				fd?
    integer->fd			fd->integer

    pid				pid?
    integer->pid		pid->integer

    ssl				ssl?
    pointer->ssl		ssl->pointer

    FILE*			FILE*?
    pointer->FILE*		FILE*->pointer

;;; --------------------------------------------------------------------

    <connect-option>		<connect-option-rtd>
    make-<connect-option>	<connect-option>?
    <connect-option>-keyword
    <connect-option>-envvar
    <connect-option>-compiled
    <connect-option>-val
    <connect-option>-label
    <connect-option>-dispchar
    <connect-option>-dispsize

    pointer-><connect-option>

;;; --------------------------------------------------------------------

    <parameter>				<parameter-rtd>
    make-<parameter>			<parameter>?
    <parameter>-value
    <parameter>-size
    <parameter>-text?
    <parameter>-oid

    parameter

;;; --------------------------------------------------------------------

    <notification>			<notification-rtd>
    make-<notification>			<notification>?
    <notification>-relname
    <notification>-pid
    <notification>-extra

    )
  (import (rnrs)
    (only (foreign cstrings) cstring->string)
    (only (foreign memory) pointer-null?)
    (foreign databases postgresql sizeof))


(define-record-type (connection pointer->connection connection?)
  (fields (immutable pointer connection->pointer)))

(define-record-type (cancel-handler pointer->cancel-handler cancel-handler?)
  (fields (immutable pointer cancel-handler->pointer)))

(define-record-type (query-result pointer->query-result query-result?)
  (fields (immutable pointer query-result->pointer)))

;;; --------------------------------------------------------------------

(define-record-type (fd integer->fd fd?)
  (nongenerative nausicaa:posix:fd)
  (fields (immutable object fd->integer)))

(define-record-type (pid integer->pid pid?)
  (nongenerative nausicaa:posix:pid)
  (fields (immutable object pid->integer)))

(define-record-type (ssl pointer->ssl ssl?)
  (nongenerative nausicaa:openssl:ssl)
  (fields (immutable object ssl->pointer)))

(define-record-type (FILE* pointer->FILE* FILE*?)
  (nongenerative nausicaa:posix:FILE*)
  (fields (immutable object FILE*->pointer)))


(define-record-type <connect-option>
  (fields (immutable keyword)
	  (immutable envvar)
	  (immutable compiled)
	  (immutable val)
	  (immutable label)
	  (immutable dispchar)
	  (immutable dispsize)))

(define <connect-option-rtd>
  (record-type-descriptor <connect-option>))

(define (pointer-><connect-option> pointer)
  (make-<connect-option>
   (cstring->string (struct-PQconninfoOption-keyword-ref pointer))
   (cstring->string (struct-PQconninfoOption-envvar-ref pointer))
   (let ((p (struct-PQconninfoOption-compiled-ref pointer)))
     (if (pointer-null? p)
	 #f
       (cstring->string p)))
   (let ((p (struct-PQconninfoOption-val-ref pointer)))
     (if (pointer-null? p)
	 #f
       (cstring->string p)))
   (cstring->string (struct-PQconninfoOption-label-ref pointer))
   (cstring->string (struct-PQconninfoOption-dispchar-ref pointer))
   (struct-PQconninfoOption-dispsize-ref pointer)))


(define-record-type <parameter>
  (fields (immutable value)
	  (immutable size)
	  (immutable text?)
	  (immutable oid)))

(define <parameter-rtd>
  (record-type-descriptor <parameter>))

(define %invalid-oid    0)
(define %default-size #f)
(define %default-text   #t)

(define-syntax parameter
  (syntax-rules (value size text? oid)

    ((_ (value ?value) (size ?size) (text? ?text) (oid ?oid))
     (make-<parameter> ?value ?size ?text ?oid))

    ((_ (value ?value) (size ?size) (text? ?text))
     (parameter (value ?value) (size ?size) (text? ?text) (oid %invalid-oid)))

    ((_ (value ?value) (text? ?text) (oid ?oid))
     (parameter (value ?value) (size %default-size) (text? ?text) (oid ?oid)))

    ((_ (value ?value) (text? ?text))
     (parameter (value ?value) (size %default-size) (text? ?text) (oid %invalid-oid)))

    ((_ (value ?value) (oid ?oid))
     (parameter (value ?value) (size %default-size) (text? %default-text) (oid ?oid)))

    ((_ (value ?value))
     (parameter (value ?value) (size %default-size) (text? %default-text) (oid %invalid-oid)))

    ((_ ?value)
     (parameter (value ?value) (size %default-size) (text? %default-text) (oid %invalid-oid)))))


(define-record-type <notification>
  (fields (immutable relname)
	  (immutable pid)
	  (immutable extra)))

(define <notification-rtd>
  (record-type-descriptor <notification>))


;;;; done

)

;;; end of file
