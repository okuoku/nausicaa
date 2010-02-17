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
    <connection>		<connection-rtd>
    <connection>?
    pointer-><connection>	<connection>->pointer

;;; --------------------------------------------------------------------

    <fd>			<fd>?
    integer-><fd>		<fd>->integer

    pid				pid?
    integer->pid		pid->integer

    ssl				ssl?
    pointer->ssl		ssl->pointer

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

    )
  (import (rnrs)
    (only (foreign cstrings) cstring->string)
    (only (foreign memory) pointer-null?)
    (foreign databases postgresql sizeof))


(define-record-type (<connection> pointer-><connection> <connection>?)
  (fields (immutable pointer <connection>->pointer)))

(define <connection-rtd>
  (record-type-descriptor <connection>))

;;; --------------------------------------------------------------------

(define-record-type (<fd> integer-><fd> <fd>?)
  (nongenerative nausicaa:posix:<fd>)
  (fields (immutable object <fd>->integer)))

(define-record-type (pid integer->pid pid?)
  (nongenerative nausicaa:posix:pid)
  (fields (immutable object pid->integer)))

(define-record-type (ssl pointer->ssl ssl?)
  (nongenerative nausicaa:openssl:ssl)
  (fields (immutable object ssl->pointer)))


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


;;;; done

)

;;; end of file
