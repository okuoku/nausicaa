;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/MHD
;;;Contents: record types
;;;Date: Thu Dec  3, 2009
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


(library (foreign net mhd record-types)
  (export

    <mhd-pointer-wrapper>
    make-<mhd-pointer-wrapper>		<mhd-pointer-wrapper>?
    <mhd-pointer-wrapper>-pointer

    <mhd-daemon>
    make-<mhd-daemon>			<mhd-daemon>?

    <mhd-connection>
    make-<mhd-connection>		<mhd-connection>?

    <mhd-response>
    make-<mhd-response>			<mhd-response>?

    <mhd-post-processor>
    make-<mhd-post-processor>		<mhd-post-processor>?

    <mhd-daemon-config>			<mhd-daemon-config>?
    make-<mhd-daemon-config>
    <mhd-daemon-config>-connection-memory-limit
    <mhd-daemon-config>-connection-limit
    <mhd-daemon-config>-connection-timeout
    <mhd-daemon-config>-notify-completed
    <mhd-daemon-config>-per-ip-connection-limit
    <mhd-daemon-config>-sock-addr
    <mhd-daemon-config>-uri-log-callback
    <mhd-daemon-config>-https-mem-key
    <mhd-daemon-config>-https-mem-cert
    <mhd-daemon-config>-cred-type
    <mhd-daemon-config>-protocol-version
    <mhd-daemon-config>-cipher-algorithm
    <mhd-daemon-config>-external-logger
    <mhd-daemon-config>-thread-pool-size

    mhd-daemon-config
    )
  (import (rnrs))


(define-record-type <mhd-pointer-wrapper>
  (fields (immutable pointer)))

(define-record-type <mhd-daemon>
  (parent <mhd-pointer-wrapper>))

(define-record-type <mhd-connection>
  (parent <mhd-pointer-wrapper>))

(define-record-type <mhd-response>
  (parent <mhd-pointer-wrapper>))

(define-record-type <mhd-post-processor>
  (parent <mhd-pointer-wrapper>))


(define-record-type <mhd-daemon-config>
  (fields (mutable connection-memory-limit)
	  (mutable connection-limit)
	  (mutable connection-timeout)
	  (mutable notify-completed)
	  (mutable per-ip-connection-limit)
	  (mutable sock-addr)
	  (mutable uri-log-callback)
	  (mutable https-mem-key)
	  (mutable https-mem-cert)
	  (mutable cred-type)
	  (mutable protocol-version)
	  (mutable cipher-algorithm)
	  (mutable external-logger)
	  (mutable thread-pool-size)))

(define <mhd-daemon-config-rtd>
  (record-type-descriptor <mhd-daemon-config>))

(define-syntax mhd-daemon-config
  (syntax-rules ()
    ((_ ?option ...)
     (let ((cfg (make-<mhd-daemon-config>
		 #f #f #f
		 #f #f #f
		 #f #f #f
		 #f #f #f
		 #f #f)))
       (%daemon-config cfg ?option ...)))))

(define-syntax %daemon-config
  (syntax-rules (connection-memory-limit
		 connection-limit
		 connection-timeout
		 notify-completed
		 per-ip-connection-limit
		 sock-addr
		 uri-log-callback
		 https-mem-key
		 https-mem-cert
		 cred-type
		 protocol-version
		 cipher-algorithm
		 external-logger
		 thread-pool-size)

    ((_ ?cfg (thread-pool-size ?value) ?option ...)
     (begin
       (<mhd-daemon-config>-thread-pool-size-set! ?cfg ?value)
       (%daemon-config ?cfg ?option ...)))

    ((_ ?cfg (external-logger ?value) ?option ...)
     (begin
       (<mhd-daemon-config>-external-logger-set! ?cfg ?value)
       (%daemon-config ?cfg ?option ...)))

    ((_ ?cfg (cipher-algorithm ?value) ?option ...)
     (begin
       (<mhd-daemon-config>-cipher-algorithm-set! ?cfg ?value)
       (%daemon-config ?cfg ?option ...)))

    ((_ ?cfg (protocol-version ?value) ?option ...)
     (begin
       (<mhd-daemon-config>-protocol-version-set! ?cfg ?value)
       (%daemon-config ?cfg ?option ...)))

    ((_ ?cfg (cred-type ?value) ?option ...)
     (begin
       (<mhd-daemon-config>-cred-type-set! ?cfg ?value)
       (%daemon-config ?cfg ?option ...)))

    ((_ ?cfg (https-mem-cert ?value) ?option ...)
     (begin
       (<mhd-daemon-config>-https-mem-cert-set! ?cfg ?value)
       (%daemon-config ?cfg ?option ...)))

    ((_ ?cfg (https-mem-key ?value) ?option ...)
     (begin
       (<mhd-daemon-config>-https-mem-key-set! ?cfg ?value)
       (%daemon-config ?cfg ?option ...)))

    ((_ ?cfg (uri-log-callback ?value) ?option ...)
     (begin
       (<mhd-daemon-config>-uri-log-callback-set! ?cfg ?value)
       (%daemon-config ?cfg ?option ...)))

    ((_ ?cfg (sock-addr ?value) ?option ...)
     (begin
       (<mhd-daemon-config>-sock-addr-set! ?cfg ?value)
       (%daemon-config ?cfg ?option ...)))

    ((_ ?cfg (per-ip-connection-limit ?value) ?option ...)
     (begin
       (<mhd-daemon-config>-per-ip-connection-limit-set! ?cfg ?value)
       (%daemon-config ?cfg ?option ...)))

    ((_ ?cfg (notify-completed ?value) ?option ...)
     (begin
       (<mhd-daemon-config>-notify-completed-set! ?cfg ?value)
       (%daemon-config ?cfg ?option ...)))

    ((_ ?cfg (connection-memory-limit ?value) ?option ...)
     (begin
       (<mhd-daemon-config>-connection-memory-limit-set! ?cfg ?value)
       (%daemon-config ?cfg ?option ...)))

    ((_ ?cfg (connection-limit ?value) ?option ...)
     (begin
       (<mhd-daemon-config>-connection-limit-set! ?cfg ?value)
       (%daemon-config ?cfg ?option ...)))

    ((_ ?cfg (connection-timeout ?value) ?option ...)
     (begin
       (<mhd-daemon-config>-connection-timeout-set! ?cfg ?value)
       (%daemon-config ?cfg ?option ...)))

    ((_ ?cfg)
     ?cfg)))


;;;; done

)

;;; end of file
