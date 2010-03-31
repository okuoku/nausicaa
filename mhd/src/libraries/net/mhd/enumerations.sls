;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/MHD
;;;Contents: enumeration types
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


(library (net mhd enumerations)
  (export
    enum-mhd-flags			mhd-flags
    mhd-flags->value			value->mhd-flags

    enum-mhd-option			mhd-option
    mhd-option->value			value->mhd-option

    enum-mhd-value-kind			mhd-value-kind
    mhd-value-kind->value		value->mhd-value-kind

    enum-mhd-request-termination-code	mhd-request-termination-code
    mhd-request-termination-code->value	value->mhd-request-termination-code

    enum-mhd-gnutls-cipher-algorithm	mhd-gnutls-cipher-algorithm
    mhd-gnutls-cipher-algorithm->value	value->mhd-gnutls-cipher-algorithm

    enum-mhd-gnutls-protocol		mhd-gnutls-protocol
    mhd-gnutls-protocol->value		value->mhd-gnutls-protocol

    enum-mhd-connection-info-type	mhd-connection-info-type
    mhd-connection-info-type->value	value->mhd-connection-info-type

    enum-mhd-daemon-info-type		mhd-daemon-info-type
    mhd-daemon-info-type->value		value->mhd-daemon-info-type)
  (import (rnrs)
    (enumerations)
    (net mhd sizeof))


(define-c-ior-flags mhd-flags
  (MHD_NO_FLAG
   MHD_USE_DEBUG
   MHD_USE_SSL
   MHD_USE_THREAD_PER_CONNECTION
   MHD_USE_SELECT_INTERNALLY
   MHD_USE_IPv6
   MHD_USE_PEDANTIC_CHECKS)
  (no-flag
   use-debug
   use-ssl
   use-thread-per-connection
   use-select-internally
   use-ipv6
   use-pedantic-checks))

(define-c-flags mhd-option
  (MHD_OPTION_END MHD_OPTION_ARRAY
   MHD_OPTION_CONNECTION_MEMORY_LIMIT
   MHD_OPTION_CONNECTION_LIMIT
   MHD_OPTION_CONNECTION_TIMEOUT
   MHD_OPTION_NOTIFY_COMPLETED
   MHD_OPTION_PER_IP_CONNECTION_LIMIT
   MHD_OPTION_SOCK_ADDR
   MHD_OPTION_URI_LOG_CALLBACK
   MHD_OPTION_HTTPS_MEM_KEY
   MHD_OPTION_HTTPS_MEM_CERT
   MHD_OPTION_CRED_TYPE
   MHD_OPTION_PROTOCOL_VERSION
   MHD_OPTION_CIPHER_ALGORITHM
   MHD_OPTION_EXTERNAL_LOGGER
   MHD_OPTION_THREAD_POOL_SIZE)
  (end array
   connection-memory-limit
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
   thread-pool-size))

(define-c-flags mhd-value-kind
  (MHD_RESPONSE_HEADER_KIND
   MHD_HEADER_KIND
   MHD_COOKIE_KIND
   MHD_POSTDATA_KIND
   MHD_GET_ARGUMENT_KIND
   MHD_FOOTER_KIND)
  (response-header-kind
   header-kind
   cookie-kind
   postdata-kind
   get-argument-kind
   footer-kind))

(define-c-flags mhd-request-termination-code
  (MHD_REQUEST_TERMINATED_COMPLETED_OK
   MHD_REQUEST_TERMINATED_WITH_ERROR
   MHD_REQUEST_TERMINATED_TIMEOUT_REACHED
   MHD_REQUEST_TERMINATED_DAEMON_SHUTDOWN)
  (completed-ok
   with-error
   timeout-reached
   daemon-shutdown))

(define-c-flags mhd-gnutls-cipher-algorithm
  (MHD_GNUTLS_CIPHER_UNKNOWN
   MHD_GNUTLS_CIPHER_NULL
   MHD_GNUTLS_CIPHER_ARCFOUR_128
   MHD_GNUTLS_CIPHER_3DES_CBC
   MHD_GNUTLS_CIPHER_AES_128_CBC
   MHD_GNUTLS_CIPHER_AES_256_CBC)
  (unknown
   null
   arcfour-128
   des3-cbc
   aes-128-cbc
   aes-256-cbc))

(define-c-flags mhd-gnutls-protocol
  (MHD_GNUTLS_PROTOCOL_END
   MHD_GNUTLS_PROTOCOL_SSL3
   MHD_GNUTLS_PROTOCOL_TLS1_0
   MHD_GNUTLS_PROTOCOL_TLS1_1
   MHD_GNUTLS_PROTOCOL_TLS1_2
   MHD_GNUTLS_PROTOCOL_VERSION_UNKNOWN)
  (end
   ssl3
   tls1-0
   tls1-1
   tls1-2
   version-unknown))

(define-c-flags mhd-connection-info-type
  (MHD_CONNECTION_INFO_CIPHER_ALGO
   MHD_CONNECTION_INFO_PROTOCOL
   MHD_CONNECTION_INFO_CLIENT_ADDRESS)
  (cipher-algo
   protocol
   client-address))

(define-c-flags mhd-daemon-info-type
  (MHD_DAEMON_INFO_KEY_SIZE
   MHD_DAEMON_INFO_MAC_KEY_SIZE
   MHD_DAEMON_INFO_LISTEN_FD)
  (key-size
   mac-key-size
   listen-fd))


;;;; done

)

;;; end of file
