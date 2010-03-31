;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/MHD
;;;Contents: compound library, high-level API
;;;Date: Wed Dec  2, 2009
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


(library (net mhd)
  (export

    ;; daemon
    mhd-start-daemon			mhd-stop-daemon
    mhd-run
    mhd-get-fdset			mhd-get-timeout

    mhd-set-panic-func
    make-mhd-accept-policy-callback	make-mhd-access-contents-callback

    ;; connection
    mhd-get-connection-values		mhd-set-connection-value
    mhd-lookup-connection-value

    make-mhd-header-callback

    ;; response
    mhd-create-response-from-callback	mhd-create-response-from-data
    mhd-queue-response			mhd-destroy-response
    mhd-add-response-header		mhd-del-response-header
    mhd-get-response-headers		mhd-get-response-header

    make-mhd-content-reader-callback
    make-mhd-content-reader-free-callback

    ;; HTTP POST
    mhd-create-post-processor		mhd-destroy-post-processor
    mhd-post-process

;;; mhd-get-connection-info
;;; mhd-get-daemon-info

;;; mhd-get-version

;;; --------------------------------------------------------------------
;;; enumerations

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
    mhd-daemon-info-type->value		value->mhd-daemon-info-type

;;; --------------------------------------------------------------------
;;; record types

    daemon				daemon?
    pointer->daemon			daemon->pointer

    connection				connection?
    pointer->connection			connection->pointer

    response				response?
    pointer->response			response->pointer

    post-processor			post-processor?
    pointer->post-processor		post-processor->pointer

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

;;; --------------------------------------------------------------------
;;; constants

    MHD_VERSION

    MHD_YES				MHD_NO

    MHD_SIZE_UNKNOWN

    MHD_HTTP_CONTINUE			MHD_HTTP_SWITCHING_PROTOCOLS
    MHD_HTTP_PROCESSING

    MHD_HTTP_OK				MHD_HTTP_CREATED
    MHD_HTTP_ACCEPTED			MHD_HTTP_NON_AUTHORITATIVE_INFORMATION
    MHD_HTTP_NO_CONTENT			MHD_HTTP_RESET_CONTENT
    MHD_HTTP_PARTIAL_CONTENT		MHD_HTTP_MULTI_STATUS

    MHD_HTTP_MULTIPLE_CHOICES		MHD_HTTP_MOVED_PERMANENTLY
    MHD_HTTP_FOUND			MHD_HTTP_SEE_OTHER
    MHD_HTTP_NOT_MODIFIED		MHD_HTTP_USE_PROXY
    MHD_HTTP_SWITCH_PROXY		MHD_HTTP_TEMPORARY_REDIRECT

    MHD_HTTP_BAD_REQUEST		MHD_HTTP_UNAUTHORIZED
    MHD_HTTP_PAYMENT_REQUIRED		MHD_HTTP_FORBIDDEN
    MHD_HTTP_NOT_FOUND			MHD_HTTP_METHOD_NOT_ALLOWED
    MHD_HTTP_METHOD_NOT_ACCEPTABLE	MHD_HTTP_PROXY_AUTHENTICATION_REQUIRED
    MHD_HTTP_REQUEST_TIMEOUT		MHD_HTTP_CONFLICT
    MHD_HTTP_GONE			MHD_HTTP_LENGTH_REQUIRED
    MHD_HTTP_PRECONDITION_FAILED	MHD_HTTP_REQUEST_ENTITY_TOO_LARGE
    MHD_HTTP_REQUEST_URI_TOO_LONG	MHD_HTTP_UNSUPPORTED_MEDIA_TYPE
    MHD_HTTP_REQUESTED_RANGE_NOT_SATISFIABLE
    MHD_HTTP_EXPECTATION_FAILED		MHD_HTTP_UNPROCESSABLE_ENTITY
    MHD_HTTP_LOCKED			MHD_HTTP_FAILED_DEPENDENCY
    MHD_HTTP_UNORDERED_COLLECTION	MHD_HTTP_UPGRADE_REQUIRED
    MHD_HTTP_RETRY_WITH
    MHD_HTTP_INTERNAL_SERVER_ERROR	MHD_HTTP_NOT_IMPLEMENTED
    MHD_HTTP_BAD_GATEWAY		MHD_HTTP_SERVICE_UNAVAILABLE
    MHD_HTTP_GATEWAY_TIMEOUT		MHD_HTTP_HTTP_VERSION_NOT_SUPPORTED
    MHD_HTTP_VARIANT_ALSO_NEGOTIATES	MHD_HTTP_INSUFFICIENT_STORAGE
    MHD_HTTP_BANDWIDTH_LIMIT_EXCEEDED	MHD_HTTP_NOT_EXTENDED

    MHD_HTTP_HEADER_ACCEPT		MHD_HTTP_HEADER_ACCEPT_CHARSET
    MHD_HTTP_HEADER_ACCEPT_ENCODING	MHD_HTTP_HEADER_ACCEPT_LANGUAGE
    MHD_HTTP_HEADER_ACCEPT_RANGES	MHD_HTTP_HEADER_AGE
    MHD_HTTP_HEADER_ALLOW		MHD_HTTP_HEADER_AUTHORIZATION
    MHD_HTTP_HEADER_CACHE_CONTROL	MHD_HTTP_HEADER_CONNECTION
    MHD_HTTP_HEADER_CONTENT_ENCODING	MHD_HTTP_HEADER_CONTENT_LANGUAGE
    MHD_HTTP_HEADER_CONTENT_LENGTH	MHD_HTTP_HEADER_CONTENT_LOCATION
    MHD_HTTP_HEADER_CONTENT_MD5		MHD_HTTP_HEADER_CONTENT_RANGE
    MHD_HTTP_HEADER_CONTENT_TYPE	MHD_HTTP_HEADER_COOKIE
    MHD_HTTP_HEADER_DATE		MHD_HTTP_HEADER_ETAG
    MHD_HTTP_HEADER_EXPECT		MHD_HTTP_HEADER_EXPIRES
    MHD_HTTP_HEADER_FROM		MHD_HTTP_HEADER_HOST
    MHD_HTTP_HEADER_IF_MATCH		MHD_HTTP_HEADER_IF_MODIFIED_SINCE
    MHD_HTTP_HEADER_IF_NONE_MATCH	MHD_HTTP_HEADER_IF_RANGE
    MHD_HTTP_HEADER_IF_UNMODIFIED_SINCE	MHD_HTTP_HEADER_LAST_MODIFIED
    MHD_HTTP_HEADER_LOCATION		MHD_HTTP_HEADER_MAX_FORWARDS
    MHD_HTTP_HEADER_PRAGMA		MHD_HTTP_HEADER_PROXY_AUTHENTICATE
    MHD_HTTP_HEADER_PROXY_AUTHORIZATION	MHD_HTTP_HEADER_RANGE
    MHD_HTTP_HEADER_REFERER		MHD_HTTP_HEADER_RETRY_AFTER
    MHD_HTTP_HEADER_SERVER		MHD_HTTP_HEADER_SET_COOKIE
    MHD_HTTP_HEADER_SET_COOKIE2		MHD_HTTP_HEADER_TE
    MHD_HTTP_HEADER_TRAILER		MHD_HTTP_HEADER_TRANSFER_ENCODING
    MHD_HTTP_HEADER_UPGRADE		MHD_HTTP_HEADER_USER_AGENT
    MHD_HTTP_HEADER_VARY		MHD_HTTP_HEADER_VIA
    MHD_HTTP_HEADER_WARNING		MHD_HTTP_HEADER_WWW_AUTHENTICATE

    MHD_HTTP_VERSION_1_0		MHD_HTTP_VERSION_1_1

    MHD_HTTP_METHOD_CONNECT		MHD_HTTP_METHOD_DELETE
    MHD_HTTP_METHOD_GET			MHD_HTTP_METHOD_HEAD
    MHD_HTTP_METHOD_OPTIONS		MHD_HTTP_METHOD_POST
    MHD_HTTP_METHOD_PUT			MHD_HTTP_METHOD_TRACE

    MHD_HTTP_POST_ENCODING_FORM_URLENCODED
    MHD_HTTP_POST_ENCODING_MULTIPART_FORMDATA

    MHD_NO_FLAG				MHD_USE_DEBUG
    MHD_USE_SSL				MHD_USE_THREAD_PER_CONNECTION
    MHD_USE_SELECT_INTERNALLY		MHD_USE_IPv6
    MHD_USE_PEDANTIC_CHECKS

    MHD_OPTION_END			MHD_OPTION_CONNECTION_MEMORY_LIMIT
    MHD_OPTION_CONNECTION_LIMIT		MHD_OPTION_CONNECTION_TIMEOUT
    MHD_OPTION_NOTIFY_COMPLETED		MHD_OPTION_PER_IP_CONNECTION_LIMIT
    MHD_OPTION_SOCK_ADDR		MHD_OPTION_URI_LOG_CALLBACK
    MHD_OPTION_HTTPS_MEM_KEY		MHD_OPTION_HTTPS_MEM_CERT
    MHD_OPTION_CRED_TYPE		MHD_OPTION_PROTOCOL_VERSION
    MHD_OPTION_CIPHER_ALGORITHM		MHD_OPTION_EXTERNAL_LOGGER
    MHD_OPTION_THREAD_POOL_SIZE

    MHD_RESPONSE_HEADER_KIND		MHD_HEADER_KIND
    MHD_COOKIE_KIND			MHD_POSTDATA_KIND
    MHD_GET_ARGUMENT_KIND		MHD_FOOTER_KIND

    MHD_REQUEST_TERMINATED_COMPLETED_OK	MHD_REQUEST_TERMINATED_WITH_ERROR
    MHD_REQUEST_TERMINATED_TIMEOUT_REACHED
    MHD_REQUEST_TERMINATED_DAEMON_SHUTDOWN

    MHD_GNUTLS_CIPHER_UNKNOWN		MHD_GNUTLS_CIPHER_NULL
    MHD_GNUTLS_CIPHER_ARCFOUR_128	MHD_GNUTLS_CIPHER_3DES_CBC
    MHD_GNUTLS_CIPHER_AES_128_CBC	MHD_GNUTLS_CIPHER_AES_256_CBC

    MHD_GNUTLS_PROTOCOL_END		MHD_GNUTLS_PROTOCOL_SSL3
    MHD_GNUTLS_PROTOCOL_TLS1_0		MHD_GNUTLS_PROTOCOL_TLS1_1
    MHD_GNUTLS_PROTOCOL_TLS1_2		MHD_GNUTLS_PROTOCOL_VERSION_UNKNOWN

    MHD_CONNECTION_INFO_CIPHER_ALGO	MHD_CONNECTION_INFO_PROTOCOL
    MHD_CONNECTION_INFO_CLIENT_ADDRESS

    MHD_DAEMON_INFO_KEY_SIZE		MHD_DAEMON_INFO_MAC_KEY_SIZE
    MHD_DAEMON_INFO_LISTEN_FD)
  (import (rnrs)
    (net mhd typedefs)
    (net mhd enumerations)
    (net mhd primitives)
    (net mhd sizeof)))

;;; end of file
