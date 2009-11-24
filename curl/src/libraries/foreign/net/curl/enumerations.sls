;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/cURL
;;;Contents: enumerations definitions
;;;Date: Sat Nov 21, 2009
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


(library (foreign net curl enumerations)
  (export

    curl-easy-numeric-code->symbolic-code	curl-easy-symbolic-code->numeric-code
    curl-multi-numeric-code->symbolic-code	curl-multi-symbolic-code->numeric-code
    curl-share-numeric-code->symbolic-code	curl-share-symbolic-code->numeric-code

    curl-pause-symbol			curl-pause-mask
    %curl-pause-set->bitmask
    )
  (import (rnrs)
    (foreign net curl sizeof))


(define (curl-easy-numeric-code->symbolic-code numeric-code)
  (let ((symbolic-code (hashtable-ref easy-numeric-error-code-table numeric-code #f)))
    (or symbolic-code (assertion-violation #f "invalid cURL easy numeric error code" numeric-code))))

(define (curl-easy-symbolic-code->numeric-code symbolic-code)
  (let ((numeric-code (hashtable-ref easy-symbolic-error-code-table symbolic-code #f)))
    (or numeric-code (assertion-violation #f "invalid cURL easy symbolic error code" symbolic-code))))

(define (curl-multi-numeric-code->symbolic-code numeric-code)
  (let ((symbolic-code (hashtable-ref multi-numeric-error-code-table numeric-code #f)))
    (or symbolic-code (assertion-violation #f "invalid cURL multi numeric error code" numeric-code))))

(define (curl-multi-symbolic-code->numeric-code symbolic-code)
  (let ((numeric-code (hashtable-ref multi-symbolic-error-code-table symbolic-code #f)))
    (or numeric-code (assertion-violation #f "invalid cURL multi symbolic error code" symbolic-code))))

(define (curl-share-numeric-code->symbolic-code numeric-code)
  (let ((symbolic-code (hashtable-ref share-numeric-error-code-table numeric-code #f)))
    (or symbolic-code (assertion-violation #f "invalid cURL share numeric error code" numeric-code))))

(define (curl-share-symbolic-code->numeric-code symbolic-code)
  (let ((numeric-code (hashtable-ref share-symbolic-error-code-table symbolic-code #f)))
    (or numeric-code (assertion-violation #f "invalid cURL share symbolic error code" symbolic-code))))


(define easy-numeric-error-code-table
  (let ((table (make-eqv-hashtable)))
    (hashtable-set! table CURLE_OK				'CURLE_OK)
    (hashtable-set! table CURLE_UNSUPPORTED_PROTOCOL		'CURLE_UNSUPPORTED_PROTOCOL)
    (hashtable-set! table CURLE_FAILED_INIT			'CURLE_FAILED_INIT)
    (hashtable-set! table CURLE_URL_MALFORMAT			'CURLE_URL_MALFORMAT)
    (hashtable-set! table CURLE_OBSOLETE4			'CURLE_OBSOLETE4)
    (hashtable-set! table CURLE_COULDNT_RESOLVE_PROXY		'CURLE_COULDNT_RESOLVE_PROXY)
    (hashtable-set! table CURLE_COULDNT_RESOLVE_HOST		'CURLE_COULDNT_RESOLVE_HOST)
    (hashtable-set! table CURLE_COULDNT_CONNECT			'CURLE_COULDNT_CONNECT)
    (hashtable-set! table CURLE_FTP_WEIRD_SERVER_REPLY		'CURLE_FTP_WEIRD_SERVER_REPLY)
    (hashtable-set! table CURLE_REMOTE_ACCESS_DENIED		'CURLE_REMOTE_ACCESS_DENIED)
    (hashtable-set! table CURLE_OBSOLETE10			'CURLE_OBSOLETE10)
    (hashtable-set! table CURLE_FTP_WEIRD_PASS_REPLY		'CURLE_FTP_WEIRD_PASS_REPLY)
    (hashtable-set! table CURLE_OBSOLETE12			'CURLE_OBSOLETE12)
    (hashtable-set! table CURLE_FTP_WEIRD_PASV_REPLY		'CURLE_FTP_WEIRD_PASV_REPLY)
    (hashtable-set! table CURLE_FTP_WEIRD_227_FORMAT		'CURLE_FTP_WEIRD_227_FORMAT)
    (hashtable-set! table CURLE_FTP_CANT_GET_HOST		'CURLE_FTP_CANT_GET_HOST)
    (hashtable-set! table CURLE_OBSOLETE16			'CURLE_OBSOLETE16)
    (hashtable-set! table CURLE_FTP_COULDNT_SET_TYPE		'CURLE_FTP_COULDNT_SET_TYPE)
    (hashtable-set! table CURLE_PARTIAL_FILE			'CURLE_PARTIAL_FILE)
    (hashtable-set! table CURLE_FTP_COULDNT_RETR_FILE		'CURLE_FTP_COULDNT_RETR_FILE)
    (hashtable-set! table CURLE_OBSOLETE20			'CURLE_OBSOLETE20)
    (hashtable-set! table CURLE_QUOTE_ERROR			'CURLE_QUOTE_ERROR)
    (hashtable-set! table CURLE_HTTP_RETURNED_ERROR		'CURLE_HTTP_RETURNED_ERROR)
    (hashtable-set! table CURLE_WRITE_ERROR			'CURLE_WRITE_ERROR)
    (hashtable-set! table CURLE_OBSOLETE24			'CURLE_OBSOLETE24)
    (hashtable-set! table CURLE_UPLOAD_FAILED			'CURLE_UPLOAD_FAILED)
    (hashtable-set! table CURLE_READ_ERROR			'CURLE_READ_ERROR)
    (hashtable-set! table CURLE_OUT_OF_MEMORY			'CURLE_OUT_OF_MEMORY)
    (hashtable-set! table CURLE_OPERATION_TIMEDOUT		'CURLE_OPERATION_TIMEDOUT)
    (hashtable-set! table CURLE_OBSOLETE29			'CURLE_OBSOLETE29)
    (hashtable-set! table CURLE_FTP_PORT_FAILED			'CURLE_FTP_PORT_FAILED)
    (hashtable-set! table CURLE_FTP_COULDNT_USE_REST		'CURLE_FTP_COULDNT_USE_REST)
    (hashtable-set! table CURLE_OBSOLETE32			'CURLE_OBSOLETE32)
    (hashtable-set! table CURLE_RANGE_ERROR			'CURLE_RANGE_ERROR)
    (hashtable-set! table CURLE_HTTP_POST_ERROR			'CURLE_HTTP_POST_ERROR)
    (hashtable-set! table CURLE_SSL_CONNECT_ERROR		'CURLE_SSL_CONNECT_ERROR)
    (hashtable-set! table CURLE_BAD_DOWNLOAD_RESUME		'CURLE_BAD_DOWNLOAD_RESUME)
    (hashtable-set! table CURLE_FILE_COULDNT_READ_FILE		'CURLE_FILE_COULDNT_READ_FILE)
    (hashtable-set! table CURLE_LDAP_CANNOT_BIND		'CURLE_LDAP_CANNOT_BIND)
    (hashtable-set! table CURLE_LDAP_SEARCH_FAILED		'CURLE_LDAP_SEARCH_FAILED)
    (hashtable-set! table CURLE_OBSOLETE40			'CURLE_OBSOLETE40)
    (hashtable-set! table CURLE_FUNCTION_NOT_FOUND		'CURLE_FUNCTION_NOT_FOUND)
    (hashtable-set! table CURLE_ABORTED_BY_CALLBACK		'CURLE_ABORTED_BY_CALLBACK)
    (hashtable-set! table CURLE_BAD_FUNCTION_ARGUMENT		'CURLE_BAD_FUNCTION_ARGUMENT)
    (hashtable-set! table CURLE_OBSOLETE44			'CURLE_OBSOLETE44)
    (hashtable-set! table CURLE_INTERFACE_FAILED		'CURLE_INTERFACE_FAILED)
    (hashtable-set! table CURLE_OBSOLETE46			'CURLE_OBSOLETE46)
    (hashtable-set! table CURLE_TOO_MANY_REDIRECTS		'CURLE_TOO_MANY_REDIRECTS)
    (hashtable-set! table CURLE_UNKNOWN_TELNET_OPTION		'CURLE_UNKNOWN_TELNET_OPTION)
    (hashtable-set! table CURLE_TELNET_OPTION_SYNTAX		'CURLE_TELNET_OPTION_SYNTAX)
    (hashtable-set! table CURLE_OBSOLETE50			'CURLE_OBSOLETE50)
    (hashtable-set! table CURLE_PEER_FAILED_VERIFICATION	'CURLE_PEER_FAILED_VERIFICATION)
    (hashtable-set! table CURLE_GOT_NOTHING			'CURLE_GOT_NOTHING)
    (hashtable-set! table CURLE_SSL_ENGINE_NOTFOUND		'CURLE_SSL_ENGINE_NOTFOUND)
    (hashtable-set! table CURLE_SSL_ENGINE_SETFAILED		'CURLE_SSL_ENGINE_SETFAILED)
    (hashtable-set! table CURLE_SEND_ERROR			'CURLE_SEND_ERROR)
    (hashtable-set! table CURLE_RECV_ERROR			'CURLE_RECV_ERROR)
    (hashtable-set! table CURLE_OBSOLETE57			'CURLE_OBSOLETE57)
    (hashtable-set! table CURLE_SSL_CERTPROBLEM			'CURLE_SSL_CERTPROBLEM)
    (hashtable-set! table CURLE_SSL_CIPHER			'CURLE_SSL_CIPHER)
    (hashtable-set! table CURLE_SSL_CACERT			'CURLE_SSL_CACERT)
    (hashtable-set! table CURLE_BAD_CONTENT_ENCODING		'CURLE_BAD_CONTENT_ENCODING)
    (hashtable-set! table CURLE_LDAP_INVALID_URL		'CURLE_LDAP_INVALID_URL)
    (hashtable-set! table CURLE_FILESIZE_EXCEEDED		'CURLE_FILESIZE_EXCEEDED)
    (hashtable-set! table CURLE_USE_SSL_FAILED			'CURLE_USE_SSL_FAILED)
    (hashtable-set! table CURLE_SEND_FAIL_REWIND		'CURLE_SEND_FAIL_REWIND)
    (hashtable-set! table CURLE_SSL_ENGINE_INITFAILED		'CURLE_SSL_ENGINE_INITFAILED)
    (hashtable-set! table CURLE_LOGIN_DENIED			'CURLE_LOGIN_DENIED)
    (hashtable-set! table CURLE_TFTP_NOTFOUND			'CURLE_TFTP_NOTFOUND)
    (hashtable-set! table CURLE_TFTP_PERM			'CURLE_TFTP_PERM)
    (hashtable-set! table CURLE_REMOTE_DISK_FULL		'CURLE_REMOTE_DISK_FULL)
    (hashtable-set! table CURLE_TFTP_ILLEGAL			'CURLE_TFTP_ILLEGAL)
    (hashtable-set! table CURLE_TFTP_UNKNOWNID			'CURLE_TFTP_UNKNOWNID)
    (hashtable-set! table CURLE_REMOTE_FILE_EXISTS		'CURLE_REMOTE_FILE_EXISTS)
    (hashtable-set! table CURLE_TFTP_NOSUCHUSER			'CURLE_TFTP_NOSUCHUSER)
    (hashtable-set! table CURLE_CONV_FAILED			'CURLE_CONV_FAILED)
    (hashtable-set! table CURLE_CONV_REQD			'CURLE_CONV_REQD)
    (hashtable-set! table CURLE_SSL_CACERT_BADFILE		'CURLE_SSL_CACERT_BADFILE)
    (hashtable-set! table CURLE_REMOTE_FILE_NOT_FOUND		'CURLE_REMOTE_FILE_NOT_FOUND)
    (hashtable-set! table CURLE_SSH				'CURLE_SSH)
    (hashtable-set! table CURLE_SSL_SHUTDOWN_FAILED		'CURLE_SSL_SHUTDOWN_FAILED)
    (hashtable-set! table CURLE_AGAIN				'CURLE_AGAIN)
    (hashtable-set! table CURLE_SSL_CRL_BADFILE			'CURLE_SSL_CRL_BADFILE)
    (hashtable-set! table CURLE_SSL_ISSUER_ERROR		'CURLE_SSL_ISSUER_ERROR)
    (hashtable-set! table CURL_LAST				'CURL_LAST)
    table))


(define easy-symbolic-error-code-table
  (let ((table (make-eq-hashtable)))
    (hashtable-set! table 'CURLE_OK				CURLE_OK)
    (hashtable-set! table 'CURLE_UNSUPPORTED_PROTOCOL		CURLE_UNSUPPORTED_PROTOCOL)
    (hashtable-set! table 'CURLE_FAILED_INIT			CURLE_FAILED_INIT)
    (hashtable-set! table 'CURLE_URL_MALFORMAT			CURLE_URL_MALFORMAT)
    (hashtable-set! table 'CURLE_OBSOLETE4			CURLE_OBSOLETE4)
    (hashtable-set! table 'CURLE_COULDNT_RESOLVE_PROXY		CURLE_COULDNT_RESOLVE_PROXY)
    (hashtable-set! table 'CURLE_COULDNT_RESOLVE_HOST		CURLE_COULDNT_RESOLVE_HOST)
    (hashtable-set! table 'CURLE_COULDNT_CONNECT		CURLE_COULDNT_CONNECT)
    (hashtable-set! table 'CURLE_FTP_WEIRD_SERVER_REPLY		CURLE_FTP_WEIRD_SERVER_REPLY)
    (hashtable-set! table 'CURLE_REMOTE_ACCESS_DENIED		CURLE_REMOTE_ACCESS_DENIED)
    (hashtable-set! table 'CURLE_OBSOLETE10			CURLE_OBSOLETE10)
    (hashtable-set! table 'CURLE_FTP_WEIRD_PASS_REPLY		CURLE_FTP_WEIRD_PASS_REPLY)
    (hashtable-set! table 'CURLE_OBSOLETE12			CURLE_OBSOLETE12)
    (hashtable-set! table 'CURLE_FTP_WEIRD_PASV_REPLY		CURLE_FTP_WEIRD_PASV_REPLY)
    (hashtable-set! table 'CURLE_FTP_WEIRD_227_FORMAT		CURLE_FTP_WEIRD_227_FORMAT)
    (hashtable-set! table 'CURLE_FTP_CANT_GET_HOST		CURLE_FTP_CANT_GET_HOST)
    (hashtable-set! table 'CURLE_OBSOLETE16			CURLE_OBSOLETE16)
    (hashtable-set! table 'CURLE_FTP_COULDNT_SET_TYPE		CURLE_FTP_COULDNT_SET_TYPE)
    (hashtable-set! table 'CURLE_PARTIAL_FILE			CURLE_PARTIAL_FILE)
    (hashtable-set! table 'CURLE_FTP_COULDNT_RETR_FILE		CURLE_FTP_COULDNT_RETR_FILE)
    (hashtable-set! table 'CURLE_OBSOLETE20			CURLE_OBSOLETE20)
    (hashtable-set! table 'CURLE_QUOTE_ERROR			CURLE_QUOTE_ERROR)
    (hashtable-set! table 'CURLE_HTTP_RETURNED_ERROR		CURLE_HTTP_RETURNED_ERROR)
    (hashtable-set! table 'CURLE_WRITE_ERROR			CURLE_WRITE_ERROR)
    (hashtable-set! table 'CURLE_OBSOLETE24			CURLE_OBSOLETE24)
    (hashtable-set! table 'CURLE_UPLOAD_FAILED			CURLE_UPLOAD_FAILED)
    (hashtable-set! table 'CURLE_READ_ERROR			CURLE_READ_ERROR)
    (hashtable-set! table 'CURLE_OUT_OF_MEMORY			CURLE_OUT_OF_MEMORY)
    (hashtable-set! table 'CURLE_OPERATION_TIMEDOUT		CURLE_OPERATION_TIMEDOUT)
    (hashtable-set! table 'CURLE_OBSOLETE29			CURLE_OBSOLETE29)
    (hashtable-set! table 'CURLE_FTP_PORT_FAILED		CURLE_FTP_PORT_FAILED)
    (hashtable-set! table 'CURLE_FTP_COULDNT_USE_REST		CURLE_FTP_COULDNT_USE_REST)
    (hashtable-set! table 'CURLE_OBSOLETE32			CURLE_OBSOLETE32)
    (hashtable-set! table 'CURLE_RANGE_ERROR			CURLE_RANGE_ERROR)
    (hashtable-set! table 'CURLE_HTTP_POST_ERROR		CURLE_HTTP_POST_ERROR)
    (hashtable-set! table 'CURLE_SSL_CONNECT_ERROR		CURLE_SSL_CONNECT_ERROR)
    (hashtable-set! table 'CURLE_BAD_DOWNLOAD_RESUME		CURLE_BAD_DOWNLOAD_RESUME)
    (hashtable-set! table 'CURLE_FILE_COULDNT_READ_FILE		CURLE_FILE_COULDNT_READ_FILE)
    (hashtable-set! table 'CURLE_LDAP_CANNOT_BIND		CURLE_LDAP_CANNOT_BIND)
    (hashtable-set! table 'CURLE_LDAP_SEARCH_FAILED		CURLE_LDAP_SEARCH_FAILED)
    (hashtable-set! table 'CURLE_OBSOLETE40			CURLE_OBSOLETE40)
    (hashtable-set! table 'CURLE_FUNCTION_NOT_FOUND		CURLE_FUNCTION_NOT_FOUND)
    (hashtable-set! table 'CURLE_ABORTED_BY_CALLBACK		CURLE_ABORTED_BY_CALLBACK)
    (hashtable-set! table 'CURLE_BAD_FUNCTION_ARGUMENT		CURLE_BAD_FUNCTION_ARGUMENT)
    (hashtable-set! table 'CURLE_OBSOLETE44			CURLE_OBSOLETE44)
    (hashtable-set! table 'CURLE_INTERFACE_FAILED		CURLE_INTERFACE_FAILED)
    (hashtable-set! table 'CURLE_OBSOLETE46			CURLE_OBSOLETE46)
    (hashtable-set! table 'CURLE_TOO_MANY_REDIRECTS		CURLE_TOO_MANY_REDIRECTS)
    (hashtable-set! table 'CURLE_UNKNOWN_TELNET_OPTION		CURLE_UNKNOWN_TELNET_OPTION)
    (hashtable-set! table 'CURLE_TELNET_OPTION_SYNTAX		CURLE_TELNET_OPTION_SYNTAX)
    (hashtable-set! table 'CURLE_OBSOLETE50			CURLE_OBSOLETE50)
    (hashtable-set! table 'CURLE_PEER_FAILED_VERIFICATION	CURLE_PEER_FAILED_VERIFICATION)
    (hashtable-set! table 'CURLE_GOT_NOTHING			CURLE_GOT_NOTHING)
    (hashtable-set! table 'CURLE_SSL_ENGINE_NOTFOUND		CURLE_SSL_ENGINE_NOTFOUND)
    (hashtable-set! table 'CURLE_SSL_ENGINE_SETFAILED		CURLE_SSL_ENGINE_SETFAILED)
    (hashtable-set! table 'CURLE_SEND_ERROR			CURLE_SEND_ERROR)
    (hashtable-set! table 'CURLE_RECV_ERROR			CURLE_RECV_ERROR)
    (hashtable-set! table 'CURLE_OBSOLETE57			CURLE_OBSOLETE57)
    (hashtable-set! table 'CURLE_SSL_CERTPROBLEM		CURLE_SSL_CERTPROBLEM)
    (hashtable-set! table 'CURLE_SSL_CIPHER			CURLE_SSL_CIPHER)
    (hashtable-set! table 'CURLE_SSL_CACERT			CURLE_SSL_CACERT)
    (hashtable-set! table 'CURLE_BAD_CONTENT_ENCODING		CURLE_BAD_CONTENT_ENCODING)
    (hashtable-set! table 'CURLE_LDAP_INVALID_URL		CURLE_LDAP_INVALID_URL)
    (hashtable-set! table 'CURLE_FILESIZE_EXCEEDED		CURLE_FILESIZE_EXCEEDED)
    (hashtable-set! table 'CURLE_USE_SSL_FAILED			CURLE_USE_SSL_FAILED)
    (hashtable-set! table 'CURLE_SEND_FAIL_REWIND		CURLE_SEND_FAIL_REWIND)
    (hashtable-set! table 'CURLE_SSL_ENGINE_INITFAILED		CURLE_SSL_ENGINE_INITFAILED)
    (hashtable-set! table 'CURLE_LOGIN_DENIED			CURLE_LOGIN_DENIED)
    (hashtable-set! table 'CURLE_TFTP_NOTFOUND			CURLE_TFTP_NOTFOUND)
    (hashtable-set! table 'CURLE_TFTP_PERM			CURLE_TFTP_PERM)
    (hashtable-set! table 'CURLE_REMOTE_DISK_FULL		CURLE_REMOTE_DISK_FULL)
    (hashtable-set! table 'CURLE_TFTP_ILLEGAL			CURLE_TFTP_ILLEGAL)
    (hashtable-set! table 'CURLE_TFTP_UNKNOWNID			CURLE_TFTP_UNKNOWNID)
    (hashtable-set! table 'CURLE_REMOTE_FILE_EXISTS		CURLE_REMOTE_FILE_EXISTS)
    (hashtable-set! table 'CURLE_TFTP_NOSUCHUSER		CURLE_TFTP_NOSUCHUSER)
    (hashtable-set! table 'CURLE_CONV_FAILED			CURLE_CONV_FAILED)
    (hashtable-set! table 'CURLE_CONV_REQD			CURLE_CONV_REQD)
    (hashtable-set! table 'CURLE_SSL_CACERT_BADFILE		CURLE_SSL_CACERT_BADFILE)
    (hashtable-set! table 'CURLE_REMOTE_FILE_NOT_FOUND		CURLE_REMOTE_FILE_NOT_FOUND)
    (hashtable-set! table 'CURLE_SSH				CURLE_SSH)
    (hashtable-set! table 'CURLE_SSL_SHUTDOWN_FAILED		CURLE_SSL_SHUTDOWN_FAILED)
    (hashtable-set! table 'CURLE_AGAIN				CURLE_AGAIN)
    (hashtable-set! table 'CURLE_SSL_CRL_BADFILE		CURLE_SSL_CRL_BADFILE)
    (hashtable-set! table 'CURLE_SSL_ISSUER_ERROR		CURLE_SSL_ISSUER_ERROR)
    (hashtable-set! table 'CURL_LAST				CURL_LAST)
    table))


(define multi-numeric-error-code-table
  (let ((table (make-eqv-hashtable)))
    (hashtable-set! table CURLM_OK			'CURLM_OK)
    (hashtable-set! table CURLM_CALL_MULTI_PERFORM	'CURLM_CALL_MULTI_PERFORM)
    (hashtable-set! table CURLM_BAD_HANDLE		'CURLM_BAD_HANDLE)
    (hashtable-set! table CURLM_BAD_EASY_HANDLE		'CURLM_BAD_EASY_HANDLE)
    (hashtable-set! table CURLM_OUT_OF_MEMORY		'CURLM_OUT_OF_MEMORY)
    (hashtable-set! table CURLM_INTERNAL_ERROR		'CURLM_INTERNAL_ERROR)
    (hashtable-set! table CURLM_BAD_SOCKET		'CURLM_BAD_SOCKET)
    (hashtable-set! table CURLM_UNKNOWN_OPTION		'CURLM_UNKNOWN_OPTION)
    (hashtable-set! table CURLM_LAST			'CURLM_LAST)
    table))

(define multi-symbolic-error-code-table
  (let ((table (make-eq-hashtable)))
    (hashtable-set! table 'CURLM_OK			CURLM_OK)
    (hashtable-set! table 'CURLM_CALL_MULTI_PERFORM	CURLM_CALL_MULTI_PERFORM)
    (hashtable-set! table 'CURLM_BAD_HANDLE		CURLM_BAD_HANDLE)
    (hashtable-set! table 'CURLM_BAD_EASY_HANDLE	CURLM_BAD_EASY_HANDLE)
    (hashtable-set! table 'CURLM_OUT_OF_MEMORY		CURLM_OUT_OF_MEMORY)
    (hashtable-set! table 'CURLM_INTERNAL_ERROR		CURLM_INTERNAL_ERROR)
    (hashtable-set! table 'CURLM_BAD_SOCKET		CURLM_BAD_SOCKET)
    (hashtable-set! table 'CURLM_UNKNOWN_OPTION		CURLM_UNKNOWN_OPTION)
    (hashtable-set! table 'CURLM_LAST			CURLM_LAST)
    table))


(define share-numeric-error-code-table
  (let ((table (make-eqv-hashtable)))
    (hashtable-set! table CURLSHE_OK		'CURLSHE_OK)
    (hashtable-set! table CURLSHE_BAD_OPTION	'CURLSHE_BAD_OPTION)
    (hashtable-set! table CURLSHE_IN_USE	'CURLSHE_IN_USE)
    (hashtable-set! table CURLSHE_INVALID	'CURLSHE_INVALID)
    (hashtable-set! table CURLSHE_NOMEM		'CURLSHE_NOMEM)
    (hashtable-set! table CURLSHE_LAST		'CURLSHE_LAST)
    table))

(define share-symbolic-error-code-table
  (let ((table (make-eq-hashtable)))
    (hashtable-set! table 'CURLSHE_OK		CURLSHE_OK)
    (hashtable-set! table 'CURLSHE_BAD_OPTION	CURLSHE_BAD_OPTION)
    (hashtable-set! table 'CURLSHE_IN_USE	CURLSHE_IN_USE)
    (hashtable-set! table 'CURLSHE_INVALID	CURLSHE_INVALID)
    (hashtable-set! table 'CURLSHE_NOMEM	CURLSHE_NOMEM)
    (hashtable-set! table 'CURLSHE_LAST		CURLSHE_LAST)
    table))



(define-enumeration curl-pause-symbol
  (RECV SEND ALL CONT)
  curl-pause-mask)

(define (%curl-pause-set->bitmask set)
  (apply bitwise-ior (map (lambda (symbol)
			    (case symbol
			      ((RECV)	CURLPAUSE_RECV)
			      ((SEND)	CURLPAUSE_SEND)
			      ((ALL)	CURLPAUSE_ALL)
			      ((CONT)	CURLPAUSE_CONT)))
		       (enum-set->list set))))


;;;; done

)

;;; end of file
