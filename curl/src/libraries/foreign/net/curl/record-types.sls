;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/cURL
;;;Contents: record type definitions
;;;Date: Fri Nov 20, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (foreign net curl record-types)
  (export

    <curl-handle>			<curl-handle-rtd>
    make-<curl-handle>			<curl-handle>?
    <curl-handle>-pointer

    <curl-easy-handle>			<curl-easy-handle-rtd>
    make-<curl-easy-handle>		<curl-easy-handle>?

    <curl-multi-handle>			<curl-multi-handle-rtd>
    make-<curl-multi-handle>		<curl-multi-handle>?

    <curl-shared-object>		<curl-shared-object-rtd>
    make-<curl-shared-object>		<curl-shared-object>?
    <curl-shared-object>-pointer

    <curl-version-info>			<curl-version-info-rtd>
    make-<curl-version-info>		<curl-version-info>?
    <curl-version-info>-age
    <curl-version-info>-version
    <curl-version-info>-version-num
    <curl-version-info>-host
    <curl-version-info>-features
    <curl-version-info>-ssl-version
    <curl-version-info>-ssl-version-num
    <curl-version-info>-libz-version
    <curl-version-info>-protocols
    <curl-version-info>-ares
    <curl-version-info>-ares-num
    <curl-version-info>-libidn
    <curl-version-info>-iconv
    <curl-version-info>-libssh-version
    %struct-curl-version-info-data->record

    <curl-message>			<curl-message-rtd>
    make-<curl-message>			<curl-message>?
    %struct-curlmsg->record)
  (import (rnrs)
    (only (foreign ffi pointers) pointer-null?)
    (only (foreign cstrings) cstring->string argv->strings)
    (foreign net curl platform)
    (foreign net curl sizeof))


(define-record-type <curl-handle>
  (fields (immutable pointer)))

(define <curl-handle-rtd>
  (record-type-descriptor <curl-handle>))

(define-record-type <curl-easy-handle>
  (parent <curl-handle>))

(define <curl-easy-handle-rtd>
  (record-type-descriptor <curl-easy-handle>))

(define-record-type <curl-multi-handle>
  (parent <curl-handle>))

(define <curl-multi-handle-rtd>
  (record-type-descriptor <curl-multi-handle>))

(define-record-type <curl-shared-object>
  (fields (immutable pointer)))

(define <curl-shared-object-rtd>
  (record-type-descriptor <curl-shared-object>))


(define-record-type <curl-version-info>
  (fields (immutable age)
	  (immutable version)
	  (immutable version-num)
	  (immutable host)
	  (immutable features)
	  (immutable ssl-version)
	  (immutable ssl-version-num)
	  (immutable libz-version)
	  (immutable protocols)
	  (immutable ares)
	  (immutable ares-num)
	  (immutable libidn)
	  (immutable iconv)
	  (immutable libssh-version)))

(define <curl-version-info-rtd>
  (record-type-descriptor <curl-version-info>))

(define (%struct-curl-version-info-data->record struct*)
  (let ((age (struct-curl_version_info_data-age-ref struct*)))
    (make-<curl-version-info>

     age

     (let ((p (struct-curl_version_info_data-version-ref struct*)))
       (if (pointer-null? p) #f (cstring->string p)))

     (struct-curl_version_info_data-version_num-ref struct*)

     (let ((p (struct-curl_version_info_data-host-ref struct*)))
       (if (pointer-null? p) #f (cstring->string p)))

     (let ((bitmask	(struct-curl_version_info_data-features-ref struct*))
	   (features	'()))
       (when (bitwise-ior bitmask CURL_VERSION_IPV6)
	 (set! features (cons 'IPV6 features)))
       (when (bitwise-ior bitmask CURL_VERSION_KERBEROS4)
	 (set! features (cons 'KERBEROS4 features)))
       (when (bitwise-ior bitmask CURL_VERSION_SSL)
	 (set! features (cons 'SSL features)))
       (when (bitwise-ior bitmask CURL_VERSION_LIBZ)
	 (set! features (cons 'LIBZ features)))
       (when (bitwise-ior bitmask CURL_VERSION_NTLM)
	 (set! features (cons 'NTLM features)))
       (when (bitwise-ior bitmask CURL_VERSION_GSSNEGOTIATE)
	 (set! features (cons 'GSSNEGOTIATE features)))
       (when (bitwise-ior bitmask CURL_VERSION_DEBUG)
	 (set! features (cons 'DEBUG features)))
       (when (bitwise-ior bitmask CURL_VERSION_ASYNCHDNS)
	 (set! features (cons 'ASYNCHDNS features)))
       (when (bitwise-ior bitmask CURL_VERSION_SPNEGO)
	 (set! features (cons 'SPNEGO features)))
       (when (bitwise-ior bitmask CURL_VERSION_LARGEFILE)
	 (set! features (cons 'LARGEFILE features)))
       (when (bitwise-ior bitmask CURL_VERSION_IDN)
	 (set! features (cons 'IDN features)))
       (when (bitwise-ior bitmask CURL_VERSION_SSPI)
	 (set! features (cons 'SSPI features)))
       (when (bitwise-ior bitmask CURL_VERSION_CONV)
	 (set! features (cons 'CONV features)))
       (when (bitwise-ior bitmask CURL_VERSION_CURLDEBUG)
	 (set! features (cons 'CURLDEBUG features)))
       features)

     (let ((p (struct-curl_version_info_data-ssl_version-ref struct*)))
       (if (pointer-null? p) #f (cstring->string p)))

     (struct-curl_version_info_data-ssl_version_num-ref struct*)

     (let ((p (struct-curl_version_info_data-libz_version-ref struct*)))
       (if (pointer-null? p) #f (cstring->string p)))

     (let ((p (struct-curl_version_info_data-protocols-ref struct*)))
       (if (pointer-null? p) #f (map string->symbol (argv->strings p))))

     (if (<= 1 age)
	 (let ((p (struct-curl_version_info_data-ares-ref struct*)))
	   (if (pointer-null? p) #f (cstring->string p)))
       #f)

     (if (<= 1 age)
	 (struct-curl_version_info_data-ares_num-ref struct*)
       #f)

     (if (<= 2 age)
	 (let ((p (struct-curl_version_info_data-libidn-ref struct*)))
	   (if (pointer-null? p) #f (cstring->string p)))
       #f)

     (if (<= 3 age)
	 (struct-curl_version_info_data-iconv_ver_num-ref struct*)
       #f)

     (if (<= 3 age)
	 (let ((p (struct-curl_version_info_data-libssh_version-ref struct*)))
	   (if (pointer-null? p) #f (cstring->string p)))
       #f))))


(define-record-type <curl-message>
  (fields (immutable code)
	  (immutable handle)
	  (immutable result)))

(define <curl-message-rtd>
  (record-type-descriptor <curl-message>))

(define (%struct-curlmsg->record msg*)
  (make-<curl-message> 'DONE	;this is the only code in version 7.19.7 of cURL
		       (make-<curl-handle> (struct-CURLMsg-easy_handle-ref msg*))
		       (struct-CURLMsg-data.result-ref msg*)))


;;;; done

)

;;; end of file
