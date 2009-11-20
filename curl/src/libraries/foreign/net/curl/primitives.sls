;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/cURL
;;;Contents: marshaling interface
;;;Date: Mon Nov 16, 2009
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


(library (foreign net curl primitives)
  (export

    curl-version		curl-version-info
    curl-easy-setopt

    curl-make-read-callback	curl-make-write-callback

    (rename (curl_global_init		curl-global-init)
	    (curl_global_init_mem	curl-global-init-mem)
	    (curl_global_cleanup	curl-global-cleanup)

	    ;;(curl_version		curl-version)		;marshaled
	    ;;(curl_version_info	curl-version-info)	;marshaled

	    (curl_easy_strerror		curl-easy-strerror)
	    (curl_share_strerror	curl-share-strerror)
	    (curl_easy_pause		curl-easy-pause)
	    (curl_easy_init		curl-easy-init)
	    ;;(curl_easy_setopt		curl-easy-setopt)	marshaled
	    (curl_easy_perform		curl-easy-perform)
	    (curl_easy_cleanup		curl-easy-cleanup)
	    ;;(curl_easy_getinfo	curl-easy-getinfo)	;this is variadic
	    (curl_easy_duphandle	curl-easy-duphandle)
	    (curl_easy_reset		curl-easy-reset)
	    (curl_easy_recv		curl-easy-recv)
	    (curl_easy_send		curl-easy-send)

	    (curl_easy_escape		curl-easy-escape)
	    (curl_easy_unescape		curl-easy-unescape)
	    (curl_escape		curl-escape)
	    (curl_unescape		curl-unescape)

	    (curl_multi_init		curl-multi-init)
	    (curl_multi_add_handle	curl-multi-add-handle)
	    (curl_multi_remove_handle	curl-multi-remove-handle)
	    (curl_multi_fdset		curl-multi-fdset)
	    (curl_multi_perform		curl-multi-perform)
	    (curl_multi_cleanup		curl-multi-cleanup)
	    (curl_multi_info_read	curl-multi-info-read)
	    (curl_multi_strerror	curl-multi-strerror)
	    (curl_multi_socket		curl-multi-socket)
	    (curl_multi_socket_action	curl-multi-socket-action)
	    (curl_multi_socket_all	curl-multi-socket-all)
	    (curl_multi_timeout		curl-multi-timeout)
	    ;;curl_multi_setopt		curl-multi-setopt)	;this is variadic
	    (curl_multi_assign		curl-multi-assign)

	    ;;(curl_formadd		curl-formadd)		;this is variadic
	    (curl_formget		curl-formget)
	    (curl_formfree		curl-formfree)

	    (curl_free			curl-free)
	    (curl_getenv		curl-getenv)
	    (curl_getdate		curl-getdate)
	    (curl_slist_append		curl-slist-append)
	    (curl_slist_free_all	curl-slist-free-all)

	    (curl_share_init		curl-share-init)
	    ;;(curl_share_setopt	curl-share-setopt)	;this is variadic
	    (curl_share_cleanup		curl-share-cleanup)))
  (import (rnrs)
    (compensations)
    (foreign ffi)
    (only (foreign cstrings)
	  string->cstring/c
	  cstring->string
	  argv->strings)
    (foreign net curl record-types)
    (foreign net curl sizeof)
    (foreign net curl platform))


(define (curl-version)
  (cstring->string (curl_version)))

(define (curl-version-info)
  (let* ((struct*	(curl_version_info CURLVERSION_NOW))
	 (age		(struct-curl_version_info_data-age-ref struct*)))
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


(define (curl-easy-setopt handle option value)
  (with-compensations
    (cond ((= option CURLOPT_URL)
	   (curl_easy_setopt/void* handle option (string->cstring/c value)))

	  ((memv option (list CURLOPT_WRITEFUNCTION CURLOPT_WRITEFUNCTION))
	   (curl_easy_setopt/callback handle option value))

	  ((memv option (list CURLOPT_WRITEDATA CURLOPT_READDATA))
	   (curl_easy_setopt/void* handle option value))

	  ((memv option (list CURLOPT_VERBOSE CURLOPT_HEADER
			      CURLOPT_NOPROGRESS CURLOPT_NOSIGNAL))
	   (curl_easy_setopt/long handle option (if value 1 0)))

	  (else
	   (assertion-violation 'curl-easy-setopt
	     "invalid cURL option for handle" option)))))


(define (curl-make-write-callback scheme-function)
  (make-c-callback size_t
		   (lambda (buffer item-size item-count custom)
		     (guard (E (else 0))
		       (scheme-function buffer item-size item-count)))
		   (void* size_t size_t void*)))

(define (curl-make-read-callback scheme-function)
  (make-c-callback size_t
		   (lambda (buffer item-size item-count custom)
		     (guard (E (else 0))
		       (scheme-function buffer item-size item-count)))
		   (void* size_t size_t void*)))




;;;; done

)

;;; end of file
