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

    curl-version			curl-version-info

    (rename (<curl-handle>?		curl-handle?))
    curl-easy-init			curl-easy-duphandle
    curl-easy-cleanup			curl-easy-reset
    curl-easy-setopt			curl-easy-getinfo
    curl-easy-perform			curl-easy-pause
    curl-easy-recv/string		curl-easy-recv/bytevector
    curl-easy-send
    curl-easy-escape			curl-easy-unescape
    curl-easy-strerror			curl-share-strerror

    curl-multi-strerror

    curl-make-read-callback		curl-make-write-callback
    curl-make-ioctl-callback		curl-make-seek-callback
    curl-make-sockopt-callback		curl-make-opensocket-callback
    curl-make-progress-callback		curl-make-header-callback

    (rename (curl_global_init		curl-global-init)
	    (curl_global_init_mem	curl-global-init-mem)
	    (curl_global_cleanup	curl-global-cleanup)

	    (curl_escape		curl-escape)
	    (curl_unescape		curl-unescape)

	    (curl_multi_init		curl-multi-init)
	    (curl_multi_add_handle	curl-multi-add-handle)
	    (curl_multi_remove_handle	curl-multi-remove-handle)
	    (curl_multi_fdset		curl-multi-fdset)
	    (curl_multi_perform		curl-multi-perform)
	    (curl_multi_cleanup		curl-multi-cleanup)
	    (curl_multi_info_read	curl-multi-info-read)
	    ;;(curl_multi_strerror	curl-multi-strerror)	;marshaled
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
    (only (foreign memory)
	  malloc-small/c	malloc-block/c
	  malloc/c
	  bytevector->pointer	pointer->bytevector)
    (only (foreign cstrings)
	  string->cstring/c	cstring->string
	  strlen		argv->strings)
    (foreign net curl conditions)
    (foreign net curl enumerations)
    (foreign net curl record-types)
    (foreign net curl sizeof)
    (foreign net curl platform))


;;;; helpers

(define (%slist->strings slist*)
  (let loop ((slist*	slist*)
	     (engines	'()))
    (if (pointer-null? slist*)
	engines
      (loop (struct-curl_slist-next-ref slist*)
	    (cstring->string (struct-curl_slist-data-ref slist*))))))

;;*FIXME* !!!!
(define pointer-ref-c-size_t	pointer-ref-c-unsigned-int)


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


(define (curl-easy-init)
  (let ((handle* (curl_easy_init)))
    (if (pointer-null? handle*)
	(raise-curl-init-error 'curl-easy-init "error initialising cURL session handle")
      (make-<curl-handle> handle*))))

(define (curl-easy-duphandle handle)
  (let ((handle* (curl_easy_duphandle (<curl-handle>-pointer handle))))
    (if (pointer-null? handle*)
	(raise-curl-init-error 'curl-easy-duphandle "error duplicating cURL session handle")
      (make-<curl-handle> handle*))))

(define (curl-easy-cleanup handle)
  (curl_easy_cleanup (<curl-handle>-pointer handle)))

(define (curl-easy-reset handle)
  (curl_easy_reset (<curl-handle>-pointer handle)))

;;; --------------------------------------------------------------------

(define (curl-easy-perform handle)
  (let ((code (curl_easy_perform (<curl-handle>-pointer handle))))
    (if (= code CURLE_OK)
	code
      (raise-curl-easy-error 'curl-easy-perform code handle))))

(define (curl-easy-pause handle bitmask-set)
  (let ((code (curl_easy_pause (<curl-handle>-pointer handle)
			       (%curl-pause-set->bitmask bitmask-set))))
    (if (= code CURLE_OK)
	code
      (raise-curl-easy-error 'curl-easy-pause code handle))))

;;; --------------------------------------------------------------------

(define (curl-easy-send handle data)
  (with-compensations
    (let* ((buffer*	(if (string? data)
			    (string->cstring/c data)
			  (bytevector->pointer data malloc/c)))
	   (size	(if (string? data)
			    (strlen buffer*)
			  (bytevector-length data)))
	   (sent*	(malloc-small/c))
	   (code	(curl_easy_send (<curl-handle>-pointer handle) buffer* size sent*)))
      (if (= code CURLE_OK)
	  (pointer-ref-c-size_t sent* 0)
	(raise-curl-easy-error 'curl-easy-perform code handle)))))

(define (curl-easy-recv/string handle number-of-bytes)
  (with-compensations
    (let* ((buffer*	(malloc-block/c number-of-bytes))
	   (recv*	(malloc-small/c))
	   (code	(curl_easy_recv (<curl-handle>-pointer handle)
					buffer* number-of-bytes recv*)))
      (if (= code CURLE_OK)
	  (cstring->string buffer* (pointer-ref-c-size_t recv* 0))
	(raise-curl-easy-error 'curl-easy-perform code handle)))))

(define (curl-easy-recv/bytevector handle number-of-bytes)
  (with-compensations
    (let* ((buffer*	(malloc-block/c number-of-bytes))
	   (recv*	(malloc-small/c))
	   (code	(curl_easy_recv (<curl-handle>-pointer handle)
					buffer* number-of-bytes recv*)))
      (if (= code CURLE_OK)
	  (pointer->bytevector buffer* (pointer-ref-c-size_t recv* 0))
	(raise-curl-easy-error 'curl-easy-perform code handle)))))


(define (curl-easy-escape handle str)
  (with-compensations
    (let ((p (curl_easy_escape (<curl-handle>-pointer handle) (string->cstring/c str) 0)))
      (if (pointer-null? p)
	  (raise (condition (make-curl-error-condition)
			    (make-curl-handle-condition handle)
			    (make-who-condition 'curl-easy-escape)
			    (make-message-condition "error escaping a string as URL")))
	(begin
	  (push-compensation (curl_free p))
	  (cstring->string p))))))

(define (curl-easy-unescape handle str)
  (with-compensations
    (let* ((len*	(malloc-small/c))
	   (p		(curl_easy_unescape (<curl-handle>-pointer handle)
					    (string->cstring/c str) 0 len*)))
      (if (pointer-null? p)
	  (raise (condition (make-curl-error-condition)
			    (make-curl-handle-condition handle)
			    (make-who-condition 'curl-easy-unescape)
			    (make-message-condition "error unescaping a string as URL")))
	(begin
	  (push-compensation (curl_free p))
	  (cstring->string p))))))



(define curl-easy-setopt
  (let (($list-bool	(list CURLOPT_VERBOSE			CURLOPT_HEADER
			      CURLOPT_NOPROGRESS		CURLOPT_NOSIGNAL
			      CURLOPT_CONNECT_ONLY
			      ))

	($list-string	(list CURLOPT_URL))

	($list-callback	(list CURLOPT_READFUNCTION		CURLOPT_WRITEFUNCTION
			      CURLOPT_IOCTLFUNCTION		CURLOPT_SEEKFUNCTION
			      CURLOPT_SOCKOPTFUNCTION		CURLOPT_OPENSOCKETFUNCTION
			      CURLOPT_PROGRESSFUNCTION		CURLOPT_HEADERFUNCTION
			      ))

	($list-pointer	(list CURLOPT_WRITEDATA			CURLOPT_READDATA
			      CURLOPT_IOCTLDATA			CURLOPT_SEEKDATA
			      CURLOPT_SOCKOPTDATA		CURLOPT_OPENSOCKETDATA
			      CURLOPT_PROGRESSDATA		CURLOPT_HEADERDATA
			      CURLOPT_WRITEHEADER
			      ))

	)
    (lambda (handle option value)
      (let ((handle* (<curl-handle>-pointer handle)))
	(with-compensations
	  (cond ((memv option $list-string)
		 (curl_easy_setopt/void* handle* option (string->cstring/c value)))

		((memv option $list-callback)
		 (curl_easy_setopt/callback handle* option value))

		((memv option $list-pointer)
		 (curl_easy_setopt/void* handle* option value))

		((memv option $list-bool)
		 (curl_easy_setopt/long handle* option (if value 1 0)))

		(else
		 (assertion-violation 'curl-easy-setopt
		   "invalid cURL option for handle" handle option value))))))))


(define curl-easy-getinfo
  (let (($long-list	(list CURLINFO_RESPONSE_CODE		CURLINFO_HTTP_CONNECTCODE
			      CURLINFO_FILETIME			CURLINFO_REDIRECT_COUNT
			      CURLINFO_HEADER_SIZE		CURLINFO_REQUEST_SIZE
			      CURLINFO_SSL_VERIFYRESULT		CURLINFO_HTTPAUTH_AVAIL
			      CURLINFO_PROXYAUTH_AVAIL		CURLINFO_OS_ERRNO
			      CURLINFO_NUM_CONNECTS		CURLINFO_LASTSOCKET
			      CURLINFO_CONDITION_UNMET))
	($string-list	(list CURLINFO_EFFECTIVE_URL		CURLINFO_REDIRECT_URL
			      CURLINFO_CONTENT_TYPE		CURLINFO_PRIVATE
			      CURLINFO_PRIMARY_IP		CURLINFO_FTP_ENTRY_PATH))
	($double-list	(list CURLINFO_TOTAL_TIME		CURLINFO_NAMELOOKUP_TIME
			      CURLINFO_CONNECT_TIME		CURLINFO_APPCONNECT_TIME
			      CURLINFO_PRETRANSFER_TIME		CURLINFO_STARTTRANSFER_TIME
			      CURLINFO_REDIRECT_TIME		CURLINFO_SIZE_UPLOAD
			      CURLINFO_SIZE_DOWNLOAD		CURLINFO_SPEED_DOWNLOAD
			      CURLINFO_SPEED_UPLOAD		CURLINFO_CONTENT_LENGTH_DOWNLOAD
			      CURLINFO_CONTENT_LENGTH_UPLOAD))
	($slist-list	(list CURLINFO_SSL_ENGINES		CURLINFO_COOKIELIST)))
    (lambda (handle option)
      (let ((handle* (<curl-handle>-pointer handle)))
	(if (= option CURLINFO_CERTINFO)
	    (let* ((info*	(malloc-block/c sizeof-curl_certinfo))
		   (code	(curl_easy_getinfo handle* option info*)))
	      (unless (= code CURLE_OK)
		(raise-curl-easy-error 'curl-easy-getinfo code handle))
	      (let ((num-of-certs	(struct-curl_certinfo-num_of_certs-ref info*))
		    (slist**		(struct-curl_certinfo-certinfo-ref     info*))
		    (certinfo		'()))
		(do ((i 0 (+ 1 i)))
		    ((= i num-of-certs)
		     (reverse certinfo))
		  (set! certinfo (cons (%slist->strings (array-ref-c-pointer slist** i))
				       certinfo)))))
	  (let* ((output*	(malloc-small/c))
		 (code		(curl_easy_getinfo handle* option output*)))
	    (unless (= code CURLE_OK)
	      (raise-curl-easy-error 'curl-easy-getinfo code handle))
	    (with-compensations
	      (cond ((memv option $string-list)
		     (let ((cstr* (pointer-ref-c-pointer output* 0)))
		       (if (pointer-null? cstr*)
			   #f
			 (cstring->string cstr*))))

		    ((memv option $long-list)
		     (pointer-ref-c-signed-long output* 0))

		    ((memv option $double-list)
		     (pointer-ref-c-double output* 0))

		    ((memv option $slist-list)
		     (let ((slist*	(pointer-ref-c-double output* 0)))
		       (push-compensation (curl_slist_free_all slist*))
		       (%slist->strings slist*)))

		    (else
		     (assertion-violation 'curl-easy-getinfo
		       "invalid cURL option information request" handle option))))))))))


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

(define (curl-make-ioctl-callback scheme-function)
  (make-c-callback curlioerr
		   (lambda (handle size custom-data)
		     (guard (E (else -1))
		       (scheme-function handle size custom-data)))
		   (void* int void*)))

(define (curl-make-seek-callback scheme-function)
  (make-c-callback curlioerr
		   (lambda (instream offset whence)
		     (guard (E (else CURL_SEEKFUNC_FAIL))
		       (scheme-function instream offset whence)))
		   (void* curl_off_t int)))

(define (curl-make-sockopt-callback scheme-function)
  (make-c-callback curlioerr
		   (lambda (instream offset whence)
		     (guard (E (else 1))
		       (scheme-function instream offset whence)))
		   (void* curl_socket_t curlsocktype)))

(define (curl-make-opensocket-callback scheme-function)
  (make-c-callback curl_socket_t
		   (lambda (custom-pointer purpose address)
		     (guard (E (else CURL_SOCKET_BAD))
		       (scheme-function custom-pointer purpose)))
		   (void* curlsocktype void*)))

(define (curl-make-progress-callback scheme-function)
  (make-c-callback curl_socket_t
		   (lambda (custom-pointer dltotal dlnow ultotal ulnow)
		     (guard (E (else 1))
		       (scheme-function custom-pointer dltotal dlnow ultotal ulnow)))
		   (void* double double double double)))

(define (curl-make-header-callback scheme-function)
  (make-c-callback size_t
		   (lambda (custom-pointer item-size item-count stream)
		     (guard (E (else -1))
		       (scheme-function custom-pointer item-size item-count stream)))
		   (void* size_t size_t void*)))


(define (curl-easy-strerror code)
  (cstring->string (curl_easy_strerror code)))

(define (curl-share-strerror code)
  (cstring->string (curl_share_strerror code)))

(define (curl-multi-strerror code)
  (cstring->string (curl_multi_strerror code)))


;;;; done

)

;;; end of file
