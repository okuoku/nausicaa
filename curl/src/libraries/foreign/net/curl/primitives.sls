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

    ;; global initialisation
    (rename (curl_global_init		curl-global-init)
	    (curl_global_init_mem	curl-global-init-mem)
	    (curl_global_cleanup	curl-global-cleanup))

    ;; version
    curl-version			curl-version-info

    ;; easy session handlers
    curl-easy-handle?
    curl-easy-init			curl-easy-duphandle
    curl-easy-cleanup			curl-easy-reset
    curl-easy-setopt			curl-easy-getinfo
    curl-easy-perform			curl-easy-pause
    curl-easy-recv/string		curl-easy-recv/bytevector
    curl-easy-send			curl-easy-strerror

    ;; multi session handlers
    curl-multi-handle?
    curl-multi-init			curl-multi-cleanup
    curl-multi-add-handle		curl-multi-remove-handle
    curl-multi-fdset			curl-multi-info-read
    curl-multi-perform			curl-multi-socket-action
    curl-multi-setopt			curl-multi-assign
    curl-multi-timeout			curl-multi-strerror

    ;; HTTP POST
    curl-formadd
    (rename (curl_formget		curl-formget)
	    (curl_formfree		curl-formfree))

    ;; mutex/locking
    curl-shared-object?
    curl-share-init			curl-share-cleanup
    curl-share-setopt			curl-share-strerror

    ;; URL escaping
    curl-easy-escape			curl-easy-unescape
    (rename (curl_escape		curl-escape)
	    (curl_unescape		curl-unescape))

    ;; callback utilities
    curl-make-read-callback		curl-make-write-callback
    curl-make-ioctl-callback		curl-make-seek-callback
    curl-make-sockopt-callback		curl-make-opensocket-callback
    curl-make-progress-callback		curl-make-header-callback
    curl-make-debug-callback		curl-make-ssl-ctx-callback
    curl-make-conv-callback		curl-make-sshkey-callback

    ;; cURL linked list management
    strings->curl-slist
    (rename (curl_slist_append		curl-slist-append)
	    (curl_slist_free_all	curl-slist-free-all))

    ;; miscellaneous
    (rename (curl_free			curl-free)
	    (curl_getenv		curl-getenv)
	    (curl_getdate		curl-getdate)))
  (import (rnrs)
    (begin0)
    (compensations)
    (foreign ffi)
    (foreign memory)
    (foreign cstrings)
    (foreign net curl conditions)
    (foreign net curl enumerations)
    (rename (foreign net curl record-types)
	    (<curl-handle>-pointer	pointer)
	    (<curl-easy-handle>?	curl-easy-handle?)
	    (<curl-multi-handle>?	curl-multi-handle?)
	    (<curl-shared-object>?	curl-shared-object?))
    (foreign net curl sizeof)
    (foreign net curl platform))


;;;; slist

(define (%slist->strings slist*)
  (let loop ((slist*	slist*)
	     (engines	'()))
    (if (pointer-null? slist*)
	engines
      (loop (struct-curl_slist-next-ref slist*)
	    (cstring->string (struct-curl_slist-data-ref slist*))))))

(define (strings->curl-slist strings)
  (let loop ((strings	strings)
	     (slist*	pointer-null))
    (if (null? strings)
	slist*
      (let ((p (curl_slist_append slist* (string->cstring (car strings) primitive-malloc))))
	(if (pointer-null? p)
	    (begin
	      (unless (pointer-null? slist*)
		(curl_slist_free_all slist*))
	      (raise-out-of-memory 'strings->curl-slist #f))
	  (loop (cdr strings) p))))))


;;;; cURL version informations

(define (curl-version)
  (cstring->string (curl_version)))

(define (curl-version-info)
  (%struct-curl-version-info-data->record (curl_version_info CURLVERSION_NOW)))


;;;; easy initialisation and finalisation

(define (curl-easy-init)
  (let ((easy* (curl_easy_init)))
    (if (pointer-null? easy*)
	(raise (condition (make-curl-init-error-condition)
			  (make-who-condition 'curl-easy-init)
			  (make-message-condition "error initialising cURL session handle")))
      (make-<curl-easy-handle> easy*))))

(define (curl-easy-duphandle easy)
  (let ((easy* (curl_easy_duphandle (pointer easy))))
    (if (pointer-null? easy*)
	(raise (condition (make-curl-init-error-condition)
			  (make-who-condition 'curl-easy-duphandle)
			  (make-message-condition "error duplicating cURL session handle")
			  (make-curl-easy-handle-condition easy)))
      (make-<curl-easy-handle> easy*))))

(define (curl-easy-cleanup easy)
  (curl_easy_cleanup (pointer easy)))

(define (curl-easy-reset easy)
  (curl_easy_reset (pointer easy)))


;;;; easy performing

(define (curl-easy-perform easy)
  (let ((code (curl_easy_perform (pointer easy))))
    (if (= code CURLE_OK)
	code
      (raise (condition (make-curl-easy-action-error-condition)
			(make-who-condition 'curl-easy-perform)
			(make-curl-easy-message-condition code)
			(make-curl-easy-error-code-condition code)
			(make-curl-easy-handle-condition easy))))))

(define (curl-easy-pause easy bitmask-set)
  (let ((code (curl_easy_pause (pointer easy) (%curl-pause-set->bitmask bitmask-set))))
    (if (= code CURLE_OK)
	code
      (raise (condition (make-curl-easy-action-error-condition)
			(make-who-condition 'curl-easy-pause)
			(make-curl-easy-message-condition code)
			(make-curl-easy-error-code-condition code)
			(make-curl-easy-handle-condition easy))))))


;;;; easy raw sending and receiving

(define (curl-easy-send easy data)
  (with-compensations
    (let* ((buffer*	(if (string? data)
			    (string->cstring/c data)
			  (bytevector->pointer data malloc/c)))
	   (size	(if (string? data)
			    (strlen buffer*)
			  (bytevector-length data)))
	   (sent*	(malloc-small/c))
	   (code	(curl_easy_send (pointer easy) buffer* size sent*)))
      (if (= code CURLE_OK)
	  (pointer-ref-c-size_t sent* 0)
	(raise (condition (make-curl-action-error-condition)
			  (make-who-condition 'curl-easy-send)
			  (make-curl-easy-error-code-condition code)
			  (make-curl-easy-message-condition code)
			  (make-curl-easy-handle-condition easy)))))))

(define (curl-easy-recv/string easy number-of-bytes)
  (with-compensations
    (let* ((buffer*	(malloc-block/c number-of-bytes))
	   (recv*	(malloc-small/c))
	   (code	(curl_easy_recv (pointer easy)
					buffer* number-of-bytes recv*)))
      (if (= code CURLE_OK)
	  (cstring->string buffer* (pointer-ref-c-size_t recv* 0))
	(raise (condition (make-curl-action-error-condition)
			  (make-who-condition 'curl-easy-recv/string)
			  (make-curl-easy-error-code-condition code)
			  (make-curl-easy-message-condition code)
			  (make-curl-easy-handle-condition easy)))))))

(define (curl-easy-recv/bytevector easy number-of-bytes)
  (with-compensations
    (let* ((buffer*	(malloc-block/c number-of-bytes))
	   (recv*	(malloc-small/c))
	   (code	(curl_easy_recv (pointer easy)
					buffer* number-of-bytes recv*)))
      (if (= code CURLE_OK)
	  (pointer->bytevector buffer* (pointer-ref-c-size_t recv* 0))
	(raise (condition (make-curl-action-error-condition)
			  (make-who-condition 'curl-easy-recv/bytevector)
			  (make-curl-easy-error-code-condition code)
			  (make-curl-easy-message-condition code)
			  (make-curl-easy-handle-condition easy)))))))


;;;; easy options setting

(define curl-easy-setopt
  (let (($list-bool	(list CURLOPT_VERBOSE			CURLOPT_HEADER
			      CURLOPT_NOPROGRESS		CURLOPT_NOSIGNAL
			      CURLOPT_CONNECT_ONLY		CURLOPT_FAILONERROR
			      CURLOPT_DNS_USE_GLOBAL_CACHE	CURLOPT_TCP_NODELAY
			      CURLOPT_AUTOREFERER		CURLOPT_FOLLOWLOCATION
			      CURLOPT_UNRESTRICTED_AUTH		CURLOPT_PUT
			      CURLOPT_POST			CURLOPT_COOKIESESSION
			      CURLOPT_HTTPGET			CURLOPT_IGNORE_CONTENT_LENGTH
			      CURLOPT_HTTP_CONTENT_DECODING	CURLOPT_HTTP_TRANSFER_DECODING
			      CURLOPT_DIRLISTONLY		CURLOPT_APPEND
			      CURLOPT_FTP_USE_EPRT		CURLOPT_FTP_USE_EPSV
			      CURLOPT_FTP_CREATE_MISSING_DIRS	CURLOPT_FTP_SKIP_PASV_IP
			      CURLOPT_TRANSFERTEXT		CURLOPT_PROXY_TRANSFER_MODE
			      CURLOPT_CRLF			CURLOPT_FILETIME
			      CURLOPT_NOBODY			CURLOPT_UPLOAD
			      CURLOPT_TIMEOUT			CURLOPT_TIMEOUT_MS
			      CURLOPT_LOW_SPEED_LIMIT		CURLOPT_LOW_SPEED_TIME
			      CURLOPT_FRESH_CONNECT		CURLOPT_CONNECT_ONLY
			      CURLOPT_SSL_VERIFYPEER		CURLOPT_CERTINFO
			      CURLOPT_SSL_SESSIONID_CACHE))

	($list-long	(list CURLOPT_PROTOCOLS			CURLOPT_REDIR_PROTOCOLS
			      CURLOPT_PROXYPORT			CURLOPT_PROXYTYPE
			      CURLOPT_HTTPPROXYTUNNEL		CURLOPT_SOCKS5_GSSAPI_NEC
			      CURLOPT_LOCALPORT			CURLOPT_LOCALPORTRANGE
			      CURLOPT_DNS_CACHE_TIMEOUT		CURLOPT_BUFFERSIZE
			      CURLOPT_PORT			CURLOPT_ADDRESS_SCOPE
			      CURLOPT_NETRC			CURLOPT_HTTPAUTH
			      CURLOPT_PROXYAUTH			CURLOPT_MAXREDIRS
			      CURLOPT_POSTREDIR			CURLOPT_POSTFIELDSIZE
			      CURLOPT_HTTP_VERSION		CURLOPT_TFTP_BLKSIZE
			      CURLOPT_FTPPORT			CURLOPT_FTP_RESPONSE_TIMEOUT
			      CURLOPT_USE_SSL			CURLOPT_FTPSSLAUTH
			      CURLOPT_FTP_SSL_CCC		CURLOPT_FTP_FILEMETHOD
			      CURLOPT_RESUME_FROM		CURLOPT_INFILESIZE
			      CURLOPT_MAXFILESIZE		CURLOPT_TIMECONDITION
			      CURLOPT_MAXCONNECTS		CURLOPT_CONNECTTIMEOUT
			      CURLOPT_CONNECTTIMEOUT_MS		CURLOPT_IPRESOLVE
			      CURLOPT_SSLVERSION		CURLOPT_SSL_VERIFYHOST
			      CURLOPT_SSH_AUTH_TYPES		CURLOPT_NEW_FILE_PERMS
			      CURLOPT_NEW_DIRECTORY_PERMS))

	($list-off_t	(list CURLOPT_POSTFIELDSIZE_LARGE	CURLOPT_RESUME_FROM_LARGE
			      CURLOPT_INFILESIZE_LARGE		CURLOPT_MAXFILESIZE_LARGE
			      CURLOPT_MAX_SEND_SPEED_LARGE	CURLOPT_MAX_RECV_SPEED_LARGE))

	($list-string	(list CURLOPT_URL			CURLOPT_PROXY
			      CURLOPT_NOPROXY			CURLOPT_SOCKS5_GSSAPI_SERVICE
			      CURLOPT_INTERFACE			CURLOPT_NETRC_FILE
			      CURLOPT_USERPWD			CURLOPT_PROXYUSERPWD
			      CURLOPT_USERNAME			CURLOPT_PASSWORD
			      CURLOPT_PROXYUSERNAME		CURLOPT_PROXYPASSWORD
			      CURLOPT_ENCODING			CURLOPT_COPYPOSTFIELDS
			      CURLOPT_REFERER			CURLOPT_USERAGENT
			      CURLOPT_COOKIE			CURLOPT_COOKIEFILE
			      CURLOPT_COOKIEJAR			CURLOPT_COOKIELIST
			      CURLOPT_FTP_ALTERNATIVE_TO_USER	CURLOPT_FTP_ACCOUNT
			      CURLOPT_RANGE			CURLOPT_CUSTOMREQUEST
			      CURLOPT_SSLCERT			CURLOPT_SSLCERTTYPE
			      CURLOPT_SSLKEY			CURLOPT_SSLKEYTYPE
			      CURLOPT_KEYPASSWD			CURLOPT_SSLENGINE
			      CURLOPT_CAINFO			CURLOPT_ISSUERCERT
			      CURLOPT_CAPATH			CURLOPT_CRLFILE
			      CURLOPT_RANDOM_FILE		CURLOPT_EGDSOCKET
			      CURLOPT_SSL_CIPHER_LIST		CURLOPT_KRBLEVEL
			      CURLOPT_SSH_HOST_PUBLIC_KEY_MD5	CURLOPT_SSH_PUBLIC_KEYFILE
			      CURLOPT_SSH_PRIVATE_KEYFILE	CURLOPT_SSH_KNOWNHOSTS))

	($list-callback	(list CURLOPT_READFUNCTION		CURLOPT_WRITEFUNCTION
			      CURLOPT_IOCTLFUNCTION		CURLOPT_SEEKFUNCTION
			      CURLOPT_SOCKOPTFUNCTION		CURLOPT_OPENSOCKETFUNCTION
			      CURLOPT_PROGRESSFUNCTION		CURLOPT_HEADERFUNCTION
			      CURLOPT_DEBUGFUNCTION		CURLOPT_SSL_CTX_FUNCTION
			      CURLOPT_CONV_TO_NETWORK_FUNCTION	CURLOPT_CONV_FROM_NETWORK_FUNCTION
			      CURLOPT_CONV_FROM_UTF8_FUNCTION	CURLOPT_SSH_KEYFUNCTION))

	($list-pointer	(list CURLOPT_WRITEDATA			CURLOPT_READDATA
			      CURLOPT_IOCTLDATA			CURLOPT_SEEKDATA
			      CURLOPT_SOCKOPTDATA		CURLOPT_OPENSOCKETDATA
			      CURLOPT_PROGRESSDATA		CURLOPT_HEADERDATA CURLOPT_WRITEHEADER
			      CURLOPT_DEBUGDATA			CURLOPT_SSL_CTX_DATA
			      CURLOPT_ERRORBUFFER		CURLOPT_STDERR
			      CURLOPT_POSTFIELDS		CURLOPT_HTTPPOST
			      CURLOPT_HTTPHEADER		CURLOPT_HTTP200ALIASES
			      CURLOPT_QUOTE			CURLOPT_POSTQUOTE
			      CURLOPT_PREQUOTE			CURLOPT_PRIVATE
			      CURLOPT_SHARE			CURLOPT_TELNETOPTIONS)))

    (lambda (easy option value)
      (let ((easy* (pointer easy)))
	(with-compensations
	  (let ((code (cond ((memv option $list-string)
			     (curl_easy_setopt/void* easy* option (if value
								      (string->cstring/c value)
								    pointer-null)))

			    ((memv option $list-callback)
			     (curl_easy_setopt/callback easy* option value))

			    ((memv option $list-pointer)
			     (curl_easy_setopt/void* easy* option value))

			    ((memv option $list-bool)
			     (curl_easy_setopt/long easy* option (if value 1 0)))

			    ((memv option $list-long)
			     (curl_easy_setopt/long easy* option value))

			    ((memv option $list-off_t)
			     (curl_easy_setopt/off_t easy* option value))

			    (else
			     (assertion-violation 'curl-easy-setopt
			       "invalid cURL option for easy session handle" easy option value)))))
	    (if (= code CURLE_OK)
		code
	      (raise (condition (make-curl-error-condition)
				(make-who-condition 'curl-easy-setopt)
				(make-curl-easy-error-code-condition code)
				(make-curl-easy-message-condition code)
				(make-curl-easy-handle-condition easy))))))))))


;;;; easy information retrieving

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
    (lambda (easy option)
      (let ((easy* (pointer easy)))
	(if (= option CURLINFO_CERTINFO)
	    (let* ((info*	(malloc-block/c sizeof-curl_certinfo))
		   (code	(curl_easy_getinfo easy* option info*)))
	      (unless (= code CURLE_OK)
		(raise (condition (make-curl-error-condition)
				  (make-who-condition 'curl-easy-getinfo)
				  (make-curl-easy-error-code-condition code)
				  (make-curl-easy-message-condition code)
				  (make-curl-easy-handle-condition easy))))
	      (let ((num-of-certs	(struct-curl_certinfo-num_of_certs-ref info*))
		    (slist**		(struct-curl_certinfo-certinfo-ref     info*))
		    (certinfo		'()))
		(do ((i 0 (+ 1 i)))
		    ((= i num-of-certs)
		     (reverse certinfo))
		  (set! certinfo (cons (%slist->strings (array-ref-c-pointer slist** i))
				       certinfo)))))
	  (let* ((output*	(malloc-small/c))
		 (code		(curl_easy_getinfo easy* option output*)))
	    (unless (= code CURLE_OK)
	      (raise (condition (make-curl-error-condition)
				(make-who-condition 'curl-easy-getinfo)
				(make-curl-easy-error-code-condition code)
				(make-curl-easy-message-condition code)
				(make-curl-easy-handle-condition easy))))
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
		       "invalid cURL option information request" easy option))))))))))


;;;; HTTP POST composition

(define curl-formadd
  (case-lambda
   ((alist)
    (curl-formadd alist #f #f))
   ((alist first* last*)
    (with-compensations
      (let* ((len	(length alist))
	     (first**	(begin0-let ((p (malloc-small/c))) ;already sets memory to zero
			  (when first* (pointer-set-c-pointer! p 0 first*))))
	     (last**	(begin0-let ((p (malloc-small/c))) ;already sets memory to zero
			  (when last*  (pointer-set-c-pointer! p 0 last*))))
	     (array*	(malloc-block/c (sizeof-curl_forms-array len))))
	(let fill-array ((i	0)
			 (alist	alist))
	  (unless (null? alist)
	    (let ((p (array-ref-c-curl_forms array* i)))
	      (struct-curl_forms-option-set! p (caar alist))
	      (struct-curl_forms-value-set!  p (cdar alist)))
	    (fill-array (+ 1 i) (cdr alist))))
	(let* ((first*	(pointer-ref-c-pointer first** 0))
	       (last*	(pointer-ref-c-pointer last** 0))
	       (code	(compensate
			    (curl_formadd_1 first* last* CURLFORM_ARRAY array* CURLFORM_END)
			  (with
			   (curl_formfree first*)))))
	  (if (= code CURLE_OK)
	      (values first* last*)
	    (raise (condition (make-curl-error-condition)
			      (make-who-condition 'curl-formadd)
			      (make-curl-easy-error-code-condition code)
			      (make-curl-easy-message-condition code)
			      (make-irritants-condition alist))))))))))


;;;; multi interface

(define (curl-multi-init)
  (let ((multi* (curl_multi_init)))
    (if (pointer-null? multi*)
	(raise (condition (make-curl-init-error-condition)
			  (make-who-condition 'curl-multi-init)
			  (make-message-condition "error initialising cURL multi session handle")))
      (make-<curl-multi-handle> multi*))))

(define (curl-multi-cleanup multi)
  (let ((code (curl_multi_cleanup (pointer multi))))
    (if (= code CURLM_OK)
	code
      (raise (condition (make-curl-init-error-condition)
			(make-who-condition 'curl-multi-cleanup)
			(make-curl-multi-error-code-condition code)
			(make-curl-multi-message-condition code)
			(make-curl-multi-handle-condition multi))))))

(define (curl-multi-add-handle multi easy)
  (let ((code (curl_multi_add_handle (pointer multi) (pointer easy))))
    (if (= code CURLM_OK)
	code
      (raise (condition (make-curl-error-condition)
			(make-who-condition 'curl-multi-add-handle)
			(make-curl-multi-error-code-condition code)
			(make-curl-multi-message-condition code)
			(make-curl-multi-handle-condition multi)
			(make-curl-easy-handle-condition easy))))))

(define (curl-multi-remove-handle multi easy)
  (let ((code (curl_multi_remove_handle (pointer multi) (pointer easy))))
    (if (= code CURLM_OK)
	code
      (raise (condition (make-curl-init-error-condition)
			(make-who-condition 'curl-multi-remove-handle)
			(make-curl-multi-error-code-condition code)
			(make-curl-multi-message-condition code)
			(make-curl-multi-handle-condition multi)
			(make-curl-easy-handle-condition easy))))))

(define (curl-multi-fdset multi read-fdset* write-fdset* exc-fdset)
  (with-compensations
    (let* ((max-fd*	(malloc-small/c))
	   (code	(curl_multi_fdset (pointer multi) read-fdset* write-fdset* max-fd*)))
      (if (= code CURLM_OK)
	  (pointer-ref-c-signed-int max-fd* 0)
	(raise (condition (make-curl-action-error-condition)
			  (make-who-condition 'curl-multi-fdset)
			  (make-curl-multi-error-code-condition code)
			  (make-curl-multi-message-condition code)
			  (make-curl-multi-handle-condition multi)))))))

(define (curl-multi-perform multi)
  (with-compensations
    (let* ((running-handlers*	(malloc-small/c))
	   (code		(curl_multi_perform (pointer multi) running-handlers*)))
      (if (or (= code CURLM_OK) (= code CURLM_CALL_MULTI_PERFORM))
	  (values code (pointer-ref-c-signed-int running-handlers*))
	(raise (condition (make-curl-action-error-condition)
			  (make-who-condition 'curl-multi-perform)
			  (make-curl-multi-error-code-condition code)
			  (make-curl-multi-message-condition code)
			  (make-curl-multi-handle-condition multi)))))))

(define (curl-multi-info-read multi)
  ;;*NOTE*  The pointer  returned by  CURL_MULTI_INFO_READ  references a
  ;;structure whose  memory is  managed by  cURL, so we  DO NOT  have to
  ;;release it.
  ;;
  (with-compensations
    (let ((msgs-in-queue*	(malloc-small/c))
	  (multi*		(pointer multi)))
      (let loop ((messages	'())
		 (msg*		(curl_multi_info_read multi* msgs-in-queue*)))
	(if (pointer-null? msg*)
	    messages
	  (loop (cons (%struct-curlmsg->record msg*) messages)
		(curl_multi_info_read multi* msgs-in-queue*)))))))

(define (curl-multi-socket-action multi sockfd event-bitmask)
  (with-compensations
    (let* ((running*	(malloc-small/c))
	   (code	(curl_multi_socket_action (pointer multi) sockfd event-bitmask running*)))
      (if (or (= code CURLM_OK) (= code CURLM_CALL_MULTI_PERFORM))
	  (values code (pointer-ref-c-signed-int running* 0))
	(raise (condition (make-curl-action-error-condition)
			  (make-who-condition 'curl-multi-socket-action)
			  (make-curl-multi-error-code-condition code)
			  (make-curl-multi-message-condition code)
			  (make-curl-multi-handle-condition multi)))))))

(define (curl-multi-timeout multi)
  (with-compensations
    (let* ((timeout*	(malloc-small/c))
	   (code	(curl_multi_timeout (pointer multi) timeout*)))
      (if (= code CURLM_OK)
	  (pointer-ref-c-signed-int timeout* 0)
	(raise (condition (make-curl-action-error-condition)
			  (make-who-condition 'curl-multi-timeout)
			  (make-curl-multi-error-code-condition code)
			  (make-curl-multi-message-condition code)
			  (make-curl-multi-handle-condition multi)))))))

(define (curl-multi-assign multi sockfd custom-pointer)
  (let ((code (curl_multi_timeout (pointer multi) sockfd custom-pointer)))
    (if (= code CURLM_OK)
	code
      (raise (condition (make-curl-error-condition)
			(make-who-condition 'curl-multi-assign)
			(make-curl-multi-error-code-condition code)
			(make-curl-multi-message-condition code)
			(make-curl-multi-handle-condition multi))))))


;;;; multi options setting

(define curl-multi-setopt
  (let (($list-bool	(list CURLMOPT_PIPELINING))

	($list-long	(list CURLMOPT_MAXCONNECTS))

;;;	($list-off_t	(list))
;;;	($list-string	(list))

	($list-callback	(list CURLMOPT_SOCKETFUNCTION	CURLMOPT_TIMERFUNCTION))

	($list-pointer	(list CURLMOPT_SOCKETDATA	CURLMOPT_TIMERDATA)))

    (lambda (multi option value)
      (let ((multi* (pointer multi)))
	(with-compensations
	  (let ((code (cond ((memv option $list-callback)
			     (curl_multi_setopt/callback multi* option value))

;;;		((memv option $list-string)
;;;		 (curl_multi_setopt/void* multi* option (if value
;;;		                                            (string->cstring/c value)
;;;		                                          pointer-null)))

			    ((memv option $list-pointer)
			     (curl_multi_setopt/void* multi* option value))

			    ((memv option $list-bool)
			     (curl_multi_setopt/long multi* option (if value 1 0)))

			    ((memv option $list-long)
			     (curl_multi_setopt/long multi* option value))

;;;		((memv option $list-off_t)
;;;		 (curl_multi_setopt/off_t multi* option value))

			    (else
			     (assertion-violation 'curl-multi-setopt
			       "invalid cURL option for multi session handle" multi option value)))))
	    (if (= code CURLM_OK)
		code
	      (raise (condition (make-curl-error-condition)
				(make-who-condition 'curl-multi-setopt)
				(make-curl-multi-error-code-condition code)
				(make-curl-multi-message-condition code)
				(make-curl-multi-handle-condition multi))))))))))


;;;; callback constructors

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

(define (curl-make-debug-callback scheme-function)
  (make-c-callback int
		   (lambda (handle type data size custom-pointer)
		     (guard (E (else 0))
		       (scheme-function handle type data size custom-pointer)))
		   (void* curl_infotype char* size_t void*)))

(define (curl-make-ssl-ctx-callback scheme-function)
  (make-c-callback int
		   (lambda (handle ssl_ctx custom-pointer)
		     (guard (E (else -1))
		       (scheme-function handle ssl_ctx custom-pointer)))
		   (void* void* void*)))

(define (curl-make-conv-callback scheme-function)
  (make-c-callback CURLcode
		   (lambda (buffer len)
		     (guard (E (else CURLE_ABORTED_BY_CALLBACK))
		       (scheme-function buffer len)))
		   (char* size_t)))

(define (curl-make-sshkey-callback scheme-function)
  (make-c-callback int
		   (lambda (handle knownkey foundkey status custom-pointer)
		     (guard (E (else CURLKHSTAT_REJECT))
		       (scheme-function handle knownkey foundkey status custom-pointer)))
		   (void* void* void* curl_khmatch void*)))


;;;; URL escaping

(define (curl-easy-escape handle str)
  (with-compensations
    (let ((p (curl_easy_escape (pointer handle) (string->cstring/c str) 0)))
      (if (pointer-null? p)
	  (raise (condition (make-curl-error-condition)
			    (make-curl-easy-handle-condition handle)
			    (make-who-condition 'curl-easy-escape)
			    (make-message-condition "error escaping a string as URL")))
	(begin
	  (push-compensation (curl_free p))
	  (cstring->string p))))))

(define (curl-easy-unescape handle str)
  (with-compensations
    (let* ((len*	(malloc-small/c))
	   (p		(curl_easy_unescape (pointer handle) (string->cstring/c str) 0 len*)))
      (if (pointer-null? p)
	  (raise (condition (make-curl-error-condition)
			    (make-curl-easy-handle-condition handle)
			    (make-who-condition 'curl-easy-unescape)
			    (make-message-condition "error unescaping a string as URL")))
	(begin
	  (push-compensation (curl_free p))
	  (cstring->string p))))))


;;;; mutex/locking
;;
;;Use of this API makes sense only in multithreading applications.
;;

(define (curl-share-init)
  (let ((shared* (curl_share_init)))
    (if (pointer-null? shared*)
	(raise (condition (make-curl-error-condition)
			  (make-who-condition 'curl-share-init)
			  (make-message-condition "error initialising cURL shared object")))
      (make-<curl-shared-object> shared*))))

(define (curl-share-cleanup shared)
  (let ((code (curl_share_cleanup (<curl-shared-object>-pointer shared))))
    (if (= code CURLSHE_OK)
	code
      (raise (condition (make-curl-error-condition)
			(make-who-condition 'curl-share-cleanup)
			(make-curl-shared-object-condition shared)
			(make-curl-share-message-condition code))))))

(define curl-share-setopt
  (let (($list-long	(list CURLSHOPT_SHARE		CURLSHOPT_UNSHARE))

	($list-callback	(list CURLSHOPT_LOCKFUNC	CURLSHOPT_UNLOCKFUNC))

	($list-pointer	(list CURLSHOPT_USERDATA)))

    (lambda (shared option value)
      (let ((shared* (<curl-shared-object>-pointer shared)))
	(with-compensations
	  (let ((code (cond ((memv option $list-callback)
			     (curl_share_setopt/callback shared* option value))

			    ((memv option $list-pointer)
			     (curl_share_setopt/void* shared* option value))

			    ((memv option $list-long)
			     (curl_share_setopt/long shared* option value))

			    (else
			     (assertion-violation 'curl-share-setopt
			       "invalid cURL option for shared object" shared option value)))))
	    (if (= code CURLSHE_OK)
		code
	      (raise (condition (make-curl-error-condition)
				(make-who-condition 'curl-share-setopt)
				(make-curl-share-error-code-condition code)
				(make-curl-share-message-condition code)
				(make-curl-shared-object-condition shared))))))))))


;;;; error strings

(define (curl-easy-strerror code)
  (cstring->string (curl_easy_strerror code)))

(define (curl-share-strerror code)
  (cstring->string (curl_share_strerror code)))

(define (curl-multi-strerror code)
  (cstring->string (curl_multi_strerror code)))


;;;; done

)

;;; end of file
