;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/MHD
;;;Contents: primitive functions
;;;Date: Wed Dec  2, 2009
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


(library (foreign net mhd primitives)
  (export

    ;; daemon
    mhd-start-daemon			mhd-stop-daemon
    mhd-daemon?				mhd-run
    mhd-get-fdset			mhd-get-timeout

    make-mhd-accept-policy-callback	make-mhd-access-contents-callback

    ;; connection
    mhd-connection?
    mhd-get-connection-values		mhd-set-connection-value
    mhd-lookup-connection-value

    make-mhd-header-callback

    ;; response
    mhd-create-response-from-callback	mhd-create-response-from-data
    mhd-queue-response			mhd-destroy-response
    mhd-response?
    mhd-add-response-header		mhd-del-response-header
    mhd-get-response-headers		mhd-get-response-header

    make-mhd-content-reader-callback
    make-mhd-content-reader-free-callback

    ;; HTTP POST
    mhd-create-post-processor		mhd-destroy-post-processor
    mhd-post-process			mhd-post-processor?

;;; mhd-get-connection-info
;;; mhd-get-daemon-info

;;; mhd-get-version
    )
  (import (rnrs)
    (compensations)
    (foreign ffi)
    (foreign memory)
    (foreign cstrings)
    (rename (foreign net mhd record-types)
	    (<mhd-pointer-wrapper>-pointer	pointer)
	    (<mhd-daemon>?			mhd-daemon?)
	    (<mhd-connection>?			mhd-connection?)
	    (<mhd-response>?			mhd-response?)
	    (<mhd-post-processor>?		mhd-post-processor?))
    (foreign net mhd enumerations)
    (foreign net mhd platform)
    (foreign net mhd sizeof))


;;;; helpers

(define (%socket-port? obj)
  (and (integer?  obj)
       (exact?    obj)
       (positive? obj)))


;;;; daemon

(define (mhd-start-daemon flags-set port accept-policy-callback access-handler-callback)
  (assert (%mhd-flags-set? flags-set))
  (assert (%socket-port?   port))
  (assert (pointer?        access-handler-callback))
  (let* ((flags		(%mhd-flags-set->flags flags-set))
	 (daemon*	(MHD_start_daemon flags
					  port
					  (if accept-policy-callback
					      accept-policy-callback
					    pointer-null)
					  pointer-null
					  access-handler-callback pointer-null
					  MHD_OPTION_END)))
    (if (pointer-null? daemon*)
	(error 'mhd-start-daemon
	  "unable to initialise MHD daemon"
	  flags-set port accept-policy-callback access-handler-callback)
      (make-<mhd-daemon> daemon*))))

(define (mhd-stop-daemon daemon)
  (assert (mhd-daemon? daemon))
  (MHD_stop_daemon (pointer daemon)))

(define (mhd-run daemon)
  (assert (mhd-daemon? daemon))
  (unless (= MHD_YES (MHD_run (pointer daemon)))
    (error 'mhd-run "error running MHD daemon" daemon)))

(define (make-mhd-accept-policy-callback scheme-function)
  (make-c-callback*
   int
   (lambda (custom-pointer sockaddr.ptr sockaddr.len)
     (guard (E (else MHD_NO))
       (scheme-function (make-<memblock> sockaddr.ptr sockaddr.len #f))))
   (void* void* socklen_t)))

(define (make-mhd-access-contents-callback scheme-function)
  (make-c-callback*
   int
   (lambda (unused-custom-pointer connection*
            url-cstr method-cstr version-cstr
	    upload-data.ptr upload-data-len.ptr
	    custom-connection-pointer*)
     (guard (E (else MHD_NO))
       (scheme-function (make-<mhd-connection> connection*)
			(cstring->string url-cstr)
			(string->symbol (cstring->string method-cstr))
			(string->symbol version-cstr)
			upload-data.ptr
			upload-data-len.ptr
			(and (pointer-null? (pointer-ref-c-pointer custom-connection-pointer* 0))
			     (begin
			       (pointer-set-c-pointer! custom-connection-pointer* 0 (integer->pointer 1))
			       #t)))))
   (void* void* char* char* char* char* void* void*)))

(define (mhd-get-fdset daemon read-fd-set* write-fd-set* except-fd-set*)
  (assert (mhd-daemon? daemon))
  (assert (pointer? read-fd-set*))
  (assert (pointer? write-fd-set*))
  (assert (pointer? except-fd-set*))
  (with-compensations
    (let ((max-fd*	(malloc-small/c)))
      (if (= MHD_YES (MHD_get_fdset (pointer daemon)
				    read-fd-set* write-fd-set* except-fd-set*
				    max-fd*))
	  (pointer-ref-c-signed-int max-fd* 0)
	(error 'mhd-get-fdset "error getting MHD daemon file descriptor sets" daemon)))))

(define (mhd-get-timeout daemon)
  (assert (mhd-daemon? daemon))
  (with-compensations
    (let ((timeout* (malloc-small/c)))
      (if (= MHD_YES (MHD_get_timeout (pointer daemon) timeout*))
	  (pointer-ref-c-unsigned-long-long timeout* 0)
	(error 'mhd-get-timeout "error getting MHD daemon timeout" daemon)))))


;;;; connection

(define (mhd-get-connection-values connection header-kind header-callback)
  (assert (mhd-connection? connection))
  (let ((code (MHD_get_connection_values (pointer connection) header-kind
					 header-callback pointer-null)))
    (if (= code MHD_NO)
	(error 'mhd-get-connection-values
	  "error retrieving header values for MHD daemon"
	  connection header-kind header-callback)
      code)))

(define (make-mhd-header-callback scheme-function)
  (make-c-callback*
   int
   (lambda (unused-custom-pointer header-kind header-name* header-value*)
     (guard (E (else MHD_NO))
       (scheme-function header-kind (cstring->string header-name*) (cstring->string header-value*))))
   (void* MHD_ValueKind char* char*)))

(define (mhd-set-connection-value connection kind key value)
  (assert (mhd-connection? connection))
  (assert (string? key))
  (assert (string? value))
  (unless (= MHD_YES (MHD_set_connection_value (pointer connection)
					       kind
					       (string->cstring/c key)
					       (string->cstring/c value)))
    (error 'mhd-set-connectin-value
      "error setting header value for MHD daemon"
      connection kind key value)))

(define (mhd-lookup-connection-value connection header-kind key malloc)
  (assert (mhd-connection? connection))
  (assert (string? key))
  (with-compensations
    (let ((value* (MHD_lookup_connection_value (pointer connection) header-kind
					       (string->cstring key malloc))))
      (if (pointer-null? value*)
	  #f
	(cstring->string value*)))))


;;;; response

(define (mhd-create-response-from-callback content-size block-size
					   content-reader-callback content-reader-free-callback)
  (assert (pointer? content-reader-callback))
  (assert (pointer? content-reader-free-callback))
  (let ((response* (MHD_create_response_from_callback content-size block-size
						      content-reader-callback pointer-null
						      content-reader-free-callback)))
    (if (pointer-null? response*)
	(error 'mhd-create-response-from-callback
	  "error creating response for MHD daemon"
	  content-size block-size content-reader-callback content-reader-free-callback)
      (make-<mhd-response> response*))))

(define (make-mhd-content-reader-callback scheme-function)
  (make-c-callback*
   int
   (lambda (unused-custom-pointer position-in-input-buffer output-buffer* max-bytes)
     (guard (E (else -1))
       (scheme-function position-in-input-buffer output-buffer* max-bytes)))
   (void* uint64_t char* int)))

(define (make-mhd-content-reader-free-callback scheme-function)
  (make-c-callback* void* (lambda (unused-custom-pointer)
			    (guard (E (else pointer-null))
			      (scheme-function)))
		    (void*)))

;;; --------------------------------------------------------------------

(define (mhd-create-response-from-data data must-free must-copy)
  (assert (<memblock>? data))
  (let ((response* (MHD_create_response_from_data (<memblock>-size    data)
						  (<memblock>-pointer data)
						  must-free must-copy)))
    (if (pointer-null? response*)
	(error 'mhd-create-response-from-data
	  "error creating response for MHD daemon"
	  data must-free must-copy)
      (make-<mhd-response> response*))))

;;; --------------------------------------------------------------------

(define (mhd-queue-response connection status-code response)
  (assert (mhd-connection? connection))
  (assert (mhd-response?   response))
  (unless (= MHD_YES (MHD_queue_response (pointer connection) status-code (pointer response)))
    (error 'mhd-queue-response
      "error enqueuing response to MHD daemon"
      connection status-code response)))

(define (mhd-destroy-response response)
  (assert (mhd-response?  response))
  (MHD_destroy_response (pointer response)))

;;; --------------------------------------------------------------------

(define (mhd-add-response-header response key value)
  (assert (mhd-response? response))
  (assert (string? key))
  (assert (string? value))
  (with-compensations
    (unless (= MHD_YES (MHD_add_response_header (pointer response)
						(string->cstring/c key)
						(string->cstring/c value)))
      (error 'mhd-add-response-header
	"error adding header to response for MHD daemon"
	response key value))))

(define (mhd-del-response-header response key value)
  (assert (mhd-response? response))
  (assert (string? key))
  (assert (string? value))
  (with-compensations
    (unless (= MHD_YES (MHD_del_response_header (pointer response)
						(string->cstring/c key)
						(string->cstring/c value)))
      (error 'mhd-del-response-header
	"error deleting header from response for MHD daemon"
	response key value))))

(define (mhd-get-response-headers response header-callback)
  (assert (mhd-response? response))
  (assert (pointer? header-callback))
  (let ((code (MHD_get_response_headers (pointer response) header-callback pointer-null)))
    (if (= code MHD_NO)
	(error 'mhd-get-response-headers
	  "error querying headers in response object for MHD daemon"
	  response header-callback)
      code)))

(define (mhd-get-response-header response key)
  (assert (mhd-response? response))
  (assert (string? key))
  (with-compensations
    (let ((cstr (MHD_get_response_header (pointer response) (string->cstring/c key))))
      (if (pointer-null? cstr)
	  #f
	(cstring->string cstr)))))


;;;; HTTP POST processor

(define (mhd-create-post-processor connection buffer-size post-data-callback)
  (assert (mhd-connection? connection))
  (assert (pointer? post-data-callback))
  (let ((post-processor* (MHD_create_post_processor (pointer connection) buffer-size post-data-callback)))
    (if (pointer-null? post-processor*)
	(error 'mhd-create-post-processor
	  "error creating HTTP POST data processor for MHD daemon"
	  connection buffer-size post-data-callback)
      (make-<mhd-post-processor> post-processor*))))

(define (mhd-destroy-post-processor post-processor)
  (assert (mhd-post-processor? post-processor))
  (MHD_destroy_post_processor (pointer post-processor)))

(define (mhd-post-process post-processor data.ptr data.len)
  (assert (mhd-post-processor? post-processor))
  (unless (= MHD_YES (MHD_post_process (pointer post-processor) data.ptr data.len))
    (error 'mhd-post-process
      "error processing HTTP POST data for MHD daemon"
      post-processor data.ptr data.len)))


;;;; miscellaneous

;;; mhd-get-connection-info
;;; mhd-get-daemon-info



;;;; done

)

;;; end of file
