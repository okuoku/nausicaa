;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/MHD
;;;Contents: bindings to foreign functions
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


(library (net mhd platform)
  (export
    MHD_set_panic_func
    MHD_start_daemon
    MHD_stop_daemon
    MHD_get_fdset
    MHD_get_timeout
    MHD_run

    MHD_get_connection_values
    MHD_set_connection_value
    MHD_lookup_connection_value
    MHD_queue_response

    MHD_create_response_from_callback
    MHD_create_response_from_data
    MHD_destroy_response
    MHD_add_response_header
    MHD_del_response_header
    MHD_get_response_headers
    MHD_get_response_header

    MHD_create_post_processor
    MHD_post_process
    MHD_destroy_post_processor

;;; MHD_get_connection_info
;;; MHD_get_daemon_info

;;;    MHD_get_version
    )
  (import (rnrs)
    (foreign ffi)
    (foreign ffi sizeof)
    (net mhd sizeof)
    (net mhd shared-object))


(define fd_set*			'pointer)
(define int*			'pointer)
(define unsigned-long-long*	'pointer)


(define-c-functions mhd-shared-object

  (MHD_set_panic_func
   (void MHD_set_panic_func (callback pointer)))

  (MHD_start_daemon
   (MHD_Daemon* MHD_start_daemon (unsigned-int unsigned-short
					       MHD_AcceptPolicyCallback
					       void*
					       MHD_AccessHandlerCallback
					       void*
					       ;;This is for use with MHD_OPTION_ARRAY.
					       signed-int pointer signed-int)))

  (MHD_stop_daemon
   (void MHD_stop_daemon (MHD_Daemon*)))

  (MHD_get_fdset
   (int MHD_get_fdset (MHD_Daemon* fd_set* fd_set* fd_set* int*)))

  (MHD_get_timeout
   (int MHD_get_timeout (MHD_Daemon* unsigned-long-long*)))

  (MHD_run
   (int MHD_run (MHD_Daemon*))))

(define-c-functions mhd-shared-object

  (MHD_get_connection_values
   (int MHD_get_connection_values (MHD_Connection* MHD_ValueKind MHD_KeyValueIterator void*)))

  (MHD_set_connection_value
   (int MHD_set_connection_value (MHD_Connection* MHD_ValueKind char* char*)))

  (MHD_lookup_connection_value
   (char* MHD_lookup_connection_value (MHD_Connection* MHD_ValueKind char*)))

  (MHD_queue_response
   (int MHD_queue_response (MHD_Connection* unsigned-int MHD_Response*))))

(define-c-functions mhd-shared-object

  (MHD_create_response_from_callback
   (MHD_Response* MHD_create_response_from_callback
		  (uint64_t size_t MHD_ContentReaderCallback void* MHD_ContentReaderFreeCallback)))

  (MHD_create_response_from_data
   (MHD_Response* MHD_create_response_from_data (size_t void* int int)))

  (MHD_destroy_response
   (void MHD_destroy_response (MHD_Response*)))

  (MHD_add_response_header
   (int MHD_add_response_header (MHD_Response* char* char*)))

  (MHD_del_response_header
   (int MHD_del_response_header (MHD_Response* char* char*)))

  (MHD_get_response_headers
   (int MHD_get_response_headers (MHD_Response* MHD_KeyValueIterator void*)))

  (MHD_get_response_header
   (char* MHD_get_response_header (MHD_Response* char*))))

(define-c-functions mhd-shared-object

  (MHD_create_post_processor
   (MHD_PostProcessor* MHD_create_post_processor (MHD_Connection* size_t MHD_PostDataIterator void*)))

  (MHD_post_process
   (int MHD_post_process (MHD_PostProcessor* char* size_t)))

  (MHD_destroy_post_processor
   (int MHD_destroy_post_processor (MHD_PostProcessor*))))

#;(define-c-functions mhd-shared-object

;;; This is variadic
;;;
  ;; (MHD_get_connection_info
  ;;  (MHD_ConnectionInfo* MHD_get_connection_info (MHD_Connection* MHD_ConnectionInfoType ...)))


;;; This is variadic
;;;
  ;; (MHD_get_daemon_info
  ;;  (MHD_DaemonInfo* MHD_get_daemon_info (MHD_Daemon* MHD_DaemonInfoType ...)))

  )

;;;This is in the header file, but  then it is not present in the shared
;;;library version 0.4.4.
;;;
;;; (define-c-functions mhd-shared-object
;;;
;;;   (MHD_get_version
;;;    (char* MHD_get_version (void))))


;;;; done

)

;;; end of file
