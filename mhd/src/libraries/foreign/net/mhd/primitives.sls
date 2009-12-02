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
    (rename (MHD_start_daemon			mhd-start-daemon)
	    (MHD_stop_daemon			mhd-stop-daemon)
	    (MHD_get_fdset			mhd-get-fdset)
	    (MHD_get_timeout			mhd-get-timeout)
	    (MHD_run				mhd-run)

	    (MHD_get_connection_values		mhd-get-connection-values)
	    (MHD_set_connection_value		mhd-set-connection-value)
	    (MHD_lookup_connection_value	mhd-lookup-connection-value)
	    (MHD_queue_response			mhd-queue-response)

	    (MHD_create_response_from_callback	mhd-create-response-from-callback)
	    (MHD_create_response_from_data	mhd-create-response-from-data)
	    (MHD_destroy_response		mhd-destroy-response)
	    (MHD_add_response_header		mhd-add-response-header)
	    (MHD_del_response_header		mhd-del-response-header)
	    (MHD_get_response_headers		mhd-get-response-headers)
	    (MHD_get_response_header		mhd-get-response-header)

	    (MHD_create_post_processor		mhd-create-post-processor)
	    (MHD_post_process			mhd-post-process)
	    (MHD_destroy_post_processor		mhd-destroy-post-processor)

;;;	(MHD_get_connection_info	 mhd-get-connection-info)
;;;	(MHD_get_daemon_info		mhd-get-daemon-info)

;;;	    (MHD_get_version			mhd-get-version)
	    ))
  (import (rnrs)
    (compensations)
    (foreign ffi)
    (foreign memory)
    (foreign cstrings)
    (foreign net mhd platform)
    (foreign net mhd sizeof))





;;;; done

)

;;; end of file
