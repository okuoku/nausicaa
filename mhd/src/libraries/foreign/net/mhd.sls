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


(library (foreign net mhd)
  (export
    mhd-start-daemon
    mhd-stop-daemon
    mhd-get-fdset
    mhd-get-timeout
    mhd-run

    mhd-get-connection-values
    mhd-set-connection-value
    mhd-lookup-connection-value
    mhd-queue-response

    mhd-create-response-from-callback
    mhd-create-response-from-data
    mhd-destroy-response
    mhd-add-response-header
    mhd-del-response-header
    mhd-get-response-headers
    mhd-get-response-header

    mhd-create-post-processor
    mhd-post-process
    mhd-destroy-post-processor

;;; mhd-get-connection-info
;;; mhd-get-daemon-info

;;; mhd-get-version
    )
  (import (rnrs)
    (foreign net mhd primitives)
    (foreign net mhd sizeof))


;;;; code



;;;; done

)

;;; end of file
