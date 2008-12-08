;;;
;;;Part of: Nausicaa/Zlib
;;;Contents: Zlib interface for R6RS Scheme
;;;Date: Sun Dec  7, 2008
;;;Time-stamp: <2008-12-08 13:49:58 marco>
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
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



;;;; setup

(library (zlib)
  (export

    deflateInit		deflate		deflateEnd
    inflateInit		inflate		inflateEnd
    zlibVersion		zError

    ;;stream structure accessors
    zstream-next_in-set!	zstream-next_in-ref
    zstream-avail_in-set!	zstream-avail_in-ref
    zstream-total_in-set!	zstream-total_in-ref

    zstream-next_out-set!	zstream-next_out-ref
    zstream-avail_out-set!	zstream-avail_out-ref
    zstream-total_out-set!	zstream-total_out-ref

    zstream-msg-set!		zstream-msg-ref
    zstream-data_type-set!	zstream-data_type-ref
    zstream-adler-set!		zstream-adler-ref

    zstream-zalloc-set!		zstream-zalloc-ref
    zstream-zfree-set!		zstream-zfree-ref
    zstream-opaque-set!		zstream-opaque-ref

    ;; constants
    sizeof-zstream

    ZLIB_VERNUM

    Z_NO_FLUSH		Z_PARTIAL_FLUSH		Z_SYNC_FLUSH
    Z_FULL_FLUSH	Z_FINISH		Z_BLOCK

    Z_OK		Z_STREAM_END		Z_NEED_DICT
    Z_ERRNO		Z_STREAM_ERROR		Z_DATA_ERROR
    Z_MEM_ERROR		Z_BUF_ERROR		Z_VERSION_ERROR

    Z_NO_COMPRESSION	Z_BEST_SPEED		Z_BEST_COMPRESSION
    Z_DEFAULT_COMPRESSION

    Z_FILTERED		Z_HUFFMAN_ONLY		Z_RLE
    Z_FIXED		Z_DEFAULT_STRATEGY

    Z_BINARY		Z_TEXT			Z_ASCII
    Z_UNKNOWN

    Z_DEFLATED

    Z_NULL)
  (import (rnrs)
    (uriel ffi)
    (uriel ffi sizeof)
    (zlib sizeof)
    (zlib fields))

(define zlib
  (let ((o (open-shared-object 'libz.so)))
    (shared-object o)
    o))



;;;; code

(define z_streamp	'pointer)
(define gz_headerp	'pointer)
(define gzFile		'pointer)

(define ZLIB_VERSION	(string->cstring "1.2.3"))


(define-c-function zlibVersion
  (char* zlibVersion (void)))

(define-c-function zError
  (char* zError (int)))

;;; basic functions

(define-c-function deflateInit_
  (int deflateInit_ (z_streamp int char* int)))

(define (deflateInit zstream compression-level)
  (deflateInit_ zstream compression-level ZLIB_VERSION sizeof-zstream))

(define-c-function deflate
  (int deflate (z_streamp int)))

(define-c-function deflateEnd
  (int deflateEnd (z_streamp)))

(define-c-function inflateInit_
  (int inflateInit_ (z_streamp char* int)))

(define (inflateInit zstream)
  (inflateInit_ zstream ZLIB_VERSION sizeof-zstream))

(define-c-function inflate
  (int inflate (z_streamp int)))

(define-c-function inflateEnd
  (int inflateEnd (z_streamp)))

;;; advanced functions



;; (define (deflateInit2 stream level method window-bits mem-level strategy)
;;   (deflateInit2_ stream level method window-bits mem-level strategy
;;     ZLIB_VERSION sizeof-zstream))

;; (define (inflateInit2 stream window-bits)
;;   (inflateInit2_ stream window-bits
;; 		 ZLIB_VERSION sizeof-zstream))

;; (define (inflateBackInit stream window-bits window)
;;   (inflateBackInit_ stream window-bits window
;; 		    ZLIB_VERSION sizeof-zstream))



;;;; done

)

;;; end of file
