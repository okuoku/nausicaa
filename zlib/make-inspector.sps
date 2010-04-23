;;;!mosh
;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Zlib
;;;Contents: foreign library inspection generator
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


(import (nausicaa)
  (ffi inspector-maker))


(define-c-type uInt	unsigned-int)
(define-c-type uLong	unsigned-int)
(define-c-type z_off_t	unsigned-int)

(define-c-struct z_stream
  "z_stream"
  (pointer		next_in)
  (unsigned-int		avail_in)
  (unsigned-int		total_in)

  (pointer		next_out)
  (unsigned-int		avail_out)
  (unsigned-int		total_out)

  (pointer		msg)
  (signed-int		data_type)
  (signed-int		adler)

  (pointer		zalloc)
  (pointer		zfree)
  (pointer		opaque))

(define-c-struct gz_header
  "gz_header"

  (signed-int		text)
  (unsigned-int		time)
  (signed-int		xflags)
  (signed-int		os)
  (pointer		extra)
  (unsigned-int		extra_len)
  (unsigned-int		extra_max)
  (pointer		name)
  (unsigned-int		name_max)
  (pointer		comment)
  (unsigned-int		comm_max)
  (signed-int		hcrc)
  (signed-int		done))


;;;; constants inspection

(define-c-defines "version number"
  ZLIB_VERNUM)

(define-c-string-defines "version number"
  ZLIB_VERSION)

(define-c-defines "stream control"
  Z_NO_FLUSH
  Z_PARTIAL_FLUSH
  Z_SYNC_FLUSH
  Z_FULL_FLUSH
  Z_FINISH
  Z_BLOCK
  Z_TREES)

(define-c-defines "return values"
  Z_OK
  Z_STREAM_END
  Z_NEED_DICT
  Z_ERRNO
  Z_STREAM_ERROR
  Z_DATA_ERROR
  Z_MEM_ERROR
  Z_BUF_ERROR
  Z_VERSION_ERROR)

(define-c-defines "compression level"
  Z_NO_COMPRESSION
  Z_BEST_SPEED
  Z_BEST_COMPRESSION
  Z_DEFAULT_COMPRESSION)

(define-c-defines "compression algorithm"
  Z_FILTERED
  Z_HUFFMAN_ONLY
  Z_RLE
  Z_FIXED
  Z_DEFAULT_STRATEGY)

(define-c-defines "data category"
  Z_BINARY
  Z_TEXT
  Z_ASCII
  Z_UNKNOWN)

(define-c-defines "miscellaneous"
  Z_DEFLATED
  Z_NULL)



;;;; done

(define-shared-object zlib libz.so)

(define zlib-library-spec
  '(compression zlib sizeof))

(autoconf-lib-write "configuration/zlib-inspector.m4" zlib-library-spec
		    "NAUSICAA_ZLIB")
(sizeof-lib-write   "src/libraries/compression/zlib/sizeof.sls.in" zlib-library-spec)
(structs-lib-write  "src/libraries/compression/zlib/structs.sls"
		    '(compression zlib structs)
		    zlib-library-spec)

;;; end of file
