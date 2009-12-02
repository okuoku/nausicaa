;;;
;;;Part of: Nausicaa/Zlib
;;;Contents: Zlib interface for R6RS Scheme
;;;Date: Sun Dec  7, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008, 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (foreign compression zlib)
  (export

    ;; basic functions
    deflateInit		deflate		deflateEnd
    inflateInit		inflate		inflateEnd

    ;; advanced functions
    deflateInit2	deflateSetDictionary
    deflateCopy		deflateReset	deflateParams
    deflatePrime	deflateBound	deflateTune
    deflateSetHeader

    inflateInit2	inflateSetDictionary
    inflateSync		inflateCopy	inflateReset
    inflatePrime

    inflateBackInit	inflateBack	inflateBackEnd

    ;; utility functions
    compress		compress2	compressBound
    uncompress

    ;; file input/output
    gzopen		gzopen*
    gzdopen		gzdopen*
    gzclose

    gzwrite		gzputc
    gzputs		gzputs*
    gzflush

    gzsetparams		gzdirect

    gzread		gzgets
    gzgetc		gzungetc

    gzseek		gzrewind
    gztell		gzeof

    gzerror		gzerror*
    gzclearerr

    ;; checksum functions
    adler32		adler32_combine
    crc32		crc32_combine

    ;; auxiliary functions
    zlibVersion		zlibCompileFlags
    zError		zError*

    ;;stream structure accessors
    struct-z_stream-next_in-set!	struct-z_stream-next_in-ref
    struct-z_stream-avail_in-set!	struct-z_stream-avail_in-ref
    struct-z_stream-total_in-set!	struct-z_stream-total_in-ref

    struct-z_stream-next_out-set!	struct-z_stream-next_out-ref
    struct-z_stream-avail_out-set!	struct-z_stream-avail_out-ref
    struct-z_stream-total_out-set!	struct-z_stream-total_out-ref

    struct-z_stream-msg-set!		struct-z_stream-msg-ref
    struct-z_stream-data_type-set!	struct-z_stream-data_type-ref
    struct-z_stream-adler-set!		struct-z_stream-adler-ref

    struct-z_stream-zalloc-set!		struct-z_stream-zalloc-ref
    struct-z_stream-zfree-set!		struct-z_stream-zfree-ref
    struct-z_stream-opaque-set!		struct-z_stream-opaque-ref

    struct-gz_header-text-set!		struct-gz_header-text-ref
    struct-gz_header-time-set!		struct-gz_header-time-ref
    struct-gz_header-xflags-set!	struct-gz_header-xflags-ref
    struct-gz_header-os-set!		struct-gz_header-os-ref
    struct-gz_header-extra-set!		struct-gz_header-extra-ref
    struct-gz_header-extra_len-set!	struct-gz_header-extra_len-ref
    struct-gz_header-extra_max-set!	struct-gz_header-extra_max-ref
    struct-gz_header-name-set!		struct-gz_header-name-ref
    struct-gz_header-name_max-set!	struct-gz_header-name_max-ref
    struct-gz_header-comment-set!	struct-gz_header-comment-ref
    struct-gz_header-comm_max-set!	struct-gz_header-comm_max-ref
    struct-gz_header-hcrc-set!		struct-gz_header-hcrc-ref
    struct-gz_header-done-set!		struct-gz_header-done-ref

    ;; constants
    sizeof-z_stream		sizeof-gz_header

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
    (foreign compression zlib primitives)
    (foreign compression zlib sizeof)))

;;; end of file
