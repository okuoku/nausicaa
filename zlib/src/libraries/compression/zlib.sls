;;;
;;;Part of: Nausicaa/Zlib
;;;Contents: Zlib interface for R6RS Scheme
;;;Date: Sun Dec  7, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008-2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (compression zlib)
  (export

    ;; struct classes
    <struct-z_stream> <struct-gz_header>

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
    inflatePrime	inflateMark	inflateUndermine

    inflateBackInit	inflateBack	inflateBackEnd

    ;; utility functions
    compress		compress2	compressBound
    uncompress

    ;; file input/output
    gzopen		gzopen*
    gzdopen		gzdopen*
    gzclose		gzclose_r	gzclose_w

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

    ;; constants
    c-sizeof c-strideof c-alignof c-valueof c-inspect
    pointer-c-ref pointer-c-set! pointer-c-accessor pointer-c-mutator
    array-c-ref array-c-set! array-c-pointer-to

    ;; sizeof-z_stream		sizeof-gz_header

    ;; ZLIB_VERNUM		ZLIB_VERSION

    ;; Z_NO_FLUSH		Z_PARTIAL_FLUSH		Z_SYNC_FLUSH
    ;; Z_FULL_FLUSH	Z_FINISH		Z_BLOCK

    ;; Z_OK		Z_STREAM_END		Z_NEED_DICT
    ;; Z_ERRNO		Z_STREAM_ERROR		Z_DATA_ERROR
    ;; Z_MEM_ERROR		Z_BUF_ERROR		Z_VERSION_ERROR

    ;; Z_NO_COMPRESSION	Z_BEST_SPEED		Z_BEST_COMPRESSION
    ;; Z_DEFAULT_COMPRESSION

    ;; Z_FILTERED		Z_HUFFMAN_ONLY		Z_RLE
    ;; Z_FIXED		Z_DEFAULT_STRATEGY

    ;; Z_BINARY		Z_TEXT			Z_ASCII
    ;; Z_UNKNOWN

    ;; Z_DEFLATED

    ;; Z_NULL
    )
  (import (rnrs)
    (compression zlib primitives)
    (compression zlib sizeof)))

;;; end of file
