;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Bzlib
;;;Contents: compound library, high-level API
;;;Date: Fri Dec  4, 2009
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


(library (foreign compression bzlib)
  (export
    bzlib-compress-init
    bzlib-compress
    bzlib-compress-end
    bzlib-decompress-init
    bzlib-decompress
    bzlib-decompress-end

    bzlib-read-open
    bzlib-read-close
    bzlib-read-get-unused
    bzlib-read
    bzlib-write-open
    bzlib-write
    bzlib-write-close
    bzlib-write-close64

    bzlib-buff-to-buff-compress
    bzlib-buff-to-buff-decompress

    bzlib-lib-version

    bzopen
    bzdopen
    bzread
    bzwrite
    bzflush
    bzclose
    bzerror

;;; --------------------------------------------------------------------

    BZ_RUN			BZ_FLUSH
    BZ_FINISH

    BZ_OK			BZ_RUN_OK
    BZ_FLUSH_OK			BZ_FINISH_OK
    BZ_STREAM_END		BZ_SEQUENCE_ERROR
    BZ_PARAM_ERROR		BZ_MEM_ERROR
    BZ_DATA_ERROR		BZ_DATA_ERROR_MAGIC
    BZ_IO_ERROR			BZ_UNEXPECTED_EOF
    BZ_OUTBUFF_FULL		BZ_CONFIG_ERROR

    BZ_MAX_UNUSED

;;; --------------------------------------------------------------------

    sizeof-bz_stream
    alignof-bz_stream
    strideof-bz_stream

    struct-bz_stream-next_in-set!		struct-bz_stream-next_in-ref
    struct-bz_stream-avail_in-set!		struct-bz_stream-avail_in-ref
    struct-bz_stream-total_in_lo32-set!		struct-bz_stream-total_in_lo32-ref
    struct-bz_stream-total_in_hi32-set!		struct-bz_stream-total_in_hi32-ref
    struct-bz_stream-next_out-set!		struct-bz_stream-next_out-ref
    struct-bz_stream-avail_out-set!		struct-bz_stream-avail_out-ref
    struct-bz_stream-total_out_lo32-set!	struct-bz_stream-total_out_lo32-ref
    struct-bz_stream-total_out_hi32-set!	struct-bz_stream-total_out_hi32-ref
    struct-bz_stream-state-set!			struct-bz_stream-state-ref
    struct-bz_stream-bzalloc-set!		struct-bz_stream-bzalloc-ref
    struct-bz_stream-bzfree-set!		struct-bz_stream-bzfree-ref
    struct-bz_stream-opaque-set!		struct-bz_stream-opaque-ref)
  (import (rnrs)
    (foreign compression bzlib primitives)
    (foreign compression bzlib sizeof)))

;;; end of file
