;;;!mosh
;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Bzlib
;;;Contents: foreign library inspection generator
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


(import (nausicaa)
  (foreign ffi inspector-maker))


;;;; constants

(define-c-defines "control codes"
  BZ_RUN
  BZ_FLUSH
  BZ_FINISH)

(define-c-defines "return values"
  BZ_OK
  BZ_RUN_OK
  BZ_FLUSH_OK
  BZ_FINISH_OK
  BZ_STREAM_END
  BZ_SEQUENCE_ERROR
  BZ_PARAM_ERROR
  BZ_MEM_ERROR
  BZ_DATA_ERROR
  BZ_DATA_ERROR_MAGIC
  BZ_IO_ERROR
  BZ_UNEXPECTED_EOF
  BZ_OUTBUFF_FULL
  BZ_CONFIG_ERROR)

(define-c-defines "miscellaneous"
  BZ_MAX_UNUSED)


;;;; data structures

(define-c-type-alias bz_stream*		pointer)
(define-c-type-alias BZFILE*		pointer)

(define-c-struct bz_stream
  "bz_stream"
  (pointer		next_in)
  (unsigned-int		avail_in)
  (unsigned-int		total_in_lo32)
  (unsigned-int		total_in_hi32)
  (pointer		next_out)
  (unsigned-int		avail_out)
  (unsigned-int		total_out_lo32)
  (unsigned-int		total_out_hi32)
  (pointer		state)
  (pointer		bzalloc)
  (pointer		bzfree)
  (pointer		opaque))


;;;; done

(define bzlib-library-spec
  '(foreign compression bzlib sizeof))

(define-shared-object bzlib libbz2.so)

(autoconf-lib-write "configuration/bzlib-inspector.m4" bzlib-library-spec)
(sizeof-lib-write   "src/libraries/foreign/compression/bzlib/sizeof.sls.in" bzlib-library-spec)

;;; end of file
