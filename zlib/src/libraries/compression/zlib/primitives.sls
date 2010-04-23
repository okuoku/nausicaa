;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Zlib
;;;Contents: primitive functions
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


(library (compression zlib primitives)
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
    zError		zError*)
  (import (nausicaa)
    (ffi)
    (ffi memory)
    (ffi cstrings)
    (ffi errno)
    (compression zlib platform)
    (compression zlib structs)
    (compression zlib sizeof))


(define zlib_version* (string->cstring ZLIB_VERSION))

(define (zError* errcode)
  (cstring->string (zError errcode)))

;;; --------------------------------------------------------------------

(define (deflateInit zstream compression-level)
  (deflateInit_ zstream compression-level zlib_version* sizeof-z_stream))

(define (inflateInit zstream)
  (inflateInit_ zstream zlib_version* sizeof-z_stream))

;;; --------------------------------------------------------------------

(define (deflateInit2 stream level method window-bits mem-level strategy)
  (deflateInit2_ stream level method window-bits mem-level strategy
    zlib_version* sizeof-z_stream))

(define (inflateInit2 stream window-bits)
  (inflateInit2_ stream window-bits zlib_version* sizeof-z_stream))

(define (inflateBackInit stream window-bits window)
  (inflateBackInit_ stream window-bits window zlib_version* sizeof-z_stream))

;;; --------------------------------------------------------------------

(define (gzopen* pathname mode)
  (with-compensations
    (gzopen (string->cstring/c pathname)
	    (string->cstring/c mode))))

(define (gzdopen* fd mode)
  (with-compensations
    (gzdopen fd (string->cstring/c mode))))

(define (gzputs* file string)
  (with-compensations
    (gzputs file (string->cstring/c string))))

(define (gzerror* file)
  (with-compensations
    (let* ((*errcode	(malloc-small/c))
	   (cstr	(gzerror file *errcode)))
      (values (pointer-ref-c-signed-int *errcode 0)
	      (if (pointer-null? cstr)
		  ""
		(cstring->string cstr))))))


;;;; done

)

;;; end of file
