;;;
;;;Part of: Nausicaa/Zlib
;;;Contents: Zlib interface for R6RS Scheme
;;;Date: Sun Dec  7, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008, 2009 Marco Maggi <marcomaggi@gna.org>
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


(library (foreign zlib)
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
    gzopen		primitive-gzopen
    gzdopen		primitive-gzdopen
    gzclose

    gzwrite		gzputc
    gzputs		primitive-gzputs
    gzflush

    gzsetparams		gzdirect

    gzread		gzgets
    gzgetc		gzungetc

    gzseek		gzrewind
    gztell		gzeof

    primitive-gzerror	gzerror
    gzclearerr

    ;; checksum functions
    adler32		adler32_combine
    crc32		crc32_combine

    ;; auxiliary functions
    zlibVersion		zError		zlibCompileFlags
    primitive-zError

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
    (receive)
    (compensations)
    (foreign ffi)
    (foreign memory)
    (foreign cstrings)
    (foreign errno)
    (foreign zlib sizeof))

(define zlib
  (let ((o (open-shared-object 'libz.so)))
    (shared-object o)
    o))


;;;; type definitions and miscellaneous functions

(define z_streamp	'pointer)
(define gz_headerp	'pointer)
(define gzFile		'pointer)

(define ZLIB_VERSION	(string->cstring "1.2.3"))

(define-c-function zlibVersion
  (char* zlibVersion (void)))

(define-c-function primitive-zError
  (char* zError (int)))

(define-c-function zlibCompileFlags
  (uLong zlibCompileFlags (void)))

(define (zError errcode)
  (cstring->string (primitive-zError)))


;;;; basic functions

(define-c-function deflateInit_
  (int deflateInit_ (z_streamp int char* int)))

(define-c-function deflate
  (int deflate (z_streamp int)))

(define-c-function deflateEnd
  (int deflateEnd (z_streamp)))

(define-c-function inflateInit_
  (int inflateInit_ (z_streamp char* int)))

(define-c-function inflate
  (int inflate (z_streamp int)))

(define-c-function inflateEnd
  (int inflateEnd (z_streamp)))

(define (deflateInit zstream compression-level)
  (deflateInit_ zstream compression-level ZLIB_VERSION sizeof-zstream))

(define (inflateInit zstream)
  (inflateInit_ zstream ZLIB_VERSION sizeof-zstream))



;;;; advanced functions

(define-c-function deflateInit2_
  (int deflateInit2_ (z_streamp int int int int int char* int)))

(define-c-function deflateSetDictionary
  (int deflateSetDictionary (z_streamp pointer uInt)))

(define-c-function deflateCopy
  (int deflateCopy (z_streamp z_streamp)))

(define-c-function deflateReset
  (int deflateReset (z_streamp)))

(define-c-function deflateParams
  (int deflateParams (z_streamp int int)))

(define-c-function deflateTune
  (int deflateTune (z_streamp int int int int)))

(define-c-function deflateBound
  (uLong deflateBound (z_streamp uLong)))

(define-c-function deflatePrime
  (int deflatePrime (z_streamp int int)))

(define-c-function deflateSetHeader
  (int deflateSetHeader (z_streamp gz_headerp)))

(define-c-function inflateInit2_
  (int inflateInit2_ (z_streamp int char* int)))

(define-c-function inflateSetDictionary
  (int inflateSetDictionary (z_streamp pointer uInt)))

(define-c-function inflateSync
  (int inflateSync (z_streamp)))

(define-c-function inflateCopy
  (int inflateCopy (z_streamp z_streamp)))

(define-c-function inflateReset
  (int inflateReset (z_streamp)))

(define-c-function inflateBackInit_
  (int inflateBackInit_ (z_streamp int char* char* int)))

(define-c-function inflateBack
  (int inflateBack (z_streamp callback pointer callback pointer)))

(define-c-function inflateBackEnd
  (int inflateBackEnd (z_streamp)))

(define-c-function inflatePrime
  (int inflatePrime (z_streamp int int)))

(define (deflateInit2 stream level method window-bits mem-level strategy)
  (deflateInit2_ stream level method window-bits mem-level strategy
    ZLIB_VERSION sizeof-zstream))

(define (inflateInit2 stream window-bits)
  (inflateInit2_ stream window-bits ZLIB_VERSION sizeof-zstream))

(define (inflateBackInit stream window-bits window)
  (inflateBackInit_ stream window-bits window ZLIB_VERSION sizeof-zstream))


;;;; utility functions

(define-c-function compress
  (int compress (pointer pointer pointer uLong)))

(define-c-function compress2
  (int compress (pointer pointer pointer uLong int)))

(define-c-function compressBound
  (uLong compressBound (uLong)))

(define-c-function uncompress
  (int uncompress (pointer uLong pointer uLong)))


;;;; file functions

(define-c-function/with-errno primitive-gzopen
  (gzFile gzopen (char* char*)))

(define-c-function primitive-gzdopen
  (gzFile gzopen (int char*)))

(define-c-function gzsetparams
  (int gzsetparams (gzFile int int)))

(define-c-function gzread
  (int gzread (gzFile pointer unsigned-int)))

(define-c-function gzwrite
  (int gzwrite (gzFile pointer unsigned-int)))

(define-c-function primitive-gzputs
  (int gzputs (gzFile char*)))

(define-c-function gzgets
  (char* gzgets (gzFile char* int)))

(define-c-function gzputc
  (int gzputc (gzFile int)))

(define-c-function gzgetc
  (int gzgetc (gzFile)))

(define-c-function gzungetc
  (int gzungetc (int gzFile)))

(define-c-function gzflush
  (int gzflush (gzFile int)))

(define-c-function gzseek
  (z_off_t gzseek (gzFile z_off_t int)))

(define-c-function gzrewind
  (int gzrewind (gzFile)))

(define-c-function gztell
  (z_off_t gztell (gzFile)))

(define-c-function gzeof
  (int gzeof (gzFile)))

(define-c-function gzdirect
  (int gzdirect (gzFile)))

(define-c-function gzclose
  (int gzclose (gzFile)))

(define-c-function/with-errno primitive-gzerror
  (char* gzerror (gzFile pointer)))

(define-c-function gzclearerr
  (void gzclearerr (gzFile)))

(define (gzopen pathname mode)
  (with-compensations
    (let ((pathname	(string->cstring/c pathname))
	  (mode		(string->cstring/c mode)))
      (primitive-gzopen pathname mode))))

(define (gzdopen fd mode)
  (with-compensations
    (let ((mode		(string->cstring/c mode)))
      (primitive-gzdopen fd mode))))

(define (gzputs file string)
  (with-compensations
    (let ((string	(string->cstring/c string)))
      (primitive-gzputs file string))))

(define (gzerror file)
  (with-compensations
    (let* ((*errcode	(malloc-small/c)))
      (receive (cstr errno)
	  (primitive-gzerror file *errcode)
	(let ((errcode (pointer-ref-c-signed-int *errcode 0)))
	  (if (= Z_ERRNO errcode)
	      (values errno (strerror errno))
	    (values errcode (cstring->string cstr))))))))



;;;; checksum functions

(define-c-function adler32
  (uLong adler32 (uLong pointer uInt)))

(define-c-function adler32_combine
  (uLong adler32_combine (uLong uLong z_off_t)))

(define-c-function crc32
  (uLong crc32 (uLong pointer uInt)))

(define-c-function crc32_combine
  (uLong crc32_combine (uLong uLong z_off_t)))


;;;; done

)

;;; end of file
