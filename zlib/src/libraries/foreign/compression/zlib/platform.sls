;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Zlib
;;;Contents: bindings to foreign functions
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


(library (foreign compression zlib platform)
  (export
    ;; basic functions
    deflateInit_	deflate		deflateEnd
    inflateInit_	inflate		inflateEnd

    ;; advanced functions
    deflateInit2_	deflateSetDictionary
    deflateCopy		deflateReset	deflateParams
    deflatePrime	deflateBound	deflateTune
    deflateSetHeader

    inflateInit2_	inflateSetDictionary
    inflateSync		inflateCopy	inflateReset
    inflatePrime	inflateMark	inflateUndermine

    inflateBackInit_	inflateBack	inflateBackEnd

    ;; utility functions
    compress		compress2	compressBound
    uncompress

    ;; file input/output
    gzopen
    gzdopen
    gzclose		gzclose_r	gzclose_w

    gzwrite
    gzputs		gzputc
    gzflush

    gzsetparams		gzdirect

    gzread		gzgets
    gzgetc		gzungetc

    gzseek		gzrewind
    gztell		gzeof

    gzerror
    gzclearerr

    ;; checksum functions
    adler32		adler32_combine
    crc32		crc32_combine

    ;; auxiliary functions
    zlibVersion		zlibCompileFlags
    zError
    )
  (import (rnrs)
    (unimplemented)
    (foreign ffi)
    (foreign ffi sizeof)
    (only (foreign cstrings)
	  string->cstring)
    (foreign compression zlib sizeof)
    (foreign compression zlib shared-object))


;;;; type definitions and miscellaneous functions

(define z_streamp	'pointer)
(define gz_headerp	'pointer)
(define gzFile		'pointer)

(define-c-functions zlib-shared-object
  (zlibVersion
   (char* zlibVersion (void)))

  (zError
   (char* zError (int)))

  (zlibCompileFlags
   (uLong zlibCompileFlags (void))))


;;;; basic functions

(define-c-functions zlib-shared-object
  (deflateInit_
    (int deflateInit_ (z_streamp int char* int)))

  (deflate
    (int deflate (z_streamp int)))

  (deflateEnd
    (int deflateEnd (z_streamp)))

  (inflateInit_
   (int inflateInit_ (z_streamp char* int)))

  (inflate
   (int inflate (z_streamp int)))

  (inflateEnd
   (int inflateEnd (z_streamp))))


;;;; advanced functions

(define-c-functions zlib-shared-object
  (deflateInit2_
    (int deflateInit2_ (z_streamp int int int int int char* int)))

  (deflateSetDictionary
    (int deflateSetDictionary (z_streamp pointer uInt)))

  (deflateCopy
    (int deflateCopy (z_streamp z_streamp)))

  (deflateReset
    (int deflateReset (z_streamp)))

  (deflateParams
    (int deflateParams (z_streamp int int)))

  (deflateTune
    (int deflateTune (z_streamp int int int int)))

  (deflateBound
    (uLong deflateBound (z_streamp uLong)))

  (deflatePrime
    (int deflatePrime (z_streamp int int)))

  (deflateSetHeader
    (int deflateSetHeader (z_streamp gz_headerp)))

  (inflateInit2_
   (int inflateInit2_ (z_streamp int char* int)))

  (inflateSetDictionary
   (int inflateSetDictionary (z_streamp pointer uInt)))

  (inflateSync
   (int inflateSync (z_streamp)))

  (inflateCopy
   (int inflateCopy (z_streamp z_streamp)))

  (inflateReset
   (int inflateReset (z_streamp)))

  (inflateBackInit_
   (int inflateBackInit_ (z_streamp int char* char* int)))

  (inflateBack
   (int inflateBack (z_streamp callback pointer callback pointer)))

  (inflateBackEnd
   (int inflateBackEnd (z_streamp)))

  (inflatePrime
   (int inflatePrime (z_streamp int int))))

(define inflateMark
  (if (<= #x1240 ZLIB_VERNUM)
      (make-c-function* zlib-shared-object
			long inflateMark (z_streamp))
    (lambda args
      (raise-unimplemented-error 'inflateMark "function not implemented in this version of Zlib"))))

(define inflateUndermine
  (if (<= #x1240 ZLIB_VERNUM)
      (make-c-function* zlib-shared-object
			int inflateUndermine (z_streamp int))
    (lambda args
      (raise-unimplemented-error 'inflateUndermine "function not implemented in this version of Zlib"))))


;;;; utility functions

(define-c-functions zlib-shared-object
  (compress
   (int compress (pointer pointer pointer uLong)))

  (compress2
   (int compress (pointer pointer pointer uLong int)))

  (compressBound
   (uLong compressBound (uLong)))

  (uncompress
   (int uncompress (pointer pointer pointer uLong))))


;;;; file functions

(define-c-functions/with-errno zlib-shared-object
  (gzopen
   (gzFile gzopen (char* char*))))

(define-c-functions zlib-shared-object
  (gzdopen
   (gzFile gzopen (int char*)))

  (gzsetparams
   (int gzsetparams (gzFile int int)))

  (gzread
   (int gzread (gzFile pointer unsigned-int)))

  (gzwrite
   (int gzwrite (gzFile pointer unsigned-int)))

  (gzputs
   (int gzputs (gzFile char*)))

  (gzgets
   (char* gzgets (gzFile char* int)))

  (gzputc
   (int gzputc (gzFile int)))

  (gzgetc
   (int gzgetc (gzFile)))

  (gzungetc
   (int gzungetc (int gzFile)))

  (gzflush
   (int gzflush (gzFile int)))

  (gzseek
   (z_off_t gzseek (gzFile z_off_t int)))

  (gzrewind
   (int gzrewind (gzFile)))

  (gztell
   (z_off_t gztell (gzFile)))

  (gzeof
   (int gzeof (gzFile)))

  (gzdirect
   (int gzdirect (gzFile)))

  (gzclose
   (int gzclose (gzFile)))

  (gzerror
   (char* gzerror (gzFile pointer)))

  (gzclearerr
   (void gzclearerr (gzFile))))

(define gzclose_r
  (if (<= #x1240 ZLIB_VERNUM)
      (make-c-function* zlib-shared-object int gzclose_r (gzFile))
    (lambda args
      (raise-unimplemented-error 'gzclose_r "function not implemented in this version of Zlib"))))

(define gzclose_w
  (if (<= #x1240 ZLIB_VERNUM)
      (make-c-function* zlib-shared-object int gzclose_w (gzFile))
    (lambda args
      (raise-unimplemented-error 'gzclose_w "function not implemented in this version of Zlib"))))


;;;; checksum functions

(define-c-functions zlib-shared-object
  (adler32
   (uLong adler32 (uLong pointer uInt)))

  (adler32_combine
   (uLong adler32_combine (uLong uLong z_off_t)))

  (crc32
   (uLong crc32 (uLong pointer uInt)))

  (crc32_combine
   (uLong crc32_combine (uLong uLong z_off_t))))


;;;; done

)

;;; end of file
