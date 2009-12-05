;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Bzlib
;;;Contents: bindings to foreign functions
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


(library (foreign compression bzlib platform)
  (export

    BZ2_bzCompressInit
    BZ2_bzCompress
    BZ2_bzCompressEnd
    BZ2_bzDecompressInit
    BZ2_bzDecompress
    BZ2_bzDecompressEnd

    BZ2_bzReadOpen
    BZ2_bzReadClose
    BZ2_bzReadGetUnused
    BZ2_bzRead
    BZ2_bzWriteOpen
    BZ2_bzWrite
    BZ2_bzWriteClose
    BZ2_bzWriteClose64

    BZ2_bzBuffToBuffCompress
    BZ2_bzBuffToBuffDecompress

    BZ2_bzlibVersion
    BZ2_bzopen
    BZ2_bzdopen
    BZ2_bzread
    BZ2_bzwrite
    BZ2_bzflush
    BZ2_bzclose
    BZ2_bzerror)
  (import (rnrs)
    (foreign ffi)
    (foreign ffi sizeof)
    (foreign compression bzlib shared-object)
    (foreign compression bzlib sizeof))


;;;; type aliases

(define int*		'pointer)
(define void**		'pointer)
(define unsigned-int*	'pointer)


;;;; core (low-level) library functions

(define-c-functions bzlib-shared-object

  (BZ2_bzCompressInit
   (int BZ2_bzCompressInit (bz_stream* int int int)))

  (BZ2_bzCompress
   (int BZ2_bzCompress (bz_stream* int)))

  (BZ2_bzCompressEnd
   (int BZ2_bzCompressEnd (bz_stream*)))

  (BZ2_bzDecompressInit
   (int BZ2_bzDecompressInit (bz_stream* int int)))

  (BZ2_bzDecompress
   (int BZ2_bzDecompress (bz_stream*)))

  (BZ2_bzDecompressEnd
   (int BZ2_bzDecompressEnd (bz_stream*))))


;;;; high(er) level library functions

(define-c-functions bzlib-shared-object

  (BZ2_bzReadOpen
   (BZFILE* BZ2_bzReadOpen (int* FILE* int int void* int)))

  (BZ2_bzReadClose
   (void BZ2_bzReadClose (int* BZFILE*)))

  (BZ2_bzReadGetUnused
   (void BZ2_bzReadGetUnused (int* BZFILE* void** int*)))

  (BZ2_bzRead
   (int BZ2_bzRead (int* BZFILE* void* int)))

  (BZ2_bzWriteOpen
   (BZFILE* BZ2_bzWriteOpen (int* FILE* int int int)))

  (BZ2_bzWrite
   (void BZ2_bzWrite (int* BZFILE* void* int)))

  (BZ2_bzWriteClose
   (void BZ2_bzWriteClose (int* BZFILE* int unsigned-int* unsigned-int*)))

  (BZ2_bzWriteClose64
   (void BZ2_bzWriteClose64 (int* BZFILE* int unsigned-int* unsigned-int* unsigned-int* unsigned-int*))))


;;;; utility functions

(define-c-functions bzlib-shared-object

  (BZ2_bzBuffToBuffCompress
   (int BZ2_bzBuffToBuffCompress (char* unsigned-int* char* unsigned-int int int int)))

  (BZ2_bzBuffToBuffDecompress
   (int BZ2_bzBuffToBuffDecompress (char* unsigned-int* char* unsigned-int int int))))


;;;; better zlib compatibility

(define-c-functions bzlib-shared-object

  (BZ2_bzlibVersion
   (char* BZ2_bzlibVersion (void))))

(define-c-functions bzlib-shared-object

  (BZ2_bzopen
   (BZFILE* BZ2_bzopen (char* char*)))

  (BZ2_bzdopen
   (BZFILE* BZ2_bzdopen (int char*)))

  (BZ2_bzread
   (int BZ2_bzread (BZFILE* void* int)))

  (BZ2_bzwrite
   (int BZ2_bzwrite (BZFILE* void* int)))

  (BZ2_bzflush
   (int BZ2_bzflush (BZFILE*)))

  (BZ2_bzclose
   (void BZ2_bzclose (BZFILE*)))

  (BZ2_bzerror
   (char* BZ2_bzerror (BZFILE* int*))))


;;;; done

)

;;; end of file
