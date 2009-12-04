;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Bzlib
;;;Contents: primitive functions
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


(library (foreign compression bzlib primitives)
  (export
    (rename (BZ2_bzCompressInit		bzlib-compress-init)
	    (BZ2_bzCompress		bzlib-compress)
	    (BZ2_bzCompressEnd		bzlib-compress-end)
	    (BZ2_bzDecompressInit	bzlib-decompress-init)
	    (BZ2_bzDecompress		bzlib-decompress)
	    (BZ2_bzDecompressEnd	bzlib-decompress-end))

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
    bzerror)
  (import (rnrs)
    (compensations)
    (foreign ffi)
    (foreign memory)
    (foreign cstrings)
    (foreign compression bzlib platform)
    (foreign compression bzlib sizeof))


(define (bzlib-read-open)
  #t)

(define (bzlib-read-close)
  #t)

(define (bzlib-read-get-unused)
  #t)

(define (bzlib-read)
  #t)

(define (bzlib-write-open)
  #t)

(define (bzlib-write)
  #t)

(define (bzlib-write-close)
  #t)

(define (bzlib-write-close64)
  #t)


(define (bzlib-buff-to-buff-compress)
  #t)

(define (bzlib-buff-to-buff-decompress)
  #t)


(define (bzlib-lib-version)
  (cstring->string (BZ2_bzlibVersion)))


(define (bzopen)
  #t)

(define (bzdopen)
  #t)

(define (bzread)
  #t)

(define (bzwrite)
  #t)

(define (bzflush)
  #t)

(define (bzclose)
  #t)

(define (bzerror)
  #t)


;;;; done

)

;;; end of file
