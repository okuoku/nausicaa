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
	    (BZ2_bzDecompress		bzlib-decompress)
	    (BZ2_bzDecompressEnd	bzlib-decompress-end)

	    (BZ2_bzBuffToBuffCompress	bzlib-buff-to-buff-compress)
	    (BZ2_bzBuffToBuffDecompress	bzlib-buff-to-buff-decompress)

	    (BZ2_bzReadOpen		bzlib-read-open)
	    (BZ2_bzReadClose		bzlib-read-close)
	    (BZ2_bzReadGetUnused	bzlib-read-get-unused)
	    (BZ2_bzRead			bzlib-read)
	    (BZ2_bzWriteOpen		bzlib-write-open)
	    (BZ2_bzWrite		bzlib-write)
	    (BZ2_bzWriteClose		bzlib-write-close)
	    (BZ2_bzWriteClose64		bzlib-write-close64)

	    (BZ2_bzread			bzread)
	    (BZ2_bzwrite		bzwrite)
	    (BZ2_bzflush		bzflush)
	    (BZ2_bzclose		bzclose))

    bzlib-lib-version
    bzlib-decompress-init
    bzopen bzdopen bzerror)
  (import (rnrs)
    (compensations)
    (foreign ffi)
    (foreign memory)
    (foreign cstrings)
    (foreign compression bzlib platform)
    (foreign compression bzlib sizeof))


(define (bzlib-decompress-init stream verbosity small)
  (BZ2_bzDecompressInit stream verbosity (if small 1 0)))

(define (bzlib-lib-version)
  (cstring->string (BZ2_bzlibVersion)))

(define (bzopen pathname mode)
  (with-compensations
    (let ((bz* (BZ2_bzopen (string->cstring/c pathname) (string->cstring/c mode))))
      (if (pointer-null? bz*) #f bz*))))

(define (bzdopen fd mode)
  (with-compensations
    (let ((bz* (BZ2_bzdopen fd (string->cstring/c mode))))
      (if (pointer-null? bz*) #f bz*))))

(define (bzerror bzfile*)
  (with-compensations
    (let* ((errnum*	(malloc-small/c))
	   (message*	(BZ2_bzerror bzfile* errnum*)))
      (let ((code (pointer-ref-c-signed-int errnum* 0)))
	(values code
		(if (= code BZ_OK)
		    "success"
		  (cstring->string message*)))))))


;;;; done

)

;;; end of file
