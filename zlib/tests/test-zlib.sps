;;;
;;;Part of: Nausicaa/Zlib
;;;Contents: tests for zlib
;;;Date: Mon Dec  8, 2008
;;;Time-stamp: <2008-12-08 14:11:04 marco>
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
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



;;;; setup

(import (rnrs)
  (uriel printing)
  (uriel test)
  (uriel lang)
  (uriel ffi)
  (zlib)
  (srfi random))

(check-set-mode! 'report-failed)

(define original-string (call-with-input-file "Makefile"
			  (lambda (port)
			    (get-string-all port))))
(define original-ptr (string->cstring original-string))
(define original-len (strlen original-ptr))

(define (dump-zstream zstream)
  (print #t "dumping ~s:
next_in:\t~s
avail_in:\t~s
total_in:\t~s
next_out:\t~s
avail_out:\t~s
total_out:\t~s\n"
	 (pointer->integer zstream)
	 (zstream-next_in-ref zstream)
	 (zstream-avail_in-ref zstream)
	 (zstream-total_in-ref zstream)
	 (zstream-next_out-ref zstream)
	 (zstream-avail_out-ref zstream)
	 (zstream-total_out-ref zstream)))



;;;; code

(check
    (let ()
      (cstring->string (zlibVersion)))
  => "1.2.3")

(check
    (with-compensations
      (let* ((complen		(* 2 original-len))
	     (compressed	(compensate-malloc/block complen))
	     (outlen		(* 2 original-len))
	     (output		(compensate-malloc/block outlen))
	     (zstream		(compensate-malloc/block sizeof-zstream)))

	(zstream-next_in-set!  zstream original-ptr)
	(zstream-avail_in-set! zstream original-len)

	(zstream-next_out-set!  zstream compressed)
	(zstream-avail_out-set! zstream complen)

	(zstream-zalloc-set! zstream (integer->pointer 0))
	(zstream-zfree-set!  zstream (integer->pointer 0))
	(zstream-opaque-set! zstream (integer->pointer 0))
;;	(dump-zstream zstream)

	(deflateInit zstream Z_BEST_COMPRESSION)
	(deflate zstream Z_FINISH)
	(deflateEnd zstream)
;;	(dump-zstream zstream)

	(let ((compressed-len
	       (pointer-diff (zstream-next_out-ref zstream) compressed)))
	  (zstream-next_in-set! zstream compressed)
	  (zstream-avail_in-set! zstream compressed-len))

	(zstream-next_out-set! zstream output)
	(zstream-avail_out-set! zstream outlen)
;;	(dump-zstream zstream)

	(inflateInit zstream)
	(inflate zstream Z_FINISH)
	(inflateEnd zstream)
;;	(dump-zstream zstream)

	(let ((decompressed-len
	       (pointer-diff (zstream-next_out-ref zstream) output)))
	  (and (= original-len decompressed-len)
	       (equal? (cstring->string/len output decompressed-len)
		       original-string)))))
  => #t)



;;;; done

(check-report)

;;; end of file
