;;;
;;;Part of: Nausicaa/Zlib
;;;Contents: tests for zlib
;;;Date: Mon Dec  8, 2008
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


(import (nausicaa)
  (foreign ffi)
  (foreign memory)
  (foreign cstrings)
  (foreign zlib)
  (compensations)
  (checks)
  (format))

(check-set-mode! 'report-failed)


;;;; helpers

(define original-string (call-with-input-file "Makefile"
			  (lambda (port)
			    (get-string-all port))))
(define original-ptr (string->cstring original-string))
(define original-len (strlen original-ptr))

(define (dump-zstream zstream)
  (format #t "dumping ~s:
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

(define dictionary "ENABLEDISABLEstripinstallallcleannausicaapackageuninstallslack")


(parametrise ((check-test-name 'auxiliary-functions))

  (check
      (cstring->string (zlibVersion))
    => "1.2.3")

  #t)


(parametrise ((check-test-name 'basic-functions))

  (check
      (with-compensations
	(let* ((complen		(* 2 original-len))
	       (compressed	(malloc-block/c complen))
	       (outlen		(* 2 original-len))
	       (output		(malloc-block/c outlen))
	       (zstream		(malloc-block/c sizeof-zstream)))

	  (zstream-next_in-set!  zstream original-ptr)
	  (zstream-avail_in-set! zstream original-len)

	  (zstream-next_out-set!  zstream compressed)
	  (zstream-avail_out-set! zstream complen)

	  (zstream-zalloc-set! zstream pointer-null)
	  (zstream-zfree-set!  zstream pointer-null)
	  (zstream-opaque-set! zstream pointer-null)
	  ;;	(dump-zstream zstream)

	  (deflateInit zstream Z_BEST_COMPRESSION)
	  (deflate zstream Z_FINISH)
	  (deflateEnd zstream)
	  ;;	(dump-zstream zstream)

	  (let ((compressed-len (pointer-diff (zstream-next_out-ref zstream) compressed)))
	    (zstream-next_in-set! zstream compressed)
	    (zstream-avail_in-set! zstream compressed-len))

	  (zstream-next_out-set! zstream output)
	  (zstream-avail_out-set! zstream outlen)
	  ;;	(dump-zstream zstream)

	  (inflateInit zstream)
	  (inflate zstream Z_FINISH)
	  (inflateEnd zstream)
	  ;;	(dump-zstream zstream)

	  (let ((decompressed-len (pointer-diff (zstream-next_out-ref zstream) output)))
	    (and (= original-len decompressed-len)
		 (equal? (cstring->string output decompressed-len)
			 original-string)))))
    => #t)

  #t)


(parametrise ((check-test-name 'advanced-functions))

  (check
      (with-compensations
	(let* ((complen		(* 2 original-len))
	       (compressed	(malloc-block/c complen))
	       (outlen		(* 2 original-len))
	       (output		(malloc-block/c outlen))
	       (zstream		(malloc-block/c sizeof-zstream))
	       (dictionary	(string->cstring/c dictionary))
	       (dictionary-len	(strlen dictionary)))

	  (zstream-next_in-set!  zstream original-ptr)
	  (zstream-avail_in-set! zstream original-len)

	  (zstream-next_out-set!  zstream compressed)
	  (zstream-avail_out-set! zstream complen)

	  (zstream-zalloc-set! zstream pointer-null)
	  (zstream-zfree-set!  zstream pointer-null)
	  (zstream-opaque-set! zstream pointer-null)
;;;	(dump-zstream zstream)

	  (deflateInit2 zstream Z_BEST_COMPRESSION
	    Z_DEFLATED	     ;; method
	    10		     ;; windowBits
	    3		     ;; memLevel
	    Z_DEFAULT_STRATEGY ;; strategy
	    )
	  (deflateSetDictionary zstream dictionary dictionary-len)

	  (deflate zstream Z_FINISH)
	  (deflateEnd zstream)
;;;	(dump-zstream zstream)

	  (let ((compressed-len
		 (pointer-diff (zstream-next_out-ref zstream) compressed)))
	    (zstream-next_in-set! zstream compressed)
	    (zstream-avail_in-set! zstream compressed-len))

	  (zstream-next_out-set! zstream output)
	  (zstream-avail_out-set! zstream outlen)
	  ;;	(dump-zstream zstream)

	  (inflateInit2 zstream 10)
	  (inflate zstream Z_FINISH)
	  (inflateSetDictionary zstream dictionary dictionary-len)
	  (inflate zstream Z_FINISH)
	  (inflateEnd zstream)
	  ;;	(dump-zstream zstream)

	  (let ((decompressed-len
		 (pointer-diff (zstream-next_out-ref zstream) output)))
	    (and (= original-len decompressed-len)
		 (equal? (cstring->string output decompressed-len)
			 original-string)))))
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file
