;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Nettle
;;;Contents: tests for Nettle platform libraries loading
;;;Date: Thu Jan  7, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (compensations)
  (foreign cstrings)
  (foreign memory)
  (foreign crypto nettle platform)
  (foreign crypto nettle sizeof)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing Nettle platform\n")


(parametrise ((check-test-name	'base16))

  (check
      (with-compensations
	(let* ((bv		'#vu8(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17))
	       (buf.ptr		(bytevector->pointer bv malloc-block/c))
	       (buf.len		(bytevector-length bv))
	       (str.len		(BASE16_ENCODE_LENGTH buf.len))
	       (str.ptr		(malloc-block/c str.len)))
	  (base16_encode_update str.ptr buf.len buf.ptr)
	  (cstring->string str.ptr str.len)))
    => "000102030405060708090a0b0c0d0e0f1011")

  (check
      (with-compensations
	(let* ((bv		'#vu8(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17))
	       (in.ptr		(bytevector->pointer bv malloc-block/c))
	       (in.len		(bytevector-length bv))
	       (str.len		(BASE16_ENCODE_LENGTH in.len))
	       (str.ptr		(malloc-block/c str.len)))
	  (base16_encode_update str.ptr in.len in.ptr)
	  (let* ((b16*		(malloc-block/c sizeof-base16_decode_ctx))
		 (out.len	(BASE16_DECODE_LENGTH str.len))
		 (out.ptr	(malloc-block/c out.len))
		 (out.len*	(malloc-small/c)))
	    (pointer-set-c-unsigned-int! out.len* 0 out.len)
	    (base16_decode_init   b16*)
	    (base16_decode_update b16* out.len* out.ptr str.len str.ptr)
	    (assert (= 1 (base16_decode_final  b16*)))
	    (pointer->bytevector out.ptr (pointer-ref-c-unsigned-int out.len* 0)))))
    => '#vu8(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17))

  (let ()

    (define (str->base16 str)
      (with-compensations
	(let* ((in.ptr		(string->cstring/c str))
	       (in.len		(strlen in.ptr))
	       (out.len		(BASE16_ENCODE_LENGTH in.len))
	       (out.ptr		(malloc-block/c out.len)))
	  (base16_encode_update out.ptr in.len in.ptr)
	  (cstring->string out.ptr out.len))))

    (define (base16->str str)
      (let* ((in.ptr	(string->cstring/c str))
	     (in.len	(strlen in.ptr))
	     (b16*	(malloc-block/c sizeof-base16_decode_ctx))
	     (out.len	(BASE16_DECODE_LENGTH in.len))
	     (out.ptr	(malloc-block/c out.len))
	     (out.len*	(malloc-small/c)))
	(pointer-set-c-unsigned-int! out.len* 0 out.len)
	(base16_decode_init   b16*)
	(base16_decode_update b16* out.len* out.ptr in.len in.ptr)
	(assert (= 1 (base16_decode_final  b16*)))
	(cstring->string out.ptr (pointer-ref-c-unsigned-int out.len* 0))))

    (check
	(str->base16 "ABC")
      => "414243")

    (check
	(base16->str "41424344")
      => "ABCD")

    (check
	(base16->str "414243")
      => "ABC")

    #f)

  #t)


(parametrise ((check-test-name	'hash))

  (check
      (with-compensations
	(let* ((ctx*	(malloc-block/c sizeof-md5_ctx))
	       (buf.ptr	(string->cstring "abc"))
	       (buf.len	(strlen buf.ptr)))
	  (md5_init ctx*)
	  (md5_update ctx* buf.len buf.ptr)
	  (let* ((md.len MD5_DIGEST_SIZE)
		 (md.ptr (malloc-block/c md.len)))
	    (md5_digest ctx* md.len md.ptr)
	    (let* ((b16*	(malloc-block/c sizeof-base16_decode_ctx))
		   (str.len	(BASE16_DECODE_LENGTH md.len))
		   (str.ptr	(malloc-block/c str.len))
		   (str.len*	(malloc-small/c)))
	      (pointer-set-c-unsigned-int! str.len* 0 str.len)
	      (base16_decode_init   b16*)
	      (base16_decode_update b16* str.len* str.ptr md.len md.ptr)
	      (base16_decode_final  b16*)
	      (cstring->string str.ptr str.len)
	      (memblock->string-hex (make-<memblock> md.ptr md.len #f))
	      ))))
    => "900150983CD24FB0D6963F7D28E17F72")

  #t)


;;;; done

(check-report)

;;; end of file
