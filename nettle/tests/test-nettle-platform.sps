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

  (define (str->base16 str)
    (with-compensations
      (let* ((in.ptr	(string->cstring/c str))
	     (in.len	(strlen in.ptr))
	     (out.len	(BASE16_ENCODE_LENGTH in.len))
	     (out.ptr	(malloc-block/c out.len)))
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

  #t)


(parametrise ((check-test-name	'base64))

  (define (str->base64 str)
    (with-compensations
      (let* ((in.ptr	(string->cstring/c str))
	     (in.len	(strlen in.ptr))
	     (b64*	(malloc-block/c sizeof-base64_encode_ctx))
	     (out.len	(+ (BASE64_ENCODE_LENGTH in.len) BASE64_ENCODE_FINAL_LENGTH))
	     (out.ptr	(malloc-block/c out.len)))
	(let ((out.len1 (base64_encode_update b64* out.ptr in.len in.ptr)))
	  (let ((out.len2 (base64_encode_final b64* (pointer-add out.ptr out.len1))))
	    (cstring->string out.ptr (+ out.len1 out.len2)))))))

  (define (base64->str str)
    (let* ((in.ptr	(string->cstring/c str))
	   (in.len	(strlen in.ptr))
	   (b64*	(malloc-block/c sizeof-base64_decode_ctx))
	   (out.len	(BASE64_DECODE_LENGTH in.len))
	   (out.ptr	(malloc-block/c out.len))
	   (out.len*	(malloc-small/c)))
      (pointer-set-c-unsigned-int! out.len* 0 out.len)
      (base64_decode_init   b64*)
      (base64_decode_update b64* out.len* out.ptr in.len in.ptr)
      (assert (= 1 (base64_decode_final b64*)))
      (cstring->string out.ptr (pointer-ref-c-unsigned-int out.len* 0))))

  (let ((a "ABC") (b "QUJD"))
    (check (str->base64 a)	=> b)
    (check (base64->str b)	=> a))

  (let ((a "") (b ""))
    (check (str->base64 a)	=> b)
    (check (base64->str b)	=> a))

  (let ((a "H") (b "SA=="))
    (check (str->base64 a)	=> b)
    (check (base64->str b)	=> a))

  (let ((a "He") (b "SGU="))
    (check (str->base64 a)	=> b)
    (check (base64->str b)	=> a))

  (let ((a "Hel") (b "SGVs"))
    (check (str->base64 a)	=> b)
    (check (base64->str b)	=> a))

  (let ((a "Hell") (b "SGVsbA=="))
    (check (str->base64 a)	=> b)
    (check (base64->str b)	=> a))

  (let ((a "Hello") (b "SGVsbG8="))
    (check (str->base64 a)	=> b)
    (check (base64->str b)	=> a))

  #t)


(parametrise ((check-test-name	'md))

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
	    (memblock->string-hex (make-<memblock> md.ptr md.len #f)))))
    => "900150983CD24FB0D6963F7D28E17F72")

  #t)


;;;; done

(check-report)

;;; end of file
