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
  (only (foreign ffi) make-c-callback*)
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

  (let ()

    (define (string->md5 str)
      (with-compensations
	(let* ((buf.ptr	(string->cstring str))
	       (buf.len	(strlen buf.ptr))
	       (ctx*	(malloc-block/c sizeof-md5_ctx)))
	  (md5_init ctx*)
	  (md5_update ctx* buf.len buf.ptr)
	  (let* ((md.len MD5_DIGEST_SIZE)
		 (md.ptr (malloc-block/c md.len)))
	    (md5_digest ctx* md.len md.ptr)
	    (memblock->string-hex (make-<memblock> md.ptr md.len #f))))))

    (define (string->md5-hmac key str)
      (with-compensations
	(let* ((buf.ptr	(string->cstring str))
	       (buf.len	(strlen buf.ptr))
	       (key.ptr	(string->cstring key))
	       (key.len	(strlen key.ptr))
	       (ctx*	(malloc-block/c sizeof-hmac_md5_ctx)))
	  (hmac_md5_set_key ctx* key.len key.ptr)
	  (hmac_md5_update ctx* buf.len buf.ptr)
	  (let* ((mac.len MD5_DIGEST_SIZE)
		 (mac.ptr (malloc-block/c mac.len)))
	    (hmac_md5_digest ctx* mac.len mac.ptr)
	    (memblock->string-hex (make-<memblock> mac.ptr mac.len #f))))))

    (check
	(string->md5 "abc")
      => "900150983CD24FB0D6963F7D28E17F72")

    (check
	(string->md5-hmac "Jefe" "what do ya want for nothing?")
      => "750C783E6AB0B503EAA86E310A5DB738")

    #f)

;;; --------------------------------------------------------------------

  (let ()

    (define (string->sha256 str)
      (with-compensations
	(let* ((buf.ptr	(string->cstring str))
	       (buf.len	(strlen buf.ptr))
	       (ctx*	(malloc-block/c sizeof-sha256_ctx)))
	  (sha256_init ctx*)
	  (sha256_update ctx* buf.len buf.ptr)
	  (let* ((md.len SHA256_DIGEST_SIZE)
		 (md.ptr (malloc-block/c md.len)))
	    (sha256_digest ctx* md.len md.ptr)
	    (memblock->string-hex (make-<memblock> md.ptr md.len #f))))))

    (define (string->sha256-hmac key str)
      (with-compensations
	(let* ((buf.ptr	(string->cstring str))
	       (buf.len	(strlen buf.ptr))
	       (mb	(string-hex->memblock key malloc-block/c))
	       (ctx*	(malloc-block/c sizeof-hmac_sha256_ctx)))
	  (hmac_sha256_set_key ctx* (<memblock>-size mb) (<memblock>-pointer mb))
	  (hmac_sha256_update ctx* buf.len buf.ptr)
	  (let* ((mac.len SHA256_DIGEST_SIZE)
		 (mac.ptr (malloc-block/c mac.len)))
	    (hmac_sha256_digest ctx* mac.len mac.ptr)
	    (memblock->string-hex (make-<memblock> mac.ptr mac.len #f))))))

    (check
	(string->sha256 "abc")
      => "BA7816BF8F01CFEA414140DE5DAE2223B00361A396177A9CB410FF61F20015AD")

    (check
	(string->sha256-hmac "0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20"
			     "abc")
      => "A21B1F5D4CF4F73A4DD939750F7A066A7F98CC131CB16A6692759021CFAB8181")

    #f)

  #t)


(parametrise ((check-test-name	'cipher-ecb))

  (let ()

    (define (aes-encrypt key bv)
      (with-compensations
	(let* ((in.ptr		(bytevector->pointer bv malloc-block/c))
	       (in.len		(bytevector-length bv))
	       (key.ptr		(string->cstring key))
	       (key.len		(strlen key.ptr))
	       (ou.len		AES_BLOCK_SIZE)
	       (ou.ptr		(malloc-block/c ou.len))
	       (aes*		(malloc-block/c sizeof-aes_ctx)))
	  (aes_set_encrypt_key aes* key.len key.ptr)
	  (aes_encrypt aes* in.len ou.ptr in.ptr)
	  (pointer->bytevector ou.ptr ou.len))))

    (define (aes-decrypt key bv)
      (with-compensations
	(let* ((in.ptr		(bytevector->pointer bv malloc-block/c))
	       (in.len		(bytevector-length bv))
	       (key.ptr		(string->cstring key))
	       (key.len		(strlen key.ptr))
	       (ou.len		AES_BLOCK_SIZE)
	       (ou.ptr		(malloc-block/c ou.len))
	       (aes*		(malloc-block/c sizeof-aes_ctx)))
	  (aes_set_decrypt_key aes* key.len key.ptr)
	  (aes_decrypt aes* in.len ou.ptr in.ptr)
	  (pointer->bytevector ou.ptr ou.len))))

    (let ((data '#vu8(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)))

      (check
	  (let* ((keyword	"abcdefghilmnopqr")
		 (enc		(aes-encrypt keyword data)))
	    (aes-decrypt keyword enc))
	=> data)

      #f)
    #f)

  #t)


(parametrise ((check-test-name	'cipher-ecb))

  (with-compensations

    (let* ((len		AES_BLOCK_SIZE)

	   (iv		'#vu8(10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25))
	   (iv.ptr	(bytevector->pointer iv malloc-block/c))

	   (key	'#vu8(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
	   (key.ptr	(bytevector->pointer key malloc-block/c))

	   (data1	'#vu8(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
	   (data1.ptr	(bytevector->pointer key malloc-block/c))

	   (data2	'#vu8(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
	   (data2.ptr	(bytevector->pointer key malloc-block/c))

	   (aes*	(malloc-block/c sizeof-aes_ctx))

	   (enc1.ptr	(malloc-block/c len))
	   (enc2.ptr	(malloc-block/c len))

	   (encrypt	(make-c-callback* void aes_encrypt
					  (void* unsigned void* void*))))

      (aes_set_encrypt_key aes* len key.ptr)
      (cbc_encrypt aes* encrypt len iv.ptr len enc1.ptr data1.ptr)
      (cbc_encrypt aes* encrypt len iv.ptr len enc2.ptr data2.ptr)

      (let ((iv.ptr	(bytevector->pointer iv malloc-block/c))
	    (ou1.ptr	(malloc-block/c len))
	    (ou2.ptr	(malloc-block/c len))
	    (decrypt	(make-c-callback* void aes_decrypt
					  (void* unsigned void* void*))))

	(aes_set_decrypt_key aes* len key.ptr)
	(cbc_decrypt aes* decrypt len iv.ptr len ou1.ptr enc1.ptr)
	(cbc_decrypt aes* decrypt len iv.ptr len ou2.ptr enc2.ptr)

	(check
	    (memcmp ou1.ptr data1.ptr len)
	  => 0)

	(check
	    (memcmp ou2.ptr data2.ptr len)
	  => 0)

	#f))

    #f)

  #t)


(parametrise ((check-test-name	'cipher-ctr))

  (with-compensations

    (let* ((len		AES_BLOCK_SIZE)

	   (ic		'#vu8(10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25))
	   (ic.ptr	(bytevector->pointer ic malloc-block/c))

	   (key	'#vu8(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
	   (key.ptr	(bytevector->pointer key malloc-block/c))

	   (data1	'#vu8(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
	   (data1.ptr	(bytevector->pointer key malloc-block/c))

	   (data2	'#vu8(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
	   (data2.ptr	(bytevector->pointer key malloc-block/c))

	   (aes*	(malloc-block/c sizeof-aes_ctx))

	   (enc1.ptr	(malloc-block/c len))
	   (enc2.ptr	(malloc-block/c len))

	   (encrypt	(make-c-callback* void aes_encrypt
					  (void* unsigned void* void*))))

      (aes_set_encrypt_key aes* len key.ptr)
      (ctr_crypt aes* encrypt len ic.ptr len enc1.ptr data1.ptr)
      (ctr_crypt aes* encrypt len ic.ptr len enc2.ptr data2.ptr)

      (let ((ic.ptr	(bytevector->pointer ic malloc-block/c))
	    (ou1.ptr	(malloc-block/c len))
	    (ou2.ptr	(malloc-block/c len)))

	(aes_set_encrypt_key aes* len key.ptr)
	(ctr_crypt aes* encrypt len ic.ptr len ou1.ptr enc1.ptr)
	(ctr_crypt aes* encrypt len ic.ptr len ou2.ptr enc2.ptr)

	(check
	    (memcmp ou1.ptr data1.ptr len)
	  => 0)

	(check
	    (memcmp ou2.ptr data2.ptr len)
	  => 0)

	#f))

    #f)

  #t)


(parametrise ((check-test-name	'arcfour))

  (define (arcfour-key-setup arcfour* key)
    (with-compensations
      (let* ((key.ptr	(string->cstring/c key))
	     (key.len	(strlen key.ptr))
	     (sha*	(malloc-block/c sizeof-sha256_ctx))
	     (md.len	SHA256_DIGEST_SIZE)
	     (md.ptr	(malloc-block/c md.len))
	     (dummy.len 512)
	     (dummy.ptr	(malloc-block/c dummy.len)))
	  (sha256_init sha*)
	  (sha256_update sha* key.len key.ptr)
	  (sha256_digest sha* md.len  md.ptr)
	  (arcfour_set_key arcfour* md.len md.ptr)
	  (arcfour_crypt arcfour* dummy.len dummy.ptr dummy.ptr))))

  (define (arcfour-crypt key bv)
    (with-compensations
      (let* ((ctx*	(malloc-block/c sizeof-arcfour_ctx))
	     (in.ptr	(bytevector->pointer bv malloc-block/c))
	     (in.len	(bytevector-length bv))
	     (ou.ptr	(malloc-block/c in.len)))
	(arcfour-key-setup ctx* key)
	(arcfour_crypt ctx* in.len ou.ptr in.ptr)
	(pointer->bytevector ou.ptr in.len))))

  (check
      (arcfour-crypt "ciao" '#vu8(0 1 2 3 4 5 6))
    => #vu8(23 184 137 227 187 115 16))

  (check
      (arcfour-crypt "ciao" '#vu8(23 184 137 227 187 115 16))
    => '#vu8(0 1 2 3 4 5 6))

  #t)


(parametrise ((check-test-name	'knuth))

  (define (knuth-lfib->bv seed buf.len)
    (with-compensations
      (let* ((ctx*	(malloc-block/c sizeof-knuth_lfib_ctx))
	     (buf.ptr	(malloc-block/c buf.len)))
	(knuth_lfib_init ctx* seed)
	(knuth_lfib_random ctx* buf.len buf.ptr)
	(pointer->bytevector buf.ptr buf.len))))

;;;  (write (knuth-lfib->bv 543 10))(newline)

  (check
      (bytevector-length (knuth-lfib->bv 543 10))
    => 10)

  #t)


;;;; done

(check-report)

;;; end of file
