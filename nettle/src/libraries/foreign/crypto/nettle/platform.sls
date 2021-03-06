;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Nettle
;;;Contents: bindings to foreign functions
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


(library (foreign crypto nettle platform)
  (export
    aes_set_encrypt_key
    aes_set_decrypt_key
    aes_encrypt
    aes_decrypt

    arcfour_set_key
    arcfour_crypt
    arcfour_stream

    arctwo_set_key_ekb
    arctwo_set_key
    arctwo_set_key_gutmann
    arctwo_encrypt
    arctwo_decrypt

    ;; asn1_der_iterator_first
    ;; asn1_der_iterator_next
    ;; asn1_der_decode_constructed
    ;; asn1_der_decode_constructed_last
    ;; asn1_der_decode_bitstring
    ;; asn1_der_decode_bitstring_last
    ;; asn1_der_get_uint32

    base16_encode_single
    base16_encode_update
    base16_decode_init
    base16_decode_single
    base16_decode_update
    base16_decode_final

    BASE16_ENCODE_LENGTH
    BASE16_DECODE_LENGTH
    BASE64_ENCODE_LENGTH
    BASE64_ENCODE_RAW_LENGTH
    BASE64_DECODE_LENGTH

    base64_encode_init
    base64_encode_single
    base64_encode_update
    base64_encode_final
    base64_encode_raw
    base64_encode_group
    base64_decode_init
    base64_decode_single
    base64_decode_update
    base64_decode_final

    blowfish_set_key
    blowfish_encrypt
    blowfish_decrypt

    nettle_buffer_init
    nettle_buffer_init_realloc
    nettle_buffer_init_size
    nettle_buffer_clear
    nettle_buffer_reset
    nettle_buffer_grow
    nettle_buffer_write
    nettle_buffer_space
    nettle_buffer_copy

    NETTLE_BUFFER_PUTC

    cast128_set_key
    cast128_encrypt
    cast128_decrypt

    cbc_encrypt
    cbc_decrypt

    ctr_crypt

    des_set_key
    des_encrypt
    des_decrypt
    des_fix_parity

    des3_set_key
    des3_encrypt
    des3_decrypt

    hmac_set_key
    hmac_update
    hmac_digest
    hmac_md5_set_key
    hmac_md5_update
    hmac_md5_digest
    hmac_sha1_set_key
    hmac_sha1_update
    hmac_sha1_digest
    hmac_sha256_set_key
    hmac_sha256_update
    hmac_sha256_digest

    knuth_lfib_init
    knuth_lfib_get
    knuth_lfib_get_array
    knuth_lfib_random

    md2_init
    md2_update
    md2_digest

    md4_init
    md4_update
    md4_digest

    md5_init
    md5_update
    md5_digest
    _nettle_md5_compress

    memxor
    memxor3

    serpent_set_key
    serpent_encrypt
    serpent_decrypt

    sha1_init
    sha1_update
    sha1_digest
    _nettle_sha1_compress

    sha256_init
    sha256_update
    sha256_digest

    twofish_set_key
    twofish_encrypt
    twofish_decrypt

    yarrow256_init
    yarrow256_seed
    yarrow256_update
    yarrow256_random
    yarrow256_is_seeded
    yarrow256_needed_sources
    yarrow256_fast_reseed
    yarrow256_slow_reseed
    yarrow_key_event_init
    yarrow_key_event_estimate)
  (import (rnrs)
    (foreign ffi)
    (foreign ffi sizeof)
    (only (foreign memory) memcpy)
    (foreign crypto nettle shared-object)
    (foreign crypto nettle sizeof))


(define-c-functions nettle-shared-object
  (aes_set_encrypt_key		(void nettle_aes_set_encrypt_key (pointer unsigned pointer)))
  (aes_set_decrypt_key		(void nettle_aes_set_decrypt_key (pointer unsigned pointer)))
  (aes_encrypt			(void nettle_aes_encrypt (pointer unsigned pointer pointer)))
  (aes_decrypt			(void nettle_aes_decrypt (pointer unsigned pointer pointer))))

(define-c-functions nettle-shared-object
  (arcfour_set_key		(void nettle_arcfour_set_key (pointer unsigned pointer)))
  (arcfour_crypt		(void nettle_arcfour_crypt (pointer unsigned pointer pointer)))
  (arcfour_stream		(void nettle_arcfour_stream (pointer unsigned pointer))))

(define-c-functions nettle-shared-object
  (arctwo_set_key_ekb		(void nettle_arctwo_set_key_ekb (pointer unsigned pointer unsigned)))
  (arctwo_set_key		(void nettle_arctwo_set_key (pointer unsigned pointer)))
  (arctwo_set_key_gutmann	(void nettle_arctwo_set_key_gutmann (pointer unsigned pointer)))
  (arctwo_encrypt		(void nettle_arctwo_encrypt (pointer unsigned pointer pointer)))
  (arctwo_decrypt		(void nettle_arctwo_decrypt (pointer unsigned pointer pointer))))

;; (define-c-functions nettle-shared-object
;;   (asn1_der_iterator_first
;;    (asn1_iterator_result nettle_asn1_der_iterator_first (pointer unsigned pointer)))
;;   (asn1_der_iterator_next
;;    (asn1_iterator_result nettle_asn1_der_iterator_next (pointer)))
;;   (asn1_der_decode_constructed
;;    (asn1_iterator_result nettle_asn1_der_decode_constructed (pointer pointer)))
;;   (asn1_der_decode_constructed_last
;;    (asn1_iterator_result nettle_asn1_der_decode_constructed_last (pointer)))
;;   (asn1_der_decode_bitstring
;;    (asn1_iterator_result nettle_asn1_der_decode_bitstring (pointer pointer)))
;;   (asn1_der_decode_bitstring_last
;;    (asn1_iterator_result nettle_asn1_der_decode_bitstring_last (pointer)))
;;   (asn1_der_get_uint32
;;    (int nettle_asn1_der_get_uint32 (pointer pointer))))

(define-c-functions nettle-shared-object
  (base16_encode_single		(void nettle_base16_encode_single (pointer uint8_t)))
  (base16_encode_update		(void nettle_base16_encode_update (pointer unsigned pointer)))
  (base16_decode_init		(void nettle_base16_decode_init (pointer)))
  (base16_decode_single		(int nettle_base16_decode_single (pointer pointer uint8_t)))
  (base16_decode_update
   (int nettle_base16_decode_update (pointer pointer pointer unsigned pointer)))
  (base16_decode_final		(int nettle_base16_decode_final (pointer))))

(define (BASE16_ENCODE_LENGTH len)
  (* len 2))

(define (BASE16_DECODE_LENGTH len)
  (ceiling (/ (+ 1 len) 2)))

(define (BASE64_ENCODE_LENGTH len)
  (ceiling (/ (+ (* len 8) 4) 6)))

(define (BASE64_ENCODE_RAW_LENGTH len)
  (ceiling (* (/ (+ len 2) 3) 4)))

(define (BASE64_DECODE_LENGTH len)
  (ceiling (/ (* (+ len 1) 6) 8)))

(define-c-functions nettle-shared-object
  (base64_encode_init		(void nettle_base64_encode_init (pointer)))
  (base64_encode_single		(unsigned nettle_base64_encode_single (pointer pointer uint8_t)))
  (base64_encode_update		(unsigned nettle_base64_encode_update (pointer pointer unsigned pointer)))
  (base64_encode_final		(unsigned nettle_base64_encode_final (pointer pointer)))
  (base64_encode_raw		(void nettle_base64_encode_raw (pointer unsigned pointer)))
  (base64_encode_group		(void nettle_base64_encode_group (pointer uint32_t)))
  (base64_decode_init		(void nettle_base64_decode_init (pointer)))
  (base64_decode_single		(int nettle_base64_decode_single (pointer pointer uint8_t)))
  (base64_decode_update
   (int nettle_base64_decode_update (pointer pointer pointer unsigned pointer)))
  (base64_decode_final		(int nettle_base64_decode_final (pointer))))

(define-c-functions nettle-shared-object
  (blowfish_set_key		(int nettle_blowfish_set_key (pointer unsigned pointer)))
  (blowfish_encrypt		(void nettle_blowfish_encrypt (pointer unsigned pointer pointer)))
  (blowfish_decrypt		(void nettle_blowfish_decrypt (pointer unsigned pointer pointer))))

(define-c-functions nettle-shared-object
  (nettle_buffer_init		(void nettle_buffer_init (pointer)))
  (nettle_buffer_init_realloc	(void nettle_buffer_init_realloc (pointer pointer nettle_realloc_func)))
  (nettle_buffer_init_size	(void nettle_buffer_init_size (pointer unsigned pointer)))
  (nettle_buffer_clear		(void nettle_buffer_clear (pointer)))
  (nettle_buffer_reset		(void nettle_buffer_reset (pointer)))
  (nettle_buffer_grow		(int nettle_buffer_grow (pointer unsigned)))
  (nettle_buffer_write		(int nettle_buffer_write (pointer unsigned pointer)))
  (nettle_buffer_space		(pointer nettle_buffer_space (pointer unsigned)))
  (nettle_buffer_copy		(int nettle_buffer_copy (pointer pointer))))

(define (NETTLE_BUFFER_PUTC buffer c)
  (let ((size (struct-nettle_buffer-size-ref buffer)))
  (unless (< size (struct-nettle_buffer-alloc-ref buffer))
    (nettle_buffer_grow buffer 1))
  (let ((p    (struct-nettle_buffer-contents-ref buffer))
	(size (+ 1 size)))
    (pointer-set-c-unsigned-char! p size)
    (struct-nettle_buffer-size-set! buffer size))))

(define-c-functions nettle-shared-object
  (cast128_set_key		(void nettle_cast128_set_key (pointer unsigned pointer)))
  (cast128_encrypt		(void nettle_cast128_encrypt (pointer unsigned pointer pointer)))
  (cast128_decrypt		(void nettle_cast128_decrypt (pointer unsigned pointer pointer))))

(define-c-functions nettle-shared-object
  (cbc_encrypt
   (void nettle_cbc_encrypt (pointer nettle_crypt_func unsigned pointer unsigned pointer pointer)))
  (cbc_decrypt
   (void nettle_cbc_decrypt (pointer nettle_crypt_func unsigned pointer unsigned pointer pointer))))

(define-c-functions nettle-shared-object
  (ctr_crypt
   (void nettle_ctr_crypt (pointer nettle_crypt_func unsigned pointer unsigned pointer pointer))))

(define-c-functions nettle-shared-object
  (des_set_key			(int nettle_des_set_key (pointer pointer)))
  (des_encrypt			(void nettle_des_encrypt (pointer unsigned pointer pointer)))
  (des_decrypt			(void nettle_des_decrypt (pointer unsigned pointer pointer)))
  (des_fix_parity		(void nettle_des_fix_parity (unsigned pointer pointer)))
  (des3_set_key			(int nettle_des3_set_key (pointer pointer)))
  (des3_encrypt			(void nettle_des3_encrypt (pointer unsigned pointer pointer)))
  (des3_decrypt			(void nettle_des3_decrypt (pointer unsigned pointer pointer))))

(define-c-functions nettle-shared-object
  (hmac_set_key
   (void nettle_hmac_set_key (pointer pointer pointer pointer unsigned pointer)))
  (hmac_update			(void nettle_hmac_update (pointer pointer unsigned pointer)))
  (hmac_digest
   (void nettle_hmac_digest (pointer pointer pointer pointer unsigned pointer)))
  (hmac_md5_set_key		(void nettle_hmac_md5_set_key (pointer unsigned pointer)))
  (hmac_md5_update		(void nettle_hmac_md5_update (pointer unsigned pointer)))
  (hmac_md5_digest		(void nettle_hmac_md5_digest (pointer unsigned pointer)))
  (hmac_sha1_set_key		(void nettle_hmac_sha1_set_key (pointer unsigned pointer)))
  (hmac_sha1_update		(void nettle_hmac_sha1_update (pointer unsigned pointer)))
  (hmac_sha1_digest		(void nettle_hmac_sha1_digest (pointer unsigned pointer)))
  (hmac_sha256_set_key		(void nettle_hmac_sha256_set_key (pointer unsigned pointer)))
  (hmac_sha256_update		(void nettle_hmac_sha256_update (pointer unsigned pointer)))
  (hmac_sha256_digest		(void nettle_hmac_sha256_digest (pointer unsigned pointer))))

(define-c-functions nettle-shared-object
  (knuth_lfib_init		(void nettle_knuth_lfib_init (pointer uint32_t)))
  (knuth_lfib_get		(uint32_t nettle_knuth_lfib_get (pointer)))
  (knuth_lfib_get_array		(void nettle_knuth_lfib_get_array (pointer unsigned uint32_t)))
  (knuth_lfib_random		(void nettle_knuth_lfib_random (pointer unsigned pointer))))

(define-c-functions nettle-shared-object
  (md2_init			(void nettle_md2_init (pointer)))
  (md2_update			(void nettle_md2_update (pointer unsigned pointer)))
  (md2_digest			(void nettle_md2_digest (pointer unsigned pointer))))

(define-c-functions nettle-shared-object
  (md4_init			(void nettle_md4_init (pointer)))
  (md4_update			(void nettle_md4_update (pointer unsigned pointer)))
  (md4_digest			(void nettle_md4_digest (pointer unsigned pointer))))

(define-c-functions nettle-shared-object
  (md5_init			(void nettle_md5_init (pointer)))
  (md5_update			(void nettle_md5_update (pointer unsigned pointer)))
  (md5_digest			(void nettle_md5_digest (pointer unsigned pointer)))
  (_nettle_md5_compress		(void _nettle_md5_compress (pointer pointer))))

(define-c-functions nettle-shared-object
  (memxor			(pointer memxor (pointer pointer size_t)))
  (memxor3			(pointer memxor3 (pointer pointer pointer size_t))))

;; (define-c-functions nettle-shared-object
;;   (nettle_realloc		())
;;   (nettle_xrealloc		()))

(define-c-functions nettle-shared-object
  (serpent_set_key		(void nettle_serpent_set_key (pointer unsigned pointer)))
  (serpent_encrypt		(void nettle_serpent_encrypt (pointer unsigned pointer pointer)))
  (serpent_decrypt		(void nettle_serpent_decrypt (pointer unsigned pointer pointer))))

(define-c-functions nettle-shared-object
  (sha1_init			(void nettle_sha1_init (pointer)))
  (sha1_update			(void nettle_sha1_update (pointer unsigned pointer)))
  (sha1_digest			(void nettle_sha1_digest (pointer unsigned pointer)))
  (_nettle_sha1_compress	(void _nettle_sha1_compress (pointer pointer))))

(define-c-functions nettle-shared-object
  (sha256_init			(void nettle_sha256_init (pointer)))
  (sha256_update		(void nettle_sha256_update (pointer unsigned pointer)))
  (sha256_digest		(void nettle_sha256_digest (pointer unsigned pointer))))

(define-c-functions nettle-shared-object
  (twofish_set_key		(void nettle_twofish_set_key (pointer unsigned pointer)))
  (twofish_encrypt		(void nettle_twofish_encrypt (pointer unsigned pointer pointer)))
  (twofish_decrypt		(void nettle_twofish_decrypt (pointer unsigned pointer pointer))))

(define-c-functions nettle-shared-object
  (yarrow256_init		(void nettle_yarrow256_init (pointer unsigned pointer)))
  (yarrow256_seed		(void nettle_yarrow256_seed (pointer unsigned pointer)))
  (yarrow256_update
   (int nettle_yarrow256_update (pointer unsigned unsigned unsigned pointer)))
  (yarrow256_random		(void nettle_yarrow256_random (pointer unsigned pointer)))
  (yarrow256_is_seeded		(int nettle_yarrow256_is_seeded (pointer)))
  (yarrow256_needed_sources	(unsigned nettle_yarrow256_needed_sources (pointer)))
  (yarrow256_fast_reseed	(void nettle_yarrow256_fast_reseed (pointer)))
  (yarrow256_slow_reseed	(void nettle_yarrow256_slow_reseed (pointer)))
  (yarrow_key_event_init	(void nettle_yarrow_key_event_init (pointer)))
  (yarrow_key_event_estimate	(unsigned nettle_yarrow_key_event_estimate (pointer unsigned unsigned))))


;;;; done

)

;;; end of file
