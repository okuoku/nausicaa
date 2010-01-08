;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Nettle
;;;Contents: bindings to foreign functions for Hogweed
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


(library (foreign crypto hogweed platform)
  (export
    nettle_mpz_sizeinbase_256_s
    nettle_mpz_sizeinbase_256_u
    nettle_mpz_get_str_256
    nettle_mpz_set_str_256_s
    nettle_mpz_init_set_str_256_s
    nettle_mpz_set_str_256_u
    nettle_mpz_init_set_str_256_u
    nettle_mpz_random_size

    nettle_mpz_random
    nettle_next_prime

    nettle_mpz_set_sexp
    nettle_asn1_der_get_bignum

    dsa_public_key_init
    dsa_public_key_clear
    dsa_private_key_init
    dsa_private_key_clear
    dsa_signature_init
    dsa_signature_clear
    dsa_sign
    dsa_verify
    dsa_sign_digest
    dsa_verify_digest
    dsa_generate_keypair
    dsa_signature_from_sexp
    dsa_keypair_from_sexp_alist
    dsa_keypair_from_sexp

    pgp_put_uint32
    pgp_put_uint16
    pgp_put_mpi
    pgp_put_string
    pgp_put_length
    pgp_put_header
    pgp_put_header_length
    pgp_sub_packet_start
    pgp_put_sub_packet
    pgp_sub_packet_end
    pgp_put_public_rsa_key
    pgp_put_rsa_sha1_signature
    pgp_put_userid
    pgp_crc24
    pgp_armor

    pkcs1_signature_prefix
    pkcs1_rsa_md5_encode
    pkcs1_rsa_md5_encode_digest
    pkcs1_rsa_sha1_encode
    pkcs1_rsa_sha1_encode_digest
    pkcs1_rsa_sha256_encode
    pkcs1_rsa_sha256_encode_digest

    rsa_public_key_init
    rsa_public_key_clear
    rsa_public_key_prepare
    rsa_private_key_init
    rsa_private_key_clear
    rsa_private_key_prepare
    rsa_md5_sign
    rsa_md5_verify
    rsa_sha1_sign
    rsa_sha1_verify
    rsa_sha256_sign
    rsa_sha256_verify
    rsa_md5_sign_digest
    rsa_md5_verify_digest
    rsa_sha1_sign_digest
    rsa_sha1_verify_digest
    rsa_sha256_sign_digest
    rsa_sha256_verify_digest
    rsa_encrypt
    rsa_decrypt
    rsa_compute_root
    rsa_generate_keypair

    rsa_keypair_to_sexp
    rsa_keypair_from_sexp_alist
    rsa_keypair_from_sexp
    rsa_public_key_from_der_iterator
    rsa_private_key_from_der_iterator
    rsa_keypair_from_der
    rsa_keypair_to_openpgp

    sexp_iterator_first
    sexp_transport_iterator_first
    sexp_iterator_next
    sexp_iterator_enter_list
    sexp_iterator_exit_list
    sexp_iterator_subexpr
    sexp_iterator_get_uint32
    sexp_iterator_check_type
    sexp_iterator_check_types
    sexp_iterator_assoc)
  (import (rnrs)
    (foreign ffi)
    (foreign ffi sizeof)
    (foreign crypto hogweed shared-object)
    (foreign crypto nettle sizeof))


(define-c-functions hogweed-shared-object
  (nettle_mpz_sizeinbase_256_s		(unsigned nettle_mpz_sizeinbase_256_s (mpz_t)))
  (nettle_mpz_sizeinbase_256_u		(unsigned nettle_mpz_sizeinbase_256_u (mpz_t)))
  (nettle_mpz_get_str_256		(void nettle_mpz_get_str_256 (unsigned pointer mpz_t)))
  (nettle_mpz_set_str_256_s		(void nettle_mpz_set_str_256_s (mpz_t unsigned pointer)))
  (nettle_mpz_init_set_str_256_s	(void nettle_mpz_init_set_str_256_s (mpz_t unsigned pointer)))
  (nettle_mpz_set_str_256_u		(void nettle_mpz_set_str_256_u (mpz_t unsigned pointer)))
  (nettle_mpz_init_set_str_256_u	(void nettle_mpz_init_set_str_256_u (mpz_t unsigned pointer)))
  (nettle_mpz_random_size		(void nettle_mpz_random_size
					      (mpz_t pointer nettle_random_func unsigned)))
  (nettle_mpz_random			(void nettle_mpz_random (mpz_t pointer nettle_random_func mpz_t)))
  (nettle_next_prime			(void nettle_next_prime (mpz_t mpz_t unsigned unsigned
								       pointer nettle_progress_func)))
  (nettle_mpz_set_sexp			(int nettle_mpz_set_sexp (mpz_t unsigned pointer)))
  (nettle_asn1_der_get_bignum		(int nettle_asn1_der_get_bignum (pointer mpz_t unsigned))))

(define-c-functions hogweed-shared-object
  (dsa_public_key_init			(void dsa_public_key_init (pointer)))
  (dsa_public_key_clear			(void dsa_public_key_clear (pointer)))
  (dsa_private_key_init			(void dsa_private_key_init (pointer)))
  (dsa_private_key_clear		(void dsa_private_key_clear (pointer)))
  (dsa_signature_init			(void dsa_signature_init (pointer)))
  (dsa_signature_clear			(void dsa_signature_clear (pointer)))
  (dsa_sign				(void dsa_sign (pointer pointer pointer nettle_random_func
							pointer pointer)))
  (dsa_verify				(int dsa_verify (pointer pointer pointer)))
  (dsa_sign_digest			(void dsa_sign_digest (pointer pointer pointer nettle_random_func
								       pointer pointer)))
  (dsa_verify_digest			(int dsa_verify_digest (pointer pointer pointer)))
  (dsa_generate_keypair			(int dsa_generate_keypair (pointer pointer
								   pointer nettle_random_func
								   pointer nettle_progress_func
								   unsigned)))
  (dsa_signature_from_sexp		(int dsa_signature_from_sexp (pointer pointer)))
  (dsa_keypair_from_sexp_alist		(int dsa_keypair_from_sexp_alist (pointer pointer
									  unsigned pointer)))
  (dsa_keypair_from_sexp		(int dsa_keypair_from_sexp (pointer pointer unsigned
								    unsigned pointer))))

(define-c-functions hogweed-shared-object
  (pgp_put_uint32		(int pgp_put_uint32 (pointer uint32_t)))
  (pgp_put_uint16		(int pgp_put_uint16 (pointer unsigned)))
  (pgp_put_mpi			(int pgp_put_mpi (pointer pointer)))
  (pgp_put_string		(int pgp_put_string (pointer unsigned pointer)))
  (pgp_put_length		(int pgp_put_length (pointer unsigned)))
  (pgp_put_header		(int pgp_put_header (pointer unsigned unsigned)))
  (pgp_put_header_length	(void pgp_put_header_length (pointer unsigned unsigned)))
  (pgp_sub_packet_start		(unsigned pgp_sub_packet_start (pointer)))
  (pgp_put_sub_packet		(int pgp_put_sub_packet (pointer unsigned unsigned pointer)))
  (pgp_sub_packet_end		(void pgp_sub_packet_end (pointer unsigned)))
  (pgp_put_public_rsa_key	(int pgp_put_public_rsa_key (pointer pointer time_t)))
  (pgp_put_rsa_sha1_signature	(int pgp_put_rsa_sha1_signature (pointer pointer pointer
								 unsigned pointer)))
  (pgp_put_userid		(int pgp_put_userid (pointer unsigned pointer)))
  (pgp_crc24			(uint32_t pgp_crc24 (unsigned pointer)))
  (pgp_armor			(int pgp_armor (pointer char* unsigned pointer))))

(define-c-functions hogweed-shared-object
  (pkcs1_signature_prefix	(void pkcs1_signature_prefix (unsigned pointer unsigned pointer)))
  (pkcs1_rsa_md5_encode		(void pkcs1_rsa_md5_encode (pointer unsigned pointer)))
  (pkcs1_rsa_md5_encode_digest	(void pkcs1_rsa_md5_encode_digest (pointer unsigned pointer)))
  (pkcs1_rsa_sha1_encode	(void pkcs1_rsa_sha1_encode (pointer unsigned pointer)))
  (pkcs1_rsa_sha1_encode_digest	(void pkcs1_rsa_sha1_encode_digest (pointer unsigned pointer)))
  (pkcs1_rsa_sha256_encode	(void pkcs1_rsa_sha256_encode (pointer unsigned pointer)))
  (pkcs1_rsa_sha256_encode_digest (void pkcs1_rsa_sha256_encode_digest (pointer unsigned pointer))))

(define-c-functions hogweed-shared-object
  (rsa_public_key_init		(void rsa_public_key_init (pointer)))
  (rsa_public_key_clear		(void rsa_public_key_clear (pointer)))
  (rsa_public_key_prepare	(int rsa_public_key_prepare (pointer)))
  (rsa_private_key_init		(void rsa_private_key_init (pointer)))
  (rsa_private_key_clear	(void rsa_private_key_clear (pointer)))
  (rsa_private_key_prepare	(int rsa_private_key_prepare (pointer)))
  (rsa_md5_sign			(void rsa_md5_sign (pointer pointer pointer)))
  (rsa_md5_verify		(int rsa_md5_verify (pointer pointer pointer)))
  (rsa_sha1_sign		(void rsa_sha1_sign (pointer pointer pointer)))
  (rsa_sha1_verify		(int rsa_sha1_verify (pointer pointer pointer)))
  (rsa_sha256_sign		(void rsa_sha256_sign (pointer pointer pointer)))
  (rsa_sha256_verify		(int rsa_sha256_verify (pointer pointer pointer)))
  (rsa_md5_sign_digest		(void rsa_md5_sign_digest (pointer pointer pointer)))
  (rsa_md5_verify_digest	(int rsa_md5_verify_digest (pointer pointer pointer)))
  (rsa_sha1_sign_digest		(void rsa_sha1_sign_digest (pointer pointer pointer)))
  (rsa_sha1_verify_digest	(int rsa_sha1_verify_digest (pointer pointer pointer)))
  (rsa_sha256_sign_digest	(void rsa_sha256_sign_digest (pointer pointer pointer)))
  (rsa_sha256_verify_digest	(int rsa_sha256_verify_digest (pointer pointer pointer)))
  (rsa_encrypt			(int rsa_encrypt (pointer pointer nettle_random_func
							  unsigned pointer pointer)))
  (rsa_decrypt			(int rsa_decrypt (pointer pointer pointer pointer)))
  (rsa_compute_root		(void rsa_compute_root (pointer pointer pointer)))
  (rsa_generate_keypair		(int rsa_generate_keypair (pointer pointer
							   pointer nettle_random_func
							   pointer nettle_progress_func
							   unsigned unsigned)))

  (rsa_keypair_to_sexp		(int rsa_keypair_to_sexp (pointer char* pointer pointer)))
  (rsa_keypair_from_sexp_alist	(int rsa_keypair_from_sexp_alist (pointer pointer unsigned pointer)))
  (rsa_keypair_from_sexp	(int rsa_keypair_from_sexp (pointer pointer unsigned unsigned pointer)))
  (rsa_public_key_from_der_iterator (int rsa_public_key_from_der_iterator (pointer unsigned pointer)))
  (rsa_private_key_from_der_iterator (int rsa_private_key_from_der_iterator (pointer pointer
									     unsigned pointer)))
  (rsa_keypair_from_der		(int rsa_keypair_from_der (pointer pointer unsigned unsigned pointer)))
  (rsa_keypair_to_openpgp	(int rsa_keypair_to_openpgp (pointer pointer pointer char*))))


(define-c-functions hogweed-shared-object
  (sexp_iterator_first		(int sexp_iterator_first (pointer unsigned pointer)))
  (sexp_transport_iterator_first (int sexp_transport_iterator_first (pointer unsigned pointer)))
  (sexp_iterator_next		(int sexp_iterator_next (pointer)))
  (sexp_iterator_enter_list	(int sexp_iterator_enter_list (pointer)))
  (sexp_iterator_exit_list	(int sexp_iterator_exit_list (pointer)))
  (sexp_iterator_subexpr	(pointer sexp_iterator_subexpr (pointer pointer)))
  (sexp_iterator_get_uint32	(int sexp_iterator_get_uint32 (pointer pointer)))
  (sexp_iterator_check_type	(int sexp_iterator_check_type (pointer pointer)))
  (sexp_iterator_check_types	(pointer sexp_iterator_check_types (pointer unsigned pointer)))
  (sexp_iterator_assoc		(int sexp_iterator_assoc (pointer unsigned pointer pointer)))
;;; Variadic!!!
;;;  (sexp_format			(unsigned sexp_format (pointer char* ...)))
;;;  (sexp_vformat			(unsigned sexp_vformat (pointer char* va_list)))
;;;  (sexp_transport_format	(unsigned sexp_transport_format (pointer char* ...)))
;;;  (sexp_transport_vformat	(unsigned sexp_transport_vformat (pointer char* va_list)))
  )



;;;; done

)

;;; end of file
