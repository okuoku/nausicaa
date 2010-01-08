;;;!mosh
;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Nettle
;;;Contents: foreign library inspection generator
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
  (foreign ffi inspector-maker))


;;;; "aes.h"

(define-c-defines "AES symmetric cipher"
  AES_BLOCK_SIZE
  AES_MIN_KEY_SIZE
  AES_MAX_KEY_SIZE
  AES_KEY_SIZE)

(define-c-struct aes_ctx
  "struct aes_ctx"
  (embedded		keys)
  (unsigned-int		nrounds))


;;;; "arcfour.h"

(define-c-defines "arcfour symmetric cipher"
  ARCFOUR_MIN_KEY_SIZE
  ARCFOUR_MAX_KEY_SIZE
  ARCFOUR_KEY_SIZE)

(define-c-struct arcfour_ctx
  "struct arcfour_ctx"
  (embedded		S)
  (unsigned-int		i)
  (unsigned-int		j))



;;;; "arctwo.h"

(define-c-defines "arctwo symmetric cipher"
  ARCTWO_BLOCK_SIZE
  ARCTWO_MIN_KEY_SIZE
  ARCTWO_MAX_KEY_SIZE
  ARCTWO_KEY_SIZE)

(define-c-struct arctwo_ctx
  "struct arctwo_ctx"
  (embedded		S))


;;;; "asn1.h"

(define-c-defines "ASN1 classes"
  ASN1_TYPE_CONSTRUCTED
  ASN1_CLASS_UNIVERSAL
  ASN1_CLASS_APPLICATION
  ASN1_CLASS_CONTEXT_SPECIFIC
  ASN1_CLASS_PRIVATE
  ASN1_CLASS_MASK
  ASN1_CLASS_SHIFT)

(define-c-enumeration asn1_type
  "enum asn1_type"
  ASN1_BOOLEAN
  ASN1_INTEGER
  ASN1_BITSTRING
  ASN1_OCTETSTRING
  ASN1_NULL
  ASN1_IDENTIFIER
  ASN1_REAL
  ASN1_ENUMERATED
  ASN1_UTF8STRING
  ASN1_SEQUENCE
  ASN1_SET
  ASN1_PRINTABLESTRING
  ASN1_TELETEXSTRING
  ASN1_IA5STRING
  ASN1_UTC
  ASN1_UNIVERSALSTRING
  ASN1_BMPSTRING)

(define-c-enumeration asn1_iterator_result
  "enum asn1_iterator_result"
  ASN1_ITERATOR_ERROR
  ASN1_ITERATOR_PRIMITIVE
  ASN1_ITERATOR_CONSTRUCTED
  ASN1_ITERATOR_END)

(define-c-struct asn1_der_iterator
  "struct asn1_der_iterator"
  (unsigned-int		buffer_length)
  (pointer		buffer)
  (unsigned-int		pos)
  (signed-int		type)
  (unsigned-int		length)
  (pointer		data))


;;;; "base16.h"

(define-c-struct  base16_decode_ctx
  "struct base16_decode_ctx"
  (unsigned-int		word)
  (unsigned-int		bits))


;;;; "base64.h"

(define-c-defines "base64 constants"
  BASE64_BINARY_BLOCK_SIZE
  BASE64_TEXT_BLOCK_SIZE
  BASE64_ENCODE_FINAL_LENGTH)

(define-c-struct base64_encode_ctx
  "struct base64_encode_ctx"
  (unsigned-int		word)
  (unsigned-int		bits))

(define-c-struct base64_decode_ctx
  "struct base64_decode_ctx"
  (unsigned-int		word)
  (unsigned-int		bits)
  (unsigned-int		padding))


;;;; "bignum.h"

(define-c-type-alias mpz_t		pointer)
(define-c-type-alias mpq_t		pointer)


;;;; "blowfish.h"

(define-c-defines "blowfish symmetric cipher"
  BLOWFISH_BLOCK_SIZE
  BLOWFISH_MIN_KEY_SIZE
  BLOWFISH_MAX_KEY_SIZE
  BLOWFISH_KEY_SIZE
  _BLOWFISH_ROUNDS)

(define-c-enumeration blowfish_error
  "enum blowfish_error"
  BLOWFISH_OK
  BLOWFISH_WEAK_KEY)

(define-c-struct blowfish_ctx
  "struct blowfish_ctx"
  (embedded		s)
  (embedded		p)
  (signed-int		status))


;;;; "buffer.h"

(define-c-struct nettle_buffer
  "struct nettle_buffer"
  (pointer		contents)
  (unsigned-int		alloc)
  (pointer		realloc_ctx)
  (pointer		realloc)
  (unsigned-int		size))


;;;; "cast128.h"

(define-c-defines "cast128 symmetric cipher"
  CAST128_BLOCK_SIZE
  CAST128_MIN_KEY_SIZE
  CAST128_MAX_KEY_SIZE
  CAST128_KEY_SIZE)

(define-c-struct cast128_ctx
  "struct cast128_ctx"
  (embedded		keys)
  (unsigned-int		rounds))


;;;; "cbc.h"

;;no definitions, only functions


;;;; "ctr.h"

;;no definitions, only functions


;;;; "des.h"

(define-c-defines "DES symmetric cipher"
  DES_KEY_SIZE
  DES_BLOCK_SIZE
  _DES_KEY_LENGTH)

(define-c-enumeration des_error
  "enum des_error"
  DES_OK
  DES_BAD_PARITY
  DES_WEAK_KEY)

(define-c-struct des_ctx
  "struct des_ctx"
  (embedded		key)
  (signed-int		status))

(define-c-defines "DES3 symmetric cipher"
  DES3_KEY_SIZE
  DES3_BLOCK_SIZE)

(define-c-struct des3_ctx
  "struct des3_ctx"
  (embedded		des)
  (signed-int		status))


;;;; "dsa.h"

(define-c-defines "DSA pubkey cipher"
  DSA_MIN_P_BITS
  DSA_Q_OCTETS
  DSA_Q_BITS)

(define-c-struct dsa_public_key
  "struct dsa_public_key"
  (embedded		p)
  (embedded		q)
  (embedded		g)
  (embedded		y))

(define-c-struct dsa_private_key
  "struct dsa_private_key"
  (embedded		x))

(define-c-struct dsa_signature
  "struct dsa_signature"
  (embedded		r)
  (embedded		s))


;;;; "hmac.h"

(define-c-struct hmac_md5_ctx
  "struct hmac_md5_ctx")

(define-c-struct hmac_sha1_ctx
  "struct hmac_sha1_ctx")

(define-c-struct hmac_sha256_ctx
  "struct hmac_sha256_ctx")


;;;; "knuth-lfib.h"

(define-c-defines "Knuth random numbers generator"
  _KNUTH_LFIB_KK)

(define-c-struct knuth_lfib_ctx
  "struct knuth_lfib_ctx"
  (embedded		x)
  (unsigned-int		index))


;;;; "md2.h"

(define-c-defines "MD2 message digest"
  MD2_DIGEST_SIZE
  MD2_DATA_SIZE)

(define-c-struct md2_ctx
  "struct md2_ctx"
  (embedded		C)
  (embedded		X)
  (embedded		block)
  (unsigned-int		index))


;;;; "md4.h"

(define-c-defines "MD4 message digest"
  MD4_DIGEST_SIZE
  MD4_DATA_SIZE
  _MD4_DIGEST_LENGTH)

(define-c-struct md4_ctx
  "struct md4_ctx"
  (embedded		digest)
  (unsigned-int		count_l)
  (unsigned-int		count_h)
  (embedded		block)
  (unsigned-int		index))


;;;; "md5.h"

(define-c-defines "MD5 message digest"
  MD5_DIGEST_SIZE
  MD5_DATA_SIZE
  _MD5_DIGEST_LENGTH)

(define-c-struct md5_ctx
  "struct md5_ctx"
  (embedded		digest)
  (unsigned-int		count_l)
  (unsigned-int		count_h)
  (embedded		block)
  (unsigned-int		index))


;;;; "memxor.h"

;; no definitions, only functions


;;;; "nettle-types.h"

(define-c-type-alias nettle_random_func		callback)
(define-c-type-alias nettle_progress_func	callback)

(define-c-type-alias nettle_set_key_func	callback)
(define-c-type-alias nettle_crypt_func		callback)

(define-c-type-alias nettle_hash_init_func	callback)
(define-c-type-alias nettle_hash_update_func	callback)
(define-c-type-alias nettle_hash_digest_func	callback)

(define-c-type-alias nettle_armor_length_func	callback)
(define-c-type-alias nettle_armor_init_func	callback)

(define-c-type-alias nettle_armor_encode_update_func	callback)
(define-c-type-alias nettle_armor_encode_final_func	callback)
(define-c-type-alias nettle_armor_decode_update_func	callback)
(define-c-type-alias nettle_armor_decode_final_func	callback)


;;;; "pgp.h"

(define-c-type time_t		signed-int)

(define-c-enumeration pgp_lengths
  "enum pgp_lengths"
  PGP_LENGTH_ONE_OCTET
  PGP_LENGTH_TWO_OCTETS
  PGP_LENGTH_FOUR_OCTETS)

(define-c-enumeration pgp_public_key_algorithm
  "enum pgp_public_key_algorithm"
  PGP_RSA
  PGP_RSA_ENCRYPT
  PGP_RSA_SIGN
  PGP_EL_GAMAL_ENCRYPT
  PGP_DSA
  PGP_EL_GAMAL)

(define-c-enumeration pgp_symmetric_algorithm
  "enum pgp_symmetric_algorithm"
  PGP_PLAINTEXT
  PGP_IDEA
  PGP_3DES
  PGP_CAST5
  PGP_BLOWFISH
  PGP_SAFER_SK
  PGP_AES128
  PGP_AES192
  PGP_AES256)

(define-c-enumeration pgp_compression_algorithm
  "enum pgp_compression_algorithm"
  PGP_UNCOMPRESSED
  PGP_ZIP
  PGP_ZLIB)

(define-c-enumeration pgp_hash_algorithm
  "enum pgp_hash_algorithm"
  PGP_MD5
  PGP_SHA1
  PGP_RIPEMD
  PGP_MD2
  PGP_TIGER192
  PGP_HAVAL)

(define-c-enumeration pgp_tag
  "enum pgp_tag"
  PGP_TAG_PUBLIC_SESSION_KEY
  PGP_TAG_SIGNATURE
  PGP_TAG_SYMMETRIC_SESSION_KEY
  PGP_TAG_ONE_PASS_SIGNATURE
  PGP_TAG_SECRET_KEY
  PGP_TAG_PUBLIC_KEY
  PGP_TAG_SECRET_SUBKEY
  PGP_TAG_COMPRESSED
  PGP_TAG_ENCRYPTED
  PGP_TAG_MARKER
  PGP_TAG_LITERAL
  PGP_TAG_TRUST
  PGP_TAG_USERID
  PGP_TAG_PUBLIC_SUBKEY)

(define-c-enumeration pgp_signature_type
  "enum pgp_signature_type"
  PGP_SIGN_BINARY
  PGP_SIGN_TEXT
  PGP_SIGN_STANDALONE
  PGP_SIGN_CERTIFICATION
  PGP_SIGN_CERTIFICATION_PERSONA
  PGP_SIGN_CERTIFICATION_CASUAL
  PGP_SIGN_CERTIFICATION_POSITIVE
  PGP_SIGN_SUBKEY
  PGP_SIGN_KEY
  PGP_SIGN_REVOCATION
  PGP_SIGN_REVOCATION_SUBKEY
  PGP_SIGN_REVOCATION_CERTIFICATE
  PGP_SIGN_TIMESTAMP)

(define-c-enumeration pgp_subpacket_tag
  "enum pgp_subpacket_tag"
  PGP_SUBPACKET_CREATION_TIME
  PGP_SUBPACKET_SIGNATURE_EXPIRATION_TIME
  PGP_SUBPACKET_EXPORTABLE_CERTIFICATION
  PGP_SUBPACKET_TRUST_SIGNATURE
  PGP_SUBPACKET_REGULAR_EXPRESSION
  PGP_SUBPACKET_REVOCABLE
  PGP_SUBPACKET_KEY_EXPIRATION_TIME
  PGP_SUBPACKET_PLACEHOLDER
  PGP_SUBPACKET_PREFERRED_SYMMETRIC_ALGORITHMS
  PGP_SUBPACKET_REVOCATION_KEY
  PGP_SUBPACKET_ISSUER_KEY_ID
  PGP_SUBPACKET_NOTATION_DATA
  PGP_SUBPACKET_PREFERRED_HASH_ALGORITHMS
  PGP_SUBPACKET_PREFERRED_COMPRESSION_ALGORITHMS
  PGP_SUBPACKET_KEY_SERVER_PREFERENCES
  PGP_SUBPACKET_PREFERRED_KEY_SERVER
  PGP_SUBPACKET_PRIMARY_USER_ID
  PGP_SUBPACKET_POLICY_URL
  PGP_SUBPACKET_KEY_FLAGS
  PGP_SUBPACKET_SIGNERS_USER_ID
  PGP_SUBPACKET_REASON_FOR_REVOCATION)


;;;; "pkcs1.h"

;; no definitions, only functions


;;;; "realloc.h"

;; no definitions, only functions

(define-c-type-alias nettle_realloc_func	callback)


;;;; "rsa.h"

(define-c-defines "RSA pubkey cipher"
  RSA_MINIMUM_N_OCTETS
  RSA_MINIMUM_N_BITS)

(define-c-struct rsa_public_key
  "struct rsa_public_key"
  (unsigned-int		size)
  (embedded		n)
  (embedded		e))

(define-c-struct rsa_private_key
  "struct rsa_private_key"
  (unsigned-int		size)
  (embedded		d)
  (embedded		p)
  (embedded		q)
  (embedded		a)
  (embedded		b)
  (embedded		c))


;;;; "serpent.h"

(define-c-defines "serpent symmetric cipher"
  SERPENT_BLOCK_SIZE
  SERPENT_KEY_SIZE
  SERPENT_MIN_KEY_SIZE
  SERPENT_MAX_KEY_SIZE)

(define-c-struct serpent_ctx
  "struct serpent_ctx"
  (embedded		keys))


;;;; "sexp.h"

(define-c-enumeration sexp_type
  "enum sexp_type"
  SEXP_ATOM
  SEXP_LIST
  SEXP_END)

(define-c-struct sexp_iterator
  "struct sexp_iterator"
  (unsigned-int		length)
  (pointer		buffer)
  (unsigned-int		start)
  (unsigned-int		pos)
  (unsigned-int		level)
  (signed-int		type)
  (unsigned-int		display_length)
  (pointer		display)
  (unsigned-int		atom_length)
  (pointer		atom))


;;;; "sha.h"

(define-c-defines "SHA1 message digest"
  SHA1_DIGEST_SIZE
  SHA1_DATA_SIZE
  _SHA1_DIGEST_LENGTH)

(define-c-struct sha1_ctx
  "struct sha1_ctx"
  (embedded		digest)
  (unsigned-int		count_low)
  (unsigned-int		count_high)
  (embedded		block)
  (unsigned-int		index))

(define-c-defines "SHA256 message digest"
  SHA256_DIGEST_SIZE
  SHA256_DATA_SIZE
  _SHA256_DIGEST_LENGTH)

(define-c-struct sha256_ctx
  "struct sha256_ctx"
  (embedded		state)
  (unsigned-int		count_low)
  (unsigned-int		count_high)
  (embedded		block)
  (unsigned-int		index))


;;;; "twofish.h"

(define-c-defines "twofish symmetric cipher"
  TWOFISH_BLOCK_SIZE
  TWOFISH_MIN_KEY_SIZE
  TWOFISH_MAX_KEY_SIZE
  TWOFISH_KEY_SIZE)

(define-c-struct twofish_ctx
  "struct twofish_ctx"
  (embedded		keys)
  (embedded		s_box))


;;;; "yarrow.h"

(define-c-enumeration yarrow_pool_id
  "enum yarrow_pool_id"
  YARROW_FAST
  YARROW_SLOW
  YARROW256_SEED_FILE_SIZE
  YARROW_KEY_EVENT_BUFFER)

(define-c-struct yarrow_source
  "struct yarrow_source"
  (embedded		estimate)
  (signed-int		next))

(define-c-struct yarrow256_ctx
  "struct yarrow256_ctx"
  (embedded		pools)
  (signed-int		seeded)
  (embedded		key)
  (embedded		counter)
  (unsigned-int		nsources)
  (pointer		sources))

(define-c-struct yarrow_key_event_ctx
  "struct yarrow_key_event_ctx"
  (unsigned-int		index)
  (embedded		chars)
  (unsigned-int		previous))


;;;; done

(define nettle-library-spec
  '(foreign crypto nettle sizeof))

(define-shared-object nettle libnettle.so)
(define-shared-object hogweed libhogweed.so)

(autoconf-lib-write "configuration/nettle-inspector.m4" nettle-library-spec)
(sizeof-lib-write   "src/libraries/foreign/crypto/nettle/sizeof.sls.in" nettle-library-spec)

;;; end of file
