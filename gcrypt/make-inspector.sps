;;;!mosh
;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Gcrypt
;;;Contents: foreign library inspection generator
;;;Date: Sat Dec 26, 2009
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


(import (nausicaa)
  (foreign ffi inspector-maker))


;;;; type definitions

(define-c-type gpg_error_t		signed-int)
(define-c-type gpg_err_code_t		signed-int)
(define-c-type gpg_err_source_t		signed-int)

(define-c-type-alias gcry_error_t	signed-int)
(define-c-type-alias gcry_err_code_t	signed-int)
(define-c-type-alias gcry_err_source_t	signed-int)

(define-c-type-alias gcry_error_t*	pointer)

(define-c-type-alias gcry_mpi_t		pointer)
(define-c-type-alias gcry_mpi_t*	pointer)
(define-c-type-alias gcry_mpi_t**	pointer)

;;Forward declaration.
;;
;; (define-c-struct gcry_mpi
;;   "struct gcry_mpi")

(define-c-type-alias gcry_sexp_t	pointer)
(define-c-type-alias gcry_sexp_t*	pointer)

;;Forward declaration.
;;
;; (define-c-struct gcry_sexp
;;   "struct gcry_sexp")

(define-c-type-alias gcry_cipher_hd_t	pointer)
(define-c-type-alias gcry_cipher_hd_t*	pointer)

;;Forward declaration.
;;
;; (define-c-struct gcry_cipher_handle
;;   "struct gcry_cipher_handle")


;; Forward declaration.
;;
;; (define-c-struct gcry_md_context
;;   "struct gcry_md_context")

(define-c-type-alias gcry_md_hd_t	pointer)
(define-c-type-alias gcry_md_hd_t*	pointer)

(define-c-struct gcry_md_handle
  "struct gcry_md_handle"
  (pointer		ctx)
  (signed-int		bufpos)
  (signed-int		bufsize)
  (embedded		buf))

(define-c-type-alias gcry_prime_check_func_t		callback)

(define-c-type-alias gcry_handler_progress_t		callback)
(define-c-type-alias gcry_handler_alloc_t		callback)
(define-c-type-alias gcry_handler_secure_check_t	callback)
(define-c-type-alias gcry_handler_realloc_t		callback)
(define-c-type-alias gcry_handler_free_t		callback)
(define-c-type-alias gcry_handler_no_mem_t		callback)
(define-c-type-alias gcry_handler_error_t		callback)
(define-c-type-alias gcry_handler_log_t			callback)


;;;; constants

(define-c-defines "global constants"
  GPG_ERR_SOURCE_GCRYPT
  GCRY_THREAD_OPTION_VERSION
  )

(define-c-string-defines "global string constants"
  GCRYPT_VERSION)

(define-c-defines "the thread model to use"
  GCRY_THREAD_OPTION_DEFAULT
  GCRY_THREAD_OPTION_USER
  GCRY_THREAD_OPTION_PTH
  GCRY_THREAD_OPTION_PTHREAD)

(define-c-enumeration gcry_ctl_cmds
  "enum gcry_ctl_cmds"
  GCRYCTL_SET_KEY
  GCRYCTL_SET_IV
  GCRYCTL_CFB_SYNC
  GCRYCTL_RESET
  GCRYCTL_FINALIZE
  GCRYCTL_GET_KEYLEN
  GCRYCTL_GET_BLKLEN
  GCRYCTL_TEST_ALGO
  GCRYCTL_IS_SECURE
  GCRYCTL_GET_ASNOID
  GCRYCTL_ENABLE_ALGO
  GCRYCTL_DISABLE_ALGO
  GCRYCTL_DUMP_RANDOM_STATS
  GCRYCTL_DUMP_SECMEM_STATS
  GCRYCTL_GET_ALGO_NPKEY
  GCRYCTL_GET_ALGO_NSKEY
  GCRYCTL_GET_ALGO_NSIGN
  GCRYCTL_GET_ALGO_NENCR
  GCRYCTL_SET_VERBOSITY
  GCRYCTL_SET_DEBUG_FLAGS
  GCRYCTL_CLEAR_DEBUG_FLAGS
  GCRYCTL_USE_SECURE_RNDPOOL
  GCRYCTL_DUMP_MEMORY_STATS
  GCRYCTL_INIT_SECMEM
  GCRYCTL_TERM_SECMEM
  GCRYCTL_DISABLE_SECMEM_WARN
  GCRYCTL_SUSPEND_SECMEM_WARN
  GCRYCTL_RESUME_SECMEM_WARN
  GCRYCTL_DROP_PRIVS
  GCRYCTL_ENABLE_M_GUARD
  GCRYCTL_START_DUMP
  GCRYCTL_STOP_DUMP
  GCRYCTL_GET_ALGO_USAGE
  GCRYCTL_IS_ALGO_ENABLED
  GCRYCTL_DISABLE_INTERNAL_LOCKING
  GCRYCTL_DISABLE_SECMEM
  GCRYCTL_INITIALIZATION_FINISHED
  GCRYCTL_INITIALIZATION_FINISHED_P
  GCRYCTL_ANY_INITIALIZATION_P
  GCRYCTL_SET_CBC_CTS
  GCRYCTL_SET_CBC_MAC
  GCRYCTL_SET_CTR
  GCRYCTL_ENABLE_QUICK_RANDOM
  GCRYCTL_SET_RANDOM_SEED_FILE
  GCRYCTL_UPDATE_RANDOM_SEED_FILE
  GCRYCTL_SET_THREAD_CBS
  GCRYCTL_FAST_POLL
  GCRYCTL_SET_RANDOM_DAEMON_SOCKET
  GCRYCTL_USE_RANDOM_DAEMON
  GCRYCTL_FAKED_RANDOM_P
  GCRYCTL_SET_RNDEGD_SOCKET
  GCRYCTL_PRINT_CONFIG
  GCRYCTL_OPERATIONAL_P
  GCRYCTL_FIPS_MODE_P
  GCRYCTL_FORCE_FIPS_MODE
  GCRYCTL_SELFTEST)

(define-c-enumeration gcry_sexp_format
  "enum gcry_sexp_format"
  GCRYSEXP_FMT_DEFAULT
  GCRYSEXP_FMT_CANON
  GCRYSEXP_FMT_BASE64
  GCRYSEXP_FMT_ADVANCED)

(define-c-enumeration gcry_mpi_format
  "enum gcry_mpi_format"
  GCRYMPI_FMT_NONE
  GCRYMPI_FMT_STD
  GCRYMPI_FMT_PGP
  GCRYMPI_FMT_SSH
  GCRYMPI_FMT_HEX
  GCRYMPI_FMT_USG)

(define-c-enumeration gcry_mpi_flag
  "enum gcry_mpi_flag"
  GCRYMPI_FLAG_SECURE
  GCRYMPI_FLAG_OPAQUE)

(define-c-enumeration gcry_cipher_algos
  "enum gcry_cipher_algos"
  GCRY_CIPHER_NONE
  GCRY_CIPHER_IDEA
  GCRY_CIPHER_3DES
  GCRY_CIPHER_CAST5
  GCRY_CIPHER_BLOWFISH
  GCRY_CIPHER_SAFER_SK128
  GCRY_CIPHER_DES_SK
  GCRY_CIPHER_AES
  GCRY_CIPHER_AES192
  GCRY_CIPHER_AES256
  GCRY_CIPHER_TWOFISH
  GCRY_CIPHER_ARCFOUR
  GCRY_CIPHER_DES
  GCRY_CIPHER_TWOFISH128
  GCRY_CIPHER_SERPENT128
  GCRY_CIPHER_SERPENT192
  GCRY_CIPHER_SERPENT256
  GCRY_CIPHER_RFC2268_40
  GCRY_CIPHER_RFC2268_128
  GCRY_CIPHER_SEED
  GCRY_CIPHER_CAMELLIA128
  GCRY_CIPHER_CAMELLIA192
  GCRY_CIPHER_CAMELLIA256)

(define-c-defines "the Rijndael algorithm is basically AES, so provide some macros"
  GCRY_CIPHER_AES128
  GCRY_CIPHER_RIJNDAEL
  GCRY_CIPHER_RIJNDAEL128
  GCRY_CIPHER_RIJNDAEL192
  GCRY_CIPHER_RIJNDAEL256)

(define-c-enumeration gcry_cipher_modes
  "enum gcry_cipher_modes"
  GCRY_CIPHER_MODE_NONE
  GCRY_CIPHER_MODE_ECB
  GCRY_CIPHER_MODE_CFB
  GCRY_CIPHER_MODE_CBC
  GCRY_CIPHER_MODE_STREAM
  GCRY_CIPHER_MODE_OFB
  GCRY_CIPHER_MODE_CTR)

(define-c-enumeration gcry_cipher_flags
  "enum gcry_cipher_flags"
  GCRY_CIPHER_SECURE
  GCRY_CIPHER_ENABLE_SYNC
  GCRY_CIPHER_CBC_CTS
  GCRY_CIPHER_CBC_MAC)

(define-c-enumeration gcry_pk_algos
  "enum gcry_pk_algos"
  GCRY_PK_RSA
  GCRY_PK_RSA_E
  GCRY_PK_RSA_S
  GCRY_PK_ELG_E
  GCRY_PK_DSA
  GCRY_PK_ELG
  GCRY_PK_ECDSA)

(define-c-defines "flags describing usage capabilities of a PK algorithm"
  GCRY_PK_USAGE_SIGN
  GCRY_PK_USAGE_ENCR
  GCRY_PK_USAGE_CERT
  GCRY_PK_USAGE_AUTH
  GCRY_PK_USAGE_UNKN)

(define-c-enumeration gcry_md_algos
  "enum gcry_md_algos"
  GCRY_MD_NONE
  GCRY_MD_MD5
  GCRY_MD_SHA1
  GCRY_MD_RMD160
  GCRY_MD_MD2
  GCRY_MD_TIGER
  GCRY_MD_HAVAL
  GCRY_MD_SHA256
  GCRY_MD_SHA384
  GCRY_MD_SHA512
  GCRY_MD_SHA224
  GCRY_MD_MD4
  GCRY_MD_CRC32
  GCRY_MD_CRC32_RFC1510
  GCRY_MD_CRC24_RFC2440
  GCRY_MD_WHIRLPOOL)

(define-c-enumeration gcry_md_flags
  "enum gcry_md_flags"
  GCRY_MD_FLAG_SECURE
  GCRY_MD_FLAG_HMAC)

(define-c-enumeration gcry_random_level_t
  "enum gcry_random_level"
  GCRY_WEAK_RANDOM
  GCRY_STRONG_RANDOM
  GCRY_VERY_STRONG_RANDOM)

(define-c-defines "mode values passed to a gcry_prime_check_func_t"
  GCRY_PRIME_CHECK_AT_FINISH
  GCRY_PRIME_CHECK_AT_GOT_PRIME
  GCRY_PRIME_CHECK_AT_MAYBE_PRIME)

(define-c-defines "prime numbers constants"
  GCRY_PRIME_FLAG_SECRET
  GCRY_PRIME_FLAG_SPECIAL_FACTOR)

(define-c-enumeration gcry_log_levels
  "enum gcry_log_levels"
  GCRY_LOG_CONT
  GCRY_LOG_INFO
  GCRY_LOG_WARN
  GCRY_LOG_ERROR
  GCRY_LOG_FATAL
  GCRY_LOG_BUG
  GCRY_LOG_DEBUG)



;;;; done

(define gcrypt-library-spec
  '(foreign crypto gcrypt sizeof))

(define-shared-object gcrypt libgcrypt.so)

(autoconf-lib-write "configuration/gcrypt-inspector.m4" gcrypt-library-spec)
(sizeof-lib-write   "src/libraries/foreign/crypto/gcrypt/sizeof.sls.in" gcrypt-library-spec)

;;; end of file
