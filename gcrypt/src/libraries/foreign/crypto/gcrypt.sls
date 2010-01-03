;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Gcrypt
;;;Contents: compound library, high-level API
;;;Date: Sat Dec 26, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (foreign crypto gcrypt)
  (export

    GPG_ERR_SOURCE_GCRYPT
    GCRY_THREAD_OPTION_VERSION
    GCRYPT_VERSION
    GCRY_THREAD_OPTION_DEFAULT
    GCRY_THREAD_OPTION_USER
    GCRY_THREAD_OPTION_PTH
    GCRY_THREAD_OPTION_PTHREAD
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
    GCRYCTL_SELFTEST
    GCRYSEXP_FMT_DEFAULT
    GCRYSEXP_FMT_CANON
    GCRYSEXP_FMT_BASE64
    GCRYSEXP_FMT_ADVANCED
    GCRYMPI_FMT_NONE
    GCRYMPI_FMT_STD
    GCRYMPI_FMT_PGP
    GCRYMPI_FMT_SSH
    GCRYMPI_FMT_HEX
    GCRYMPI_FMT_USG
    GCRYMPI_FLAG_SECURE
    GCRYMPI_FLAG_OPAQUE
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
    GCRY_CIPHER_CAMELLIA256
    GCRY_CIPHER_AES128
    GCRY_CIPHER_RIJNDAEL
    GCRY_CIPHER_RIJNDAEL128
    GCRY_CIPHER_RIJNDAEL192
    GCRY_CIPHER_RIJNDAEL256
    GCRY_CIPHER_MODE_NONE
    GCRY_CIPHER_MODE_ECB
    GCRY_CIPHER_MODE_CFB
    GCRY_CIPHER_MODE_CBC
    GCRY_CIPHER_MODE_STREAM
    GCRY_CIPHER_MODE_OFB
    GCRY_CIPHER_MODE_CTR
    GCRY_CIPHER_SECURE
    GCRY_CIPHER_ENABLE_SYNC
    GCRY_CIPHER_CBC_CTS
    GCRY_CIPHER_CBC_MAC
    GCRY_PK_RSA
    GCRY_PK_RSA_E
    GCRY_PK_RSA_S
    GCRY_PK_ELG_E
    GCRY_PK_DSA
    GCRY_PK_ELG
    GCRY_PK_ECDSA
    GCRY_PK_USAGE_SIGN
    GCRY_PK_USAGE_ENCR
    GCRY_PK_USAGE_CERT
    GCRY_PK_USAGE_AUTH
    GCRY_PK_USAGE_UNKN
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
    GCRY_MD_WHIRLPOOL
    GCRY_MD_FLAG_SECURE
    GCRY_MD_FLAG_HMAC
    GCRY_WEAK_RANDOM
    GCRY_STRONG_RANDOM
    GCRY_VERY_STRONG_RANDOM
    GCRY_PRIME_CHECK_AT_FINISH
    GCRY_PRIME_CHECK_AT_GOT_PRIME
    GCRY_PRIME_CHECK_AT_MAYBE_PRIME
    GCRY_PRIME_FLAG_SECRET
    GCRY_PRIME_FLAG_SPECIAL_FACTOR
    GCRY_LOG_CONT
    GCRY_LOG_INFO
    GCRY_LOG_WARN
    GCRY_LOG_ERROR
    GCRY_LOG_FATAL
    GCRY_LOG_BUG
    GCRY_LOG_DEBUG
    GCRYPT_SHARED_OBJECT


;;;; functions

;; type definitions
gcry-symmetric-handle		gcry-symmetric-handle?
pointer->gcry-symmetric-handle	gcry-symmetric-handle->pointer

gcry-md-handle			gcry-md-handle?
pointer->gcry-md-handle		gcry-md-handle->pointer

;; enumerations
gcry-cipher-algo	gcry-cipher-algo->value		value->gcry-cipher-algo
gcry-cipher-mode	gcry-cipher-mode->value		value->gcry-cipher-mode
gcry-cipher-flags	gcry-cipher-flags->value	value->gcry-cipher-flags
gcry-md-algo		gcry-md-algo->value		value->gcry-md-algo
gcry-md-flags		gcry-md-flags->value		value->gcry-md-flags
gcry-random-quality	gcry-random-quality->value	value->gcry-random-quality
gcry-mpi-format		gcry-mpi-format->value		value->gcry-mpi-format
gcry-sexp-format	gcry-sexp-format->value		value->gcry-sexp-format

;; control functions
gcry-check-version		gcry-control/int
gcry-control/uint		gcry-control/ptr

;; errors
gcry-strerror			gcry-strsource
gcry-err-code-from-errno	gcry-err-code-to-errno
gcry-err-make-from-errno	gcry-error-from-errno

;; symmetric cryptography
gcry-cipher-open		gcry-cipher-close
gcry-cipher-ctl			gcry-cipher-info
gcry-cipher-algo-info		gcry-cipher-algo-name
gcry-cipher-map-name		gcry-cipher-mode-from-oid
gcry-cipher-encrypt		gcry-cipher-encrypt*
gcry-cipher-decrypt		gcry-cipher-decrypt*
gcry-cipher-setkey		gcry-cipher-setiv
gcry-cipher-setctr
gcry-cipher-get-algo-keylen	gcry-cipher-get-algo-blklen
gcry-cipher-list		gcry-cipher-reset
gcry-cipher-sync		gcry-cipher-cts
gcry-cipher-test-algo

;;; S-expressions
gcry-sexp-new			gcry-sexp-create
gcry-sexp-sscan			;;;gcry-sexp-build
gcry-sexp-build-array		gcry-sexp-release
gcry-sexp-canon-len		gcry-sexp-sprint
gcry-sexp-dump			gcry-sexp-cons
gcry-sexp-alist			;;;gcry-sexp-vlist
gcry-sexp-append		gcry-sexp-prepend
gcry-sexp-find-token		gcry-sexp-length
gcry-sexp-nth			gcry-sexp-car
gcry-sexp-cdr			gcry-sexp-cadr
gcry-sexp-nth-data		gcry-sexp-nth-string
gcry-sexp-nth-mpi

gcry-sexp->list			list->gcry-sexp
gcry-sexp->string		string->gcry-sexp
gcry-sexp-find-token/str

;;; multi-precision integers
gcry-mpi-new			gcry-mpi-snew
gcry-mpi-release		gcry-mpi-copy
gcry-mpi-set			gcry-mpi-set-ui
gcry-mpi-swap			gcry-mpi-cmp
gcry-mpi-cmp-ui			gcry-mpi-scan
gcry-mpi-print			gcry-mpi-aprint
gcry-mpi-dump			gcry-mpi-add
gcry-mpi-add-ui			gcry-mpi-addm
gcry-mpi-sub			gcry-mpi-sub-ui
gcry-mpi-subm			gcry-mpi-mul
gcry-mpi-mul-ui			gcry-mpi-mulm
gcry-mpi-mul-2exp		gcry-mpi-div
gcry-mpi-mod			gcry-mpi-powm
gcry-mpi-gcd			gcry-mpi-invm
gcry-mpi-get-nbits		gcry-mpi-test-bit
gcry-mpi-set-bit		gcry-mpi-clear-bit
gcry-mpi-set-highbit		gcry-mpi-clear-highbit
gcry-mpi-rshift			gcry-mpi-lshift
gcry-mpi-set-opaque		gcry-mpi-get-opaque
gcry-mpi-set-flag		gcry-mpi-clear-flag
gcry-mpi-get-flag

gcry-mpi->uint

gcry-mpi=?
gcry-mpi<?			gcry-mpi<=?
gcry-mpi>?			gcry-mpi>=?

;;; public key cryptography
gcry-pk-encrypt			gcry-pk-decrypt
gcry-pk-sign			gcry-pk-verify
gcry-pk-testkey			gcry-pk-genkey
gcry-pk-ctl			gcry-pk-algo-info
gcry-pk-algo-name		gcry-pk-map-name
gcry-pk-get-nbits		gcry-pk-get-keygrip
gcry-pk-list			gcry-pk-test-algo

;;; message digest
gcry-md-open			gcry-md-close
gcry-md-enable			gcry-md-copy
gcry-md-reset			gcry-md-ctl
gcry-md-write			gcry-md-write*
gcry-md-read			gcry-md-hash-buffer
gcry-md-hash-buffer*		gcry-md-get-algo
gcry-md-get-algo-dlen		gcry-md-is-enabled?
gcry-md-is-enabled		gcry-md-is-secure?
gcry-md-is-secure		gcry-md-info
gcry-md-algo-info		gcry-md-algo-name
gcry-md-map-name		gcry-md-setkey
gcry-md-debug			gcry-md-list
gcry-md-enabled-algos		gcry-md-final
gcry-md-test-algo		gcry-md-get-asnoid

;;; pseudo-random numbers
gcry-randomize			gcry-random-add-bytes
gcry-random-bytes
gcry-random-bytes-secure	gcry-random-bytes/secure
gcry-mpi-randomize		gcry-create-nonce
gcry-fast-random-poll

;;; prime numbers
gcry-prime-generate		gcry-prime-group-generator
gcry-prime-release-factors	gcry-prime-check

;;; miscellaneous functions
gcry-set-progress-handler	gcry-set-allocation-handler
gcry-set-outofcore-handler	gcry-set-fatalerror-handler
gcry-set-log-handler		gcry-set-gettext-handler
gcry-fips-mode-active

;;; memory allocation
gcry-malloc			gcry-calloc
gcry-malloc-secure		gcry-calloc-secure
gcry-realloc			gcry-strdup
gcry-xmalloc			gcry-xcalloc
gcry-xmalloc-secure		gcry-xcalloc-secure
gcry-xrealloc			gcry-xstrdup
gcry-free			gcry-is-secure

)


(import (foreign crypto gcrypt sizeof)
  (foreign crypto gcrypt typedefs)
  (foreign crypto gcrypt enumerations)
  (foreign crypto gcrypt primitives)))

;;; end of file
