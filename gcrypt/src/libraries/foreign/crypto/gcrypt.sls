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

    (rename (primitive:gcry-check-version		gcry-check-version)
	    (primitive:gcry-control/int			gcry-control/int)
	    (primitive:gcry-control/uint		gcry-control/uint)
	    (primitive:gcry-control/ptr			gcry-control/ptr)

	    (primitive:gcry-strerror			gcry-strerror)
	    (primitive:gcry-strsource			gcry-strsource)
	    (primitive:gcry-err-code-from-errno		gcry-err-code-from-errno)
	    (primitive:gcry-err-code-to-errno		gcry-err-code-to-errno)
	    (primitive:gcry-err-make-from-errno		gcry-err-make-from-errno)
	    (primitive:gcry-error-from-errno		gcry-error-from-errno)

	    (primitive:gcry-sexp-new			gcry-sexp-new)
	    (primitive:gcry-sexp-create			gcry-sexp-create)
	    (primitive:gcry-sexp-sscan			gcry-sexp-sscan)
;;;  (primitive:gcry-sexp-build		gcry-sexp-build)
	    (primitive:gcry-sexp-build-array		gcry-sexp-build-array)
	    (primitive:gcry-sexp-release		gcry-sexp-release)
	    (primitive:gcry-sexp-canon-len		gcry-sexp-canon-len)
	    (primitive:gcry-sexp-sprint			gcry-sexp-sprint)
	    (primitive:gcry-sexp-dump			gcry-sexp-dump)
	    (primitive:gcry-sexp-cons			gcry-sexp-cons)
	    (primitive:gcry-sexp-alist			gcry-sexp-alist)
;;;  (primitive:gcry-sexp-vlist		gcry-sexp-vlist)
	    (primitive:gcry-sexp-append			gcry-sexp-append)
	    (primitive:gcry-sexp-prepend		gcry-sexp-prepend)
	    (primitive:gcry-sexp-find-token		gcry-sexp-find-token)
	    (primitive:gcry-sexp-length			gcry-sexp-length)
	    (primitive:gcry-sexp-nth			gcry-sexp-nth)
	    (primitive:gcry-sexp-car			gcry-sexp-car)
	    (primitive:gcry-sexp-cdr			gcry-sexp-cdr)
	    (primitive:gcry-sexp-cadr			gcry-sexp-cadr)
	    (primitive:gcry-sexp-nth-data		gcry-sexp-nth-data)
	    (primitive:gcry-sexp-nth-string		gcry-sexp-nth-string)
	    (primitive:gcry-sexp-nth-mpi		gcry-sexp-nth-mpi)

	    (primitive:gcry-mpi-new			gcry-mpi-new)
	    (primitive:gcry-mpi-snew			gcry-mpi-snew)
	    (primitive:gcry-mpi-release			gcry-mpi-release)
	    (primitive:gcry-mpi-copy			gcry-mpi-copy)
	    (primitive:gcry-mpi-set			gcry-mpi-set)
	    (primitive:gcry-mpi-set-ui			gcry-mpi-set-ui)
	    (primitive:gcry-mpi-swap			gcry-mpi-swap)
	    (primitive:gcry-mpi-cmp			gcry-mpi-cmp)
	    (primitive:gcry-mpi-cmp-ui			gcry-mpi-cmp-ui)
	    (primitive:gcry-mpi-scan			gcry-mpi-scan)
	    (primitive:gcry-mpi-print			gcry-mpi-print)
	    (primitive:gcry-mpi-aprint			gcry-mpi-aprint)
	    (primitive:gcry-mpi-dump			gcry-mpi-dump)
	    (primitive:gcry-mpi-add			gcry-mpi-add)
	    (primitive:gcry-mpi-add-ui			gcry-mpi-add-ui)
	    (primitive:gcry-mpi-addm			gcry-mpi-addm)
	    (primitive:gcry-mpi-sub			gcry-mpi-sub)
	    (primitive:gcry-mpi-sub-ui			gcry-mpi-sub-ui)
	    (primitive:gcry-mpi-subm			gcry-mpi-subm)
	    (primitive:gcry-mpi-mul			gcry-mpi-mul)
	    (primitive:gcry-mpi-mul-ui			gcry-mpi-mul-ui)
	    (primitive:gcry-mpi-mulm			gcry-mpi-mulm)
	    (primitive:gcry-mpi-mul-2exp		gcry-mpi-mul-2exp)
	    (primitive:gcry-mpi-div			gcry-mpi-div)
	    (primitive:gcry-mpi-mod			gcry-mpi-mod)
	    (primitive:gcry-mpi-powm			gcry-mpi-powm)
	    (primitive:gcry-mpi-gcd			gcry-mpi-gcd)
	    (primitive:gcry-mpi-invm			gcry-mpi-invm)
	    (primitive:gcry-mpi-get-nbits		gcry-mpi-get-nbits)
	    (primitive:gcry-mpi-test-bit		gcry-mpi-test-bit)
	    (primitive:gcry-mpi-set-bit			gcry-mpi-set-bit)
	    (primitive:gcry-mpi-clear-bit		gcry-mpi-clear-bit)
	    (primitive:gcry-mpi-set-highbit		gcry-mpi-set-highbit)
	    (primitive:gcry-mpi-clear-highbit		gcry-mpi-clear-highbit)
	    (primitive:gcry-mpi-rshift			gcry-mpi-rshift)
	    (primitive:gcry-mpi-lshift			gcry-mpi-lshift)
	    (primitive:gcry-mpi-set-opaque		gcry-mpi-set-opaque)
	    (primitive:gcry-mpi-get-opaque		gcry-mpi-get-opaque)
	    (primitive:gcry-mpi-set-flag		gcry-mpi-set-flag)
	    (primitive:gcry-mpi-clear-flag		gcry-mpi-clear-flag)
	    (primitive:gcry-mpi-get-flag		gcry-mpi-get-flag)

	    (primitive:gcry-cipher-open			gcry-cipher-open)
	    (primitive:gcry-cipher-close		gcry-cipher-close)
	    (primitive:gcry-cipher-ctl			gcry-cipher-ctl)
	    (primitive:gcry-cipher-info			gcry-cipher-info)
	    (primitive:gcry-cipher-algo-info		gcry-cipher-algo-info)
	    (primitive:gcry-cipher-algo-name		gcry-cipher-algo-name)
	    (primitive:gcry-cipher-map-name		gcry-cipher-map-name)
	    (primitive:gcry-cipher-mode-from-oid	gcry-cipher-mode-from-oid)
	    (primitive:gcry-cipher-encrypt		gcry-cipher-encrypt)
	    (primitive:gcry-cipher-decrypt		gcry-cipher-decrypt)
	    (primitive:gcry-cipher-setkey		gcry-cipher-setkey)
	    (primitive:gcry-cipher-setiv		gcry-cipher-setiv)
	    (primitive:gcry-cipher-setctr		gcry-cipher-setctr)
	    (primitive:gcry-cipher-get-algo-keylen	gcry-cipher-get-algo-keylen)
	    (primitive:gcry-cipher-get-algo-blklen	gcry-cipher-get-algo-blklen)
	    (primitive:gcry-cipher-list			gcry-cipher-list)

	    (primitive:gcry-cipher-reset		gcry-cipher-reset)
	    (primitive:gcry-cipher-sync			gcry-cipher-sync)
	    (primitive:gcry-cipher-cts			gcry-cipher-cts)
	    (primitive:gcry-cipher-test-algo		gcry-cipher-test-algo)

	    (primitive:gcry-pk-encrypt			gcry-pk-encrypt)
	    (primitive:gcry-pk-decrypt			gcry-pk-decrypt)
	    (primitive:gcry-pk-sign			gcry-pk-sign)
	    (primitive:gcry-pk-verify			gcry-pk-verify)
	    (primitive:gcry-pk-testkey			gcry-pk-testkey)
	    (primitive:gcry-pk-genkey			gcry-pk-genkey)
	    (primitive:gcry-pk-ctl			gcry-pk-ctl)
	    (primitive:gcry-pk-algo-info		gcry-pk-algo-info)
	    (primitive:gcry-pk-algo-name		gcry-pk-algo-name)
	    (primitive:gcry-pk-map-name			gcry-pk-map-name)
	    (primitive:gcry-pk-get-nbits		gcry-pk-get-nbits)
	    (primitive:gcry-pk-get-keygrip		gcry-pk-get-keygrip)
	    (primitive:gcry-pk-list			gcry-pk-list)

	    (primitive:gcry-pk-test-algo		gcry-pk-test-algo)

	    (primitive:gcry-md-open			gcry-md-open)
	    (primitive:gcry-md-close			gcry-md-close)
	    (primitive:gcry-md-enable			gcry-md-enable)
	    (primitive:gcry-md-copy			gcry-md-copy)
	    (primitive:gcry-md-reset			gcry-md-reset)
	    (primitive:gcry-md-ctl			gcry-md-ctl)
	    (primitive:gcry-md-write			gcry-md-write)
	    (primitive:gcry-md-read			gcry-md-read)
	    (primitive:gcry-md-hash-buffer		gcry-md-hash-buffer)
	    (primitive:gcry-md-get-algo			gcry-md-get-algo)
	    (primitive:gcry-md-get-algo-dlen		gcry-md-get-algo-dlen)
	    (primitive:gcry-md-is-enabled		gcry-md-is-enabled)
	    (primitive:gcry-md-is-secure		gcry-md-is-secure)
	    (primitive:gcry-md-info			gcry-md-info)
	    (primitive:gcry-md-algo-info		gcry-md-algo-info)
	    (primitive:gcry-md-algo-name		gcry-md-algo-name)
	    (primitive:gcry-md-map-name			gcry-md-map-name)
	    (primitive:gcry-md-setkey			gcry-md-setkey)
	    (primitive:gcry-md-debug			gcry-md-debug)
	    (primitive:gcry-md-list			gcry-md-list)

	    (primitive:gcry-md-final			gcry-md-final)
	    (primitive:gcry-md-test-algo		gcry-md-test-algo)
	    (primitive:gcry-md-get-asnoid		gcry-md-get-asnoid)

	    (primitive:gcry-randomize			gcry-randomize)
	    (primitive:gcry-random-add-bytes		gcry-random-add-bytes)
	    (primitive:gcry-random-bytes		gcry-random-bytes)
	    (primitive:gcry-random-bytes-secure		gcry-random-bytes-secure)
	    (primitive:gcry-mpi-randomize		gcry-mpi-randomize)
	    (primitive:gcry-create-nonce		gcry-create-nonce)

	    (primitive:gcry-fast-random-poll		gcry-fast-random-poll)

	    (primitive:gcry-prime-generate		gcry-prime-generate)
	    (primitive:gcry-prime-group-generator	gcry-prime-group-generator)
	    (primitive:gcry-prime-release-factors	gcry-prime-release-factors)
	    (primitive:gcry-prime-check			gcry-prime-check)
	    (primitive:gcry-set-progress-handler	gcry-set-progress-handler)
	    (primitive:gcry-set-allocation-handler	gcry-set-allocation-handler)
	    (primitive:gcry-set-outofcore-handler	gcry-set-outofcore-handler)
	    (primitive:gcry-set-fatalerror-handler	gcry-set-fatalerror-handler)
	    (primitive:gcry-set-log-handler		gcry-set-log-handler)
	    (primitive:gcry-set-gettext-handler		gcry-set-gettext-handler)
	    (primitive:gcry-malloc			gcry-malloc)
	    (primitive:gcry-calloc			gcry-calloc)
	    (primitive:gcry-malloc-secure		gcry-malloc-secure)
	    (primitive:gcry-calloc-secure		gcry-calloc-secure)
	    (primitive:gcry-realloc			gcry-realloc)
	    (primitive:gcry-strdup			gcry-strdup)
	    (primitive:gcry-xmalloc			gcry-xmalloc)
	    (primitive:gcry-xcalloc			gcry-xcalloc)
	    (primitive:gcry-xmalloc-secure		gcry-xmalloc-secure)
	    (primitive:gcry-xcalloc-secure		gcry-xcalloc-secure)
	    (primitive:gcry-xrealloc			gcry-xrealloc)
	    (primitive:gcry-xstrdup			gcry-xstrdup)
	    (primitive:gcry-free			gcry-free)
	    (primitive:gcry-is-secure			gcry-is-secure)

	    (primitive:gcry-fips-mode-active		gcry-fips-mode-active)))
  (import (foreign crypto gcrypt sizeof)
    (prefix (foreign crypto gcrypt primitives) primitive:)))

;;; end of file
