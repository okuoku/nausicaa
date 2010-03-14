;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Gcrypt
;;;Contents: primitive functions
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


(library (foreign crypto gcrypt primitives)
  (export

    gcry-check-version
    gcry-strerror
    gcry-strsource

    ;; symmetric cryptography
    gcry-cipher-open		gcry-cipher-close
    gcry-cipher-setkey		gcry-cipher-reset
    gcry-cipher-setiv		gcry-cipher-setctr
    gcry-cipher-encrypt		gcry-cipher-decrypt
    gcry-cipher-encrypt*	gcry-cipher-decrypt*
    gcry-cipher-sync

    ;; message digest
    gcry-md-open		gcry-md-copy
    gcry-md-final		gcry-md-close
    gcry-md-reset		gcry-md-enable
    gcry-md-setkey
    gcry-md-write		gcry-md-write*
    gcry-md-read
    gcry-md-hash-buffer		gcry-md-hash-buffer*
    gcry-md-algo-name		gcry-md-map-name
    gcry-md-test-algo		gcry-md-get-algo-dlen
    gcry-md-is-enabled		gcry-md-is-secure
    gcry-md-is-enabled?		gcry-md-is-secure?
    gcry-md-get-asnoid		gcry-md-enabled-algos

    ;; pseudo-random numbers
    gcry-randomize
    gcry-random-add-bytes
    gcry-random-bytes
    gcry-random-bytes-secure	gcry-random-bytes/secure
    gcry-create-nonce

    ;; MPI numbers
    gcry-mpi-scan		gcry-mpi-print
    gcry-mpi->uint
    gcry-mpi=?
    gcry-mpi<?			gcry-mpi<=?
    gcry-mpi>?			gcry-mpi>=?

    ;; S-expressions
    gcry-sexp->list		list->gcry-sexp
    string->gcry-sexp		gcry-sexp->string
    gcry-sexp-find-token/str

    ;; public key cryptography
    gcry-pk-genkey
    gcry-pk-encrypt		gcry-pk-decrypt
    gcry-pk-sign		gcry-pk-verify

    (rename (platform:gcry_control/int			gcry-control/int)
	    (platform:gcry_control/uint			gcry-control/uint)
	    (platform:gcry_control/ptr			gcry-control/ptr)

	    (platform:gcry_err_code_from_errno		gcry-err-code-from-errno)
	    (platform:gcry_err_code_to_errno		gcry-err-code-to-errno)
	    (platform:gcry_err_make_from_errno		gcry-err-make-from-errno)
	    (platform:gcry_error_from_errno		gcry-error-from-errno)

	    (platform:gcry_sexp_new			gcry-sexp-new)
	    (platform:gcry_sexp_create			gcry-sexp-create)
	    (platform:gcry_sexp_sscan			gcry-sexp-sscan)
;;;  (platform:gcry_sexp_build		gcry-sexp-build)
	    (platform:gcry_sexp_build_array		gcry-sexp-build-array)
	    (platform:gcry_sexp_release			gcry-sexp-release)
	    (platform:gcry_sexp_canon_len		gcry-sexp-canon-len)
	    (platform:gcry_sexp_sprint			gcry-sexp-sprint)
	    (platform:gcry_sexp_dump			gcry-sexp-dump)
	    (platform:gcry_sexp_cons			gcry-sexp-cons)
	    (platform:gcry_sexp_alist			gcry-sexp-alist)
;;;  (platform:gcry_sexp_vlist		gcry-sexp-vlist)
	    (platform:gcry_sexp_append			gcry-sexp-append)
	    (platform:gcry_sexp_prepend			gcry-sexp-prepend)
	    (platform:gcry_sexp_find_token		gcry-sexp-find-token)
	    (platform:gcry_sexp_length			gcry-sexp-length)
	    (platform:gcry_sexp_nth			gcry-sexp-nth)
	    (platform:gcry_sexp_car			gcry-sexp-car)
	    (platform:gcry_sexp_cdr			gcry-sexp-cdr)
	    (platform:gcry_sexp_cadr			gcry-sexp-cadr)
	    (platform:gcry_sexp_nth_data		gcry-sexp-nth-data)
	    (platform:gcry_sexp_nth_string		gcry-sexp-nth-string)
	    (platform:gcry_sexp_nth_mpi			gcry-sexp-nth-mpi)

	    (platform:gcry_mpi_new			gcry-mpi-new)
	    (platform:gcry_mpi_snew			gcry-mpi-snew)
	    (platform:gcry_mpi_release			gcry-mpi-release)
	    (platform:gcry_mpi_copy			gcry-mpi-copy)
	    (platform:gcry_mpi_set			gcry-mpi-set)
	    (platform:gcry_mpi_set_ui			gcry-mpi-set-ui)
	    (platform:gcry_mpi_swap			gcry-mpi-swap)
	    (platform:gcry_mpi_cmp			gcry-mpi-cmp)
	    (platform:gcry_mpi_cmp_ui			gcry-mpi-cmp-ui)
	    (platform:gcry_mpi_dump			gcry-mpi-dump)
	    (platform:gcry_mpi_add			gcry-mpi-add)
	    (platform:gcry_mpi_add_ui			gcry-mpi-add-ui)
	    (platform:gcry_mpi_addm			gcry-mpi-addm)
	    (platform:gcry_mpi_sub			gcry-mpi-sub)
	    (platform:gcry_mpi_sub_ui			gcry-mpi-sub-ui)
	    (platform:gcry_mpi_subm			gcry-mpi-subm)
	    (platform:gcry_mpi_mul			gcry-mpi-mul)
	    (platform:gcry_mpi_mul_ui			gcry-mpi-mul-ui)
	    (platform:gcry_mpi_mulm			gcry-mpi-mulm)
	    (platform:gcry_mpi_mul_2exp			gcry-mpi-mul-2exp)
	    (platform:gcry_mpi_div			gcry-mpi-div)
	    (platform:gcry_mpi_mod			gcry-mpi-mod)
	    (platform:gcry_mpi_powm			gcry-mpi-powm)
	    (platform:gcry_mpi_gcd			gcry-mpi-gcd)
	    (platform:gcry_mpi_invm			gcry-mpi-invm)
	    (platform:gcry_mpi_get_nbits		gcry-mpi-get-nbits)
	    (platform:gcry_mpi_test_bit			gcry-mpi-test-bit)
	    (platform:gcry_mpi_set_bit			gcry-mpi-set-bit)
	    (platform:gcry_mpi_clear_bit		gcry-mpi-clear-bit)
	    (platform:gcry_mpi_set_highbit		gcry-mpi-set-highbit)
	    (platform:gcry_mpi_clear_highbit		gcry-mpi-clear-highbit)
	    (platform:gcry_mpi_rshift			gcry-mpi-rshift)
	    (platform:gcry_mpi_lshift			gcry-mpi-lshift)
	    (platform:gcry_mpi_set_opaque		gcry-mpi-set-opaque)
	    (platform:gcry_mpi_get_opaque		gcry-mpi-get-opaque)
	    (platform:gcry_mpi_set_flag			gcry-mpi-set-flag)
	    (platform:gcry_mpi_clear_flag		gcry-mpi-clear-flag)
	    (platform:gcry_mpi_get_flag			gcry-mpi-get-flag)
	    (platform:gcry_mpi_randomize		gcry-mpi-randomize)
	    (platform:gcry_mpi_aprint			gcry-mpi-aprint)

	    (platform:gcry_cipher_ctl			gcry-cipher-ctl)
	    (platform:gcry_cipher_info			gcry-cipher-info)
	    (platform:gcry_cipher_algo_info		gcry-cipher-algo-info)
	    (platform:gcry_cipher_algo_name		gcry-cipher-algo-name)
	    (platform:gcry_cipher_map_name		gcry-cipher-map-name)
	    (platform:gcry_cipher_mode_from_oid		gcry-cipher-mode-from-oid)
	    (platform:gcry_cipher_get_algo_keylen	gcry-cipher-get-algo-keylen)
	    (platform:gcry_cipher_get_algo_blklen	gcry-cipher-get-algo-blklen)
	    (platform:gcry_cipher_list			gcry-cipher-list)
	    (platform:gcry_cipher_cts			gcry-cipher-cts)
	    (platform:gcry_cipher_test_algo		gcry-cipher-test-algo)

	    (platform:gcry_pk_testkey			gcry-pk-testkey)
	    (platform:gcry_pk_ctl			gcry-pk-ctl)
	    (platform:gcry_pk_algo_info			gcry-pk-algo-info)
	    (platform:gcry_pk_algo_name			gcry-pk-algo-name)
	    (platform:gcry_pk_map_name			gcry-pk-map-name)
	    (platform:gcry_pk_get_nbits			gcry-pk-get-nbits)
	    (platform:gcry_pk_get_keygrip		gcry-pk-get-keygrip)
	    (platform:gcry_pk_list			gcry-pk-list)

	    (platform:gcry_pk_test_algo			gcry-pk-test-algo)

	    (platform:gcry_md_ctl			gcry-md-ctl)
	    (platform:gcry_md_get_algo			gcry-md-get-algo)
	    (platform:gcry_md_info			gcry-md-info)
	    (platform:gcry_md_algo_info			gcry-md-algo-info)
	    (platform:gcry_md_debug			gcry-md-debug)
	    (platform:gcry_md_list			gcry-md-list)

	    (platform:gcry_fast_random_poll		gcry-fast-random-poll)

	    (platform:gcry_prime_generate		gcry-prime-generate)
	    (platform:gcry_prime_group_generator	gcry-prime-group-generator)
	    (platform:gcry_prime_release_factors	gcry-prime-release-factors)
	    (platform:gcry_prime_check			gcry-prime-check)
	    (platform:gcry_set_progress_handler		gcry-set-progress-handler)
	    (platform:gcry_set_allocation_handler	gcry-set-allocation-handler)
	    (platform:gcry_set_outofcore_handler	gcry-set-outofcore-handler)
	    (platform:gcry_set_fatalerror_handler	gcry-set-fatalerror-handler)
	    (platform:gcry_set_log_handler		gcry-set-log-handler)
	    (platform:gcry_set_gettext_handler		gcry-set-gettext-handler)
	    (platform:gcry_malloc			gcry-malloc)
	    (platform:gcry_calloc			gcry-calloc)
	    (platform:gcry_malloc_secure		gcry-malloc-secure)
	    (platform:gcry_calloc_secure		gcry-calloc-secure)
	    (platform:gcry_realloc			gcry-realloc)
	    (platform:gcry_strdup			gcry-strdup)
	    (platform:gcry_xmalloc			gcry-xmalloc)
	    (platform:gcry_xcalloc			gcry-xcalloc)
	    (platform:gcry_xmalloc_secure		gcry-xmalloc-secure)
	    (platform:gcry_xcalloc_secure		gcry-xcalloc-secure)
	    (platform:gcry_xrealloc			gcry-xrealloc)
	    (platform:gcry_xstrdup			gcry-xstrdup)
	    (platform:gcry_free				gcry-free)
	    (platform:gcry_is_secure			gcry-is-secure)

	    (platform:gcry_fips_mode_active		gcry-fips-mode-active))
    )
  (import (rnrs)
    (only (language-extensions)
	  receive begin0 begin0-let)
    (compensations)
    (foreign ffi)
    (only (foreign ffi sizeof) sizeof-int)
    (foreign memory)
    (foreign cstrings)
    (foreign crypto gpg-error conditions)
    (foreign crypto gcrypt typedefs)
    (foreign crypto gcrypt enumerations)
    (prefix (foreign crypto gcrypt platform) platform:)
    (foreign crypto gcrypt sizeof)
    (only (foreign crypto gpg-error sizeof) GPG_ERR_BAD_SIGNATURE))


;;;; helpers

(define (%object->ptr&len obj who description)
  ;;Take an  object OBJ, convert  it to a  memory buffer and  return two
  ;;values: the pointer and the number of bytes.  Memory is allocated by
  ;;pushing release closures to the current compensation stack.
  ;;
  ;;OBJ can  be a Scheme string,  a Scheme bytevector  or a "<memblock>"
  ;;object.
  ;;
  ;;WHO must be a Scheme symbol  representing the caller: it is used for
  ;;the  "&who"   condition.   DESCRIPTION  must  be   a  Scheme  string
  ;;describing  the  nature  of  OBJ:  it is  used  for  the  "&message"
  ;;condition.
  ;;
  (cond ((string? obj)
	 (let ((obj.ptr (string->cstring/c obj)))
	   (values obj.ptr (strlen obj.ptr))))
	((bytevector? obj)
	 (let ((obj.ptr (bytevector->pointer obj malloc-block/c)))
	   (values obj.ptr (bytevector-length obj))))
	((<memblock>? obj)
	 (values (<memblock>-pointer obj) (<memblock>-size obj)))
	(else
	 (assertion-violation who description obj))))


;;;; initialisation, control, errors

(define (gcry-check-version)
  (with-compensations
    (let ((p (platform:gcry_check_version (string->cstring/c GCRYPT_VERSION))))
      (if (pointer-null? p)
	  #f
	(cstring->string p)))))

(define (gcry-strerror errcode)
  (cstring->string (platform:gcry_strerror errcode)))

(define (gcry-strsource errcode)
  (cstring->string (platform:gcry_strsource errcode)))


;;;; symmetric cryptography

(define gcry-cipher-open
  (case-lambda
   ((algo mode)
    (gcry-cipher-open algo mode (gcry-cipher-flags)))
   ((algo mode flags)
    (with-compensations
      (let* ((hd*	(malloc-small/c))
	     (errcode	(platform:gcry_cipher_open hd*
						   (gcry-cipher-algo->value  algo)
						   (gcry-cipher-mode->value  mode)
						   (gcry-cipher-flags->value flags))))
	(if (= 0 errcode)
	    (pointer->gcry-symmetric-handle (pointer-ref-c-pointer hd* 0))
	  (raise-gpg-error 'gcry-cipher-open errcode algo mode flags)))))))

(define (gcry-cipher-close syhd)
  (platform:gcry_cipher_close (gcry-symmetric-handle->pointer syhd)))

;;; --------------------------------------------------------------------

(define (%gcry-cipher-set-vector who platform-function syhd obj description)
  (with-compensations
    (receive (obj.ptr obj.len)
	(%object->ptr&len obj who (string-append "expected string, bytevector or memblock as "
						 description))
      (let ((errcode (platform-function (gcry-symmetric-handle->pointer syhd) obj.ptr obj.len)))
	(unless (= 0 errcode)
	  (raise-gpg-error who errcode syhd obj))))))

(define (gcry-cipher-setkey syhd key)
  (%gcry-cipher-set-vector 'gcry-cipher-setkey platform:gcry_cipher_setkey
			   syhd key "encryption key"))

(define (gcry-cipher-setiv syhd iv)
  (%gcry-cipher-set-vector 'gcry-cipher-setiv platform:gcry_cipher_setiv
			   syhd iv "initialisation vector"))

(define (gcry-cipher-setctr syhd ctr)
  (%gcry-cipher-set-vector 'gcry-cipher-setctr platform:gcry_cipher_setctr
			   syhd ctr "counter"))

;;; --------------------------------------------------------------------

(define (gcry-cipher-encrypt syhd ou.ptr ou.len in.ptr in.len)
  (let ((errcode (platform:gcry_cipher_encrypt (gcry-symmetric-handle->pointer syhd)
					       ou.ptr ou.len in.ptr in.len)))
    (unless (= 0 errcode)
      (raise-gpg-error 'gcry-cipher-encrypt errcode syhd ou.ptr ou.len in.ptr in.len))))

(define (gcry-cipher-encrypt* syhd obj)
  (with-compensations
    (receive (in.ptr len)
	(%object->ptr&len obj 'gcry-cipher-encrypt* "encryption input")
      (let ((ou.ptr (malloc-block/c len)))
	(gcry-cipher-encrypt syhd ou.ptr len in.ptr len)
	(pointer->bytevector ou.ptr len)))))

;;; --------------------------------------------------------------------

(define (gcry-cipher-decrypt syhd ou.ptr ou.len in.ptr in.len)
  (let ((errcode (platform:gcry_cipher_decrypt (gcry-symmetric-handle->pointer syhd)
					       ou.ptr ou.len in.ptr in.len)))
    (unless (= 0 errcode)
      (raise-gpg-error 'gcry-cipher-decrypt errcode syhd ou.ptr ou.len in.ptr in.len))))

(define (gcry-cipher-decrypt* syhd obj)
  (with-compensations
    (receive (in.ptr len)
	(%object->ptr&len obj 'gcry-cipher-decrypt* "decryption input")
      (let ((ou.ptr (malloc-block/c len)))
	(gcry-cipher-decrypt syhd ou.ptr len in.ptr len)
	(pointer->bytevector ou.ptr len)))))

;;; --------------------------------------------------------------------

(define (gcry-cipher-reset syhd)
  (let ((errcode (platform:gcry_cipher_reset (gcry-symmetric-handle->pointer syhd))))
    (unless (= 0 errcode)
      (raise-gpg-error 'gcry-cipher-reset errcode syhd))))

(define (gcry-cipher-sync syhd)
  (let ((errcode (platform:gcry_cipher_sync (gcry-symmetric-handle->pointer syhd))))
    (unless (= 0 errcode)
      (raise-gpg-error 'gcry-cipher-sync errcode syhd))))

(define (gcry-cipher-ctl syhd command buf.ptr buf.len)
  (let ((errcode (platform:gcry_cipher_sync (gcry-symmetric-handle->pointer syhd))))
    (unless (= 0 errcode)
      (raise-gpg-error 'gcry-cipher-ctl errcode syhd command buf.ptr buf.len))))


;;;; message digest

(define gcry-md-open
  (case-lambda
   ((algo)
    (gcry-md-open algo #f))
   ((algo flags)
    (with-compensations
      (let* ((hd*	(malloc-small/c))
	     (errcode	(platform:gcry_md_open hd*
					       (gcry-md-algo->value algo)
					       (if flags
						   (gcry-md-flags->value flags)
						 0))))
	(if (= 0 errcode)
	    (pointer->gcry-md-handle (pointer-ref-c-pointer hd* 0))
	  (raise-gpg-error 'gcry-md-open errcode algo flags)))))))

(define (gcry-md-copy mdhd)
  (with-compensations
    (let* ((hd*		(malloc-small/c))
	   (errcode	(platform:gcry_md_copy hd* (gcry-md-handle->pointer mdhd))))
      (if (= 0 errcode)
	  (pointer->gcry-md-handle (pointer-ref-c-pointer hd* 0))
	(raise-gpg-error 'gcry-md-copy errcode mdhd)))))

(define (gcry-md-close mdhd)
  (platform:gcry_md_close (gcry-md-handle->pointer mdhd)))

(define (gcry-md-reset mdhd)
  (platform:gcry_md_reset (gcry-md-handle->pointer mdhd)))

(define (gcry-md-final mdhd)
  (platform:gcry_md_final (gcry-md-handle->pointer mdhd)))

;;; --------------------------------------------------------------------

(define (gcry-md-enable mdhd algo)
  (let ((errcode (platform:gcry_md_enable (gcry-md-handle->pointer mdhd)
					  (gcry-md-algo->value algo))))
    (unless (= 0 errcode)
      (raise-gpg-error 'gcry-cipher-encrypt errcode mdhd algo))))

(define (gcry-md-setkey mdhd obj)
  (with-compensations
    (receive (obj.ptr obj.len)
	(%object->ptr&len obj 'gcry-md-setkey "expected string, bytevector or memblock as MAC key")
      (let ((errcode (platform:gcry_md_setkey (gcry-md-handle->pointer mdhd) obj.ptr obj.len)))
	(unless (= 0 errcode)
	  (raise-gpg-error 'gcry-md-setkey errcode mdhd obj))))))

;;; --------------------------------------------------------------------

(define (gcry-md-write mdhd in.ptr in.len)
  (platform:gcry_md_write (gcry-md-handle->pointer mdhd) in.ptr in.len))

(define (gcry-md-write* mdhd obj)
  (with-compensations
    (receive (in.ptr in.len)
	(%object->ptr&len obj 'gcry-md-write* "message digest input")
      (gcry-md-write mdhd in.ptr in.len))))

(define (gcry-md-read mdhd algo)
  (let* ((algo    (gcry-md-algo->value algo))
	 (buf.len (platform:gcry_md_get_algo_dlen algo))
	 (buf.ptr (platform:gcry_md_read (gcry-md-handle->pointer mdhd) algo)))
    (pointer->bytevector buf.ptr buf.len)))

(define (gcry-md-hash-buffer algo in.ptr in.len)
  (with-compensations
    (let* ((algo-int	(gcry-md-algo->value algo))
	   (dig.len	(platform:gcry_md_get_algo_dlen algo-int))
	   (dig.ptr	(malloc-block/c dig.len)))
      (platform:gcry_md_hash_buffer algo-int dig.ptr in.ptr in.len)
      (pointer->bytevector dig.ptr dig.len))))

(define (gcry-md-hash-buffer* algo obj)
  (with-compensations
    (receive (in.ptr in.len)
	(%object->ptr&len obj 'gcry-md-hash-buffer* "message digest input")
      (gcry-md-hash-buffer algo in.ptr in.len))))

;;; --------------------------------------------------------------------

(define (gcry-md-algo-name algo)
  (cstring->string (platform:gcry_md_algo_name (gcry-md-algo->value algo))))

(define (gcry-md-map-name name)
  (with-compensations
    (value->gcry-md-algo (platform:gcry_md_map_name (string->cstring/c name)))))

(define (gcry-md-test-algo algo)
  (= 0 (platform:gcry_md_test_algo (gcry-md-algo->value algo))))

(define (gcry-md-get-algo-dlen algo)
  (platform:gcry_md_get_algo_dlen (gcry-md-algo->value algo)))

(define (gcry-md-is-secure mdhd)
  (not (= 0 (platform:gcry_md_is_secure (gcry-md-handle->pointer mdhd)))))

(define gcry-md-is-secure? gcry-md-is-secure)

(define (gcry-md-is-enabled mdhd algo)
  (not (= 0 (platform:gcry_md_is_enabled (gcry-md-handle->pointer mdhd)
					 (gcry-md-algo->value algo)))))

(define gcry-md-is-enabled? gcry-md-is-enabled)

(define gcry-md-enabled-algos
  (let ((algos (list GCRY_MD_MD5		GCRY_MD_SHA1
		     GCRY_MD_RMD160		GCRY_MD_MD2
		     GCRY_MD_TIGER		GCRY_MD_HAVAL
		     GCRY_MD_SHA256		GCRY_MD_SHA384
		     GCRY_MD_SHA512		GCRY_MD_SHA224
		     GCRY_MD_MD4		GCRY_MD_CRC32
		     GCRY_MD_CRC32_RFC1510	GCRY_MD_CRC24_RFC2440
		     GCRY_MD_WHIRLPOOL)))
    (lambda (mdhd)
      (let ((hd (gcry-md-handle->pointer mdhd)))
	(fold-left (lambda (knil algo)
		     (if (= 0 (platform:gcry_md_is_enabled hd algo))
			 knil
		       (enum-set-union (value->gcry-md-algo algo) knil)))
		   (%gcry-md-algo)
		   algos)))))

(define (gcry-md-get-asnoid algo)
  (with-compensations
    (let* ((algo-int	(gcry-md-algo->value algo))
	   (buf.len*	(malloc-small/c)))
      (platform:gcry_md_get_asnoid algo-int pointer-null buf.len*)
      (let* ((buf.ptr	(malloc-block/c (pointer-ref-c-size_t buf.len* 0)))
	     (errcode	(platform:gcry_md_get_asnoid algo-int buf.ptr buf.len*)))
	(if (= 0 errcode)
	    (pointer->bytevector buf.ptr (pointer-ref-c-size_t buf.len* 0))
	  (raise-gpg-error 'gcry-md-get-asnoid errcode algo))))))


;;;; S-expressions

(define strtoul
  (make-c-function/with-errno* libc-shared-object unsigned-long strtoul (char* void* int)))

(define (gcry-sexp->list sexp)
  (assert (pointer? sexp))
  (with-compensations

    (define (elm->item sexp i)
      (let* ((buf.len*	(malloc-small/c))
	     (buf.ptr	(platform:gcry_sexp_nth_data sexp i buf.len*)))
	(if (pointer-null? buf.ptr) ;true if element is a pair
	    (letrec ((sub (compensate
			      (begin0-let ((p (platform:gcry_sexp_nth sexp i)))
				(when (pointer-null? p)
				  (error 'gcry-sexp->list "memory error converting sexp to list" sexp)))
			    (with
			     (platform:gcry_sexp_release sub)))))
	      (gcry-sexp->list sub))
	  (let* ((buf.len	(pointer-ref-c-size_t buf.len* 0))
		 (cstr		(malloc-block/c (+ 1 buf.len)))
		 (tail**	(malloc-small/c)))
	    (memcpy cstr buf.ptr buf.len)
	    (pointer-set-c-signed-char! cstr buf.len 0)
	    (receive (uint errno)
		(strtoul cstr tail** 0)
	      (cond ((not (pointer=? cstr (pointer-ref-c-pointer tail** 0)))
		     (if (= 0 errno)
			 uint
		       (error 'gcry-sexp->list "overflow in integer conversion" sexp)))
		    ((= 0 (pointer-ref-c-unsigned-char buf.ptr 0))
		     (let ((mpi (platform:gcry_sexp_nth_mpi sexp i GCRYMPI_FMT_STD)))
		       (if (not (pointer-null? mpi))
			   mpi
			 (error 'gcry-sexp->list "invalid MPI number in sexp" sexp))))
		    (else
		     (string->symbol (cstring->string buf.ptr buf.len)))))))))

    (let ((len (platform:gcry_sexp_length sexp)))
      (let loop ((i 0) (ell '()))
	(if (= i len)
	    (reverse ell)
	  (loop (+ 1 i) (cons (elm->item sexp i) ell)))))))

(define (list->gcry-sexp ell)
  ;;Convert ELL to a canonical sexp string, then apply STRING->GCRY-SEXP.
  ;;
  (define (print-list obj port)
    (cond ((null? obj)
	   (display "()" port))
	  ((pair? obj)
	   (display "(" port)
	   (for-each (lambda (item)
		       (print-list item port))
	     obj)
	   (display ")" port))
	  ((symbol? obj)
	   (let ((str (symbol->string obj)))
	     (display (string-length str) port)
	     (display #\: port)
	     (display str port)))
	  ((and (integer? obj) (exact? obj) (<= 0 obj))
	   (let ((str (number->string obj)))
	     (display (string-length str) port)
	     (display #\: port)
	     (display str port)))
	  ((pointer? obj)
	   (let ((str (gcry-mpi-print obj (gcry-mpi-format hex))))
	     (display (string-length str) port)
	     (display #\: port)
	     (display (string-downcase str) port)))
	  (else
	   (error 'list->gcry-sexp "invalid sexp element in list" obj))))
  (string->gcry-sexp (call-with-string-output-port
			 (lambda (port)
			   (print-list ell port)))))

;;; --------------------------------------------------------------------

(define (string->gcry-sexp str)
  (assert (string? str))
  (with-compensations
    (let* ((sexp*	(malloc-small/c))
	   (errcode	(platform:gcry_sexp_new sexp* (string->cstring/c str) 0 1)))
      (if (= 0 errcode)
	  (pointer-ref-c-pointer sexp* 0)
	(raise-gpg-error 'gcry-sexp-new errcode str)))))

(define gcry-sexp->string
  (case-lambda
   ((sexp)
    (gcry-sexp->string sexp (gcry-sexp-format default)))
   ((sexp fmt)
    (assert (pointer? sexp))
    (let* ((mode	(gcry-sexp-format->value fmt))
	   (buf.len	(platform:gcry_sexp_sprint sexp mode pointer-null 0)))
      (with-compensations
	(let ((buf.ptr (malloc-block/c buf.len)))
	  (platform:gcry_sexp_sprint sexp mode buf.ptr buf.len)
	  (cstring->string buf.ptr (- buf.len 1))))))))

(define (gcry-sexp-find-token/str sexp str)
  (assert (pointer? sexp))
  (assert (or (symbol? str) (string? str)))
  (with-compensations
    (let ((p (platform:gcry_sexp_find_token sexp (string->cstring/c str) 0)))
      (if (pointer-null? p)
	  #f
	p))))


;;;; public key cryptography

(define (%obj->sexp/c obj procname)
  (cond ((string? obj)
	 (letrec ((sex (compensate
			   (string->gcry-sexp obj)
			 (with
			  (platform:gcry_sexp_release sex)))))
	   sex))
	((list? obj)
	 (letrec ((sex (compensate
			   (list->gcry-sexp obj)
			 (with
			  (platform:gcry_sexp_release sex)))))
	   sex))
	((pointer? obj)
	 obj)
	(else
	 (assertion-violation procname "expected Gcrypt sexp or string or list or pointer" obj))))

(define (gcry-pk-genkey params)
  (with-compensations
    (let* ((params-s	(%obj->sexp/c params 'gcry-pk-genkey))
	   (result*	(malloc-small/c))
	   (errcode	(platform:gcry_pk_genkey result* params-s)))
      (if (= 0 errcode)
	  (pointer-ref-c-pointer result* 0)
	(raise-gpg-error 'gcry-pk-genkey errcode params)))))

(define (gcry-pk-encrypt data pub-key)
  (with-compensations
    (let* ((data-s	(%obj->sexp/c data 'gcry-pk-encrypt))
	   (pub-key-s	(%obj->sexp/c pub-key 'gcry-pk-encrypt))
	   (result*	(malloc-small/c))
	   (errcode	(platform:gcry_pk_encrypt result* data-s pub-key-s)))
      (if (= 0 errcode)
	  (pointer-ref-c-pointer result* 0)
	(raise-gpg-error 'gcry-pk-encrypt errcode data pub-key)))))

(define (gcry-pk-decrypt data pri-key)
  (with-compensations
    (let* ((data-s	(%obj->sexp/c data 'gcry-pk-decrypt))
	   (pri-key-s	(%obj->sexp/c pri-key 'gcry-pk-decrypt))
	   (result*	(malloc-small/c))
	   (errcode	(platform:gcry_pk_decrypt result* data-s pri-key-s)))
      (if (= 0 errcode)
	  (pointer-ref-c-pointer result* 0)
	(raise-gpg-error 'gcry-pk-decrypt errcode data pri-key)))))

(define (gcry-pk-sign data pri-key)
  (with-compensations
    (let* ((data-s	(%obj->sexp/c data 'gcry-pk-sign))
	   (pri-key-s	(%obj->sexp/c pri-key 'gcry-pk-sign))
	   (result*	(malloc-small/c))
	   (errcode	(platform:gcry_pk_sign result* data-s pri-key-s)))
      (if (= 0 errcode)
	  (pointer-ref-c-pointer result* 0)
	(raise-gpg-error 'gcry-pk-sign errcode data pri-key)))))

(define (gcry-pk-verify signature data pub-key)
  (with-compensations
    (let* ((signature-s	(%obj->sexp/c signature 'gcry-pk-verify))
	   (data-s	(%obj->sexp/c data 'gcry-pk-verify))
	   (pub-key-s	(%obj->sexp/c pub-key 'gcry-pk-verify))
	   (errcode	(platform:gcry_pk_verify signature-s data-s pub-key-s)))
      (cond ((= 0 errcode)
	     #t)
	    ((= errcode GPG_ERR_BAD_SIGNATURE)
	     #f)
	    (else
	     (raise-gpg-error 'gcry-pk-verify errcode signature data pub-key))))))


;;;; pseudo-random numbers

(define (gcry-randomize mb quality)
  (platform:gcry_randomize (<memblock>-pointer mb) (<memblock>-size mb)
			   (gcry-random-quality->value quality)))

(define gcry-random-add-bytes
  (case-lambda
   ((mb)
    (gcry-random-add-bytes mb #f))
   ((mb quality)
    (platform:gcry_random_add_bytes (<memblock>-pointer mb) (<memblock>-size mb)
				    (if quality
					(gcry-random-quality->value quality)
				      -1)))))

(define (gcry-random-bytes nbytes quality)
  (let ((ptr (platform:gcry_random_bytes nbytes (gcry-random-quality->value quality))))
    (make-<memblock> ptr nbytes #f)))

(define (gcry-random-bytes-secure nbytes quality)
  (let ((ptr (platform:gcry_random_bytes_secure nbytes (gcry-random-quality->value quality))))
    (make-<memblock> ptr nbytes #f)))

(define gcry-random-bytes/secure gcry-random-bytes-secure)

(define (gcry-create-nonce mb)
  (platform:gcry_create_nonce (<memblock>-pointer mb) (<memblock>-size mb)))


;;;; MPI numbers

(define gcry-mpi=?
  (case-lambda
   (() #f)
   ((n) #t)
   ((a b)
    (= 0 (platform:gcry_mpi_cmp a b)))
   ((a b . args)
    (and (gcry-mpi=? a b) (apply gcry-mpi=? b args)))))

(define gcry-mpi<?
  (case-lambda
   (() #f)
   ((n) #t)
   ((a b)
    (> 0 (platform:gcry_mpi_cmp a b)))
   ((a b . args)
    (and (gcry-mpi<? a b) (apply gcry-mpi<? b args)))))

(define gcry-mpi>?
  (case-lambda
   (() #f)
   ((n) #t)
   ((a b)
    (< 0 (platform:gcry_mpi_cmp a b)))
   ((a b . args)
    (and (gcry-mpi>? a b) (apply gcry-mpi>? b args)))))

(define gcry-mpi<=?
  (case-lambda
   (() #f)
   ((n) #t)
   ((a b)
    (>= 0 (platform:gcry_mpi_cmp a b)))
   ((a b . args)
    (and (gcry-mpi<=? a b) (apply gcry-mpi<=? b args)))))

(define gcry-mpi>=?
  (case-lambda
   (() #f)
   ((n) #t)
   ((a b)
    (<= 0 (platform:gcry_mpi_cmp a b)))
   ((a b . args)
    (and (gcry-mpi>=? a b) (apply gcry-mpi>=? b args)))))

;;; --------------------------------------------------------------------

(define (gcry-mpi-scan obj fmt)
  (with-compensations
    (receive (obj.ptr obj.len)
	(%object->ptr&len obj 'gcry-mpi-scan
			  "expected string, bytevector or memblock as MPI representation")
      (let* ((mpi*	(malloc-small/c))
	     (errcode	(platform:gcry_mpi_scan mpi* (gcry-mpi-format->value fmt)
						obj.ptr (if (= (gcry-mpi-format->value fmt)
							       GCRYMPI_FMT_HEX)
							    0
							  obj.len)
						pointer-null)))
	(if (= 0 errcode)
	    (pointer-ref-c-pointer mpi* 0)
	  (raise-gpg-error 'gcry-mpi-scan errcode obj fmt))))))

(define (gcry-mpi-print mpi fmt)
  (with-compensations
    (let* ((fmt-int	(gcry-mpi-format->value fmt))
	   (buf.len	(let* ((nwritten* (malloc-small/c))
			       (errcode	  (platform:gcry_mpi_print fmt-int pointer-null 0 nwritten* mpi)))
			  (if (= 0 errcode)
			      (pointer-ref-c-size_t nwritten* 0)
			    (raise-gpg-error 'gcry-mpi-print errcode mpi fmt)))))
      (let* ((buf.ptr	(malloc-block/c buf.len))
	     (errcode	(platform:gcry_mpi_print fmt-int buf.ptr buf.len pointer-null mpi)))
	(cond ((not (= 0 errcode))
	       (raise-gpg-error 'gcry-mpi-print errcode mpi fmt))
	      ((= fmt-int GCRYMPI_FMT_HEX)
	       (cstring->string buf.ptr (- buf.len 1)))
	      (else
	       (pointer->bytevector buf.ptr buf.len)))))))

(define (gcry-mpi->uint mpi)
  (with-compensations
    (let* ((buf.ptr	(malloc-block/c sizeof-int))
	   (errcode	(platform:gcry_mpi_print GCRYMPI_FMT_USG buf.ptr sizeof-int pointer-null mpi)))
      (if (= 0 errcode)
	  (pointer-ref-c-unsigned-int buf.ptr 0)
	(raise-gpg-error 'gcry-mpi-print errcode mpi)))))


;;;; callback makers

(define (make-gcry-prime-check-func-t-callback scheme-function)
  (make-c-callback* int
		    scheme-function
		    (void* int gcry_mpi_t)))

(define (make-gcry_handler_progress_t-callback scheme-function)
  (make-c-callback* void
		    scheme-function
		    (void* char* int int int)))

(define (make-gcry_handler_alloc_t-callback scheme-function)
  (make-c-callback* void*
		    scheme-function
		    (size_t)))

(define (make-gcry_handler_secure_check_t-callback scheme-function)
  (make-c-callback* int
		    scheme-function
		    (void*)))

(define (make-gcry_handler_realloc_t-callback scheme-function)
  (make-c-callback* void*
		    scheme-function
		    (void* size_t)))

(define (make-gcry_handler_free_t-callback scheme-function)
  (make-c-callback* void
		    scheme-function
		    (void*)))

(define (make-gcry_handler_no_mem_t-callback scheme-function)
  (make-c-callback* int
		    scheme-function
		    (void* size_t unsigned-int)))

(define (make-gcry_handler_error_t-callback scheme-function)
  (make-c-callback* void
		    scheme-function
		    (void* int char*)))

;;Variadic!!!
;;
;; (define (make-gcry_handler_log_t-callback scheme-function)
;;   (make-c-callback* void
;; 		    gcry_handler_log_t
;; 		    (void* int char* va_list)))



;;;; done

)

;;; end of file
