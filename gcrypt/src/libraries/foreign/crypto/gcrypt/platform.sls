;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Gcrypt
;;;Contents: bindings to foreign functions
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


(library (foreign crypto gcrypt platform)
  (export
    gcry_check_version
    gcry_control/int
    gcry_control/uint
    gcry_control/ptr

    gcry_strerror
    gcry_strsource
    gcry_err_code_from_errno
    gcry_err_code_to_errno
    gcry_err_make_from_errno
    gcry_error_from_errno

    gcry_sexp_new
    gcry_sexp_create
    gcry_sexp_sscan
;;;gcry_sexp_build
    gcry_sexp_build_array
    gcry_sexp_release
    gcry_sexp_canon_len
    gcry_sexp_sprint
    gcry_sexp_dump
    gcry_sexp_cons
    gcry_sexp_alist
;;;    gcry_sexp_vlist
    gcry_sexp_append
    gcry_sexp_prepend
    gcry_sexp_find_token
    gcry_sexp_length
    gcry_sexp_nth
    gcry_sexp_car
    gcry_sexp_cdr
    gcry_sexp_cadr
    gcry_sexp_nth_data
    gcry_sexp_nth_string
    gcry_sexp_nth_mpi

    gcry_mpi_new
    gcry_mpi_snew
    gcry_mpi_release
    gcry_mpi_copy
    gcry_mpi_set
    gcry_mpi_set_ui
    gcry_mpi_swap
    gcry_mpi_cmp
    gcry_mpi_cmp_ui
    gcry_mpi_scan
    gcry_mpi_print
    gcry_mpi_aprint
    gcry_mpi_dump
    gcry_mpi_add
    gcry_mpi_add_ui
    gcry_mpi_addm
    gcry_mpi_sub
    gcry_mpi_sub_ui
    gcry_mpi_subm
    gcry_mpi_mul
    gcry_mpi_mul_ui
    gcry_mpi_mulm
    gcry_mpi_mul_2exp
    gcry_mpi_div
    gcry_mpi_mod
    gcry_mpi_powm
    gcry_mpi_gcd
    gcry_mpi_invm
    gcry_mpi_get_nbits
    gcry_mpi_test_bit
    gcry_mpi_set_bit
    gcry_mpi_clear_bit
    gcry_mpi_set_highbit
    gcry_mpi_clear_highbit
    gcry_mpi_rshift
    gcry_mpi_lshift
    gcry_mpi_set_opaque
    gcry_mpi_get_opaque
    gcry_mpi_set_flag
    gcry_mpi_clear_flag
    gcry_mpi_get_flag

    gcry_cipher_open
    gcry_cipher_close
    gcry_cipher_ctl
    gcry_cipher_info
    gcry_cipher_algo_info
    gcry_cipher_algo_name
    gcry_cipher_map_name
    gcry_cipher_mode_from_oid
    gcry_cipher_encrypt
    gcry_cipher_decrypt
    gcry_cipher_setkey
    gcry_cipher_setiv
    gcry_cipher_setctr
    gcry_cipher_get_algo_keylen
    gcry_cipher_get_algo_blklen
    gcry_cipher_list

    gcry_cipher_reset
    gcry_cipher_sync
    gcry_cipher_cts
    gcry_cipher_test_algo

    gcry_pk_encrypt
    gcry_pk_decrypt
    gcry_pk_sign
    gcry_pk_verify
    gcry_pk_testkey
    gcry_pk_genkey
    gcry_pk_ctl
    gcry_pk_algo_info
    gcry_pk_algo_name
    gcry_pk_map_name
    gcry_pk_get_nbits
    gcry_pk_get_keygrip
    gcry_pk_list

    gcry_pk_test_algo

    gcry_md_open
    gcry_md_close
    gcry_md_enable
    gcry_md_copy
    gcry_md_reset
    gcry_md_ctl
    gcry_md_write
    gcry_md_read
    gcry_md_hash_buffer
    gcry_md_get_algo
    gcry_md_get_algo_dlen
    gcry_md_is_enabled
    gcry_md_is_secure
    gcry_md_info
    gcry_md_algo_info
    gcry_md_algo_name
    gcry_md_map_name
    gcry_md_setkey
    gcry_md_debug
    gcry_md_list

    gcry_md_final
    gcry_md_test_algo
    gcry_md_get_asnoid

    gcry_randomize
    gcry_random_add_bytes
    gcry_random_bytes
    gcry_random_bytes_secure
    gcry_mpi_randomize
    gcry_create_nonce

    gcry_fast_random_poll

    gcry_prime_generate
    gcry_prime_group_generator
    gcry_prime_release_factors
    gcry_prime_check
    gcry_set_progress_handler
    gcry_set_allocation_handler
    gcry_set_outofcore_handler
    gcry_set_fatalerror_handler
    gcry_set_log_handler
    gcry_set_gettext_handler
    gcry_malloc
    gcry_calloc
    gcry_malloc_secure
    gcry_calloc_secure
    gcry_realloc
    gcry_strdup
    gcry_xmalloc
    gcry_xcalloc
    gcry_xmalloc_secure
    gcry_xcalloc_secure
    gcry_xrealloc
    gcry_xstrdup
    gcry_free
    gcry_is_secure

    gcry_fips_mode_active)
  (import (rnrs)
    (foreign ffi)
    (foreign ffi sizeof)
    (foreign crypto gcrypt shared-object)
    (foreign crypto gcrypt sizeof)
    (only (foreign crypto gpg-error sizeof) gpg_error_t))


(define void**			'pointer)
(define int*			'pointer)
(define int**			'pointer)
(define unsigned-int*		'pointer)
(define size_t*			'pointer)


(define-c-functions gcrypt-shared-object
  (gcry_check_version		(char* gcry_check_version (char*)))

  (gcry_control/int		(gcry_error_t gcry_control (gcry_ctl_cmds int)))
  (gcry_control/uint		(gcry_error_t gcry_control (gcry_ctl_cmds unsigned-int)))
  (gcry_control/ptr		(gcry_error_t gcry_control (gcry_ctl_cmds pointer)))
  )

(define-c-functions gcrypt-shared-object
  (gcry_strerror		(char* gcry_strerror (gcry_error_t)))
  (gcry_strsource		(char* gcry_strsource (gcry_error_t)))
  (gcry_err_code_from_errno	(gcry_err_code_t gcry_err_code_from_errno (int)))
  (gcry_err_code_to_errno	(int gcry_err_code_to_errno (gcry_err_code_t)))
  (gcry_err_make_from_errno	(gcry_error_t gcry_err_make_from_errno (gcry_err_source_t int)))
  (gcry_error_from_errno	(gcry_err_code_t gcry_error_from_errno (int)))
  )


(define-c-functions gcrypt-shared-object
  (gcry_sexp_new		(gcry_error_t gcry_sexp_new (gcry_sexp_t* void* size_t int)))
  (gcry_sexp_create		(gcry_error_t gcry_sexp_create
					      (gcry_sexp_t* void* size_t int callback)))
  (gcry_sexp_sscan		(gcry_error_t gcry_sexp_sscan (gcry_sexp_t* size_t* char* size_t)))
;;;(gcry_sexp_build		(gcry_error_t gcry_sexp_build (gcry_sexp_t* size_t* char* ...)))
  (gcry_sexp_build_array	(gcry_error_t gcry_sexp_build_array (gcry_sexp_t* size_t* char* void**)))
  (gcry_sexp_release		(void gcry_sexp_release (gcry_sexp_t)))
  (gcry_sexp_canon_len		(size_t gcry_sexp_canon_len (void* size_t size_t* gcry_error_t*)))
  (gcry_sexp_sprint		(size_t gcry_sexp_sprint (gcry_sexp_t int void* size_t)))
  (gcry_sexp_dump		(void gcry_sexp_dump (gcry_sexp_t)))
  (gcry_sexp_cons		(gcry_sexp_t gcry_sexp_cons (gcry_sexp_t gcry_sexp_t)))
  (gcry_sexp_alist		(gcry_sexp_t gcry_sexp_alist (gcry_sexp_t*)))
;;;  (gcry_sexp_vlist		(gcry_sexp_t gcry_sexp_vlist (gcry_sexp_t ...)))
  (gcry_sexp_append		(gcry_sexp_t gcry_sexp_append (gcry_sexp_t gcry_sexp_t)))
  (gcry_sexp_prepend		(gcry_sexp_t gcry_sexp_prepend (gcry_sexp_t gcry_sexp_t)))
  (gcry_sexp_find_token		(gcry_sexp_t gcry_sexp_find_token (gcry_sexp_t char* size_t)))
  (gcry_sexp_length		(int gcry_sexp_length (gcry_sexp_t)))
  (gcry_sexp_nth		(gcry_sexp_t gcry_sexp_nth (gcry_sexp_t int)))
  (gcry_sexp_car		(gcry_sexp_t gcry_sexp_car (gcry_sexp_t)))
  (gcry_sexp_cdr		(gcry_sexp_t gcry_sexp_cdr (gcry_sexp_t)))
  (gcry_sexp_cadr		(gcry_sexp_t gcry_sexp_cadr (gcry_sexp_t)))
  (gcry_sexp_nth_data		(char* gcry_sexp_nth_data (gcry_sexp_t int size_t*)))
  (gcry_sexp_nth_string		(char* gcry_sexp_nth_string (gcry_sexp_t int)))
  (gcry_sexp_nth_mpi		(gcry_mpi_t gcry_sexp_nth_mpi (gcry_sexp_t int int)))
  )

(define-c-functions gcrypt-shared-object
  (gcry_mpi_new			(gcry_mpi_t gcry_mpi_new (unsigned int)))
  (gcry_mpi_snew		(gcry_mpi_t gcry_mpi_snew (unsigned int)))
  (gcry_mpi_release		(void gcry_mpi_release (gcry_mpi_t)))
  (gcry_mpi_copy		(gcry_mpi_t gcry_mpi_copy (gcry_mpi_t)))
  (gcry_mpi_set			(gcry_mpi_t gcry_mpi_set (gcry_mpi_t gcry_mpi_t)))
  (gcry_mpi_set_ui		(gcry_mpi_t gcry_mpi_set_ui (gcry_mpi_t unsigned-long)))
  (gcry_mpi_swap		(void gcry_mpi_swap (gcry_mpi_t gcry_mpi_t)))
  (gcry_mpi_cmp			(int gcry_mpi_cmp (gcry_mpi_t gcry_mpi_t)))
  (gcry_mpi_cmp_ui		(int gcry_mpi_cmp_ui (gcry_mpi_t unsigned-long)))
  (gcry_mpi_scan		(gcry_error_t gcry_mpi_scan
					      (gcry_mpi_t* gcry_mpi_format void* size_t size_t*)))
  (gcry_mpi_print		(gcry_error_t gcry_mpi_print
					      (gcry_mpi_format void* size_t size_t* gcry_mpi_t)))
  (gcry_mpi_aprint		(gcry_error_t gcry_mpi_aprint (gcry_mpi_format void** size_t* gcry_mpi_t)))
  (gcry_mpi_dump		(void gcry_mpi_dump (gcry_mpi_t)))
  (gcry_mpi_add			(void gcry_mpi_add (gcry_mpi_t gcry_mpi_t gcry_mpi_t)))
  (gcry_mpi_add_ui		(void gcry_mpi_add_ui (gcry_mpi_t gcry_mpi_t unsigned-long)))
  (gcry_mpi_addm		(void gcry_mpi_addm (gcry_mpi_t gcry_mpi_t gcry_mpi_t gcry_mpi_t)))
  (gcry_mpi_sub			(void gcry_mpi_sub (gcry_mpi_t gcry_mpi_t gcry_mpi_t)))
  (gcry_mpi_sub_ui		(void gcry_mpi_sub_ui (gcry_mpi_t gcry_mpi_t unsigned-long)))
  (gcry_mpi_subm		(void gcry_mpi_subm (gcry_mpi_t gcry_mpi_t gcry_mpi_t gcry_mpi_t)))
  (gcry_mpi_mul			(void gcry_mpi_mul (gcry_mpi_t gcry_mpi_t gcry_mpi_t)))
  (gcry_mpi_mul_ui		(void gcry_mpi_mul_ui (gcry_mpi_t gcry_mpi_t unsigned-long)))
  (gcry_mpi_mulm		(void gcry_mpi_mulm (gcry_mpi_t gcry_mpi_t gcry_mpi_t gcry_mpi_t)))
  (gcry_mpi_mul_2exp		(void gcry_mpi_mul_2exp (gcry_mpi_t gcry_mpi_t unsigned-long)))
  (gcry_mpi_div			(void gcry_mpi_div (gcry_mpi_t gcry_mpi_t gcry_mpi_t gcry_mpi_t int)))
  (gcry_mpi_mod			(void gcry_mpi_mod (gcry_mpi_t gcry_mpi_t gcry_mpi_t)))
  (gcry_mpi_powm		(void gcry_mpi_powm (gcry_mpi_t gcry_mpi_t gcry_mpi_t gcry_mpi_t)))
  (gcry_mpi_gcd			(int gcry_mpi_gcd (gcry_mpi_t gcry_mpi_t gcry_mpi_t)))
  (gcry_mpi_invm		(int gcry_mpi_invm (gcry_mpi_t gcry_mpi_t gcry_mpi_t)))
  (gcry_mpi_get_nbits		(unsigned-int gcry_mpi_get_nbits (gcry_mpi_t)))
  (gcry_mpi_test_bit		(int gcry_mpi_test_bit (gcry_mpi_t unsigned-int)))
  (gcry_mpi_set_bit		(void gcry_mpi_set_bit (gcry_mpi_t unsigned-int)))
  (gcry_mpi_clear_bit		(void gcry_mpi_clear_bit (gcry_mpi_t unsigned-int)))
  (gcry_mpi_set_highbit		(void gcry_mpi_set_highbit (gcry_mpi_t unsigned-int)))
  (gcry_mpi_clear_highbit	(void gcry_mpi_clear_highbit (gcry_mpi_t unsigned-int)))
  (gcry_mpi_rshift		(void gcry_mpi_rshift (gcry_mpi_t gcry_mpi_t unsigned-int)))
  (gcry_mpi_lshift		(void gcry_mpi_lshift (gcry_mpi_t gcry_mpi_t unsigned-int)))
  (gcry_mpi_set_opaque		(gcry_mpi_t gcry_mpi_set_opaque (gcry_mpi_t void* unsigned-int)))
  (gcry_mpi_get_opaque		(void* gcry_mpi_get_opaque (gcry_mpi_t unsigned-int*)))
  (gcry_mpi_set_flag		(void gcry_mpi_set_flag (gcry_mpi_t gcry_mpi_flag)))
  (gcry_mpi_clear_flag		(void gcry_mpi_clear_flag (gcry_mpi_t gcry_mpi_flag)))
  (gcry_mpi_get_flag		(int gcry_mpi_get_flag (gcry_mpi_t gcry_mpi_flag)))
  )


(define-c-functions gcrypt-shared-object
  (gcry_cipher_open		(gcry_error_t gcry_cipher_open (gcry_cipher_hd_t* int int unsigned-int)))
  (gcry_cipher_close		(void gcry_cipher_close (gcry_cipher_hd_t)))
  (gcry_cipher_ctl		(gcry_error_t gcry_cipher_ctl (gcry_cipher_hd_t int void* size_t)))
  (gcry_cipher_info		(gcry_error_t gcry_cipher_info (gcry_cipher_hd_t int void* size_t*)))
  (gcry_cipher_algo_info	(gcry_error_t gcry_cipher_algo_info (int int void* size_t*)))
  (gcry_cipher_algo_name	(char* gcry_cipher_algo_name (int)))
  (gcry_cipher_map_name		(int gcry_cipher_map_name (char*)))
  (gcry_cipher_mode_from_oid	(int gcry_cipher_mode_from_oid (char*)))
  (gcry_cipher_encrypt		(gcry_error_t gcry_cipher_encrypt
					      (gcry_cipher_hd_t void* size_t void* size_t)))
  (gcry_cipher_decrypt		(gcry_error_t gcry_cipher_decrypt
					      (gcry_cipher_hd_t void* size_t void* size_t)))
  (gcry_cipher_setkey		(gcry_error_t gcry_cipher_setkey (gcry_cipher_hd_t void* size_t)))
  (gcry_cipher_setiv		(gcry_error_t gcry_cipher_setiv (gcry_cipher_hd_t void* size_t)))
  (gcry_cipher_setctr		(gpg_error_t gcry_cipher_setctr (gcry_cipher_hd_t void* size_t)))
  (gcry_cipher_get_algo_keylen	(size_t gcry_cipher_get_algo_keylen (int)))
  (gcry_cipher_get_algo_blklen	(size_t gcry_cipher_get_algo_blklen (int)))
  (gcry_cipher_list		(gcry_error_t gcry_cipher_list (int* int*))))

(define (gcry_cipher_reset h)
  (gcry_cipher_ctl h GCRYCTL_RESET pointer-null 0))

(define (gcry_cipher_sync h)
  (gcry_cipher_ctl h GCRYCTL_CFB_SYNC pointer-null 0))

(define (gcry_cipher_cts h on)
  (gcry_cipher_ctl h GCRYCTL_SET_CBC_CTS pointer-null on))

(define (gcry_cipher_test_algo a)
  (gcry_cipher_algo_info a GCRYCTL_TEST_ALGO pointer-null pointer-null))


(define-c-functions gcrypt-shared-object
  (gcry_pk_encrypt		(gcry_error_t gcry_pk_encrypt (gcry_sexp_t* gcry_sexp_t gcry_sexp_t)))
  (gcry_pk_decrypt		(gcry_error_t gcry_pk_decrypt (gcry_sexp_t* gcry_sexp_t gcry_sexp_t)))
  (gcry_pk_sign			(gcry_error_t gcry_pk_sign (gcry_sexp_t* gcry_sexp_t gcry_sexp_t)))
  (gcry_pk_verify		(gcry_error_t gcry_pk_verify (gcry_sexp_t gcry_sexp_t gcry_sexp_t )))
  (gcry_pk_testkey		(gcry_error_t gcry_pk_testkey (gcry_sexp_t)))
  (gcry_pk_genkey		(gcry_error_t gcry_pk_genkey (gcry_sexp_t* gcry_sexp_t)))
  (gcry_pk_ctl			(gcry_error_t gcry_pk_ctl (int void* size_t)))
  (gcry_pk_algo_info		(gcry_error_t gcry_pk_algo_info (int int void* size_t*)))
  (gcry_pk_algo_name		(char* gcry_pk_algo_name (int)))
  (gcry_pk_map_name		(int gcry_pk_map_name (char*)))
  (gcry_pk_get_nbits		(unsigned-int gcry_pk_get_nbits (gcry_sexp_t)))
  (gcry_pk_get_keygrip		(void* gcry_pk_get_keygrip (gcry_sexp_t void*)))
  (gcry_pk_list			(gcry_error_t gcry_pk_list (int* int*))))

(define (gcry_pk_test_algo a)
  (gcry_pk_algo_info a GCRYCTL_TEST_ALGO pointer-null pointer-null))


(define-c-functions gcrypt-shared-object
  (gcry_md_open			(gcry_error_t gcry_md_open (gcry_md_hd_t* int unsigned-int)))
  (gcry_md_close		(void gcry_md_close (gcry_md_hd_t)))
  (gcry_md_enable		(gcry_error_t gcry_md_enable (gcry_md_hd_t int)))
  (gcry_md_copy			(gcry_error_t gcry_md_copy (gcry_md_hd_t* gcry_md_hd_t)))
  (gcry_md_reset		(void gcry_md_reset (gcry_md_hd_t)))
  (gcry_md_ctl			(gcry_error_t gcry_md_ctl (gcry_md_hd_t int void* size_t)))
  (gcry_md_write		(void gcry_md_write (gcry_md_hd_t void* size_t)))
  (gcry_md_read			(void* gcry_md_read (gcry_md_hd_t int)))
  (gcry_md_hash_buffer		(void gcry_md_hash_buffer (int void* void* size_t)))
  (gcry_md_get_algo		(int gcry_md_get_algo (gcry_md_hd_t)))
  (gcry_md_get_algo_dlen		(unsigned-int gcry_md_get_algo_dlen (int)))
  (gcry_md_is_enabled		(int gcry_md_is_enabled (gcry_md_hd_t int)))
  (gcry_md_is_secure		(int gcry_md_is_secure (gcry_md_hd_t)))
  (gcry_md_info			(gcry_error_t gcry_md_info (gcry_md_hd_t int void* size_t*)))
  (gcry_md_algo_info		(gcry_error_t gcry_md_algo_info (int int void* size_t*)))
  (gcry_md_algo_name		(char* gcry_md_algo_name (int)))
  (gcry_md_map_name		(int gcry_md_map_name (char*)))
  (gcry_md_setkey		(gcry_error_t gcry_md_setkey (gcry_md_hd_t void* size_t)))
  (gcry_md_debug		(void gcry_md_debug (gcry_md_hd_t char*)))
  (gcry_md_list			(gcry_error_t gcry_md_list (int* int*))))

(define (gcry_md_final a)
  (gcry_md_ctl a GCRYCTL_FINALIZE pointer-null 0))

(define (gcry_md_test_algo a)
  (gcry_md_algo_info a GCRYCTL_TEST_ALGO pointer-null pointer-null))

(define (gcry_md_get_asnoid a b n)
  (gcry_md_algo_info a GCRYCTL_GET_ASNOID b n))


(define-c-functions gcrypt-shared-object
  (gcry_randomize		(void gcry_randomize (void* size_t gcry_random_level_t)))
  (gcry_random_add_bytes	(gcry_error_t gcry_random_add_bytes (void* size_t int)))
  (gcry_random_bytes		(void* gcry_random_bytes (size_t gcry_random_level_t)))
  (gcry_random_bytes_secure	(void* gcry_random_bytes_secure (size_t gcry_random_level_t)))
  (gcry_mpi_randomize		(void gcry_mpi_randomize (gcry_mpi_t unsigned-int gcry_random_level_t)))
  (gcry_create_nonce		(void gcry_create_nonce (void* size_t))))

(define (gcry_fast_random_poll)
  (gcry_control/ptr GCRYCTL_FAST_POLL pointer-null))


(define-c-functions gcrypt-shared-object
  (gcry_prime_generate		(gcry_error_t gcry_prime_generate
					      (gcry_mpi_t* unsigned-int unsigned-int gcry_mpi_t**
							   gcry_prime_check_func_t void*
							   gcry_random_level_t unsigned-int)))
  (gcry_prime_group_generator	(gcry_error_t gcry_prime_group_generator
					      (gcry_mpi_t* gcry_mpi_t gcry_mpi_t* gcry_mpi_t)))
  (gcry_prime_release_factors	(void gcry_prime_release_factors (gcry_mpi_t*)))
  (gcry_prime_check		(gcry_error_t gcry_prime_check (gcry_mpi_t unsigned-int))))

(define-c-functions gcrypt-shared-object
  (gcry_set_progress_handler	(void gcry_set_progress_handler (gcry_handler_progress_t void*)))
  (gcry_set_allocation_handler	(void gcry_set_allocation_handler
				      (gcry_handler_alloc_t gcry_handler_alloc_t
							    gcry_handler_secure_check_t
							    gcry_handler_realloc_t gcry_handler_free_t)))
  (gcry_set_outofcore_handler	(void gcry_set_outofcore_handler (gcry_handler_no_mem_t void*)))
  (gcry_set_fatalerror_handler	(void gcry_set_fatalerror_handler (gcry_handler_error_t void*)))
  (gcry_set_log_handler		(void gcry_set_log_handler (gcry_handler_log_t void*)))
  (gcry_set_gettext_handler	(void gcry_set_gettext_handler (callback)))
  (gcry_malloc			(void* gcry_malloc (size_t)))
  (gcry_calloc			(void* gcry_calloc (size_t size_t)))
  (gcry_malloc_secure		(void* gcry_malloc_secure (size_t)))
  (gcry_calloc_secure		(void* gcry_calloc_secure (size_t size_t)))
  (gcry_realloc			(void* gcry_realloc (void* size_t)))
  (gcry_strdup			(char* gcry_strdup (char*)))
  (gcry_xmalloc			(void* gcry_xmalloc (size_t)))
  (gcry_xcalloc			(void* gcry_xcalloc (size_t size_t)))
  (gcry_xmalloc_secure		(void* gcry_xmalloc_secure (size_t)))
  (gcry_xcalloc_secure		(void* gcry_xcalloc_secure (size_t size_t)))
  (gcry_xrealloc		(void* gcry_xrealloc (void* size_t)))
  (gcry_xstrdup			(char* gcry_xstrdup (char*)))
  (gcry_free			(void gcry_free (void*)))
  (gcry_is_secure		(int gcry_is_secure (void*))))

(define (gcry_fips_mode_active)
  (not (= 0 (gcry_control/int GCRYCTL_FIPS_MODE_P 0))))


;;;; done

)

;;; end of file
