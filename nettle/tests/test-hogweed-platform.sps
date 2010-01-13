;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Nettle
;;;Contents: tests for Hogweed platform libraries loading
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
  (receive)
  (compensations)
  (only (foreign ffi) make-c-callback*)
  (foreign cstrings)
  (foreign memory)
  (foreign math mp mpz)
  (foreign math mp sizeof)
  (foreign crypto nettle platform)
  (foreign crypto hogweed platform)
  (foreign crypto nettle sizeof)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing Hogweed platform\n")

(define (mpz/c)
  (letrec ((n (compensate
		  (malloc-block/c sizeof-mpz_t)
		(with
		 (mpz_clear n)))))
    (mpz_init n)
    n))


(parametrise ((check-test-name	'rsa))

  (define (rsa-make-keypair n-size e-size random-maker progress-printer)
    (with-compensations
      (let* ((pub*	(malloc sizeof-rsa_public_key))
	     (pri*	(malloc sizeof-rsa_private_key)))
	(rsa_public_key_init  pub*)
	(rsa_private_key_init pri*)
	(when (= 0 e-size)
	  (mpz_set_ui (struct-rsa_public_key-e-ref pub*) 65537))
	(unless (= 1 (rsa_generate_keypair
		      pub* pri*
		      pointer-null
		      (make-c-callback* void random-maker
					(void* unsigned void*))
		      pointer-null
		      (make-c-callback* void progress-printer
					(void* int))
		      n-size e-size))
	  (error 'rsa-make-keypair
	    "error generating RSA key pair"))
	(unless (= 1 (rsa_public_key_prepare  pub*))
	  (error 'rsa-make-keypair
	    "error preparing generated RSA public key"))
	(unless (= 1 (rsa_private_key_prepare pri*))
	  (error 'rsa-make-keypair
	    "error preparing generated RSA private key"))
	(values pub* pri*))))

  (define (rsa-md5-sign-string pri* str signature)
    (with-compensations
      (let* ((buf.ptr	(string->cstring/c str))
	     (buf.len	(strlen buf.ptr))
	     (md5*	(malloc-block/c sizeof-md5_ctx)))
	(md5_init md5*)
	(md5_update md5* buf.len buf.ptr)
	(rsa_md5_sign pri* md5* signature))))

  (define (rsa-md5-verify-string pub* str signature)
    (with-compensations
      (let* ((buf.ptr	(string->cstring/c str))
	     (buf.len	(strlen buf.ptr))
	     (md5*	(malloc-block/c sizeof-md5_ctx)))
	(md5_init md5*)
	(md5_update md5* buf.len buf.ptr)
	(rsa_md5_verify pub* md5* signature))))

;;; --------------------------------------------------------------------
;;; key generation

  (check
      (with-compensations
	(let* ((random*	(malloc-block/c sizeof-knuth_lfib_ctx))
	       (pub*	(malloc-block/c sizeof-rsa_public_key))
	       (pri*	(malloc-block/c sizeof-rsa_private_key)))
	  (knuth_lfib_init random* 123)
	  (rsa_public_key_init  pub*)
	  (rsa_private_key_init pri*)
	  (unless (= 1 (rsa_generate_keypair
			pub* pri*
			random* (make-c-callback* void knuth_lfib_random (void* unsigned void*))
			pointer-null (make-c-callback* void (lambda (unused i)
							      (display (integer->char i)))
						       (void* int))
			1024 50))
	    (error #f "error generating RSA key pair"))
	  (unless (= 1 (rsa_public_key_prepare  pub*))
	    (error #f "error preparing generated RSA public key"))
	  (unless (= 1 (rsa_private_key_prepare pri*))
	    (error #f "error preparing generated RSA private key"))
	  (rsa_public_key_clear  pub*)
	  (rsa_private_key_clear pri*)
	  #f))
    => #f)

  (check	;fixed "e" selection
      (with-compensations
	(let ((random*	(malloc-block/c sizeof-knuth_lfib_ctx)))
	  (knuth_lfib_init random* 123)
	  (receive (pub* pri*)
	      (rsa-make-keypair 1024 0
				(lambda (unused buf.len buf.ptr)
				  (knuth_lfib_random random* buf.len buf.ptr))
				(lambda (unused i)
				  (display (integer->char i))))
	    (rsa_public_key_clear  pub*)
	    (rsa_private_key_clear pri*)
	    #f)))
    => #f)

  (check	;explicit "e" selection
      (with-compensations
	(let ((random*	(malloc-block/c sizeof-knuth_lfib_ctx)))
	  (knuth_lfib_init random* 123)
	  (receive (pub* pri*)
	      (rsa-make-keypair 1024 50
				(lambda (unused buf.len buf.ptr)
				  (knuth_lfib_random random* buf.len buf.ptr))
				(lambda (unused i)
				  (display (integer->char i))))
	    (rsa_public_key_clear  pub*)
	    (rsa_private_key_clear pri*)
	    #f)))
    => #f)

;;; --------------------------------------------------------------------
;;; signing and encryption

  (with-compensations
    (let ((random* (malloc-block/c sizeof-knuth_lfib_ctx)))
      (knuth_lfib_init random* 123)
      (receive (pub* pri*)
	  (rsa-make-keypair 1024 0
			    (lambda (unused buf.len buf.ptr)
			      (knuth_lfib_random random* buf.len buf.ptr))
			    (lambda (unused i)
			      (display (integer->char i))))
	(push-compensation (rsa_public_key_clear  pub*))
	(push-compensation (rsa_private_key_clear pri*))

	(let ((str "calm like a bomb"))

	  (check
	      (let ((signature (mpz/c)))
		(rsa-md5-sign-string   pri* str signature)
		(rsa-md5-verify-string pub* str signature))
	    => 1)

	  (check
	      (let* ((cipher	(mpz/c))
		     (str.ptr	(string->cstring/c str))
		     (str.len	(strlen str.ptr))
		     (out.ptr	(malloc-block/c str.len))
		     (out.len*	(malloc-small/c)))
		(pointer-set-c-unsigned-int! out.len* 0 str.len)
		(unless (= 1 (rsa_encrypt pub* random*
					  (make-c-callback* void knuth_lfib_random
							    (void* unsigned void*))
					  str.len str.ptr cipher))
		  (error #f "error encrypting message with RSA"))
		(unless (= 1 (rsa_decrypt pri* out.len* out.ptr cipher))
		  (error #f "error decrypting message with RSA"))
		(cstring->string out.ptr (pointer-ref-c-unsigned-int out.len* 0)))
	    => str)


	  #f))))

  #t)


(parametrise ((check-test-name	'dsa))

  (define (dsa-make-keypair bits random-maker progress-printer)
    (with-compensations
      (let* ((pub*	(malloc sizeof-dsa_public_key))
	     (pri*	(malloc sizeof-dsa_private_key)))
	(dsa_public_key_init  pub*)
	(dsa_private_key_init pri*)
	(unless (= 1 (dsa_generate_keypair
		      pub* pri*
		      pointer-null
		      (make-c-callback* void random-maker
					(void* unsigned void*))
		      pointer-null
		      (make-c-callback* void progress-printer
					(void* int))
		      bits))
	  (error 'dsa-make-keypair
	    "error generating DSA key pair"))
	(values pub* pri*))))

  (define (dsa-sign-string pub* pri* str signature*)
    (with-compensations
      (let* ((buf.ptr	(string->cstring/c str))
  	     (buf.len	(strlen buf.ptr))
   	     (sha1*	(malloc-block/c sizeof-sha1_ctx))
	     (random*	(malloc-block/c sizeof-knuth_lfib_ctx)))
   	(sha1_init sha1*)
   	(sha1_update sha1* buf.len buf.ptr)
	(knuth_lfib_init random* 123)
  	(dsa_sign pub* pri*
		  random* (make-c-callback* void knuth_lfib_random (void* unsigned void*))
		  sha1* signature*)
	)))

  (define (dsa-verify-string pub* str signature*)
    (with-compensations
      (let* ((buf.ptr	(string->cstring/c str))
  	     (buf.len	(strlen buf.ptr))
  	     (sha1*	(malloc-block/c sizeof-sha1_ctx)))
  	(sha1_init sha1*)
  	(sha1_update sha1* buf.len buf.ptr)
  	(dsa_verify pub* sha1* signature*))))

;;; --------------------------------------------------------------------
;;; key generation

  (check
      (with-compensations
	(let* ((random*	(malloc-block/c sizeof-knuth_lfib_ctx))
	       (pub*	(malloc-block/c sizeof-dsa_public_key))
	       (pri*	(malloc-block/c sizeof-dsa_private_key)))
	  (knuth_lfib_init random* 123)
	  (dsa_public_key_init  pub*)
	  (dsa_private_key_init pri*)
	  (unless (= 1 (dsa_generate_keypair
			pub* pri*
			random* (make-c-callback* void knuth_lfib_random (void* unsigned void*))
			pointer-null (make-c-callback* void (lambda (unused i)
							      (display (integer->char i)))
						       (void* int))
			1024))
	    (error #f "error generating DSA key pair"))
	  (dsa_public_key_clear  pub*)
	  (dsa_private_key_clear pri*)
	  #f))
    => #f)

  (check
      (with-compensations
	(let ((random*	(malloc-block/c sizeof-knuth_lfib_ctx)))
	  (knuth_lfib_init random* 123)
	  (receive (pub* pri*)
	      (dsa-make-keypair 1024
				(lambda (unused buf.len buf.ptr)
				  (knuth_lfib_random random* buf.len buf.ptr))
				(lambda (unused i)
				  (display (integer->char i))))
	    (dsa_public_key_clear  pub*)
	    (dsa_private_key_clear pri*)
	    #f)))
    => #f)

;;; --------------------------------------------------------------------
;;; signing

  (with-compensations
    (let ((random* (malloc-block/c sizeof-knuth_lfib_ctx)))
      (knuth_lfib_init random* 123)
      (receive (pub* pri*)
  	  (dsa-make-keypair 1024
  			    (lambda (unused buf.len buf.ptr)
  			      (knuth_lfib_random random* buf.len buf.ptr))
  			    (lambda (unused i)
  			      (display (integer->char i))))
  	(push-compensation (dsa_public_key_clear  pub*))
  	(push-compensation (dsa_private_key_clear pri*))

  	(let ((str "calm like a bomb"))

  	  (check
  	      (let ((signature* (malloc-block/c sizeof-dsa_signature)))
	  	(compensate
	  	    (dsa_signature_init signature*)
	  	  (with
	  	   (dsa_signature_clear signature*)))
  	  	(dsa-sign-string pub* pri* str signature*)
  	  	(dsa-verify-string pub* str signature*))
  	    => 1)

  	  #f))))

  #t)


;;;; done

(check-report)

;;; end of file
