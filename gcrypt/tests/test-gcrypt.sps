;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Gcrypt
;;;Contents: test high-level API
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


(import (nausicaa)
  (compensations)
  (foreign memory)
  (foreign cstrings)
  (foreign crypto gcrypt)
  (foreign crypto gcrypt compensated)
  (prefix (foreign crypto gcrypt enumerations) enums:)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing Gcrypt high-level API\n")

(assert (gcry-check-version))
(gcry-control/int GCRYCTL_DISABLE_SECMEM 0)
(gcry-control/int GCRYCTL_INITIALIZATION_FINISHED 0)


(parametrise ((check-test-name	'enum))

  (check (gcry-cipher-algo->value (gcry-cipher-algo cast5))	=> GCRY_CIPHER_CAST5)
  (check (gcry-cipher-algo->value (gcry-cipher-algo blowfish))	=> GCRY_CIPHER_BLOWFISH)

  (check (value->gcry-cipher-algo GCRY_CIPHER_CAST5)	(=> enum-set=?) (gcry-cipher-algo cast5))
  (check (value->gcry-cipher-algo GCRY_CIPHER_BLOWFISH)	(=> enum-set=?) (gcry-cipher-algo blowfish))

;;; --------------------------------------------------------------------

  (check (gcry-cipher-mode->value (gcry-cipher-mode ecb))	=> GCRY_CIPHER_MODE_ECB)
  (check (gcry-cipher-mode->value (gcry-cipher-mode ctr))	=> GCRY_CIPHER_MODE_CTR)

  (check (value->gcry-cipher-mode GCRY_CIPHER_MODE_ECB)	(=> enum-set=?) (gcry-cipher-mode ecb))
  (check (value->gcry-cipher-mode GCRY_CIPHER_MODE_CTR)	(=> enum-set=?) (gcry-cipher-mode ctr))

;;; --------------------------------------------------------------------

  (check (gcry-cipher-flags->value (gcry-cipher-flags secure))		=> GCRY_CIPHER_SECURE)
  (check (gcry-cipher-flags->value (gcry-cipher-flags enable-sync))	=> GCRY_CIPHER_ENABLE_SYNC)
  (check (gcry-cipher-flags->value (gcry-cipher-flags cbc-cts))		=> GCRY_CIPHER_CBC_CTS)
  (check (gcry-cipher-flags->value (gcry-cipher-flags cbc-mac))		=> GCRY_CIPHER_CBC_MAC)

  (check
      (gcry-cipher-flags->value (gcry-cipher-flags secure enable-sync))
    => (bitwise-ior GCRY_CIPHER_SECURE GCRY_CIPHER_ENABLE_SYNC))

;;; --------------------------------------------------------------------

  (check (gcry-random-quality->value (gcry-random-quality weak))	=> GCRY_WEAK_RANDOM)
  (check (gcry-random-quality->value (gcry-random-quality strong))	=> GCRY_STRONG_RANDOM)
  (check (gcry-random-quality->value (gcry-random-quality very-strong))	=> GCRY_VERY_STRONG_RANDOM)

  (check (value->gcry-random-quality GCRY_WEAK_RANDOM)
    (=> enum-set=?) (gcry-random-quality weak))
  (check (value->gcry-random-quality GCRY_STRONG_RANDOM)
    (=> enum-set=?) (gcry-random-quality strong))
  (check (value->gcry-random-quality GCRY_VERY_STRONG_RANDOM)
    (=> enum-set=?) (gcry-random-quality very-strong))

  #t)


(parametrise ((check-test-name	'symmetric))

  (check
      (with-compensations
	(define (make)
	  (begin0-let ((hd (gcry-cipher-open/c (gcry-cipher-algo arcfour)
					       (gcry-cipher-mode stream))))
	    (gcry-cipher-setkey hd "ciao, ciao")))
	(let ((enc (make))
	      (dec (make)))
	  (let ((bv (gcry-cipher-encrypt* enc "you complete my fate")))
	    (utf8->string (gcry-cipher-decrypt* dec bv)))))
    => "you complete my fate")

  (let ((str "you complete my fate0123"))
;;;           012345678901234567890123
    (check
	(with-compensations
	  (define (make)
	    (begin0-let ((hd (gcry-cipher-open/c (gcry-cipher-algo des)
						 (gcry-cipher-mode ecb)
						 (gcry-cipher-flags))))
	      (gcry-cipher-setkey hd "12345678")
	      (gcry-cipher-setiv  hd "hello678")))
	  (let ((enc (make))
		(dec (make)))
	    (let ((bv (gcry-cipher-encrypt* enc str)))
	      (utf8->string (gcry-cipher-decrypt* dec bv)))))
      => str))

  (let ((str "you complete my fate0123"))
;;;           012345678901234567890123
    (check
	(with-compensations
	  (define (make)
	    (begin0-let ((hd (gcry-cipher-open/c (gcry-cipher-algo des)
						 (gcry-cipher-mode ctr)
						 (gcry-cipher-flags))))
	      (gcry-cipher-setkey hd "12345678")
	      (gcry-cipher-setiv  hd "hello678")
	      (gcry-cipher-setctr hd "salut678")))
	  (let ((enc (make))
		(dec (make)))
	    (let ((bv (gcry-cipher-encrypt* enc str)))
	      (utf8->string (gcry-cipher-decrypt* dec bv)))))
      => str))

  (let ((str "you complete my fate0123"))
;;;           012345678901234567890123
    (check
	(with-compensations
	  (define (make)
	    (begin0-let ((hd (gcry-cipher-open/c (gcry-cipher-algo des)
						 (gcry-cipher-mode ofb)
						 (gcry-cipher-flags))))
	      (gcry-cipher-setkey hd "12345678")
	      (gcry-cipher-setiv  hd "hello678")))
	  (let ((enc (make))
		(dec (make)))
	    (let ((bv (gcry-cipher-encrypt* enc str)))
	      (utf8->string (gcry-cipher-decrypt* dec bv)))))
      => str))

  #t)


(parametrise ((check-test-name	'hash))

  (check
      (with-compensations
	(let ((hd (gcry-md-open/c (gcry-md-algo md5))))
	  (gcry-md-write* hd "ciao, ciao")
	  (bytevector? (gcry-md-read hd (gcry-md-algo md5)))))
    => #t)

  (check	;final
      (with-compensations
	(let ((hd (gcry-md-open/c (gcry-md-algo md5))))
	  (gcry-md-write* hd "ciao, ciao")
	  (gcry-md-final hd)
	  (bytevector? (gcry-md-read hd (gcry-md-algo md5)))))
    => #t)

  (check	;copy
      (with-compensations
	(let* ((hd  (gcry-md-open/c (gcry-md-algo md5)))
	       (hd1 (gcry-md-copy/c hd)))
	  (gcry-md-write* hd1 "ciao, ciao")
	  (bytevector? (gcry-md-read hd1 (gcry-md-algo md5)))))
    => #t)

  (check	;reset
      (with-compensations
	(let ((hd  (gcry-md-open/c (gcry-md-algo md5))))
	  (gcry-md-write* hd "ciao, ciao")
	  (gcry-md-read hd (gcry-md-algo md5))
	  (gcry-md-reset hd)
	  (gcry-md-write* hd "hello, hello")
	  (bytevector? (gcry-md-read hd (gcry-md-algo md5)))))
    => #t)

  (check	;enable
      (with-compensations
	(let ((hd (gcry-md-open/c (gcry-md-algo md5))))
	  (gcry-md-enable hd (gcry-md-algo sha1))
	  (gcry-md-write* hd "ciao, ciao")
	  (list (bytevector? (gcry-md-read hd (gcry-md-algo md5)))
		(bytevector? (gcry-md-read hd (gcry-md-algo sha1))))))
    => '(#t #t))

  (check	;mac
      (with-compensations
	(let ((hd (gcry-md-open/c (gcry-md-algo md5) (gcry-md-flags hmac))))
	  (gcry-md-setkey hd "woppa")
	  (gcry-md-write* hd "ciao, ciao")
	  (bytevector? (gcry-md-read hd (gcry-md-algo md5)))))
    => #t)

  (check	;hash buffer
      (bytevector? (gcry-md-hash-buffer* (gcry-md-algo md5) "ciao, ciao"))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (gcry-md-algo-name (gcry-md-algo md5))
    => "MD5")

  (check
      (gcry-md-map-name "MD5")
    (=> enum-set=?) (gcry-md-algo md5))

  (check
      (gcry-md-test-algo (gcry-md-algo md5))
    => #t)

  (check
      (with-compensations
	(let ((hd (gcry-md-open/c (gcry-md-algo md5))))
	  (list (gcry-md-is-enabled? hd (gcry-md-algo md5))
		(gcry-md-is-enabled? hd (gcry-md-algo sha1)))))
    => '(#t #f))

  (check
      (with-compensations
	(let ((hd1 (gcry-md-open/c (gcry-md-algo md5) (gcry-md-flags secure)))
	      (hd2 (gcry-md-open/c (gcry-md-algo md5))))
	  (list (gcry-md-is-secure? hd1)
		(gcry-md-is-secure? hd2))))
    => '(#t #f))


  (check
      (gcry-md-get-algo-dlen (gcry-md-algo md5))
    => 16)

  (check
      (with-compensations
	(let ((hd (gcry-md-open/c (gcry-md-algo md5))))
	  (gcry-md-enable hd (gcry-md-algo sha1))
	  (gcry-md-enable hd (gcry-md-algo sha256))
	  (gcry-md-enabled-algos hd)))
    (=> enum-set=?) (enums:%gcry-md-algo md5 sha1 sha256))

  (check
      (bytevector? (gcry-md-get-asnoid (gcry-md-algo md5)))
    => #t)

  #t)


(parametrise ((check-test-name	'random))

  (check
      (with-compensations
	(letrec ((mb (compensate
			 (memblocks-cache 1024)
		       (with
			(memblocks-cache mb)))))
	  (gcry-randomize mb (gcry-random-quality weak)))
	#t)
    => #t)

  (check
      (with-compensations
	(letrec ((mb (compensate
			 (memblocks-cache 1024)
		       (with
			(memblocks-cache mb)))))
	  (gcry-random-add-bytes mb (gcry-random-quality weak)))
	#t)
    => #t)

  (check
      (with-compensations
	(letrec ((mb (compensate
			 (memblocks-cache 1024)
		       (with
			(memblocks-cache mb)))))
	  (gcry-random-add-bytes mb))
	#t)
    => #t)

  (check
      (<memblock>? (gcry-random-bytes 100 (gcry-random-quality weak)))
    => #t)

  (check
      (<memblock>? (gcry-random-bytes/secure 100 (gcry-random-quality weak)))
    => #t)

  (check
      (with-compensations
	(letrec ((mb (compensate
			 (memblocks-cache 1024)
		       (with
			(memblocks-cache mb)))))
	  (gcry-create-nonce mb))
	#t)
    => #t)

  #t)


(parametrise ((check-test-name	'mpi))

  (check	;format STD
      (with-compensations
	(let ((n (gcry-mpi-new/c 50)))
	  (gcry-mpi-set-ui n 100)
	  (bytevector? (gcry-mpi-print n (gcry-mpi-format std)))))
    => #t)

  (check	;format STD
      (with-compensations
	(let ((a (gcry-mpi-new/c 50))
	      (b (gcry-mpi-new/c 50)))
	  (gcry-mpi-set-ui a 100)
	  (gcry-mpi-set-ui b 100)
	  (gcry-mpi=? a (gcry-mpi-scan (gcry-mpi-print b (gcry-mpi-format std))
				       (gcry-mpi-format std)))))
    => #t)

  (check	;format PGP
      (with-compensations
  	(letrec ((n (compensate
  			(gcry-mpi-new 50)
  		      (with
  		       (gcry-mpi-release n)))))
  	  (gcry-mpi-set-ui n 100)
  	  (bytevector? (gcry-mpi-print n (gcry-mpi-format pgp)))))
    => #t)

  (check	;format PGP
      (with-compensations
	(let ((a (gcry-mpi-new/c 50))
	      (b (gcry-mpi-new/c 50)))
	  (gcry-mpi-set-ui a 100)
	  (gcry-mpi-set-ui b 100)
	  (gcry-mpi=? a (gcry-mpi-scan (gcry-mpi-print b (gcry-mpi-format pgp))
				       (gcry-mpi-format pgp)))))
    => #t)

  (check	;format SSH
      (with-compensations
  	(letrec ((n (compensate
  			(gcry-mpi-new 50)
  		      (with
  		       (gcry-mpi-release n)))))
  	  (gcry-mpi-set-ui n 100)
  	  (bytevector? (gcry-mpi-print n (gcry-mpi-format ssh)))))
    => #t)

  (check	;format SSH
      (with-compensations
	(let ((a (gcry-mpi-new/c 50))
	      (b (gcry-mpi-new/c 50)))
	  (gcry-mpi-set-ui a 100)
	  (gcry-mpi-set-ui b 100)
	  (gcry-mpi=? a (gcry-mpi-scan (gcry-mpi-print b (gcry-mpi-format ssh))
				       (gcry-mpi-format ssh)))))
    => #t)

  (check	;format HEX
      (with-compensations
  	(letrec ((n (compensate
  			(gcry-mpi-new 50)
  		      (with
  		       (gcry-mpi-release n)))))
  	  (gcry-mpi-set-ui n 100)
  	  (string? (gcry-mpi-print n (gcry-mpi-format hex)))))
    => #t)

  (check	;format HEX
      (with-compensations
	(let ((a (gcry-mpi-new/c 50))
	      (b (gcry-mpi-new/c 50)))
	  (gcry-mpi-set-ui a 100)
	  (gcry-mpi-set-ui b 100)
	  (gcry-mpi=? a (gcry-mpi-scan (gcry-mpi-print b (gcry-mpi-format hex))
				       (gcry-mpi-format hex)))))
    => #t)

  (check 	;format USG
      (with-compensations
	(letrec ((n (compensate
			(gcry-mpi-new 50)
		      (with
		       (gcry-mpi-release n)))))
	  (gcry-mpi-set-ui n 100)
	  (bytevector? (gcry-mpi-print n (gcry-mpi-format usg)))))
    => #t)

  (check	;format USG
      (with-compensations
  	(let ((a (gcry-mpi-new/c 50))
  	      (b (gcry-mpi-new/c 50)))
  	  (gcry-mpi-set-ui a 100)
  	  (gcry-mpi-set-ui b 100)
  	  (gcry-mpi=? a (gcry-mpi-scan (gcry-mpi-print b (gcry-mpi-format usg))
  				       (gcry-mpi-format usg)))))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (with-compensations
	(letrec ((n (compensate
			(gcry-mpi-new 50)
		      (with
		       (gcry-mpi-release n)))))
	  (gcry-mpi-set-ui n 100)
	  (gcry-mpi->uint n)))
    => 100)

;;; --------------------------------------------------------------------

  (check
      (with-compensations
  	(let ((a (gcry-mpi-new/c 50))
  	      (b (gcry-mpi-new/c 50))
  	      (c (gcry-mpi-new/c 50)))
  	  (gcry-mpi-set-ui a 100)
  	  (gcry-mpi-set-ui b 50)
	  (gcry-mpi-add c a b)
	  (gcry-mpi->uint c)))
    => 150)

  (check
      (with-compensations
  	(let ((a (gcry-mpi-new/c 50))
  	      (b (gcry-mpi-new/c 50))
  	      (c (gcry-mpi-new/c 50)))
  	  (gcry-mpi-set-ui a 100)
  	  (gcry-mpi-set-ui b 50)
	  (gcry-mpi-sub c a b)
	  (gcry-mpi->uint c)))
    => 50)

;;; --------------------------------------------------------------------

  (let ()
    (define (cmp f A B C)
      (with-compensations
  	(let ((a (gcry-mpi-new/c 50))
  	      (b (gcry-mpi-new/c 50))
  	      (c (gcry-mpi-new/c 50)))
  	  (gcry-mpi-set-ui a A)
  	  (gcry-mpi-set-ui b B)
  	  (gcry-mpi-set-ui c C)
	  (f a b c))))

    (check (cmp gcry-mpi=? 10 10 10)	=> #t)
    (check (cmp gcry-mpi=? 10 12 10)	=> #f)

    (check (cmp gcry-mpi<? 10 10 10)	=> #f)
    (check (cmp gcry-mpi<? 10 12 10)	=> #f)
    (check (cmp gcry-mpi<? 10 12 14)	=> #t)

    (check (cmp gcry-mpi<=? 10 10 10)	=> #t)
    (check (cmp gcry-mpi<=? 10 12 10)	=> #f)
    (check (cmp gcry-mpi<=? 10 12 14)	=> #t)

    (check (cmp gcry-mpi>? 10 10 10)	=> #f)
    (check (cmp gcry-mpi>? 10 12 10)	=> #f)
    (check (cmp gcry-mpi>? 14 12 10)	=> #t)

    (check (cmp gcry-mpi>=? 10 10 10)	=> #t)
    (check (cmp gcry-mpi>=? 10 12 10)	=> #f)
    (check (cmp gcry-mpi>=? 14 12 10)	=> #t)

    #t)

  #t)


(parametrise ((check-test-name	'sexps))

  (let ((str "(a (b (c)))"))

    (check
	(with-compensations
	  (gcry-sexp->string (string->gcry-sexp str)))
      => "(1:a\n (1:b\n  (1:c)\n  )\n )\n")

    (check
	(with-compensations
	  (gcry-sexp->string (string->gcry-sexp str) (gcry-sexp-format canon)))
      => "(1:a(1:b(1:c)))")

    (check
	(with-compensations
	  (gcry-sexp->string (string->gcry-sexp str) (gcry-sexp-format advanced)))
      => "(a \n (b \n  (c)\n  )\n )\n")

    (check
    	(with-compensations
    	  (gcry-sexp->string (gcry-sexp-find-token/str (string->gcry-sexp str) "b")
			     (gcry-sexp-format canon)))
      => "(1:b(1:c))")

    #t)

;;; --------------------------------------------------------------------

  (check
      (with-compensations
	(gcry-sexp->list (string->gcry-sexp/c "")))
    => '())

  (check
      (with-compensations
	(gcry-sexp->list (string->gcry-sexp/c "()")))
    => '())

  (check
      (with-compensations
	(gcry-sexp->list (string->gcry-sexp/c "(a)")))
    => '(a))

  (check
      (with-compensations
	(gcry-sexp->list (string->gcry-sexp/c "(a b c)")))
    => '(a b c))

  (check
      (with-compensations
	(gcry-sexp->list (string->gcry-sexp/c "(a (b (c)))")))
    => '(a (b (c))))

  (check
      (with-compensations
	(gcry-sexp->list (string->gcry-sexp/c "(a (b (c) d) e)")))
    => '(a (b (c) d) e))

  (check
      (with-compensations
	(gcry-sexp->list (string->gcry-sexp/c "(a 2:12 c)")))
    => '(a 12 c))

  (check
      (with-compensations
	(gcry-sexp->list (string->gcry-sexp/c "(a 4:1024 5:12345)")))
    => '(a 1024 12345))

;;; --------------------------------------------------------------------

  (check
      (with-compensations
	(gcry-sexp->string (list->gcry-sexp/c '()) (gcry-sexp-format canon)))
    => "()")

  (check
      (with-compensations
	(gcry-sexp->string (list->gcry-sexp/c '(a)) (gcry-sexp-format canon)))
    => "(1:a)")

  (check
      (with-compensations
	(gcry-sexp->string (list->gcry-sexp/c '(a b c)) (gcry-sexp-format canon)))
    => "(1:a1:b1:c)")

  (check
      (with-compensations
	(gcry-sexp->string (list->gcry-sexp/c '(a (b (c)))) (gcry-sexp-format canon)))
    => "(1:a(1:b(1:c)))")

  (check
      (with-compensations
	(gcry-sexp->string (list->gcry-sexp/c '(a (b (c) d) e)) (gcry-sexp-format canon)))
    => "(1:a(1:b(1:c)1:d)1:e)")

  (check
      (with-compensations
  	(gcry-sexp->string (list->gcry-sexp/c '(a 12 c)) (gcry-sexp-format canon)))
    => "(1:a2:121:c)")

  (check
      (with-compensations
  	(gcry-sexp->string (list->gcry-sexp/c '(a 1024 12345)) (gcry-sexp-format canon)))
    => "(1:a4:10245:12345)")

  #t)


(parametrise ((check-test-name	'pubkey))

  (define (gcry-pk-genkey/c . args)
    (letrec ((key-pair (compensate
			   (apply gcry-pk-genkey args)
			 (with
			  (gcry-sexp-release key-pair)))))
      key-pair))

  (check
      (with-compensations
	(let ((key-pair (gcry-pk-genkey/c "(genkey (dsa (nbits 4:1024)(transient-key)))")))
	  (letrec ((pub-key (compensate
				(gcry-sexp-find-token/str key-pair "public-key")
			      (with
			       (gcry-sexp-release pub-key))))
		   (pri-key (compensate
				(gcry-sexp-find-token/str key-pair "private-key")
			      (with
			       (gcry-sexp-release pri-key)))))
;;;(display (gcry-sexp->list pub-key))(newline)
	    (let ((s (gcry-sexp->string key-pair (gcry-sexp-format canon))))
	      (write (string-length s))(newline)
	      (display s)
	      (newline)
	      ;; (display (gcry-sexp->string pub-key (gcry-sexp-format canon)))
	      ;; (newline)
	      )
;; (display (gcry-sexp->string
;; 	  (string->gcry-sexp
;; 	   (gcry-sexp->string key-pair (gcry-sexp-format advanced)))
;; 	  (gcry-sexp-format advanced)))

	    #t)))
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file
