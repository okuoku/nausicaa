;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Gcrypt
;;;Contents: enumeration types
;;;Date: Tue Dec 29, 2009
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


(library (foreign crypto gcrypt enumerations)
  (export
    gcry-cipher-algo	gcry-cipher-algo->value		value->gcry-cipher-algo
    gcry-cipher-mode	gcry-cipher-mode->value		value->gcry-cipher-mode
    gcry-cipher-flags	gcry-cipher-flags->value	value->gcry-cipher-flags
    )
  (import (rnrs)
    (enumerations)
    (foreign crypto gcrypt sizeof))


(define-c-flags gcry-cipher-algo
  ;;; GCRY_CIPHER_NONE = 0
  (GCRY_CIPHER_IDEA		GCRY_CIPHER_3DES
   GCRY_CIPHER_CAST5		GCRY_CIPHER_BLOWFISH
   GCRY_CIPHER_SAFER_SK128	GCRY_CIPHER_DES_SK
   GCRY_CIPHER_AES		GCRY_CIPHER_AES192
   GCRY_CIPHER_AES256		GCRY_CIPHER_TWOFISH
   GCRY_CIPHER_ARCFOUR		GCRY_CIPHER_DES
   GCRY_CIPHER_TWOFISH128	GCRY_CIPHER_SERPENT128
   GCRY_CIPHER_SERPENT192	GCRY_CIPHER_SERPENT256
   GCRY_CIPHER_RFC2268_40	GCRY_CIPHER_RFC2268_128
   GCRY_CIPHER_SEED		GCRY_CIPHER_CAMELLIA128
   GCRY_CIPHER_CAMELLIA192	GCRY_CIPHER_CAMELLIA256
   GCRY_CIPHER_AES128		GCRY_CIPHER_RIJNDAEL
   GCRY_CIPHER_RIJNDAEL128	GCRY_CIPHER_RIJNDAEL192
   GCRY_CIPHER_RIJNDAEL256)
  (idea				triple-des
   cast5			blowfish
   safer-sk128			des-sk
   aes				aes192
   aes256			twofish
   arcfour			des
   twofish128			serpent128
   serpent192			serpent256
   rfc2268-40			rfc2268-128
   seed				camellia128
   camellia192			camellia256
   aes128			rijndael
   rijndael128			rijndael192
   rijndael256))

(define-c-flags gcry-cipher-mode
  ;;; GCRY_CIPHER_MODE_NONE = 0
  (GCRY_CIPHER_MODE_ECB		GCRY_CIPHER_MODE_CFB
   GCRY_CIPHER_MODE_CBC		GCRY_CIPHER_MODE_STREAM
   GCRY_CIPHER_MODE_OFB		GCRY_CIPHER_MODE_CTR)
  (ecb cfb cbc stream ofb ctr))

(define-c-ior-flags gcry-cipher-flags
  (GCRY_CIPHER_SECURE
   GCRY_CIPHER_ENABLE_SYNC
   GCRY_CIPHER_CBC_CTS
   GCRY_CIPHER_CBC_MAC)
  (secure enable-sync cbc-cts cbc-mac))


;;;; done

)

;;; end of file
