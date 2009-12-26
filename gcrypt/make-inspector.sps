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



(define-c-type gcrypt_t		signed-int)


;;;; done

(define gcrypt-library-spec
  '(foreign crypto gcrypt sizeof))

(define-shared-object gcrypt libgcrypt.so)

(autoconf-lib-write "configuration/gcrypt-inspector.m4" gcrypt-library-spec)
(sizeof-lib-write   "src/libraries/foreign/crypto/gcrypt/sizeof.sls.in" gcrypt-library-spec)

;;; end of file
