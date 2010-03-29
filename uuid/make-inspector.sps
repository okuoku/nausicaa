;;;!mosh
;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Uuid
;;;Contents: foreign library inspection generator
;;;Date: Fri Nov 27, 2009
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
  (foreign ffi inspector-maker))

(define-c-type-alias uuid_t		pointer)

(define-c-defines "version"
  UUID_VERSION)

(define-c-defines "encoding octet stream lengths"
  UUID_LEN_BIN
  UUID_LEN_STR
  UUID_LEN_SIV)

(define-c-enumeration uuid_rc_t
  "uuid_rc_t"
  UUID_RC_OK
  UUID_RC_ARG
  UUID_RC_MEM
  UUID_RC_SYS
  UUID_RC_INT
  UUID_RC_IMP)

(define-c-defines "UUID make modes"
  UUID_MAKE_V1
  UUID_MAKE_V3
  UUID_MAKE_V4
  UUID_MAKE_V5
  UUID_MAKE_MC)

(define-c-enumeration uuid_fmt_t
  "uuid_fmt_t"
  UUID_FMT_BIN
  UUID_FMT_STR
  UUID_FMT_SIV
  UUID_FMT_TXT)


;;;; done

(define-shared-object uuid libuuid.so)

(define uuid-library-spec
  '(foreign uuid sizeof))

(autoconf-lib-write "configuration/uuid-inspector.m4" uuid-library-spec
		    "NAUSICAA_UUID")
(sizeof-lib-write   "src/libraries/foreign/uuid/sizeof.sls.in" uuid-library-spec)

;;; end of file
