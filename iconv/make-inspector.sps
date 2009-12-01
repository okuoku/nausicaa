;;;!mosh
;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Iconv
;;;Contents: foreign library inspection generator
;;;Date: Fri Nov 27, 2009
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

(define iconv-library-spec
  '(foreign i18n iconv sizeof))

(define-shared-object iconv libiconv.so)

(define-c-type-alias iconv_t		pointer)

(autoconf-lib-write "configuration/iconv-inspector.m4" iconv-library-spec)
(sizeof-lib-write   "src/libraries/foreign/i18n/iconv/sizeof.sls.in" iconv-library-spec)

;;; end of file
