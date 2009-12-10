;;;!mosh
;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Libxml2
;;;Contents: foreign library inspection generator
;;;Date: Thu Dec 10, 2009
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



(define-c-type-alias libxml2_t		pointer)


;;;; done

(define libxml2-library-spec
  '(foreign xml libxml2 sizeof))

(define-shared-object libxml2 libxml2.so)

(autoconf-lib-write "configuration/libxml2-inspector.m4" libxml2-library-spec)
(sizeof-lib-write   "src/libraries/foreign/xml/libxml2/sizeof.sls.in" libxml2-library-spec)

;;; end of file
