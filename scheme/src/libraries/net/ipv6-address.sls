;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: IPv6 address object type
;;;Date: Wed Jun  9, 2010
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


#!r6rs
(library (net ipv6-address)
  (export

    <ipv6-address>		<ipv6-address>?
    make-<ipv6-address>
    <ipv6-address>-zeroth
    <ipv6-address>-first
    <ipv6-address>-second
    <ipv6-address>-third
    <ipv6-address>-fourth
    <ipv6-address>-fifth
    <ipv6-address>-sixth
    <ipv6-address>-seventh
    )
  (import (nausicaa)
    (net helpers ipv6-address-lexer)
    (net helpers ipv6-address-parser)
    (parser-tools lexical-token)
    (parser-tools source-location)
    )


(define-class <ipv6-address>
  (fields zeroth  first  second  third
	  fourth  fifth  sixth   seventh)
  (nongenerative nausicaa:net:ipv6-address:<ipv6-address>))


;;;; done

)

;;; end of file
