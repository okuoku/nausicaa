;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/cURL
;;;Contents: record type definitions
;;;Date: Fri Nov 20, 2009
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


(library (foreign net curl record-types)
  (export

    <curl-version-info>			<curl-version-info-rtd>
    make-<curl-version-info>		<curl-version-info>?
    <curl-version-info>-age
    <curl-version-info>-version
    <curl-version-info>-version-num
    <curl-version-info>-host
    <curl-version-info>-features
    <curl-version-info>-ssl-version
    <curl-version-info>-ssl-version-num
    <curl-version-info>-libz-version
    <curl-version-info>-protocols
    <curl-version-info>-ares
    <curl-version-info>-ares-num
    <curl-version-info>-libidn
    <curl-version-info>-iconv
    <curl-version-info>-libssh-version

    )
  (import (rnrs))


(define-record-type <curl-version-info>
  (fields (immutable age)
	  (immutable version)
	  (immutable version-num)
	  (immutable host)
	  (immutable features)
	  (immutable ssl-version)
	  (immutable ssl-version-num)
	  (immutable libz-version)
	  (immutable protocols)
	  (immutable ares)
	  (immutable ares-num)
	  (immutable libidn)
	  (immutable iconv)
	  (immutable libssh-version)))

(define <curl-version-info-rtd>
  (record-type-descriptor <curl-version-info>))


;;;; done

)

;;; end of file
