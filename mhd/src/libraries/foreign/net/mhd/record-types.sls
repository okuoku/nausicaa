;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/MHD
;;;Contents: record types
;;;Date: Thu Dec  3, 2009
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


(library (foreign net mhd record-types)
  (export

    <mhd-pointer-wrapper>
    make-<mhd-pointer-wrapper>		<mhd-pointer-wrapper>?
    <mhd-pointer-wrapper>-pointer

    <mhd-daemon>
    make-<mhd-daemon>			<mhd-daemon>?

    <mhd-connection>
    make-<mhd-connection>		<mhd-connection>?

    <mhd-response>
    make-<mhd-response>			<mhd-response>?

    <mhd-post-processor>
    make-<mhd-post-processor>		<mhd-post-processor>?
    )
  (import (rnrs))


(define-record-type <mhd-pointer-wrapper>
  (fields (immutable pointer)))

(define-record-type <mhd-daemon>
  (parent <mhd-pointer-wrapper>))

(define-record-type <mhd-connection>
  (parent <mhd-pointer-wrapper>))

(define-record-type <mhd-response>
  (parent <mhd-pointer-wrapper>))

(define-record-type <mhd-post-processor>
  (parent <mhd-pointer-wrapper>))


;;;; done

)

;;; end of file
