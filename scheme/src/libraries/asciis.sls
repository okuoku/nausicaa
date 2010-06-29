;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: byte as ASCII characters
;;;Date: Tue Jun 29, 2010
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


(library (asciis)
  (export
    ascii-upper-case?	ascii-lower-case?	ascii-title-case?
    ascii-cased?
    ascii-alphabetic?	ascii-numeric?
    ascii-upcase	ascii-downcase		ascii-titlecase
    )
  (import (rnrs))

  (define $int-a (char->integer #\a))
  (define $int-z (char->integer #\z))

  (define (ascii-upper-case? byte)
    (<= 65 byte 90))

  (define (ascii-lower-case? byte)
    (<= $int-a byte $int-z))

  (define ascii-title-case? ascii-upper-case?)

  (define (ascii-cased? byte)
    (or (<= 65 byte 90)
	(<= 97 byte 122)))

  (define ascii-alphabetic? ascii-cased?)

  (define (ascii-numeric? byte)
    (<= 48 byte 57))

  (define (ascii-upcase byte)
    (if (ascii-lower-case? byte)
	(- byte 32)
      byte))

  (define (ascii-downcase byte)
    (if (ascii-upper-case? byte)
	(+ 32 byte)
      byte))

  (define (ascii-titlecase byte)
    (if (ascii-lower-case? byte)
	(- byte 32)
      byte))

  )

;;; end of file
