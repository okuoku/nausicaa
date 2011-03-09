;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: collection of strings
;;;Date: Fri Jan 14, 2011
;;;
;;;Abstract
;;;
;;;	This library  defines and exports bindings to  strings which are
;;;	meant to be comparable using EQ?.
;;;
;;;Copyright (C) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa xml markups fixed-strings)
  (export
    tag-open			tag-close
    processor-instructions-open	processor-instructions-close
    comment-open		comment-close
    )
  (import (rnrs))

  (define tag-open			"<")
  (define tag-close			">")

  (define processor-instructions-open	"<?")
  (define processor-instructions-close	"?>")

  (define comment-open			"<!--")
  (define comment-close			"-->")
  )

;;; end of file
