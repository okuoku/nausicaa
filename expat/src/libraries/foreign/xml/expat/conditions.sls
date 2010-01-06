;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Expat
;;;Contents: condition object types
;;;Date: Wed Jan  6, 2010
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


(library (foreign xml expat conditions)
  (export

    &expat-error
    make-expat-error-condition		expat-error-condition?

    &expat-parser
    make-expat-parser-condition		expat-parser-condition?
    condition-expat-parser
    )
  (import (rnrs)
    (foreign xml expat platform))

  (define-condition-type &expat-error &error
    make-expat-error-condition expat-error-condition?)

  (define-condition-type &expat-parser &condition
    make-expat-parser-condition expat-parser-condition?
    (parser	condition-expat-parser))

  )

;;; end of file
