;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: auxiliary library for testing makers
;;;Date: Wed Jun  2, 2010
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
(library (makers-lib)
  (export doit1 doit2 doit3 doit4 :alpha :beta :gamma)
  (import (rnrs)
    (nausicaa language makers))

  (define-auxiliary-syntax :alpha)
  (define-auxiliary-syntax :beta)
  (define-auxiliary-syntax :gamma)

  (define v 2)
  (define t "abc")

  (define-maker doit1
    list ((:alpha	1)
	  (:beta	2)
	  (:gamma	(+ 1 v))))

  (define S "ciao")

  (define-maker doit2
    (list (string-ref S 2) #\b)
    ((:alpha	1)
     (:beta	2)
     (:gamma	3)))

  (define-maker (doit3 a b)
    list
    ((:alpha	1)
     (:beta	2)
     (:gamma	3)))

  (define-maker (doit4 a b)
    (list #\a (string-ref t 1))
    ((:alpha	1)
     (:beta	2)
     (:gamma	3)))

  )

;;; end of file
