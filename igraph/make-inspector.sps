;;;!mosh
;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/IGraph
;;;Contents: foreign library inspection generator
;;;Date:
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


(import (nausicaa)
  (foreign ffi inspector-maker))



(define-c-type-alias igraph_t		pointer)


;;;; done

(define igraph-library-spec
  '(foreign math igraph sizeof))

(define-shared-object igraph libigraph.so)

(autoconf-lib-write "configuration/igraph-inspector.m4" igraph-library-spec)
(sizeof-lib-write   "src/libraries/foreign/math/igraph/sizeof.sls.in" igraph-library-spec)

;;; end of file
