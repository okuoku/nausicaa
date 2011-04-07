;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: compile script for basic libraries with Racket
;;;Date: Thu Apr  7, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (rnrs)

;;; basic libraries
  (only (nausicaa generics object-to-string))
  (only (nausicaa profiling))
  (only (nausicaa enumerations))
  (only (nausicaa lists))
  (only (nausicaa one-dimension-cc))
  (only (nausicaa one-dimension-co))
  (only (nausicaa char-sets))
  (only (nausicaa char-sets blocks))
  (only (nausicaa char-sets categories))
  (only (nausicaa asciis))
  (only (nausicaa bytevectors u8))
  (only (nausicaa strings))
  (only (nausicaa strings xstrings))
  (only (nausicaa streams))
  (only (nausicaa vectors))
  (only (nausicaa vectors xvectors))
  (only (nausicaa bytevectors u8))
  (only (nausicaa debugging))
  (only (nausicaa checks))
  (only (nausicaa loops))
  (only (nausicaa formations))
  (only (nausicaa randomisations))
  (only (nausicaa randomisations vectors))
  (only (nausicaa randomisations strings))
  (only (nausicaa randomisations distributions))
  (only (nausicaa randomisations borosh))
  (only (nausicaa comparisons))
  (only (nausicaa arrays))
  (only (nausicaa msgcat))
  (only (nausicaa times-and-dates))

  (only (nausicaa variables))
  (only (nausicaa keywords))

  (only (nausicaa object-properties))
  (only (nausicaa cleanup-handlers))

  (only (nausicaa queues))
  (only (nausicaa stacks))

  )

;;; end of file
