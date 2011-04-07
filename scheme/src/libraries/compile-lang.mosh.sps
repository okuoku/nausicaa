;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: compile script for language libraries with Mosh
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

;;; core libraries
  (only (nausicaa language cond-expand))
  (only (nausicaa language unimplemented))
  (only (nausicaa language conditions))
  (only (nausicaa language extensions))
  (only (nausicaa language parameters))
  (only (nausicaa language pretty-print))
  (only (nausicaa language shared-structures))
  (only (nausicaa language sentinel))
  (only (nausicaa language getenv))
  (only (nausicaa configuration))
  (only (nausicaa contracts))
  (only (nausicaa language assertions))
  (only (nausicaa language makers))
  (only (nausicaa language identifier-properties))
  (only (nausicaa language classes))
  (only (nausicaa language generics))
  (only (nausicaa language deferred-exceptions))
  (only (nausicaa language compensations))
  (only (nausicaa language matches))
  (only (nausicaa))
  (only (nausicaa mutable-pairs))
  (only (nausicaa mutable-strings))

  )

;;; end of file
