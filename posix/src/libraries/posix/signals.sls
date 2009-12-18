;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: parametrised API to interprocess signal functions
;;;Date: Fri Dec 18, 2009
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


(library (posix signals)
  (export
    signal-bub-delivered?	signal-bub-all-delivered
    (rename (primitive:signal-bub-init		signal-bub-init)
	    (primitive:signal-bub-final		signal-bub-final)
	    (primitive:signal-bub-acquire	signal-bub-acquire))

    signal-raise
    kill
    pause
    )
  (import (rnrs)
    (posix helpers)
    (prefix (posix signals primitives) primitive:))

  (define-parametrised signal-bub-delivered? signum)
  (define-parametrised signal-bub-all-delivered)
  (define-parametrised signal-raise signum)
  (define-parametrised kill pid signum)
  (define-parametrised pause)

  )

;;; end of file
