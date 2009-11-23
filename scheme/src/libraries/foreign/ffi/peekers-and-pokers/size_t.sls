;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: peekers and pokers for size_t and ssize_t
;;;Date: Mon Nov 23, 2009
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

(library (foreign ffi peekers-and-pokers size_t)
  (export
    pointer-set-c-size_t!	pointer-set-c-ssize_t!
    pointer-ref-c-size_t	pointer-ref-c-ssize_t)
  (import (rnrs))

  (define pointer-set-c-size_t!		pointer-set-c-unsigned-int!)
  (define pointer-set-c-ssize_t!	pointer-set-c-signed-int!)
  (define pointer-ref-c-size_t		pointer-ref-c-unsigned-int)
  (define pointer-ref-c-ssize_t		pointer-ref-c-signed-int))

;;; end of file
