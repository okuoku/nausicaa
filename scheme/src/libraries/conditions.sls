;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: predefined condition types
;;;Date:Thu Sep  3, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
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


(library (conditions)
  (export

    ;; mismatch
    &mismatch make-mismatch-condition mismatch-condition?

    ;; unimplemented
    &unimplemented make-unimplemented-condition
    unimplemented-condition? raise-unimplemented-error)
  (import (rnrs)
    (unimplemented))


(define-condition-type &mismatch
  &assertion make-mismatch-condition mismatch-condition?)


;;;; done

)

;;; end of file
