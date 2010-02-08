;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/LAPACK
;;;Contents: condition objects
;;;Date: Mon Feb  8, 2010
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


(library (foreign math lapack conditions)
  (export

    &lapack-error
    make-lapack-error-condition
    lapack-error-condition?

    &lapack-invalid-argument
    make-lapack-invalid-argument-condition
    lapack-invalid-argument-condition?
    condition-lapack-argument-position
    condition-lapack-argument-name
    condition-lapack-argument-value

    &lapack-failed-step
    make-lapack-failed-step-condition
    lapack-failed-step-condition?
    condition-lapack-failed-step)
  (import (rnrs))


(define-condition-type &lapack-error
  &error
  make-lapack-error-condition
  lapack-error-condition?)

(define-condition-type &lapack-invalid-argument
  &lapack-error
  make-lapack-invalid-argument-condition
  lapack-invalid-argument-condition?
  (argument-position	condition-lapack-argument-position)
  (argument-name	condition-lapack-argument-name)
  (argument-value	condition-lapack-argument-value))

(define-condition-type &lapack-failed-step
  &lapack-error
  make-lapack-failed-step-condition
  lapack-failed-step-condition?
  (failed-step		condition-lapack-failed-step))


;;;; done

)

;;; end of file
