;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: overall configuration options
;;;Date: Mon Oct 25, 2010
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
(library (nausicaa configuration)
  (export enable-function-arguments-validation?
	  enable-assertions?
	  enable-contracts?)
  (import (rnrs)
    (nausicaa language parameters)
    (nausicaa language getenv))


(define (%bool-from-env-var name default)
  (let ((v (getenv name)))
    (case (and v (string-downcase v))
      (("true" "1")
       #t)
      (("false" "0")
       #f)
      (else default))))

(define enable-function-arguments-validation?
  ;;True  if the  functions must  perform the  full validation  of their
  ;;arguments; else the validation code is excluded at expand time.
  ;;
  (make-parameter (%bool-from-env-var "NAUSICAA_ENABLE_ARGUMENTS_VALIDATION" #t)
    (lambda (v)
      (assert (boolean? v))
      v)))

(define enable-assertions?
  ;;True if the assertions must be included.
  ;;
  (make-parameter (%bool-from-env-var "NAUSICAA_ENABLE_ASSERTIONS" #t)
    (lambda (v)
      (assert (boolean? v))
      v)))

(define enable-contracts?
  ;;True if usage of contracts must be included.
  ;;
  (make-parameter (%bool-from-env-var "NAUSICAA_ENABLE_CONTRACTS" #t)
    (lambda (v)
      (assert (boolean? v))
      v)))


;;;; done

)

;;; end of file
