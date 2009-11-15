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
    &mismatch
    make-mismatch-condition
    mismatch-condition?

    ;; wrong num args
    &wrong-num-args
    make-wrong-num-args-condition
    wrong-num-args-condition?
    condition-wrong-num-args-procname
    condition-expected-arguments-number
    condition-given-arguments-number
    raise-wrong-num-args

    ;; unimplemented
    &unimplemented
    make-unimplemented-condition
    unimplemented-condition?
    raise-unimplemented-error)
  (import (rnrs)
    (unimplemented))


(define-condition-type &mismatch
  &assertion make-mismatch-condition mismatch-condition?)

;;; --------------------------------------------------------------------

(define-condition-type &wrong-num-args
  &assertion make-wrong-num-args-condition wrong-num-args-condition?
  (procname	condition-wrong-num-args-procname)
  (expected	condition-expected-arguments-number)
  (given	condition-given-arguments-number))

(define-syntax raise-wrong-num-args
  (syntax-rules ()
    ((_ ?who ?message ?procname ?expected ?given)
     (raise
      (condition (make-who-condition ?who)
		 (make-message-condition ?message)
		 (make-wrong-num-args-condition ?procname ?expected ?given))))))


;;;; done

)

;;; end of file
