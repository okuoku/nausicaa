;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: parametrised FFI interface
;;;Date: Mon Nov 30, 2009
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


(library (foreign ffi parametrised)
  (export
    current-shared-object
    define-c-function		define-c-function/with-errno)
  (import (rnrs)
    (foreign ffi))


(define current-shared-object
  (make-parameter primitive:libc-shared-object))

(define-syntax define-c-function
  (syntax-rules ()
    ((_ ?name (?ret-type ?funcname (?arg-type0 ?arg-type ...)))
     (define ?name
       (make-c-function* (current-shared-object) ?ret-type ?funcname (?arg-type0 ?arg-type ...))))))

(define-syntax define-c-function/with-errno
  (syntax-rules ()
    ((_ ?name (?ret-type ?funcname (?arg-type0 ?arg-type ...)))
     (define ?name
       (make-c-function/with-errno* (current-shared-object) ?ret-type ?funcname
				    (?arg-type0 ?arg-type ...))))))


;;;; done

)

;;; end of file
