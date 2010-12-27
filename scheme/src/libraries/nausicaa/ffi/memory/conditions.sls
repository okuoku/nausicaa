;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: condition types for raw memory handling
;;;Date: Tue Oct 13, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa ffi memory conditions)
  (export
    &out-of-memory &memory-request
    make-out-of-memory-condition make-memory-request-condition
    out-of-memory-condition? memory-request-condition?
    condition-out-of-memory/number-of-bytes
    (rename (condition-out-of-memory/number-of-bytes
	     condition-memory-request/number-of-bytes))
    condition-memory-request/clean?
    raise-out-of-memory raise-memory-request)
  (import (rnrs))

  (define-condition-type &out-of-memory &error
    make-out-of-memory-condition
    out-of-memory-condition?
    (number-of-bytes condition-out-of-memory/number-of-bytes))

  (define (raise-out-of-memory who number-of-bytes)
    (raise
     (condition (make-who-condition who)
		(make-message-condition "out of memory")
		(make-out-of-memory-condition number-of-bytes)
		(make-non-continuable-violation))))

  (define-condition-type &memory-request &out-of-memory
    make-memory-request-condition
    memory-request-condition?
    (clean condition-memory-request/clean?))

  (define (raise-memory-request who number-of-bytes clean)
    (raise-continuable
     (condition (make-who-condition who)
		(make-message-condition "out of memory")
		(make-memory-request-condition number-of-bytes clean)))))

;;; end of file
