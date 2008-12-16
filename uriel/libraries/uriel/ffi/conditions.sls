;;;
;;;Part of: Uriel libraries for R6RS Scheme
;;;Contents: condition types definitions
;;;Date: Mon Dec  1, 2008
;;;Time-stamp: <2008-12-16 10:13:31 marco>
;;;
;;;Abstract
;;;
;;;	Condition type definitions.
;;;
;;;	  The   function    RAISE-ERRNO-ERROR   is   defined    in   the
;;;	"compat.*.sls" files because it requires the STRERROR interface.
;;;
;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
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

(library (uriel ffi conditions)
  (export

    ;;out of memory
    &out-of-memory
    make-out-of-memory-condition out-of-memory-condition?
    out-of-memory-requested-number-of-bytes
    raise-out-of-memory

    ;;errno error
    &errno
    (rename (make-errno-condition* make-errno-condition))
    errno-condition?
    errno-numeric-value errno-symbolic-value)
  (import (r6rs)
    (uriel ffi errno))

  (define-condition-type &out-of-memory &error
    make-out-of-memory-condition out-of-memory-condition?
    (number-of-bytes out-of-memory-requested-number-of-bytes))

  (define-condition-type &errno &error
    make-errno-condition errno-condition?
    (numeric-value errno-numeric-value)
    (symbolic-value errno-symbolic-value))

  (define (make-errno-condition* errno-numeric-value)
    (make-errno-condition errno-numeric-value
			  (errno->symbol/or-error errno-numeric-value)))

  (define (raise-out-of-memory who number-of-bytes)
    (raise (condition (make-who-condition who)
		      (make-message-condition "out of memory")
		      (make-out-of-memory-condition number-of-bytes)))))

;;; end of file
