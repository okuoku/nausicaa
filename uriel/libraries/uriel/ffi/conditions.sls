;;;
;;;Part of: Uriel libraries for R6RS Scheme
;;;Contents: out of memory condition
;;;Date: Mon Dec  1, 2008
;;;Time-stamp: <2008-12-02 20:19:38 marco>
;;;
;;;Abstract
;;;
;;;
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

    &out-of-memory make-out-of-memory-condition out-of-memory-condition?
    out-of-memory-requested-number-of-bytes

    &errno make-errno-condition errno-condition? errno-code)
  (import (rnrs))

  (define-condition-type &out-of-memory &error
    make-out-of-memory-condition out-of-memory-condition?
    (number-of-bytes out-of-memory-requested-number-of-bytes))

  (define-condition-type &errno &error
    make-errno-condition errno-condition?
    (code errno-code)))

;;; end of file
