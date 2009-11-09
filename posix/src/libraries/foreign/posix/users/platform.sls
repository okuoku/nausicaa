;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: low level interface to users and groups functions
;;;Date: Tue Jan  6, 2009
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


(library (foreign posix users platform)
  (export
    )
  (import (rnrs)
    (foreign posix shared-object)
    (foreign ffi)
    (foreign posix sizeof))

  (define dummy
    (shared-object standard-c-library))



;;;; code





;;;; done

)

;;; end of file
