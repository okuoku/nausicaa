;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: &unimplemented condition definition
;;;Date: Wed Jan 21, 2009
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

(library (scheme unimplemented)
  (export
    &unimplemented
    make-unimplemented-condition
    unimplemented-condition?
    raise-unimplemented-error)
  (import (rnrs))

  (define-condition-type &unimplemented &error
    make-unimplemented-condition
    unimplemented-condition?)

  (define raise-unimplemented-error
    (case-lambda
     ((who)
      (raise-unimplemented-error who "feature not implemented or not available" #f))
     ((who message)
      (raise-unimplemented-error who message #f))
     ((who message . irritants)
      (raise (let ((c (condition (make-who-condition who)
				 (make-message-condition message)
				 (make-unimplemented-condition))))
	       (if irritants
		   (condition c (make-irritants-condition irritants))
		 c)))))))

;;; end of file
