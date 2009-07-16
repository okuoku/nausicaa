;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: dependency maker
;;;Date: Mon Jun 29, 2009
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



(import (rnrs))

(let* ((port (let ((port (open-input-port (car (command-line)))))
	       (dynamic-wind
		   (lambda () #f)
		   (lambda ()
		     (open-string-input-port
		      (string-append "(quote (\n"
				     (get-string-all port)
				     "\n)")))
		   (lambda () (close-port port)))))
       (ell (read port)))
  )

;;; end of file
