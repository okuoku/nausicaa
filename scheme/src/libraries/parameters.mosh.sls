;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: parameters from Mosh
;;;Date: Thu Jun 18, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2010 Marco Maggi <marcomaggi@gna.org>
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
(library (parameters)
  (export make-parameter
	  (rename (%parameterize parameterize)
		  (%parameterize parameterise)
		  (%parameterize parametrise)))
  (import (rnrs)
    (only (system) make-parameter parameterize))
  (define-syntax %parameterize
    (syntax-rules ()
      ((_ ?bindings . ?body)
       (parameterize ?bindings (let () . ?body))))))

;;; end of file
