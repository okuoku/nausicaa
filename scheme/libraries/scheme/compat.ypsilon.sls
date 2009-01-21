;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: Ypsilon compatibility library for (scheme) language
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

#!r6rs
(library (scheme compat)
  (export

    equal-hash pretty-print

    ;; parameters
    make-parameter parameterize

    ;; environment variables
    (rename (lookup-process-environment get-environment-variable)
	    (process-environment->alist get-environment-variables)))
  (import (rnrs)
    (only (core)
	  make-parameter parameterize pretty-print
	  lookup-process-environment process-environment->alist)))

;;; end of file
