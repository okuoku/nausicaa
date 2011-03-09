;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: profiling library
;;;Date: Tue Jul 28, 2009
;;;
;;;Abstract
;;;
;;;	Collection of utilities to profile code execution.
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


#!r6rs
(library (nausicaa profiling)
  (export repeat time)
  (import (rnrs)
    (nausicaa profiling compat))



(define-syntax repeat
  (syntax-rules ()
    ((_ ?times ?expr)
     (do ((n 0 (+ 1 n)))
	 ((= n ?times))
       ?expr))))


;;;; done

)

;;; end of file
