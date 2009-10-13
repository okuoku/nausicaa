;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: compatibility raw memory operation functions for Larceny
;;;Date: Tue Oct 13, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008, 2009 Marco Maggi <marcomaggi@gna.org>
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


(library (foreign memory operations compat)
  (export memset memmove memcpy memcmp)
  (import (rnrs)
    (primitives foreign-procedure))

  (define memset
    (foreign-procedure "memset" '(void* int unsigned) 'void*))

  (define memmove
    (foreign-procedure "memmove" '(void* void* unsigned) 'void*))

  (define memcpy
    (foreign-procedure "memcpy" '(void* void* unsigned) 'void*))

  (define memcmp
    (foreign-procedure "memcmp" '(void* void* unsigned) 'int)))

;;; end of file
