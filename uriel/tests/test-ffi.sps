;;;
;;;Part of: Uriel libraries
;;;Contents: tests for ffi library
;;;Date: Tue Nov 18, 2008
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


;;;; setup

(import (r6rs)
  (srfi parameters)
  (uriel printing)
  (uriel lang)
  (uriel ffi)
  (uriel ffi sizeof)
  (uriel cstring)
  (uriel test))

(check-set-mode! 'report-failed)



;;;; string functions

(parameterize ((testname 'string))

  (check
      (cstring->string (string->cstring "ciao"))
    => "ciao")

  (check
      (cstring->string (string->cstring ""))
    => "")

  (check
      (strlen (string->cstring "ciao"))
    => 4)

  (check
      (strlen (string->cstring ""))
    => 0)

  (check
      (cstring->string/len (string->cstring "ciao, hello") 4)
    => "ciao")

  (check
      (cstring->string/len (string->cstring "ciao, hello") 1)
    => "c")

  (check
      (cstring->string/len (string->cstring "ciao, hello") 0)
    => "")

  (check
      (cstring->string/len (string->cstring "ciao, hello") 11)
    => "ciao, hello"))



;;;; done

(check-report)

;;; end of file
