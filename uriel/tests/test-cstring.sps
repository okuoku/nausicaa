;;;
;;;Part of: Nausicaa/Uriel
;;;Contents: tests for the cstring library
;;;Date: Wed Dec 17, 2008
;;;Time-stamp: <2008-12-17 20:49:33 marco>
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
;;;
;;;This  program  is free  software:  you  can redistribute  it
;;;and/or modify it  under the terms of the  GNU General Public
;;;License as published by the Free Software Foundation, either
;;;version  3 of  the License,  or (at  your option)  any later
;;;version.
;;;
;;;This  program is  distributed in  the hope  that it  will be
;;;useful, but  WITHOUT ANY WARRANTY; without  even the implied
;;;warranty  of  MERCHANTABILITY or  FITNESS  FOR A  PARTICULAR
;;;PURPOSE.   See  the  GNU  General Public  License  for  more
;;;details.
;;;
;;;You should  have received a  copy of the GNU  General Public
;;;License   along   with    this   program.    If   not,   see
;;;<http://www.gnu.org/licenses/>.
;;;



;;;; setup

(import (r6rs)
  (uriel lang)
  (uriel printing)
  (uriel test)
  (uriel memory)
  (uriel cstring)
  (srfi parameters))

(check-set-mode! 'report-failed)



(parameterize ((testname 'conversion))

  (check
      (with-compensations
	(cstring->string (string->cstring "ciao" malloc/c)))
    => "ciao")

  (check
      (with-compensations
	(cstring->string (string->cstring "" malloc/c)))
    => "")

  (check
      (with-compensations
	(strlen (string->cstring "ciao" malloc/c)))
    => 4)

  (check
      (with-compensations
	(strlen (string->cstring "" malloc/c)))
    => 0)

  (check
      (with-compensations
	(cstring->string/len (string->cstring "ciao, hello" malloc/c) 4))
    => "ciao")

  (check
      (with-compensations
	(cstring->string/len (string->cstring "ciao, hello" malloc/c) 1))
    => "c")

  (check
      (with-compensations
	(cstring->string/len (string->cstring "ciao, hello" malloc/c) 0))
    => "")

  (check
      (with-compensations
	(cstring->string/len (string->cstring "ciao, hello" malloc/c) 11))
    => "ciao, hello")

  (check
      (with-compensations
	(cstring->string/len (string->cstring/c "ciao, hello") 4))
    => "ciao")

  )




;;;; done

(check-report)

;;; end of file
